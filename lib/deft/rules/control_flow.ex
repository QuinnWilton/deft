defmodule Deft.Rules.ControlFlow do
  @moduledoc """
  Control flow typing rules implemented using the declarative DSL.

  These rules handle:
  - If expressions
  - Cond expressions
  - Case expressions (with exhaustiveness checking)
  - Match (assignment) expressions
  """

  use Deft.Rules.DSL

  alias Deft.AST
  alias Deft.AST.Erased
  alias Deft.Error
  alias Deft.PatternMatching
  alias Deft.Span
  alias Deft.Subtyping
  alias Deft.Type

  # ============================================================================
  # If Expression Rule
  # ============================================================================

  defrule :if, %AST.If{predicate: pred, do: do_, else: else_, meta: meta} do
    # Synthesize predicate
    pred ~> {pred_e, pred_t}

    # Check predicate type subtypes boolean
    pred_t <<< Type.boolean()

    # Synthesize branches
    do_ ~> {do_e, do_t}
    else_ ~> {else_e, else_t}

    conclude(
      Erased.if_expr(meta, pred_e, do_e, else_e)
      ~> Type.union(do_t, else_t)
    )
  end

  # ============================================================================
  # Cond Expression Rule
  # ============================================================================

  defrule :cond, %AST.Cond{branches: branches, meta: meta} do
    compute {erased_branches, types} do
      Enum.map(branches, fn %AST.CondBranch{predicate: pred, body: body, meta: branch_meta} ->
        {erased_pred, pred_type, _} = Deft.Rules.DSL.Helpers.synth!(pred, [], ctx)
        require_subtype!(pred_type, Type.boolean())

        {erased_body, body_type, _} = Deft.Rules.DSL.Helpers.synth!(body, [], ctx)

        {Erased.branch(branch_meta, erased_pred, erased_body), body_type}
      end)
      |> Enum.unzip()
    end

    conclude(Erased.cond_expr(meta, erased_branches) ~> union_types(types))
  end

  # ============================================================================
  # Case Expression Rule
  # ============================================================================

  defrule :case, %AST.Case{subject: subject, branches: branches, meta: meta} do
    # Synthesize subject
    subject ~> {erased_subject, subject_type}

    # Process all branches
    compute {erased_branches, pattern_types, branch_types} do
      Enum.map(branches, fn %AST.CaseBranch{pattern: pattern, body: body, meta: branch_meta} ->
        # Handle pattern against subject type
        {erased_pattern, pattern_type, pattern_bindings} =
          PatternMatching.handle_pattern(pattern, subject_type, ctx,
            branch_meta: branch_meta,
            subject: subject,
            subject_type: subject_type
          )

        # Synthesize body with pattern bindings
        all_bindings = bindings ++ pattern_bindings

        {erased_body, body_type, _} =
          Deft.Rules.DSL.Helpers.synth!(body, all_bindings, ctx)

        {Erased.branch(branch_meta, erased_pattern, erased_body), pattern_type, body_type}
      end)
      |> Enum.reduce({[], [], []}, fn {e, p, b}, {es, ps, bs} ->
        {es ++ [e], ps ++ [p], bs ++ [b]}
      end)
    end

    # Check exhaustiveness if feature enabled
    if_feature :exhaustiveness_checking do
      Deft.Rules.ControlFlow.exhaustive_check!(
        subject_type,
        pattern_types,
        subject: subject,
        branches: branches,
        ctx: ctx
      )
    end

    conclude(
      Erased.case_expr(meta, erased_subject, erased_branches)
      ~> union_types(branch_types)
    )
  end

  @doc false
  # Backwards-compatible 2-argument version for testing
  def exhaustive_check!(subject_type, pattern_types) do
    exhaustive_check!(subject_type, pattern_types, nil)
  end

  def exhaustive_check!(subject_type, pattern_types, opts)

  # Simple location-only call (for tests and simple cases)
  def exhaustive_check!(%Type.Union{} = subject_type, pattern_types, nil) do
    exhaustive_check!(subject_type.fst, pattern_types, nil)
    exhaustive_check!(subject_type.snd, pattern_types, nil)
  end

  def exhaustive_check!(%Type.Union{} = subject_type, pattern_types, opts) when is_list(opts) do
    exhaustive_check!(subject_type.fst, pattern_types, opts)
    exhaustive_check!(subject_type.snd, pattern_types, opts)
  end

  def exhaustive_check!(%Type.ADT{} = subject_type, pattern_types, nil) do
    # Find which variants are covered
    {covered, missing} =
      Enum.split_with(subject_type.variants, fn variant ->
        Enum.any?(pattern_types, fn pattern ->
          Subtyping.subtype_of?(pattern, variant)
        end)
      end)

    unless Enum.empty?(missing) do
      Error.raise!(
        Error.inexhaustive_patterns(
          missing: missing,
          covered: covered
        )
      )
    end
  end

  def exhaustive_check!(%Type.ADT{} = subject_type, pattern_types, opts) when is_list(opts) do
    # Find which variants are covered
    {covered, missing} =
      Enum.split_with(subject_type.variants, fn variant ->
        Enum.any?(pattern_types, fn pattern ->
          Subtyping.subtype_of?(pattern, variant)
        end)
      end)

    unless Enum.empty?(missing) do
      raise_inexhaustive_error(subject_type, missing, covered, opts)
    end
  end

  def exhaustive_check!(subject_type, pattern_types, nil) do
    unless Enum.any?(pattern_types, &Subtyping.subtype_of?(&1, subject_type)) do
      Error.raise!(Error.inexhaustive_patterns(missing: subject_type))
    end
  end

  def exhaustive_check!(subject_type, pattern_types, opts) when is_list(opts) do
    unless Enum.any?(pattern_types, &Subtyping.subtype_of?(&1, subject_type)) do
      raise_inexhaustive_error(subject_type, subject_type, [], opts)
    end
  end

  defp raise_inexhaustive_error(subject_type, missing, covered, opts) do
    subject = Keyword.fetch!(opts, :subject)
    branches = Keyword.fetch!(opts, :branches)
    ctx = Keyword.fetch!(opts, :ctx)

    # Build spans for the case expression and all branches
    subject_location = Span.extract(subject)

    # Create labels for each covered variant
    covered_labels =
      Enum.map(covered, fn variant ->
        Error.format_type(variant)
      end)

    # Build branch spans showing what each covers
    branch_spans =
      branches
      |> Enum.zip(Stream.concat(covered_labels, Stream.repeatedly(fn -> nil end)))
      |> Enum.map(fn {%AST.CaseBranch{pattern: pattern, meta: branch_meta}, covered_label} ->
        pattern_location = Span.extract(pattern) || Span.extract(branch_meta)
        label = if covered_label, do: "covers `#{covered_label}`", else: "branch"
        Span.secondary(pattern_location, label, nil)
      end)

    # All spans are context (secondary) since the error is about missing branches
    spans =
      Span.filter([
        Span.secondary(subject_location, "subject has type", subject_type)
        | branch_spans
      ])

    Error.raise!(
      Error.inexhaustive_patterns(
        missing: missing,
        covered: covered,
        location: subject_location,
        spans: spans
      ),
      ctx
    )
  end

  # ============================================================================
  # Match (Assignment) Rule
  # ============================================================================

  defrule :match, %AST.Match{pattern: pattern, value: value, meta: meta} do
    # Synthesize the value
    value ~> {erased_value, value_type}

    # Handle pattern matching
    compute {erased_pattern, pattern_bindings} do
      {erased_pattern, _pattern_type, pattern_bindings} =
        PatternMatching.handle_pattern(pattern, value_type, ctx)

      {erased_pattern, pattern_bindings}
    end

    conclude(Erased.match(meta, erased_pattern, erased_value) ~> value_type,
      bind: pattern_bindings
    )
  end
end
