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
  alias Deft.Span
  alias Deft.Subtyping
  alias Deft.Type

  # ============================================================================
  # If Expression Rule
  # ============================================================================

  defrule :if, %AST.If{predicate: pred, do: do_, else: else_, meta: meta} do
    # Check predicate against boolean
    (pred <~ Type.boolean()) >>> pred_e

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
    # Synthesize all branches - each branch rule returns body_t
    branches ~>> {branches_e, body_ts}

    conclude(Erased.cond_expr(meta, branches_e) ~> union_types(body_ts))
  end

  # ============================================================================
  # Case Expression Rule
  # ============================================================================

  defrule :case, %AST.Case{subject: subj, branches: branches, meta: meta} do
    # Synthesize subject
    subj ~> {subj_e, subj_t}

    # Synthesize all branches with subject type and subject AST in scoped context
    # The 3-element binding auto-unzips {pat_t, body_t} tuples from each branch
    (branches &&& [subj_t: subj_t, subj: subj]) ~>> {branches_e, pat_ts, body_ts}

    # Check exhaustiveness if feature enabled
    if_feature :exhaustiveness_checking do
      Deft.Rules.ControlFlow.exhaustive_check!(
        subj_t,
        pat_ts,
        subject: subj,
        branches: branches,
        ctx: ctx
      )
    end

    conclude(
      Erased.case_expr(meta, subj_e, branches_e)
      ~> union_types(body_ts)
    )
  end

  def exhaustive_check!(subject_type, pattern_types, opts \\ nil)

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

  defrule :match, %AST.Match{pattern: pat, value: val, meta: meta} do
    # Synthesize the value
    val ~> {val_e, val_t}

    # Handle pattern matching using pattern judgment
    (pat <~> val_t) >>> {pat_e, _pat_t, pat_bs}

    conclude(Erased.match(meta, pat_e, val_e) ~> val_t, bind: pat_bs)
  end

  # ============================================================================
  # Case Branch Rule (for use with scoped context)
  # ============================================================================

  # Types a case branch against a subject type passed via scoped context.
  #
  # This rule is invoked when the parent `:case` rule uses:
  #     (branches &&& [subj_t: subj_t, subj: subj]) ~>> {branches_e, pat_ts, body_ts}
  #
  # The scoped context provides subject info for error messages in pattern matching.
  # Returns {pat_t, body_t} as its synthesized type.
  defrule :case_branch, %AST.CaseBranch{pattern: pat, body: body, meta: meta} do
    subj_t = scoped(:subj_t)

    # Pattern judgment - handle_pattern reads subj/subj_t from scoped context for errors
    (pat <~> subj_t) >>> {pat_e, pat_t, pat_bs}

    # Synthesize body with pattern bindings in scope
    (pat_bs +++ body) ~> {body_e, body_t}

    # Return {pat_t, body_t} for parent to collect and unzip
    conclude(
      Erased.branch(meta, pat_e, body_e)
      ~> {pat_t, body_t}
    )
  end

  # ============================================================================
  # Cond Branch Rule
  # ============================================================================

  # Types a cond branch (predicate -> body).
  #
  # Returns body_t as its synthesized type.
  defrule :cond_branch, %AST.CondBranch{predicate: pred, body: body, meta: meta} do
    # Check predicate against boolean
    (pred <~ Type.boolean()) >>> pred_e

    # Synthesize body
    body ~> {body_e, body_t}

    conclude(Erased.branch(meta, pred_e, body_e) ~> body_t)
  end
end
