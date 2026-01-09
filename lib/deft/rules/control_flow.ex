defmodule Deft.Rules.ControlFlow do
  @moduledoc """
  Control flow typing rules implemented using the declarative DSL.

  These rules handle:
  - If expressions
  - Cond expressions
  - Case expressions (with exhaustiveness checking)
  - Match (assignment) expressions
  """

  use Deft.Rule.DSL

  alias Deft.AST
  alias Deft.PatternMatching
  alias Deft.Subtyping
  alias Deft.Type

  # ============================================================================
  # If Expression Rule
  # ============================================================================

  defrule(:if,
    match: %AST.If{},
    judgment: :synth,
    do:
      (
        %AST.If{predicate: predicate, do: do_branch, else: else_branch, meta: meta} = ast

        # Type check predicate
        {erased_pred, pred_type, pred_bindings} = synth!(predicate, ctx)

        # Validate predicate is boolean
        require_subtype!(pred_type, Type.boolean())

        # Type check branches with predicate bindings injected
        {erased_do, do_type, _do_bindings} = synth_with_bindings!(do_branch, pred_bindings, ctx)

        {erased_else, else_type, _else_bindings} =
          synth_with_bindings!(else_branch, pred_bindings, ctx)

        type = Type.union(do_type, else_type)
        erased = {:if, meta, [erased_pred, [do: erased_do, else: erased_else]]}

        emit(erased, type, pred_bindings)
      )
  )

  # ============================================================================
  # Cond Expression Rule
  # ============================================================================

  defrule(:cond,
    match: %AST.Cond{},
    judgment: :synth,
    do:
      (
        %AST.Cond{branches: branches, meta: meta} = ast

        {erased_branches, types} =
          Enum.map(branches, fn %AST.CondBranch{predicate: pred, body: body, meta: branch_meta} ->
            {erased_pred, pred_type, _bindings} = synth!(pred, ctx)

            require_subtype!(pred_type, Type.boolean())

            {erased_body, body_type, _bindings} = synth!(body, ctx)

            {{:->, branch_meta, [[erased_pred], erased_body]}, body_type}
          end)
          |> Enum.unzip()

        type = union_types(types)
        erased = {:cond, meta, [[do: erased_branches]]}

        emit(erased, type)
      )
  )

  # ============================================================================
  # Case Expression Rule
  # ============================================================================

  defrule(:case,
    match: %AST.Case{},
    judgment: :synth,
    do:
      (
        %AST.Case{subject: subject, branches: branches, meta: meta} = ast

        # Type check subject
        {erased_subject, subject_type, subject_bindings} = synth!(subject, ctx)

        # Type check each branch
        {erased_branches, pattern_types, branch_types} =
          Enum.map(branches, fn %AST.CaseBranch{pattern: pattern, body: body, meta: branch_meta} ->
            # Handle pattern against subject type
            {erased_pattern, pattern_type, pattern_bindings} =
              PatternMatching.handle_pattern(pattern, subject_type, ctx)

            # Type check body with subject and pattern bindings injected
            all_bindings = subject_bindings ++ pattern_bindings

            {erased_body, body_type, _body_bindings} =
              synth_with_bindings!(body, all_bindings, ctx)

            {{:->, branch_meta, [[erased_pattern], erased_body]}, pattern_type, body_type}
          end)
          |> Enum.reduce({[], [], []}, fn {e, p, b}, {es, ps, bs} ->
            {es ++ [e], ps ++ [p], bs ++ [b]}
          end)

        # Check exhaustiveness if feature is enabled
        if feature_enabled?(ctx, :exhaustiveness_checking) do
          Deft.Rules.ControlFlow.exhaustive_check!(subject_type, pattern_types)
        end

        type = union_types(branch_types)
        erased = {:case, meta, [erased_subject, [do: erased_branches]]}

        emit(erased, type, subject_bindings)
      )
  )

  @doc false
  def exhaustive_check!(%Type.Union{} = subject_type, pattern_types) do
    exhaustive_check!(subject_type.fst, pattern_types)
    exhaustive_check!(subject_type.snd, pattern_types)
  end

  def exhaustive_check!(%Type.ADT{} = subject_type, pattern_types) do
    for variant <- subject_type.variants do
      unless Enum.any?(pattern_types, fn pattern ->
               Subtyping.subtype_of?(pattern, variant)
             end) do
        raise Deft.InexhaustivePatterns, missing: variant
      end
    end
  end

  def exhaustive_check!(subject_type, pattern_types) do
    unless Enum.any?(pattern_types, &Subtyping.subtype_of?(&1, subject_type)) do
      raise Deft.InexhaustivePatterns, missing: subject_type
    end
  end

  # ============================================================================
  # Match (Assignment) Rule
  # ============================================================================

  defrule(:match,
    match: %AST.Match{},
    judgment: :synth,
    do:
      (
        %AST.Match{pattern: pattern, value: value, meta: meta} = ast

        # Type check the value first
        {erased_value, value_type, value_bindings} = synth!(value, ctx)

        # Handle pattern matching
        {erased_pattern, _pattern_type, pattern_bindings} =
          PatternMatching.handle_pattern(pattern, value_type, ctx)

        erased = {:=, meta, [erased_pattern, erased_value]}
        bindings = value_bindings ++ pattern_bindings

        emit(erased, value_type, bindings)
      )
  )
end
