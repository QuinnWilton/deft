defmodule Deft.Rules.ControlFlow do
  @moduledoc """
  Typing rules for control flow constructs: if, cond, case, and match.
  """

  alias Deft.AST
  alias Deft.PatternMatching
  alias Deft.Subtyping
  alias Deft.Type
  alias Deft.TypeChecker

  # If expression rule
  defmodule If do
    @behaviour Deft.Rule

    @impl true
    def name, do: :if

    @impl true
    def judgment, do: :synth

    @impl true
    def matches?(%AST.If{}), do: true
    def matches?(_), do: false

    @impl true
    def apply(
          %AST.If{predicate: predicate, do: do_branch, else: else_branch, meta: meta},
          _expected,
          ctx
        ) do
      # Type check predicate
      {:ok, erased_pred, pred_type, pred_bindings, ctx} = TypeChecker.check(predicate, ctx)

      # Validate predicate is boolean
      unless Subtyping.subtype_of?(Type.boolean(), pred_type) do
        raise Deft.TypecheckingError, expected: Type.boolean(), actual: pred_type
      end

      # Type check branches with predicate bindings injected
      {:ok, erased_do, do_type, _do_bindings, _ctx} =
        TypeChecker.check_in_context(do_branch, pred_bindings, ctx)

      {:ok, erased_else, else_type, _else_bindings, _ctx} =
        TypeChecker.check_in_context(else_branch, pred_bindings, ctx)

      type = Type.union(do_type, else_type)
      erased = {:if, meta, [erased_pred, [do: erased_do, else: erased_else]]}

      {:ok, erased, type, pred_bindings, ctx}
    end
  end

  # Cond expression rule
  defmodule Cond do
    @behaviour Deft.Rule

    @impl true
    def name, do: :cond

    @impl true
    def judgment, do: :synth

    @impl true
    def matches?(%AST.Cond{}), do: true
    def matches?(_), do: false

    @impl true
    def apply(%AST.Cond{branches: branches, meta: meta}, _expected, ctx) do
      {erased_branches, types} =
        Enum.map(branches, fn %AST.CondBranch{predicate: pred, body: body, meta: branch_meta} ->
          {:ok, erased_pred, pred_type, _bindings, _ctx} = TypeChecker.check(pred, ctx)

          unless Subtyping.subtype_of?(Type.boolean(), pred_type) do
            raise Deft.TypecheckingError, expected: Type.boolean(), actual: pred_type
          end

          {:ok, erased_body, body_type, _bindings, _ctx} = TypeChecker.check(body, ctx)

          {{:->, branch_meta, [[erased_pred], erased_body]}, body_type}
        end)
        |> Enum.unzip()

      type = Enum.reduce(types, &Type.union/2)
      erased = {:cond, meta, [[do: erased_branches]]}

      {:ok, erased, type, [], ctx}
    end
  end

  # Case expression rule
  defmodule Case do
    @behaviour Deft.Rule

    @impl true
    def name, do: :case

    @impl true
    def judgment, do: :synth

    @impl true
    def matches?(%AST.Case{}), do: true
    def matches?(_), do: false

    @impl true
    def apply(%AST.Case{subject: subject, branches: branches, meta: meta}, _expected, ctx) do
      # Type check subject
      {:ok, erased_subject, subject_type, subject_bindings, ctx} = TypeChecker.check(subject, ctx)

      # Type check each branch
      {erased_branches, pattern_types, branch_types} =
        Enum.map(branches, fn %AST.CaseBranch{pattern: pattern, body: body, meta: branch_meta} ->
          # Handle pattern against subject type
          {erased_pattern, pattern_type, pattern_bindings} =
            PatternMatching.handle_pattern(pattern, subject_type, ctx)

          # Type check body with subject and pattern bindings injected
          all_bindings = subject_bindings ++ pattern_bindings

          {:ok, erased_body, body_type, _body_bindings, _ctx} =
            TypeChecker.check_in_context(body, all_bindings, ctx)

          {{:->, branch_meta, [[erased_pattern], erased_body]}, pattern_type, body_type}
        end)
        |> unzip3()

      # Check exhaustiveness
      exhaustive_check!(subject_type, pattern_types)

      type = Enum.reduce(branch_types, &Type.union/2)
      erased = {:case, meta, [erased_subject, [do: erased_branches]]}

      {:ok, erased, type, subject_bindings, ctx}
    end

    defp exhaustive_check!(%Type.Union{} = subject_type, pattern_types) do
      exhaustive_check!(subject_type.fst, pattern_types)
      exhaustive_check!(subject_type.snd, pattern_types)
    end

    defp exhaustive_check!(%Type.ADT{} = subject_type, pattern_types) do
      for variant <- subject_type.variants do
        unless Enum.any?(pattern_types, fn pattern ->
                 Subtyping.subtype_of?(pattern, variant)
               end) do
          raise Deft.InexhaustivePatterns, missing: variant
        end
      end
    end

    defp exhaustive_check!(subject_type, pattern_types) do
      unless Enum.any?(pattern_types, &Subtyping.subtype_of?(&1, subject_type)) do
        raise Deft.InexhaustivePatterns, missing: subject_type
      end
    end

    defp unzip3(list) do
      {a, b, c} =
        Enum.reduce(list, {[], [], []}, fn {x, y, z}, {xs, ys, zs} ->
          {xs ++ [x], ys ++ [y], zs ++ [z]}
        end)

      {a, b, c}
    end
  end

  # Match (assignment) rule
  defmodule Match do
    @behaviour Deft.Rule

    @impl true
    def name, do: :match

    @impl true
    def judgment, do: :synth

    @impl true
    def matches?(%AST.Match{}), do: true
    def matches?(_), do: false

    @impl true
    def apply(%AST.Match{pattern: pattern, value: value, meta: meta}, _expected, ctx) do
      # Type check the value first
      {:ok, erased_value, value_type, value_bindings, ctx} = TypeChecker.check(value, ctx)

      # Handle pattern matching
      {erased_pattern, _pattern_type, pattern_bindings} =
        PatternMatching.handle_pattern(pattern, value_type, ctx)

      erased = {:=, meta, [erased_pattern, erased_value]}
      bindings = value_bindings ++ pattern_bindings

      {:ok, erased, value_type, bindings, ctx}
    end
  end

  @doc """
  Returns all control flow rules.
  """
  def rules do
    [If, Cond, Case, Match]
  end
end
