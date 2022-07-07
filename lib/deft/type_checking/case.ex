defmodule Deft.TypeChecking.Case do
  import Deft.Helpers

  alias Deft.AST
  alias Deft.PatternMatching
  alias Deft.Subtyping
  alias Deft.Type

  def type_check(%AST.Case{} = case_ast, env) do
    {subject, subject_type, bindings} = compute_and_erase_types(case_ast.subject, env)

    {branches, types} =
      Enum.map(case_ast.branches, fn
        %AST.CaseBranch{} = branch ->
          {pattern, pattern_t, pattern_bindings} =
            PatternMatching.handle_pattern(branch.pattern, subject_type, env)

          {body, body_t, _} =
            compute_and_erase_type_in_context(
              branch.body,
              bindings ++ pattern_bindings,
              env
            )

          {{:->, branch.meta, [[pattern], body]}, {pattern_t, body_t}}
      end)
      |> Enum.unzip()

    {pattern_types, branch_types} = Enum.unzip(types)

    exhaustive_check!(subject_type, pattern_types)

    type = Enum.reduce(branch_types, &Type.union/2)

    {:case, case_ast.meta, [subject, [do: branches]]}
    |> annotate_type(type)
    |> annotate_bindings(bindings)
  end

  def exhaustive_check!(%Type.Union{} = subject_type, pattern_types) do
    exhaustive_check!(subject_type.fst, pattern_types)
    exhaustive_check!(subject_type.snd, pattern_types)
  end

  def exhaustive_check!(subject_type, pattern_types) do
    unless Enum.any?(pattern_types, &Subtyping.subtype_of?(&1, subject_type)) do
      raise Deft.InexhaustivePatterns, missing: subject_type
    end
  end
end
