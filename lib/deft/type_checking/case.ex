defmodule Deft.TypeChecking.Case do
  import Deft.Helpers

  alias Deft.AST
  alias Deft.PatternMatching
  alias Deft.Subtyping
  alias Deft.Type

  def type_check(%AST.Case{} = case_ast, env) do
    {subject, subject_t, bindings} = compute_and_erase_types(case_ast.subject, env)

    {branches, types} =
      Enum.map(case_ast.branches, fn
        %AST.CaseBranch{} = branch ->
          {pattern, pattern_t, pattern_bindings} =
            PatternMatching.handle_pattern(branch.pattern, subject_t, env)

          {body, body_t, _} =
            compute_and_erase_type_in_context(
              branch.body,
              bindings ++ pattern_bindings,
              env
            )

          {{:->, branch.meta, [[pattern], body]}, {pattern_t, body_t}}
      end)
      |> Enum.unzip()

    {patterns_t, branches_t} = Enum.unzip(types)

    # HACK: This is just to see whether basic exhaustiveness checking works
    if is_struct(subject_t, Type.Union) do
      missing =
        Enum.find(Type.Union.types(subject_t), fn t ->
          not Enum.any?(patterns_t, &Subtyping.subtype_of?(&1, t))
        end)

      if missing do
        raise Deft.InexhaustivePatterns, missing: missing
      end
    end

    type = Type.union(branches_t)

    {:case, case_ast.meta, [subject, [do: branches]]}
    |> annotate_type(type)
    |> annotate_bindings(bindings)
  end
end
