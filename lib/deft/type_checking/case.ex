defmodule Deft.TypeChecking.Case do
  import Deft.Helpers

  alias Deft.AST
  alias Deft.PatternMatching
  alias Deft.Type

  def type_check(%AST.Case{} = case_ast, env) do
    {subject, subject_t, bindings} = compute_and_erase_types(case_ast.subject, env)

    {branches, branches_t} =
      Enum.map(case_ast.branches, fn
        %AST.CaseBranch{} = branch ->
          {pattern, _, pattern_bindings} =
            PatternMatching.handle_pattern(branch.pattern, subject_t, env)

          {body, body_t, _} =
            compute_and_erase_type_in_context(
              branch.body,
              bindings ++ pattern_bindings,
              env
            )

          {{:->, branch.meta, [[pattern], body]}, body_t}
      end)
      |> Enum.unzip()

    type = Type.Union.new(branches_t)

    {:case, case_ast.meta, [subject, [do: branches]]}
    |> annotate_type(type)
    |> annotate_bindings(bindings)
  end
end
