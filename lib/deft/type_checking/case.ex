defmodule Deft.TypeChecking.Case do
  import Deft.TypeChecking

  alias Deft.AST
  alias Deft.Type

  def type_check(%AST.Case{} = case_ast, env) do
    {subject, subject_t} = compute_and_erase_types(case_ast.subject, env)

    {branches, branches_t} =
      Enum.map(case_ast.branches, fn
        # TODO: Pattern matching. Most uses of case will fail.
        %AST.CaseBranch{} = branch ->
          pattern = erase_types(branch.pattern, env)

          {body, body_t} =
            compute_and_erase_type_in_context(
              branch.body,
              [{pattern, subject_t}],
              env
            )

          {{:->, branch.meta, [[pattern], body]}, body_t}
      end)
      |> Enum.unzip()

    type = Type.Union.new(branches_t)

    annotate({:case, case_ast.meta, [subject, [do: branches]]}, type)
  end
end
