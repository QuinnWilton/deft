defmodule Deft.TypeChecking.Cond do
  import Deft.Helpers

  alias Deft.AST
  alias Deft.Subtyping
  alias Deft.Type

  def type_check(%AST.Cond{} = cond_ast, env, opts) do
    {branches, branch_ts} =
      Enum.map(cond_ast.branches, fn
        %AST.CondBranch{} = branch ->
          {predicate, predicate_t, _} = compute_and_erase_types(branch.predicate, env, opts)
          {body, body_t, _} = compute_and_erase_types(branch.body, env, opts)

          unless Subtyping.subtype_of?(Type.boolean(), predicate_t) do
            raise Deft.TypecheckingError, expected: Type.boolean(), actual: predicate_t
          end

          {{:->, branch.meta, [[predicate], body]}, body_t}
      end)
      |> Enum.unzip()

    type = Enum.reduce(branch_ts, &Type.union/2)

    {:cond, cond_ast.meta, [[do: branches]]}
    |> annotate_type(type)
  end
end
