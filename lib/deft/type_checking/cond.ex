defmodule Deft.TypeChecking.Cond do
  import Deft.TypeChecking

  alias Deft.AST
  alias Deft.Subtyping
  alias Deft.Type

  def type_check(%AST.Cond{} = cond_ast, env) do
    {branches, branch_ts} =
      Enum.map(cond_ast.branches, fn
        %AST.CondBranch{} = branch ->
          {predicate, predicate_t} = compute_and_erase_types(branch.predicate, env)
          {body, body_t} = compute_and_erase_types(branch.body, env)

          unless Subtyping.subtype_of?(Type.boolean(), predicate_t) do
            raise Deft.TypecheckingError, expected: Type.boolean(), actual: predicate_t
          end

          {{:->, branch.meta, [[predicate], body]}, body_t}
      end)
      |> Enum.unzip()

    type = Type.Union.new(branch_ts)

    annotate({:cond, cond_ast.meta, [[do: branches]]}, type)
  end
end
