defmodule Deft.TypeChecking.Block do
  import Deft.Helpers

  alias Deft.AST

  def type_check(%AST.Block{} = block, env) do
    {exprs, _ctx, type} =
      Enum.reduce(block.exprs, {[], [], nil}, fn
        expr, {exprs, ctx, _} ->
          {expr, expr_t, bindings} = compute_and_erase_type_in_context(expr, ctx, env)

          {exprs ++ [expr], ctx ++ bindings, expr_t}
      end)

    annotate_type({:__block__, block.meta, exprs}, type)
  end
end
