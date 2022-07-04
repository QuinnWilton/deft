defmodule Deft.TypeChecking.Block do
  import Deft.TypeChecking

  alias Deft.AST

  def type_check(%AST.Block{} = block, env) do
    {exprs, _ctx, type} =
      Enum.reduce(block.exprs, {[], [], nil}, fn
        # TODO: Only simple assignment so far, no pattern matching
        %AST.Match{} = match, {exprs, ctx, _} ->
          pattern = erase_types(match.pattern, env)

          {value, value_t} =
            compute_and_erase_type_in_context(
              match.value,
              ctx,
              env
            )

          expr = {:=, match.meta, [pattern, value]}

          {exprs ++ [expr], ctx ++ [{pattern, value_t}], value_t}

        expr, {exprs, ctx, _} ->
          {expr, expr_t} = compute_and_erase_type_in_context(expr, ctx, env)

          {exprs ++ [expr], ctx, expr_t}
      end)

    annotate({:__block__, block.meta, exprs}, type)
  end
end
