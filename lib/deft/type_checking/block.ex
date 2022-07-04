defmodule Deft.TypeChecking.Block do
  import Deft.Helpers

  alias Deft.AST

  def type_check(%AST.Block{} = block, env) do
    {exprs, _ctx, type} =
      Enum.reduce(block.exprs, {[], [], nil}, fn
        # TODO: Only simple assignment so far, no pattern matching
        %AST.Match{} = match, {exprs, ctx, _} ->
          {match, match_t} =
            compute_and_erase_type_in_context(
              match,
              ctx,
              env
            )

          # HACK: Need a better way to propagate bindings
          # from matches. I thought matches could only
          # appear in patterns, and within blocks,
          # but they can appear anywhere. Annotate nodes
          # with the bindings from further down the AST
          # maybe?
          {:=, _, [pattern, _]} = match

          {exprs ++ [match], ctx ++ [{pattern, match_t}], match_t}

        expr, {exprs, ctx, _} ->
          {expr, expr_t} = compute_and_erase_type_in_context(expr, ctx, env)

          {exprs ++ [expr], ctx, expr_t}
      end)

    annotate({:__block__, block.meta, exprs}, type)
  end
end
