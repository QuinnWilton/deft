defmodule Deft.TypeChecking.Block do
  import Deft.Helpers

  alias Deft.AST
  alias Deft.Type

  def type_check(%AST.Block{} = block, env, opts) do
    {exprs, _ctx, type} =
      Enum.reduce(block.exprs, {[], [], nil}, fn
        %AST.DefData{} = node, {exprs, ctx, prev_type} ->
          type =
            node
            |> inject_bindings(ctx, env, opts)
            |> handle_def_data(env, opts)

          adt_bindings =
            Enum.reduce(type.variants, [{:adt, node.name, type}], fn
              %Type.Variant{} = variant, acc ->
                acc ++
                  [
                    {:adt_variant, variant.name, type, variant}
                  ]
            end)

          {exprs, ctx ++ adt_bindings, prev_type}

        expr, {exprs, ctx, _} ->
          {expr, expr_t, bindings} = compute_and_erase_type_in_context(expr, ctx, env, opts)

          {exprs ++ [expr], ctx ++ bindings, expr_t}
      end)

    annotate_type({:__block__, block.meta, exprs}, type)
  end

  def handle_def_data(%AST.DefData{} = node, _env, _opts) do
    variants =
      Enum.map(node.variants, fn
        %AST.Variant{} = variant ->
          Type.variant(variant.name, node.name, variant.columns)
      end)

    Type.adt(node.name, variants)
  end
end
