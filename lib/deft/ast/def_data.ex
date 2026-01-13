defmodule Deft.AST.DefData do
  use Deft.AST.Node,
    fields: [:name, :variants, :def_meta, :variants_meta],
    children: [:variants],
    no_meta: true

  alias Deft.AST

  def new(name, variants, def_meta \\ [], variants_meta \\ [])
      when is_list(variants) and length(variants) > 0 do
    %__MODULE__{name: name, variants: variants, def_meta: def_meta, variants_meta: variants_meta}
  end

  defimpl AST do
    def to_raw_ast(node) do
      name = @protocol.to_raw_ast(node.name)

      variants =
        Enum.reduce(node.variants, fn
          %AST.Variant{} = variant, acc ->
            variant = @protocol.to_raw_ast(variant)

            {:|, [], [acc, variant]}
        end)

      {:defdata, node.def_meta, {:"::", node.variants_meta, [name, variants]}}
    end
  end
end
