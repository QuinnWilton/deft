defmodule Deft.AST.DefData do
  @moduledoc """
  AST node for algebraic data type definitions.

  ## Fields

  - `name` - The ADT name (as AST.Local)
  - `variants` - List of AST.Variant nodes
  - `params` - List of type parameter names (atoms), empty for monomorphic ADTs
  - `def_meta` - Metadata from the defdata macro call
  - `variants_meta` - Metadata from the :: operator
  """

  use Deft.AST.Node,
    fields: [:name, :variants, :params, :def_meta, :variants_meta],
    children: [:variants],
    no_meta: true

  alias Deft.AST

  @doc """
  Creates a new DefData AST node.

  ## Parameters

  - `name` - The ADT name (AST.Local)
  - `variants` - List of AST.Variant nodes
  - `def_meta` - Metadata from defdata macro
  - `variants_meta` - Metadata from :: operator
  - `params` - Optional list of type parameter names (default: [])
  """
  def new(name, variants, def_meta \\ [], variants_meta \\ [], params \\ [])
      when is_list(variants) and length(variants) > 0 do
    %__MODULE__{
      name: name,
      variants: variants,
      params: params,
      def_meta: def_meta,
      variants_meta: variants_meta
    }
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
