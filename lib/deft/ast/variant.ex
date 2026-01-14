defmodule Deft.AST.Variant do
  @moduledoc """
  Represents a variant definition in an ADT declaration.

  Variants define the constructors of an algebraic data type,
  specifying the name and typed columns. For example, in
  `defdata option(a) :: some(a) | none`, both `some` and `none`
  are variants.
  """

  use Deft.AST.Node, fields: [:name, :adt_name, :columns], children: [:columns]

  alias Deft.AST

  def new(name, adt_name, columns, meta \\ []) do
    %__MODULE__{name: name, adt_name: adt_name, columns: columns, meta: meta}
  end

  defimpl AST do
    def to_raw_ast(node) do
      name = @protocol.to_raw_ast(node.name)
      columns = Enum.map(node.columns, &@protocol.to_raw_ast/1)

      {name, node.meta, columns}
    end
  end
end
