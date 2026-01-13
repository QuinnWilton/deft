defmodule Deft.AST.Literal do
  use Deft.AST.Node, fields: [:value], children: []

  alias Deft.AST

  def new(value, meta \\ []) do
    %__MODULE__{value: value, meta: meta}
  end

  defimpl AST do
    def to_raw_ast(node) do
      node.value
    end
  end
end
