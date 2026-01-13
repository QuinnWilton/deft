defmodule Deft.AST.List do
  use Deft.AST.Node, fields: [:elements], children: [:elements]

  alias Deft.AST

  def new(elements, meta \\ []) do
    %__MODULE__{elements: elements, meta: meta}
  end

  defimpl AST do
    def to_raw_ast(node) do
      Enum.map(node.elements, &@protocol.to_raw_ast/1)
    end
  end
end
