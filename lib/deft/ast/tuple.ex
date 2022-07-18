defmodule Deft.AST.Tuple do
  alias Deft.AST

  @enforce_keys [:elements, :meta]
  defstruct @enforce_keys

  def new(elements, meta \\ []) do
    %__MODULE__{
      elements: elements,
      meta: meta
    }
  end

  defimpl AST do
    def to_raw_ast(node) do
      elements = Enum.map(node.elements, &@protocol.to_raw_ast/1)

      {:{}, node.meta, elements}
    end
  end
end
