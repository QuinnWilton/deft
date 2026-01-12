defmodule Deft.AST.List do
  alias Deft.AST

  @enforce_keys [:elements]
  defstruct [:elements, meta: []]

  def new(elements, meta \\ []) do
    %__MODULE__{
      elements: elements,
      meta: meta
    }
  end

  defimpl AST do
    def to_raw_ast(node) do
      Enum.map(node.elements, &@protocol.to_raw_ast/1)
    end
  end
end
