defmodule Deft.AST.List do
  alias Deft.AST

  @enforce_keys [:elements]
  defstruct @enforce_keys

  def new(elements) do
    %__MODULE__{
      elements: elements
    }
  end

  defimpl AST do
    def to_raw_ast(node) do
      Enum.map(node.elements, &@protocol.to_raw_ast/1)
    end
  end
end
