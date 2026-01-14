defmodule Deft.AST.List do
  @moduledoc """
  Represents a list literal in the AST.

  List literals contain zero or more elements. Type checking infers
  a FixedList type with the unified element type.
  """

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
