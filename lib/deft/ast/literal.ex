defmodule Deft.AST.Literal do
  @moduledoc """
  Represents a literal value in the AST.

  Literals are primitive values like integers, floats, booleans,
  atoms, and strings. Their types are inferred directly from
  the value.
  """

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
