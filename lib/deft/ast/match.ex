defmodule Deft.AST.Match do
  @moduledoc """
  Represents a pattern match expression in the AST.

  Match expressions bind pattern variables to the corresponding
  parts of the matched value. Type checking ensures the pattern
  is compatible with the value's type.
  """

  use Deft.AST.Node, fields: [:pattern, :value], children: [:pattern, :value]

  alias Deft.AST

  def new(pattern, value, meta \\ []) do
    %__MODULE__{pattern: pattern, value: value, meta: meta}
  end

  defimpl AST do
    def to_raw_ast(node) do
      pattern = @protocol.to_raw_ast(node.pattern)
      value = @protocol.to_raw_ast(node.value)

      {:=, node.meta, [pattern, value]}
    end
  end
end
