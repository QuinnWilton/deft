defmodule Deft.AST.Match do
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
