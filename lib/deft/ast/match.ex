defmodule Deft.AST.Match do
  alias Deft.AST

  @enforce_keys [:pattern, :value, :meta]
  defstruct @enforce_keys

  def new(pattern, value, meta \\ []) do
    %__MODULE__{
      pattern: pattern,
      value: value,
      meta: meta
    }
  end

  defimpl AST do
    def to_raw_ast(node) do
      pattern = @protocol.to_raw_ast(node.pattern)
      value = @protocol.to_raw_ast(node.value)

      {:=, node.meta, [pattern, value]}
    end
  end
end
