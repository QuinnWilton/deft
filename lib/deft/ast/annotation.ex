defmodule Deft.AST.Annotation do
  alias Deft.AST

  @enforce_keys [:pattern, :type, :meta]
  defstruct @enforce_keys

  def new(pattern, type, meta \\ []) do
    %__MODULE__{
      pattern: pattern,
      type: type,
      meta: meta
    }
  end

  defimpl AST do
    def to_raw_ast(node) do
      pattern = @protocol.to_raw_ast(node.pattern)
      type = @protocol.to_raw_ast(node.type)

      {:"::", node.meta, [pattern, type]}
    end
  end
end
