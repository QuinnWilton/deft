defmodule Deft.AST.CaseBranch do
  alias Deft.AST

  @enforce_keys [:pattern, :body, :meta]
  defstruct @enforce_keys

  def new(pattern, body, meta \\ []) do
    %__MODULE__{
      pattern: pattern,
      body: body,
      meta: meta
    }
  end

  defimpl AST do
    def to_raw_ast(node) do
      pattern = @protocol.to_raw_ast(node.pattern)
      body = @protocol.to_raw_ast(node.body)

      {:->, node.meta, [[pattern], body]}
    end
  end
end
