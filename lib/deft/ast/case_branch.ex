defmodule Deft.AST.CaseBranch do
  use Deft.AST.Node, fields: [:pattern, :body], children: [:pattern, :body]

  alias Deft.AST

  def new(pattern, body, meta \\ []) do
    %__MODULE__{pattern: pattern, body: body, meta: meta}
  end

  defimpl AST do
    def to_raw_ast(node) do
      pattern = @protocol.to_raw_ast(node.pattern)
      body = @protocol.to_raw_ast(node.body)

      {:->, node.meta, [[pattern], body]}
    end
  end
end
