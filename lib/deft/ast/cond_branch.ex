defmodule Deft.AST.CondBranch do
  use Deft.AST.Node, fields: [:predicate, :body], children: [:predicate, :body]

  alias Deft.AST

  def new(predicate, body, meta \\ []) do
    %__MODULE__{predicate: predicate, body: body, meta: meta}
  end

  defimpl AST do
    def to_raw_ast(node) do
      predicate = @protocol.to_raw_ast(node.predicate)
      body = @protocol.to_raw_ast(node.body)

      {:->, node.meta, [[predicate], body]}
    end
  end
end
