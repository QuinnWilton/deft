defmodule Deft.AST.Cons do
  use Deft.AST.Node, fields: [:head, :rest], children: [:head, :rest]

  alias Deft.AST

  def new(head, rest, meta \\ []) do
    %__MODULE__{head: head, rest: rest, meta: meta}
  end

  defimpl AST do
    def to_raw_ast(node) do
      head = @protocol.to_raw_ast(node.head)
      rest = @protocol.to_raw_ast(node.rest)

      {:|, node.meta, [head, rest]}
    end
  end
end
