defmodule Deft.AST.LocalCall do
  use Deft.AST.Node, fields: [:name, :args], children: [:args]

  alias Deft.AST

  def new(name, args, meta \\ []) do
    %__MODULE__{name: name, args: args, meta: meta}
  end

  defimpl AST do
    def to_raw_ast(node) do
      args = Enum.map(node.args, &@protocol.to_raw_ast/1)

      {node.name, node.meta, args}
    end
  end
end
