defmodule Deft.AST.Local do
  use Deft.AST.Node, fields: [:name, :context], children: []

  alias Deft.AST

  @type t :: %__MODULE__{}

  def new(name, context, meta \\ []) do
    %__MODULE__{name: name, context: context, meta: meta}
  end

  defimpl AST do
    def to_raw_ast(node) do
      {node.name, node.meta, node.context}
    end
  end
end
