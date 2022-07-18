defmodule Deft.AST.Local do
  alias Deft.AST

  @enforce_keys [:name, :context, :meta]
  defstruct @enforce_keys

  def new(name, context, meta \\ []) do
    %__MODULE__{
      name: name,
      context: context,
      meta: meta
    }
  end

  defimpl AST do
    def to_raw_ast(node) do
      {node.name, node.meta, node.context}
    end
  end
end
