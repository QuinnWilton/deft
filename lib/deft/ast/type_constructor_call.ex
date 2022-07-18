defmodule Deft.AST.TypeConstructorCall do
  alias Deft.AST

  @enforce_keys [
    :name,
    :args,
    :type,
    :variant,
    :meta
  ]
  defstruct @enforce_keys

  def new(name, args, type, variant, meta \\ []) when is_list(args) do
    %__MODULE__{
      name: name,
      args: args,
      type: type,
      variant: variant,
      meta: meta
    }
  end

  defimpl AST do
    def to_raw_ast(node) do
      args = Enum.map(node.args, &@protocol.to_raw_ast/1)

      {node.name, [], args}
    end
  end
end
