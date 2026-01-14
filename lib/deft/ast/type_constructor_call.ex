defmodule Deft.AST.TypeConstructorCall do
  @moduledoc """
  Represents an ADT variant constructor call in the AST.

  TypeConstructorCall is created when a LocalCall in pattern position
  matches an ADT variant name. It carries the resolved type and variant
  information for type checking.
  """

  use Deft.AST.Node, fields: [:name, :args, :type, :variant], children: [:args]

  alias Deft.AST

  def new(name, args, type, variant, meta \\ []) when is_list(args) do
    %__MODULE__{name: name, args: args, type: type, variant: variant, meta: meta}
  end

  defimpl AST do
    def to_raw_ast(node) do
      args = Enum.map(node.args, &@protocol.to_raw_ast/1)

      {node.name, [], args}
    end
  end
end
