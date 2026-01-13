defmodule Deft.AST.RemoteCall do
  @moduledoc """
  AST node for module-qualified function calls like String.to_integer(s).
  """

  use Deft.AST.Node, fields: [:module, :function, :args], children: [:args]

  alias Deft.AST

  @type t :: %__MODULE__{
          module: module(),
          function: atom(),
          args: [term()],
          meta: keyword()
        }

  def new(module, function, args, meta \\ []) do
    %__MODULE__{module: module, function: function, args: args, meta: meta}
  end

  defimpl AST do
    def to_raw_ast(node) do
      args = Enum.map(node.args, &@protocol.to_raw_ast/1)
      dot_meta = Keyword.take(node.meta, [:line, :column])

      {{:., dot_meta, [node.module, node.function]}, node.meta, args}
    end
  end
end
