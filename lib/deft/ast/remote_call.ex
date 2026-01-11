defmodule Deft.AST.RemoteCall do
  @moduledoc """
  AST node for module-qualified function calls like String.to_integer(s).
  """

  alias Deft.AST
  alias Deft.Walkable

  @type t :: %__MODULE__{
          module: module(),
          function: atom(),
          args: [term()],
          meta: keyword()
        }

  @enforce_keys [:module, :function, :args, :meta]
  defstruct @enforce_keys

  def new(module, function, args, meta \\ []) do
    %__MODULE__{
      module: module,
      function: function,
      args: args,
      meta: meta
    }
  end

  defimpl AST do
    def to_raw_ast(node) do
      args = Enum.map(node.args, &@protocol.to_raw_ast/1)
      dot_meta = Keyword.take(node.meta, [:line, :column])

      {{:., dot_meta, [node.module, node.function]}, node.meta, args}
    end
  end

  defimpl Walkable do
    def children(node) do
      [node.args]
    end

    def rebuild(node, [new_args]) do
      %{node | args: new_args}
    end
  end
end
