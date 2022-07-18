defmodule Deft.AST.FnApplication do
  alias Deft.AST

  @enforce_keys [:fun, :fun_meta, :args, :args_meta]
  defstruct @enforce_keys

  def new(fun, args, fun_meta \\ [], args_meta \\ []) do
    %__MODULE__{
      fun: fun,
      args: args,
      fun_meta: fun_meta,
      args_meta: args_meta
    }
  end

  defimpl AST do
    def to_raw_ast(node) do
      fun = @protocol.to_raw_ast(node.fun)
      args = Enum.map(node.args, &@protocol.to_raw_ast/1)

      {{:., node.fun_meta, [fun]}, node.args_meta, args}
    end
  end
end
