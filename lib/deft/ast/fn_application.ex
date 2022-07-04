defmodule Deft.AST.FnApplication do
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
end
