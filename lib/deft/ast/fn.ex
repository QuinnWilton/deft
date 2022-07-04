defmodule Deft.AST.Fn do
  @enforce_keys [:body, :args, :fn_meta, :arrow_meta]
  defstruct @enforce_keys

  def new(body, args, fn_meta \\ [], arrow_meta \\ []) do
    %__MODULE__{
      body: body,
      args: args,
      fn_meta: fn_meta,
      arrow_meta: arrow_meta
    }
  end
end
