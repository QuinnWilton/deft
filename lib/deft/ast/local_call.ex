defmodule Deft.AST.LocalCall do
  @enforce_keys [:name, :args, :meta]
  defstruct @enforce_keys

  def new(name, args, meta \\ []) do
    %__MODULE__{
      name: name,
      args: args,
      meta: meta
    }
  end
end
