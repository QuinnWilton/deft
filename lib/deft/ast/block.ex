defmodule Deft.AST.Block do
  @enforce_keys [:exprs, :meta]
  defstruct @enforce_keys

  def new(exprs, meta \\ []) do
    %__MODULE__{
      exprs: exprs,
      meta: meta
    }
  end
end
