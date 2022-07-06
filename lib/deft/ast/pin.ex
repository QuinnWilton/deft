defmodule Deft.AST.Pin do
  @enforce_keys [:expr, :meta]
  defstruct @enforce_keys

  def new(expr, meta \\ []) do
    %__MODULE__{
      expr: expr,
      meta: meta
    }
  end
end
