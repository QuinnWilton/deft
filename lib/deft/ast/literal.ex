defmodule Deft.AST.Literal do
  @enforce_keys [:value, :meta]
  defstruct @enforce_keys

  def new(value, meta \\ []) do
    %__MODULE__{
      value: value,
      meta: meta
    }
  end
end
