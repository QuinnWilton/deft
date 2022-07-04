defmodule Deft.AST.Cond do
  @enforce_keys [:branches, :meta]
  defstruct @enforce_keys

  def new(branches, meta \\ []) do
    %__MODULE__{
      branches: branches,
      meta: meta
    }
  end
end
