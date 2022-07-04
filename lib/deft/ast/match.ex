defmodule Deft.AST.Match do
  @enforce_keys [:pattern, :value, :meta]
  defstruct @enforce_keys

  def new(pattern, value, meta \\ []) do
    %__MODULE__{
      pattern: pattern,
      value: value,
      meta: meta
    }
  end
end
