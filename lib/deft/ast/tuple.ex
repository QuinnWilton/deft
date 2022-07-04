defmodule Deft.AST.Tuple do
  @enforce_keys [:elements, :meta]
  defstruct @enforce_keys

  def new(elements, meta \\ []) do
    %__MODULE__{
      elements: elements,
      meta: meta
    }
  end
end
