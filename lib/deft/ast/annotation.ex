defmodule Deft.AST.Annotation do
  @enforce_keys [:pattern, :type, :meta]
  defstruct @enforce_keys

  def new(pattern, type, meta \\ []) do
    %__MODULE__{
      pattern: pattern,
      type: type,
      meta: meta
    }
  end
end
