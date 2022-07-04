defmodule Deft.AST.Annotation do
  @enforce_keys [:name, :type, :meta]
  defstruct @enforce_keys

  def new(name, type, meta \\ []) do
    %__MODULE__{
      name: name,
      type: type,
      meta: meta
    }
  end
end
