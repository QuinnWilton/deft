defmodule Deft.AST.Local do
  @enforce_keys [:name, :context, :meta]
  defstruct @enforce_keys

  def new(name, context, meta \\ []) do
    %__MODULE__{
      name: name,
      context: context,
      meta: meta
    }
  end
end
