defmodule Deft.Type.Alias do
  @type t :: %__MODULE__{}

  @enforce_keys [:name, :context]
  defstruct @enforce_keys

  def new(name, context) do
    %__MODULE__{
      name: name,
      context: context
    }
  end

  defimpl Inspect do
    def inspect(t, _opts) do
      Atom.to_string(t.name)
    end
  end
end
