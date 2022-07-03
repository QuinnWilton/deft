defmodule Deft.Type.List do
  alias Deft.Helpers
  alias Deft.Type

  @enforce_keys [:contents]
  defstruct @enforce_keys

  def new(contents) do
    %__MODULE__{
      contents: contents
    }
  end

  def contents(%__MODULE__{} = list) do
    list.contents
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(t, opts) do
      concat([
        string("["),
        Inspect.inspect(t.contents, opts),
        string("]")
      ])
    end
  end
end
