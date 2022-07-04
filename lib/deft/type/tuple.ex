defmodule Deft.Type.Tuple do
  @enforce_keys [:elements]
  defstruct @enforce_keys

  def new(elements) do
    %__MODULE__{
      elements: elements
    }
  end

  def elements(%__MODULE__{} = tuple) do
    tuple.elements
  end

  def unique_types(%__MODULE__{} = tuple) do
    MapSet.new(tuple.elements)
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(t, opts) do
      container_doc("{", t.elements, "}", opts, fn i, _opts -> Inspect.inspect(i, opts) end,
        separator: ","
      )
    end
  end
end
