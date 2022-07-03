defmodule Deft.Type.Tuple do
  alias Deft.Helpers
  alias Deft.Type

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

  defimpl Deft.Type do
    def subtype_of?(t1, %Type.Tuple{} = t2) do
      elements = Enum.zip(t1.elements, t2.elements)

      Enum.all?(elements, fn {te1, te2} ->
        Helpers.subtype_of?(te1, te2)
      end)
    end

    def subtype_of(_, _) do
      false
    end
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
