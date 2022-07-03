defmodule Deft.Type.Union do
  @enforce_keys [:elements]
  defstruct @enforce_keys

  def new(elements) do
    %__MODULE__{
      elements: MapSet.new(elements)
    }
  end

  def put_type(%__MODULE__{} = union, type) do
    case type do
      %__MODULE__{} ->
        %{union | elements: MapSet.union(union.elements, type.elements)}

      _ ->
        %{union | elements: MapSet.put(union.elements, type)}
    end
  end

  defimpl Deft.Type do
    def subtype_of?(t1, t2) do
      Enum.any?(t1.elements, fn te1 ->
        Deft.Type.subtype_of?(te1, t2)
      end)
    end
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(t, opts) do
      container_doc(
        "",
        Enum.to_list(t.elements),
        "",
        opts,
        fn i, _opts -> Inspect.inspect(i, opts) end,
        separator: " |"
      )
    end
  end
end
