defmodule Deft.Type.Union do
  @enforce_keys [:types]
  defstruct @enforce_keys

  def new(types) do
    types =
      Enum.flat_map(types, fn
        %__MODULE__{} = union ->
          types(union)

        type ->
          [type]
      end)

    %__MODULE__{
      types: MapSet.new(types)
    }
  end

  def size(%__MODULE__{} = union) do
    MapSet.size(union.types)
  end

  def types(%__MODULE__{} = union) do
    MapSet.to_list(union.types)
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(t, opts) do
      container_doc(
        "",
        @for.types(t),
        "",
        opts,
        fn i, _opts -> Inspect.inspect(i, opts) end,
        separator: " |"
      )
    end
  end
end
