defmodule Deft.Type.Union do
  alias Deft.Subtyping

  @enforce_keys [:types]
  defstruct @enforce_keys

  def new(types) do
    union = %__MODULE__{
      types: MapSet.new()
    }

    Enum.reduce(types, union, &put_type(&2, &1))
  end

  def size(%__MODULE__{} = union) do
    MapSet.size(union.types)
  end

  def types(%__MODULE__{} = union) do
    MapSet.to_list(union.types)
  end

  def put_type(%__MODULE__{} = union, type) do
    # TODO: Very inefficient
    candidate_types =
      case type do
        %__MODULE__{} ->
          types(type)

        _ ->
          [type]
      end

    new_types =
      Enum.reduce(candidate_types, union.types, fn new_type, acc ->
        if Enum.any?(acc, &Subtyping.subtype_of?(&1, new_type)) do
          acc
        else
          acc
          |> Enum.reject(&Subtyping.subtype_of?(new_type, &1))
          |> MapSet.new()
          |> MapSet.put(new_type)
        end
      end)

    %{union | types: new_types}
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
