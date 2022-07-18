defmodule Deft.Type.Intersection do
  alias Deft.AST

  @enforce_keys [:types]
  defstruct @enforce_keys

  def new(types) do
    %__MODULE__{
      types: MapSet.new(types)
    }
  end

  def size(%__MODULE__{} = intersection) do
    MapSet.size(intersection.types)
  end

  def types(%__MODULE__{} = intersection) do
    MapSet.to_list(intersection.types)
  end

  defimpl AST do
    def to_raw_ast(type) do
      types = Enum.map(type.types, &@protocol.to_raw_ast/1)

      {:&, [], types}
    end
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
        separator: " &"
      )
    end
  end
end
