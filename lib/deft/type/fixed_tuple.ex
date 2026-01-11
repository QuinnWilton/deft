defmodule Deft.Type.FixedTuple do
  use Deft.Subtyping.DSL

  subtype_of(Deft.Type.Tuple)

  parameter(:elements, variance: :covariant)

  structural_rule(fn sub, super ->
    length(sub.elements) == length(super.elements) and
      Enum.zip(sub.elements, super.elements)
      |> Enum.all?(fn {sub_elem, super_elem} ->
        Deft.Subtyping.subtype_of?(super_elem, sub_elem)
      end)
  end)

  alias Deft.AST

  @type t :: %__MODULE__{}

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

  defimpl AST do
    def to_raw_ast(type) do
      elements = Enum.map(type.elements, &@protocol.to_raw_ast/1)

      {:{}, [], elements}
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
