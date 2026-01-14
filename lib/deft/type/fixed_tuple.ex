defmodule Deft.Type.FixedTuple do
  @moduledoc """
  Represents a tuple type with known element types in Deft's type system.

  FixedTuple is a subtype of Tuple. It tracks the type of each element
  position. For example, `{integer, boolean}` represents a 2-tuple where
  the first element is an integer and the second is a boolean.

  Subtyping is covariant with arity matching: `{Integer, String}` is a
  subtype of `{Number, Binary}` only if they have the same arity.
  """

  use Deft.Subtyping.DSL

  subtype_of(Deft.Type.Tuple)

  # Covariant with arity check: {Integer, String} <: {Number, Binary}
  # requires matching tuple sizes and element-wise subtyping
  parameter(:elements, variance: :covariant, arity: :must_match)

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

  defimpl Deft.Walkable do
    def children(node), do: [node.elements]
    def rebuild(node, [elements]), do: %{node | elements: elements}
  end
end
