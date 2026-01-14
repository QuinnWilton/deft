defmodule Deft.Type.FixedList do
  @moduledoc """
  Represents a homogeneous list type in Deft's type system.

  FixedList is a subtype of List. It specifies that all elements have
  the same type. For example, `[integer]` represents a list where every
  element is an integer.

  Subtyping is covariant: `[Integer]` is a subtype of `[Number]`.
  """

  use Deft.Subtyping.DSL

  subtype_of(Deft.Type.List)

  # Covariant: [Integer] <: [Number] because Integer <: Number
  parameter(:contents, variance: :covariant)

  alias Deft.AST

  @type t :: %__MODULE__{}

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

  defimpl AST do
    def to_raw_ast(type) do
      contents_ast = @protocol.to_raw_ast(type.contents)
      [contents_ast]
    end
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

  defimpl Deft.Walkable do
    def children(node), do: [node.contents]
    def rebuild(node, [contents]), do: %{node | contents: contents}
  end
end
