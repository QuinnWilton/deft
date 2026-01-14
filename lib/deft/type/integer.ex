defmodule Deft.Type.Integer do
  @moduledoc """
  Represents the integer type in Deft's type system.

  Integer is a subtype of Number. Integer literals like `42` have this type.
  """

  use Deft.Subtyping.DSL

  subtype_of(Deft.Type.Number)

  alias Deft.AST

  @type t :: %__MODULE__{}

  @enforce_keys []
  defstruct @enforce_keys

  def new() do
    %__MODULE__{}
  end

  defimpl AST do
    def to_raw_ast(_type) do
      {:integer, [], nil}
    end
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(_t, _opts) do
      string("integer")
    end
  end

  defimpl Deft.Walkable do
    def children(_node), do: []
    def rebuild(node, []), do: node
  end
end
