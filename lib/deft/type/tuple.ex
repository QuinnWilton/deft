defmodule Deft.Type.Tuple do
  @moduledoc """
  Represents the generic tuple type in Deft's type system.

  This is the supertype of all specific tuple types (FixedTuple).
  It represents "any tuple" without specifying the element types or arity.
  """

  use Deft.Subtyping.DSL

  @type t :: %__MODULE__{}

  @enforce_keys []
  defstruct @enforce_keys

  def new() do
    %__MODULE__{}
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(_t, _opts) do
      string("tuple")
    end
  end

  defimpl Deft.Walkable do
    def children(_node), do: []
    def rebuild(node, []), do: node
  end
end
