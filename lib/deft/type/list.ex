defmodule Deft.Type.List do
  @moduledoc """
  Represents the generic list type in Deft's type system.

  This is the supertype of all specific list types (FixedList).
  It represents "any list" without specifying the element type.
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
      string("list")
    end
  end

  defimpl Deft.Walkable do
    def children(_node), do: []
    def rebuild(node, []), do: node
  end
end
