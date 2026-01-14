defmodule Deft.Type.Top do
  @moduledoc """
  Represents the top type (⊤) in Deft's type system.

  Top is the supertype of all types. Any value can be assigned to a
  variable of type Top. It's useful for representing truly polymorphic
  contexts where any type is acceptable.
  """

  use Deft.Subtyping.DSL

  alias Deft.AST

  @type t :: %__MODULE__{}

  @enforce_keys []
  defstruct @enforce_keys

  def new() do
    %__MODULE__{}
  end

  defimpl AST do
    def to_raw_ast(_type) do
      {:top, [], nil}
    end
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(_t, _opts) do
      string("⊤")
    end
  end

  defimpl Deft.Walkable do
    def children(_node), do: []
    def rebuild(node, []), do: node
  end
end
