defmodule Deft.Type.Boolean do
  @moduledoc """
  Represents the boolean type in Deft's type system.

  Values `true` and `false` have this type. Guard functions like
  `is_integer/1` and comparison operators return booleans.
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
      {:boolean, [], nil}
    end
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(_t, _opts) do
      string("boolean")
    end
  end

  defimpl Deft.Walkable do
    def children(_node), do: []
    def rebuild(node, []), do: node
  end
end
