defmodule Deft.Type.Number do
  @moduledoc """
  Represents the number type in Deft's type system.

  Number is the supertype of both Integer and Float. It represents any
  numeric value and is used for arithmetic operations that work on both.
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
      {:number, [], nil}
    end
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(_t, _opts) do
      string("number")
    end
  end

  defimpl Deft.Walkable do
    def children(_node), do: []
    def rebuild(node, []), do: node
  end
end
