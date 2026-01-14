defmodule Deft.Type.Bottom do
  @moduledoc """
  Represents the bottom type (⊥) in Deft's type system.

  Bottom is the subtype of all types. It represents an uninhabited type -
  no values have this type. It appears as the result type of functions
  that never return (e.g., functions that always raise).
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
      {:bottom, [], nil}
    end
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(_t, _opts) do
      string("⊥")
    end
  end

  defimpl Deft.Walkable do
    def children(_node), do: []
    def rebuild(node, []), do: node
  end
end
