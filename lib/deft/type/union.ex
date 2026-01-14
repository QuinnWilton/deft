defmodule Deft.Type.Union do
  @moduledoc """
  Represents a union type (sum type) in Deft's type system.

  A union type `A | B` represents values that can be either type A or type B.
  Union types are covariant in both components: `Integer | Boolean` is a subtype
  of `Number | Boolean` because `Integer <: Number`.

  Union types are useful for representing values that can take multiple forms,
  such as function return types that may succeed or fail.
  """

  use Deft.Subtyping.DSL

  parameter(:fst, variance: :covariant)
  parameter(:snd, variance: :covariant)

  alias Deft.AST

  @type t :: %__MODULE__{}

  @enforce_keys [:fst, :snd]
  defstruct @enforce_keys

  def new(fst, snd) do
    %__MODULE__{
      fst: fst,
      snd: snd
    }
  end

  defimpl AST do
    def to_raw_ast(type) do
      fst = @protocol.to_raw_ast(type.fst)
      snd = @protocol.to_raw_ast(type.snd)

      {:|, [], [fst, snd]}
    end
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(t, opts) do
      concat([
        Inspect.inspect(t.fst, opts),
        " | ",
        Inspect.inspect(t.snd, opts)
      ])
    end
  end

  defimpl Deft.Walkable do
    def children(node), do: [node.fst, node.snd]
    def rebuild(node, [fst, snd]), do: %{node | fst: fst, snd: snd}
  end
end
