defmodule Deft.Type.Var do
  @moduledoc """
  Represents a type variable in polymorphic types.

  Type variables are placeholders that get bound to concrete types
  during instantiation. They are identified by name (atom).

  ## Examples

      Type.Var.new(:a)  # The type variable 'a'

  ## Subtyping

  Type variables use name-based equality for subtyping:
  - `a <: a` (reflexive)
  - `a </: b` (different vars are not subtypes)

  During instantiation, variables are substituted with concrete types,
  so these rules only apply to uninstantiated (free) variables.

  The invariant parameter on :name auto-generates the equality check.
  """
  use Deft.Subtyping.DSL

  # Invariant over name means exact equality required: a <: a, but a </: b
  parameter(:name, variance: :invariant)

  @type t :: %__MODULE__{name: atom()}

  @enforce_keys [:name]
  defstruct @enforce_keys

  @doc """
  Creates a new type variable with the given name.
  """
  @spec new(atom()) :: t()
  def new(name) when is_atom(name), do: %__MODULE__{name: name}

  defimpl Inspect do
    def inspect(t, _opts), do: Atom.to_string(t.name)
  end

  defimpl Deft.Walkable do
    def children(_node), do: []
    def rebuild(node, []), do: node
  end
end
