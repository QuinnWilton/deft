defmodule Deft.Substitution do
  @moduledoc """
  Type variable substitution using the Walkable protocol.

  ## What is Substitution?

  Substitution is the process of replacing type variables with concrete types.
  Given a substitution map `%{a: integer, b: boolean}` and a type `(a, b) -> a`,
  substitution produces `(integer, boolean) -> integer`.

  ## How It Works

  1. We have a substitution map: `%{var_name => concrete_type}`
  2. We traverse the type tree using `Walker.postwalk/2`
  3. At each node:
     - If it's a `Type.Var` whose name is in the map, replace it
     - Otherwise, the Walkable protocol handles recursion automatically

  The Walkable protocol (already implemented for all types) provides:
  - `children/1`: Returns child types to recurse into
  - `rebuild/2`: Reconstructs the type with transformed children

  This means we don't need to pattern match on every type constructor -
  the protocol handles the traversal, we just handle the transformation.

  ## Example

      subst = %{a: Type.integer(), b: Type.boolean()}
      type = Type.fun([Type.Var.new(:a)], Type.Var.new(:b))

      Substitution.substitute(type, subst)
      # => Type.fun([Type.integer()], Type.boolean())
  """

  alias Deft.Walker
  alias Deft.Type

  @type substitution :: %{atom() => Type.t()}

  @doc """
  Substitute type variables in a type according to the given mapping.

  Uses Walker.postwalk to traverse the type tree. The Walkable protocol
  implementations handle recursion into child types automatically.
  """
  @spec substitute(Type.t(), substitution()) :: Type.t()
  def substitute(type, subst) when map_size(subst) == 0, do: type

  def substitute(type, subst) do
    Walker.postwalk(type, fn
      # Replace type variables that are in the substitution.
      %Type.Var{name: name} = var ->
        Map.get(subst, name, var)

      # All other types pass through unchanged.
      # Their children were already transformed by postwalk.
      other ->
        other
    end)
  end
end
