defmodule Deft.Type.Forall do
  @moduledoc """
  Universal quantification: forall(a, b). T

  Represents a polymorphic type that abstracts over type variables.
  Can be instantiated by substituting concrete types for variables.

  ## Examples

      # The identity function type: forall a. a -> a
      Type.Forall.new([:a], Type.fun([Type.Var.new(:a)], Type.Var.new(:a)))

      # List head: forall a. [a] -> a
      Type.Forall.new([:a], Type.fun([Type.fixed_list(Type.Var.new(:a))], Type.Var.new(:a)))

  ## Subtyping Rules

  Forall types have special subtyping semantics defined via structural_rule:

  1. **Forall-to-Forall** (same quantifier count):
     `(forall a. T) <: (forall b. S)` iff `T[a := b] <: S`
     Alpha-equivalent forall types are subtypes of each other.

  2. **Different quantifier counts**:
     Not subtypes (a 2-ary polymorphic type is not a subtype of a 1-ary one).

  Note: Cross-type subtyping (forall vs non-forall) is handled in the
  Subtyping module, not via structural_rule.
  """
  use Deft.Subtyping.DSL

  @type t :: %__MODULE__{vars: [atom()], body: Deft.Type.t()}

  @enforce_keys [:vars, :body]
  defstruct @enforce_keys

  @doc """
  Creates a new forall type with the given type variable names and body.
  """
  @spec new([atom()], Deft.Type.t()) :: t()
  def new(vars, body) when is_list(vars) do
    %__MODULE__{vars: vars, body: body}
  end

  @doc """
  Instantiate the forall by substituting concrete types for variables.

  ## How Instantiation Works

  Given `forall(a, b). T` and type arguments `[S1, S2]`:
  1. Create substitution map: `%{a: S1, b: S2}`
  2. Walk the body type T, replacing each `Type.Var{name: a}` with S1, etc.
  3. Return the resulting concrete type

  Uses the Walkable protocol for generic traversal via Substitution module.
  """
  @spec instantiate(t(), [Deft.Type.t()]) :: Deft.Type.t()
  def instantiate(%__MODULE__{vars: vars, body: body}, type_args)
      when length(vars) == length(type_args) do
    subst = Enum.zip(vars, type_args) |> Map.new()
    Deft.Substitution.substitute(body, subst)
  end

  # Custom subtyping for polymorphic types (both are Forall).
  # Check body compatibility with alpha-renaming.
  structural_rule(fn sub, super ->
    if length(sub.vars) == length(super.vars) do
      # Rename sub's vars to match super's for comparison.
      rename = Enum.zip(sub.vars, Enum.map(super.vars, &Deft.Type.Var.new/1)) |> Map.new()
      renamed_body = Deft.Substitution.substitute(sub.body, rename)
      Deft.Subtyping.subtype_of?(super.body, renamed_body)
    else
      false
    end
  end)

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(t, opts) do
      vars = Enum.join(t.vars, ", ")

      concat([
        "forall(",
        vars,
        "). ",
        Inspect.inspect(t.body, opts)
      ])
    end
  end

  defimpl Deft.Walkable do
    def children(node), do: [node.body]
    def rebuild(node, [body]), do: %{node | body: body}
  end
end
