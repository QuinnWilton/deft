defmodule Deft.Subtyping.Lattice do
  @moduledoc """
  The subtyping lattice built from type module metadata.

  Provides introspection APIs for querying the lattice structure.
  """

  alias Deft.Type

  # All type modules participating in the lattice.
  @type_modules [
    Type.Top,
    Type.Bottom,
    Type.Number,
    Type.Integer,
    Type.Float,
    Type.Boolean,
    Type.Atom,
    Type.Tuple,
    Type.FixedTuple,
    Type.List,
    Type.FixedList,
    Type.Fn,
    Type.Union,
    Type.Intersection,
    Type.ADT,
    Type.Variant,
    Type.Alias
  ]

  @doc """
  Returns all type modules in the lattice.
  """
  @spec type_modules() :: [module()]
  def type_modules, do: @type_modules

  @doc """
  Returns all declared edges in the lattice as `{subtype, supertype}` tuples.

  This only includes direct declarations via `subtype_of`, not transitive
  or structural relationships.
  """
  @spec edges() :: [{module(), module()}]
  def edges do
    for mod <- @type_modules,
        has_metadata?(mod),
        supertype <- mod.__subtyping_metadata__().supertypes do
      {mod, supertype}
    end
  end

  defp has_metadata?(mod) do
    Code.ensure_loaded!(mod)
    function_exported?(mod, :__subtyping_metadata__, 0)
  end

  @doc """
  Returns the immediate declared supertypes of a type module.
  """
  @spec immediate_supertypes(module()) :: [module()]
  def immediate_supertypes(type_module) do
    if has_metadata?(type_module) do
      type_module.__subtyping_metadata__().supertypes
    else
      []
    end
  end

  @doc """
  Returns the immediate declared subtypes of a type module.
  """
  @spec immediate_subtypes(module()) :: [module()]
  def immediate_subtypes(type_module) do
    for mod <- @type_modules,
        has_metadata?(mod),
        type_module in mod.__subtyping_metadata__().supertypes do
      mod
    end
  end

  @doc """
  Returns all supertypes of a type module (transitive closure).

  Includes Top for all types.
  """
  @spec supertypes_of(module()) :: [module()]
  def supertypes_of(type_module) do
    direct = immediate_supertypes(type_module)
    transitive = Enum.flat_map(direct, &supertypes_of/1)
    result = Enum.uniq(direct ++ transitive)

    # Top is supertype of everything except itself
    if type_module != Type.Top and Type.Top not in result do
      result ++ [Type.Top]
    else
      result
    end
  end

  @doc """
  Returns all subtypes of a type module (transitive closure).

  Includes Bottom for all types.
  """
  @spec subtypes_of(module()) :: [module()]
  def subtypes_of(type_module) do
    direct = immediate_subtypes(type_module)
    transitive = Enum.flat_map(direct, &subtypes_of/1)
    result = Enum.uniq(direct ++ transitive)

    # Bottom is subtype of everything except itself
    if type_module != Type.Bottom and Type.Bottom not in result do
      result ++ [Type.Bottom]
    else
      result
    end
  end

  @doc """
  Returns variance metadata for a type module's parameters.

  Returns a keyword list of `{parameter_name, variance}`.
  """
  @spec variance_of(module()) :: keyword(Deft.Subtyping.Variance.t())
  def variance_of(type_module) do
    if has_metadata?(type_module) do
      type_module.__subtyping_metadata__().parameters
    else
      []
    end
  end

  @doc """
  Returns true if the type module has a structural subtyping rule.
  """
  @spec has_structural_rule?(module()) :: boolean()
  def has_structural_rule?(type_module) do
    if has_metadata?(type_module) do
      type_module.__subtyping_metadata__().has_structural_rule?
    else
      false
    end
  end

  @doc """
  Returns a map representation of the full lattice for introspection.
  """
  @spec to_map() :: map()
  def to_map do
    %{
      type_modules: @type_modules,
      edges: edges(),
      supertypes:
        Map.new(@type_modules, fn mod ->
          {mod, supertypes_of(mod)}
        end),
      subtypes:
        Map.new(@type_modules, fn mod ->
          {mod, subtypes_of(mod)}
        end),
      variance:
        Map.new(@type_modules, fn mod ->
          {mod, variance_of(mod)}
        end)
    }
  end
end
