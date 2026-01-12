defmodule Deft.Generators.Types do
  @moduledoc """
  StreamData generators for Deft types.

  Supports depth-bounded recursive generation to produce nested compound types
  (e.g., functions returning functions, tuples of tuples) while avoiding
  infinite recursion.
  """

  import StreamData

  alias Deft.Type

  @default_depth 2
  @max_length 4

  # ============================================================================
  # Public API
  # ============================================================================

  @doc """
  Generates any well-formed type up to the given depth.

  Depth controls how deeply compound types can nest:
  - depth 0: only primitives and top/bottom
  - depth 1: compound types with primitive components
  - depth 2+: compound types with compound components
  """
  def type(depth \\ @default_depth) do
    if depth <= 0 do
      one_of([top_type(), primitive_type()])
    else
      one_of([
        top_type(),
        primitive_type(),
        compound_type(depth)
      ])
    end
  end

  @doc """
  Generates primitive types (non-compound, non-lattice).
  """
  def primitive_type do
    one_of([
      atom_type(),
      binary_type(),
      boolean_type(),
      float_type(),
      integer_type(),
      number_type()
    ])
  end

  @doc """
  Generates compound types up to the given depth.

  At depth 1, uses primitives as components.
  At depth 2+, allows nested compound types.
  """
  def compound_type(depth \\ @default_depth) do
    component_depth = max(0, depth - 1)

    one_of([
      fn_type(component_depth),
      fixed_tuple_type(component_depth),
      fixed_list_type(component_depth),
      union_type(component_depth),
      intersection_type(component_depth)
    ])
  end

  # ============================================================================
  # Primitive Type Generators
  # ============================================================================

  def atom_type do
    constant(Type.atom())
  end

  def binary_type do
    constant(Type.binary())
  end

  def boolean_type do
    constant(Type.boolean())
  end

  def bottom_type do
    constant(Type.bottom())
  end

  def float_type do
    constant(Type.float())
  end

  def integer_type do
    constant(Type.integer())
  end

  def number_type do
    constant(Type.number())
  end

  def top_type do
    constant(Type.top())
  end

  # ============================================================================
  # Compound Type Generators
  # ============================================================================

  @doc """
  Generates function types with the given component depth.
  """
  def fn_type(depth \\ 0) do
    component_gen = type(depth)

    bind(list_of(component_gen, max_length: @max_length), fn inputs ->
      map(component_gen, fn output ->
        Type.fun(inputs, output)
      end)
    end)
  end

  @doc """
  Generates fixed tuple types with the given component depth.
  """
  def fixed_tuple_type(depth \\ 0) do
    component_gen = type(depth)

    bind(list_of(component_gen, max_length: @max_length), fn elements ->
      constant(Type.fixed_tuple(elements))
    end)
  end

  @doc """
  Generates fixed list types with the given component depth.
  """
  def fixed_list_type(depth \\ 0) do
    component_gen = type(depth)

    map(component_gen, fn element_type ->
      Type.fixed_list(element_type)
    end)
  end

  @doc """
  Generates union types with the given component depth.
  """
  def union_type(depth \\ 0) do
    component_gen = type(depth)

    bind(list_of(component_gen, min_length: 2, max_length: @max_length), fn elements ->
      elements
      |> Enum.reduce(Type.bottom(), &Type.union/2)
      |> constant()
    end)
  end

  @doc """
  Generates intersection types with the given component depth.
  Filters out uninhabited intersections (those that simplify to Bottom).
  """
  def intersection_type(depth \\ 0) do
    component_gen = type(depth)

    bind(list_of(component_gen, min_length: 2, max_length: @max_length), fn elements ->
      result =
        elements
        |> Enum.reduce(Type.top(), &Type.intersection/2)

      # Filter out Bottom (uninhabited intersections).
      case result do
        %Type.Bottom{} -> constant(Type.top())
        _ -> constant(result)
      end
    end)
  end
end
