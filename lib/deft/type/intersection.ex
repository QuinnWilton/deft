defmodule Deft.Type.Intersection do
  @moduledoc """
  Represents an intersection type (product type) in Deft's type system.

  An intersection type `A & B` represents values that satisfy both type A and
  type B simultaneously. Unlike union types, intersection types are relatively
  rare in practice but useful for expressing constraints.

  The `new/2` constructor automatically normalizes intersection types by:
  1. Flattening nested intersections
  2. Detecting uninhabited intersections (disjoint types) and simplifying to Bottom
  3. Removing redundant components (if A <: B, keep only A)
  4. Sorting for canonical ordering

  For example, `Integer & Number` normalizes to just `Integer` since Integer is
  already a subtype of Number. And `Integer & Boolean` normalizes to Bottom since
  no value can be both an integer and a boolean.
  """

  use Deft.Subtyping.DSL

  parameter(:fst, variance: :covariant)
  parameter(:snd, variance: :covariant)

  alias Deft.AST

  @type t :: %__MODULE__{}

  @enforce_keys [:fst, :snd]
  defstruct @enforce_keys

  @doc """
  Creates a new intersection type with canonically normalized components.

  Normalization includes:
  1. Flattening nested intersections
  2. Detecting uninhabited intersections (disjoint types) -> simplify to Bottom
  3. Removing redundant components (if A <: B, keep only A)
  4. Sorting for canonical ordering (ensures commutativity/associativity)
  """
  def new(fst, snd) do
    components = flatten(fst) ++ flatten(snd)
    unique = Enum.uniq(components)

    # Check for disjoint types - if any two components have no overlap, the
    # intersection is uninhabited and simplifies to Bottom.
    if has_disjoint_pair?(unique) do
      Deft.Type.Bottom.new()
    else
      minimal = remove_redundant(unique)
      sorted = Enum.sort(minimal, &type_compare/2)

      case sorted do
        [] -> raise ArgumentError, "intersection must have at least one component"
        [single] -> single
        [first | rest] -> Enum.reduce(rest, first, fn t, acc -> %__MODULE__{fst: acc, snd: t} end)
      end
    end
  end

  # Flatten nested intersections into a list of non-intersection components.
  defp flatten(%__MODULE__{fst: fst, snd: snd}) do
    flatten(fst) ++ flatten(snd)
  end

  defp flatten(type), do: [type]

  # Check if any two components in the list are disjoint (have no overlap).
  defp has_disjoint_pair?(components) do
    pairs = for a <- components, b <- components, a != b, do: {a, b}
    Enum.any?(pairs, fn {a, b} -> disjoint?(a, b) end)
  end

  # Two types are disjoint if they have no values in common.
  # This is a conservative check - returns true only for known disjoint pairs.
  defp disjoint?(a, b) do
    a_group = type_group(a)
    b_group = type_group(b)

    cond do
      # Functions with different arities are disjoint
      a_group == :function and b_group == :function ->
        if length(a.inputs) != length(b.inputs) do
          true
        else
          # Same arity - check if parameter types are disjoint
          same_group_disjoint?(:function, a, b)
        end

      # Tuples with different arities are disjoint
      a_group == :tuple and b_group == :tuple ->
        if tuple_arity(a) != tuple_arity(b) do
          true
        else
          # Same arity - check if element types are disjoint
          same_group_disjoint?(:tuple, a, b)
        end

      # Same group - need to check component types recursively
      a_group == b_group ->
        same_group_disjoint?(a_group, a, b)

      # Integer and Float are disjoint (no value is both)
      {a_group, b_group} in [{:integer, :float}, {:float, :integer}] ->
        true

      # Integer/Float are subtypes of Number, so not disjoint from it
      a_group in [:integer, :float] and b_group == :number ->
        false

      b_group in [:integer, :float] and a_group == :number ->
        false

      # Top is compatible with everything
      a_group == :top or b_group == :top ->
        false

      # Bottom is disjoint from everything except itself
      a_group == :bottom or b_group == :bottom ->
        a_group != b_group

      # Different groups are disjoint
      true ->
        true
    end
  end

  defp tuple_arity(%Deft.Type.FixedTuple{elements: elements}), do: length(elements)
  defp tuple_arity(%Deft.Type.Tuple{}), do: :any

  # Check if two types in the same group are disjoint by examining their components.
  defp same_group_disjoint?(:list, %Deft.Type.FixedList{contents: a}, %Deft.Type.FixedList{
         contents: b
       }) do
    disjoint?(a, b)
  end

  defp same_group_disjoint?(
         :tuple,
         %Deft.Type.FixedTuple{elements: a_elems},
         %Deft.Type.FixedTuple{elements: b_elems}
       )
       when length(a_elems) == length(b_elems) do
    # Tuples are disjoint if any corresponding element types are disjoint.
    Enum.zip(a_elems, b_elems)
    |> Enum.any?(fn {a, b} -> disjoint?(a, b) end)
  end

  defp same_group_disjoint?(:function, %Deft.Type.Fn{inputs: a_inputs}, %Deft.Type.Fn{
         inputs: b_inputs
       })
       when length(a_inputs) == length(b_inputs) do
    # Functions with same arity are disjoint if any corresponding parameter types are disjoint.
    # This is because the intersection would require handling both parameter type signatures.
    Enum.zip(a_inputs, b_inputs)
    |> Enum.any?(fn {a, b} -> disjoint?(a, b) end)
  end

  defp same_group_disjoint?(_group, _a, _b), do: false

  # Classify types into groups for disjointness checking.
  defp type_group(%Deft.Type.Integer{}), do: :integer
  defp type_group(%Deft.Type.Float{}), do: :float
  defp type_group(%Deft.Type.Number{}), do: :number
  defp type_group(%Deft.Type.Boolean{}), do: :boolean
  defp type_group(%Deft.Type.Atom{}), do: :atom
  defp type_group(%Deft.Type.Binary{}), do: :binary
  defp type_group(%Deft.Type.FixedTuple{}), do: :tuple
  defp type_group(%Deft.Type.Tuple{}), do: :tuple
  defp type_group(%Deft.Type.FixedList{}), do: :list
  defp type_group(%Deft.Type.List{}), do: :list
  defp type_group(%Deft.Type.Fn{}), do: :function
  defp type_group(%Deft.Type.Top{}), do: :top
  defp type_group(%Deft.Type.Bottom{}), do: :bottom
  defp type_group(%Deft.Type.Union{}), do: :union
  defp type_group(_), do: :unknown

  # Remove redundant components from an intersection.
  # A component is redundant if another component is a subtype of it.
  # Uses simple subtyping without going through Type.intersection to avoid cycles.
  defp remove_redundant(components) do
    Enum.reject(components, fn comp ->
      # comp is redundant if some OTHER component is a strict subtype of it
      Enum.any?(components, fn other ->
        other != comp and strict_subtype?(comp, other)
      end)
    end)
  end

  # Check if sub is a strict subtype of super (sub <: super but super !<: sub).
  # Uses basic subtyping rules to avoid infinite loops.
  defp strict_subtype?(super, sub) do
    Deft.Subtyping.subtype_of?(super, sub) and not Deft.Subtyping.subtype_of?(sub, super)
  end

  # Compare types for canonical ordering.
  # Primary key: module name (alphabetical).
  # Secondary key: struct contents (Erlang term order).
  defp type_compare(a, b) do
    a_mod = a.__struct__
    b_mod = b.__struct__

    cond do
      a_mod < b_mod -> true
      a_mod > b_mod -> false
      true -> :erlang.term_to_binary(a) <= :erlang.term_to_binary(b)
    end
  end

  defimpl AST do
    def to_raw_ast(type) do
      fst = @protocol.to_raw_ast(type.fst)
      snd = @protocol.to_raw_ast(type.snd)

      {:&, [], [fst, snd]}
    end
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(t, opts) do
      concat([
        Inspect.inspect(t.fst, opts),
        " & ",
        Inspect.inspect(t.snd, opts)
      ])
    end
  end

  defimpl Deft.Walkable do
    def children(node), do: [node.fst, node.snd]
    def rebuild(node, [fst, snd]), do: %{node | fst: fst, snd: snd}
  end
end
