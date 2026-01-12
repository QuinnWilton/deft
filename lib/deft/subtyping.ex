defmodule Deft.Subtyping do
  @moduledoc """
  Subtyping relation built from the declarative type lattice.

  The lattice is defined declaratively in each type module using `Deft.Subtyping.DSL`.

  ## Introspection

      Deft.Subtyping.Lattice.subtypes_of(Deft.Type.Number)
      Deft.Subtyping.Lattice.supertypes_of(Deft.Type.Integer)
      Deft.Subtyping.Lattice.edges()
  """

  alias Deft.Subtyping.Lattice
  alias Deft.Type

  @type mismatch_info :: %{
          path: [non_neg_integer() | atom()],
          expected: Type.t(),
          actual: Type.t(),
          expr: term() | nil
        }

  @type check_result :: :ok | {:mismatch, mismatch_info()}

  @doc """
  Checks subtype relation and returns detailed mismatch info including path.

  The `sub_exprs` parameter provides AST nodes corresponding to elements in `sub` type,
  enabling precise error location for compound types like tuples.

  Returns `:ok` if `sub` is a subtype of `super`, or `{:mismatch, info}` with details
  about the first element that fails the check.
  """
  @spec check_subtype(Type.t(), Type.t(), term() | [term()] | nil) :: check_result()
  def check_subtype(super, sub, sub_exprs \\ nil)

  # FixedTuple: element-by-element checking with path tracking
  def check_subtype(
        %Type.FixedTuple{elements: super_elems},
        %Type.FixedTuple{elements: sub_elems},
        sub_exprs
      )
      when is_list(sub_exprs) and length(super_elems) == length(sub_elems) do
    super_elems
    |> Enum.zip(sub_elems)
    |> Enum.zip(sub_exprs)
    |> Enum.with_index()
    |> Enum.find_value(:ok, fn {{{super_elem, sub_elem}, elem_expr}, idx} ->
      case check_subtype(super_elem, sub_elem, elem_expr) do
        :ok -> nil
        {:mismatch, info} -> {:mismatch, %{info | path: [idx | info.path]}}
      end
    end)
  end

  # Fallback for all other types or when no sub_exprs available
  def check_subtype(super, sub, expr) do
    if subtype_of?(super, sub) do
      :ok
    else
      {:mismatch, %{path: [], expected: super, actual: sub, expr: expr}}
    end
  end

  @doc """
  Checks if all types in `supers` are supertypes of corresponding types in `subs`.
  """
  @spec subtypes_of?([Type.t()], [Type.t()]) :: boolean()
  def subtypes_of?(supers, subs) do
    length(supers) == length(subs) and
      Enum.zip(supers, subs)
      |> Enum.all?(fn {super, sub} -> subtype_of?(super, sub) end)
  end

  @doc """
  Returns true if `sub` is a subtype of `super`.

  Note: The argument order is `subtype_of?(super, sub)`.
  """
  @spec subtype_of?(Type.t(), Type.t()) :: boolean()

  # Top is supertype of everything
  def subtype_of?(%Type.Top{}, _sub), do: true

  # Bottom is subtype of everything
  def subtype_of?(_super, %Type.Bottom{}), do: true

  # Reflexivity
  def subtype_of?(t, t), do: true

  # Union as sub: Union(a, b) <: super if a <: super AND b <: super
  # (Must be checked before Union as super when both args are Unions)
  def subtype_of?(super, %Type.Union{fst: fst, snd: snd}) do
    subtype_of?(super, fst) and subtype_of?(super, snd)
  end

  # Union as super: sub <: Union(a, b) if sub <: a OR sub <: b
  def subtype_of?(%Type.Union{fst: fst, snd: snd}, sub) do
    subtype_of?(fst, sub) or subtype_of?(snd, sub)
  end

  # Intersection as sub: Intersection(a, b) <: super if a <: super OR b <: super
  # (Must be checked before Intersection as super when both args are Intersections)
  def subtype_of?(super, %Type.Intersection{fst: fst, snd: snd}) do
    subtype_of?(super, fst) or subtype_of?(super, snd)
  end

  # Intersection as super: sub <: Intersection(a, b) if sub <: a AND sub <: b
  def subtype_of?(%Type.Intersection{fst: fst, snd: snd}, sub) do
    subtype_of?(fst, sub) and subtype_of?(snd, sub)
  end

  # ADT/Variant relationship: Variant <: ADT if variant belongs to ADT
  def subtype_of?(%Type.ADT{} = adt, %Type.Variant{} = variant) do
    adt.name == variant.adt_name and variant in adt.variants
  end

  # Same type constructor - try structural rule
  def subtype_of?(%{__struct__: mod} = super, %{__struct__: mod} = sub) do
    if Lattice.has_structural_rule?(mod) do
      mod.structural_subtype?(sub, super)
    else
      # No structural rule, not reflexively equal (checked above)
      false
    end
  end

  # Different type constructors - check declared lattice edges
  def subtype_of?(%{__struct__: super_mod}, %{__struct__: sub_mod}) do
    super_mod in Lattice.supertypes_of(sub_mod)
  end

  # Fallback
  def subtype_of?(_super, _sub), do: false
end
