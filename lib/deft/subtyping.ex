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

  # Intersection as super: sub <: Intersection(a, b) if sub <: a AND sub <: b
  # (Must be checked before Intersection as sub when both args are Intersections,
  # so that A & B & C <: D & E decomposes to checking against D and E separately)
  def subtype_of?(%Type.Intersection{fst: fst, snd: snd}, sub) do
    subtype_of?(fst, sub) and subtype_of?(snd, sub)
  end

  # Intersection as sub: Intersection(a, b) <: super if a <: super OR b <: super
  def subtype_of?(super, %Type.Intersection{fst: fst, snd: snd}) do
    subtype_of?(super, fst) or subtype_of?(super, snd)
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

  # ============================================================================
  # Detailed Subtype Checking with Span Tracking
  # ============================================================================

  @typedoc """
  Information about a type mismatch, including the path to the failing element
  and the AST expression at that location.
  """
  @type mismatch_info :: %{
          path: [non_neg_integer() | atom()],
          expected: Type.t(),
          actual: Type.t(),
          expr: term() | nil
        }

  @type check_result :: :ok | {:mismatch, mismatch_info()}

  @doc """
  Checks subtype relation and returns detailed mismatch info including the failing AST node.

  The `sub_exprs` parameter provides AST nodes corresponding to elements in `sub` type,
  which allows pinpointing the exact location of type errors in compound types.

  ## Examples

      # Check a simple type
      check_subtype(Type.number(), Type.integer(), some_expr)
      # => :ok

      # Check a tuple with element expressions for span tracking
      check_subtype(
        Type.fixed_tuple([Type.number(), Type.float()]),
        Type.fixed_tuple([Type.integer(), Type.integer()]),
        [expr_for_1, expr_for_2]
      )
      # => {:mismatch, %{path: [1], expected: Type.float(), actual: Type.integer(), expr: expr_for_2}}
  """
  @spec check_subtype(Type.t(), Type.t(), term() | [term()] | nil) :: check_result()
  def check_subtype(super, sub, sub_expr \\ nil)

  # Same type constructor with auto-generated check_subtype - delegate to it
  def check_subtype(%{__struct__: mod} = super, %{__struct__: mod} = sub, sub_expr) do
    if Lattice.has_check_subtype?(mod) do
      mod.check_subtype(sub, super, sub_expr)
    else
      # Fallback to basic check
      if subtype_of?(super, sub) do
        :ok
      else
        {:mismatch, %{path: [], expected: super, actual: sub, expr: sub_expr}}
      end
    end
  end

  # Fallback: use subtype_of? and report full types if mismatch
  def check_subtype(super, sub, expr) do
    if subtype_of?(super, sub) do
      :ok
    else
      {:mismatch, %{path: [], expected: super, actual: sub, expr: expr}}
    end
  end
end
