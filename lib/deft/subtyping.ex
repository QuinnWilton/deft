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

  # FixedTuple: check element-by-element
  def check_subtype(
        %Type.FixedTuple{elements: super_elems},
        %Type.FixedTuple{elements: sub_elems},
        sub_exprs
      )
      when is_list(sub_exprs) do
    if length(super_elems) != length(sub_elems) do
      {:mismatch,
       %{
         path: [],
         expected: Type.fixed_tuple(super_elems),
         actual: Type.fixed_tuple(sub_elems),
         expr: nil
       }}
    else
      check_tuple_elements(super_elems, sub_elems, sub_exprs, 0)
    end
  end

  # FixedList: check element type
  def check_subtype(
        %Type.FixedList{contents: super_contents},
        %Type.FixedList{contents: sub_contents},
        sub_expr
      ) do
    case check_subtype(super_contents, sub_contents, nil) do
      :ok -> :ok
      {:mismatch, info} -> {:mismatch, %{info | expr: sub_expr}}
    end
  end

  # Fn: check inputs and output
  def check_subtype(
        %Type.Fn{inputs: super_inputs, output: super_output},
        %Type.Fn{inputs: sub_inputs, output: sub_output},
        sub_expr
      ) do
    # Function subtyping: contravariant in inputs, covariant in output.
    # sub <: super means sub can be used where super is expected.
    # For inputs: super_inputs <: sub_inputs (contravariant).
    # For output: sub_output <: super_output (covariant).
    with :ok <- check_fn_inputs(super_inputs, sub_inputs, 0),
         :ok <- check_subtype(super_output, sub_output, nil) do
      :ok
    else
      {:mismatch, info} -> {:mismatch, %{info | expr: info.expr || sub_expr}}
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

  # Helper: check tuple elements one by one
  defp check_tuple_elements([], [], [], _idx), do: :ok

  defp check_tuple_elements(
         [super_elem | super_rest],
         [sub_elem | sub_rest],
         [elem_expr | expr_rest],
         idx
       ) do
    case check_subtype(super_elem, sub_elem, elem_expr) do
      :ok ->
        check_tuple_elements(super_rest, sub_rest, expr_rest, idx + 1)

      {:mismatch, info} ->
        {:mismatch, %{info | path: [idx | info.path]}}
    end
  end

  # Handle case where sub_exprs is shorter than elements (or nil)
  defp check_tuple_elements(
         [super_elem | super_rest],
         [sub_elem | sub_rest],
         [],
         idx
       ) do
    case check_subtype(super_elem, sub_elem, nil) do
      :ok ->
        check_tuple_elements(super_rest, sub_rest, [], idx + 1)

      {:mismatch, info} ->
        {:mismatch, %{info | path: [idx | info.path]}}
    end
  end

  # Helper: check function inputs (contravariant)
  defp check_fn_inputs([], [], _idx), do: :ok

  defp check_fn_inputs([super_input | super_rest], [sub_input | sub_rest], idx) do
    # Contravariant: super_input <: sub_input
    case check_subtype(sub_input, super_input, nil) do
      :ok ->
        check_fn_inputs(super_rest, sub_rest, idx + 1)

      {:mismatch, info} ->
        {:mismatch, %{info | path: [:input, idx | info.path]}}
    end
  end

  defp check_fn_inputs(_, _, _idx) do
    # Arity mismatch - let the caller handle with full type comparison
    :ok
  end
end
