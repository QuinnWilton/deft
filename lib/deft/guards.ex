defmodule Deft.Guards do
  @moduledoc """
  Type checking for built-in guard functions and operators.

  This module defines the type signatures and checking logic for
  Elixir's built-in guard functions that can be used in typed code.
  """

  alias Deft.Context
  alias Deft.Subtyping
  alias Deft.Type
  alias Deft.TypeChecker

  @comparisons [:!=, :!==, :<, :<=, :==, :===, :>, :>=]
  @unary_math [:-, :abs, :ceil, :floor, :round, :trunc]
  @binary_math [:*, :+, :-]
  @integer_division [:rem, :div]

  @type_guards [
    :is_atom,
    :is_boolean,
    :is_float,
    :is_function,
    :is_integer,
    :is_number,
    :is_tuple,
    :is_list
  ]

  @supported_guards [
    !=: 2,
    !==: 2,
    *: 2,
    +: 1,
    +: 2,
    -: 1,
    -: 2,
    /: 2,
    <: 2,
    <=: 2,
    ==: 2,
    ===: 2,
    >: 2,
    >=: 2,
    abs: 1,
    ceil: 1,
    div: 2,
    elem: 2,
    floor: 1,
    hd: 1,
    is_atom: 1,
    is_boolean: 1,
    is_float: 1,
    is_function: 1,
    is_function: 2,
    is_integer: 1,
    is_list: 1,
    is_number: 1,
    is_tuple: 1,
    length: 1,
    not: 1,
    rem: 2,
    round: 1,
    tl: 1,
    trunc: 1,
    tuple_size: 1
  ]

  @doc """
  Returns the list of supported guards with their arities.
  """
  def supported_guards, do: @supported_guards

  @doc """
  Returns true if the given function name and arity is a supported guard.
  """
  def supported?(name, arity), do: {name, arity} in @supported_guards

  @doc """
  Type checks a guard function call.

  Returns `{erased_args, result_type, bindings}`.
  """
  @spec handle_guard(atom(), [term()], Context.t()) :: {[term()], Type.t(), [term()]}
  def handle_guard(name, args, %Context{} = ctx) do
    do_handle_guard(name, args, ctx)
  end

  # Comparison operators: any types, returns boolean
  defp do_handle_guard(name, [fst, snd], ctx) when name in @comparisons do
    {:ok, fst, _, fst_bindings, _} = TypeChecker.check(fst, ctx)
    {:ok, snd, _, snd_bindings, _} = TypeChecker.check(snd, ctx)

    {[fst, snd], Type.boolean(), fst_bindings ++ snd_bindings}
  end

  # Unary math: number -> same type
  defp do_handle_guard(name, [term], ctx) when name in @unary_math do
    {:ok, term, term_t, bindings, _} = TypeChecker.check(term, ctx)

    unless Subtyping.subtype_of?(Type.number(), term_t) do
      raise Deft.TypecheckingError, expected: Type.number(), actual: term_t
    end

    {[term], term_t, bindings}
  end

  # Binary math: number, number -> number (preserves most specific type)
  defp do_handle_guard(name, [fst, snd], ctx) when name in @binary_math do
    {:ok, fst, fst_t, fst_bindings, _} = TypeChecker.check(fst, ctx)
    {:ok, snd, snd_t, snd_bindings, _} = TypeChecker.check(snd, ctx)

    unless Subtyping.subtype_of?(Type.number(), fst_t) do
      raise Deft.TypecheckingError, expected: Type.number(), actual: fst_t
    end

    unless Subtyping.subtype_of?(Type.number(), snd_t) do
      raise Deft.TypecheckingError, expected: Type.number(), actual: snd_t
    end

    # Compute result type: most specific common type
    type =
      cond do
        Subtyping.subtype_of?(fst_t, snd_t) -> fst_t
        Subtyping.subtype_of?(snd_t, fst_t) -> snd_t
        true -> Type.number()
      end

    {[fst, snd], type, fst_bindings ++ snd_bindings}
  end

  # Integer division: integer, integer -> integer
  defp do_handle_guard(name, [fst, snd], ctx) when name in @integer_division do
    {:ok, fst, fst_t, fst_bindings, _} = TypeChecker.check(fst, ctx)
    {:ok, snd, snd_t, snd_bindings, _} = TypeChecker.check(snd, ctx)

    unless Subtyping.subtype_of?(Type.integer(), fst_t) do
      raise Deft.TypecheckingError, expected: Type.integer(), actual: fst_t
    end

    unless Subtyping.subtype_of?(Type.integer(), snd_t) do
      raise Deft.TypecheckingError, expected: Type.integer(), actual: snd_t
    end

    {[fst, snd], Type.integer(), fst_bindings ++ snd_bindings}
  end

  # Division: number, number -> float
  defp do_handle_guard(:/, [fst, snd], ctx) do
    {:ok, fst, fst_t, fst_bindings, _} = TypeChecker.check(fst, ctx)
    {:ok, snd, snd_t, snd_bindings, _} = TypeChecker.check(snd, ctx)

    unless Subtyping.subtype_of?(Type.number(), fst_t) do
      raise Deft.TypecheckingError, expected: Type.number(), actual: fst_t
    end

    unless Subtyping.subtype_of?(Type.number(), snd_t) do
      raise Deft.TypecheckingError, expected: Type.number(), actual: snd_t
    end

    {[fst, snd], Type.float(), fst_bindings ++ snd_bindings}
  end

  # tuple_size: tuple -> integer
  defp do_handle_guard(:tuple_size, [term], ctx) do
    {:ok, term, term_t, bindings, _} = TypeChecker.check(term, ctx)

    unless Subtyping.subtype_of?(Type.tuple(), term_t) do
      raise Deft.TypecheckingError, expected: Type.tuple(), actual: term_t
    end

    {[term], Type.integer(), bindings}
  end

  # length: list -> integer
  defp do_handle_guard(:length, [term], ctx) do
    {:ok, term, term_t, bindings, _} = TypeChecker.check(term, ctx)

    unless Subtyping.subtype_of?(Type.list(), term_t) do
      raise Deft.TypecheckingError, expected: Type.list(), actual: term_t
    end

    {[term], Type.integer(), bindings}
  end

  # Type guards: any -> boolean
  defp do_handle_guard(name, [term], ctx) when name in @type_guards do
    {:ok, term, _, bindings, _} = TypeChecker.check(term, ctx)

    {[term], Type.boolean(), bindings}
  end

  # is_function/2: any, integer -> boolean
  defp do_handle_guard(:is_function, [fun, arity], ctx) do
    {:ok, fun, _, fun_bindings, _} = TypeChecker.check(fun, ctx)
    {:ok, arity, arity_t, arity_bindings, _} = TypeChecker.check(arity, ctx)

    unless Subtyping.subtype_of?(Type.integer(), arity_t) do
      raise Deft.TypecheckingError, expected: Type.integer(), actual: arity_t
    end

    {[fun, arity], Type.boolean(), fun_bindings ++ arity_bindings}
  end

  # not: boolean -> boolean
  defp do_handle_guard(:not, [term], ctx) do
    {:ok, term, term_t, bindings, _} = TypeChecker.check(term, ctx)

    unless Subtyping.subtype_of?(Type.boolean(), term_t) do
      raise Deft.TypecheckingError, expected: Type.boolean(), actual: term_t
    end

    {[term], term_t, bindings}
  end

  # hd: list(a) -> a
  defp do_handle_guard(:hd, [term], ctx) do
    {:ok, term, term_t, bindings, _} = TypeChecker.check(term, ctx)

    unless Subtyping.subtype_of?(Type.list(), term_t) do
      raise Deft.TypecheckingError, expected: Type.list(), actual: term_t
    end

    {[term], Type.FixedList.contents(term_t), bindings}
  end

  # tl: list(a) -> list(a)
  defp do_handle_guard(:tl, [term], ctx) do
    {:ok, term, term_t, bindings, _} = TypeChecker.check(term, ctx)

    unless Subtyping.subtype_of?(Type.list(), term_t) do
      raise Deft.TypecheckingError, expected: Type.list(), actual: term_t
    end

    {[term], term_t, bindings}
  end

  # elem: tuple, integer -> union of element types
  defp do_handle_guard(:elem, [tuple, index], ctx) do
    {:ok, tuple, tuple_t, tuple_bindings, _} = TypeChecker.check(tuple, ctx)
    {:ok, index, index_t, index_bindings, _} = TypeChecker.check(index, ctx)

    unless Subtyping.subtype_of?(Type.tuple(), tuple_t) do
      raise Deft.TypecheckingError, expected: Type.tuple(), actual: tuple_t
    end

    unless Subtyping.subtype_of?(Type.integer(), index_t) do
      raise Deft.TypecheckingError, expected: Type.integer(), actual: index_t
    end

    type =
      tuple_t
      |> Type.FixedTuple.unique_types()
      |> Enum.reduce(Type.bottom(), &Type.union/2)

    {[tuple, index], type, tuple_bindings ++ index_bindings}
  end
end
