defmodule Deft.TypeChecking.Guards do
  import Deft.Helpers

  alias Deft.Subtyping
  alias Deft.Type

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
    # binary_part: 3,
    # bit_size: 1,
    # byte_size: 1,
    ceil: 1,
    div: 2,
    elem: 2,
    floor: 1,
    hd: 1,
    is_atom: 1,
    # is_binary: 1,
    # is_bitstring: 1,
    is_boolean: 1,
    is_float: 1,
    is_function: 1,
    is_function: 2,
    is_integer: 1,
    is_list: 1,
    # is_map: 1,
    # is_map_key: 2,
    is_number: 1,
    # is_pid: 1,
    # is_port: 1,
    # is_reference: 1,
    is_tuple: 1,
    length: 1,
    # map_size: 1,
    # node: 0,
    # node: 1,
    not: 1,
    rem: 2,
    round: 1,
    # self: 0,
    tl: 1,
    trunc: 1,
    tuple_size: 1
  ]

  def supported_guards() do
    @supported_guards
  end

  def supported?(name, arity) do
    {name, arity} in supported_guards()
  end

  def handle_guard(name, [fst, snd], env) when name in @comparisons do
    fst = erase_types(fst, env)
    snd = erase_types(snd, env)

    {[fst, snd], Type.boolean()}
  end

  def handle_guard(name, [term], env) when name in @unary_math do
    {term, term_t} = compute_and_erase_types(term, env)

    unless Subtyping.subtype_of?(Type.number(), term_t) do
      raise Deft.TypecheckingError, expected: Type.number(), actual: term_t
    end

    {[term], term_t}
  end

  def handle_guard(name, [fst, snd], env) when name in @binary_math do
    {fst, fst_t} = compute_and_erase_types(fst, env)
    {snd, snd_t} = compute_and_erase_types(snd, env)

    unless Subtyping.subtype_of?(Type.number(), fst_t) do
      raise Deft.TypecheckingError, expected: Type.number(), actual: fst_t
    end

    unless Subtyping.subtype_of?(Type.number(), fst_t) do
      raise Deft.TypecheckingError, expected: Type.number(), actual: snd_t
    end

    # TODO: Reusable?
    # TODO: needs to use subtyping
    type =
      case {fst_t, snd_t} do
        {%Type.Number{}, _} ->
          Type.number()

        {_, %Type.Number{}} ->
          Type.number()

        {%Type.Float{}, _} ->
          Type.float()

        {_, %Type.Float{}} ->
          Type.float()

        {%Type.Integer{}, %Type.Integer{}} ->
          Type.integer()
      end

    {[fst, snd], type}
  end

  def handle_guard(name, [fst, snd], env) when name in @integer_division do
    {fst, fst_t} = compute_and_erase_types(fst, env)
    {snd, snd_t} = compute_and_erase_types(snd, env)

    unless Subtyping.subtype_of?(Type.integer(), fst_t) do
      raise Deft.TypecheckingError, expected: Type.integer(), actual: fst_t
    end

    unless Subtyping.subtype_of?(Type.integer(), snd_t) do
      raise Deft.TypecheckingError, expected: Type.integer(), actual: snd_t
    end

    {[fst, snd], Type.integer()}
  end

  def handle_guard(:/, [fst, snd], env) do
    {fst, fst_t} = compute_and_erase_types(fst, env)
    {snd, snd_t} = compute_and_erase_types(snd, env)

    unless Subtyping.subtype_of?(Type.number(), fst_t) do
      raise Deft.TypecheckingError, expected: Type.number(), actual: fst_t
    end

    unless Subtyping.subtype_of?(Type.number(), snd_t) do
      raise Deft.TypecheckingError, expected: Type.number(), actual: snd_t
    end

    {[fst, snd], Type.float()}
  end

  def handle_guard(:tuple_size, [term], env) do
    {term, term_t} = compute_and_erase_types(term, env)

    unless is_struct(term_t, Type.Tuple) do
      # TODO This doesn't handle unions. I need a way of checking if a type
      # subtypes any tuple
      raise Deft.TypecheckingError, expected: Type.tuple([Type.top()]), actual: term_t
    end

    {[term], Type.integer()}
  end

  def handle_guard(:length, [term], env) do
    {term, term_t} = compute_and_erase_types(term, env)

    unless Subtyping.subtype_of?(Type.list(Type.top()), term_t) do
      raise Deft.TypecheckingError, expected: Type.list(Type.top()), actual: term_t
    end

    {[term], term_t}
  end

  def handle_guard(name, [term], env) when name in @type_guards do
    term = erase_types(term, env)

    {[term], Type.boolean()}
  end

  def handle_guard(:is_function, [fun, arity], env) do
    fun = erase_types(fun, env)
    {arity, arity_t} = compute_and_erase_types(arity, env)

    unless Subtyping.subtype_of?(Type.integer(), arity_t) do
      raise Deft.TypecheckingError, expected: Type.integer(), actual: arity_t
    end

    {[fun, arity], Type.boolean()}
  end

  def handle_guard(:not, [term], env) do
    {term, term_t} = compute_and_erase_types(term, env)

    unless Subtyping.subtype_of?(Type.boolean(), term_t) do
      raise Deft.TypecheckingError, expected: Type.integer(), actual: term_t
    end

    {[term], term_t}
  end

  def handle_guard(:hd, [term], env) do
    {term, term_t} = compute_and_erase_types(term, env)

    unless Subtyping.subtype_of?(Type.list(Type.top()), term_t) do
      raise Deft.TypecheckingError, expected: Type.list(Type.top()), actual: term_t
    end

    {[term], Type.List.contents(term_t)}
  end

  def handle_guard(:tl, [term], env) do
    {term, term_t} = compute_and_erase_types(term, env)

    unless Subtyping.subtype_of?(Type.list(Type.top()), term_t) do
      raise Deft.TypecheckingError, expected: Type.list(Type.top()), actual: term_t
    end

    {[term], term_t}
  end

  def handle_guard(:elem, [tuple, index], env) do
    {tuple, tuple_t} = compute_and_erase_types(tuple, env)
    {index, index_t} = compute_and_erase_types(index, env)

    unless is_struct(tuple_t, Type.Tuple) do
      raise "Expected a tuple"
    end

    unless Subtyping.subtype_of?(Type.integer(), index_t) do
      raise Deft.TypecheckingError, expected: Type.integer(), actual: index_t
    end

    type =
      tuple_t
      |> Type.Tuple.unique_types()
      |> Type.Union.new()

    {[tuple, index], type}
  end
end