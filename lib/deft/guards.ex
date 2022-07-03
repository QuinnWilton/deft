defmodule Deft.Guards do
  import Deft.Helpers

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

  def handle_guard(name, [a, b], env) when name in @comparisons do
    a = erase_type(a, env)
    b = erase_type(b, env)

    {[a, b], Type.Boolean.new()}
  end

  def handle_guard(name, [a], env) when name in @unary_math do
    {a, a_t} = compute_and_erase_type(a, env)

    unless subtype_of?(Type.Number.new(), a_t) do
      raise Deft.TypecheckingError, expected: Type.Number.new(), actual: a_t
    end

    {[a], a_t}
  end

  def handle_guard(name, [a, b], env) when name in @binary_math do
    {a, a_t} = compute_and_erase_type(a, env)
    {b, b_t} = compute_and_erase_type(b, env)

    unless subtype_of?(Type.Number.new(), a_t) do
      raise Deft.TypecheckingError, expected: Type.Number.new(), actual: a_t
    end

    unless subtype_of?(Type.Number.new(), b_t) do
      raise Deft.TypecheckingError, expected: Type.Number.new(), actual: b_t
    end

    # TODO: Reusable?
    type =
      case {a_t, b_t} do
        {%Type.Number{}, _} ->
          Type.Number.new()

        {_, %Type.Number{}} ->
          Type.Number.new()

        {%Type.Float{}, _} ->
          Type.Float.new()

        {_, %Type.Float{}} ->
          Type.Float.new()

        {%Type.Integer{}, %Type.Integer{}} ->
          Type.Integer.new()
      end

    {[a, b], type}
  end

  def handle_guard(name, [a, b], env) when name in @integer_division do
    {a, a_t} = compute_and_erase_type(a, env)
    {b, b_t} = compute_and_erase_type(b, env)

    unless subtype_of?(Type.Integer.new(), a_t) do
      raise Deft.TypecheckingError, expected: Type.Integer.new(), actual: a_t
    end

    unless subtype_of?(Type.Integer.new(), b_t) do
      raise Deft.TypecheckingError, expected: Type.Integer.new(), actual: b_t
    end

    {[a, b], Type.Integer.new()}
  end

  def handle_guard(:/, [a, b], env) do
    {a, a_t} = compute_and_erase_type(a, env)
    {b, b_t} = compute_and_erase_type(b, env)

    unless subtype_of?(Type.Number.new(), a_t) do
      raise Deft.TypecheckingError, expected: Type.Number.new(), actual: a_t
    end

    unless subtype_of?(Type.Number.new(), b_t) do
      raise Deft.TypecheckingError, expected: Type.Number.new(), actual: b_t
    end

    {[a, b], Type.Float.new()}
  end

  def handle_guard(:tuple_size, [term], env) do
    {term, term_t} = compute_and_erase_type(term, env)

    unless is_struct(term_t, Type.Tuple) do
      # TODO This doesn't handle unions. I need a way of checking if a type
      # subtypes any tuple
      raise Deft.TypecheckingError, expected: Type.Tuple.new([Type.Top.new()]), actual: term_t
    end

    {[term], Type.Integer.new()}
  end

  def handle_guard(:length, [term], env) do
    {term, term_t} = compute_and_erase_type(term, env)

    unless subtype_of?(Type.List.new(Type.Top.new()), term_t) do
      raise Deft.TypecheckingError, expected: Type.List.new(Type.Top.new()), actual: term_t
    end

    {[term], term_t}
  end

  def handle_guard(name, [term], env) when name in @type_guards do
    term = erase_type(term, env)

    {[term], Type.Boolean.new()}
  end

  def handle_guard(:is_function, [fun, arity], env) do
    fun = erase_type(fun, env)
    {arity, arity_t} = compute_and_erase_type(arity, env)

    unless subtype_of?(Type.Integer.new(), arity_t) do
      raise Deft.TypecheckingError, expected: Type.Integer.new(), actual: arity_t
    end

    {[fun, arity], Type.Boolean.new()}
  end

  def handle_guard(:not, [term], env) do
    {term, term_t} = compute_and_erase_type(term, env)

    unless subtype_of?(Type.Boolean.new(), term_t) do
      raise Deft.TypecheckingError, expected: Type.Integer.new(), actual: term_t
    end

    {[term], term_t}
  end

  def handle_guard(:hd, [term], env) do
    {term, term_t} = compute_and_erase_type(term, env)

    unless subtype_of?(Type.List.new(Type.Top.new()), term_t) do
      raise Deft.TypecheckingError, expected: Type.List.new(Type.Top.new()), actual: term_t
    end

    {[term], Type.List.contents(term_t)}
  end

  def handle_guard(:tl, [term], env) do
    {term, term_t} = compute_and_erase_type(term, env)

    unless subtype_of?(Type.List.new(Type.Top.new()), term_t) do
      raise Deft.TypecheckingError, expected: Type.List.new(Type.Top.new()), actual: term_t
    end

    {[term], term_t}
  end

  def handle_guard(:elem, [tuple, index], env) do
    {tuple, tuple_t} = compute_and_erase_type(tuple, env)
    {index, index_t} = compute_and_erase_type(index, env)

    unless is_struct(tuple_t, Type.Tuple) do
      raise "Expected a tuple"
    end

    unless subtype_of?(Type.Integer.new(), index_t) do
      raise Deft.TypecheckingError, expected: Type.Integer.new(), actual: index_t
    end

    type =
      tuple_t
      |> Type.Tuple.unique_types()
      |> Type.Union.new()

    {[tuple, index], type}
  end

  def handle_guard(:-, [e], env) do
    # HACK? Macro.expand_once(quote do -1 end) expands to 1, but
    #       Macro.expand_once(quote do -1.0 end) does not
    compute_and_erase_type(e, env)
  end
end
