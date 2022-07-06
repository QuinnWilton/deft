defmodule Deft.Generators.Types do
  import StreamData

  alias Deft.Type

  def type() do
    one_of([
      top_type(),
      primitive_type(),
      compound_type()
    ])
  end

  def primitive_type() do
    one_of([
      atom_type(),
      boolean_type(),
      float_type(),
      integer_type(),
      number_type()
    ])
  end

  def compound_type() do
    one_of([
      fn_type(),
      fixed_tuple_type(),
      union_type(),
      list_type()
    ])
  end

  def atom_type() do
    constant(Type.atom())
  end

  def boolean_type() do
    constant(Type.boolean())
  end

  def bottom_type() do
    constant(Type.bottom())
  end

  def float_type() do
    constant(Type.float())
  end

  def fn_type() do
    bind(list_of(primitive_type(), max_length: 8), fn inputs ->
      map(primitive_type(), fn output ->
        Type.fun(inputs, output)
      end)
    end)
  end

  def fixed_tuple_type() do
    bind(list_of(primitive_type(), max_length: 8), fn elements ->
      constant(Type.fixed_tuple(elements))
    end)
  end

  def union_type() do
    bind(list_of(primitive_type(), min_length: 1, max_length: 8), fn elements ->
      constant(Type.union(elements))
    end)
  end

  def list_type() do
    bind(primitive_type(), fn type ->
      constant(Type.list(type))
    end)
  end

  def integer_type() do
    constant(Type.integer())
  end

  def number_type() do
    constant(Type.number())
  end

  def top_type() do
    constant(Type.top())
  end
end
