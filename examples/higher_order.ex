# Higher-order function example using Deft type checking
# Demonstrates typed higher-order functions with the deft macro

defmodule Examples.HigherOrder do
  @moduledoc """
  Demonstrates type-checked higher-order function patterns using the deft macro.
  Functions accept function parameters with declared types showing:
  - Functions as arguments: (number -> number) parameter type
  - Subtyping: functions accepting number work with integer inputs
  """

  use Deft

  # Basic operations to be passed as function arguments
  deft double(x :: number) :: number do
    x * 2
  end

  deft increment(x :: number) :: number do
    x + 1
  end

  deft negate(x :: number) :: number do
    0 - x
  end

  # Higher-order: apply a unary operation twice
  # f(f(x)) - demonstrates function as argument
  deft apply_twice(x :: number, f :: (number -> number)) :: number do
    f.(f.(x))
  end

  # Higher-order: compose two unary operations
  # Returns f(g(x)) - demonstrates function composition
  deft compose(x :: number, f :: (number -> number), g :: (number -> number)) :: number do
    f.(g.(x))
  end

  # Higher-order: apply operation to both elements of a pair
  deft map_pair(a :: number, b :: number, f :: (number -> number)) :: {number, number} do
    {f.(a), f.(b)}
  end

  # Higher-order: fold a pair with a binary operation
  deft fold_pair(a :: number, b :: number, f :: (number, number -> number)) :: number do
    f.(a, b)
  end

  # Binary operations for fold_pair
  deft add(a :: number, b :: number) :: number do
    a + b
  end

  deft multiply(a :: number, b :: number) :: number do
    a * b
  end

  # Curried addition - returns a function
  deft curry_add(a :: number, b :: number) :: number do
    a + b
  end

  # Typed main functions for each higher-order pattern
  deft main_twice(x :: integer) :: atom do
    # apply_twice(double, x) = double(double(x)) = 4x
    result = apply_twice(x, &double/1)
    IO.puts(result)
  end

  deft main_compose(x :: integer) :: atom do
    # compose(increment, double)(x) = increment(double(x)) = 2x + 1
    result = compose(x, &increment/1, &double/1)
    IO.puts(result)
  end

  # main_map uses regular def since it needs string interpolation for output format
  def main_map(a, b) do
    {ra, rb} = map_pair(a, b, &double/1)
    IO.puts("#{ra},#{rb}")
  end

  deft main_fold_add(a :: integer, b :: integer) :: atom do
    # fold_pair(add, {a, b}) = a + b
    result = fold_pair(a, b, &add/2)
    IO.puts(result)
  end

  deft main_fold_mul(a :: integer, b :: integer) :: atom do
    # fold_pair(multiply, {a, b}) = a * b
    result = fold_pair(a, b, &multiply/2)
    IO.puts(result)
  end

  deft main_curry(a :: integer, b :: integer) :: atom do
    # Demonstrate partial application style
    result = curry_add(a, b)
    IO.puts(result)
  end
end

# Only run CLI when invoked directly with valid arguments
case System.argv() do
  ["twice", x] ->
    Examples.HigherOrder.main_twice(String.to_integer(x))

  ["compose", x] ->
    Examples.HigherOrder.main_compose(String.to_integer(x))

  ["map", a, b] ->
    Examples.HigherOrder.main_map(String.to_integer(a), String.to_integer(b))

  ["fold_add", a, b] ->
    Examples.HigherOrder.main_fold_add(String.to_integer(a), String.to_integer(b))

  ["fold_mul", a, b] ->
    Examples.HigherOrder.main_fold_mul(String.to_integer(a), String.to_integer(b))

  ["curry", a, b] ->
    Examples.HigherOrder.main_curry(String.to_integer(a), String.to_integer(b))

  _ ->
    :ok
end
