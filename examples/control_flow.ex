# Control flow example using Deft type checking
# Demonstrates typed control flow patterns

defmodule Examples.ControlFlow do
  @moduledoc """
  Demonstrates type-checked control flow patterns with the deft macro.
  """

  use Deft

  # Absolute value using if expression
  deft abs_value(x :: integer) :: integer do
    if x < 0 do
      0 - x
    else
      x
    end
  end

  # Clamp a value to a range
  deft clamp(x :: integer, min_val :: integer, max_val :: integer) :: integer do
    cond do
      x < min_val -> min_val
      x > max_val -> max_val
      true -> x
    end
  end

  # Return the maximum of two values
  deft max_of(a :: integer, b :: integer) :: integer do
    if a > b do
      a
    else
      b
    end
  end

  # Return the minimum of two values
  deft min_of(a :: integer, b :: integer) :: integer do
    if a < b do
      a
    else
      b
    end
  end

  # Typed main functions for each operation
  deft main_abs(x :: integer) :: atom do
    IO.puts(abs_value(x))
  end

  deft main_clamp(x :: integer, min_v :: integer, max_v :: integer) :: atom do
    IO.puts(clamp(x, min_v, max_v))
  end

  deft main_max(a :: integer, b :: integer) :: atom do
    IO.puts(max_of(a, b))
  end

  deft main_min(a :: integer, b :: integer) :: atom do
    IO.puts(min_of(a, b))
  end
end

# Only run CLI when invoked directly with valid arguments
case System.argv() do
  ["abs", x] ->
    Examples.ControlFlow.main_abs(String.to_integer(x))

  ["clamp", x, min_v, max_v] ->
    Examples.ControlFlow.main_clamp(
      String.to_integer(x),
      String.to_integer(min_v),
      String.to_integer(max_v)
    )

  ["max", a, b] ->
    Examples.ControlFlow.main_max(String.to_integer(a), String.to_integer(b))

  ["min", a, b] ->
    Examples.ControlFlow.main_min(String.to_integer(a), String.to_integer(b))

  _ ->
    :ok
end
