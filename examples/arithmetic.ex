# Arithmetic example using Deft type checking
# Demonstrates typed function definitions with the deft macro

defmodule Examples.Arithmetic do
  @moduledoc """
  Demonstrates type-checked arithmetic with the deft macro.
  """

  use Deft

  deft add(x :: integer, y :: integer) :: integer do
    x + y
  end

  deft subtract(x :: integer, y :: integer) :: integer do
    x - y
  end

  deft multiply(x :: integer, y :: integer) :: integer do
    x * y
  end

  deft divide(x :: integer, y :: integer) :: integer do
    div(x, y)
  end

  deft modulo(x :: integer, y :: integer) :: integer do
    rem(x, y)
  end

  deft main(op :: binary, a :: integer, b :: integer) :: atom do
    result =
      case op do
        "add" -> add(a, b)
        "sub" -> subtract(a, b)
        "mul" -> multiply(a, b)
        "div" -> divide(a, b)
        "mod" -> modulo(a, b)
      end

    IO.puts(result)
  end
end

# Only run CLI when invoked directly with valid arguments
if match?([op, _, _] when op in ~w[add sub mul div mod], System.argv()) do
  [op, a_str, b_str] = System.argv()
  Examples.Arithmetic.main(op, String.to_integer(a_str), String.to_integer(b_str))
end
