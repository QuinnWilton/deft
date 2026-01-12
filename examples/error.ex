defmodule Examples.Error do
  use Deft

  deft sum(a :: integer, b :: integer) :: integer do
    # Compute the sum of two integers
    # (but accidentally add a float)
    a + b + 1.5
  end

  deft main(a :: integer, b :: integer) :: atom do
    IO.puts(sum(a, b))
  end
end

[a_str, b_str] = System.argv()

Examples.Error.main(String.to_integer(a_str), String.to_integer(b_str))
