# Geometry example using Deft type checking
# Demonstrates ADTs with defdata, pattern matching, and subtyping

defmodule Examples.Geometry do
  @moduledoc """
  Demonstrates type-checked geometric calculations using proper ADTs
  defined with defdata, pattern matching on variants, and subtyping
  (integer <: number).
  """

  use Deft

  # Helper functions must be defined before they're used
  deft parse(s :: binary) :: integer do
    String.to_integer(s)
  end

  # CLI dispatch - parses args and calls compute
  deft main(args :: list(binary)) :: atom do
    # Define Shape ADT with three variants
    defdata(
      shape ::
        rectangle(number, number)
        | square(number)
        | triangle(number, number)
    )

    # Area function using pattern matching on ADT variants
    area = fn s :: shape ->
      case s do
        rectangle(w, h) -> w * h
        square(side) -> side * side
        triangle(base, height) -> base * height / 2
      end
    end

    # Perimeter function using pattern matching
    perimeter = fn s :: shape ->
      case s do
        rectangle(w, h) -> 2 * (w + h)
        square(side) -> 4 * side
        triangle(base, height) -> base + height + height
      end
    end

    {shape, f} =
      case args do
        ["rectangle", "area", w, h] -> {rectangle(parse(w), parse(h)), area}
        ["rectangle", "perimeter", w, h] -> {rectangle(parse(w), parse(h)), perimeter}
        ["square", "area", s] -> {square(parse(s)), area}
        ["square", "perimeter", s] -> {square(parse(s)), perimeter}
        ["triangle", "area", b, h] -> {triangle(parse(b), parse(h)), area}
      end

    result = f.(shape)

    IO.puts(result)
  end
end

# Only run CLI when invoked directly (not when compiled as a dependency)
if System.argv() != [] and hd(System.argv()) in ~w[rectangle square triangle] do
  Examples.Geometry.main(System.argv())
end
