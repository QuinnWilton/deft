# Geometry example using Deft type checking
# Demonstrates ADTs with defdata, pattern matching, and subtyping

defmodule Examples.Geometry do
  @moduledoc """
  Demonstrates type-checked geometric calculations using proper ADTs
  defined with defdata, pattern matching on variants, and subtyping
  (integer <: number).
  """

  use Deft

  # Define Shape ADT with three variants at module level
  defdata(
    shape ::
      rectangle(number, number)
      | square(number)
      | triangle(number, number)
  )

  # CLI dispatch - parses args and calls compute
  # Note: main can call parse_shape even though it's defined later
  deft main(args :: list(binary)) :: atom do
    {shape, f} = parse_shape(args)
    result = f.(shape)

    IO.puts(result)
  end

  # Helper functions
  deft parse(s :: binary) :: integer do
    String.to_integer(s)
  end

  deft parse_shape(args :: list(binary)) :: {shape, (shape -> number)} do
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

    case args do
      ["rectangle", "area", w, h] -> {rectangle(parse(w), parse(h)), area}
      ["rectangle", "perimeter", w, h] -> {rectangle(parse(w), parse(h)), perimeter}
      ["square", "area", s] -> {square(parse(s)), area}
      ["square", "perimeter", s] -> {square(parse(s)), perimeter}
      ["triangle", "area", b, h] -> {triangle(parse(b), parse(h)), area}
    end
  end
end

# Only run CLI when invoked directly (not when compiled as a dependency)
if System.argv() != [] and hd(System.argv()) in ~w[rectangle square triangle] do
  Examples.Geometry.main(System.argv())
end
