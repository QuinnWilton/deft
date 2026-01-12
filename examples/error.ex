# Error examples for Deft type checking
#
# This file demonstrates various error types and their formatted output.
# Run with: elixir examples/error.ex [error_type]
#
# Available error types:
#   return_type    - E0001: Return type mismatch
#   operator       - E0001: Operator type mismatch
#   inexhaustive   - E0005: Inexhaustive patterns
#   unreachable    - E0006: Unreachable branch
#   unsupported    - E0004: Unsupported call

error_type = List.first(System.argv()) || "return_type"

case error_type do
  "return_type" ->
    defmodule Examples.ReturnTypeError do
      @moduledoc """
      E0001: Return type mismatch

      The function body returns a float, but the declared return type is integer.
      """
      use Deft

      deft compute() :: integer do
        # Declared to return integer, but returns a float
        1.5
      end

      deft main() :: atom do
        IO.puts(compute())
      end
    end

    Examples.ReturnTypeError.main()

  "operator" ->
    defmodule Examples.OperatorError do
      @moduledoc """
      E0001: Operator type mismatch

      The + operator requires number operands, but receives an atom.
      """
      use Deft

      deft compute(x :: atom) :: number do
        # The + operator requires number operands, but x is an atom
        x + 1
      end

      deft main() :: atom do
        IO.puts(compute(:foo))
      end
    end

    Examples.OperatorError.main()

  "inexhaustive" ->
    defmodule Examples.InexhaustiveError do
      @moduledoc """
      E0005: Inexhaustive patterns

      The case expression doesn't cover all variants of the ADT.
      """
      use Deft, features: [:exhaustiveness_checking]

      defdata option :: some(integer) | none()

      deft unwrap(opt :: option) :: integer do
        # Missing the none() case
        case opt do
          some(x) -> x
        end
      end

      deft main() :: atom do
        IO.puts(unwrap(some(42)))
      end
    end

    Examples.InexhaustiveError.main()

  "unreachable" ->
    defmodule Examples.UnreachableError do
      @moduledoc """
      E0006: Unreachable branch

      The pattern can never match the subject type.
      """
      use Deft

      deft classify(x :: integer) :: atom do
        case x do
          # integer can never match an atom pattern
          :foo -> :matched
          _ -> :default
        end
      end

      deft main() :: atom do
        IO.puts(classify(42))
      end
    end

    Examples.UnreachableError.main()

  "unsupported" ->
    defmodule Examples.UnsupportedCallError do
      @moduledoc """
      E0004: Unsupported call

      Calling a function without a registered type signature.
      """
      use Deft

      deft compute(x :: integer) :: integer do
        # unknown_function is not a supported builtin
        unknown_function(x)
      end

      deft main() :: atom do
        IO.puts(compute(42))
      end
    end

    Examples.UnsupportedCallError.main()

  other ->
    IO.puts("Unknown error type: #{other}")
    IO.puts("")
    IO.puts("Available types:")
    IO.puts("  return_type    - E0001: Return type mismatch")
    IO.puts("  operator       - E0001: Operator type mismatch")
    IO.puts("  inexhaustive   - E0005: Inexhaustive patterns")
    IO.puts("  unreachable    - E0006: Unreachable branch")
    IO.puts("  unsupported    - E0004: Unsupported call")
end
