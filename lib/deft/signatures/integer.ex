defmodule Deft.Signatures.Integer do
  @moduledoc """
  Type signatures for Integer module functions.
  """

  use Deft.Signatures.DSL, for: Integer

  # ============================================================================
  # Digit Operations
  # ============================================================================

  sig digits(integer) :: [integer]
  sig digits(integer, integer) :: [integer]
  sig undigits([integer]) :: integer
  sig undigits([integer], integer) :: integer

  # ============================================================================
  # Conversion
  # ============================================================================

  sig to_string(integer) :: binary
  sig to_string(integer, integer) :: binary
  sig to_charlist(integer) :: [integer]
  sig to_charlist(integer, integer) :: [integer]

  # ============================================================================
  # Math Operations
  # ============================================================================

  sig gcd(integer, integer) :: integer
  sig pow(integer, integer) :: integer
  sig mod(integer, integer) :: integer
  sig floor_div(integer, integer) :: integer

  # ============================================================================
  # Predicates
  # ============================================================================

  sig is_even(integer) :: boolean
  sig is_odd(integer) :: boolean

  # ============================================================================
  # Extended Euclidean Algorithm
  # ============================================================================

  sig extended_gcd(integer, integer) :: {integer, integer, integer}

  # ============================================================================
  # Unsupported - Tagged tuple returns
  # ============================================================================

  sig_unsupported(parse(binary) :: top,
    reason: "Returns {integer, binary} | :error which requires literal atom types"
  )

  sig_unsupported(parse(binary, integer) :: top,
    reason: "Returns {integer, binary} | :error which requires literal atom types"
  )
end
