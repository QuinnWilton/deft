defmodule Deft.Signatures.Float do
  @moduledoc """
  Type signatures for Float module functions.
  """

  use Deft.Signatures.DSL, for: Float

  # ============================================================================
  # Rounding
  # ============================================================================

  sig ceil(float, integer) :: float
  sig floor(float, integer) :: float
  sig round(float, integer) :: float

  # ============================================================================
  # Conversion
  # ============================================================================

  sig to_string(float) :: binary
  sig to_charlist(float) :: [integer]
  sig ratio(float) :: {integer, integer}

  # ============================================================================
  # Predicates
  # ============================================================================

  sig finite?(float) :: boolean

  # ============================================================================
  # Unsupported - Tagged tuple returns
  # ============================================================================

  sig_unsupported(parse(binary) :: top,
    reason: "Returns {float, binary} | :error which requires literal atom types"
  )

  # ============================================================================
  # Unsupported - Atom returns
  # ============================================================================

  sig_unsupported(min_finite() :: top,
    reason: "Returns float or :neg_infinity which requires literal atom types"
  )

  sig_unsupported(max_finite() :: top,
    reason: "Returns float or :infinity which requires literal atom types"
  )
end
