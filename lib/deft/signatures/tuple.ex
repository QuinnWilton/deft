defmodule Deft.Signatures.Tuple do
  @moduledoc """
  Type signatures for Tuple module functions.

  Note: Most tuple operations in Elixir are in Kernel (`elem/2`, `put_elem/3`,
  `tuple_size/1`). This module covers the additional Tuple module functions.
  """

  use Deft.Signatures.DSL, for: Tuple

  # ============================================================================
  # Construction
  # ============================================================================

  sig append(tuple, top) :: tuple
  sig duplicate(top, integer) :: tuple
  sig insert_at(tuple, integer, top) :: tuple

  # ============================================================================
  # Modification
  # ============================================================================

  sig delete_at(tuple, integer) :: tuple

  # ============================================================================
  # Conversion
  # ============================================================================

  sig to_list(tuple) :: [top]

  # ============================================================================
  # Numeric Tuple Operations
  # ============================================================================

  sig product({number, number}) :: number
  sig sum({number, number}) :: number
end
