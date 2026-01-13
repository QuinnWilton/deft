defmodule Deft.Signatures.List do
  @moduledoc """
  Type signatures for List module functions.

  These complement the list operations in Kernel (`hd`, `tl`, `++`, etc.)
  with additional List-specific functions.

  ## FFI Conversion

  Some functions return `value | nil` in Elixir. These are typed using
  `option(a)` with automatic FFI conversion at call boundaries:

  - `first/1`, `last/1` → `option(a)`
  """

  use Deft.Signatures.DSL, for: List

  # ============================================================================
  # Construction
  # ============================================================================

  sig duplicate(a, integer) :: [a]
  sig wrap(a) :: [a]

  # ============================================================================
  # Modification
  # ============================================================================

  sig insert_at([a], integer, a) :: [a]
  sig replace_at([a], integer, a) :: [a]
  sig update_at([a], integer, (a -> a)) :: [a]
  sig delete_at([a], integer) :: [a]
  sig delete([a], a) :: [a]

  # ============================================================================
  # Flattening
  # ============================================================================

  sig flatten([top]) :: [top]
  sig flatten([top], integer) :: [top]

  # ============================================================================
  # Folding
  # ============================================================================

  sig foldl([a], b, (a, b -> b)) :: b
  sig foldr([a], b, (a, b -> b)) :: b

  # ============================================================================
  # Conversion
  # ============================================================================

  sig to_string([integer]) :: binary
  sig to_integer([integer]) :: integer
  sig to_integer([integer], integer) :: integer
  sig to_float([integer]) :: float
  sig to_atom([integer]) :: atom
  sig to_existing_atom([integer]) :: atom
  sig to_tuple([a]) :: tuple

  # ============================================================================
  # Predicates
  # ============================================================================

  sig ascii_printable?([integer]) :: boolean
  sig ascii_printable?([integer], integer) :: boolean

  # ============================================================================
  # Zipping
  # ============================================================================

  sig zip([[a]]) :: [[a]]

  # ============================================================================
  # Keylist Operations (approximated - no keyword type)
  # ============================================================================

  sig keyfind([tuple], a, integer) :: tuple
  sig keyreplace([tuple], a, integer, tuple) :: [tuple]
  sig keystore([tuple], a, integer, tuple) :: [tuple]
  sig keydelete([tuple], a, integer) :: [tuple]
  sig keytake([tuple], a, integer) :: {tuple, [tuple]}
  sig keymember?([tuple], a, integer) :: boolean
  sig keysort([tuple], integer) :: [tuple]

  # ============================================================================
  # Optional returns (value | nil) - with FFI conversion
  # ============================================================================

  # Returns value | nil, converted to option(a) at FFI boundary
  sig first([a]) :: option(a)
  sig last([a]) :: option(a)

  # ============================================================================
  # Unsupported - Nullable returns with defaults
  # ============================================================================

  sig_unsupported(first([a], b) :: top,
    reason: "Return type is value | default which cannot be precisely typed"
  )

  sig_unsupported(last([a], b) :: top,
    reason: "Return type is value | default which cannot be precisely typed"
  )

  sig_unsupported(pop_at([a], integer) :: top,
    reason: "Returns {value, list} | {nil, list} which cannot be precisely typed"
  )

  sig_unsupported(pop_at([a], integer, b) :: top,
    reason: "Return type depends on whether index is valid"
  )

  # ============================================================================
  # Unsupported - Complex returns
  # ============================================================================

  sig_unsupported(myers_difference([a], [a]) :: top,
    reason: "Returns keyword list of edit operations which Deft cannot represent"
  )

  sig_unsupported(myers_difference([a], [a], (a, a -> top)) :: top,
    reason: "Returns keyword list of edit operations which Deft cannot represent"
  )

  sig_unsupported(starts_with?([a], [a]) :: top,
    reason: "Returns boolean or raises, behavior depends on input types"
  )
end
