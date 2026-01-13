defmodule Deft.Signatures.String do
  @moduledoc """
  Type signatures for String module functions.

  Most String functions can be precisely typed since they operate on
  and return binary types.

  ## FFI Conversion

  Some functions return `grapheme | nil` in Elixir. These are typed using
  `option(binary)` with automatic FFI conversion at call boundaries:

  - `at/2`, `first/1`, `last/1` → `option(binary)`
  """

  use Deft.Signatures.DSL, for: String

  # ============================================================================
  # Predicates
  # ============================================================================

  sig contains?(binary, binary) :: boolean
  sig starts_with?(binary, binary) :: boolean
  sig ends_with?(binary, binary) :: boolean
  sig valid?(binary) :: boolean
  sig printable?(binary) :: boolean
  sig printable?(binary, integer) :: boolean
  sig equivalent?(binary, binary) :: boolean

  # ============================================================================
  # Length / Size
  # ============================================================================

  sig length(binary) :: integer
  sig byte_size(binary) :: integer

  # ============================================================================
  # Case Conversion
  # ============================================================================

  sig downcase(binary) :: binary
  sig upcase(binary) :: binary
  sig capitalize(binary) :: binary

  # ============================================================================
  # Trimming
  # ============================================================================

  sig trim(binary) :: binary
  sig trim_leading(binary) :: binary
  sig trim_trailing(binary) :: binary
  sig trim(binary, binary) :: binary
  sig trim_leading(binary, binary) :: binary
  sig trim_trailing(binary, binary) :: binary

  # ============================================================================
  # Padding
  # ============================================================================

  sig pad_leading(binary, integer) :: binary
  sig pad_leading(binary, integer, binary) :: binary
  sig pad_trailing(binary, integer) :: binary
  sig pad_trailing(binary, integer, binary) :: binary

  # ============================================================================
  # Splitting
  # ============================================================================

  sig split(binary) :: [binary]
  sig split(binary, binary) :: [binary]
  sig split_at(binary, integer) :: {binary, binary}
  sig graphemes(binary) :: [binary]
  sig codepoints(binary) :: [binary]
  sig to_charlist(binary) :: [integer]
  sig next_codepoint(binary) :: {binary, binary}
  sig next_grapheme(binary) :: {binary, binary}

  # ============================================================================
  # Duplication / Repetition
  # ============================================================================

  sig duplicate(binary, integer) :: binary

  # ============================================================================
  # Replacement
  # ============================================================================

  sig replace(binary, binary, binary) :: binary
  sig replace_prefix(binary, binary, binary) :: binary
  sig replace_suffix(binary, binary, binary) :: binary
  sig replace_leading(binary, binary, binary) :: binary
  sig replace_trailing(binary, binary, binary) :: binary
  sig reverse(binary) :: binary

  # ============================================================================
  # Slicing
  # ============================================================================

  sig slice(binary, integer, integer) :: binary

  # ============================================================================
  # Conversion
  # ============================================================================

  sig to_integer(binary) :: integer
  sig to_integer(binary, integer) :: integer
  sig to_float(binary) :: float
  sig to_atom(binary) :: atom
  sig to_existing_atom(binary) :: atom

  # ============================================================================
  # Normalization
  # ============================================================================

  sig normalize(binary, atom) :: binary

  # ============================================================================
  # Optional returns (grapheme | nil) - with FFI conversion
  # ============================================================================

  # Returns grapheme | nil, converted to option(binary) at FFI boundary
  sig at(binary, integer) :: option(binary)
  sig first(binary) :: option(binary)
  sig last(binary) :: option(binary)

  # ============================================================================
  # Unsupported - Complex nullable returns
  # ============================================================================

  sig_unsupported(next_grapheme_size(binary) :: top,
    reason: "Returns {size, rest} | nil which cannot be precisely typed as option"
  )

  # ============================================================================
  # Unsupported - Regex patterns
  # ============================================================================

  sig_unsupported(match?(binary, top) :: top,
    reason: "Second argument is Regex type which Deft cannot represent"
  )

  sig_unsupported(replace(binary, top, binary, top) :: top,
    reason: "Pattern argument can be Regex which Deft cannot represent"
  )

  sig_unsupported(split(binary, top, top) :: top,
    reason: "Pattern argument can be Regex which Deft cannot represent"
  )
end
