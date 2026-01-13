defmodule Deft.Signatures.Enum do
  @moduledoc """
  Type signatures for Enum module functions.

  Functions that can be precisely typed use `sig`, while functions that
  require types Deft cannot represent use `sig_unsupported` with an
  explanation.

  ## Unsupported Categories

  - **Map returns**: `group_by/2`, `frequencies/1` - no map type
  - **Tagged tuples**: `fetch/2` - returns `{:ok, v} | :error`
  - **Nullable returns**: `find/2`, `at/2` - return `v | nil`
  - **Raises on empty**: `max/1`, `min/1`, `reduce/2` - behavior depends on input
  """

  use Deft.Signatures.DSL, for: Enum

  # ============================================================================
  # Predicates (returning boolean)
  # ============================================================================

  sig all?([a], (a -> boolean)) :: boolean
  sig any?([a], (a -> boolean)) :: boolean
  sig empty?([a]) :: boolean
  sig member?([a], a) :: boolean

  # ============================================================================
  # Transformations (list -> list)
  # ============================================================================

  sig map([a], (a -> b)) :: [b]
  sig filter([a], (a -> boolean)) :: [a]
  sig reject([a], (a -> boolean)) :: [a]
  sig flat_map([a], (a -> [b])) :: [b]
  sig reverse([a]) :: [a]
  sig reverse([a], [b]) :: [a | b]
  sig sort([a]) :: [a]
  sig sort([a], (a, a -> boolean)) :: [a]
  sig sort_by([a], (a -> b)) :: [a]
  sig sort_by([a], (a -> b), (b, b -> boolean)) :: [a]
  sig uniq([a]) :: [a]
  sig uniq_by([a], (a -> b)) :: [a]
  sig dedup([a]) :: [a]
  sig dedup_by([a], (a -> b)) :: [a]
  sig take([a], integer) :: [a]
  sig drop([a], integer) :: [a]
  sig take_while([a], (a -> boolean)) :: [a]
  sig drop_while([a], (a -> boolean)) :: [a]
  sig take_every([a], integer) :: [a]
  sig drop_every([a], integer) :: [a]
  sig shuffle([a]) :: [a]
  sig intersperse([a], a) :: [a]
  sig zip([a], [b]) :: [{a, b}]
  sig zip_with([a], [b], (a, b -> c)) :: [c]
  sig concat([[a]]) :: [a]
  sig concat([a], [b]) :: [a | b]
  sig scan([a], (a, a -> a)) :: [a]
  sig scan([a], b, (a, b -> b)) :: [b]
  sig slide([a], integer, integer) :: [[a]]
  sig chunk_every([a], integer) :: [[a]]
  sig chunk_every([a], integer, integer) :: [[a]]
  sig chunk_by([a], (a -> b)) :: [[a]]

  # ============================================================================
  # Reductions (list -> value)
  # ============================================================================

  sig reduce([a], b, (a, b -> b)) :: b
  sig count([a]) :: integer
  sig count([a], (a -> boolean)) :: integer
  sig sum([number]) :: number
  sig product([number]) :: number
  sig join([a]) :: binary
  sig join([a], binary) :: binary
  sig map_join([a], (a -> b)) :: binary
  sig map_join([a], binary, (a -> b)) :: binary

  # ============================================================================
  # Conversions
  # ============================================================================

  sig to_list([a]) :: [a]
  sig with_index([a]) :: [{a, integer}]
  sig with_index([a], integer) :: [{a, integer}]

  # ============================================================================
  # Splitting
  # ============================================================================

  sig split([a], integer) :: {[a], [a]}
  sig split_while([a], (a -> boolean)) :: {[a], [a]}
  sig split_with([a], (a -> boolean)) :: {[a], [a]}

  # ============================================================================
  # Iteration (side effects)
  # ============================================================================

  sig each([a], (a -> b)) :: atom

  # ============================================================================
  # Unsupported - Map/Keyword returns
  # ============================================================================

  sig_unsupported(group_by([a], (a -> b)) :: top,
    reason: "Returns a map type which Deft cannot represent"
  )

  sig_unsupported(group_by([a], (a -> b), (a -> c)) :: top,
    reason: "Returns a map type which Deft cannot represent"
  )

  sig_unsupported(frequencies([a]) :: top,
    reason: "Returns a map type which Deft cannot represent"
  )

  sig_unsupported(frequencies_by([a], (a -> b)) :: top,
    reason: "Returns a map type which Deft cannot represent"
  )

  sig_unsupported(into([a], top) :: top,
    reason: "Target collection type cannot be determined statically"
  )

  sig_unsupported(into([a], top, (a -> b)) :: top,
    reason: "Target collection type cannot be determined statically"
  )

  sig_unsupported(:map_intersperse, [[a], b, (a -> c)] :: top,
    reason: "Returns a map type which Deft cannot represent"
  )

  # ============================================================================
  # Unsupported - Tagged tuple returns
  # ============================================================================

  sig_unsupported(fetch([a], integer) :: top,
    reason: "Returns {:ok, value} | :error which requires literal atom types"
  )

  sig_unsupported(fetch!([a], integer) :: a,
    reason: "Raises on invalid index; prefer Enum.at/3 with default or handle error case"
  )

  # ============================================================================
  # Unsupported - Nullable returns (value | nil)
  # ============================================================================

  sig_unsupported(find([a], (a -> boolean)) :: top,
    reason: "Returns value | nil but Deft has no nil type; use Enum.filter/2 instead"
  )

  sig_unsupported(find([a], b, (a -> boolean)) :: top,
    reason: "Returns value | default but return type depends on whether element found"
  )

  sig_unsupported(find_value([a], (a -> b)) :: top,
    reason: "Returns value | nil but Deft has no nil type"
  )

  sig_unsupported(find_index([a], (a -> boolean)) :: top,
    reason: "Returns integer | nil but Deft has no nil type"
  )

  sig_unsupported(at([a], integer) :: top,
    reason: "Returns value | nil but Deft has no nil type; use Enum.fetch/2 or provide default"
  )

  sig_unsupported(at([a], integer, b) :: top,
    reason: "Return type is value | default which cannot be precisely typed"
  )

  sig_unsupported(random([a]) :: top,
    reason: "Raises on empty list; return type depends on input being non-empty"
  )

  # ============================================================================
  # Unsupported - Raises on empty list
  # ============================================================================

  sig_unsupported(max([a]) :: top,
    reason: "Raises on empty list; use Enum.max/3 with empty_fallback or check non-empty first"
  )

  sig_unsupported(min([a]) :: top,
    reason: "Raises on empty list; use Enum.min/3 with empty_fallback or check non-empty first"
  )

  sig_unsupported(max_by([a], (a -> b)) :: top,
    reason: "Raises on empty list; use Enum.max_by/4 with empty_fallback"
  )

  sig_unsupported(min_by([a], (a -> b)) :: top,
    reason: "Raises on empty list; use Enum.min_by/4 with empty_fallback"
  )

  sig_unsupported(reduce([a], (a, a -> a)) :: top,
    reason: "Raises on empty list; use Enum.reduce/3 with explicit accumulator"
  )
end
