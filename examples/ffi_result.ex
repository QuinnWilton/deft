# FFI Result Types Example
# Demonstrates calling native Elixir functions that return result types

defmodule Examples.FFIResult do
  @moduledoc """
  Demonstrates Deft's FFI conversion for result and option types.

  When calling external Elixir functions with signatures that return
  `option(a)`, `result(t, e)`, or `fetch_result(t)`, Deft automatically
  converts the native Elixir return values to ADT representations:

  For option(a):
    nil   -> {:none}
    value -> {:some, value}

  For result(t, e) - functions returning {:ok, t} | {:error, e}:
    {:ok, value}     -> {:ok, value}
    {:error, reason} -> {:err, reason}

  For fetch_result(t) - functions returning {:ok, t} | :error:
    {:ok, value} -> {:ok, value}
    :error       -> {:error}

  This allows type-safe pattern matching on nullable or failable operations.
  """

  use Deft

  # ============================================================================
  # Fetch result type examples (Enum.fetch returns {:ok, v} | :error)
  # ============================================================================

  # Safely get an element from a list by index
  deft safe_get(list :: [integer], index :: integer) :: integer do
    case Enum.fetch(list, index) do
      ok(value) -> value
      error -> 0
    end
  end

  # ============================================================================
  # Option type examples (Enum.find, List.first, etc. return value | nil)
  # ============================================================================

  # Find the first element matching a predicate
  deft find_positive(list :: [integer]) :: integer do
    case Enum.find(list, fn x :: integer -> x > 0 end) do
      some(value) -> value
      none -> 0
    end
  end

  # Get the first element of a list safely
  deft head_or_default(list :: [integer], default :: integer) :: integer do
    case List.first(list) do
      some(value) -> value
      none -> default
    end
  end

  # Get the first character of a string safely
  deft first_char(s :: binary) :: binary do
    case String.first(s) do
      some(c) -> c
      none -> ""
    end
  end

  # ============================================================================
  # Chaining failable operations
  # ============================================================================

  # Chain multiple fetch_result operations
  deft nested_fetch(list :: [integer], idx1 :: integer, _idx2 :: integer) :: integer do
    case Enum.fetch(list, idx1) do
      ok(intermediate) ->
        case Enum.fetch([100, 200, 300, 400, 500], intermediate) do
          ok(final) -> final
          error -> 0
        end

      error ->
        0
    end
  end

  # Combine option and fetch_result operations
  deft find_and_fetch(list :: [integer], target :: integer) :: integer do
    case Enum.find_index(list, fn x :: integer -> x == target end) do
      some(idx) ->
        case Enum.fetch([10, 20, 30, 40, 50], idx) do
          ok(value) -> value
          error -> 0
        end

      none ->
        0
    end
  end

  # ============================================================================
  # Demo functions
  # ============================================================================

  deft demo_fetch_result() :: atom do
    list = [10, 20, 30, 40, 50]
    IO.puts("=== Fetch result type (Enum.fetch) ===")
    IO.puts("Fetching index 2 from [10,20,30,40,50]:")
    IO.puts(safe_get(list, 2))
    IO.puts("Fetching index 10 (out of bounds):")
    IO.puts(safe_get(list, 10))
  end

  deft demo_option() :: atom do
    IO.puts("=== Option type (Enum.find) ===")
    IO.puts("Finding positive in [-3, -1, 0, 5, 2]:")
    IO.puts(find_positive([-3, -1, 0, 5, 2]))
    IO.puts("Finding positive in [-3, -1, -2]:")
    IO.puts(find_positive([-3, -1, -2]))
  end

  deft demo_list_first() :: atom do
    IO.puts("=== Option type (List.first) ===")
    IO.puts("Head of [1, 2, 3] or default 99:")
    IO.puts(head_or_default([1, 2, 3], 99))
    IO.puts("Head of [] or default 99:")
    IO.puts(head_or_default([], 99))
  end

  deft demo_string() :: atom do
    IO.puts("=== Option type (String.first) ===")
    IO.puts("First char of 'hello':")
    IO.puts(first_char("hello"))
    IO.puts("First char of empty string:")
    result = first_char("")

    if result == "" do
      IO.puts("(empty)")
    else
      IO.puts(result)
    end
  end

  deft demo_chained() :: atom do
    IO.puts("=== Chained operations ===")
    IO.puts("nested_fetch([0,1,2,3], 2, 0) - fetches list[2]=2, then [100..500][2]=300:")
    IO.puts(nested_fetch([0, 1, 2, 3], 2, 0))
    IO.puts("find_and_fetch([5,10,15,20], 15) - finds idx=2, then fetches [10..50][2]=30:")
    IO.puts(find_and_fetch([5, 10, 15, 20], 15))
  end

  deft main() :: atom do
    demo_fetch_result()
    IO.puts("")
    demo_option()
    IO.puts("")
    demo_list_first()
    IO.puts("")
    demo_string()
    IO.puts("")
    demo_chained()
  end
end

# Run when invoked directly
case System.argv() do
  [] -> Examples.FFIResult.main()
  _ -> :ok
end
