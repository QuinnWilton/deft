defmodule Deft.Datatypes.Core do
  @moduledoc """
  Standard library ADTs for Deft.

  Provides common algebraic data types:
  - `option(a)` - Optional values (some/none)
  - `result(t, e)` - Success/error results (ok/err)
  - `fetch_result(t)` - Success/bare-error results (ok/error)

  ## Usage

  These ADTs are included by default in `Deft.TypeSystem.Default`.
  You can also include them explicitly:

      defmodule MyApp.TypeSystem do
        use Deft.TypeSystem

        include_datatypes Deft.Datatypes.Core
      end

  ## Result Types

  There are two result types for different Elixir conventions:

  - `result(t, e)` - For functions returning `{:ok, t} | {:error, e}`
    Examples: `File.read/1`, `GenServer.call/2`

  - `fetch_result(t)` - For functions returning `{:ok, t} | :error`
    Examples: `Enum.fetch/2`, `Integer.parse/1`

  ## Examples

      # Using option
      deft wrap(x :: integer) :: option(integer) do
        some(x)
      end

      deft unwrap_or(opt :: option(a), default :: a) :: a do
        case opt do
          some(x) -> x
          none -> default
        end
      end

      # Using result (for {:ok, t} | {:error, e} functions)
      deft read_file(path :: binary) :: result(binary, atom) do
        File.read(path)
      end

      # Using fetch_result (for {:ok, t} | :error functions)
      deft safe_get(list :: [integer], idx :: integer) :: integer do
        case Enum.fetch(list, idx) do
          ok(v) -> v
          error -> 0
        end
      end
  """

  use Deft.Datatypes

  defdata option(a) :: some(a) | none
  defdata result(t, e) :: ok(t) | err(e)
  defdata fetch_result(t) :: ok(t) | error
end
