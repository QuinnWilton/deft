defmodule Deft.Datatypes.Core do
  @moduledoc """
  Standard library ADTs for Deft.

  Provides common algebraic data types:
  - `option(a)` - Optional values (some/none)
  - `result(t, e)` - Success/error results (ok/err)

  ## Usage

  These ADTs are included by default in `Deft.TypeSystem.Default`.
  You can also include them explicitly:

      defmodule MyApp.TypeSystem do
        use Deft.TypeSystem

        include_datatypes Deft.Datatypes.Core
      end

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

      # Using result
      deft divide(x :: integer, y :: integer) :: result(integer, atom) do
        case y do
          0 -> err(:division_by_zero)
          _ -> ok(div(x, y))
        end
      end
  """

  use Deft.Datatypes

  defdata option(a) :: some(a) | none
  defdata result(t, e) :: ok(t) | err(e)
end
