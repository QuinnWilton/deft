defmodule Deft.Subtyping.Variance do
  @moduledoc """
  Variance types for parameterized types.

  - `:covariant` - Subtyping follows the same direction as the parameter
  - `:contravariant` - Subtyping goes in the opposite direction
  - `:invariant` - No subtyping relationship for the parameter
  """

  @type t :: :covariant | :contravariant | :invariant
end
