defmodule Deft.Subtyping.Variance do
  @moduledoc """
  Variance types for parameterized types.

  - `:covariant` - Subtyping follows the same direction as the parameter
  - `:contravariant` - Subtyping goes in the opposite direction
  - `:invariant` - No subtyping relationship for the parameter
  """

  @type t :: :covariant | :contravariant | :invariant

  @doc """
  Composes two variances when nesting type constructors.
  """
  @spec compose(t(), t()) :: t()
  def compose(:covariant, v), do: v
  def compose(:contravariant, :covariant), do: :contravariant
  def compose(:contravariant, :contravariant), do: :covariant
  def compose(:contravariant, :invariant), do: :invariant
  def compose(:invariant, _), do: :invariant
end
