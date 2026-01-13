defmodule Deft.Signatures.String do
  @moduledoc """
  Type signatures for String module functions.
  """

  use Deft.Signatures.DSL, for: String

  sig(to_integer(binary) :: integer)
  sig(to_float(binary) :: float)
end
