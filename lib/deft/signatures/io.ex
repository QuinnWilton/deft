defmodule Deft.Signatures.IO do
  @moduledoc """
  Type signatures for IO module functions.
  """

  use Deft.Signatures.DSL, for: IO

  sig(puts(top) :: atom)
end
