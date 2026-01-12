defmodule Deft.Error.Exception do
  @moduledoc """
  Exception wrapper for Deft.Error.

  This module wraps a structured `Deft.Error` in an exception that can
  be raised and caught using Elixir's standard exception mechanisms.

  ## Usage

      error = Deft.Error.type_mismatch(expected: Type.integer(), actual: Type.float())
      raise Deft.Error.to_exception(error)

  Or directly:

      Deft.Error.raise!(error)
  """

  defexception [:error]

  @type t :: %__MODULE__{error: Deft.Error.t()}

  @impl true
  def message(%__MODULE__{error: error}) do
    Deft.Error.Formatter.format(error, colors: true)
  end
end
