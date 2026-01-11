defmodule Deft.Type.Binary do
  @moduledoc """
  Represents the binary (string) type in the Deft type system.
  """

  use Deft.Subtyping.DSL

  alias Deft.AST

  @type t :: %__MODULE__{}

  @enforce_keys []
  defstruct @enforce_keys

  def new() do
    %__MODULE__{}
  end

  defimpl AST do
    def to_raw_ast(_type) do
      {:binary, [], nil}
    end
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(_t, _opts) do
      string("binary")
    end
  end
end
