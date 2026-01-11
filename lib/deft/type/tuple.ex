defmodule Deft.Type.Tuple do
  use Deft.Subtyping.DSL

  @type t :: %__MODULE__{}

  @enforce_keys []
  defstruct @enforce_keys

  def new() do
    %__MODULE__{}
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(_t, _opts) do
      string("tuple")
    end
  end
end
