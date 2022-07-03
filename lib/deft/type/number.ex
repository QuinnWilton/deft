defmodule Deft.Type.Number do
  alias Deft.Type

  @enforce_keys []
  defstruct @enforce_keys

  def new() do
    %__MODULE__{}
  end

  defimpl Deft.Type do
    def subtype_of?(_t1, t2)
        when is_struct(t2, Type.Number)
        when is_struct(t2, Type.Integer)
        when is_struct(t2, Type.Float) do
      true
    end

    def subtype_of?(_t1, _t2) do
      false
    end
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(_t, _opts) do
      string("number")
    end
  end
end
