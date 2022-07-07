defmodule Deft.Type.Union do
  @enforce_keys [:fst, :snd]
  defstruct @enforce_keys

  def new(fst, snd) do
    %__MODULE__{
      fst: fst,
      snd: snd
    }
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(t, opts) do
      concat([
        Inspect.inspect(t.fst, opts),
        " | ",
        Inspect.inspect(t.snd, opts)
      ])
    end
  end
end
