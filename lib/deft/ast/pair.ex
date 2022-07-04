defmodule Deft.AST.Pair do
  @enforce_keys [:fst, :snd]
  defstruct @enforce_keys

  def new(fst, snd) do
    %__MODULE__{
      fst: fst,
      snd: snd
    }
  end
end
