defmodule Deft.AST.Pair do
  alias Deft.AST

  @enforce_keys [:fst, :snd]
  defstruct @enforce_keys

  def new(fst, snd) do
    %__MODULE__{
      fst: fst,
      snd: snd
    }
  end

  defimpl AST do
    def to_raw_ast(node) do
      fst = @protocol.to_raw_ast(node.fst)
      snd = @protocol.to_raw_ast(node.snd)

      {fst, snd}
    end
  end
end
