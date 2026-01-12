defmodule Deft.AST.Pair do
  alias Deft.AST

  @enforce_keys [:fst, :snd]
  defstruct [:fst, :snd, meta: []]

  def new(fst, snd, meta \\ []) do
    %__MODULE__{
      fst: fst,
      snd: snd,
      meta: meta
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
