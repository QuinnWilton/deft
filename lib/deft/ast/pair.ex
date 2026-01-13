defmodule Deft.AST.Pair do
  use Deft.AST.Node, fields: [:fst, :snd], children: [:fst, :snd]

  alias Deft.AST

  def new(fst, snd, meta \\ []) do
    %__MODULE__{fst: fst, snd: snd, meta: meta}
  end

  defimpl AST do
    def to_raw_ast(node) do
      fst = @protocol.to_raw_ast(node.fst)
      snd = @protocol.to_raw_ast(node.snd)

      {fst, snd}
    end
  end
end
