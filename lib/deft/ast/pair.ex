defmodule Deft.AST.Pair do
  @moduledoc """
  Represents a two-element tuple (pair) in the AST.

  Pairs are the base case for tuple construction and are used
  internally for keyword lists and two-tuples like `{:ok, value}`.
  """

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
