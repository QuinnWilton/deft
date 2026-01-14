defmodule Deft.AST.Cond do
  @moduledoc """
  Represents a cond expression in the AST.

  Cond expressions evaluate boolean predicates in order, executing
  the body of the first branch whose predicate evaluates to true.
  """

  use Deft.AST.Node, fields: [:branches], children: [:branches]

  alias Deft.AST

  def new(branches, meta \\ []) do
    %__MODULE__{branches: branches, meta: meta}
  end

  defimpl AST do
    def to_raw_ast(node) do
      branches = Enum.map(node.branches, &@protocol.to_raw_ast/1)

      {:cond, node.meta, [[do: branches]]}
    end
  end
end
