defmodule Deft.AST.If do
  @moduledoc """
  Represents an if expression in the AST.

  If expressions evaluate a boolean predicate and execute either
  the do branch or else branch. The result type is the union of
  both branch types.
  """

  use Deft.AST.Node, fields: [:predicate, :do, :else], children: [:predicate, :do, :else]

  alias Deft.AST

  def new(predicate, do_branch, else_branch \\ [], meta \\ []) do
    %__MODULE__{predicate: predicate, do: do_branch, else: else_branch, meta: meta}
  end

  defimpl AST do
    def to_raw_ast(node) do
      predicate = @protocol.to_raw_ast(node.predicate)
      do_branch = @protocol.to_raw_ast(node.do)
      else_branch = @protocol.to_raw_ast(node.else)

      {:if, node.meta, [predicate, [do: do_branch, else: else_branch]]}
    end
  end
end
