defmodule Deft.AST.Block do
  @moduledoc """
  Represents a sequence of expressions in the AST.

  Blocks contain multiple expressions that are evaluated in order,
  with the final expression's value becoming the block's value.
  """

  use Deft.AST.Node, fields: [:exprs], children: [:exprs]

  alias Deft.AST

  def new(exprs, meta \\ []) do
    %__MODULE__{exprs: exprs, meta: meta}
  end

  defimpl AST do
    def to_raw_ast(node) do
      exprs = Enum.map(node.exprs, &@protocol.to_raw_ast/1)

      {:__block__, node.meta, exprs}
    end
  end
end
