defmodule Deft.AST.Pin do
  use Deft.AST.Node, fields: [:expr], children: [:expr]

  alias Deft.AST

  def new(expr, meta \\ []) do
    %__MODULE__{expr: expr, meta: meta}
  end

  defimpl AST do
    def to_raw_ast(node) do
      expr = @protocol.to_raw_ast(node.expr)

      {:^, node.meta, [expr]}
    end
  end
end
