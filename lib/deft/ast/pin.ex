defmodule Deft.AST.Pin do
  alias Deft.AST

  @enforce_keys [:expr, :meta]
  defstruct @enforce_keys

  def new(expr, meta \\ []) do
    %__MODULE__{
      expr: expr,
      meta: meta
    }
  end

  defimpl AST do
    def to_raw_ast(node) do
      expr = @protocol.to_raw_ast(node.expr)

      {:^, node.meta, [expr]}
    end
  end
end
