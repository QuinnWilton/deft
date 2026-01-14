defmodule Deft.AST.Pin do
  @moduledoc """
  Represents a pin operator in the AST.

  The pin operator `^` matches against an existing variable's value
  rather than rebinding it. Used in patterns to assert equality.
  """

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
