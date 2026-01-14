defmodule Deft.AST.CondBranch do
  @moduledoc """
  Represents a single branch in a cond expression.

  Each branch contains a predicate expression and a body expression.
  The body is evaluated if the predicate evaluates to true.
  """

  use Deft.AST.Node, fields: [:predicate, :body], children: [:predicate, :body]

  alias Deft.AST

  def new(predicate, body, meta \\ []) do
    %__MODULE__{predicate: predicate, body: body, meta: meta}
  end

  defimpl AST do
    def to_raw_ast(node) do
      predicate = @protocol.to_raw_ast(node.predicate)
      body = @protocol.to_raw_ast(node.body)

      {:->, node.meta, [[predicate], body]}
    end
  end
end
