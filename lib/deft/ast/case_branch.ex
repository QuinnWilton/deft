defmodule Deft.AST.CaseBranch do
  @moduledoc """
  Represents a single branch in a case expression.

  Each branch contains a pattern to match against and a body expression
  to evaluate when the pattern matches.
  """

  use Deft.AST.Node, fields: [:pattern, :body], children: [:pattern, :body]

  alias Deft.AST

  def new(pattern, body, meta \\ []) do
    %__MODULE__{pattern: pattern, body: body, meta: meta}
  end

  defimpl AST do
    def to_raw_ast(node) do
      pattern = @protocol.to_raw_ast(node.pattern)
      body = @protocol.to_raw_ast(node.body)

      {:->, node.meta, [[pattern], body]}
    end
  end
end
