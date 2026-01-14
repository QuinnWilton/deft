defmodule Deft.AST.Annotation do
  @moduledoc """
  Represents a type annotation expression in the AST.

  Annotations attach type information to patterns using the `::` operator.
  For example, `x :: integer` annotates variable `x` with the integer type.
  """

  use Deft.AST.Node, fields: [:pattern, :type], children: [:pattern, :type]

  alias Deft.AST

  def new(pattern, type, meta \\ []) do
    %__MODULE__{pattern: pattern, type: type, meta: meta}
  end

  defimpl AST do
    def to_raw_ast(node) do
      pattern = @protocol.to_raw_ast(node.pattern)
      type = @protocol.to_raw_ast(node.type)

      {:"::", node.meta, [pattern, type]}
    end
  end
end
