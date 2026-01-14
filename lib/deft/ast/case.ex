defmodule Deft.AST.Case do
  @moduledoc """
  Represents a case expression in the AST.

  Case expressions match a subject value against multiple patterns,
  executing the body of the first matching branch.
  """

  use Deft.AST.Node, fields: [:subject, :branches], children: [:subject, :branches]

  alias Deft.AST

  def new(subject, branches, meta \\ []) do
    %__MODULE__{subject: subject, branches: branches, meta: meta}
  end

  defimpl AST do
    def to_raw_ast(node) do
      subject = @protocol.to_raw_ast(node.subject)
      branches = Enum.map(node.branches, &@protocol.to_raw_ast/1)

      {:case, node.meta, [subject, [do: branches]]}
    end
  end
end
