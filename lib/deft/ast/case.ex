defmodule Deft.AST.Case do
  alias Deft.AST

  @enforce_keys [:subject, :branches, :meta]
  defstruct @enforce_keys

  def new(subject, branches, meta \\ []) do
    %__MODULE__{
      subject: subject,
      branches: branches,
      meta: meta
    }
  end

  defimpl AST do
    def to_raw_ast(node) do
      subject = @protocol.to_raw_ast(node.subject)
      branches = Enum.map(node.branches, &@protocol.to_raw_ast/1)

      {:case, node.meta, [subject, [do: branches]]}
    end
  end
end
