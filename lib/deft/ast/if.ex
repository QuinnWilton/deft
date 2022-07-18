defmodule Deft.AST.If do
  alias Deft.AST

  @enforce_keys [:predicate, :do, :else, :meta]
  defstruct @enforce_keys

  def new(predicate, do_branch, else_branch \\ [], meta \\ []) do
    %__MODULE__{
      predicate: predicate,
      do: do_branch,
      else: else_branch,
      meta: meta
    }
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
