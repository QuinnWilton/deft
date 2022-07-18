defmodule Deft.AST.Cond do
  alias Deft.AST

  @enforce_keys [:branches, :meta]
  defstruct @enforce_keys

  def new(branches, meta \\ []) do
    %__MODULE__{
      branches: branches,
      meta: meta
    }
  end

  defimpl AST do
    def to_raw_ast(node) do
      branches = Enum.map(node.branches, &@protocol.to_raw_ast/1)

      {:cond, node.meta, [[do: branches]]}
    end
  end
end
