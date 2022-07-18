defmodule Deft.AST.Literal do
  alias Deft.AST

  @enforce_keys [:value, :meta]
  defstruct @enforce_keys

  def new(value, meta \\ []) do
    %__MODULE__{
      value: value,
      meta: meta
    }
  end

  defimpl AST do
    def to_raw_ast(node) do
      node.value
    end
  end
end
