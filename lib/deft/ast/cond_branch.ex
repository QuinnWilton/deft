defmodule Deft.AST.CondBranch do
  @enforce_keys [:predicate, :body, :meta]
  defstruct @enforce_keys

  def new(predicate, body, meta \\ []) do
    %__MODULE__{
      predicate: predicate,
      body: body,
      meta: meta
    }
  end
end
