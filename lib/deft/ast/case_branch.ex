defmodule Deft.AST.CaseBranch do
  @enforce_keys [:pattern, :body, :meta]
  defstruct @enforce_keys

  def new(pattern, body, meta \\ []) do
    %__MODULE__{
      pattern: pattern,
      body: body,
      meta: meta
    }
  end
end
