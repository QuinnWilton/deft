defmodule Deft.AST.Case do
  @enforce_keys [:subject, :branches, :meta]
  defstruct @enforce_keys

  def new(subject, branches, meta \\ []) do
    %__MODULE__{
      subject: subject,
      branches: branches,
      meta: meta
    }
  end
end
