defmodule Deft.AST.Cons do
  @enforce_keys [:head, :rest, :meta]
  defstruct @enforce_keys

  def new(head, rest, meta \\ []) do
    %__MODULE__{
      head: head,
      rest: rest,
      meta: meta
    }
  end
end
