defmodule Deft.AST.If do
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
end
