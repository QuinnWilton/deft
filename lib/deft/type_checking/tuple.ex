defmodule Deft.TypeChecking.Tuple do
  import Deft.TypeChecking

  alias Deft.AST
  alias Deft.Type

  def type_check(%AST.Tuple{} = tuple, env) do
    {elements, element_ts} = compute_and_erase_types(tuple.elements, env)

    annotate({:{}, tuple.meta, elements}, Type.tuple(element_ts))
  end
end
