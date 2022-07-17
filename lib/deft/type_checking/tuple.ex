defmodule Deft.TypeChecking.Tuple do
  import Deft.Helpers

  alias Deft.AST
  alias Deft.Type

  def type_check(%AST.Tuple{} = tuple, env, opts) do
    {elements, element_ts, bindings} = compute_and_erase_types(tuple.elements, env, opts)

    {:{}, tuple.meta, elements}
    |> annotate_type(Type.fixed_tuple(element_ts))
    |> annotate_bindings(bindings)
  end
end
