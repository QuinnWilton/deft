defmodule Deft.TypeChecking.Annotation do
  import Deft.Helpers

  alias Deft.AST

  def type_check(%AST.Annotation{} = annotation, env) do
    {local, _, local_bindings} = compute_and_erase_types(annotation.name, env)

    bindings = local_bindings ++ [{annotation.name, annotation.type}]

    local
    |> annotate_type(annotation.type)
    |> annotate_bindings(bindings)
  end
end
