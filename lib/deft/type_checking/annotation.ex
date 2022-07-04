defmodule Deft.TypeChecking.Annotation do
  import Deft.TypeChecking

  alias Deft.AST

  def type_check(%AST.Annotation{} = annotation, env) do
    local = erase_types(annotation.name, env)

    annotate(local, annotation.type)
  end
end
