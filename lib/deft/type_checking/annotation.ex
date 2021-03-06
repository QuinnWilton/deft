defmodule Deft.TypeChecking.Annotation do
  import Deft.Helpers

  alias Deft.AST
  alias Deft.PatternMatching

  def type_check(%AST.Annotation{} = annotation, env, opts) do
    {pattern, pattern_type, bindings} =
      PatternMatching.handle_pattern(annotation.pattern, annotation.type, env, opts)

    pattern
    |> annotate_type(pattern_type)
    |> annotate_bindings(bindings)
  end
end
