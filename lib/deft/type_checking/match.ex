defmodule Deft.TypeChecking.Match do
  import Deft.Helpers

  alias Deft.AST
  alias Deft.PatternMatching
  alias Deft.Subtyping
  alias Deft.Type

  def type_check(%AST.Match{} = match, env) do
    {value, value_t, value_bindings} =
      compute_and_erase_types(
        match.value,
        env
      )

    {pattern, _, pattern_bindings} =
      PatternMatching.handle_pattern(
        match.pattern,
        value_t,
        env
      )

    bindings = value_bindings ++ pattern_bindings

    {:=, match.meta, [pattern, value]}
    |> annotate_type(value_t)
    |> annotate_bindings(bindings)
  end
end
