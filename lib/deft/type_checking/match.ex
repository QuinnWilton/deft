defmodule Deft.TypeChecking.Match do
  import Deft.Helpers

  alias Deft.AST
  alias Deft.PatternMatching

  def type_check(%AST.Match{} = match, env, opts) do
    {value, value_t, value_bindings} =
      compute_and_erase_types(
        match.value,
        env,
        opts
      )

    {pattern, _, pattern_bindings} =
      PatternMatching.handle_pattern(
        match.pattern,
        value_t,
        env,
        opts
      )

    bindings = value_bindings ++ pattern_bindings

    {:=, match.meta, [pattern, value]}
    |> annotate_type(value_t)
    |> annotate_bindings(bindings)
  end
end
