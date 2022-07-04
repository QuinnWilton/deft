defmodule Deft.TypeChecking.Match do
  import Deft.Helpers

  alias Deft.AST

  def type_check(%AST.Match{} = match, env) do
    # TODO: Only simple assignment so far, no pattern matching
    pattern = erase_types(match.pattern, env)

    {value, value_t} =
      compute_and_erase_types(
        match.value,
        env
      )

    annotate({:=, match.meta, [pattern, value]}, value_t)
  end
end
