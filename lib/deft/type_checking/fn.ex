defmodule Deft.TypeChecking.Fn do
  import Deft.Helpers

  alias Deft.AST
  alias Deft.Type

  def type_check(%AST.Fn{} = fun, env, opts) do
    {args, input_types, bindings} = compute_and_erase_types(fun.args, env, opts)

    {body, output_type, _bindings} =
      compute_and_erase_type_in_context(
        fun.body,
        bindings,
        env,
        opts
      )

    type = Type.fun(input_types, output_type)

    {:fn, fun.fn_meta, [{:->, fun.arrow_meta, [args, body]}]}
    |> annotate_type(type)
  end
end
