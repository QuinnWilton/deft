defmodule Deft.TypeChecking.Fn do
  import Deft.TypeChecking

  alias Deft.AST
  alias Deft.Type

  def type_check(%AST.Fn{} = fun, env) do
    {args, input_types} = compute_and_erase_types(fun.args, env)

    {body, output_type} =
      compute_and_erase_type_in_context(fun.body, Enum.zip(args, input_types), env)

    type = Type.fun(input_types, output_type)

    annotate({:fn, fun.fn_meta, [{:->, fun.arrow_meta, [args, body]}]}, type)
  end
end
