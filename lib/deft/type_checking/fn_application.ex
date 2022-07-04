defmodule Deft.TypeChecking.FnApplication do
  import Deft.Helpers

  alias Deft.AST
  alias Deft.Subtyping

  def type_check(%AST.FnApplication{} = fn_application, env) do
    {fun, fun_t, _} = compute_and_erase_types(fn_application.fun, env)
    {args, args_t, bindings} = compute_and_erase_types(fn_application.args, env)

    unless length(fun_t.inputs) == length(args_t) and
             Subtyping.subtypes_of?(fun_t.inputs, args_t) do
      raise Deft.TypecheckingError, expected: fun_t.inputs, actual: args_t
    end

    {{:., fn_application.fun_meta, [fun]}, fn_application.args_meta, args}
    |> annotate_type(fun_t.output)
    |> annotate_bindings(bindings)
  end
end
