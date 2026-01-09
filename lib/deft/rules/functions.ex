defmodule Deft.Rules.Functions do
  @moduledoc """
  Function typing rules implemented using the declarative DSL.

  These rules handle:
  - Anonymous functions (fn)
  - Function application (f.(args))
  """

  use Deft.Rules.DSL

  alias Deft.AST
  alias Deft.Subtyping
  alias Deft.Type

  # ============================================================================
  # Anonymous Function Rule
  # ============================================================================

  defrule(:fn,
    match: %AST.Fn{},
    judgment: :synth,
    do:
      (
        %AST.Fn{args: args, body: body, fn_meta: fn_meta, arrow_meta: arrow_meta} = ast

        # Type check arguments (which should be annotations)
        {erased_args, input_types, arg_bindings} = check_all!(args, ctx)

        # Type check body with argument bindings injected
        {erased_body, output_type, _body_bindings} = synth_with_bindings!(body, arg_bindings, ctx)

        type = Type.fun(input_types, output_type)
        erased = {:fn, fn_meta, [{:->, arrow_meta, [erased_args, erased_body]}]}

        emit(erased, type)
      )
  )

  # ============================================================================
  # Function Application Rule
  # ============================================================================

  defrule(:fn_application,
    match: %AST.FnApplication{},
    judgment: :synth,
    do:
      (
        %AST.FnApplication{fun: fun, args: args, fun_meta: fun_meta, args_meta: args_meta} = ast

        # Type check the function
        {erased_fun, fun_type, fun_bindings} = synth!(fun, ctx)

        # Type check arguments
        {erased_args, arg_types, arg_bindings} = check_all!(args, ctx)

        # Validate function type and arguments
        case fun_type do
          %Type.Fn{inputs: inputs, output: output} ->
            if length(inputs) == length(arg_types) and Subtyping.subtypes_of?(inputs, arg_types) do
              erased = {{:., fun_meta, [erased_fun]}, args_meta, erased_args}
              bindings = fun_bindings ++ arg_bindings
              emit(erased, output, bindings)
            else
              {:error, %Deft.TypecheckingError{expected: inputs, actual: arg_types}}
            end

          _ ->
            {:error, {:not_a_function, fun_type}}
        end
      )
  )
end
