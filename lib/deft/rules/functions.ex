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

  defrule :fn, %AST.Fn{args: args, body: body, fn_meta: fn_meta, arrow_meta: arrow_meta} do
    # Synthesize arguments (which should be annotations, producing bindings)
    # These bindings are scoped to the function body, not returned to caller
    compute {erased_args, input_types, arg_bindings} do
      synth_all!(args, ctx)
    end

    # Synthesize body with argument bindings in scope
    (arg_bindings +++ body) ~> {erased_body, output_type}

    conclude(
      {:fn, fn_meta, [{:->, arrow_meta, [erased_args, erased_body]}]}
      ~> Type.fun(input_types, output_type)
    )
  end

  # ============================================================================
  # Function Application Rule
  # ============================================================================

  defrule :fn_application, %AST.FnApplication{fun: fun, args: args, fun_meta: fun_meta, args_meta: args_meta} do
    # Synthesize the function
    fun ~> {erased_fun, fun_type}

    # Synthesize arguments
    args ~>> {erased_args, arg_types}

    # Validate and extract output type
    compute output do
      case fun_type do
        %Type.Fn{inputs: inputs, output: output} ->
          if length(inputs) == length(arg_types) and Subtyping.subtypes_of?(inputs, arg_types) do
            output
          else
            raise Deft.TypecheckingError, expected: inputs, actual: arg_types
          end

        _ ->
          raise "Expected function type, got: #{inspect(fun_type)}"
      end
    end

    conclude({{:., fun_meta, [erased_fun]}, args_meta, erased_args} ~> output)
  end
end
