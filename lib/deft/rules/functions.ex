defmodule Deft.Rules.Functions do
  @moduledoc """
  Function typing rules implemented using the declarative DSL.

  These rules handle:
  - Anonymous functions (fn)
  - Function application (f.(args))
  """

  use Deft.Rules.DSL

  alias Deft.AST
  alias Deft.AST.Erased
  alias Deft.Type

  # ============================================================================
  # Anonymous Function Rule
  # ============================================================================

  defrule :fn, %AST.Fn{args: args, body: body, fn_meta: fn_meta, arrow_meta: arrow_meta} do
    # Synthesize arguments (which should be annotations, producing bindings)
    # These bindings are scoped to the function body, not returned to caller
    args ~>> {args_e, input_ts, arg_bs}

    # Synthesize body with argument bindings in scope
    (arg_bs +++ body) ~> {body_e, output_t}

    conclude(
      Erased.fn_expr(fn_meta, arrow_meta, args_e, body_e)
      ~> Type.fun(input_ts, output_t)
    )
  end

  # ============================================================================
  # Function Application Rule
  # ============================================================================

  defrule :fn_application, %AST.FnApplication{
    fun: fun,
    args: args,
    fun_meta: fun_meta,
    args_meta: args_meta
  } do
    # Synthesize the function to get its type
    fun ~> {fun_e, fun_t}

    # Extract input/output types from function type
    compute {input_ts, output_t} do
      case fun_t do
        %Type.Fn{inputs: input_ts, output: output_t} ->
          {input_ts, output_t}

        _ ->
          Deft.Error.raise!(
            Deft.Error.type_mismatch(
              expected: Type.fun([], Type.top()),
              actual: fun_t,
              notes: ["Expected a function type but got: #{inspect(fun_t)}"]
            )
          )
      end
    end

    # Check arguments against expected input types (heterogeneous mode)
    args <<~ input_ts >>> args_e

    conclude(Erased.fn_apply(fun_meta, args_meta, fun_e, args_e) ~> output_t)
  end
end
