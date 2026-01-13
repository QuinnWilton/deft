defmodule Deft.Rules.Functions do
  @moduledoc """
  Function typing rules implemented using the declarative DSL.

  These rules handle:
  - Anonymous functions (fn)
  - Function application (f.(args))
  - Polymorphic function instantiation (Type.Forall)
  """

  use Deft.Rules.DSL

  alias Deft.AST
  alias Deft.AST.Erased
  alias Deft.Type
  alias Deft.Substitution
  alias Deft.Unification

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

    # Handle polymorphic vs monomorphic functions differently
    compute {args_e, output_t} do
      case fun_t do
        # Polymorphic function: synthesize args for type inference, then check
        %Type.Forall{vars: vars, body: %Type.Fn{inputs: inputs, output: output}} ->
          # Synthesize arguments to get their types for unification
          {args_erased, arg_ts, _bs} = synth_all!(args, var!(ctx, nil))

          # Verify arity
          if length(arg_ts) != length(inputs) do
            Deft.Error.raise!(
              Deft.Error.type_mismatch(
                expected: Type.fixed_tuple(inputs),
                actual: Type.fixed_tuple(arg_ts),
                notes: ["Expected #{length(inputs)} argument(s), got #{length(arg_ts)}"]
              )
            )
          end

          # Infer type variable bindings via unification
          case Unification.infer(vars, inputs, arg_ts) do
            {:ok, subst} ->
              instantiated_inputs = Enum.map(inputs, &Substitution.substitute(&1, subst))
              instantiated_output = Substitution.substitute(output, subst)

              # Verify arguments are subtypes of instantiated input types
              Enum.zip([args, arg_ts, instantiated_inputs])
              |> Enum.each(fn {arg_expr, arg_t, input_t} ->
                require_subtype!(arg_t, input_t, arg_expr, var!(ctx, nil))
              end)

              {args_erased, instantiated_output}

            {:error, reason} ->
              Deft.Error.raise!(
                Deft.Error.type_mismatch(
                  expected: fun_t,
                  actual: Type.fixed_tuple(arg_ts),
                  notes: ["Type inference failed: #{inspect(reason)}"]
                )
              )
          end

        # Monomorphic function: use checking mode for arguments (original behavior)
        %Type.Fn{inputs: input_ts, output: output_t} ->
          {args_erased, _bs} = check_all!(args, input_ts, [], var!(ctx, nil))
          {args_erased, output_t}

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

    conclude(Erased.fn_apply(fun_meta, args_meta, fun_e, args_e) ~> output_t)
  end
end
