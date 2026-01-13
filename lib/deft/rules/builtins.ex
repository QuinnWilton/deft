defmodule Deft.Rules.Builtins do
  @moduledoc """
  Built-in typing rules implemented using the declarative DSL.

  These rules handle:
  - Local function calls (guards, operators)
  - Type constructor calls (ADT constructors)
  """

  use Deft.Rules.DSL

  alias Deft.AST
  alias Deft.AST.Erased
  alias Deft.Error
  alias Deft.Guards
  alias Deft.Signatures
  alias Deft.Type

  # ============================================================================
  # Local Call Rule (Guards and Built-in Functions)
  # ============================================================================

  defrule :local_call, %AST.LocalCall{name: name, args: args, meta: meta} do
    compute {args_e, result_t, bs} do
      arity = length(args)

      cond do
        # First, check if it's a built-in guard/operator
        Guards.supported?(name, arity) ->
          Guards.handle_guard(name, args, ctx)

        # Check current module's signatures (for deft-defined functions)
        ctx.env && Signatures.registered?({ctx.env.module, name, arity}) ->
          {:ok, %Type.Fn{inputs: input_ts, output: output_t}} =
            Signatures.lookup({ctx.env.module, name, arity})

          # Check arguments against input types (heterogeneous mode)
          {args_e, bs} = check_all!(args, input_ts, [], ctx)
          {args_e, output_t, bs}

        true ->
          location = Error.extract_location(meta)
          call_ast = %AST.LocalCall{name: name, args: args, meta: meta}

          Error.raise!(
            Error.unsupported_call(
              name: name,
              arity: arity,
              location: location,
              expression: call_ast
            ),
            ctx
          )
      end
    end

    conclude(Erased.local_call(meta, name, args_e) ~> result_t, bind: bs)
  end

  # ============================================================================
  # Remote Call Rule (Module.function(args))
  # ============================================================================

  defrule :remote_call, %AST.RemoteCall{
    module: mod,
    function: func,
    args: args,
    meta: meta
  } do
    compute {args_e, result_t, bs} do
      arity = length(args)

      case Signatures.lookup({mod, func, arity}) do
        {:ok, %Type.Fn{inputs: input_ts, output: output_t}} ->
          # Check arguments against input types (heterogeneous mode)
          {args_e, bs} = check_all!(args, input_ts, [], ctx)
          {args_e, output_t, bs}

        :error ->
          location = Error.extract_location(meta)
          call_ast = %AST.RemoteCall{module: mod, function: func, args: args, meta: meta}

          Error.raise!(
            Error.unsupported_call(
              name: {mod, func},
              arity: arity,
              location: location,
              expression: call_ast
            ),
            ctx
          )
      end
    end

    conclude(Erased.remote_call(meta, mod, func, args_e) ~> result_t, bind: bs)
  end

  # ============================================================================
  # Function Capture Rule (&function/arity or &Module.function/arity)
  # ============================================================================

  defrule :capture, %AST.Capture{module: mod, function: func, arity: arity, meta: meta} do
    compute fn_t do
      # Determine the module to look up: explicit module for remote, ctx.env.module for local
      lookup_mod = mod || (ctx.env && ctx.env.module)

      case lookup_mod && Signatures.lookup({lookup_mod, func, arity}) do
        {:ok, %Type.Fn{} = fn_t} ->
          fn_t

        _ ->
          # Not found in signatures - error
          name = if mod, do: {mod, func}, else: func
          location = Error.extract_location(meta)
          capture_ast = %AST.Capture{module: mod, function: func, arity: arity, meta: meta}

          Error.raise!(
            Error.unsupported_call(
              name: name,
              arity: arity,
              location: location,
              expression: capture_ast
            ),
            ctx
          )
      end
    end

    conclude(Erased.capture(meta, mod, func, arity) ~> fn_t)
  end

  # ============================================================================
  # Type Constructor Call Rule (ADT Constructors)
  # ============================================================================

  defrule :type_constructor_call, %AST.TypeConstructorCall{
    name: name,
    args: args,
    type: adt_t,
    variant: variant,
    meta: meta
  } do
    # Check arguments against variant column types (heterogeneous mode)
    args <<~ variant.columns >>> args_e

    # Build erased tuple representation
    cols_e = [name | args_e]

    conclude(Erased.tuple(meta, cols_e) ~> adt_t)
  end
end
