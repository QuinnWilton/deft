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
  alias Deft.Subtyping
  alias Deft.Type
  alias Deft.TypeChecker

  # ============================================================================
  # Local Call Rule (Guards and Built-in Functions)
  # ============================================================================

  defrule :local_call, %AST.LocalCall{name: name, args: args, meta: meta} do
    compute {erased_args, type, call_bindings} do
      arity = length(args)

      cond do
        # First, check if it's a built-in guard/operator
        Guards.supported?(name, arity) ->
          Guards.handle_guard(name, args, ctx)

        # Check current module's signatures (for deft-defined functions)
        ctx.env && Signatures.registered?({ctx.env.module, name, arity}) ->
          {:ok, %Type.Fn{inputs: input_types, output: output_type}} =
            Signatures.lookup({ctx.env.module, name, arity})

          {erased_args, bindings} =
            args
            |> Enum.zip(input_types)
            |> Enum.reduce({[], []}, fn {arg, expected_type}, {erased_acc, bindings_acc} ->
              {:ok, erased, actual_type, bindings, _} = TypeChecker.check(arg, ctx)

              unless Subtyping.subtype_of?(expected_type, actual_type) do
                Error.raise!(
                  Error.type_mismatch(
                    expected: expected_type,
                    actual: actual_type,
                    location: Error.extract_location(arg),
                    expression: arg
                  ),
                  ctx
                )
              end

              {erased_acc ++ [erased], bindings_acc ++ bindings}
            end)

          {erased_args, output_type, bindings}

        true ->
          location = Error.extract_location(meta)

          Error.raise!(
            Error.unsupported_call(
              name: name,
              arity: arity,
              location: location
            ),
            ctx
          )
      end
    end

    conclude(Erased.local_call(meta, name, erased_args) ~> type, bind: call_bindings)
  end

  # ============================================================================
  # Remote Call Rule (Module.function(args))
  # ============================================================================

  defrule :remote_call, %AST.RemoteCall{
    module: module,
    function: function,
    args: args,
    meta: meta
  } do
    compute {erased_args, type, call_bindings} do
      arity = length(args)

      case Signatures.lookup({module, function, arity}) do
        {:ok, %Type.Fn{inputs: input_types, output: output_type}} ->
          {erased_args, bindings} =
            args
            |> Enum.zip(input_types)
            |> Enum.reduce({[], []}, fn {arg, expected_type}, {erased_acc, bindings_acc} ->
              {:ok, erased, actual_type, bindings, _} = TypeChecker.check(arg, ctx)

              unless Subtyping.subtype_of?(expected_type, actual_type) do
                Error.raise!(
                  Error.type_mismatch(
                    expected: expected_type,
                    actual: actual_type,
                    location: Error.extract_location(arg),
                    expression: arg
                  ),
                  ctx
                )
              end

              {erased_acc ++ [erased], bindings_acc ++ bindings}
            end)

          {erased_args, output_type, bindings}

        :error ->
          location = Error.extract_location(meta)

          Error.raise!(
            Error.unsupported_call(
              name: {module, function},
              arity: arity,
              location: location
            ),
            ctx
          )
      end
    end

    conclude(Erased.remote_call(meta, module, function, erased_args) ~> type, bind: call_bindings)
  end

  # ============================================================================
  # Function Capture Rule (&function/arity or &Module.function/arity)
  # ============================================================================

  defrule :capture, %AST.Capture{module: module, function: function, arity: arity, meta: meta} do
    compute fn_type do
      # Determine the module to look up: explicit module for remote, ctx.env.module for local
      lookup_module = module || (ctx.env && ctx.env.module)

      case lookup_module && Signatures.lookup({lookup_module, function, arity}) do
        {:ok, %Type.Fn{} = fn_type} ->
          fn_type

        _ ->
          # Not found in signatures - error
          name = if module, do: {module, function}, else: function
          location = Error.extract_location(meta)

          Error.raise!(
            Error.unsupported_call(
              name: name,
              arity: arity,
              location: location
            ),
            ctx
          )
      end
    end

    conclude(Erased.capture(meta, module, function, arity) ~> fn_type)
  end

  # ============================================================================
  # Type Constructor Call Rule (ADT Constructors)
  # ============================================================================

  defrule :type_constructor_call, %AST.TypeConstructorCall{
    name: name,
    args: args,
    type: adt_type,
    variant: variant,
    meta: meta
  } do
    # Synthesize arguments
    args ~>> {erased_args, arg_types}

    # Validate arguments match variant columns using detailed checking.
    compute :ok do
      expected = Deft.Type.fixed_tuple(variant.columns)
      actual = Deft.Type.fixed_tuple(arg_types)
      ctx = var!(ctx, nil)
      # Pass the original args for element-level span tracking.
      require_subtype!(actual, expected, %AST.TypeConstructorCall{
        name: name, args: args, type: adt_type, variant: variant, meta: meta
      }, ctx)
      :ok
    end

    # Build erased tuple representation
    columns = [name | erased_args]

    conclude(Erased.tuple(meta, columns) ~> adt_type)
  end
end
