defmodule Deft.Rules.Builtins do
  @moduledoc """
  Built-in typing rules implemented using the declarative DSL.

  These rules handle:
  - Local function calls (guards, operators)
  - Remote calls (Module.function(args))
  - Function captures (&function/arity)
  - Type constructor calls (ADT constructors)

  ## Special Cases

  Most built-in functions use signatures from `Deft.Signatures`. However,
  some operations need special handling to preserve type precision:

  - **Binary arithmetic** (`+`, `-`, `*`): Returns the most specific type
    (e.g., `integer + integer = integer`, not just `number`)
  - **Unary arithmetic** (`-`, `abs`): Preserves the input type
  - **`elem`**: Extracts union of tuple element types when available
  """

  use Deft.Rules.DSL

  alias Deft.AST
  alias Deft.AST.Erased
  alias Deft.Error
  alias Deft.Signatures
  alias Deft.Substitution
  alias Deft.Subtyping
  alias Deft.Type
  alias Deft.TypeChecker
  alias Deft.Unification

  # ============================================================================
  # Local Call Rule (Guards and Built-in Functions)
  # ============================================================================

  defrule :local_call, %AST.LocalCall{name: name, args: args, meta: meta} do
    compute {args_e, result_t, bs} do
      arity = length(args)
      binary_math = [:+, :-, :*]
      unary_math = [:-, :abs]

      cond do
        # Special case: binary arithmetic with type preservation
        name in binary_math and arity == 2 ->
          [fst_ast, snd_ast] = args
          {:ok, fst, fst_t, fst_bindings, _} = TypeChecker.check(fst_ast, var!(ctx, nil))
          {:ok, snd, snd_t, snd_bindings, _} = TypeChecker.check(snd_ast, var!(ctx, nil))

          unless Subtyping.subtype_of?(Type.number(), fst_t) do
            Error.raise!(
              Error.type_mismatch(
                expected: Type.number(),
                actual: fst_t,
                location: Error.extract_location(fst_ast),
                expression: fst_ast,
                notes: ["Left operand of `#{name}` must be a `number`"]
              ),
              var!(ctx, nil)
            )
          end

          unless Subtyping.subtype_of?(Type.number(), snd_t) do
            Error.raise!(
              Error.type_mismatch(
                expected: Type.number(),
                actual: snd_t,
                location: Error.extract_location(snd_ast),
                expression: snd_ast,
                notes: ["Right operand of `#{name}` must be a `number`"]
              ),
              var!(ctx, nil)
            )
          end

          # Compute result type: most specific common type
          result_t =
            cond do
              Subtyping.subtype_of?(fst_t, snd_t) -> fst_t
              Subtyping.subtype_of?(snd_t, fst_t) -> snd_t
              true -> Type.number()
            end

          {[fst, snd], result_t, fst_bindings ++ snd_bindings}

        # Special case: unary arithmetic with type preservation
        name in unary_math and arity == 1 ->
          [term_ast] = args
          {:ok, term, term_t, bindings, _} = TypeChecker.check(term_ast, var!(ctx, nil))

          unless Subtyping.subtype_of?(Type.number(), term_t) do
            Error.raise!(
              Error.type_mismatch(
                expected: Type.number(),
                actual: term_t,
                location: Error.extract_location(term_ast),
                expression: term_ast,
                notes: ["Operator `#{name}` requires a `number` operand"]
              ),
              var!(ctx, nil)
            )
          end

          {[term], term_t, bindings}

        # Special case: elem extracts tuple element types
        name == :elem and arity == 2 ->
          [tuple_ast, index_ast] = args
          {:ok, tuple, tuple_t, tuple_bindings, _} = TypeChecker.check(tuple_ast, var!(ctx, nil))
          {:ok, index, index_t, index_bindings, _} = TypeChecker.check(index_ast, var!(ctx, nil))

          unless Subtyping.subtype_of?(Type.tuple(), tuple_t) do
            Error.raise!(
              Error.type_mismatch(
                expected: Type.tuple(),
                actual: tuple_t,
                location: Error.extract_location(tuple_ast),
                expression: tuple_ast,
                notes: ["First argument to `elem` must be a `tuple`"]
              ),
              var!(ctx, nil)
            )
          end

          unless Subtyping.subtype_of?(Type.integer(), index_t) do
            Error.raise!(
              Error.type_mismatch(
                expected: Type.integer(),
                actual: index_t,
                location: Error.extract_location(index_ast),
                expression: index_ast,
                notes: ["Second argument to `elem` must be an `integer` index"]
              ),
              var!(ctx, nil)
            )
          end

          # Extract element types if available, otherwise fall back to top
          result_t =
            case tuple_t do
              %Type.FixedTuple{} = fixed ->
                fixed
                |> Type.FixedTuple.unique_types()
                |> Enum.reduce(Type.bottom(), &Type.union/2)

              _ ->
                Type.top()
            end

          {[tuple, index], result_t, tuple_bindings ++ index_bindings}

        # Check Kernel signatures (polymorphic and monomorphic)
        Signatures.registered?({Kernel, name, arity}) ->
          case Signatures.lookup({Kernel, name, arity}) do
            {:ok, %Type.Fn{inputs: input_ts, output: output_t}} ->
              {args_e, bs} = check_all!(args, input_ts, [], var!(ctx, nil))
              {args_e, output_t, bs}

            {:ok, %Type.Forall{vars: vars, body: %Type.Fn{inputs: inputs, output: output}}} ->
              # Synthesize all arguments to get their types
              {erased_args, arg_types, bindings} =
                args
                |> Enum.with_index(1)
                |> Enum.reduce({[], [], []}, fn {arg_ast, idx}, {erased_acc, types_acc, bindings_acc} ->
                  {:ok, erased, actual_type, bindings, _} = TypeChecker.check(arg_ast, var!(ctx, nil))

                  if idx > length(inputs) do
                    Error.raise!(
                      Error.type_mismatch(
                        expected: Type.fixed_tuple(inputs),
                        actual: Type.fixed_tuple(types_acc ++ [actual_type]),
                        notes: ["Expected #{length(inputs)} argument(s), got #{idx}"]
                      ),
                      var!(ctx, nil)
                    )
                  end

                  {erased_acc ++ [erased], types_acc ++ [actual_type], bindings_acc ++ bindings}
                end)

              # Infer type variable bindings via unification
              case Unification.infer(vars, inputs, arg_types) do
                {:ok, subst} ->
                  instantiated_inputs = Enum.map(inputs, &Substitution.substitute(&1, subst))
                  instantiated_output = Substitution.substitute(output, subst)

                  # Verify arguments are subtypes of instantiated input types
                  Enum.zip([args, arg_types, instantiated_inputs])
                  |> Enum.with_index(1)
                  |> Enum.each(fn {{arg_ast, arg_t, input_t}, idx} ->
                    unless Subtyping.subtype_of?(input_t, arg_t) do
                      Error.raise!(
                        Error.type_mismatch(
                          expected: input_t,
                          actual: arg_t,
                          location: Error.extract_location(arg_ast),
                          expression: arg_ast,
                          notes: ["Argument #{idx} to `#{name}` has wrong type"]
                        ),
                        var!(ctx, nil)
                      )
                    end
                  end)

                  {erased_args, instantiated_output, bindings}

                {:error, reason} ->
                  Error.raise!(
                    Error.type_mismatch(
                      expected: Type.forall(vars, Type.fun(inputs, output)),
                      actual: Type.fixed_tuple(arg_types),
                      notes: ["Type inference failed: #{inspect(reason)}"]
                    ),
                    var!(ctx, nil)
                  )
              end

            :error ->
              # Should not happen since we checked registered? first
              Error.raise!(Error.unsupported_call(name: name, arity: arity), var!(ctx, nil))
          end

        # Check current module's signatures (for deft-defined functions)
        var!(ctx, nil).env && Signatures.registered?({var!(ctx, nil).env.module, name, arity}) ->
          {:ok, %Type.Fn{inputs: input_ts, output: output_t}} =
            Signatures.lookup({var!(ctx, nil).env.module, name, arity})

          {args_e, bs} = check_all!(args, input_ts, [], var!(ctx, nil))
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
            var!(ctx, nil)
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
          {args_e, bs} = check_all!(args, input_ts, [], var!(ctx, nil))
          {args_e, output_t, bs}

        {:ok, %Type.Forall{vars: vars, body: %Type.Fn{inputs: inputs, output: output}}} ->
          # Synthesize all arguments to get their types
          {erased_args, arg_types, bindings} =
            args
            |> Enum.with_index(1)
            |> Enum.reduce({[], [], []}, fn {arg_ast, idx}, {erased_acc, types_acc, bindings_acc} ->
              {:ok, erased, actual_type, bindings, _} = TypeChecker.check(arg_ast, var!(ctx, nil))

              if idx > length(inputs) do
                Error.raise!(
                  Error.type_mismatch(
                    expected: Type.fixed_tuple(inputs),
                    actual: Type.fixed_tuple(types_acc ++ [actual_type]),
                    notes: ["Expected #{length(inputs)} argument(s), got #{idx}"]
                  ),
                  var!(ctx, nil)
                )
              end

              {erased_acc ++ [erased], types_acc ++ [actual_type], bindings_acc ++ bindings}
            end)

          # Infer type variable bindings via unification
          case Unification.infer(vars, inputs, arg_types) do
            {:ok, subst} ->
              instantiated_inputs = Enum.map(inputs, &Substitution.substitute(&1, subst))
              instantiated_output = Substitution.substitute(output, subst)

              # Verify arguments are subtypes of instantiated input types
              Enum.zip([args, arg_types, instantiated_inputs])
              |> Enum.with_index(1)
              |> Enum.each(fn {{arg_ast, arg_t, input_t}, idx} ->
                unless Subtyping.subtype_of?(input_t, arg_t) do
                  Error.raise!(
                    Error.type_mismatch(
                      expected: input_t,
                      actual: arg_t,
                      location: Error.extract_location(arg_ast),
                      expression: arg_ast,
                      notes: ["Argument #{idx} to `#{mod}.#{func}` has wrong type"]
                    ),
                    var!(ctx, nil)
                  )
                end
              end)

              {erased_args, instantiated_output, bindings}

            {:error, reason} ->
              Error.raise!(
                Error.type_mismatch(
                  expected: Type.forall(vars, Type.fun(inputs, output)),
                  actual: Type.fixed_tuple(arg_types),
                  notes: ["Type inference failed: #{inspect(reason)}"]
                ),
                var!(ctx, nil)
              )
          end

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
            var!(ctx, nil)
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
      lookup_mod = mod || (var!(ctx, nil).env && var!(ctx, nil).env.module)

      case lookup_mod && Signatures.lookup({lookup_mod, func, arity}) do
        {:ok, %Type.Fn{} = fn_t} ->
          fn_t

        {:ok, %Type.Forall{body: %Type.Fn{} = fn_t}} ->
          # For polymorphic functions, return the underlying function type
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
            var!(ctx, nil)
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
