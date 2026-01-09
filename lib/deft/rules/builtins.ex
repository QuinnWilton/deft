defmodule Deft.Rules.Builtins do
  @moduledoc """
  Built-in typing rules implemented using the declarative DSL.

  These rules handle:
  - Local function calls (guards, operators)
  - Type constructor calls (ADT constructors)
  """

  use Deft.Rule.DSL

  alias Deft.AST
  alias Deft.Guards
  alias Deft.Subtyping

  # ============================================================================
  # Local Call Rule (Guards and Built-in Functions)
  # ============================================================================

  defrule(:local_call,
    match: %AST.LocalCall{},
    judgment: :synth,
    do:
      (
        %AST.LocalCall{name: name, args: args, meta: meta} = ast

        if Guards.supported?(name, length(args)) do
          {erased_args, type, bindings} = Guards.handle_guard(name, args, ctx)
          erased = {name, meta, erased_args}
          emit(erased, type, bindings)
        else
          {:error, %Deft.UnsupportedLocalCall{name: name, arity: length(args)}}
        end
      )
  )

  # ============================================================================
  # Type Constructor Call Rule (ADT Constructors)
  # ============================================================================

  defrule(:type_constructor_call,
    match: %AST.TypeConstructorCall{},
    judgment: :synth,
    do:
      (
        %AST.TypeConstructorCall{
          name: name,
          args: args,
          type: adt_type,
          variant: variant,
          meta: meta
        } = ast

        # Type check arguments
        {erased_args, arg_types, bindings} = check_all!(args, ctx)

        # Validate arguments match variant columns
        unless length(variant.columns) == length(arg_types) and
                 Subtyping.subtypes_of?(variant.columns, arg_types) do
          raise Deft.TypecheckingError, expected: variant.columns, actual: arg_types
        end

        # Build erased tuple representation
        columns = [name | erased_args]
        erased = {:{}, meta, columns}

        emit(erased, adt_type, bindings)
      )
  )
end
