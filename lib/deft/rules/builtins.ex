defmodule Deft.Rules.Builtins do
  @moduledoc """
  Built-in typing rules implemented using the declarative DSL.

  These rules handle:
  - Local function calls (guards, operators)
  - Type constructor calls (ADT constructors)
  """

  use Deft.Rules.DSL

  alias Deft.AST
  alias Deft.Guards
  alias Deft.Subtyping

  # ============================================================================
  # Local Call Rule (Guards and Built-in Functions)
  # ============================================================================

  defrule :local_call, %AST.LocalCall{name: name, args: args, meta: meta} do
    compute {erased_args, type, call_bindings} do
      if Guards.supported?(name, length(args)) do
        Guards.handle_guard(name, args, ctx)
      else
        raise Deft.UnsupportedLocalCall, name: name, arity: length(args)
      end
    end

    conclude({name, meta, erased_args} ~> type, bind: call_bindings)
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

    # Validate arguments match variant columns
    compute :ok do
      unless length(variant.columns) == length(arg_types) and
               Subtyping.subtypes_of?(variant.columns, arg_types) do
        raise Deft.TypecheckingError, expected: variant.columns, actual: arg_types
      end

      :ok
    end

    # Build erased tuple representation
    columns = [name | erased_args]

    conclude({:{}, meta, columns} ~> adt_type)
  end
end
