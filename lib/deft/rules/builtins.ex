defmodule Deft.Rules.Builtins do
  @moduledoc """
  Typing rules for built-in operations (guards) and type constructors.
  """

  alias Deft.AST
  alias Deft.Guards
  alias Deft.Subtyping
  alias Deft.TypeChecker

  # Local call rule - handles built-in functions/guards
  defmodule LocalCall do
    @behaviour Deft.Rule

    @impl true
    def name, do: :local_call

    @impl true
    def judgment, do: :synth

    @impl true
    def matches?(%AST.LocalCall{}), do: true
    def matches?(_), do: false

    @impl true
    def apply(%AST.LocalCall{name: name, args: args, meta: meta}, _expected, ctx) do
      if Guards.supported?(name, length(args)) do
        {erased_args, type, bindings} = Guards.handle_guard(name, args, ctx)
        erased = {name, meta, erased_args}
        {:ok, erased, type, bindings, ctx}
      else
        {:error, %Deft.UnsupportedLocalCall{name: name, arity: length(args)}}
      end
    end
  end

  # Type constructor call rule - handles ADT constructors
  defmodule TypeConstructorCall do
    @behaviour Deft.Rule

    @impl true
    def name, do: :type_constructor_call

    @impl true
    def judgment, do: :synth

    @impl true
    def matches?(%AST.TypeConstructorCall{}), do: true
    def matches?(_), do: false

    @impl true
    def apply(
          %AST.TypeConstructorCall{
            name: name,
            args: args,
            type: adt_type,
            variant: variant,
            meta: meta
          },
          _expected,
          ctx
        ) do
      # Type check arguments
      {erased_args, arg_types, bindings} = check_args(args, ctx)

      # Validate arguments match variant columns
      unless length(variant.columns) == length(arg_types) and
               Subtyping.subtypes_of?(variant.columns, arg_types) do
        raise Deft.TypecheckingError, expected: variant.columns, actual: arg_types
      end

      # Build erased tuple representation
      columns = [name | erased_args]
      erased = {:{}, meta, columns}

      {:ok, erased, adt_type, bindings, ctx}
    end

    defp check_args(args, ctx) do
      {erased, types, bindings} =
        Enum.reduce(args, {[], [], []}, fn arg, {acc_erased, acc_types, acc_bindings} ->
          {:ok, erased, type, arg_bindings, _ctx} = TypeChecker.check(arg, ctx)
          {acc_erased ++ [erased], acc_types ++ [type], acc_bindings ++ arg_bindings}
        end)

      {erased, types, bindings}
    end
  end

  @doc """
  Returns all builtin rules.
  """
  def rules do
    [LocalCall, TypeConstructorCall]
  end
end
