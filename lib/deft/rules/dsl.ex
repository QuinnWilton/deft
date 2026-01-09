defmodule Deft.Rules.DSL do
  @moduledoc """
  Declarative DSL for defining typing rules in the style of Turnstile.

  This module provides macros for defining typing rules declaratively,
  with premises, conclusions, and elaborations specified separately.

  ## Usage

      defmodule MyRules do
        use Deft.Rules.DSL

        defrule :literal,
          match: %AST.Literal{value: value},
          do: (
            type = type_of(value)
            emit(value, type)
          )

        defrule :fn,
          match: %AST.Fn{args: args, body: body},
          do: (
            {erased_args, input_types, bindings} = check_args(args)
            {erased_body, output_type} = synth_with_bindings(body, bindings)
            type = Type.fun(input_types, output_type)
            erased = build_fn(erased_args, erased_body)
            emit(erased, type)
          )
      end

  ## DSL Primitives

  Within a `defrule` block, you have access to:

  - `ctx` - The typing context
  - `expected` - The expected type (nil for synthesis)
  - `synth(expr)` - Synthesize a type for an expression
  - `check(expr, type)` - Check an expression against a type
  - `synth_with_bindings(expr, bindings)` - Synthesize with extra bindings
  - `emit(erased, type)` - Return the rule result
  - `emit(erased, type, bindings)` - Return with new bindings
  - `bind(var, type)` - Create a binding
  - `require_subtype(actual, expected)` - Assert subtyping relationship

  ## Features

  Rules can specify features they require:

      defrule :polymorphic_fn,
        match: %AST.Fn{...},
        requires: [:polymorphism],
        do: ...

  The rule will only be applied if the feature is enabled in the context.
  """

  @doc """
  Enables the Rule DSL in a module.

  This imports the necessary macros and sets up module attributes
  for collecting rule definitions.
  """
  defmacro __using__(_opts) do
    quote do
      import Deft.Rules.DSL, only: [defrule: 2, defrule: 3]
      import Deft.Rules, only: [ok: 4, error: 1]
      Module.register_attribute(__MODULE__, :deft_rules, accumulate: true)

      @before_compile Deft.Rules.DSL
    end
  end

  @doc false
  defmacro __before_compile__(env) do
    rules = Module.get_attribute(env.module, :deft_rules) || []

    quote do
      @doc """
      Returns all rules defined in this module.
      """
      def rules do
        unquote(Enum.reverse(rules))
      end
    end
  end

  @doc """
  Defines a typing rule with the given name and options.

  ## Options

  - `:match` - (required) The AST pattern to match
  - `:judgment` - `:synth`, `:check`, or `:both` (default: `:synth`)
  - `:requires` - List of features required for this rule
  - `:do` - The rule body

  ## Example

      defrule :literal,
        match: %AST.Literal{value: value},
        judgment: :synth,
        do: (
          type = type_of_literal(value)
          emit(value, type)
        )
  """
  defmacro defrule(name, opts) do
    define_rule(name, opts, __CALLER__)
  end

  @doc """
  Defines a typing rule with a do block.

  This is syntactic sugar for `defrule name, opts ++ [do: block]`.
  """
  defmacro defrule(name, opts, do: body) do
    define_rule(name, Keyword.put(opts, :do, body), __CALLER__)
  end

  # Generate the expected variable pattern based on judgment type.
  # For :synth rules, we use _expected to suppress unused warnings.
  # For :check rules, we expose expected so it can be used.
  defp expected_var(:synth), do: quote(do: _expected)
  defp expected_var(:check), do: quote(do: var!(expected))
  defp expected_var(:both), do: quote(do: var!(expected))

  defp define_rule(name, opts, _caller) do
    match_pattern = Keyword.fetch!(opts, :match)
    judgment = Keyword.get(opts, :judgment, :synth)
    requires = Keyword.get(opts, :requires, [])
    body = Keyword.fetch!(opts, :do)

    # Generate a unique module name suffix for this rule
    rule_module_suffix = :"Rule_#{name}"

    quote do
      # Compute full module name at compile time
      @__rule_module_name__ Module.concat(__MODULE__, unquote(rule_module_suffix))

      defmodule @__rule_module_name__ do
        @behaviour Deft.Rules

        alias Deft.Context
        alias Deft.Subtyping
        alias Deft.Type
        alias Deft.TypeChecker
        alias Deft.AST

        @impl true
        def name, do: unquote(name)

        @impl true
        def judgment, do: unquote(judgment)

        @required_features unquote(requires)

        @impl true
        def matches?(unquote(match_pattern)), do: true
        def matches?(_), do: false

        @impl true
        def apply(unquote(match_pattern) = var!(ast), unquote(expected_var(judgment)), var!(ctx)) do
          # Suppress unused variable warning for ast
          _ = var!(ast)

          # Check required features
          if features_enabled?(var!(ctx), @required_features) do
            # Make DSL helpers available
            import Deft.Rules.DSL.Helpers

            unquote(body)
          else
            {:error, {:missing_features, @required_features}}
          end
        end

        defp features_enabled?(_ctx, []), do: true

        defp features_enabled?(ctx, features) do
          Enum.all?(features, &Context.feature_enabled?(ctx, &1))
        end
      end

      # Store full module name for the rule
      @deft_rules @__rule_module_name__
    end
  end
end

defmodule Deft.Rules.DSL.Helpers do
  @moduledoc """
  Helper functions available within defrule blocks.

  These functions provide a clean interface for common type-checking
  operations within rule implementations.
  """

  alias Deft.Context
  alias Deft.Subtyping
  alias Deft.Type
  alias Deft.TypeChecker

  @doc """
  Emits a successful rule result with no new bindings.
  """
  defmacro emit(erased, type) do
    quote do
      {:ok, unquote(erased), unquote(type), [], var!(ctx)}
    end
  end

  @doc """
  Emits a successful rule result with bindings.
  """
  defmacro emit(erased, type, bindings) do
    quote do
      {:ok, unquote(erased), unquote(type), unquote(bindings), var!(ctx)}
    end
  end

  @doc """
  Emits a successful result with updated context.
  """
  defmacro emit_with_ctx(erased, type, bindings, new_ctx) do
    quote do
      {:ok, unquote(erased), unquote(type), unquote(bindings), unquote(new_ctx)}
    end
  end

  @doc """
  Synthesizes a type for the given expression.

  Returns `{erased, type, bindings}` on success.
  """
  def synth(expr, ctx) do
    case TypeChecker.check(expr, ctx) do
      {:ok, erased, type, bindings, _ctx} -> {:ok, erased, type, bindings}
      {:error, _} = err -> err
    end
  end

  @doc """
  Synthesizes a type, raising on error.

  Returns `{erased, type, bindings}`.
  """
  def synth!(expr, ctx) do
    {:ok, erased, type, bindings, _ctx} = TypeChecker.check(expr, ctx)
    {erased, type, bindings}
  end

  @doc """
  Synthesizes with additional bindings injected into the expression.
  """
  def synth_with_bindings(expr, bindings, ctx) do
    case TypeChecker.check_in_context(expr, bindings, ctx) do
      {:ok, erased, type, new_bindings, _ctx} -> {:ok, erased, type, new_bindings}
      {:error, _} = err -> err
    end
  end

  @doc """
  Synthesizes with bindings, raising on error.
  """
  def synth_with_bindings!(expr, bindings, ctx) do
    {:ok, erased, type, new_bindings, _ctx} = TypeChecker.check_in_context(expr, bindings, ctx)
    {erased, type, new_bindings}
  end

  @doc """
  Checks an expression against an expected type.
  """
  def check(expr, expected_type, ctx) do
    case TypeChecker.check_against(expr, expected_type, ctx) do
      {:ok, erased, type, bindings, _ctx} -> {:ok, erased, type, bindings}
      {:error, _} = err -> err
    end
  end

  @doc """
  Checks an expression against a type, raising on error.
  """
  def check!(expr, expected_type, ctx) do
    {:ok, erased, type, bindings, _ctx} = TypeChecker.check_against(expr, expected_type, ctx)
    {erased, type, bindings}
  end

  @doc """
  Checks a list of expressions, returning erased ASTs, types, and accumulated bindings.
  """
  def check_all(exprs, ctx) do
    Enum.reduce_while(exprs, {:ok, [], [], []}, fn expr,
                                                   {:ok, acc_erased, acc_types, acc_bindings} ->
      case synth(expr, ctx) do
        {:ok, erased, type, bindings} ->
          {:cont, {:ok, acc_erased ++ [erased], acc_types ++ [type], acc_bindings ++ bindings}}

        {:error, _} = err ->
          {:halt, err}
      end
    end)
  end

  @doc """
  Checks all expressions, raising on error.
  """
  def check_all!(exprs, ctx) do
    {:ok, erased, types, bindings} = check_all(exprs, ctx)
    {erased, types, bindings}
  end

  @doc """
  Asserts that actual is a subtype of expected.
  """
  def require_subtype!(actual, expected) do
    unless Subtyping.subtype_of?(expected, actual) do
      raise Deft.TypecheckingError, expected: expected, actual: actual
    end
  end

  @doc """
  Creates a binding tuple.
  """
  def bind(var, type), do: {var, type}

  @doc """
  Computes the type of a literal value.
  """
  def type_of_literal(v) when is_boolean(v), do: Type.boolean()
  def type_of_literal(v) when is_atom(v), do: Type.atom()
  def type_of_literal(v) when is_integer(v), do: Type.integer()
  def type_of_literal(v) when is_float(v), do: Type.float()

  @doc """
  Unions a list of types together.
  """
  def union_types([]), do: Type.bottom()
  def union_types([t]), do: t
  def union_types([t | rest]), do: Type.union(t, union_types(rest))

  @doc """
  Checks if a feature is enabled in the context.
  """
  def feature_enabled?(ctx, feature), do: Context.feature_enabled?(ctx, feature)
end
