defmodule Deft.TypeChecker do
  @moduledoc """
  Main entry point for the rule-based type checker.

  This module provides a unified interface for type checking using
  the rule registry system.

  ## Usage

      ctx = Deft.Context.new(__CALLER__)
      {:ok, erased, type, bindings, ctx} = Deft.TypeChecker.check(ast, ctx)

  ## Extensibility

  Custom rules can be added by creating modules that implement the
  `Deft.Rule` behaviour and registering them with the registry.
  """

  alias Deft.Context
  alias Deft.Helpers
  alias Deft.Rule.Registry
  alias Deft.Rules.Declarative

  # Default rule set
  @default_rules Declarative.Core.rules() ++
                   Declarative.Functions.rules() ++
                   Declarative.ControlFlow.rules() ++
                   Declarative.Builtins.rules()

  @doc """
  Returns the default rule registry.
  """
  @spec default_registry() :: Registry.t()
  def default_registry do
    Registry.new(@default_rules)
  end

  @doc """
  Type checks an AST node using the default rule set.

  Returns `{:ok, erased_ast, type, bindings, context}` on success,
  or `{:error, reason}` on failure.
  """
  @spec check(term(), Context.t()) :: Deft.Rule.result()
  def check(ast, %Context{} = ctx) do
    check(ast, ctx, default_registry())
  end

  @doc """
  Type checks an AST node using a custom rule registry.
  """
  @spec check(term(), Context.t(), Registry.t()) :: Deft.Rule.result()
  def check(ast, %Context{} = ctx, %Registry{} = registry) do
    result = Registry.apply_rule(registry, ast, nil, ctx)

    # Notify on_compute callback if present
    case result do
      {:ok, _erased, type, _bindings, ctx} ->
        Context.notify_compute(ctx, ast, type)
        result

      _ ->
        result
    end
  end

  @doc """
  Type checks an AST node against an expected type.

  This is the "checking" direction of bidirectional type checking.
  """
  @spec check_against(term(), Deft.Type.t(), Context.t()) :: Deft.Rule.result()
  def check_against(ast, expected_type, %Context{} = ctx) do
    check_against(ast, expected_type, ctx, default_registry())
  end

  @doc """
  Type checks against an expected type using a custom registry.
  """
  @spec check_against(term(), Deft.Type.t(), Context.t(), Registry.t()) :: Deft.Rule.result()
  def check_against(ast, expected_type, %Context{} = ctx, %Registry{} = registry) do
    case Registry.find_rule(registry, ast) do
      {:ok, rule} ->
        rule.apply(ast, expected_type, ctx)

      :error ->
        {:error, {:no_matching_rule, ast}}
    end
  end

  @doc """
  Type checks a list of AST nodes, returning erased nodes and their types.
  """
  @spec check_all(list(), Context.t()) ::
          {:ok, list(), list(), list(), Context.t()} | {:error, term()}
  def check_all(nodes, %Context{} = ctx) when is_list(nodes) do
    check_all(nodes, ctx, default_registry())
  end

  @spec check_all(list(), Context.t(), Registry.t()) ::
          {:ok, list(), list(), list(), Context.t()} | {:error, term()}
  def check_all(nodes, %Context{} = ctx, %Registry{} = registry) when is_list(nodes) do
    result =
      Enum.reduce_while(nodes, {:ok, [], [], [], ctx}, fn node,
                                                          {:ok, erased_acc, types_acc,
                                                           bindings_acc, ctx} ->
        case check(node, ctx, registry) do
          {:ok, erased, type, bindings, ctx} ->
            {:cont,
             {:ok, erased_acc ++ [erased], types_acc ++ [type], bindings_acc ++ bindings, ctx}}

          {:error, _} = error ->
            {:halt, error}
        end
      end)

    result
  end

  @doc """
  Type checks with context bindings pre-applied.

  This is useful when checking expressions in a scope where
  variables have already been bound (e.g., function bodies).
  """
  @spec check_in_context(term(), [Context.binding()], Context.t()) :: Deft.Rule.result()
  def check_in_context(ast, bindings, %Context{} = ctx) do
    # First inject bindings into the AST
    ast = Helpers.inject_bindings(ast, bindings)
    # Then check the modified AST
    check(ast, ctx)
  end
end
