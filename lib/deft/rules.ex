defmodule Deft.Rules do
  @moduledoc """
  Index module for all typing rules.

  This module aggregates all rule modules and provides convenient
  access to the complete rule set.

  ## Rule Modules

  - `Deft.Rules.Core` - Literals, variables, annotations, tuples, lists
  - `Deft.Rules.Functions` - Anonymous functions and application
  - `Deft.Rules.ControlFlow` - If, cond, case, match expressions
  - `Deft.Rules.Builtins` - Guards and type constructors
  """

  alias Deft.Context
  alias Deft.Rule
  alias Deft.Rule.Registry

  @doc """
  Helper to create a successful rule result.
  """
  @spec ok(term(), Deft.Type.t(), [Rule.binding()], Context.t()) :: Rule.success()
  def ok(erased_ast, type, bindings, ctx) do
    {:ok, erased_ast, type, bindings, ctx}
  end

  @doc """
  Helper to create an error result.
  """
  @spec error(term()) :: Rule.error()
  def error(reason) do
    {:error, reason}
  end

  @doc """
  Returns all rule modules.
  """
  @spec rule_modules() :: [module()]
  def rule_modules do
    [
      Deft.Rules.Core,
      Deft.Rules.Functions,
      Deft.Rules.ControlFlow,
      Deft.Rules.Builtins
    ]
  end

  @doc """
  Returns all individual rules from all modules.
  """
  @spec all_rules() :: [module()]
  def all_rules do
    Enum.flat_map(rule_modules(), fn mod ->
      Code.ensure_loaded(mod)

      if function_exported?(mod, :rules, 0) do
        mod.rules()
      else
        [mod]
      end
    end)
  end

  @doc """
  Returns a rule registry containing all rules.
  """
  @spec registry() :: Registry.t()
  def registry do
    Registry.new(all_rules())
  end
end
