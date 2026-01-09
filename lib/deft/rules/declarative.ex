defmodule Deft.Rules.Declarative do
  @moduledoc """
  Index module for declarative typing rules.

  This module provides access to all declarative rule implementations
  as an alternative to the imperative rules in `Deft.Rules.*`.

  The declarative rules use the `Deft.Rule.DSL` macro system for a more
  concise and Turnstile-like rule definition style.

  ## Available Rule Modules

  - `Deft.Rules.Declarative.Core` - Literals, locals, annotations, blocks, collections
  - `Deft.Rules.Declarative.Functions` - Anonymous functions and applications
  - `Deft.Rules.Declarative.ControlFlow` - If, cond, case, match
  - `Deft.Rules.Declarative.Builtins` - Guards and type constructors

  ## Usage

  You can use these rules directly or include them in a custom type system:

      defmodule MyTypeSystem do
        use Deft.TypeSystem

        include Deft.Rules.Declarative.Core
        include Deft.Rules.Declarative.Functions
        include Deft.Rules.Declarative.ControlFlow
        include Deft.Rules.Declarative.Builtins

        features [:exhaustiveness_checking]
      end

  Or use the convenience function to get all declarative rules:

      rules = Deft.Rules.Declarative.all_rules()
  """

  alias Deft.Rules.Declarative

  @rule_modules [
    Declarative.Core,
    Declarative.Functions,
    Declarative.ControlFlow,
    Declarative.Builtins
  ]

  @doc """
  Returns all rule modules.
  """
  @spec rule_modules() :: [module()]
  def rule_modules, do: @rule_modules

  @doc """
  Returns all individual rules from all declarative modules.
  """
  @spec all_rules() :: [module()]
  def all_rules do
    Enum.flat_map(@rule_modules, & &1.rules())
  end

  @doc """
  Returns a rule registry containing all declarative rules.
  """
  @spec registry() :: Deft.Rule.Registry.t()
  def registry do
    Deft.Rule.Registry.new(all_rules())
  end
end
