defmodule Deft.TypeSystem do
  @moduledoc """
  Module for composing custom type systems from rule sets.

  This module allows you to define custom type systems by combining
  rule modules and enabling specific features. Each type system can
  have different rules and capabilities.

  ## Usage

      defmodule MyApp.TypeSystem do
        use Deft.TypeSystem

        # Include base rule sets
        include Deft.Rules.Core
        include Deft.Rules.Functions
        include Deft.Rules.ControlFlow

        # Include builtin guards
        include Deft.Rules.Builtins

        # Enable specific features
        features [:exhaustiveness_checking, :strict_subtyping]
      end

  ## Using a Custom Type System

  Once defined, you can use your type system for type checking:

      ctx = Deft.Context.new(__ENV__, features: MyApp.TypeSystem.features())
      registry = MyApp.TypeSystem.registry()
      {:ok, erased, type, bindings, ctx} = Deft.TypeChecker.check(ast, ctx, registry)

  ## Built-in Features

  The following features can be enabled:

  - `:exhaustiveness_checking` - Verify pattern matches cover all cases
  - `:strict_subtyping` - Require explicit type conversions
  - `:polymorphism` - Enable parametric polymorphism (Phase 5)
  - `:effect_tracking` - Track side effects in types (Phase 5)

  ## Rule Priority

  Rules are applied in the order they are included. Earlier rules take
  priority over later ones. Use `include_first` to add rules at higher
  priority:

      include_first MyApp.CustomRules       # Applied before other rules
      include Deft.Rules.Core   # Applied after CustomRules
  """

  @doc """
  Enables the TypeSystem DSL in a module.
  """
  defmacro __using__(_opts) do
    quote do
      import Deft.TypeSystem, only: [include: 1, include_first: 1, features: 1]
      Module.register_attribute(__MODULE__, :deft_included_rules, accumulate: true)
      Module.register_attribute(__MODULE__, :deft_prepended_rules, accumulate: true)
      Module.register_attribute(__MODULE__, :deft_features, accumulate: false)
      Module.put_attribute(__MODULE__, :deft_features, [])

      @before_compile Deft.TypeSystem
    end
  end

  @doc false
  defmacro __before_compile__(env) do
    included = Module.get_attribute(env.module, :deft_included_rules) || []
    prepended = Module.get_attribute(env.module, :deft_prepended_rules) || []
    features = Module.get_attribute(env.module, :deft_features) || []

    # Prepended rules go first (in reverse order of declaration), then included rules
    all_rule_modules = Enum.reverse(prepended) ++ Enum.reverse(included)

    quote do
      @doc """
      Returns the list of features enabled in this type system.
      """
      @spec features() :: [atom()]
      def features, do: unquote(features)

      @doc """
      Returns all rule modules included in this type system.
      """
      @spec rule_modules() :: [module()]
      def rule_modules, do: unquote(all_rule_modules)

      @doc """
      Returns all individual rules from all included modules.
      """
      @spec all_rules() :: [module()]
      def all_rules do
        Enum.flat_map(rule_modules(), fn mod ->
          Code.ensure_loaded(mod)

          if function_exported?(mod, :rules, 0) do
            mod.rules()
          else
            # Module doesn't export rules/0, treat it as a single rule
            [mod]
          end
        end)
      end

      @doc """
      Returns a rule registry containing all rules.
      """
      @spec registry() :: Deft.Rules.Registry.t()
      def registry do
        Deft.Rules.Registry.new(all_rules())
      end

      @doc """
      Creates a typing context with this type system's features enabled.
      """
      @spec context(Macro.Env.t()) :: Deft.Context.t()
      def context(env) do
        Deft.Context.new(env, features: features())
      end

      @doc """
      Type checks an AST using this type system.
      """
      @spec check(term(), Macro.Env.t()) :: Deft.Rules.result()
      def check(ast, env) do
        ctx = context(env)
        Deft.TypeChecker.check(ast, ctx, registry())
      end
    end
  end

  @doc """
  Includes a rule module in the type system.

  The module should export a `rules/0` function that returns a list
  of rule modules, or be a single rule module itself.

  ## Example

      include Deft.Rules.Core
      include Deft.Rules.Functions
  """
  defmacro include(rule_module) do
    quote do
      @deft_included_rules unquote(rule_module)
    end
  end

  @doc """
  Includes a rule module at higher priority (before other rules).

  Use this to override default rules with custom implementations.

  ## Example

      include_first MyApp.CustomRules       # These take priority
      include Deft.Rules.Core   # Fallback rules
  """
  defmacro include_first(rule_module) do
    quote do
      @deft_prepended_rules unquote(rule_module)
    end
  end

  @doc """
  Sets the features enabled in this type system.

  ## Example

      features [:exhaustiveness_checking, :strict_subtyping]
  """
  defmacro features(feature_list) do
    quote do
      Module.put_attribute(__MODULE__, :deft_features, unquote(feature_list))
    end
  end
end

defmodule Deft.TypeSystem.Default do
  @moduledoc """
  The default type system including all standard rules.

  This is the type system used by `Deft.compile/1` when no custom
  type system is specified.
  """

  use Deft.TypeSystem

  include(Deft.Rules.Core)
  include(Deft.Rules.Functions)
  include(Deft.Rules.ControlFlow)
  include(Deft.Rules.Builtins)

  features([:exhaustiveness_checking])
end
