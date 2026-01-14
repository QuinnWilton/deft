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
        include_rule Deft.Rules.Core
        include_rule Deft.Rules.Functions
        include_rule Deft.Rules.ControlFlow

        # Include builtin guards
        include_rule Deft.Rules.Builtins

        # Include signatures
        include_signatures Deft.Signatures.Kernel

        # Include datatypes
        include_datatypes MyApp.Datatypes

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

  ## Rule Priority

  Rules are applied in the order they are included. Earlier rules take
  priority over later ones. Use `include_first` to add rules at higher
  priority:

      include_first MyApp.CustomRules       # Applied before other rules
      include_rule Deft.Rules.Core          # Applied after CustomRules
  """

  @doc """
  Enables the TypeSystem DSL in a module.
  """
  defmacro __using__(_opts) do
    quote do
      import Deft.TypeSystem,
        only: [
          include_rule: 1,
          include_first: 1,
          features: 1,
          include_signatures: 1,
          include_datatypes: 1,
          # Keep old names for backwards compatibility (deprecated)
          include: 1,
          signatures: 1
        ]

      Module.register_attribute(__MODULE__, :deft_included_rules, accumulate: true)
      Module.register_attribute(__MODULE__, :deft_prepended_rules, accumulate: true)
      Module.register_attribute(__MODULE__, :deft_signature_modules, accumulate: true)
      Module.register_attribute(__MODULE__, :deft_adt_modules, accumulate: true)
      Module.register_attribute(__MODULE__, :deft_features, accumulate: false)
      Module.put_attribute(__MODULE__, :deft_features, [])

      @before_compile Deft.TypeSystem
    end
  end

  @doc false
  defmacro __before_compile__(env) do
    included = Module.get_attribute(env.module, :deft_included_rules) || []
    prepended = Module.get_attribute(env.module, :deft_prepended_rules) || []
    signature_modules_with_loc = Module.get_attribute(env.module, :deft_signature_modules) || []
    adt_modules_with_loc = Module.get_attribute(env.module, :deft_adt_modules) || []
    features = Module.get_attribute(env.module, :deft_features) || []

    # Prepended rules go first (in reverse order of declaration), then included rules
    all_rule_modules = Enum.reverse(prepended) ++ Enum.reverse(included)

    # Signature and ADT modules in declaration order (with location info)
    all_signature_modules_data = Enum.reverse(signature_modules_with_loc)
    all_adt_modules_data = Enum.reverse(adt_modules_with_loc)

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
      Returns all signature modules included in this type system.
      """
      @spec signature_modules() :: [module()]
      def signature_modules do
        unquote(Macro.escape(all_signature_modules_data))
        |> Enum.map(fn {mod, _file, _line} -> mod end)
      end

      @doc """
      Returns all signatures from all included signature modules.

      Returns a map of `{module, function, arity} => type`.
      Raises a compile error if conflicting definitions are found.
      """
      @spec all_signatures() :: %{{module(), atom(), non_neg_integer()} => Deft.Type.t()}
      def all_signatures do
        Deft.TypeSystem.Conflict.merge_signatures(
          unquote(Macro.escape(all_signature_modules_data))
        )
      end

      @doc """
      Returns all ADT modules included in this type system.
      """
      @spec adt_modules() :: [module()]
      def adt_modules do
        unquote(Macro.escape(all_adt_modules_data))
        |> Enum.map(fn {mod, _file, _line} -> mod end)
      end

      @doc """
      Returns all datatypes from all included ADT modules.

      Returns a map of `name => Type.ADT`.
      Raises a compile error if conflicting definitions are found.
      """
      @spec all_datatypes() :: %{atom() => Deft.Type.t()}
      def all_datatypes do
        Deft.TypeSystem.Conflict.merge_datatypes(unquote(Macro.escape(all_adt_modules_data)))
      end

      @doc """
      Creates a typing context with this type system's features and signatures.
      """
      @spec context(Macro.Env.t()) :: Deft.Context.t()
      def context(env) do
        Deft.Context.new(env, features: features())
        |> Deft.Context.with_signatures(all_signatures())
        |> Deft.Context.with_adt_registry(all_datatypes())
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

      include_rule Deft.Rules.Core
      include_rule Deft.Rules.Functions
  """
  defmacro include_rule(rule_module) do
    quote do
      @deft_included_rules unquote(rule_module)
    end
  end

  @doc """
  Includes a rule module in the type system.

  Deprecated: Use `include_rule/1` instead.
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
      include_rule Deft.Rules.Core          # Fallback rules
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

  @doc """
  Includes a signature module in the type system.

  Signature modules define type signatures for external functions.
  These signatures are scoped to the type system and won't leak
  to other type systems.

  ## Example

      include_signatures Deft.Signatures.Kernel
      include_signatures Deft.Signatures.Enum
  """
  defmacro include_signatures(signature_module) do
    line = __CALLER__.line
    file = __CALLER__.file

    quote do
      @deft_signature_modules {unquote(signature_module), unquote(file), unquote(line)}
    end
  end

  @doc """
  Includes a signature module in the type system.

  Deprecated: Use `include_signatures/1` instead.
  """
  defmacro signatures(signature_module) do
    line = __CALLER__.line
    file = __CALLER__.file

    quote do
      @deft_signature_modules {unquote(signature_module), unquote(file), unquote(line)}
    end
  end

  @doc """
  Includes an ADT module in the type system.

  ADT modules define algebraic data types that can be used across modules.
  These datatypes are scoped to the type system and won't leak
  to other type systems.

  ## Example

      include_datatypes MyApp.Datatypes
      include_datatypes Deft.Datatypes.Core
  """
  defmacro include_datatypes(adt_module) do
    line = __CALLER__.line
    file = __CALLER__.file

    quote do
      @deft_adt_modules {unquote(adt_module), unquote(file), unquote(line)}
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

  include_rule Deft.Rules.Core
  include_rule Deft.Rules.Functions
  include_rule Deft.Rules.ControlFlow
  include_rule Deft.Rules.Builtins

  include_signatures Deft.Signatures.Kernel
  include_signatures Deft.Signatures.Enum
  include_signatures Deft.Signatures.List
  include_signatures Deft.Signatures.String
  include_signatures Deft.Signatures.Integer
  include_signatures Deft.Signatures.Float
  include_signatures Deft.Signatures.Tuple
  include_signatures Deft.Signatures.IO

  include_datatypes Deft.Datatypes.Core

  features([:exhaustiveness_checking])
end
