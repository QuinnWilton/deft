defmodule Deft.Rules do
  @moduledoc """
  Typing rules for the Deft type system.

  This module defines the behaviour for typing rules and provides
  the registry for rule composition and lookup.

  ## Rule Modules

  - `Deft.Rules.Core` - Literals, variables, annotations, tuples, lists
  - `Deft.Rules.Functions` - Anonymous functions and application
  - `Deft.Rules.ControlFlow` - If, cond, case, match expressions
  - `Deft.Rules.Builtins` - Guards and type constructors

  ## Implementing a Rule

  Rules are defined using the `Deft.Rules.DSL`:

      defmodule MyRules do
        use Deft.Rules.DSL

        defrule :my_rule,
          match: %MyAST{},
          judgment: :synth,
          do: (
            # rule implementation
            emit(erased, type)
          )
      end

  ## Rule Results

  Rules return a tuple:
  - `{:ok, erased_ast, type, bindings, updated_ctx}` on success
  - `{:error, error}` on type error
  """

  alias Deft.Context

  # Type definitions for rules
  @type judgment :: :synth | :check | :both
  @type binding :: {term(), Deft.Type.t()}
  # Note: type is `term()` because some rules (e.g., :case_branch) return compound
  # types like `{pat_t, body_t}` that get unpacked by parent rules.
  @type success ::
          {:ok, erased_ast :: term(), type :: term(), bindings :: [binding()], Context.t()}
  @type error :: {:error, term()}
  @type result :: success() | error()

  # Behaviour callbacks
  @doc """
  Returns the name of this rule for debugging/logging.
  """
  @callback name() :: atom()

  @doc """
  Returns the judgment mode: :synth, :check, or :both.
  """
  @callback judgment() :: judgment()

  @doc """
  Returns true if this rule can handle the given AST node.
  """
  @callback matches?(ast :: term()) :: boolean()

  @doc """
  Applies the typing rule to the AST node.

  For synthesis rules, `expected_type` will be nil.
  For checking rules, `expected_type` will be the type to check against.
  """
  @callback apply(ast :: term(), expected_type :: Deft.Type.t() | nil, ctx :: Context.t()) ::
              result()

  @doc """
  Optional callback for rules that need special handling during pattern matching.
  """
  @callback match_pattern(pattern :: term(), against_type :: Deft.Type.t(), ctx :: Context.t()) ::
              {:ok, erased :: term(), pattern_type :: Deft.Type.t(), bindings :: [binding()],
               Context.t()}
              | {:error, term()}
              | :not_a_pattern
  @optional_callbacks match_pattern: 3

  # Helper functions

  @doc """
  Helper to create a successful rule result.
  """
  @spec ok(term(), Deft.Type.t(), [binding()], Context.t()) :: success()
  def ok(erased_ast, type, bindings, ctx) do
    {:ok, erased_ast, type, bindings, ctx}
  end

  @doc """
  Helper to create an error result.
  """
  @spec error(term()) :: error()
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
  @spec registry() :: Deft.Rules.Registry.t()
  def registry do
    Deft.Rules.Registry.new(all_rules())
  end
end

defmodule Deft.Rules.Registry do
  @moduledoc """
  Registry for typing rules.

  Rules are stored in order and the first matching rule is applied.
  This allows for rule composition and overriding.
  """

  alias Deft.Context

  defstruct rules: []

  @type t :: %__MODULE__{rules: [module()]}

  @doc """
  Creates a new empty registry.
  """
  @spec new() :: t()
  def new, do: %__MODULE__{}

  @doc """
  Creates a registry with the given rules.
  """
  @spec new([module()]) :: t()
  def new(rules) when is_list(rules) do
    %__MODULE__{rules: rules}
  end

  @doc """
  Adds a rule to the registry.
  """
  @spec add(t(), module()) :: t()
  def add(%__MODULE__{rules: rules} = registry, rule) do
    %{registry | rules: rules ++ [rule]}
  end

  @doc """
  Prepends a rule to the registry (higher priority).
  """
  @spec prepend(t(), module()) :: t()
  def prepend(%__MODULE__{rules: rules} = registry, rule) do
    %{registry | rules: [rule | rules]}
  end

  @doc """
  Finds the first rule that matches the given AST node.
  """
  @spec find_rule(t(), term()) :: {:ok, module()} | :error
  def find_rule(%__MODULE__{rules: rules}, ast) do
    case Enum.find(rules, fn rule -> rule.matches?(ast) end) do
      nil -> :error
      rule -> {:ok, rule}
    end
  end

  @doc """
  Applies the first matching rule to the AST node.
  """
  @spec apply_rule(t(), term(), Deft.Type.t() | nil, Context.t()) ::
          Deft.Rules.result() | {:error, :no_matching_rule}
  def apply_rule(%__MODULE__{} = registry, ast, expected_type, ctx) do
    case find_rule(registry, ast) do
      {:ok, rule} ->
        rule.apply(ast, expected_type, ctx)

      :error ->
        {:error, {:no_matching_rule, ast}}
    end
  end
end
