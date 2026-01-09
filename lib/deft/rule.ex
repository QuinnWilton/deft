defmodule Deft.Rule do
  @moduledoc """
  Behaviour for defining typing rules.

  A typing rule specifies how to type check a particular AST node.
  Rules can operate in different modes:
  - `:synth` - Synthesize a type from the expression (bottom-up)
  - `:check` - Check an expression against an expected type (top-down)
  - `:both` - Can operate in either mode

  ## Implementing a Rule

      defmodule Deft.Rules.Literal do
        @behaviour Deft.Rule

        @impl true
        def name, do: :literal

        @impl true
        def judgment, do: :synth

        @impl true
        def matches?(%Deft.AST.Literal{}), do: true
        def matches?(_), do: false

        @impl true
        def apply(%Deft.AST.Literal{value: value}, _expected, ctx) do
          type = type_of_literal(value)
          {:ok, value, type, [], ctx}
        end

        defp type_of_literal(v) when is_boolean(v), do: Deft.Type.boolean()
        defp type_of_literal(v) when is_atom(v), do: Deft.Type.atom()
        defp type_of_literal(v) when is_integer(v), do: Deft.Type.integer()
        defp type_of_literal(v) when is_float(v), do: Deft.Type.float()
      end

  ## Rule Results

  The `apply/3` callback returns a tuple:
  - `{:ok, erased_ast, type, bindings, updated_ctx}` on success
  - `{:error, error}` on type error

  Where:
  - `erased_ast` is the AST with type annotations removed
  - `type` is the inferred/checked type
  - `bindings` is a list of new variable bindings introduced
  - `updated_ctx` is the potentially modified context
  """

  alias Deft.Context

  @type judgment :: :synth | :check | :both

  @type binding :: {term(), Deft.Type.t()}

  @type success ::
          {:ok, erased_ast :: term(), type :: Deft.Type.t(), bindings :: [binding()], Context.t()}
  @type error :: {:error, term()}
  @type result :: success() | error()

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
  Default implementation returns :not_a_pattern.
  """
  @callback match_pattern(pattern :: term(), against_type :: Deft.Type.t(), ctx :: Context.t()) ::
              {:ok, erased :: term(), pattern_type :: Deft.Type.t(), bindings :: [binding()],
               Context.t()}
              | {:error, term()}
              | :not_a_pattern
  @optional_callbacks match_pattern: 3
end

defmodule Deft.Rule.Registry do
  @moduledoc """
  Registry for typing rules.

  Rules are stored in order and the first matching rule is applied.
  This allows for rule composition and overriding.
  """

  alias Deft.Context
  alias Deft.Rule

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
          Rule.result() | {:error, :no_matching_rule}
  def apply_rule(%__MODULE__{} = registry, ast, expected_type, ctx) do
    case find_rule(registry, ast) do
      {:ok, rule} ->
        rule.apply(ast, expected_type, ctx)

      :error ->
        {:error, {:no_matching_rule, ast}}
    end
  end
end

defmodule Deft.Rules do
  @moduledoc """
  Helper module for defining and composing typing rules.
  """

  alias Deft.Context
  alias Deft.Rule

  @doc """
  Macro for defining a simple typing rule.
  """
  defmacro defrule(name, opts) do
    judgment = Keyword.get(opts, :judgment, :synth)
    match_pattern = Keyword.fetch!(opts, :match)
    body = Keyword.fetch!(opts, :do)

    quote do
      @behaviour Deft.Rule

      @impl true
      def name, do: unquote(name)

      @impl true
      def judgment, do: unquote(judgment)

      @impl true
      def matches?(unquote(match_pattern)), do: true
      def matches?(_), do: false

      @impl true
      def apply(unquote(match_pattern) = ast, expected_type, ctx) do
        _ = expected_type
        unquote(body)
      end
    end
  end

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
end
