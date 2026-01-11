defmodule Deft.Context do
  @moduledoc """
  Unified typing context that replaces scattered env/opts threading.

  The context carries all information needed during type checking:
  - The Elixir macro environment
  - Type bindings for variables
  - ADT definitions
  - Feature flags for optional type system features
  - Error accumulation support
  - Source location tracking

  ## Error Accumulation

  The context supports accumulating multiple errors during type checking
  rather than failing on the first error. This provides better UX by
  showing all errors at once.

      ctx = Context.new(env, error_mode: :accumulate)
      # ... type checking ...
      errors = Context.get_errors(ctx)
  """

  alias Deft.Type
  alias Deft.Error

  defstruct [
    # Elixir macro environment from __CALLER__
    :env,
    # Variable name -> type mappings (list of {AST.Local, Type} tuples)
    type_env: [],
    # ADT name -> Type.ADT mappings
    adt_env: [],
    # Function signatures (for future use)
    signature_env: %{},
    # Enabled features like :polymorphism, :strict_subtyping
    features: [],
    # Callback for type computation events (debugging/logging)
    on_compute: nil,
    # Source location tracking for error messages
    source_map: %{},
    # Error accumulation mode: :fail_fast | :accumulate
    error_mode: :fail_fast,
    # Accumulated errors (when error_mode is :accumulate)
    errors: [],
    # Current file being processed (for error locations)
    current_file: nil,
    # Source code lines (for error context display)
    source_lines: nil
  ]

  @type error_mode :: :fail_fast | :accumulate

  @type t :: %__MODULE__{
          env: Macro.Env.t() | nil,
          type_env: [{term(), Type.t()}],
          adt_env: [{atom(), Type.t()}],
          signature_env: map(),
          features: [atom()],
          on_compute: (term(), term() -> any()) | nil,
          source_map: map(),
          error_mode: error_mode(),
          errors: [Error.t()],
          current_file: String.t() | nil,
          source_lines: [String.t()] | nil
        }

  @type binding ::
          {term(), Type.t()}
          | {:adt, term(), Type.t()}
          | {:adt_variant, atom(), Type.t(), Type.t()}

  @doc """
  Creates a new context from a macro environment.
  """
  @spec new(Macro.Env.t()) :: t()
  def new(env) do
    %__MODULE__{env: env}
  end

  @doc """
  Creates a new context with options.

  ## Options

  - `:on_compute` - Callback function invoked on type computation
  - `:features` - List of enabled type system features
  - `:error_mode` - Error handling mode: `:fail_fast` (default) or `:accumulate`
  - `:source_lines` - Source code lines for error context display
  """
  @spec new(Macro.Env.t(), keyword()) :: t()
  def new(env, opts) when is_list(opts) do
    %__MODULE__{
      env: env,
      on_compute: Keyword.get(opts, :on_compute),
      features: Keyword.get(opts, :features, []),
      error_mode: Keyword.get(opts, :error_mode, :fail_fast),
      current_file: if(env, do: env.file, else: nil),
      source_lines: Keyword.get(opts, :source_lines)
    }
  end

  @doc """
  Adds a variable binding to the context.
  """
  @spec bind(t(), term(), Type.t()) :: t()
  def bind(%__MODULE__{} = ctx, var, type) do
    %{ctx | type_env: [{var, type} | ctx.type_env]}
  end

  @doc """
  Adds multiple bindings to the context.
  """
  @spec bind_all(t(), [binding()]) :: t()
  def bind_all(%__MODULE__{} = ctx, bindings) when is_list(bindings) do
    Enum.reduce(bindings, ctx, fn
      {:adt, name, type}, acc ->
        bind_adt(acc, name, type)

      {:adt_variant, name, adt_type, variant}, acc ->
        bind_adt_variant(acc, name, adt_type, variant)

      {var, type}, acc ->
        bind(acc, var, type)
    end)
  end

  @doc """
  Adds an ADT definition to the context.
  """
  @spec bind_adt(t(), term(), Type.t()) :: t()
  def bind_adt(%__MODULE__{} = ctx, name, type) do
    %{ctx | adt_env: [{:adt, name, type} | ctx.adt_env]}
  end

  @doc """
  Adds an ADT variant binding to the context.
  """
  @spec bind_adt_variant(t(), atom(), Type.t(), Type.t()) :: t()
  def bind_adt_variant(%__MODULE__{} = ctx, name, adt_type, variant) do
    %{ctx | adt_env: [{:adt_variant, name, adt_type, variant} | ctx.adt_env]}
  end

  @doc """
  Looks up a variable's type in the context.
  """
  @spec lookup(t(), term()) :: {:ok, Type.t()} | :error
  def lookup(%__MODULE__{type_env: type_env}, var) do
    case List.keyfind(type_env, var, 0) do
      {^var, type} -> {:ok, type}
      nil -> :error
    end
  end

  @doc """
  Checks if a feature is enabled.
  """
  @spec feature_enabled?(t(), atom()) :: boolean()
  def feature_enabled?(%__MODULE__{features: features}, feature) do
    feature in features
  end

  @doc """
  Invokes the on_compute callback if present.
  """
  @spec notify_compute(t(), term(), Type.t()) :: :ok
  def notify_compute(%__MODULE__{on_compute: nil}, _ast, _type), do: :ok

  def notify_compute(%__MODULE__{on_compute: callback}, ast, type) do
    callback.(ast, type)
    :ok
  end

  @doc """
  Returns all bindings (type_env + adt_env) for injection.
  """
  @spec all_bindings(t()) :: [binding()]
  def all_bindings(%__MODULE__{type_env: type_env, adt_env: adt_env}) do
    adt_env ++ type_env
  end

  # ============================================================================
  # Error Accumulation
  # ============================================================================

  @doc """
  Adds an error to the context.

  In `:fail_fast` mode, this will raise the error as an exception.
  In `:accumulate` mode, the error is stored for later retrieval.
  """
  @spec add_error(t(), Error.t()) :: t() | no_return()
  def add_error(%__MODULE__{error_mode: :fail_fast} = ctx, %Error{} = error) do
    # In fail_fast mode, convert to exception and raise
    raise error_to_exception(error, ctx)
  end

  def add_error(%__MODULE__{error_mode: :accumulate, errors: errors} = ctx, %Error{} = error) do
    # Enrich error with context if not already set
    error = enrich_error(error, ctx)
    %{ctx | errors: errors ++ [error]}
  end

  @doc """
  Returns all accumulated errors.
  """
  @spec get_errors(t()) :: [Error.t()]
  def get_errors(%__MODULE__{errors: errors}), do: errors

  @doc """
  Returns true if there are any accumulated errors.
  """
  @spec has_errors?(t()) :: boolean()
  def has_errors?(%__MODULE__{errors: errors}), do: errors != []

  @doc """
  Clears all accumulated errors.
  """
  @spec clear_errors(t()) :: t()
  def clear_errors(%__MODULE__{} = ctx), do: %{ctx | errors: []}

  @doc """
  Records a source location in the source map for an AST node.
  """
  @spec record_location(t(), term(), Error.location()) :: t()
  def record_location(%__MODULE__{source_map: source_map} = ctx, ast_node, location) do
    %{ctx | source_map: Map.put(source_map, ast_node, location)}
  end

  @doc """
  Gets the recorded location for an AST node.
  """
  @spec get_location(t(), term()) :: Error.location() | nil
  def get_location(%__MODULE__{source_map: source_map}, ast_node) do
    Map.get(source_map, ast_node)
  end

  @doc """
  Attempts to run a function, catching errors in accumulate mode.

  In `:fail_fast` mode, errors are raised normally.
  In `:accumulate` mode, errors are caught, stored, and a recovery value is returned.
  """
  @spec with_error_handling(t(), (-> {:ok, term()} | {:error, Error.t()}), term()) ::
          {:ok, term(), t()} | {:error, term(), t()}
  def with_error_handling(%__MODULE__{error_mode: :fail_fast} = ctx, fun, _recovery) do
    case fun.() do
      {:ok, result} -> {:ok, result, ctx}
      {:error, %Error{} = error} -> raise error_to_exception(error, ctx)
      {:error, reason} -> {:error, reason, ctx}
    end
  end

  def with_error_handling(%__MODULE__{error_mode: :accumulate} = ctx, fun, recovery) do
    case fun.() do
      {:ok, result} ->
        {:ok, result, ctx}

      {:error, %Error{} = error} ->
        ctx = add_error(ctx, error)
        {:ok, recovery, ctx}

      {:error, reason} ->
        {:error, reason, ctx}
    end
  end

  @doc """
  Gets source lines around a given line number for error context.
  """
  @spec get_source_context(t(), non_neg_integer(), non_neg_integer()) ::
          {String.t() | nil, String.t() | nil, String.t() | nil}
  def get_source_context(%__MODULE__{source_lines: nil}, _line, _context_lines) do
    {nil, nil, nil}
  end

  def get_source_context(%__MODULE__{source_lines: lines}, line, context_lines)
      when is_list(lines) do
    # Line numbers are 1-indexed
    idx = line - 1

    before_start = max(0, idx - context_lines)
    before_lines = Enum.slice(lines, before_start, idx - before_start)

    current_line = Enum.at(lines, idx)

    after_end = min(length(lines), idx + 1 + context_lines)
    after_lines = Enum.slice(lines, idx + 1, after_end - idx - 1)

    {
      if(before_lines != [], do: Enum.join(before_lines, "\n"), else: nil),
      current_line,
      if(after_lines != [], do: Enum.join(after_lines, "\n"), else: nil)
    }
  end

  # ============================================================================
  # Private Helpers
  # ============================================================================

  defp enrich_error(%Error{location: nil} = error, %__MODULE__{current_file: file, env: env}) do
    location =
      if env do
        {file || env.file, env.line, nil}
      else
        nil
      end

    %{error | location: location}
  end

  defp enrich_error(%Error{} = error, _ctx), do: error

  defp error_to_exception(%Error{} = error, _ctx) do
    Error.to_exception(error)
  end
end
