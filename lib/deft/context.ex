defmodule Deft.Context do
  @moduledoc """
  Unified typing context that replaces scattered env/opts threading.

  The context carries all information needed during type checking:
  - The Elixir macro environment
  - Type bindings for variables
  - ADT definitions
  - Feature flags for optional type system features
  - Error accumulation support
  """

  alias Deft.Type

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
    source_map: %{}
  ]

  @type t :: %__MODULE__{
          env: Macro.Env.t() | nil,
          type_env: [{term(), Type.t()}],
          adt_env: [{atom(), Type.t()}],
          signature_env: map(),
          features: [atom()],
          on_compute: (term(), term() -> any()) | nil,
          source_map: map()
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
  """
  @spec new(Macro.Env.t(), keyword()) :: t()
  def new(env, opts) when is_list(opts) do
    %__MODULE__{
      env: env,
      on_compute: Keyword.get(opts, :on_compute),
      features: Keyword.get(opts, :features, [])
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

  @doc """
  Converts context back to legacy opts format for gradual migration.
  """
  @spec to_opts(t()) :: keyword()
  def to_opts(%__MODULE__{} = ctx) do
    opts = []
    opts = if ctx.on_compute, do: [{:on_compute, ctx.on_compute} | opts], else: opts
    opts
  end
end
