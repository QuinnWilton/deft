defmodule Deft.Signatures do
  @moduledoc """
  Registry for function type signatures.

  This module provides a centralized way to:
  - Register type signatures for functions
  - Look up signatures for type checking
  - Store built-in Elixir function signatures

  Signatures are stored in ETS at compile time for efficient lookup.
  At runtime, the module is initialized with built-in signatures when
  the application starts.

  ## Usage

      # Look up a signature
      {:ok, sig} = Deft.Signatures.lookup({Kernel, :+, 2})

      # Register a custom signature
      Deft.Signatures.register({MyModule, :my_fun, 2}, sig)

  ## Signature Format

  Signatures are represented as `Deft.Type.Fn` structs:

      Type.fun([Type.integer(), Type.integer()], Type.integer())
  """

  alias Deft.Type

  @table_name :deft_signatures

  # ============================================================================
  # Built-in Signatures
  # ============================================================================

  @doc """
  Returns the built-in signatures for Kernel functions.

  These are the type signatures for Elixir's standard library
  functions that are commonly used in typed code.
  """
  @spec builtins() :: %{{module(), atom(), non_neg_integer()} => Type.Fn.t()}
  def builtins do
    %{
      # Arithmetic operators
      {Kernel, :+, 2} => Type.fun([Type.number(), Type.number()], Type.number()),
      {Kernel, :-, 2} => Type.fun([Type.number(), Type.number()], Type.number()),
      {Kernel, :*, 2} => Type.fun([Type.number(), Type.number()], Type.number()),
      {Kernel, :/, 2} => Type.fun([Type.number(), Type.number()], Type.float()),
      {Kernel, :+, 1} => Type.fun([Type.number()], Type.number()),
      {Kernel, :-, 1} => Type.fun([Type.number()], Type.number()),

      # Integer division and remainder
      {Kernel, :div, 2} => Type.fun([Type.integer(), Type.integer()], Type.integer()),
      {Kernel, :rem, 2} => Type.fun([Type.integer(), Type.integer()], Type.integer()),

      # Comparison operators (return boolean)
      {Kernel, :==, 2} => Type.fun([Type.top(), Type.top()], Type.boolean()),
      {Kernel, :!=, 2} => Type.fun([Type.top(), Type.top()], Type.boolean()),
      {Kernel, :===, 2} => Type.fun([Type.top(), Type.top()], Type.boolean()),
      {Kernel, :!==, 2} => Type.fun([Type.top(), Type.top()], Type.boolean()),
      {Kernel, :<, 2} => Type.fun([Type.top(), Type.top()], Type.boolean()),
      {Kernel, :<=, 2} => Type.fun([Type.top(), Type.top()], Type.boolean()),
      {Kernel, :>, 2} => Type.fun([Type.top(), Type.top()], Type.boolean()),
      {Kernel, :>=, 2} => Type.fun([Type.top(), Type.top()], Type.boolean()),

      # Boolean operators
      {Kernel, :not, 1} => Type.fun([Type.boolean()], Type.boolean()),
      {Kernel, :and, 2} => Type.fun([Type.boolean(), Type.boolean()], Type.boolean()),
      {Kernel, :or, 2} => Type.fun([Type.boolean(), Type.boolean()], Type.boolean()),

      # Math functions
      {Kernel, :abs, 1} => Type.fun([Type.number()], Type.number()),
      {Kernel, :ceil, 1} => Type.fun([Type.number()], Type.integer()),
      {Kernel, :floor, 1} => Type.fun([Type.number()], Type.integer()),
      {Kernel, :round, 1} => Type.fun([Type.number()], Type.integer()),
      {Kernel, :trunc, 1} => Type.fun([Type.number()], Type.integer()),
      {Kernel, :max, 2} => Type.fun([Type.number(), Type.number()], Type.number()),
      {Kernel, :min, 2} => Type.fun([Type.number(), Type.number()], Type.number()),

      # Type guards (any -> boolean)
      {Kernel, :is_atom, 1} => Type.fun([Type.top()], Type.boolean()),
      {Kernel, :is_boolean, 1} => Type.fun([Type.top()], Type.boolean()),
      {Kernel, :is_integer, 1} => Type.fun([Type.top()], Type.boolean()),
      {Kernel, :is_float, 1} => Type.fun([Type.top()], Type.boolean()),
      {Kernel, :is_number, 1} => Type.fun([Type.top()], Type.boolean()),
      {Kernel, :is_list, 1} => Type.fun([Type.top()], Type.boolean()),
      {Kernel, :is_tuple, 1} => Type.fun([Type.top()], Type.boolean()),
      {Kernel, :is_function, 1} => Type.fun([Type.top()], Type.boolean()),
      {Kernel, :is_function, 2} => Type.fun([Type.top(), Type.integer()], Type.boolean()),
      {Kernel, :is_binary, 1} => Type.fun([Type.top()], Type.boolean()),
      {Kernel, :is_map, 1} => Type.fun([Type.top()], Type.boolean()),
      {Kernel, :is_nil, 1} => Type.fun([Type.top()], Type.boolean()),

      # List operations
      {Kernel, :hd, 1} => Type.fun([Type.list()], Type.top()),
      {Kernel, :tl, 1} => Type.fun([Type.list()], Type.list()),
      {Kernel, :length, 1} => Type.fun([Type.list()], Type.integer()),
      {Kernel, :++, 2} => Type.fun([Type.list(), Type.list()], Type.list()),
      {Kernel, :--, 2} => Type.fun([Type.list(), Type.list()], Type.list()),

      # Tuple operations
      {Kernel, :elem, 2} => Type.fun([Type.tuple(), Type.integer()], Type.top()),
      {Kernel, :tuple_size, 1} => Type.fun([Type.tuple()], Type.integer()),
      {Kernel, :put_elem, 3} => Type.fun([Type.tuple(), Type.integer(), Type.top()], Type.tuple())
    }
  end

  # ============================================================================
  # Public API
  # ============================================================================

  @doc """
  Initializes the signature registry.

  This creates the ETS table and populates it with built-in signatures.
  Called automatically when the Deft application starts.
  """
  @spec init() :: :ok
  def init do
    # Create table if it doesn't exist, handling race condition
    try do
      :ets.new(@table_name, [:named_table, :set, :public, read_concurrency: true])
    rescue
      ArgumentError -> :ok
    end

    # Load built-in signatures
    for {mfa, type} <- builtins() do
      :ets.insert(@table_name, {mfa, type})
    end

    :ok
  end

  @doc """
  Registers a function signature.

  ## Example

      Deft.Signatures.register(
        {MyModule, :my_fun, 2},
        Type.fun([Type.integer(), Type.integer()], Type.boolean())
      )
  """
  @spec register({module(), atom(), non_neg_integer()}, Type.Fn.t()) :: :ok
  def register({module, function, arity} = mfa, %Type.Fn{} = type)
      when is_atom(module) and is_atom(function) and is_integer(arity) and arity >= 0 do
    ensure_initialized()
    :ets.insert(@table_name, {mfa, type})
    :ok
  end

  @doc """
  Looks up a function signature by module, function name, and arity.

  Returns `{:ok, type}` if found, `:error` otherwise.

  ## Example

      {:ok, sig} = Deft.Signatures.lookup({Kernel, :+, 2})
  """
  @spec lookup({module(), atom(), non_neg_integer()}) :: {:ok, Type.Fn.t()} | :error
  def lookup({module, function, arity} = mfa)
      when is_atom(module) and is_atom(function) and is_integer(arity) do
    ensure_initialized()

    case :ets.lookup(@table_name, mfa) do
      [{^mfa, type}] -> {:ok, type}
      [] -> :error
    end
  end

  @doc """
  Looks up a function signature, returning the type or nil.
  """
  @spec get({module(), atom(), non_neg_integer()}) :: Type.Fn.t() | nil
  def get(mfa) do
    case lookup(mfa) do
      {:ok, type} -> type
      :error -> nil
    end
  end

  @doc """
  Checks if a signature is registered for the given MFA.
  """
  @spec registered?({module(), atom(), non_neg_integer()}) :: boolean()
  def registered?(mfa) do
    lookup(mfa) != :error
  end

  @doc """
  Removes a function signature from the registry.
  """
  @spec unregister({module(), atom(), non_neg_integer()}) :: :ok
  def unregister(mfa) do
    ensure_initialized()
    :ets.delete(@table_name, mfa)
    :ok
  end

  @doc """
  Returns all registered signatures as a list of `{mfa, type}` tuples.
  """
  @spec all() :: [{{module(), atom(), non_neg_integer()}, Type.Fn.t()}]
  def all do
    ensure_initialized()
    :ets.tab2list(@table_name)
  end

  @doc """
  Clears all signatures from the registry and reloads builtins.
  """
  @spec reset() :: :ok
  def reset do
    ensure_initialized()
    :ets.delete_all_objects(@table_name)

    for {mfa, type} <- builtins() do
      :ets.insert(@table_name, {mfa, type})
    end

    :ok
  end

  # ============================================================================
  # Private Helpers
  # ============================================================================

  defp ensure_initialized do
    if :ets.whereis(@table_name) == :undefined do
      init()
    end
  end
end
