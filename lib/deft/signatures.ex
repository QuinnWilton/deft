defmodule Deft.Signatures do
  @moduledoc """
  Registry for function type signatures.

  This module provides a centralized way to:
  - Register type signatures for functions
  - Look up signatures for type checking
  - Store built-in Elixir function signatures

  Signatures are stored in the process dictionary, giving each process its
  own isolated registry. This avoids race conditions during parallel
  compilation while ensuring signatures are always available.

  ## Usage

      # Look up a signature
      {:ok, sig} = Deft.Signatures.lookup({Kernel, :+, 2})

      # Register a custom signature
      Deft.Signatures.register({MyModule, :my_fun, 2}, sig)

  ## Signature Format

  Signatures are represented as `Deft.Type.Fn` structs, or `Deft.Type.Forall`
  for polymorphic functions:

      # Monomorphic
      Type.fun([Type.integer(), Type.integer()], Type.integer())

      # Polymorphic
      Type.forall([:a], Type.fun([Type.fixed_list(Type.var(:a))], Type.var(:a)))
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

  List operations like `hd`, `tl`, `++`, `--` are polymorphic,
  allowing them to preserve element type information.
  """
  @spec builtins() :: %{{module(), atom(), non_neg_integer()} => Type.Fn.t() | Type.Forall.t()}
  def builtins do
    # Type variables for polymorphic signatures
    a = Type.var(:a)
    b = Type.var(:b)

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

      # Polymorphic list operations
      {Kernel, :hd, 1} => Type.forall([:a], Type.fun([Type.fixed_list(a)], a)),
      {Kernel, :tl, 1} => Type.forall([:a], Type.fun([Type.fixed_list(a)], Type.fixed_list(a))),
      {Kernel, :length, 1} => Type.fun([Type.list()], Type.integer()),
      # ++ concatenates two lists, result contains union of element types
      {Kernel, :++, 2} =>
        Type.forall(
          [:a, :b],
          Type.fun([Type.fixed_list(a), Type.fixed_list(b)], Type.fixed_list(Type.union(a, b)))
        ),
      # -- removes elements, result type is same as first list
      {Kernel, :--, 2} =>
        Type.forall([:a, :b], Type.fun([Type.fixed_list(a), Type.fixed_list(b)], Type.fixed_list(a))),

      # Tuple operations
      {Kernel, :elem, 2} => Type.fun([Type.tuple(), Type.integer()], Type.top()),
      {Kernel, :tuple_size, 1} => Type.fun([Type.tuple()], Type.integer()),
      {Kernel, :put_elem, 3} =>
        Type.fun([Type.tuple(), Type.integer(), Type.top()], Type.tuple()),

      # String operations
      {String, :to_integer, 1} => Type.fun([Type.binary()], Type.integer()),
      {String, :to_float, 1} => Type.fun([Type.binary()], Type.float()),

      # IO operations
      {IO, :puts, 1} => Type.fun([Type.top()], Type.atom())
    }
  end

  # ============================================================================
  # Public API
  # ============================================================================

  @doc """
  Initializes the signature registry for the current process.

  This populates the process-local registry with built-in signatures.
  Called automatically on first access via `ensure_initialized/0`.
  """
  @spec init() :: :ok
  def init do
    # Initialize the process-local registry with builtins
    Process.put(@table_name, builtins())
    :ok
  end

  @doc """
  Registers a function signature.

  Signatures can be either a direct function type (`Type.Fn`) or a
  polymorphic function type (`Type.Forall` wrapping a `Type.Fn`).

  ## Example

      # Monomorphic function
      Deft.Signatures.register(
        {MyModule, :my_fun, 2},
        Type.fun([Type.integer(), Type.integer()], Type.boolean())
      )

      # Polymorphic function
      Deft.Signatures.register(
        {Enum, :map, 2},
        Type.forall([:a, :b], Type.fun([Type.fixed_list(Type.var(:a)), ...], ...))
      )
  """
  @spec register({module(), atom(), non_neg_integer()}, Type.Fn.t() | Type.Forall.t()) :: :ok
  def register({module, function, arity} = mfa, %Type.Fn{} = type)
      when is_atom(module) and is_atom(function) and is_integer(arity) and arity >= 0 do
    ensure_initialized()
    registry = Process.get(@table_name)
    Process.put(@table_name, Map.put(registry, mfa, type))
    :ok
  end

  def register({module, function, arity} = mfa, %Type.Forall{} = type)
      when is_atom(module) and is_atom(function) and is_integer(arity) and arity >= 0 do
    ensure_initialized()
    registry = Process.get(@table_name)
    Process.put(@table_name, Map.put(registry, mfa, type))
    :ok
  end

  @doc """
  Looks up a function signature by module, function name, and arity.

  Returns `{:ok, type}` if found, `:error` otherwise.

  ## Example

      {:ok, sig} = Deft.Signatures.lookup({Kernel, :+, 2})
  """
  @spec lookup({module(), atom(), non_neg_integer()}) :: {:ok, Type.Fn.t() | Type.Forall.t()} | :error
  def lookup({module, function, arity} = mfa)
      when is_atom(module) and is_atom(function) and is_integer(arity) do
    ensure_initialized()
    registry = Process.get(@table_name)
    Map.fetch(registry, mfa)
  end

  @doc """
  Looks up a function signature, returning the type or nil.
  """
  @spec get({module(), atom(), non_neg_integer()}) :: Type.Fn.t() | Type.Forall.t() | nil
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
    registry = Process.get(@table_name)
    Process.put(@table_name, Map.delete(registry, mfa))
    :ok
  end

  @doc """
  Returns all registered signatures as a list of `{mfa, type}` tuples.
  """
  @spec all() :: [{{module(), atom(), non_neg_integer()}, Type.Fn.t() | Type.Forall.t()}]
  def all do
    ensure_initialized()
    registry = Process.get(@table_name)
    Map.to_list(registry)
  end

  @doc """
  Clears all signatures from the registry and reloads builtins.
  """
  @spec reset() :: :ok
  def reset do
    Process.put(@table_name, builtins())
    :ok
  end

  # ============================================================================
  # Private Helpers
  # ============================================================================

  defp ensure_initialized do
    if Process.get(@table_name) == nil do
      init()
    end
  end
end
