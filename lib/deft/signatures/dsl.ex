defmodule Deft.Signatures.DSL do
  @moduledoc """
  DSL for declaring type signatures for external functions.

  This module provides macros for defining signature modules that can be
  included in type systems. Signatures are scoped to the type system that
  includes them, preventing leakage across compilation units.

  ## Usage

      defmodule Deft.Signatures.Kernel do
        use Deft.Signatures.DSL, for: Kernel

        # Monomorphic signatures
        sig abs(number) :: number
        sig div(integer, integer) :: integer

        # Polymorphic signatures (single lowercase letters are type variables)
        sig hd([a]) :: a
        sig tl([a]) :: [a]
        sig ++([a], [b]) :: [a | b]

        # Higher-order functions
        sig map([a], (a -> b)) :: [b]
      end

  ## Type Syntax

  - `integer`, `float`, `number`, `boolean`, `atom`, `string`, `binary` - base types
  - `[a]` - list with element type `a`
  - `{a, b}` or `{a, b, c}` - tuple types
  - `a | b` - union type
  - `(a -> b)` or `(a, b -> c)` - function type
  - `top`, `bottom` - top and bottom types
  - Single lowercase letters (`a`, `b`, etc.) - type variables (polymorphic)

  ## Generated Functions

  Each signature module generates a `signatures/0` function that returns
  a map of `{module, function, arity} => type`.
  """

  alias Deft.Error
  alias Deft.TypeParser
  alias Deft.TypeParser.Emitter

  @doc """
  Enables the signature DSL in a module.

  ## Options

  - `:for` - Required. The source module whose functions are being declared.

  ## Example

      use Deft.Signatures.DSL, for: Kernel
  """
  defmacro __using__(opts) do
    source_module = Keyword.fetch!(opts, :for)

    quote do
      import Deft.Signatures.DSL, only: [sig: 1, sig: 2, sig_unsupported: 2, sig_unsupported: 3]
      require Deft.Signatures.DSL

      Module.register_attribute(__MODULE__, :deft_source_module, persist: true)
      Module.register_attribute(__MODULE__, :deft_signatures, accumulate: true, persist: true)

      Module.put_attribute(__MODULE__, :deft_source_module, unquote(source_module))

      @before_compile Deft.Signatures.DSL
    end
  end

  @doc false
  defmacro __before_compile__(env) do
    source_module = Module.get_attribute(env.module, :deft_source_module)
    signatures = Module.get_attribute(env.module, :deft_signatures) || []

    # Build the map entries as AST
    # Each entry is: {source_module, name, arity} => type_ast
    # The type_ast was escaped when stored, so it's now data representing AST
    map_entries =
      signatures
      |> Enum.reverse()
      |> Enum.map(fn {name, arity, type_ast} ->
        key = {source_module, name, arity}
        # type_ast is the escaped AST (data), insert it directly to be evaluated at runtime
        quote do
          {unquote(Macro.escape(key)), unquote(type_ast)}
        end
      end)

    quote do
      @doc """
      Returns all signatures defined in this module.

      Returns a map of `{module, function, arity} => type`.
      """
      @spec signatures() :: %{{module(), atom(), non_neg_integer()} => Deft.Type.t()}
      def signatures do
        [unquote_splicing(map_entries)] |> Enum.into(%{})
      end
    end
  end

  @doc """
  Declares a type signature for a function.

  ## Syntax

  For regular function names:

      sig name(arg_type1, arg_type2, ...) :: return_type

  For operators (which can't be used directly as function names):

      sig :name, [arg_type1, arg_type2, ...] :: return_type

  ## Examples

      # Regular functions
      sig abs(number) :: number
      sig hd([a]) :: a
      sig map([a], (a -> b)) :: [b]

      # Operators (use atom + list syntax)
      sig :+, [number, number] :: number
      sig :++, [[a], [b]] :: [a | b]
  """
  # Standard syntax: sig name(args) :: return
  defmacro sig({:"::", _, [{name, _, args}, return_type]}) when is_atom(name) do
    args = args || []
    build_signature(name, args, return_type)
  end

  defmacro sig(expr) do
    raise ArgumentError,
          "Invalid sig syntax. Expected: sig name(types...) :: return_type " <>
            "or sig :name, [types...] :: return_type\n" <>
            "Got: #{Macro.to_string(expr)}"
  end

  # Operator syntax: sig :name, [args] :: return
  defmacro sig(name, {:"::", _, [args, return_type]}) when is_atom(name) and is_list(args) do
    build_signature(name, args, return_type)
  end

  defmacro sig(name, expr) do
    raise ArgumentError,
          "Invalid sig syntax. Expected: sig :name, [types...] :: return_type\n" <>
            "Got: sig #{inspect(name)}, #{Macro.to_string(expr)}"
  end

  @doc """
  Marks a function as explicitly unsupported with a reason.

  When called, the type checker will emit a compile-time error with
  the reason explaining why this function cannot be typed.

  ## Syntax

  For regular function names:

      sig_unsupported name(arg_types...) :: return_type,
        reason: "Why this function cannot be typed"

  For operators:

      sig_unsupported :name, [arg_types...] :: return_type,
        reason: "Why this function cannot be typed"

  ## Examples

      # Map-returning function
      sig_unsupported group_by([a], (a -> b)) :: top,
        reason: "Returns a map type which Deft cannot represent"

      # Tagged tuple return
      sig_unsupported parse(binary) :: {integer, binary},
        reason: "Returns {:ok, value} | :error which requires literal atom types"
  """
  # Standard syntax: sig_unsupported name(args) :: return, reason: "..."
  defmacro sig_unsupported({:"::", _, [{name, _, args}, _return_type]}, opts)
           when is_atom(name) do
    args = args || []
    reason = Keyword.fetch!(opts, :reason)
    build_unsupported_signature(name, length(args), reason)
  end

  defmacro sig_unsupported(expr, opts) when is_list(opts) do
    raise ArgumentError,
          "Invalid sig_unsupported syntax. Expected: sig_unsupported name(types...) :: return_type, reason: \"...\"\n" <>
            "or sig_unsupported :name, [types...] :: return_type, reason: \"...\"\n" <>
            "Got: sig_unsupported #{Macro.to_string(expr)}, #{inspect(opts)}"
  end

  # Operator syntax: sig_unsupported :name, [args] :: return, reason: "..."
  defmacro sig_unsupported(name, {:"::", _, [args, _return_type]}, opts)
           when is_atom(name) and is_list(args) do
    reason = Keyword.fetch!(opts, :reason)
    build_unsupported_signature(name, length(args), reason)
  end

  defmacro sig_unsupported(name, expr, opts) when is_list(opts) do
    raise ArgumentError,
          "Invalid sig_unsupported syntax. Expected: sig_unsupported :name, [types...] :: return_type, reason: \"...\"\n" <>
            "Got: sig_unsupported #{inspect(name)}, #{Macro.to_string(expr)}, #{inspect(opts)}"
  end

  defp build_unsupported_signature(name, arity, reason) do
    # Build the AST for creating the Type.Unsupported struct at runtime
    # Note: @deft_source_module is resolved at compile-time in the target module
    type_ast =
      quote do
        Deft.Type.Unsupported.new(
          @deft_source_module,
          unquote(name),
          unquote(arity),
          unquote(reason)
        )
      end

    # Escape the AST so it's stored as data, not evaluated
    escaped_ast = Macro.escape(type_ast)

    quote do
      @deft_signatures {unquote(name), unquote(arity), unquote(escaped_ast)}
    end
  end

  defp build_signature(name, args, return_type) do
    opts = [output: :ast, allow_variables: true]

    # Parse all argument types
    arg_asts =
      Enum.map(args, fn arg ->
        case TypeParser.parse(arg, opts) do
          {:ok, ast} -> ast
          {:error, error} -> raise ArgumentError, format_error(error)
        end
      end)

    # Parse return type
    return_ast =
      case TypeParser.parse(return_type, opts) do
        {:ok, ast} -> ast
        {:error, error} -> raise ArgumentError, format_error(error)
      end

    # Collect type variables from all parsed ASTs
    all_asts = arg_asts ++ [return_ast]
    type_vars = Enum.flat_map(all_asts, &Emitter.collect_variables/1) |> Enum.uniq()

    # Convert to quoted AST
    arg_types = Enum.map(arg_asts, &Emitter.to_quoted/1)
    return_type_quoted = Emitter.to_quoted(return_ast)

    arity = length(arg_types)

    # Build the type list
    arg_types_list = quote do: [unquote_splicing(arg_types)]

    # Create function type, wrapped in forall if polymorphic
    fn_type_ast =
      if type_vars == [] do
        quote do: Deft.Type.fun(unquote(arg_types_list), unquote(return_type_quoted))
      else
        quote do
          Deft.Type.forall(
            unquote(type_vars),
            Deft.Type.fun(unquote(arg_types_list), unquote(return_type_quoted))
          )
        end
      end

    # Escape the AST so it's stored as data, not evaluated
    escaped_ast = Macro.escape(fn_type_ast)

    quote do
      @deft_signatures {unquote(name), unquote(arity), unquote(escaped_ast)}
    end
  end

  defp format_error(%Error{} = error) do
    Error.Formatter.format_simple(error)
  end
end
