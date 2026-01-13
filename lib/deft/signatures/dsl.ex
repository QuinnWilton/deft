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
      import Deft.Signatures.DSL, only: [sig: 1, sig: 2]
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

  defp build_signature(name, args, return_type) do
    arg_types = Enum.map(args, &parse_type/1)
    return_type_parsed = parse_type(return_type)

    # Collect type variables from all types
    all_types = arg_types ++ [return_type_parsed]
    type_vars = collect_type_vars(all_types)

    arity = length(arg_types)

    # Build the type list
    arg_types_list = quote do: [unquote_splicing(arg_types)]

    # Create function type, wrapped in forall if polymorphic
    fn_type_ast =
      if type_vars == [] do
        quote do: Deft.Type.fun(unquote(arg_types_list), unquote(return_type_parsed))
      else
        quote do
          Deft.Type.forall(
            unquote(type_vars),
            Deft.Type.fun(unquote(arg_types_list), unquote(return_type_parsed))
          )
        end
      end

    # Escape the AST so it's stored as data, not evaluated
    escaped_ast = Macro.escape(fn_type_ast)

    quote do
      @deft_signatures {unquote(name), unquote(arity), unquote(escaped_ast)}
    end
  end

  # ============================================================================
  # Type Parsing (extracted from Deft.Declare)
  # ============================================================================

  # Base types
  defp parse_type({:integer, _, _}), do: quote(do: Deft.Type.integer())
  defp parse_type({:float, _, _}), do: quote(do: Deft.Type.float())
  defp parse_type({:number, _, _}), do: quote(do: Deft.Type.number())
  defp parse_type({:boolean, _, _}), do: quote(do: Deft.Type.boolean())
  defp parse_type({:atom, _, _}), do: quote(do: Deft.Type.atom())
  defp parse_type({:list, _, _}), do: quote(do: Deft.Type.list())
  defp parse_type({:tuple, _, _}), do: quote(do: Deft.Type.tuple())
  defp parse_type({:top, _, _}), do: quote(do: Deft.Type.top())
  defp parse_type({:bottom, _, _}), do: quote(do: Deft.Type.bottom())
  defp parse_type({:string, _, _}), do: quote(do: Deft.Type.binary())
  defp parse_type({:binary, _, _}), do: quote(do: Deft.Type.binary())
  defp parse_type({nil, _, _}), do: quote(do: Deft.Type.atom())

  # Type variable (single lowercase letter)
  defp parse_type({name, _, ctx}) when is_atom(name) and is_atom(ctx) do
    name_str = Atom.to_string(name)

    if String.length(name_str) == 1 and name_str =~ ~r/^[a-z]$/ do
      # Type variable - create a Type.Var
      quote(do: Deft.Type.var(unquote(name)))
    else
      raise ArgumentError, "Unknown type: #{name}"
    end
  end

  # Single-element list containing a function type: (a -> b) gets parsed as [{:-> ...}]
  defp parse_type([{:->, _, _} = fn_type]) do
    parse_type(fn_type)
  end

  # List type: [element_type]
  defp parse_type([element_type]) do
    quote do: Deft.Type.fixed_list(unquote(parse_type(element_type)))
  end

  # Empty list - generic list type
  defp parse_type([]) do
    quote do: Deft.Type.list()
  end

  # Tuple type: {type1, type2, ...}
  defp parse_type({:{}, _, types}) do
    parsed_types = Enum.map(types, &parse_type/1)
    quote do: Deft.Type.fixed_tuple(unquote(parsed_types))
  end

  # 2-tuple: {type1, type2}
  defp parse_type({type1, type2}) do
    quote do: Deft.Type.fixed_tuple([unquote(parse_type(type1)), unquote(parse_type(type2))])
  end

  # Union type: type1 | type2
  defp parse_type({:|, _, [left, right]}) do
    quote do: Deft.Type.union(unquote(parse_type(left)), unquote(parse_type(right)))
  end

  # Function type: (arg_types -> return_type)
  defp parse_type({:->, _, [args, return]}) do
    arg_types = Enum.map(List.wrap(args), &parse_type/1)
    return_type = parse_type(return)
    quote do: Deft.Type.fun(unquote(arg_types), unquote(return_type))
  end

  # Parenthesized type
  defp parse_type({:__block__, _, [inner]}) do
    parse_type(inner)
  end

  defp parse_type(other) do
    raise ArgumentError, "Unknown type syntax in sig: #{inspect(other)}"
  end

  # ============================================================================
  # Type Variable Collection
  # ============================================================================

  # Collects type variable names from parsed type AST.
  # Returns a list of unique type variable atoms in order of first appearance.
  defp collect_type_vars(type_asts) when is_list(type_asts) do
    type_asts
    |> Enum.flat_map(&extract_vars_from_ast/1)
    |> Enum.uniq()
  end

  # Extracts type variable names from a single type AST node.
  defp extract_vars_from_ast({:., _, [{:__aliases__, _, [:Deft, :Type]}, :var]}) do
    # This matches `Deft.Type.var(name)` - we need to look at the call
    []
  end

  defp extract_vars_from_ast({{:., _, [{:__aliases__, _, [:Deft, :Type]}, :var]}, _, [name]}) do
    [name]
  end

  defp extract_vars_from_ast({:__block__, _, [inner]}) do
    extract_vars_from_ast(inner)
  end

  defp extract_vars_from_ast({_, _, args}) when is_list(args) do
    Enum.flat_map(args, &extract_vars_from_ast/1)
  end

  defp extract_vars_from_ast(list) when is_list(list) do
    Enum.flat_map(list, &extract_vars_from_ast/1)
  end

  defp extract_vars_from_ast(_), do: []
end
