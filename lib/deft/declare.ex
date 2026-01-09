defmodule Deft.Declare do
  @moduledoc """
  Module for declaring types for external Elixir functions.

  This allows you to add type annotations to existing Elixir code
  without modifying the original modules. Declared signatures are
  registered with the Deft.Signatures registry and can be used
  during type checking.

  ## Usage

      defmodule MyApp.TypeDeclarations do
        use Deft.Declare

        # Declare types for Enum functions
        declare Enum.map([a], (a -> b)) :: [b]
        declare Enum.reduce([a], b, (a, b -> b)) :: b
        declare Enum.filter([a], (a -> boolean)) :: [a]

        # Declare types for String functions
        declare String.length(string) :: integer
        declare String.upcase(string) :: string

        # Declare types for custom modules
        declare MyApp.Math.add(integer, integer) :: integer
      end

  ## Notes

  - Type declarations are processed at compile time
  - Declarations are registered with Deft.Signatures
  - You can redeclare signatures (later declarations override earlier ones)
  - Type variables like `a` and `b` represent polymorphic types (future feature)
  """

  @doc """
  Enables the declare macro in the module.
  """
  defmacro __using__(_opts) do
    quote do
      import Deft.Declare, only: [declare: 1]
      require Deft.Declare
    end
  end

  @doc """
  Declares a type signature for an external function.

  ## Syntax

      declare Module.function(arg_type1, arg_type2, ...) :: return_type

  ## Examples

      # Simple function
      declare Enum.count([integer]) :: integer

      # Function with multiple arguments
      declare Enum.at([a], integer) :: a | nil

      # Higher-order function
      declare Enum.map([a], (a -> b)) :: [b]
  """
  defmacro declare({{:., _, [module, function]}, _, args} = _call) when is_atom(function) do
    {arg_types, return_type} = extract_signature(args)

    # Resolve the module at compile time
    resolved_module = resolve_module(module)

    # Build the type list properly
    arg_types_list = quote do: [unquote_splicing(arg_types)]

    quote do
      Deft.Signatures.register(
        {unquote(resolved_module), unquote(function), unquote(length(arg_types))},
        Deft.Type.fun(unquote(arg_types_list), unquote(return_type))
      )
    end
  end

  defmacro declare({:"::", _, [{{:., _, [module, function]}, _, args}, return_type]})
           when is_atom(function) do
    arg_types = Enum.map(args, &parse_type/1)
    return_type_parsed = parse_type(return_type)

    resolved_module = resolve_module(module)

    # Build the type list properly
    arg_types_list = quote do: [unquote_splicing(arg_types)]

    quote do
      Deft.Signatures.register(
        {unquote(resolved_module), unquote(function), unquote(length(arg_types))},
        Deft.Type.fun(unquote(arg_types_list), unquote(return_type_parsed))
      )
    end
  end

  defmacro declare(expr) do
    raise ArgumentError,
          "Invalid declare syntax. Expected: declare Module.function(types...) :: return_type\n" <>
            "Got: #{Macro.to_string(expr)}"
  end

  # ============================================================================
  # Private Helpers
  # ============================================================================

  # Extracts signature from function call syntax (handles both forms)
  defp extract_signature(args) do
    # Check if last arg is :: return type annotation
    case List.last(args) do
      {:"::", _, [last_arg, return_type]} ->
        arg_types = Enum.map(Enum.drop(args, -1), &parse_type/1) ++ [parse_type(last_arg)]
        {arg_types, parse_type(return_type)}

      _ ->
        # No return type - error
        raise ArgumentError, "declare requires a return type annotation with ::"
    end
  end

  # Resolves module aliases at compile time
  defp resolve_module({:__aliases__, _, parts}) do
    Module.concat(parts)
  end

  defp resolve_module(module) when is_atom(module), do: module

  # Parses type expressions into Deft.Type constructors
  defp parse_type({:integer, _, _}), do: quote(do: Deft.Type.integer())
  defp parse_type({:float, _, _}), do: quote(do: Deft.Type.float())
  defp parse_type({:number, _, _}), do: quote(do: Deft.Type.number())
  defp parse_type({:boolean, _, _}), do: quote(do: Deft.Type.boolean())
  defp parse_type({:atom, _, _}), do: quote(do: Deft.Type.atom())
  defp parse_type({:list, _, _}), do: quote(do: Deft.Type.list())
  defp parse_type({:tuple, _, _}), do: quote(do: Deft.Type.tuple())
  defp parse_type({:top, _, _}), do: quote(do: Deft.Type.top())
  defp parse_type({:bottom, _, _}), do: quote(do: Deft.Type.bottom())
  defp parse_type({:string, _, _}), do: quote(do: Deft.Type.atom())
  defp parse_type({nil, _, _}), do: quote(do: Deft.Type.atom())

  # Type variable (single lowercase letter) - treated as top for now
  # Future: implement proper polymorphism
  defp parse_type({name, _, ctx}) when is_atom(name) and is_atom(ctx) do
    name_str = Atom.to_string(name)

    if String.length(name_str) == 1 and name_str =~ ~r/^[a-z]$/ do
      # Type variable - use top type for now (polymorphism is Phase 5)
      quote(do: Deft.Type.top())
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
    raise ArgumentError, "Unknown type syntax in declare: #{inspect(other)}"
  end
end
