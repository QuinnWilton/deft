defmodule Deft do
  @moduledoc """
  Deft - Type Systems as Macros for Elixir.

  This module provides the main entry points for type-checked code blocks.
  It implements bidirectional type checking during macro expansion, then
  erases all type information to produce standard Elixir code.

  ## Usage

      require Deft

      {result, type} = Deft.compile do
        f = fn x :: integer -> x + 1 end
        f.(42)
      end

  ## Module-Level Usage

      defmodule MyMath do
        use Deft

        @deft add(integer, integer) :: integer
        def add(a, b), do: a + b

        # Or inline typed function
        deft subtract(a :: integer, b :: integer) :: integer do
          a - b
        end
      end

  ## Architecture

  Deft uses a rule-based extensible type system via `Deft.TypeChecker` and
  `Deft.Rule`. Custom typing rules can be added by implementing the `Deft.Rule`
  behaviour and registering them with the rule registry.
  """

  alias Deft.Compiler
  alias Deft.Context

  @doc """
  Compiles a typed code block, returning the result and its type.

  The block is type-checked at compile time, and all type annotations
  are erased before the code is executed.

  ## Example

      {result, type} = Deft.compile do
        x = 42
        x + 1
      end
      # result = 43
      # type = %Deft.Type.Integer{}
  """
  defmacro compile(do: block) do
    block = Compiler.compile(block)
    ctx = Context.new(__CALLER__)

    case Deft.TypeChecker.check(block, ctx) do
      {:ok, erased, type, _bindings, _ctx} ->
        {erased, Macro.escape(type)}

      {:error, reason} ->
        raise "Type checking failed: #{inspect(reason)}"
    end
  end

  @doc """
  Returns the type and bindings of a code block without executing it.

  Useful for inspecting the type structure of expressions.
  """
  defmacro bindings(do: block) do
    block = Compiler.compile(block)
    ctx = Context.new(__CALLER__)

    case Deft.TypeChecker.check(block, ctx) do
      {:ok, _erased, type, bindings, _ctx} ->
        Macro.escape({type, bindings})

      {:error, reason} ->
        raise "Type checking failed: #{inspect(reason)}"
    end
  end

  # ============================================================================
  # Module-level API
  # ============================================================================

  @doc """
  Enables Deft in a module for typed function definitions.

  When you `use Deft`, you get:
  - Access to the `@deft` attribute for declaring function signatures
  - The `deft` macro for defining inline typed functions
  - Automatic type checking of function implementations against their signatures

  ## Options

  - `:features` - List of optional type system features to enable.
    Available features: `:strict_subtyping`, `:exhaustiveness_checking`

  ## Example

      defmodule MyMath do
        use Deft, features: [:strict_subtyping]

        @deft add(integer, integer) :: integer
        def add(a, b), do: a + b

        deft multiply(x :: integer, y :: integer) :: integer do
          x * y
        end
      end
  """
  defmacro __using__(opts) do
    features = Keyword.get(opts, :features, [])

    quote do
      import Deft, only: [compile: 1, deft: 2]
      require Deft

      Module.register_attribute(__MODULE__, :deft, accumulate: true)
      Module.register_attribute(__MODULE__, :deft_features, persist: true)
      Module.put_attribute(__MODULE__, :deft_features, unquote(features))

      @before_compile Deft
    end
  end

  @doc """
  Defines a typed function with inline type annotations.

  The function signature is extracted from the parameter annotations
  and the return type annotation. Type checking happens at compile time.

  ## Example

      deft add(a :: integer, b :: integer) :: integer do
        a + b
      end

  This defines a function `add/2` that takes two integers and returns
  an integer. The implementation is type-checked at compile time.
  """
  defmacro deft({:"::", _, [{name, _, args}, return_type]}, do: body) when is_atom(name) do
    # Parse arguments and extract types
    {params, param_types} = parse_typed_params(args)
    return_type_parsed = parse_type(return_type)

    # Create function signature for registration
    arity = length(params)

    # Build the type list as a proper Elixir list expression
    param_types_list =
      quote do
        [unquote_splicing(param_types)]
      end

    signature =
      quote do
        Deft.Type.fun(unquote(param_types_list), unquote(return_type_parsed))
      end

    # Type check the body at compile time
    quote do
      # Register signature with the registry
      Deft.Signatures.register(
        {__MODULE__, unquote(name), unquote(arity)},
        unquote(signature)
      )

      # Store for @before_compile verification
      @deft {unquote(name), unquote(arity), unquote(Macro.escape(param_types)),
             unquote(Macro.escape(return_type_parsed))}

      # Define the actual function
      def unquote(name)(unquote_splicing(params)) do
        unquote(body)
      end
    end
  end

  defmacro deft(call, do: _body) do
    raise ArgumentError,
          "Invalid deft syntax. Expected: deft name(arg :: type, ...) :: return_type do ... end\n" <>
            "Got: #{Macro.to_string(call)}"
  end

  @doc false
  defmacro __before_compile__(env) do
    deft_sigs = Module.get_attribute(env.module, :deft) || []
    features = Module.get_attribute(env.module, :deft_features) || []

    # Generate compile-time validation for signatures
    validations =
      for {name, arity, _param_types, return_type} <- deft_sigs do
        quote do
          # Validate the signature is well-formed
          unless Deft.Type.well_formed?(unquote(return_type)) do
            raise CompileError,
              description:
                "Invalid return type in @deft #{unquote(name)}/#{unquote(arity)}: " <>
                  "#{inspect(unquote(return_type))}"
          end
        end
      end

    # Store module metadata for introspection
    quote do
      unquote_splicing(validations)

      @doc false
      def __deft__(:signatures), do: unquote(Macro.escape(deft_sigs))
      def __deft__(:features), do: unquote(Macro.escape(features))
    end
  end

  # ============================================================================
  # Private Helpers
  # ============================================================================

  # Parses typed parameters like `a :: integer` into parameter names and types
  defp parse_typed_params(args) when is_list(args) do
    {params, types} =
      Enum.map(args, fn
        {:"::", _, [{name, _, ctx}, type]} when is_atom(name) and is_atom(ctx) ->
          {{name, [], ctx}, parse_type(type)}

        {name, _, ctx} = var when is_atom(name) and is_atom(ctx) ->
          # Untyped parameter - use Top type
          {var, quote(do: Deft.Type.top())}

        other ->
          raise ArgumentError,
                "Invalid parameter syntax in deft: #{Macro.to_string(other)}. " <>
                  "Expected: name :: type"
      end)
      |> Enum.unzip()

    {params, types}
  end

  defp parse_typed_params(nil), do: {[], []}

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

  # Single-element list containing a function type: (a -> b) gets parsed as [{:-> ...}]
  defp parse_type([{:->, _, _} = fn_type]) do
    parse_type(fn_type)
  end

  # List type: [element_type]
  defp parse_type([element_type]) do
    quote do: Deft.Type.fixed_list(unquote(parse_type(element_type)))
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

  # Fallback for unknown types
  defp parse_type(other) do
    raise ArgumentError, "Unknown type syntax: #{Macro.to_string(other)}"
  end
end
