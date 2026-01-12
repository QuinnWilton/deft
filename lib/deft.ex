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
  `Deft.Rules`. Custom typing rules can be added by implementing the `Deft.Rules`
  behaviour and registering them with the rule registry.
  """

  alias Deft.Compiler
  alias Deft.Context
  alias Deft.Error.Formatter

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
    source_lines = read_source_lines(__CALLER__.file)
    block = Compiler.compile(block)

    ctx =
      Context.new(__CALLER__,
        source_lines: source_lines,
        error_mode: :accumulate
      )

    # Initialize error accumulator.
    Process.put(:deft_accumulated_errors, [])

    result = Deft.TypeChecker.check(block, ctx)
    errors = Process.get(:deft_accumulated_errors, [])
    Process.delete(:deft_accumulated_errors)

    case {result, errors} do
      {{:ok, erased, type, _bindings, _ctx}, []} ->
        {erased, Macro.escape(type)}

      {_, errors} when errors != [] ->
        raise_type_errors(errors, source_lines)

      {{:error, reason}, _} ->
        raise CompileError, description: "Type checking failed: #{inspect(reason)}"
    end
  end

  @doc """
  Returns the type and bindings of a code block without executing it.

  Useful for inspecting the type structure of expressions.
  """
  defmacro bindings(do: block) do
    source_lines = read_source_lines(__CALLER__.file)
    block = Compiler.compile(block)

    ctx =
      Context.new(__CALLER__,
        source_lines: source_lines,
        error_mode: :accumulate
      )

    # Initialize error accumulator.
    Process.put(:deft_accumulated_errors, [])

    result = Deft.TypeChecker.check(block, ctx)
    errors = Process.get(:deft_accumulated_errors, [])
    Process.delete(:deft_accumulated_errors)

    case {result, errors} do
      {{:ok, _erased, type, bindings, _ctx}, []} ->
        Macro.escape({type, bindings})

      {_, errors} when errors != [] ->
        raise_type_errors(errors, source_lines)

      {{:error, reason}, _} ->
        raise CompileError, description: "Type checking failed: #{inspect(reason)}"
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
      import Deft, only: [compile: 1, deft: 2, defdata: 1]
      require Deft

      Module.register_attribute(__MODULE__, :deft, accumulate: true)
      Module.register_attribute(__MODULE__, :deft_definitions, accumulate: true)
      Module.register_attribute(__MODULE__, :deft_adts, accumulate: true)
      Module.register_attribute(__MODULE__, :deft_adt_names, accumulate: true)
      Module.register_attribute(__MODULE__, :deft_features, persist: true)
      Module.put_attribute(__MODULE__, :deft_features, unquote(features))

      @before_compile Deft
    end
  end

  @doc """
  Defines an algebraic data type (ADT) at the module level.

  The ADT is available throughout the module, in all `deft` function bodies.

  ## Example

      defmodule Shapes do
        use Deft

        defdata(
          shape ::
            rectangle(number, number)
            | square(number)
            | circle(number)
        )

        deft area(s :: shape) :: number do
          case s do
            rectangle(w, h) -> w * h
            square(side) -> side * side
            circle(r) -> 3.14159 * r * r
          end
        end
      end
  """
  defmacro defdata({:"::", _, [{name, _, _ctx}, _variants]} = type_def) do
    # Store the raw type definition for processing in __before_compile__
    quote do
      @deft_adts unquote(Macro.escape(type_def))

      # Also store the name for quick lookup
      @deft_adt_names unquote(name)
    end
  end

  @doc """
  Defines a typed function with inline type annotations.

  The function signature is extracted from the parameter annotations
  and the return type annotation. Type checking happens at compile time,
  after all function signatures in the module have been registered.

  ## Example

      deft add(a :: integer, b :: integer) :: integer do
        a + b
      end

  This defines a function `add/2` that takes two integers and returns
  an integer. The implementation is type-checked at compile time.
  """
  defmacro deft({:"::", _, [{name, meta, args}, return_type]}, do: body) when is_atom(name) do
    # Parse arguments and extract types
    {params, param_types} = parse_typed_params(args)
    return_type_parsed = parse_type(return_type)

    # Create function signature for registration
    arity = length(params)
    module = __CALLER__.module

    # Evaluate parameter types to get actual Type structs
    evaluated_param_types =
      Enum.map(param_types, fn type_ast ->
        {type, _} = Code.eval_quoted(type_ast, [], __CALLER__)
        type
      end)

    # Evaluate return type
    {evaluated_return_type, _} = Code.eval_quoted(return_type_parsed, [], __CALLER__)

    # Build the signature immediately and register it during macro expansion
    # This ensures all signatures are known before type checking begins
    signature = Deft.Type.fun(evaluated_param_types, evaluated_return_type)
    Deft.Signatures.register({module, name, arity}, signature)

    # Store definition for deferred type-checking in __before_compile__
    # We store: {name, arity, params, evaluated_param_types, evaluated_return_type, body, caller, meta}
    definition = {
      name,
      arity,
      params,
      evaluated_param_types,
      evaluated_return_type,
      body,
      Macro.escape(__CALLER__),
      meta
    }

    quote do
      @deft_definitions unquote(Macro.escape(definition))
    end
  end

  defmacro deft(call, do: _body) do
    raise ArgumentError,
          "Invalid deft syntax. Expected: deft name(arg :: type, ...) :: return_type do ... end\n" <>
            "Got: #{Macro.to_string(call)}"
  end

  @doc false
  defmacro __before_compile__(env) do
    definitions = Module.get_attribute(env.module, :deft_definitions) || []
    adts = Module.get_attribute(env.module, :deft_adts) || []
    features = Module.get_attribute(env.module, :deft_features) || []

    # Process ADT definitions to create bindings
    adt_bindings = process_adts(adts)

    # Read source once for all functions
    source_lines = read_source_lines(env.file)

    # Build a map from alias names to ADT types for resolution
    alias_map = build_alias_map(adt_bindings)

    # Type-check all function bodies and generate function definitions
    function_defs =
      Enum.map(definitions, fn {name, arity, params, param_types, _return_type, body, caller_escaped, _meta} ->
        caller = Code.eval_quoted(caller_escaped) |> elem(0)

        # Resolve alias types in parameter types
        resolved_param_types = Enum.map(param_types, &resolve_aliases(&1, alias_map))

        # Compile and type-check the body
        compiled_body = Compiler.compile(body)

        ctx =
          Context.new(caller,
            source_lines: source_lines,
            error_mode: :accumulate
          )

        # Add ADT bindings to context
        ctx = Context.bind_all(ctx, adt_bindings)

        # Build parameter bindings with resolved types
        param_bindings =
          Enum.zip(params, resolved_param_types)
          |> Enum.map(fn {{param_name, meta, param_context}, type} ->
            local = Deft.AST.Local.new(param_name, param_context, meta)
            {local, type}
          end)

        # Inject all bindings into the compiled body:
        # - ADT bindings resolve Type.Alias -> Type.ADT and LocalCall -> TypeConstructorCall
        # - Parameter bindings annotate locals with their types
        all_bindings = adt_bindings ++ param_bindings
        compiled_body = Deft.Helpers.inject_bindings(compiled_body, all_bindings)

        # Initialize error accumulator
        Process.put(:deft_accumulated_errors, [])

        # Type-check the body
        result = Deft.TypeChecker.check(compiled_body, ctx)
        errors = Process.get(:deft_accumulated_errors, [])
        Process.delete(:deft_accumulated_errors)

        erased_body =
          case {result, errors} do
            {{:ok, erased, _type, _bindings, _ctx}, []} ->
              erased

            {_, errors} when errors != [] ->
              raise_type_errors(errors, source_lines)

            {{:error, reason}, _} ->
              raise CompileError,
                description: "Type checking failed in deft #{name}/#{arity}: #{inspect(reason)}"
          end

        quote do
          def unquote(name)(unquote_splicing(params)) do
            unquote(erased_body)
          end
        end
      end)

    # Store module metadata for introspection
    sigs =
      Enum.map(definitions, fn {name, arity, _params, param_types, return_type, _body, _caller, _meta} ->
        {name, arity, param_types, return_type}
      end)

    quote do
      unquote_splicing(function_defs)

      @doc false
      def __deft__(:signatures), do: unquote(Macro.escape(sigs))
      def __deft__(:features), do: unquote(Macro.escape(features))
      def __deft__(:adts), do: unquote(Macro.escape(adt_bindings))
    end
  end

  # Process ADT definitions into bindings for type checking context
  defp process_adts(adts) do
    Enum.flat_map(adts, fn {:"::", _, [{name, _, _ctx}, variants]} ->
      # Create a fake AST.Local for the ADT name
      adt_name = Deft.AST.Local.new(name, nil, [])

      # Parse variants
      variant_types = parse_adt_variants(variants, adt_name)

      # Create the ADT type
      adt_type = Deft.Type.adt(adt_name, variant_types)

      # Return bindings for the ADT and all its variants
      adt_binding = {:adt, adt_name, adt_type}

      variant_bindings =
        Enum.map(variant_types, fn variant ->
          {:adt_variant, variant.name, adt_type, variant}
        end)

      [adt_binding | variant_bindings]
    end)
  end

  # Parse ADT variant definitions
  defp parse_adt_variants({:|, _, [first, rest]}, adt_name) do
    [parse_adt_variant(first, adt_name) | parse_adt_variants(rest, adt_name)]
  end

  defp parse_adt_variants(variant, adt_name) do
    [parse_adt_variant(variant, adt_name)]
  end

  defp parse_adt_variant({name, _meta, columns}, adt_name) when is_list(columns) do
    column_types = Enum.map(columns, &parse_type_to_struct/1)
    Deft.Type.variant(name, adt_name, column_types)
  end

  defp parse_adt_variant({name, _meta, nil}, adt_name) do
    Deft.Type.variant(name, adt_name, [])
  end

  # Parse type syntax directly to Type structs (for ADT processing at compile time)
  defp parse_type_to_struct({:integer, _, _}), do: Deft.Type.integer()
  defp parse_type_to_struct({:float, _, _}), do: Deft.Type.float()
  defp parse_type_to_struct({:number, _, _}), do: Deft.Type.number()
  defp parse_type_to_struct({:boolean, _, _}), do: Deft.Type.boolean()
  defp parse_type_to_struct({:atom, _, _}), do: Deft.Type.atom()
  defp parse_type_to_struct({:binary, _, _}), do: Deft.Type.binary()
  defp parse_type_to_struct({:list, _, [elem]}), do: Deft.Type.fixed_list(parse_type_to_struct(elem))
  defp parse_type_to_struct({:list, _, _}), do: Deft.Type.list()
  defp parse_type_to_struct({:tuple, _, _}), do: Deft.Type.tuple()
  defp parse_type_to_struct({:top, _, _}), do: Deft.Type.top()
  defp parse_type_to_struct({:bottom, _, _}), do: Deft.Type.bottom()

  defp parse_type_to_struct({:{}, _, types}) do
    Deft.Type.fixed_tuple(Enum.map(types, &parse_type_to_struct/1))
  end

  defp parse_type_to_struct({type1, type2}) do
    Deft.Type.fixed_tuple([parse_type_to_struct(type1), parse_type_to_struct(type2)])
  end

  defp parse_type_to_struct({:|, _, [left, right]}) do
    Deft.Type.union(parse_type_to_struct(left), parse_type_to_struct(right))
  end

  defp parse_type_to_struct({:->, _, [args, ret]}) do
    arg_types = Enum.map(List.wrap(args), &parse_type_to_struct/1)
    Deft.Type.fun(arg_types, parse_type_to_struct(ret))
  end

  defp parse_type_to_struct([{:->, _, _} = fn_type]), do: parse_type_to_struct(fn_type)
  defp parse_type_to_struct([elem]), do: Deft.Type.fixed_list(parse_type_to_struct(elem))

  defp parse_type_to_struct(other) do
    raise ArgumentError, "Unknown type syntax in defdata: #{Macro.to_string(other)}"
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
  defp parse_type({:binary, _, _}), do: quote(do: Deft.Type.binary())

  defp parse_type({:list, _, [elem_type]}) do
    quote do: Deft.Type.fixed_list(unquote(parse_type(elem_type)))
  end

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

  # ADT type reference (simple identifier like `shape`)
  defp parse_type({name, _, context}) when is_atom(name) and is_atom(context) do
    # This is a reference to a module-level ADT type
    quote do: Deft.Type.alias(unquote(name), nil)
  end

  # Fallback for unknown types
  defp parse_type(other) do
    raise ArgumentError, "Unknown type syntax: #{Macro.to_string(other)}"
  end

  # Build a map from alias names to ADT types for resolution
  defp build_alias_map(adt_bindings) do
    Enum.reduce(adt_bindings, %{}, fn
      {:adt, %Deft.AST.Local{name: name}, %Deft.Type.ADT{} = adt_type}, acc ->
        Map.put(acc, name, adt_type)

      _, acc ->
        acc
    end)
  end

  # Recursively resolve Type.Alias references to actual ADT types
  defp resolve_aliases(%Deft.Type.Alias{name: name}, alias_map) do
    case Map.get(alias_map, name) do
      nil -> raise CompileError, description: "Unknown type: #{name}"
      adt_type -> adt_type
    end
  end

  defp resolve_aliases(%Deft.Type.FixedTuple{elements: elements}, alias_map) do
    %Deft.Type.FixedTuple{elements: Enum.map(elements, &resolve_aliases(&1, alias_map))}
  end

  defp resolve_aliases(%Deft.Type.FixedList{contents: contents}, alias_map) do
    %Deft.Type.FixedList{contents: resolve_aliases(contents, alias_map)}
  end

  defp resolve_aliases(%Deft.Type.Fn{inputs: inputs, output: output}, alias_map) do
    %Deft.Type.Fn{
      inputs: Enum.map(inputs, &resolve_aliases(&1, alias_map)),
      output: resolve_aliases(output, alias_map)
    }
  end

  defp resolve_aliases(%Deft.Type.Union{fst: fst, snd: snd}, alias_map) do
    %Deft.Type.Union{
      fst: resolve_aliases(fst, alias_map),
      snd: resolve_aliases(snd, alias_map)
    }
  end

  defp resolve_aliases(%Deft.Type.Intersection{fst: fst, snd: snd}, alias_map) do
    %Deft.Type.Intersection{
      fst: resolve_aliases(fst, alias_map),
      snd: resolve_aliases(snd, alias_map)
    }
  end

  # Base types pass through unchanged
  defp resolve_aliases(type, _alias_map), do: type

  # Raises a compile error with a user-friendly stacktrace pointing to the error location.
  # This provides a much better developer experience than showing internal Deft stacktraces.
  defp raise_type_errors(errors, source_lines) do
    formatted = Formatter.format_all(errors, colors: true, source_lines: source_lines)
    exception = %CompileError{description: formatted}

    # Build a stacktrace pointing to the first error's location
    stacktrace =
      case errors do
        [%{location: {file, line, _col}} | _] when is_binary(file) and is_integer(line) ->
          [{:elixir_compiler, :__FILE__, 1, [file: String.to_charlist(file), line: line]}]

        _ ->
          []
      end

    :erlang.raise(:error, exception, stacktrace)
  end

  # Reads source lines from a file for error context display.
  defp read_source_lines(file) when is_binary(file) do
    case File.read(file) do
      {:ok, content} -> String.split(content, "\n")
      {:error, _} -> nil
    end
  end

  defp read_source_lines(_), do: nil
end
