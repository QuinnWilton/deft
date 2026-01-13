defmodule Deft.TypeParser.Parser do
  @moduledoc """
  Core parsing logic for type expressions.

  Transforms Elixir AST representing type syntax into `TypeParser.AST` structs,
  preserving span information for error reporting.
  """

  alias Deft.Error
  alias Deft.TypeParser.AST

  @type parse_opts :: [
          allow_variables: boolean(),
          file: String.t() | nil
        ]

  @doc """
  Parses an Elixir AST type expression into a TypeParser.AST struct.

  ## Options

  - `:allow_variables` - When `true`, single lowercase letters are parsed as
    type variables. When `false` (default), they are parsed as type aliases.
  - `:file` - File path for error location reporting.

  ## Returns

  - `{:ok, AST.t()}` on success
  - `{:error, Error.t()}` on failure
  """
  @spec parse(term(), parse_opts()) :: {:ok, AST.t()} | {:error, Error.t()}

  # Primitives
  def parse({:integer, meta, _}, _opts) do
    {:ok, %AST.Primitive{kind: :integer, span: extract_span(meta)}}
  end

  def parse({:float, meta, _}, _opts) do
    {:ok, %AST.Primitive{kind: :float, span: extract_span(meta)}}
  end

  def parse({:number, meta, _}, _opts) do
    {:ok, %AST.Primitive{kind: :number, span: extract_span(meta)}}
  end

  def parse({:boolean, meta, _}, _opts) do
    {:ok, %AST.Primitive{kind: :boolean, span: extract_span(meta)}}
  end

  def parse({:atom, meta, _}, _opts) do
    {:ok, %AST.Primitive{kind: :atom, span: extract_span(meta)}}
  end

  def parse({:binary, meta, _}, _opts) do
    {:ok, %AST.Primitive{kind: :binary, span: extract_span(meta)}}
  end

  # Alias: string -> binary
  def parse({:string, meta, _}, _opts) do
    {:ok, %AST.Primitive{kind: :binary, span: extract_span(meta)}}
  end

  def parse({:top, meta, _}, _opts) do
    {:ok, %AST.Primitive{kind: :top, span: extract_span(meta)}}
  end

  def parse({:bottom, meta, _}, _opts) do
    {:ok, %AST.Primitive{kind: :bottom, span: extract_span(meta)}}
  end

  # nil is treated as atom type
  def parse({nil, meta, _}, _opts) do
    {:ok, %AST.Primitive{kind: :atom, span: extract_span(meta)}}
  end

  # Abstract types (without element type)
  def parse({:list, meta, nil}, _opts) do
    {:ok, %AST.Abstract{kind: :list, span: extract_span(meta)}}
  end

  def parse({:list, meta, context}, _opts) when is_atom(context) do
    {:ok, %AST.Abstract{kind: :list, span: extract_span(meta)}}
  end

  # list(element_type) syntax
  def parse({:list, meta, [elem_type]}, opts) do
    with {:ok, elem} <- parse(elem_type, opts) do
      {:ok, %AST.List{element: elem, span: extract_span(meta)}}
    end
  end

  def parse({:tuple, meta, nil}, _opts) do
    {:ok, %AST.Abstract{kind: :tuple, span: extract_span(meta)}}
  end

  def parse({:tuple, meta, context}, _opts) when is_atom(context) do
    {:ok, %AST.Abstract{kind: :tuple, span: extract_span(meta)}}
  end

  # 2-tuple (pair syntax): {type1, type2}
  def parse({elem0, elem1}, opts) when not is_list(elem0) do
    with {:ok, e0} <- parse(elem0, opts),
         {:ok, e1} <- parse(elem1, opts) do
      {:ok, %AST.Tuple{elements: [e0, e1], span: nil}}
    end
  end

  # N-tuple (explicit syntax): {type1, type2, type3, ...}
  def parse({:{}, meta, elements}, opts) do
    with {:ok, elems} <- parse_all(elements, opts) do
      {:ok, %AST.Tuple{elements: elems, span: extract_span(meta)}}
    end
  end

  # Union: type1 | type2
  def parse({:|, meta, [left, right]}, opts) do
    with {:ok, l} <- parse(left, opts),
         {:ok, r} <- parse(right, opts) do
      {:ok, %AST.Union{left: l, right: r, span: extract_span(meta)}}
    end
  end

  # Function type wrapped in list: [(args -> return)]
  # This handles the common syntax (a -> b) which quotes as [{:-> ...}]
  def parse([{:->, _, _} = fn_type], opts) do
    parse(fn_type, opts)
  end

  # List type: [element_type]
  def parse([element], opts) do
    with {:ok, elem} <- parse(element, opts) do
      {:ok, %AST.List{element: elem, span: nil}}
    end
  end

  # Function type: args -> return
  def parse({:->, meta, [args, return]}, opts) do
    args = if is_list(args), do: args, else: [args]

    with {:ok, inputs} <- parse_all(args, opts),
         {:ok, output} <- parse(return, opts) do
      {:ok, %AST.Function{inputs: inputs, output: output, span: extract_span(meta)}}
    end
  end

  # Parenthesized expression: (__block__ [inner])
  def parse({:__block__, _, [inner]}, opts) do
    parse(inner, opts)
  end

  # Type variable or alias: name when is_atom(name)
  def parse({name, meta, context}, opts) when is_atom(name) and is_atom(context) do
    name_str = Atom.to_string(name)
    allow_vars = Keyword.get(opts, :allow_variables, false)

    cond do
      allow_vars and String.length(name_str) == 1 and name_str =~ ~r/^[a-z]$/ ->
        {:ok, %AST.Variable{name: name, span: extract_span(meta)}}

      true ->
        {:ok, %AST.Alias{name: name, context: context, span: extract_span(meta)}}
    end
  end

  # Empty list - error
  def parse([], opts) do
    {:error, build_error([], opts, "Empty list is not a valid type annotation.")}
  end

  # Multi-element list - error
  def parse(list, opts) when is_list(list) and length(list) > 1 do
    {:error,
     build_error(
       list,
       opts,
       "List with multiple elements is not a valid type annotation."
     )}
  end

  # Literal integer - error
  def parse(n, opts) when is_integer(n) do
    {:error,
     build_error(
       n,
       opts,
       "Literal numbers are not valid type annotations."
     )}
  end

  # Literal float - error
  def parse(n, opts) when is_float(n) do
    {:error,
     build_error(
       n,
       opts,
       "Literal numbers are not valid type annotations."
     )}
  end

  # Literal string - error
  def parse(s, opts) when is_binary(s) do
    {:error,
     build_error(
       s,
       opts,
       "Literal strings are not valid type annotations."
     )}
  end

  # Catch-all - error
  def parse(ast, opts) do
    {:error, build_error(ast, opts, "Unrecognized type expression.")}
  end

  # ============================================================================
  # Private Helpers
  # ============================================================================

  defp parse_all(asts, opts) do
    results =
      Enum.reduce_while(asts, {:ok, []}, fn ast, {:ok, acc} ->
        case parse(ast, opts) do
          {:ok, parsed} -> {:cont, {:ok, [parsed | acc]}}
          {:error, _} = err -> {:halt, err}
        end
      end)

    case results do
      {:ok, list} -> {:ok, Enum.reverse(list)}
      error -> error
    end
  end

  defp extract_span(meta) when is_list(meta) do
    line = Keyword.get(meta, :line)
    column = Keyword.get(meta, :column)
    if line, do: {line, column}, else: nil
  end

  defp extract_span(_), do: nil

  defp build_error(ast, opts, note) do
    file = Keyword.get(opts, :file)
    location = extract_location_from_ast(ast, file)

    Error.malformed_type(
      expression: ast,
      location: location,
      suggestions: suggest_fixes(ast),
      notes: [note]
    )
  end

  defp extract_location_from_ast({_, meta, _}, file) when is_list(meta) do
    line = Keyword.get(meta, :line)
    column = Keyword.get(meta, :column)

    cond do
      line -> {file, line, column}
      file -> {file, nil, nil}
      true -> nil
    end
  end

  defp extract_location_from_ast(_, file) when is_binary(file), do: {file, nil, nil}
  defp extract_location_from_ast(_, _), do: nil

  defp suggest_fixes([]) do
    ["Use `[element_type]` for list types."]
  end

  defp suggest_fixes(list) when is_list(list) and length(list) > 1 do
    ["Did you mean a tuple `{a, b}` or union `a | b`?"]
  end

  defp suggest_fixes(n) when is_integer(n) or is_float(n) do
    ["Use `integer`, `float`, or `number` instead."]
  end

  defp suggest_fixes(s) when is_binary(s) do
    ["Use `binary` for string types."]
  end

  defp suggest_fixes({name, _, _}) when is_atom(name) do
    known_types = ~w(integer float number boolean atom binary string top bottom list tuple)a

    case find_similar(name, known_types) do
      nil ->
        [
          "Check the type annotation syntax.",
          "Supported types: boolean, atom, binary, float, integer, number, top, bottom",
          "Supported composite types: {a, b}, a | b, [a], (a -> b)"
        ]

      similar ->
        ["Did you mean `#{similar}`?"]
    end
  end

  defp suggest_fixes(_) do
    [
      "Check the type annotation syntax.",
      "Supported types: boolean, atom, binary, float, integer, number, top, bottom",
      "Supported composite types: {a, b}, a | b, [a], (a -> b)"
    ]
  end

  defp find_similar(name, known_types) do
    name_str = Atom.to_string(name)

    Enum.find(known_types, fn known ->
      known_str = Atom.to_string(known)
      String.jaro_distance(name_str, known_str) > 0.8
    end)
  end
end
