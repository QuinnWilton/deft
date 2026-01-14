defmodule Deft.TypeParser do
  @moduledoc """
  Unified type annotation parser for Deft.

  This module provides a single source of truth for parsing type syntax,
  supporting both runtime type construction and compile-time quoted AST output.

  ## Usage

  ### Runtime Types (for Annotations)

      iex> TypeParser.parse({:integer, [], nil})
      {:ok, %Deft.Type.Integer{}}

      iex> TypeParser.parse!({:integer, [], nil})
      %Deft.Type.Integer{}

  ### Quoted AST (for Signatures)

      iex> TypeParser.parse({:integer, [], nil}, output: :quoted)
      {:ok, {:., [], [{:__aliases__, [], [:Deft, :Type]}, :integer]}}

  ### Type Variables

      iex> TypeParser.parse({:a, [], nil}, allow_variables: true)
      {:ok, %Deft.Type.Var{name: :a}}

  ## Output Modes

  - `:type` (default) - Returns `Deft.Type.t()` structs
  - `:quoted` - Returns quoted Elixir AST that constructs Type structs
  - `:ast` - Returns intermediate `TypeParser.AST` structs

  ## Options

  - `:output` - Output mode (`:type`, `:quoted`, or `:ast`)
  - `:allow_variables` - When `true`, single lowercase letters are type variables
  - `:file` - File path for error location reporting
  - `:env` - `Macro.Env` struct for extracting file context
  """

  alias Deft.Error
  alias Deft.TypeParser.{AST, Emitter, Parser}

  @type output_mode :: :type | :quoted | :ast

  @type parse_opts :: [
          output: output_mode(),
          allow_variables: boolean(),
          file: String.t() | nil,
          env: Macro.Env.t() | nil
        ]

  @doc """
  Parses an Elixir AST type expression.

  Returns `{:ok, result}` on success or `{:error, Error.t()}` on failure.
  The result type depends on the `:output` option.

  ## Examples

      iex> TypeParser.parse({:integer, [], nil})
      {:ok, %Deft.Type.Integer{}}

      iex> TypeParser.parse(42)
      {:error, %Deft.Error{code: :malformed_type}}
  """
  @spec parse(term(), parse_opts()) :: {:ok, term()} | {:error, Error.t()}
  def parse(ast, opts \\ []) do
    output = Keyword.get(opts, :output, :type)
    file = get_file(opts)

    parser_opts = [
      allow_variables: Keyword.get(opts, :allow_variables, false),
      file: file
    ]

    with {:ok, type_ast} <- Parser.parse(ast, parser_opts) do
      case output do
        :ast -> {:ok, type_ast}
        :type -> {:ok, Emitter.to_type(type_ast, file)}
        :quoted -> {:ok, Emitter.to_quoted(type_ast)}
      end
    end
  end

  @doc """
  Parses an Elixir AST type expression, raising on error.

  Returns the parsed result directly on success.
  Raises `CompileError` on failure.

  ## Examples

      iex> TypeParser.parse!({:integer, [], nil})
      %Deft.Type.Integer{}

      iex> TypeParser.parse!(42)
      ** (CompileError) Malformed type expression
  """
  @spec parse!(term(), parse_opts()) :: term()
  def parse!(ast, opts \\ []) do
    case parse(ast, opts) do
      {:ok, result} -> result
      {:error, error} -> Error.raise!(error)
    end
  end

  @doc """
  Parses to intermediate AST and collects type variables.

  This is a convenience function for the Signatures DSL that needs both
  the parsed AST and the list of type variables.

  Returns `{:ok, ast, variables}` or `{:error, Error.t()}`.
  """
  @spec parse_with_variables(term(), parse_opts()) ::
          {:ok, AST.t(), [atom()]} | {:error, Error.t()}
  def parse_with_variables(ast, opts \\ []) do
    opts = Keyword.put(opts, :output, :ast)

    case parse(ast, opts) do
      {:ok, type_ast} ->
        variables = Emitter.collect_variables(type_ast)
        {:ok, type_ast, variables}

      {:error, _} = error ->
        error
    end
  end

  # ============================================================================
  # Private Helpers
  # ============================================================================

  defp get_file(opts) do
    case Keyword.get(opts, :env) do
      %Macro.Env{file: file} -> file
      _ -> Keyword.get(opts, :file)
    end
  end
end
