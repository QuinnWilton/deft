defmodule Deft.Annotations do
  @moduledoc """
  Parses type annotations from Elixir AST into Deft types.

  This module converts type annotation syntax used in Deft code into
  internal `Deft.Type` structs. It's called by the compiler when processing
  type annotations on function arguments and ADT variant columns.

  ## Supported Annotation Syntax

  ### Primitive Types

  - `boolean` - Boolean type
  - `atom` - Atom type
  - `binary` - Binary/string type
  - `float` - Float type
  - `integer` - Integer type
  - `number` - Number type (supertype of integer and float)
  - `top` - Top type (any value)
  - `bottom` - Bottom type (no value)

  ### Composite Types

  - `{a, b}` - Two-element tuple
  - `{a, b, c, ...}` - N-element tuple (3+)
  - `a | b` - Union type
  - `[a]` or `list(a)` - List type

  ### Function Types

  - `(a -> b)` - Single-argument function
  - `(a, b -> c)` - Multi-argument function
  - `(-> a)` - Zero-argument function

  ### Type Aliases

  - `MyType` - References a user-defined type alias

  ## Examples

      # In function annotations
      fn (x :: integer) -> x * 2 end

      # In ADT definitions
      defdata(
        shape ::
          circle(float)
          | rectangle(float, float)
      )

  """

  alias Deft.Type
  alias Deft.TypeParser

  @doc """
  Parses an Elixir AST type annotation into a Deft type.

  ## Options

  - `:allow_variables` - When `true`, single lowercase letters are parsed as
    type variables. Default: `false`.

  Raises `CompileError` if the annotation is malformed or unrecognized.
  """
  @spec parse(term(), keyword()) :: Type.t()
  def parse(ast, opts \\ []) do
    TypeParser.parse!(ast, Keyword.put(opts, :output, :type))
  end
end
