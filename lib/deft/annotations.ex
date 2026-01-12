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

  alias Deft.Error
  alias Deft.Type

  @doc """
  Parses an Elixir AST type annotation into a Deft type.

  Raises `Deft.Error.Exception` if the annotation is malformed or unrecognized.
  """
  @spec parse(term()) :: Type.t()
  def parse({:boolean, _, _}) do
    Type.boolean()
  end

  def parse({:atom, _, _}) do
    Type.atom()
  end

  def parse({:binary, _, _}) do
    Type.binary()
  end

  def parse({:float, _, _}) do
    Type.float()
  end

  def parse({:integer, _, _}) do
    Type.integer()
  end

  def parse({:number, _, _}) do
    Type.number()
  end

  def parse({:top, _, _}) do
    Type.top()
  end

  def parse({:bottom, _, _}) do
    Type.bottom()
  end

  def parse({elem0, elem1}) do
    elem0 = parse(elem0)
    elem1 = parse(elem1)

    Type.fixed_tuple([elem0, elem1])
  end

  def parse({:{}, _, elements}) do
    elements = Enum.map(elements, &parse/1)

    Type.fixed_tuple(elements)
  end

  def parse({:|, _, [t1, t2]}) do
    t1 = parse(t1)
    t2 = parse(t2)

    Type.union(t1, t2)
  end

  def parse([{:->, _, [inputs, output]}]) do
    inputs = Enum.map(inputs, &parse/1)
    output = parse(output)

    Type.fun(inputs, output)
  end

  def parse([type]) do
    Type.fixed_list(parse(type))
  end

  def parse({:list, _, [elem_type]}) do
    Type.fixed_list(parse(elem_type))
  end

  def parse({name, _, ctx}) when is_atom(name) and is_atom(ctx) do
    Type.alias(name, ctx)
  end

  # Catch-all for unrecognized annotation patterns.
  def parse(ast) do
    error =
      Error.malformed_type(
        expression: ast,
        suggestions: [
          "Check the type annotation syntax",
          "Supported types: boolean, atom, binary, float, integer, number, top, bottom",
          "Supported composite types: {a, b}, a | b, [a], (a -> b)"
        ],
        notes: [malformed_type_note(ast)]
      )

    Error.raise!(error)
  end

  defp malformed_type_note(ast) when is_list(ast) and length(ast) == 0 do
    "Empty list [] is not a valid type annotation. Use [element_type] for list types."
  end

  defp malformed_type_note(ast) when is_list(ast) and length(ast) > 1 do
    "List with multiple elements is not a valid type annotation. " <>
      "Did you mean a tuple {a, b} or a union a | b?"
  end

  defp malformed_type_note(ast) when is_integer(ast) or is_float(ast) do
    "Literal numbers are not valid type annotations. Use 'integer', 'float', or 'number'."
  end

  defp malformed_type_note(ast) when is_binary(ast) do
    "Literal strings are not valid type annotations. Use 'binary' for string types."
  end

  defp malformed_type_note(_ast) do
    "This expression is not a recognized type annotation."
  end
end
