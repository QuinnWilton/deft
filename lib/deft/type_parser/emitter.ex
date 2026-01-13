defmodule Deft.TypeParser.Emitter do
  @moduledoc """
  Converts TypeParser.AST structs to final output formats.

  Supports two output modes:
  - Runtime types: `to_type/1` produces `Deft.Type.t()` structs
  - Quoted AST: `to_quoted/1` produces quoted Elixir AST for compile-time use
  """

  alias Deft.Type
  alias Deft.TypeParser.AST

  # ============================================================================
  # to_type/1 - Convert AST to runtime Type structs
  # ============================================================================

  @doc """
  Converts a TypeParser.AST struct to a runtime Deft.Type struct.
  """
  @spec to_type(AST.t()) :: Type.t()
  def to_type(%AST.Primitive{kind: :integer}), do: Type.integer()
  def to_type(%AST.Primitive{kind: :float}), do: Type.float()
  def to_type(%AST.Primitive{kind: :number}), do: Type.number()
  def to_type(%AST.Primitive{kind: :boolean}), do: Type.boolean()
  def to_type(%AST.Primitive{kind: :atom}), do: Type.atom()
  def to_type(%AST.Primitive{kind: :binary}), do: Type.binary()
  def to_type(%AST.Primitive{kind: :top}), do: Type.top()
  def to_type(%AST.Primitive{kind: :bottom}), do: Type.bottom()

  def to_type(%AST.Abstract{kind: :list}), do: Type.list()
  def to_type(%AST.Abstract{kind: :tuple}), do: Type.tuple()

  def to_type(%AST.Tuple{elements: elems}) do
    Type.fixed_tuple(Enum.map(elems, &to_type/1))
  end

  def to_type(%AST.Union{left: l, right: r}) do
    Type.union(to_type(l), to_type(r))
  end

  def to_type(%AST.List{element: elem}) do
    Type.fixed_list(to_type(elem))
  end

  def to_type(%AST.Function{inputs: inputs, output: output}) do
    Type.fun(Enum.map(inputs, &to_type/1), to_type(output))
  end

  def to_type(%AST.Variable{name: name}) do
    Type.var(name)
  end

  def to_type(%AST.Alias{name: name, context: context}) do
    Type.alias(name, context)
  end

  # ============================================================================
  # to_quoted/1 - Convert AST to quoted Elixir AST
  # ============================================================================

  @doc """
  Converts a TypeParser.AST struct to quoted Elixir AST.

  The quoted AST, when evaluated, produces Deft.Type structs.
  This is used by the Signatures DSL for compile-time type construction.
  """
  @spec to_quoted(AST.t()) :: Macro.t()
  def to_quoted(%AST.Primitive{kind: :integer}) do
    quote do: Deft.Type.integer()
  end

  def to_quoted(%AST.Primitive{kind: :float}) do
    quote do: Deft.Type.float()
  end

  def to_quoted(%AST.Primitive{kind: :number}) do
    quote do: Deft.Type.number()
  end

  def to_quoted(%AST.Primitive{kind: :boolean}) do
    quote do: Deft.Type.boolean()
  end

  def to_quoted(%AST.Primitive{kind: :atom}) do
    quote do: Deft.Type.atom()
  end

  def to_quoted(%AST.Primitive{kind: :binary}) do
    quote do: Deft.Type.binary()
  end

  def to_quoted(%AST.Primitive{kind: :top}) do
    quote do: Deft.Type.top()
  end

  def to_quoted(%AST.Primitive{kind: :bottom}) do
    quote do: Deft.Type.bottom()
  end

  def to_quoted(%AST.Abstract{kind: :list}) do
    quote do: Deft.Type.list()
  end

  def to_quoted(%AST.Abstract{kind: :tuple}) do
    quote do: Deft.Type.tuple()
  end

  def to_quoted(%AST.Tuple{elements: elems}) do
    quoted_elems = Enum.map(elems, &to_quoted/1)
    quote do: Deft.Type.fixed_tuple(unquote(quoted_elems))
  end

  def to_quoted(%AST.Union{left: l, right: r}) do
    quote do: Deft.Type.union(unquote(to_quoted(l)), unquote(to_quoted(r)))
  end

  def to_quoted(%AST.List{element: elem}) do
    quote do: Deft.Type.fixed_list(unquote(to_quoted(elem)))
  end

  def to_quoted(%AST.Function{inputs: inputs, output: output}) do
    quoted_inputs = Enum.map(inputs, &to_quoted/1)
    quote do: Deft.Type.fun(unquote(quoted_inputs), unquote(to_quoted(output)))
  end

  def to_quoted(%AST.Variable{name: name}) do
    quote do: Deft.Type.var(unquote(name))
  end

  def to_quoted(%AST.Alias{name: name, context: context}) do
    quote do: Deft.Type.alias(unquote(name), unquote(context))
  end

  # ============================================================================
  # collect_variables/1 - Extract type variable names
  # ============================================================================

  @doc """
  Collects all type variable names from a TypeParser.AST struct.

  Returns a list of unique variable names in order of first appearance.
  Used by the Signatures DSL to determine if a type needs Forall wrapping.
  """
  @spec collect_variables(AST.t()) :: [atom()]
  def collect_variables(ast) do
    ast
    |> do_collect_variables()
    |> Enum.uniq()
  end

  defp do_collect_variables(%AST.Variable{name: name}), do: [name]

  defp do_collect_variables(%AST.Tuple{elements: elems}) do
    Enum.flat_map(elems, &do_collect_variables/1)
  end

  defp do_collect_variables(%AST.Union{left: l, right: r}) do
    do_collect_variables(l) ++ do_collect_variables(r)
  end

  defp do_collect_variables(%AST.List{element: elem}) do
    do_collect_variables(elem)
  end

  defp do_collect_variables(%AST.Function{inputs: inputs, output: output}) do
    Enum.flat_map(inputs, &do_collect_variables/1) ++ do_collect_variables(output)
  end

  defp do_collect_variables(%AST.Primitive{}), do: []
  defp do_collect_variables(%AST.Abstract{}), do: []
  defp do_collect_variables(%AST.Alias{}), do: []
end
