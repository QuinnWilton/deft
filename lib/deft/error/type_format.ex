defmodule Deft.Error.TypeFormat do
  @moduledoc """
  Formats types for display in error messages.

  Provides human-readable string representations of Deft types
  for use in error messages and diagnostics.
  """

  alias Deft.Type

  @doc """
  Formats a type for display in error messages.

  Returns a human-readable string representation of the type.

  ## Examples

      iex> Deft.Error.TypeFormat.format(Type.integer())
      "integer"

      iex> Deft.Error.TypeFormat.format(Type.fun([Type.integer()], Type.boolean()))
      "(integer -> boolean)"

      iex> Deft.Error.TypeFormat.format(Type.fixed_tuple([Type.integer(), Type.boolean()]))
      "{integer, boolean}"
  """
  @spec format(Type.t() | nil) :: String.t()
  def format(nil), do: "unknown"
  def format(%Type.Top{}), do: "any"
  def format(%Type.Bottom{}), do: "never"
  def format(%Type.Integer{}), do: "integer"
  def format(%Type.Float{}), do: "float"
  def format(%Type.Number{}), do: "number"
  def format(%Type.Boolean{}), do: "boolean"
  def format(%Type.Atom{}), do: "atom"
  def format(%Type.Binary{}), do: "binary"

  def format(%Type.Fn{inputs: inputs, output: output}) do
    input_strs = Enum.map(inputs, &format/1)
    "(#{Enum.join(input_strs, ", ")} -> #{format(output)})"
  end

  def format(%Type.Union{fst: fst, snd: snd}) do
    "#{format(fst)} | #{format(snd)}"
  end

  def format(%Type.Intersection{fst: fst, snd: snd}) do
    "#{format(fst)} & #{format(snd)}"
  end

  def format(%Type.FixedTuple{elements: elements}) do
    elem_strs = Enum.map(elements, &format/1)
    "{#{Enum.join(elem_strs, ", ")}}"
  end

  def format(%Type.FixedList{contents: contents}) do
    "[#{format(contents)}]"
  end

  def format(%Type.List{}), do: "list"
  def format(%Type.Tuple{}), do: "tuple"

  def format(%Type.ADT{name: name}) do
    format_adt_name(name)
  end

  def format(%Type.Variant{name: name, columns: columns}) do
    col_strs = Enum.map(columns, &format/1)

    if columns == [] do
      "#{name}"
    else
      "#{name}(#{Enum.join(col_strs, ", ")})"
    end
  end

  def format(%Type.Var{name: name}) do
    "#{name}"
  end

  def format(%Type.Forall{vars: vars, body: body}) do
    vars_str = Enum.join(vars, ", ")
    "forall #{vars_str}. #{format(body)}"
  end

  def format(%Type.Alias{name: name, args: []}) do
    "#{name}"
  end

  def format(%Type.Alias{name: name, args: args}) do
    args_str = Enum.map_join(args, ", ", &format/1)
    "#{name}(#{args_str})"
  end

  def format(other), do: inspect(other)

  defp format_adt_name(%{name: name}), do: "#{name}"
  defp format_adt_name(name) when is_atom(name), do: "#{name}"
  defp format_adt_name(other), do: inspect(other)
end
