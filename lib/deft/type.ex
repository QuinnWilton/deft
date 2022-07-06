defmodule Deft.Type do
  alias Deft.Type

  @types [
    Type.Atom,
    Type.Boolean,
    Type.Bottom,
    Type.Float,
    Type.Fn,
    Type.Integer,
    Type.List,
    Type.Number,
    Type.Number,
    Type.Top,
    Type.Tuple,
    Type.Union
  ]

  def atom() do
    Type.Atom.new()
  end

  def boolean() do
    Type.Boolean.new()
  end

  def bottom() do
    Type.Bottom.new()
  end

  def float() do
    Type.Float.new()
  end

  def fun(inputs, outputs) do
    Type.Fn.new(inputs, outputs)
  end

  def integer() do
    Type.Integer.new()
  end

  def list(contents) do
    Type.List.new(contents)
  end

  def number() do
    Type.Number.new()
  end

  def top() do
    Type.Top.new()
  end

  def tuple(elements) do
    Type.Tuple.new(elements)
  end

  def union(elements) do
    elements = MapSet.new(elements)

    # A union of one type is just that type
    if Enum.count(elements) == 1 do
      Enum.at(elements, 0)
    else
      Type.Union.new(elements)
    end
  end

  def well_formed?(type) when is_struct(type) do
    type.__struct__ in @types
  end

  def well_formed?(_) do
    false
  end
end
