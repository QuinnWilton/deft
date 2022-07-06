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
    Type.FixedList,
    Type.Number,
    Type.Number,
    Type.Top,
    Type.Tuple,
    Type.FixedTuple,
    Type.Union,
    Type.Intersection
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

  def list() do
    Type.List.new()
  end

  def fixed_list(contents) do
    Type.FixedList.new(contents)
  end

  def number() do
    Type.Number.new()
  end

  def top() do
    Type.Top.new()
  end

  def tuple() do
    Type.Tuple.new()
  end

  def fixed_tuple(elements) do
    Type.FixedTuple.new(elements)
  end

  def union(elements) do
    union = Type.Union.new(elements)

    # A union of one type is just that type
    if Type.Union.size(union) == 1 do
      Enum.at(Type.Union.types(union), 0)
    else
      union
    end
  end

  def intersection(elements) do
    intersection = Type.Intersection.new(elements)

    # An intersection of one type is just that type
    if Type.Intersection.size(intersection) == 1 do
      Enum.at(Type.Intersection.types(intersection), 0)
    else
      intersection
    end
  end

  def well_formed?(type) when is_struct(type) do
    type.__struct__ in @types
  end

  def well_formed?(_) do
    false
  end
end
