defmodule Deft.Type do
  alias Deft.Subtyping
  alias Deft.Type

  @types [
    Type.Atom,
    Type.Boolean,
    Type.Bottom,
    Type.FixedList,
    Type.FixedTuple,
    Type.Float,
    Type.Fn,
    Type.Integer,
    Type.Intersection,
    Type.List,
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

  def union(fst, snd) do
    cond do
      fst == snd ->
        fst

      Subtyping.subtype_of?(fst, snd) ->
        fst

      Subtyping.subtype_of?(snd, fst) ->
        snd

      true ->
        Type.Union.new(fst, snd)
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
