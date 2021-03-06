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

  def alias(name, context) do
    Type.Alias.new(name, context)
  end

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

  def adt(name, variants) do
    Type.ADT.new(name, variants)
  end

  def variant(name, adt_name, columns) do
    Type.Variant.new(name, adt_name, columns)
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

  def intersection(fst, snd) do
    cond do
      fst == snd ->
        fst

      Subtyping.subtype_of?(fst, snd) ->
        fst

      Subtyping.subtype_of?(snd, fst) ->
        snd

      true ->
        Type.Intersection.new(fst, snd)
    end
  end

  def well_formed?(type) when is_struct(type) do
    type.__struct__ in @types
  end

  def well_formed?(_) do
    false
  end
end
