defmodule Deft.Type do
  alias Deft.Type

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
    Type.Union.new(elements)
  end

  def subtypes_of?(t1s, t2s) do
    ts = Enum.zip(t1s, t2s)

    Enum.all?(ts, fn {t1, t2} ->
      subtype_of?(t1, t2)
    end)
  end

  def subtype_of?(%Type.Top{}, _) do
    true
  end

  def subtype_of?(%Type.Number{}, t)
      when is_struct(t, Type.Number)
      when is_struct(t, Type.Integer)
      when is_struct(t, Type.Float) do
    true
  end

  def subtype_of?(t, t) do
    true
  end

  def subtype_of?(%Type.Fn{} = t1, %Type.Fn{} = t2) do
    inputs = Enum.zip(t1.inputs, t2.inputs)

    inputs_subtype? =
      Enum.all?(inputs, fn {ti1, ti2} ->
        Type.subtype_of?(ti2, ti1)
      end)

    output_subtype? = Type.subtype_of?(t1.output, t2.output)

    inputs_subtype? and output_subtype?
  end

  def subtype_of?(%Type.List{} = t1, %Type.List{} = t2) do
    Type.subtype_of?(Type.List.contents(t1), Type.List.contents(t2))
  end

  def subtype_of?(%Type.Tuple{} = t1, %Type.Tuple{} = t2) do
    elements = Enum.zip(t1.elements, t2.elements)

    Enum.all?(elements, fn {te1, te2} ->
      Type.subtype_of?(te1, te2)
    end)
  end

  def subtype_of?(t1, %Type.Union{} = t2) do
    t2
    |> Type.Union.types()
    |> Enum.all?(fn te2 ->
      Type.subtype_of?(t1, te2)
    end)
  end

  def subtype_of?(%Type.Union{} = t1, t2) do
    Enum.any?(t1.types, fn te1 ->
      Type.subtype_of?(te1, t2)
    end)
  end

  def subtype_of?(_, _) do
    false
  end
end
