defmodule Deft.Subtyping do
  alias Deft.Type

  def subtypes_of?(t1s, t2s) do
    ts = Enum.zip(t1s, t2s)

    Enum.all?(ts, fn {t1, t2} ->
      subtype_of?(t1, t2)
    end)
  end

  def subtype_of?(%Type.Top{}, _) do
    true
  end

  def subtype_of?(_, %Type.Bottom{}) do
    true
  end

  def subtype_of?(%Type.Tuple{}, %Type.FixedTuple{}) do
    true
  end

  def subtype_of?(%Type.List{}, %Type.FixedList{}) do
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
      length(t1.inputs) == length(t2.inputs) and
        Enum.all?(inputs, fn {ti1, ti2} ->
          subtype_of?(ti2, ti1)
        end)

    output_subtype? = subtype_of?(t1.output, t2.output)

    inputs_subtype? and output_subtype?
  end

  def subtype_of?(%Type.FixedList{} = t1, %Type.FixedList{} = t2) do
    subtype_of?(Type.FixedList.contents(t1), Type.FixedList.contents(t2))
  end

  def subtype_of?(%Type.FixedTuple{} = t1, %Type.FixedTuple{} = t2) do
    elements = Enum.zip(t1.elements, t2.elements)

    length(t1.elements) == length(t2.elements) and
      Enum.all?(elements, fn {te1, te2} ->
        subtype_of?(te1, te2)
      end)
  end

  def subtype_of?(t1, %Type.Union{} = t2) do
    t2
    |> Type.Union.types()
    |> Enum.all?(&subtype_of?(t1, &1))
  end

  def subtype_of?(%Type.Union{} = t1, t2) do
    t1
    |> Type.Union.types()
    |> Enum.any?(&subtype_of?(&1, t2))
  end

  def subtype_of?(t1, %Type.Intersection{} = t2) do
    t2
    |> Type.Intersection.types()
    |> Enum.any?(&subtype_of?(t1, &1))
  end

  def subtype_of?(%Type.Intersection{} = t1, t2) do
    t1
    |> Type.Intersection.types()
    |> Enum.all?(&subtype_of?(t2, &1))
  end

  def subtype_of?(_, _) do
    false
  end
end
