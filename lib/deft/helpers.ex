defmodule Deft.Helpers do
  alias Deft.Type

  def annotate(e, t) do
    Macro.update_meta(e, &Keyword.put(&1, :__deft_type__, t))
  end

  def delete_annotation(e) do
    Macro.update_meta(e, &Keyword.delete(&1, :__deft_type__))
  end

  def type_of({_, meta, _} = e) do
    case Keyword.fetch(meta, :__deft_type__) do
      :error ->
        raise Deft.MissingTypeError, expr: e

      {:ok, type} ->
        unless type_well_formed?(type) do
          raise Deft.MalformedTypedError, expr: type
        end

        type
    end
  end

  def type_of({elem0, elem1}) do
    elem0 = type_of(elem0)
    elem1 = type_of(elem1)

    Type.Tuple.new([elem0, elem1])
  end

  # is_boolean/1 must be checked before is_atom/1
  def type_of(e) when is_boolean(e), do: Type.Boolean.new()
  def type_of(e) when is_atom(e), do: Type.Atom.new()
  def type_of(e) when is_integer(e), do: Type.Integer.new()
  def type_of(e) when is_float(e), do: Type.Float.new()
  def type_of(e) when is_number(e), do: Type.Number.new()

  def type_of(e) do
    raise Deft.MissingTypeError, expr: e
  end

  def type_well_formed?(t) when is_struct(t) do
    {:consolidated, impls} = Type.__protocol__(:impls)

    t.__struct__ in impls
  end

  def type_well_formed?(_) do
    false
  end

  def types_of(es) do
    Enum.map(es, &type_of/1)
  end

  def subtype_of?(t1, %Type.Union{} = t2) do
    Enum.all?(t2.elements, fn te2 ->
      Deft.Type.subtype_of?(t1, te2)
    end)
  end

  def subtype_of?(t1, t2) do
    Deft.Type.subtype_of?(t1, t2)
  end

  def subtypes_of?(t1s, t2s) do
    Enum.zip(t1s, t2s)
    |> Enum.all?(fn {t1, t2} -> subtype_of?(t1, t2) end)
  end

  def compute_type(e, env) do
    e
    |> local_expand(env)
    |> type_of()
  end

  def erase_type(e, env) do
    e
    |> local_expand(env)
    |> delete_annotation()
  end

  def compute_and_erase_type(e, env) do
    e = local_expand(e, env)
    erased = delete_annotation(e)
    type = type_of(e)

    {erased, type}
  end

  def compute_and_erase_types(es, env) do
    es
    |> Enum.map(&compute_and_erase_type(&1, env))
    |> Enum.unzip()
  end

  def compute_and_erase_type_in_context(e, context, env) do
    e =
      Enum.reduce(context, e, fn {{x, _, x_a}, t}, acc ->
        Macro.postwalk(acc, fn
          {^x, _, ^x_a} = a ->
            annotate(a, t)

          e ->
            e
        end)
      end)

    compute_and_erase_type(e, env)
  end

  def local_expand(e, env) do
    Macro.expand_once(e, env)
  end
end
