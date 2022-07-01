defmodule Deft.Helpers do
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
        type
    end
  end

  def types_of(es) do
    Enum.map(es, &type_of/1)
  end

  def subtype_of?(t1, t2) do
    Deft.Type.subtype_of?(t1, t2)
  end

  def subtypes?(t1s, t2s) do
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
      Enum.reduce(context, e, fn {x_f, x_m, x_a} = x, acc ->
        Macro.postwalk(acc, fn {a_f, a_m, a_a} = a ->
          # TODO: Is this necessary?
          x_counter = Keyword.take(x_m, [:counter])
          a_counter = Keyword.take(a_m, [:counter])

          if x_f == a_f and x_counter == a_counter and x_a == a_a do
            annotate(a, type_of(x))
          else
            a
          end
        end)
      end)

    compute_and_erase_type(e, env)
  end

  def local_expand(e, env) do
    Macro.expand_once(e, env)
  end
end
