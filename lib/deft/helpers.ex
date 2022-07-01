defmodule Deft.Helpers do
  def annotate(e, t) do
    Macro.update_meta(e, &Keyword.put(&1, :__deft_type__, t))
  end

  def delete_annotation(e) do
    Macro.update_meta(e, &Keyword.delete(&1, :__deft_type__))
  end

  def type_of({_, meta, _}) do
    case Keyword.fetch(meta, :__deft_type__) do
      :error ->
        nil

      {:ok, type} ->
        type
    end
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
    type = type_of(e)

    {e, type}
  end

  def local_expand(e, env) do
    Macro.expand_once(e, env)
  end
end
