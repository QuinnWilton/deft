defmodule Deft.Helpers do
  alias Deft.AST
  alias Deft.Type
  alias Deft.TypeChecking

  def annotate(e, t) when is_list(e) do
    Keyword.put(e, :__deft_type__, t)
  end

  def annotate(e, t) do
    Macro.update_meta(e, &annotate(&1, t))
  end

  def delete_annotation(e) do
    Macro.update_meta(e, &Keyword.delete(&1, :__deft_type__))
  end

  def type_of({_, meta, _} = e) do
    case Keyword.fetch(meta, :__deft_type__) do
      :error ->
        raise Deft.MissingTypeError, expr: e

      {:ok, type} ->
        unless Type.well_formed?(type) do
          raise Deft.MalformedTypedError, expr: type
        end

        type
    end
  end

  def type_of({elem0, elem1}) do
    elem0 = type_of(elem0)
    elem1 = type_of(elem1)

    Type.tuple([elem0, elem1])
  end

  # is_boolean/1 must be checked before is_atom/1
  def type_of(e) when is_boolean(e), do: Type.boolean()
  def type_of(e) when is_atom(e), do: Type.atom()
  def type_of(e) when is_integer(e), do: Type.integer()
  def type_of(e) when is_float(e), do: Type.float()
  def type_of(e) when is_number(e), do: Type.number()

  def type_of(e) when is_list(e) do
    e_ts = types_of(e)

    Type.list(Type.union(e_ts))
  end

  def type_of(e) do
    raise Deft.MissingTypeError, expr: e
  end

  def types_of(es) do
    Enum.map(es, &type_of/1)
  end

  def compute_types(ast, env) do
    if is_list(ast) do
      Enum.map(ast, &compute_types(&1, env))
    else
      ast
      |> TypeChecking.type_check(env)
      |> type_of()
    end
  end

  def erase_types(ast, env) do
    if is_list(ast) do
      Enum.map(ast, &erase_types(&1, env))
    else
      ast
      |> TypeChecking.type_check(env)
      |> delete_annotation()
    end
  end

  def compute_and_erase_types(ast, env) do
    {ast, t} =
      if is_list(ast) do
        ast
        |> Enum.map(&compute_and_erase_types(&1, env))
        |> Enum.unzip()
      else
        ast = TypeChecking.type_check(ast, env)
        ast_erased = delete_annotation(ast)

        {ast_erased, type_of(ast)}
      end

    {ast, t}
  end

  def compute_and_erase_type_in_context(ast, context, env) do
    ast =
      Enum.reduce(context, ast, fn {{name, _, context}, t}, acc ->
        AST.postwalk(acc, fn
          %AST.Local{name: ^name, context: ^context} = local ->
            meta = annotate(local.meta, t)

            %{local | meta: meta}

          ast ->
            ast
        end)
      end)

    compute_and_erase_types(ast, env)
  end
end
