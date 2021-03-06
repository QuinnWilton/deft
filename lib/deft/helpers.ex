defmodule Deft.Helpers do
  alias Deft.AST
  alias Deft.Type
  alias Deft.TypeChecking

  defguard is_literal_type(term)
           when is_struct(term, Type.Atom) or
                  is_struct(term, Type.Boolean) or
                  is_struct(term, Type.Float) or
                  is_struct(term, Type.Integer) or
                  is_struct(term, Type.Number)

  def annotate_type(e, t) when is_list(e) do
    Keyword.put(e, :__deft_type__, t)
  end

  def annotate_type(e, t) do
    Macro.update_meta(e, &annotate_type(&1, t))
  end

  def annotate_bindings(e, bindings) when is_list(e) do
    e
    |> Keyword.put_new(:__deft_bindings__, [])
    |> Keyword.update!(:__deft_bindings__, &(&1 ++ bindings))
  end

  def annotate_bindings(e, bindings) do
    Macro.update_meta(e, &annotate_bindings(&1, bindings))
  end

  def delete_annotations(e) do
    e
    |> Macro.update_meta(&Keyword.delete(&1, :__deft_type__))
    |> Macro.update_meta(&Keyword.delete(&1, :__deft_bindings__))
  end

  def bindings_for({_, meta, _}) do
    Keyword.get(meta, :__deft_bindings__, [])
  end

  def bindings_for(_) do
    []
  end

  def type_of({_, meta, _}) do
    Keyword.get(meta, :__deft_type__)
  end

  def type_of({fst, snd}) do
    fst = type_of(fst)
    snd = type_of(snd)

    Type.fixed_tuple([fst, snd])
  end

  # is_boolean/1 must be checked before is_atom/1
  def type_of(e) when is_boolean(e), do: Type.boolean()
  def type_of(e) when is_atom(e), do: Type.atom()
  def type_of(e) when is_integer(e), do: Type.integer()
  def type_of(e) when is_float(e), do: Type.float()
  def type_of(e) when is_number(e), do: Type.number()

  def type_of(e) when is_list(e) do
    e
    |> types_of()
    |> Enum.reduce(Type.bottom(), &Type.union/2)
    |> Type.fixed_list()
  end

  def type_of(e) do
    raise Deft.MissingTypeError, expr: e
  end

  def types_of(es) do
    Enum.map(es, &type_of/1)
  end

  def compute_types(ast, env, opts) do
    if is_list(ast) do
      Enum.map(ast, &compute_types(&1, env, opts))
    else
      type =
        ast
        |> TypeChecking.type_check(env, opts)
        |> type_of()

      if callback = opts[:on_compute] do
        callback.(ast, type)
      end

      type
    end
  end

  def erase_types(ast, env, opts) do
    if is_list(ast) do
      Enum.map(ast, &erase_types(&1, env, opts))
    else
      ast
      |> TypeChecking.type_check(env, opts)
      |> delete_annotations()
    end
  end

  def compute_and_erase_types(ast, env, opts) do
    {ast, t, bindings} =
      if is_list(ast) do
        # TODO: Messy
        {nodes_types, bindings} =
          ast
          |> Enum.map(&compute_and_erase_types(&1, env, opts))
          |> Enum.map(fn {ast, type, bindings} -> {{ast, type}, bindings} end)
          |> Enum.unzip()

        {nodes, types} = Enum.unzip(nodes_types)
        bindings = Enum.reduce(bindings, [], &(&2 ++ &1))

        {nodes, types, bindings}
      else
        ast = TypeChecking.type_check(ast, env, opts)
        ast_erased = delete_annotations(ast)
        type = type_of(ast)

        if callback = opts[:on_compute] do
          callback.(ast, type)
        end

        {ast_erased, type, bindings_for(ast)}
      end

    {ast, t, bindings}
  end

  def inject_bindings(ast, bindings, _env, _opts) do
    Enum.reduce(bindings, ast, fn
      {:adt, %AST.Local{name: name, context: context}, %Type.ADT{} = type}, acc ->
        Deft.Macro.postwalk(acc, fn
          %Type.Alias{name: ^name, context: ^context} ->
            type

          acc ->
            acc
        end)

      {:adt_variant, name, %Type.ADT{} = type, %Type.Variant{} = variant}, acc ->
        Deft.Macro.postwalk(acc, fn
          %AST.LocalCall{name: ^name, args: args, meta: meta} ->
            AST.TypeConstructorCall.new(name, args, type, variant, meta)

          acc ->
            acc
        end)

      {%AST.Local{name: name, context: context} = x, t}, acc ->
        Deft.Macro.postwalk(acc, fn
          %AST.Local{name: ^name, context: ^context} = local ->
            # HACK: Encapsulate this in Local
            if Keyword.get(local.meta, :counter) == Keyword.get(x.meta, :counter) do
              meta = annotate_type(local.meta, t)

              %{local | meta: meta}
            else
              local
            end

          acc ->
            acc
        end)
    end)
  end

  def compute_and_erase_type_in_context(ast, bindings, env, opts) do
    ast = inject_bindings(ast, bindings, env, opts)

    compute_and_erase_types(ast, env, opts)
  end
end
