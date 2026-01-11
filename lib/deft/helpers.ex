defmodule Deft.Helpers do
  @moduledoc """
  Helper functions for type checking, annotation, and type erasure.
  """

  alias Deft.AST
  alias Deft.Type
  alias Deft.Walker

  defguard is_literal_type(term)
           when is_struct(term, Type.Atom) or
                  is_struct(term, Type.Binary) or
                  is_struct(term, Type.Boolean) or
                  is_struct(term, Type.Float) or
                  is_struct(term, Type.Integer) or
                  is_struct(term, Type.Number)

  @doc """
  Annotates an AST node with a type in its metadata.
  """
  def annotate_type(e, t) when is_list(e) do
    Keyword.put(e, :__deft_type__, t)
  end

  def annotate_type(e, t) do
    Macro.update_meta(e, &annotate_type(&1, t))
  end

  @doc """
  Extracts the type from an AST node's metadata.
  """
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
  def type_of(e) when is_binary(e), do: Type.binary()
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
    Deft.Error.raise!(Deft.Error.missing_annotation(expression: e))
  end

  @doc """
  Extracts types from a list of AST nodes.
  """
  def types_of(es) do
    Enum.map(es, &type_of/1)
  end

  @doc """
  Injects variable bindings into an AST by annotating matching locals.

  This function walks the AST and applies bindings:
  - ADT bindings replace Type.Alias with the actual Type.ADT
  - ADT variant bindings convert LocalCall to TypeConstructorCall
  - Variable bindings annotate matching AST.Local nodes with their types
  """
  @spec inject_bindings(term(), [term()]) :: term()
  def inject_bindings(ast, bindings) do
    Enum.reduce(bindings, ast, fn
      {:adt, %AST.Local{name: name, context: context}, %Type.ADT{} = type}, acc ->
        Walker.postwalk(acc, fn
          %Type.Alias{name: ^name, context: ^context} ->
            type

          other ->
            other
        end)

      {:adt_variant, name, %Type.ADT{} = type, %Type.Variant{} = variant}, acc ->
        Walker.postwalk(acc, fn
          %AST.LocalCall{name: ^name, args: args, meta: meta} ->
            AST.TypeConstructorCall.new(name, args, type, variant, meta)

          other ->
            other
        end)

      {%AST.Local{name: name, context: context} = x, t}, acc ->
        Walker.postwalk(acc, fn
          %AST.Local{name: ^name, context: ^context} = local ->
            if Keyword.get(local.meta, :counter) == Keyword.get(x.meta, :counter) do
              meta = annotate_type(local.meta, t)
              %{local | meta: meta}
            else
              local
            end

          other ->
            other
        end)
    end)
  end
end
