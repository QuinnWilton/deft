defmodule Deft.TypeChecking do
  alias Deft.AST
  alias Deft.Type
  alias Deft.TypeChecking

  @type_modules [
    Type.Atom,
    Type.Boolean,
    Type.Bottom,
    Type.Float,
    Type.Fn,
    Type.Integer,
    Type.List,
    Type.Number,
    Type.Number,
    Type.Top,
    Type.Tuple,
    Type.Union
  ]

  @supported_guards [
    !=: 2,
    !==: 2,
    *: 2,
    +: 1,
    +: 2,
    -: 1,
    -: 2,
    /: 2,
    <: 2,
    <=: 2,
    ==: 2,
    ===: 2,
    >: 2,
    >=: 2,
    abs: 1,
    # binary_part: 3,
    # bit_size: 1,
    # byte_size: 1,
    ceil: 1,
    div: 2,
    elem: 2,
    floor: 1,
    hd: 1,
    is_atom: 1,
    # is_binary: 1,
    # is_bitstring: 1,
    is_boolean: 1,
    is_float: 1,
    is_function: 1,
    is_function: 2,
    is_integer: 1,
    is_list: 1,
    # is_map: 1,
    # is_map_key: 2,
    is_number: 1,
    # is_pid: 1,
    # is_port: 1,
    # is_reference: 1,
    is_tuple: 1,
    length: 1,
    # map_size: 1,
    # node: 0,
    # node: 1,
    not: 1,
    rem: 2,
    round: 1,
    # self: 0,
    tl: 1,
    trunc: 1,
    tuple_size: 1
  ]

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
        unless type_well_formed?(type) do
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

  def type_well_formed?(t) when is_struct(t) do
    t.__struct__ in @type_modules
  end

  def type_well_formed?(_) do
    false
  end

  def types_of(es) do
    Enum.map(es, &type_of/1)
  end

  def compute_types(ast, env) do
    if is_list(ast) do
      Enum.map(ast, &compute_types(&1, env))
    else
      ast
      |> local_expand(env)
      |> type_of()
    end
  end

  def erase_types(ast, env) do
    if is_list(ast) do
      Enum.map(ast, &erase_types(&1, env))
    else
      ast
      |> local_expand(env)
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
        ast = local_expand(ast, env)
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

  def local_expand(%AST.Annotation{} = ast, env) do
    TypeChecking.Annotation.type_check(ast, env)
  end

  def local_expand(%AST.Block{} = ast, env) do
    TypeChecking.Block.type_check(ast, env)
  end

  def local_expand(%AST.Fn{} = ast, env) do
    TypeChecking.Fn.type_check(ast, env)
  end

  def local_expand(%AST.FnApplication{} = ast, env) do
    TypeChecking.FnApplication.type_check(ast, env)
  end

  def local_expand(%AST.If{} = ast, env) do
    TypeChecking.If.type_check(ast, env)
  end

  def local_expand(%AST.Cond{} = ast, env) do
    TypeChecking.Cond.type_check(ast, env)
  end

  def local_expand(%AST.Case{} = ast, env) do
    TypeChecking.Case.type_check(ast, env)
  end

  def local_expand(%AST.Tuple{} = ast, env) do
    TypeChecking.Tuple.type_check(ast, env)
  end

  def local_expand(%AST.Pair{} = ast, env) do
    fst = local_expand(ast.fst, env)
    snd = local_expand(ast.snd, env)

    {fst, snd}
  end

  def local_expand(%AST.List{} = ast, env) do
    Enum.map(ast.elements, &local_expand(&1, env))
  end

  def local_expand(%AST.LocalCall{} = ast, env) do
    if Enum.member?(@supported_guards, {ast.name, length(ast.args)}) do
      {args, t} = TypeChecking.Guards.handle_guard(ast.name, ast.args, env)

      annotate({ast.name, ast.meta, args}, t)
    else
      # TODO: Maybe raise because of an unsupported function call?
      {ast.name, ast.meta, ast.args}
    end
  end

  def local_expand(%AST.Local{} = ast, _env) do
    {ast.name, ast.meta, ast.context}
  end

  def local_expand(%AST.Literal{} = ast, _env) do
    ast.value
  end
end
