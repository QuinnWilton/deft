defmodule Deft.TypeChecking do
  alias Deft.AST
  alias Deft.Guards
  alias Deft.Subtyping
  alias Deft.Type

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

  def local_expand(ast, env) do
    case ast do
      %AST.Annotation{} = annotation ->
        local = erase_types(annotation.name, env)

        annotate(local, annotation.type)

      %AST.Block{} = block ->
        {exprs, _ctx, type} =
          Enum.reduce(block.exprs, {[], [], nil}, fn
            # TODO: Only simple assignment so far, no pattern matching
            %AST.Match{} = match, {exprs, ctx, _} ->
              pattern = erase_types(match.pattern, env)

              {value, value_t} =
                compute_and_erase_type_in_context(
                  match.value,
                  ctx,
                  env
                )

              expr = {:=, match.meta, [pattern, value]}

              {exprs ++ [expr], ctx ++ [{pattern, value_t}], value_t}

            expr, {exprs, ctx, _} ->
              {expr, expr_t} = compute_and_erase_type_in_context(expr, ctx, env)

              {exprs ++ [expr], ctx, expr_t}
          end)

        annotate({:__block__, block.meta, exprs}, type)

      %AST.Fn{} = fun ->
        {args, input_types} = compute_and_erase_types(fun.args, env)

        {body, output_type} =
          compute_and_erase_type_in_context(fun.body, Enum.zip(args, input_types), env)

        type = Type.fun(input_types, output_type)

        annotate({:fn, fun.fn_meta, [{:->, fun.arrow_meta, [args, body]}]}, type)

      %AST.FnApplication{} = fn_application ->
        {fun, fun_t} = compute_and_erase_types(fn_application.fun, env)
        {args, args_t} = compute_and_erase_types(fn_application.args, env)

        unless length(fun_t.inputs) == length(args_t) and
                 Subtyping.subtypes_of?(fun_t.inputs, args_t) do
          raise Deft.TypecheckingError, expected: fun_t.inputs, actual: args_t
        end

        annotate(
          {{:., fn_application.fun_meta, [fun]}, fn_application.args_meta, args},
          fun_t.output
        )

      %AST.If{} = if_ast ->
        {predicate, predicate_t} = compute_and_erase_types(if_ast.predicate, env)
        {do_branch, do_branch_t} = compute_and_erase_types(if_ast.do, env)
        {else_branch, else_branch_t} = compute_and_erase_types(if_ast.else, env)

        unless Subtyping.subtype_of?(Type.boolean(), predicate_t) do
          raise Deft.TypecheckingError, expected: Type.boolean(), actual: predicate_t
        end

        type = Type.Union.new([do_branch_t, else_branch_t])

        annotate({:if, if_ast.meta, [predicate, [do: do_branch, else: else_branch]]}, type)

      %AST.Cond{} = cond_ast ->
        {branches, branch_ts} =
          Enum.map(cond_ast.branches, fn
            %AST.CondBranch{} = branch ->
              {predicate, predicate_t} = compute_and_erase_types(branch.predicate, env)
              {body, body_t} = compute_and_erase_types(branch.body, env)

              unless Subtyping.subtype_of?(Type.boolean(), predicate_t) do
                raise Deft.TypecheckingError, expected: Type.boolean(), actual: predicate_t
              end

              {{:->, branch.meta, [[predicate], body]}, body_t}
          end)
          |> Enum.unzip()

        type = Type.Union.new(branch_ts)

        annotate({:cond, cond_ast.meta, [[do: branches]]}, type)

      %AST.Case{} = case_ast ->
        {subject, subject_t} = compute_and_erase_types(case_ast.subject, env)

        {branches, branches_t} =
          Enum.map(case_ast.branches, fn
            # TODO: Pattern matching. Most uses of case will fail.
            %AST.CaseBranch{} = branch ->
              pattern = erase_types(branch.pattern, env)

              {body, body_t} =
                compute_and_erase_type_in_context(
                  branch.body,
                  [{pattern, subject_t}],
                  env
                )

              {{:->, branch.meta, [[pattern], body]}, body_t}
          end)
          |> Enum.unzip()

        type = Type.Union.new(branches_t)

        annotate({:case, case_ast.meta, [subject, [do: branches]]}, type)

      %AST.Tuple{} = tuple ->
        {elements, element_ts} = compute_and_erase_types(tuple.elements, env)

        annotate({:{}, tuple.meta, elements}, Type.tuple(element_ts))

      %AST.Pair{} = pair ->
        fst = local_expand(pair.fst, env)
        snd = local_expand(pair.snd, env)

        {fst, snd}

      %AST.List{} = list ->
        Enum.map(list.elements, &local_expand(&1, env))

      %AST.LocalCall{} = local_call ->
        if Enum.member?(@supported_guards, {local_call.name, length(local_call.args)}) do
          {args, t} = Guards.handle_guard(local_call.name, local_call.args, env)

          annotate({local_call.name, local_call.meta, args}, t)
        else
          # TODO: Maybe raise because of an unsupported function call?
          {local_call.name, local_call.meta, local_call.args}
        end

      %AST.Local{} = local ->
        {local.name, local.meta, local.context}

      literal ->
        if Macro.quoted_literal?(literal) do
          literal
        else
          raise "Unexpected AST node: #{inspect(literal)}"
        end
    end
  end
end
