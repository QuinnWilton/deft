defmodule Deft do
  import Deft.Helpers

  alias Deft.Guards
  alias Deft.Type

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
    # hd: 1,
    is_atom: 1,
    # is_binary: 1,
    # is_bitstring: 1,
    is_boolean: 1,
    is_float: 1,
    is_function: 1,
    is_function: 2,
    is_integer: 1,
    # is_list: 1,
    # is_map: 1,
    # is_map_key: 2,
    is_number: 1,
    # is_pid: 1,
    # is_port: 1,
    # is_reference: 1,
    is_tuple: 1,
    # length: 1,
    # map_size: 1,
    # node: 0,
    # node: 1,
    not: 1,
    rem: 2,
    round: 1,
    # self: 0,
    # tl: 1,
    trunc: 1,
    tuple_size: 1
  ]

  defmacro compile(do: block) do
    block
    |> Macro.postwalk(&Deft.handle_annotations/1)
    |> Macro.postwalk(&Deft.wrap_type_rule/1)
    |> Deft.Helpers.compute_and_erase_type(__ENV__)
    |> elem(0)
  end

  defmacro type(do: block) do
    block
    |> Macro.postwalk(&Deft.handle_annotations/1)
    |> Macro.postwalk(&Deft.wrap_type_rule/1)
    |> Deft.Helpers.compute_and_erase_type(__ENV__)
    |> elem(1)
    |> Macro.escape()
  end

  defmacro type_rule(e) do
    case e do
      {:__block__, meta, exprs} ->
        {exprs, _ctx, block_t} =
          Enum.reduce(exprs, {[], [], nil}, fn
            # TODO: Only simple assignment so far, no pattern matching
            {:=, meta, [a, b]}, {exprs, ctx, _} ->
              a = erase_type(a, __CALLER__)
              {b, b_t} = compute_and_erase_type_in_context(b, ctx, __CALLER__)

              {exprs ++ [{:=, meta, [a, b]}], ctx ++ [{a, b_t}], b_t}

            expr, {exprs, ctx, _} ->
              {expr, expr_t} = compute_and_erase_type_in_context(expr, ctx, __CALLER__)

              {exprs ++ [expr], ctx, expr_t}
          end)

        annotate({:__block__, meta, exprs}, block_t)

      {:fn, fn_meta, [{:->, meta, [args, body]}]} ->
        {args, input_types} = compute_and_erase_types(args, __CALLER__)

        {body, output_type} =
          compute_and_erase_type_in_context(body, Enum.zip(args, input_types), __CALLER__)

        fn_type = Type.Fn.new(input_types, output_type)

        annotate({:fn, fn_meta, [{:->, meta, [args, body]}]}, fn_type)

      {{:., dot_meta, [e]}, meta, args} ->
        {e, e_t} = compute_and_erase_type(e, __CALLER__)

        {args, args_t} = compute_and_erase_types(args, __CALLER__)

        unless length(e_t.inputs) == length(args_t) and subtypes_of?(e_t.inputs, args_t) do
          raise Deft.TypecheckingError, expected: e_t.inputs, actual: args_t
        end

        annotate({{:., dot_meta, [e]}, meta, args}, e_t.output)

      {:{}, tuple_meta, es} ->
        {es, e_ts} = compute_and_erase_types(es, __CALLER__)

        annotate({:{}, tuple_meta, es}, Type.Tuple.new(e_ts))

      {:if, meta, [predicate, branches]} ->
        do_branch = branches[:do]
        else_branch = branches[:else]

        {predicate, predicate_t} = compute_and_erase_type(predicate, __CALLER__)
        {do_branch, do_branch_t} = compute_and_erase_type(do_branch, __CALLER__)
        {else_branch, else_branch_t} = compute_and_erase_type(else_branch, __CALLER__)

        unless subtype_of?(Type.Boolean.new(), predicate_t) do
          raise Deft.TypecheckingError, expected: Type.Boolean.new(), actual: predicate_t
        end

        type = Type.Union.new([do_branch_t, else_branch_t])

        annotate({:if, meta, [predicate, [do: do_branch, else: else_branch]]}, type)

      {:cond, meta, [[do: branches]]} ->
        {branches, branches_t} =
          Enum.map(branches, fn {:->, meta, [[predicate], body]} ->
            {predicate, predicate_t} = compute_and_erase_type(predicate, __CALLER__)
            {body, body_t} = compute_and_erase_type(body, __CALLER__)

            unless subtype_of?(Type.Boolean.new(), predicate_t) do
              raise Deft.TypecheckingError, expected: Type.Boolean.new(), actual: predicate_t
            end

            {{:->, meta, [[predicate], body]}, body_t}
          end)
          |> Enum.unzip()

        type = Type.Union.new(branches_t)

        annotate({:cond, meta, [[do: branches]]}, type)

      {name, meta, args} when is_list(args) ->
        if Enum.member?(@supported_guards, {name, length(args)}) do
          {args, t} = Guards.handle_guard(name, args, __CALLER__)

          annotate({name, meta, args}, t)
        else
          {name, meta, args}
        end

      {name, meta, args} ->
        {name, meta, args}
    end
  end

  def wrap_type_rule(e) do
    case e do
      {:=, meta, [left, right]} ->
        {:=, meta, [left, right]}

      {:->, meta, [args, body]} ->
        {:->, meta, [args, body]}

      {:fn, fn_meta, [arrow]} ->
        {:type_rule, [], [{:fn, fn_meta, [arrow]}]}

      {:., meta, args} ->
        {:., meta, args}

      {f, m, a} ->
        {:type_rule, [], [{f, m, a}]}

      e ->
        e
    end
  end

  def handle_annotations(e) do
    case e do
      {:"::", _, [e, t]} ->
        annotate(e, parse_type(t))

      e ->
        e
    end
  end

  def parse_type(t) do
    case t do
      {:boolean, _, _} ->
        Type.Boolean.new()

      {:atom, _, _} ->
        Type.Atom.new()

      {:float, _, _} ->
        Type.Float.new()

      {:integer, _, _} ->
        Type.Integer.new()

      {:number, _, _} ->
        Type.Number.new()

      {:top, _, _} ->
        Type.Top.new()

      {:bottom, _, _} ->
        Type.Bottom.new()

      {elem0, elem1} ->
        elem0 = parse_type(elem0)
        elem1 = parse_type(elem1)

        Type.Tuple.new([elem0, elem1])

      {:{}, _, elements} ->
        elements = Enum.map(elements, &parse_type/1)

        Type.Tuple.new(elements)

      {:|, _, [t1, t2]} ->
        t1 = parse_type(t1)
        t2 = parse_type(t2)

        [t1]
        |> Type.Union.new()
        |> Type.Union.put_type(t2)

      [{:->, _, [inputs, output]}] ->
        inputs = Enum.map(inputs, &parse_type/1)
        output = parse_type(output)

        Type.Fn.new(inputs, output)
    end
  end
end
