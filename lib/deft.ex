defmodule Deft do
  import Deft.Helpers

  alias Deft.Type

  defmacro compile(e) do
    e
    |> Macro.postwalk(&Deft.handle_annotations/1)
    |> Macro.postwalk(&Deft.wrap_type_rule/1)
    |> Deft.Helpers.compute_and_erase_type(__ENV__)
    |> elem(0)
  end

  defmacro type(e) do
    e
    |> Macro.postwalk(&Deft.handle_annotations/1)
    |> Macro.postwalk(&Deft.wrap_type_rule/1)
    |> Deft.Helpers.compute_and_erase_type(__ENV__)
    |> elem(1)
    |> Macro.escape()
  end

  defmacro type_rule(e) do
    case e do
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

      {:elem, meta, [tuple, index]} ->
        {tuple, tuple_t} = compute_and_erase_type(tuple, __CALLER__)
        {index, index_t} = compute_and_erase_type(index, __CALLER__)

        unless is_struct(tuple_t, Type.Tuple) do
          raise "Expected a tuple"
        end

        unless subtype_of?(Type.Integer.new(), index_t) do
          raise Deft.TypecheckingError, expected: Type.Integer.new(), actual: index_t
        end

        type =
          tuple_t
          |> Type.Tuple.unique_types()
          |> Type.Union.new()

        annotate({:elem, meta, [tuple, index]}, type)

      {:{}, tuple_meta, es} ->
        {es, e_ts} = compute_and_erase_types(es, __CALLER__)

        annotate({:{}, tuple_meta, es}, Type.Tuple.new(e_ts))

      # HACK? Macro.expand_once(quote do -1 end) expands to 1, but
      #       Macro.expand_once(quote do -1.0 end) does not
      {:-, meta, [e]} ->
        {e, e_t} = compute_and_erase_type(e, __CALLER__)

        annotate({:-, meta, [e]}, e_t)

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

      {name, meta, context} ->
        {name, meta, context}
    end
  end

  def wrap_type_rule(e) do
    case e do
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
