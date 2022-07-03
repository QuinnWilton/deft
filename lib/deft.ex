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
      {:->, meta, [args, body]} ->
        {args, input_types} = compute_and_erase_types(args, __CALLER__)

        {body, output_type} =
          compute_and_erase_type_in_context(body, Enum.zip(args, input_types), __CALLER__)

        fn_type = Type.Fn.new(input_types, output_type)

        annotate({:->, meta, [args, body]}, fn_type)

      {:fn, meta, [arrow]} ->
        {arrow, arrow_type} = compute_and_erase_type(arrow, __CALLER__)

        annotate({:fn, meta, [arrow]}, arrow_type)

      {:., meta, [e]} ->
        {e, e_t} = compute_and_erase_type(e, __CALLER__)

        annotate({:., meta, [e]}, e_t)

      {{:type_rule, _, _} = e, meta, args} ->
        {e, e_t} = compute_and_erase_type(e, __CALLER__)

        case e do
          {:., dot_meta, [e_fn]} ->
            {args, args_t} = compute_and_erase_types(args, __CALLER__)

            unless length(e_t.inputs) == length(args_t) and subtypes_of?(e_t.inputs, args_t) do
              raise Deft.TypecheckingError, expected: e_t.inputs, actual: args_t
            end

            annotate({{:., dot_meta, [e_fn]}, meta, args}, e_t.output)
        end

      {:elem, meta, [tuple, index]} ->
        {tuple, tuple_t} = compute_and_erase_type(tuple, __CALLER__)
        {index, index_t} = compute_and_erase_type(index, __CALLER__)

        unless is_struct(tuple_t, Type.Tuple) do
          raise "Expected a tuple"
        end

        unless subtype_of?(Type.Integer.new(), index_t) do
          raise Deft.TypecheckingError, expected: Type.Integer.new(), actual: index_t
        end

        annotate({:elem, meta, [tuple, index]}, Type.Union.new(tuple_t.elements))

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

        unless subtype_of?(Type.Boolean.new(), predicate_t) do
          raise Deft.TypecheckingError, expected: Type.Boolean.new(), actual: predicate_t
        end

        {do_branch, do_branch_t} = compute_and_erase_type(do_branch, __CALLER__)
        {else_branch, else_branch_t} = compute_and_erase_type(else_branch, __CALLER__)

        type =
          cond do
            do_branch_t == else_branch_t ->
              do_branch_t

            subtype_of?(do_branch_t, else_branch_t) ->
              do_branch_t

            subtype_of?(else_branch_t, do_branch_t) ->
              else_branch_t

            :else ->
              Type.Union.new([do_branch_t, else_branch_t])
          end

        annotate({:if, meta, [predicate, [do: do_branch, else: else_branch]]}, type)

      {name, meta, context} ->
        {name, meta, context}
    end
  end

  def wrap_type_rule(e) do
    case e do
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
