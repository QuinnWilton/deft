defmodule Deft do
  import Deft.Helpers

  alias Deft.Type

  defmacro deft(ast) do
    ast =
      ast
      |> Macro.postwalk(&handle_annotations/1)
      |> Macro.postwalk(&wrap_type_rule/1)
      |> IO.inspect(label: :wrapped)

    {e, t} = compute_and_erase_type(ast, __CALLER__)

    IO.inspect(t, label: :final_type)

    e
  end

  defmacro type_rule(e) do
    case e do
      {:fn, fn_meta, [:->, arrow_meta, [args, body]]} ->
        {body, output_type} = compute_and_erase_type_in_context(body, args, __CALLER__)
        {args, input_types} = compute_and_erase_types(args, __CALLER__)

        fn_type = Type.Fn.new(input_types, output_type)

        annotate({:fn, fn_meta, [{:->, arrow_meta, [args, body]}]}, fn_type)

      {{:., dot_meta, [e_fn]}, meta, args} ->
        {e_fn, t_fn} = compute_and_erase_type(e_fn, __CALLER__)
        {args, t_args} = compute_and_erase_types(args, __CALLER__)

        unless length(t_fn.inputs) == length(t_args) and subtypes_of?(t_fn.inputs, t_args) do
          raise Deft.TypecheckingError, expected: t_fn.inputs, actual: t_args
        end

        annotate({{:., dot_meta, [e_fn]}, meta, args}, t_fn.output)

      {:{}, tuple_meta, es} ->
        {es, e_ts} = compute_and_erase_types(es, __CALLER__)

        annotate({:{}, tuple_meta, es}, Type.Tuple.new(e_ts))
    end
  end

  def wrap_type_rule(e) do
    case e do
      {:fn, fn_meta, [{:->, arrow_meta, [args, body]}]} ->
        {:type_rule, [], [{:fn, fn_meta, [:->, arrow_meta, [args, body]]}]}

      {{:., dot_meta, [dot_args]}, meta, args} ->
        {:type_rule, [], [{{:., dot_meta, [dot_args]}, meta, args}]}

      {:{}, tuple_meta, es} ->
        {:type_rule, [], [{:{}, tuple_meta, es}]}

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

      [{:->, _, [inputs, output]}] ->
        inputs = Enum.map(inputs, &parse_type/1)
        output = parse_type(output)

        Type.Fn.new(inputs, output)
    end
  end
end
