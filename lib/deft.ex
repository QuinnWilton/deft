defmodule Deft do
  import Deft.Helpers

  alias Deft.Type

  defmacro deft(ast) do
    ast = Macro.postwalk(ast, &wrap_type_rule/1)

    IO.inspect(ast, label: :rewritten_ast)

    {e, t} = compute_and_erase_type(ast, __CALLER__)

    IO.inspect(e, label: :final_ast)
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
    end
  end

  def wrap_type_rule(e) do
    case e do
      {:fn, fn_meta, [{:->, arrow_meta, [args, body]}]} ->
        args = Enum.map(args, &parse_type_annotation/1)

        {:type_rule, [], [{:fn, fn_meta, [:->, arrow_meta, [args, body]]}]}

      {{:., dot_meta, [dot_args]}, meta, args} ->
        {:type_rule, [], [{{:., dot_meta, [dot_args]}, meta, args}]}

      e ->
        e
    end
  end

  def parse_type_annotation({:"::", _, [e, t]}) do
    annotate(e, parse_type(t))
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

      [{:->, _, [inputs, output]}] ->
        inputs = Enum.map(inputs, &parse_type/1)
        output = parse_type(output)

        Type.Fn.new(inputs, output)
    end
  end
end
