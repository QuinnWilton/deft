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
    end
  end

  def wrap_type_rule(e) do
    case e do
      {:fn, fn_meta, [{:->, arrow_meta, [args, body]}]} ->
        args = Enum.map(args, &parse_type_annotation/1)

        {:type_rule, [], [{:fn, fn_meta, [:->, arrow_meta, [args, body]]}]}

      e ->
        e
    end
  end

  def parse_type_annotation({:"::", _, [e, t]}) do
    annotate(e, t)
  end
end
