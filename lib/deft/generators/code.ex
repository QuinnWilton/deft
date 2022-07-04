defmodule Deft.Generators.Code do
  import StreamData

  alias Deft.AST
  alias Deft.Generators
  alias Deft.Type

  def code() do
    # TODO: propagate bindings
    map(nonempty(list_of(expression())), fn children ->
      {exprs, expr_types} = Enum.unzip(children)

      {AST.Block.new(exprs), Enum.at(expr_types, -1)}
    end)
  end

  def expression() do
    tree(literal_node(), fn child_data ->
      one_of([
        case_node(child_data),
        cond_node(child_data),
        fn_application_node(child_data),
        fn_node(child_data),
        if_node(child_data),
        list_node(child_data),
        local_call_node(child_data),
        match_node(child_data),
        pair_node(child_data),
        tuple_node(child_data)
      ])
    end)
  end

  def pattern(_subject_type) do
    one_of([
      local_node()
    ])
  end

  def annotation_node() do
    # TODO: Handle compound types
    map({local_node(), Generators.primitive_type()}, fn {name, type} ->
      {AST.Annotation.new(name, type), type}
    end)
  end

  def case_branch_node(subject_type, child_data \\ literal_node()) do
    map({pattern(subject_type), child_data}, fn {pattern, {body, body_type}} ->
      {AST.CaseBranch.new(pattern, body), body_type}
    end)
  end

  def case_node(child_data \\ literal_node()) do
    bind(child_data, fn {subject, subject_type} ->
      map(nonempty(list_of(case_branch_node(subject_type, child_data))), fn children ->
        {branches, branch_types} = Enum.unzip(children)
        type = Type.union(branch_types)

        {AST.Case.new(subject, branches), type}
      end)
    end)
  end

  def cond_branch_node(child_data \\ literal_node()) do
    map({predicate(child_data), child_data}, fn {{predicate, _}, {body, body_type}} ->
      {AST.CondBranch.new(predicate, body), body_type}
    end)
  end

  def cond_node(child_data \\ literal_node()) do
    map(nonempty(list_of(cond_branch_node(child_data))), fn children ->
      {branches, branch_types} = Enum.unzip(children)
      type = Type.union(branch_types)

      {AST.Cond.new(branches), type}
    end)
  end

  def fn_application_node(child_data \\ literal_node()) do
    bind(fn_node(child_data), fn {fn_node, fn_type} ->
      # TODO: Generate non-literal arguments
      arg_nodes = Enum.map(fn_type.inputs, &literal_node/1)

      map(fixed_list(arg_nodes), fn children ->
        {args, _} = Enum.unzip(children)

        {AST.FnApplication.new(fn_node, args), fn_type.output}
      end)
    end)
  end

  def fn_node(child_data \\ literal_node()) do
    map({child_data, list_of(annotation_node())}, fn {{body, body_type}, arg_children} ->
      {args, arg_types} = Enum.unzip(arg_children)

      {AST.Fn.new(body, args), Type.fun(arg_types, body_type)}
    end)
  end

  def if_node(child_data \\ literal_node()) do
    # TODO: Limited predicate generation
    map({predicate(child_data), child_data, child_data}, fn
      {
        {predicate, _},
        {do_branch, do_type},
        {else_branch, else_type}
      } ->
        type =
          Type.union([
            do_type,
            else_type
          ])

        {AST.If.new(predicate, do_branch, else_branch), type}
    end)
  end

  def list_node(child_data \\ literal_node()) do
    map(list_of(child_data), fn children ->
      {elements, element_types} = Enum.unzip(children)
      type = Type.list(Type.union(element_types))

      {AST.List.new(elements), type}
    end)
  end

  def literal_node() do
    bind(Generators.primitive_type(), &literal_node/1)
  end

  def literal_node(%Type.Atom{} = type) do
    atom(:alphanumeric)
    |> filter(&(&1 not in [true, false]))
    |> map(fn value ->
      {AST.Literal.new(value), type}
    end)
  end

  def literal_node(%Type.Boolean{} = type) do
    map(boolean(), fn value ->
      {AST.Literal.new(value), type}
    end)
  end

  def literal_node(%Type.Float{} = type) do
    map(float(), fn value ->
      {AST.Literal.new(value), type}
    end)
  end

  def literal_node(%Type.Integer{} = type) do
    map(integer(), fn value ->
      {AST.Literal.new(value), type}
    end)
  end

  def literal_node(%Type.Number{} = type) do
    map(one_of([integer(), float()]), fn value ->
      {AST.Literal.new(value), type}
    end)
  end

  def local_call_node(child_data \\ literal_node()) do
    arguments =
      one_of([
        {child_data},
        {child_data, child_data}
      ])

    guards =
      one_of([
        constant(:is_atom),
        constant(:is_boolean),
        constant(:is_float),
        constant(:is_function),
        constant(:is_integer),
        constant(:is_number),
        constant(:is_tuple),
        constant(:is_list)
      ])

    comparisons =
      one_of([
        constant(:!=),
        constant(:!==),
        constant(:<),
        constant(:<=),
        constant(:==),
        constant(:===),
        constant(:>),
        constant(:>=)
      ])

    bind(arguments, fn
      # TODO: Generate more types of local calls
      {{fst, _}, {snd, _}} ->
        map(comparisons, fn comparison ->
          {AST.LocalCall.new(comparison, [fst, snd]), Type.boolean()}
        end)

      {{term, _}} ->
        map(guards, fn guard ->
          {AST.LocalCall.new(guard, [term]), Type.boolean()}
        end)
    end)
  end

  def local_node() do
    map(atom(:alphanumeric), fn name ->
      {name, meta, context} = Macro.unique_var(name, __MODULE__)

      AST.Local.new(name, context, meta)
    end)
  end

  def match_node(child_data \\ literal_node()) do
    bind(child_data, fn {value, type} ->
      map(pattern(type), fn pattern ->
        {AST.Match.new(
           pattern,
           value
         ), type}
      end)
    end)
  end

  def pair_node(child_data \\ literal_node()) do
    map({child_data, child_data}, fn {{fst, fst_type}, {snd, snd_type}} ->
      {AST.Pair.new(fst, snd), Type.tuple([fst_type, snd_type])}
    end)
  end

  def tuple_node(child_data \\ literal_node()) do
    map(list_of(child_data), fn children ->
      {elements, element_types} = Enum.unzip(children)

      {AST.Tuple.new(elements), Type.tuple(element_types)}
    end)
  end

  def predicate(child_data \\ literal_node()) do
    local_call_node(child_data)
  end

  def number() do
    one_of([
      integer(),
      float()
    ])
  end
end
