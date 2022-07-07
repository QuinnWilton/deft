defmodule Deft.Generators.Code do
  import StreamData

  alias Deft.AST
  alias Deft.Generators
  alias Deft.Subtyping
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
    map({local_node(), Generators.Types.compound_type()}, fn {pattern, type} ->
      {AST.Annotation.new(pattern, type), type}
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
      arg_nodes = Enum.map(fn_type.inputs, &argument_node/1)

      map(fixed_list(arg_nodes), fn children ->
        {args, _} = Enum.unzip(children)

        {AST.FnApplication.new(fn_node, args), fn_type.output}
      end)
    end)
  end

  def fn_node(child_data \\ literal_node()) do
    bind(list_of(annotation_node()), fn arg_children ->
      {args, arg_types} = Enum.unzip(arg_children)

      map(child_data, fn term ->
        {body, body_type} = consume_exprs([term | arg_children])

        {AST.Fn.new(body, args), Type.fun(arg_types, body_type)}
      end)
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
      type = Type.fixed_list(Type.union(element_types))

      {AST.List.new(elements), type}
    end)
  end

  def argument_node(%Type.Union{} = type) do
    type
    |> Type.Union.types()
    |> Enum.random()
    |> argument_node()
  end

  def argument_node(type)
      when is_struct(type, Type.Atom)
      when is_struct(type, Type.Boolean)
      when is_struct(type, Type.Float)
      when is_struct(type, Type.Integer)
      when is_struct(type, Type.Number) do
    literal_node(type)
  end

  def argument_node(%Type.Fn{} = type) do
    bind(list_of(local_node(), length: length(type.inputs)), fn arg_names ->
      args =
        Enum.zip(arg_names, type.inputs)
        |> Enum.map(&AST.Annotation.new(elem(&1, 0), elem(&1, 1)))

      map(literal_node(type.output), fn {body, _} ->
        {AST.Fn.new(body, args), type}
      end)
    end)
  end

  def argument_node(%Type.FixedList{} = type) do
    type
    |> Type.FixedList.contents()
    |> argument_node()
    |> list_of()
    |> nonempty()
    |> map(fn children ->
      {elements, _} = Enum.unzip(children)
      node = AST.List.new(elements)

      {node, type}
    end)
  end

  def argument_node(%Type.FixedTuple{} = type) do
    type
    |> Type.FixedTuple.elements()
    |> Enum.map(&argument_node/1)
    |> fixed_list()
    |> map(fn children ->
      {elements, _} = Enum.unzip(children)
      node = AST.Tuple.new(elements)

      {node, type}
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
      {AST.Pair.new(fst, snd), Type.fixed_tuple([fst_type, snd_type])}
    end)
  end

  def tuple_node(child_data \\ literal_node()) do
    map(list_of(child_data), fn children ->
      {elements, element_types} = Enum.unzip(children)

      {AST.Tuple.new(elements), Type.fixed_tuple(element_types)}
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

  def consume_exprs(exprs) do
    # TODO: Slow.
    exprs = Enum.shuffle(exprs)

    if length(exprs) > 2 do
      {[fst, snd], exprs} = Enum.split(exprs, 2)
      expr = choose_fn(fst, snd)

      consume_exprs([expr | exprs])
    else
      term = Enum.random(exprs)

      choose_fn(term)
    end
  end

  def choose_fn({term, %Type.Boolean{}}) do
    node = AST.LocalCall.new(:not, [term])
    type = Type.boolean()

    {node, type}
  end

  def choose_fn({term, %Type.FixedTuple{}}) do
    node = AST.LocalCall.new(:tuple_size, [term])
    type = Type.integer()

    {node, type}
  end

  def choose_fn({term, %Type.FixedList{} = list_type}) do
    name =
      Enum.random([
        :length,
        :hd,
        :tl
      ])

    node = AST.LocalCall.new(name, [term])

    type =
      case name do
        :length ->
          Type.integer()

        :hd ->
          Type.FixedList.contents(list_type)

        :tl ->
          list_type
      end

    {node, type}
  end

  def choose_fn({term, type}) do
    if Enum.random([true, false]) do
      name =
        Enum.random([
          :is_atom,
          :is_boolean,
          :is_float,
          :is_function,
          :is_integer,
          :is_number,
          :is_tuple,
          :is_list
        ])

      node = AST.LocalCall.new(name, [term])
      type = Type.boolean()

      {node, type}
    else
      {name, meta, context} = Macro.unique_var(:x, __MODULE__)

      node =
        AST.FnApplication.new(
          AST.Fn.new(
            AST.Local.new(name, context, meta),
            [
              AST.Annotation.new(
                AST.Local.new(name, context, meta),
                type
              )
            ]
          ),
          [term]
        )

      {node, type}
    end
  end

  def choose_fn({fst, %Type.FixedTuple{} = tuple}, {snd, %Type.Integer{}}) do
    node = AST.LocalCall.new(:elem, [fst, snd])
    type = Type.union(Type.FixedTuple.elements(tuple))

    {node, type}
  end

  def choose_fn({fst, fst_type}, {snd, snd_type}) do
    cond do
      Subtyping.subtype_of?(Type.number(), fst_type) and
          Subtyping.subtype_of?(Type.number(), snd_type) ->
        name =
          Enum.random([
            :*,
            :+,
            :-
          ])

        node = AST.LocalCall.new(name, [fst, snd])

        type =
          cond do
            Subtyping.subtype_of?(fst_type, snd_type) ->
              fst_type

            Subtyping.subtype_of?(snd_type, fst_type) ->
              snd_type

            true ->
              Type.number()
          end

        {node, type}

      true ->
        name =
          Enum.random([
            :!=,
            :!==,
            :<,
            :<=,
            :==,
            :===,
            :>,
            :>=
          ])

        node = AST.LocalCall.new(name, [fst, snd])
        type = Type.boolean()

        {node, type}
    end
  end
end
