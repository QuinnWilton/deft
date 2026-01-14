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
        cons_node(child_data),
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

  @doc """
  Generates a pattern that can match values of the given type.

  Patterns are type-directed: the generated pattern is guaranteed to be
  structurally compatible with the given type.
  """
  def pattern(subject_type) do
    one_of(available_patterns(subject_type))
  end

  # Determine which pattern types are valid for a given subject type.
  # Returns generators that produce {pattern, pattern} tuples.
  defp available_patterns(subject_type) do
    base_patterns = [local_pattern()]

    type_specific_patterns =
      case subject_type do
        %Type.FixedTuple{} = tuple ->
          [tuple_pattern(tuple)]

        %Type.FixedList{} = list ->
          [list_pattern(list), cons_pattern(list)]

        %Type.Union{} ->
          # For unions, only local patterns are safe since a pattern
          # must be able to match any value of the union type.
          []

        primitive
        when is_struct(primitive, Type.Atom) or
               is_struct(primitive, Type.Boolean) or
               is_struct(primitive, Type.Integer) or
               is_struct(primitive, Type.Float) or
               is_struct(primitive, Type.Binary) ->
          [literal_pattern(primitive)]

        _ ->
          []
      end

    base_patterns ++ type_specific_patterns
  end

  @doc """
  Generates a local variable pattern (always valid for any type).
  """
  def local_pattern do
    map(local_node(), fn local ->
      {local, local}
    end)
  end

  @doc """
  Generates a literal pattern for primitive types.
  """
  def literal_pattern(%Type.Atom{}) do
    map(atom(:alphanumeric), fn value ->
      {AST.Literal.new(value), AST.Literal.new(value)}
    end)
    |> filter(fn {%AST.Literal{value: v}, _} -> v not in [true, false] end)
  end

  def literal_pattern(%Type.Boolean{}) do
    map(boolean(), fn value ->
      {AST.Literal.new(value), AST.Literal.new(value)}
    end)
  end

  def literal_pattern(%Type.Integer{}) do
    map(integer(), fn value ->
      {AST.Literal.new(value), AST.Literal.new(value)}
    end)
  end

  def literal_pattern(%Type.Float{}) do
    map(float(), fn value ->
      {AST.Literal.new(value), AST.Literal.new(value)}
    end)
  end

  def literal_pattern(%Type.Number{}) do
    map(one_of([integer(), float()]), fn value ->
      {AST.Literal.new(value), AST.Literal.new(value)}
    end)
  end

  def literal_pattern(%Type.Binary{}) do
    map(binary(), fn value ->
      {AST.Literal.new(value), AST.Literal.new(value)}
    end)
  end

  def literal_pattern(_) do
    # Fallback: generate a wildcard.
    local_pattern()
  end

  @doc """
  Generates a tuple pattern for FixedTuple types.
  """
  def tuple_pattern(%Type.FixedTuple{} = tuple) do
    element_types = Type.FixedTuple.elements(tuple)

    if element_types == [] do
      constant({AST.Tuple.new([]), AST.Tuple.new([])})
    else
      element_patterns = Enum.map(element_types, &pattern/1)

      map(fixed_list(element_patterns), fn patterns ->
        # patterns is a list of {pattern_ast, _} tuples
        elements = Enum.map(patterns, &elem(&1, 0))
        {AST.Tuple.new(elements), AST.Tuple.new(elements)}
      end)
    end
  end

  def tuple_pattern(_), do: local_pattern()

  @doc """
  Generates a list pattern for FixedList types.
  """
  def list_pattern(%Type.FixedList{} = list) do
    element_type = Type.FixedList.contents(list)

    # Generate 0-3 element patterns.
    bind(integer(0..3), fn count ->
      element_patterns = List.duplicate(pattern(element_type), count)

      map(fixed_list(element_patterns), fn patterns ->
        elements = Enum.map(patterns, &elem(&1, 0))
        {AST.List.new(elements), AST.List.new(elements)}
      end)
    end)
  end

  def list_pattern(_), do: local_pattern()

  @doc """
  Generates a cons pattern [head | tail] for FixedList types.

  Uses only local patterns for head and tail to ensure compatibility
  with the pattern matching module's type handling.
  """
  def cons_pattern(%Type.FixedList{}) do
    bind(local_pattern(), fn {head, _} ->
      map(local_pattern(), fn {tail, _} ->
        cons = AST.Cons.new(head, tail)
        {cons, cons}
      end)
    end)
  end

  def cons_pattern(_), do: local_pattern()

  def annotation_node() do
    map(tuple({local_node(), Generators.Types.compound_type()}), fn {pattern, type} ->
      {AST.Annotation.new(pattern, type), type}
    end)
  end

  def case_branch_node(subject_type, child_data \\ literal_node()) do
    map(tuple({pattern(subject_type), child_data}), fn {{pattern, _}, {body, body_type}} ->
      {AST.CaseBranch.new(pattern, body), body_type}
    end)
  end

  @doc """
  Generates a case expression with a catch-all branch for exhaustiveness.

  The catch-all branch ensures the case is exhaustive regardless of subject type.
  This is the correct approach for generated code - structurally exhaustive
  patterns (one branch per union member, ADT variant, etc.) would require
  significantly more complex generation logic.
  """
  def case_node(child_data \\ literal_node()) do
    bind(child_data, fn {subject, subject_type} ->
      map(list_of(case_branch_node(subject_type, child_data)), fn children ->
        {branches, branch_types} = Enum.unzip(children)

        # Always include a catch-all branch for exhaustiveness.
        {name, meta, context} = Macro.unique_var(:x, __MODULE__)
        local = AST.Local.new(name, context, meta)
        catch_all = AST.CaseBranch.new(local, local)

        branches = branches ++ [catch_all]
        branch_types = branch_types ++ [subject_type]
        type = Enum.reduce(branch_types, &Type.union/2)

        {AST.Case.new(subject, branches), type}
      end)
    end)
  end

  def cond_branch_node(child_data \\ literal_node()) do
    map(tuple({predicate(child_data), child_data}), fn {{predicate, _}, {body, body_type}} ->
      {AST.CondBranch.new(predicate, body), body_type}
    end)
  end

  def cond_node(child_data \\ literal_node()) do
    map(nonempty(list_of(cond_branch_node(child_data))), fn children ->
      {branches, branch_types} = Enum.unzip(children)
      type = Enum.reduce(branch_types, &Type.union/2)

      {AST.Cond.new(branches), type}
    end)
  end

  def fn_application_node(child_data \\ literal_node()) do
    bind(fn_node(child_data), fn {fn_node, fn_type} ->
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
    map(tuple({predicate(child_data), child_data, child_data}), fn
      {
        {predicate, _},
        {do_branch, do_type},
        {else_branch, else_type}
      } ->
        type =
          Type.union(
            do_type,
            else_type
          )

        {AST.If.new(predicate, do_branch, else_branch), type}
    end)
  end

  def list_node(child_data \\ literal_node()) do
    map(list_of(child_data), fn children ->
      {elements, element_types} = Enum.unzip(children)

      type =
        element_types
        |> Enum.reduce(Type.bottom(), &Type.union/2)
        |> Type.fixed_list()

      {AST.List.new(elements), type}
    end)
  end

  @doc """
  Generates a cons expression [head | tail].

  The tail must be a list expression to produce valid code.
  """
  def cons_node(child_data \\ literal_node()) do
    bind(child_data, fn {head, head_type} ->
      map(list_node(child_data), fn {tail, tail_type} ->
        # Result type is a list containing the union of head type and tail's element type.
        tail_elem_type = Type.FixedList.contents(tail_type)
        result_type = Type.fixed_list(Type.union(head_type, tail_elem_type))
        {AST.Cons.new(head, tail), result_type}
      end)
    end)
  end

  def argument_node(%Type.Union{} = type) do
    [type.fst, type.snd]
    |> Enum.random()
    |> argument_node()
  end

  def argument_node(type)
      when is_struct(type, Type.Atom)
      when is_struct(type, Type.Binary)
      when is_struct(type, Type.Boolean)
      when is_struct(type, Type.Float)
      when is_struct(type, Type.Integer)
      when is_struct(type, Type.Number) do
    literal_node(type)
  end

  # Intersection types: generate an argument that satisfies the intersection.
  # For simplicity, we pick the first component of the intersection.
  def argument_node(%Type.Intersection{fst: fst}) do
    argument_node(fst)
  end

  # Top type: can be satisfied by any value, so generate a literal.
  def argument_node(%Type.Top{}) do
    literal_node()
  end

  # Bottom type: uninhabited, no value exists.
  # Generate a placeholder that will cause a type error if used.
  # This shouldn't normally happen since we filter out Bottom from generators.
  def argument_node(%Type.Bottom{}) do
    map(literal_node(), fn {node, _} -> {node, Type.bottom()} end)
  end

  def argument_node(%Type.Fn{} = type) do
    bind(list_of(local_node(), length: length(type.inputs)), fn arg_names ->
      args =
        Enum.zip(arg_names, type.inputs)
        |> Enum.map(&AST.Annotation.new(elem(&1, 0), elem(&1, 1)))

      # Use argument_node for the body to handle nested Fn types.
      map(argument_node(type.output), fn {body, _} ->
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

  def literal_node(%Type.Number{}) do
    # Return the precise type (Integer or Float) based on the generated value,
    # since the type checker infers the concrete type for literals.
    map(one_of([integer(), float()]), fn value ->
      type = if is_integer(value), do: Type.integer(), else: Type.float()
      {AST.Literal.new(value), type}
    end)
  end

  def literal_node(%Type.Binary{} = type) do
    map(binary(), fn value ->
      {AST.Literal.new(value), type}
    end)
  end

  # Top type: generate any primitive literal, but return top as the type.
  def literal_node(%Type.Top{} = type) do
    map(one_of([atom(:alphanumeric), boolean(), integer(), float()]), fn value ->
      {AST.Literal.new(value), type}
    end)
  end

  def local_call_node(child_data \\ literal_node()) do
    arguments =
      one_of([
        tuple({child_data}),
        tuple({child_data, child_data})
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
      # Use only local patterns for match nodes to avoid mismatched
      # concrete types (e.g., float pattern against integer value).
      map(local_pattern(), fn {pattern, _} ->
        {AST.Match.new(
           pattern,
           value
         ), type}
      end)
    end)
  end

  def pair_node(child_data \\ literal_node()) do
    map(tuple({child_data, child_data}), fn {{fst, fst_type}, {snd, snd_type}} ->
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
    consume_exprs(Enum.shuffle(exprs), length(exprs))
  end

  defp consume_exprs(exprs, count) when count > 2 do
    [fst, snd | rest] = exprs
    expr = choose_fn(fst, snd)
    consume_exprs([expr | rest], count - 1)
  end

  defp consume_exprs(exprs, _count) do
    exprs |> Enum.random() |> choose_fn()
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

    type =
      tuple
      |> Type.FixedTuple.elements()
      |> Enum.reduce(Type.bottom(), &Type.union/2)

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
