defmodule Deft.PatternMatching do
  import Deft.Helpers

  alias Deft.AST
  alias Deft.Subtyping
  alias Deft.Type

  def handle_pattern(pattern, value_t, env) do
    {pattern, pattern_t, bindings} =
      do_handle_pattern(
        pattern,
        value_t,
        env
      )

    bindings =
      bindings
      |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
      |> Enum.map(fn {local, types} -> {local, Type.intersection(types)} end)

    {pattern, pattern_t, bindings}
  end

  defp do_handle_pattern(%AST.Literal{} = literal, value_t, env) do
    {literal, literal_t, inner_bindings} =
      compute_and_erase_types(
        literal,
        env
      )

    unless Subtyping.subtype_of?(value_t, literal_t) do
      raise Deft.UnreachableBranchError, expected: value_t, actual: literal_t
    end

    {literal, literal_t, inner_bindings}
  end

  defp do_handle_pattern(%AST.Local{} = local, value_t, env) do
    local_bindings =
      if local.name == :_ do
        []
      else
        [{local, value_t}]
      end

    {local, local_t, inner_bindings} =
      compute_and_erase_types(
        local,
        env
      )

    # TODO: This seems a bit hacky
    type =
      if is_nil(local_t) do
        value_t
      else
        local_t
      end

    {local, type, inner_bindings ++ local_bindings}
  end

  defp do_handle_pattern(%AST.Pin{} = pin, value_t, env) do
    {expr, expr_t, bindings} = handle_pattern(pin.expr, value_t, env)

    # Is this correct? Should the local be an intersection of expr_t and value_t?
    unless Subtyping.subtype_of?(value_t, expr_t) do
      raise Deft.TypecheckingError, expected: value_t, actual: expr_t
    end

    {{:^, pin.meta, [expr]}, expr_t, bindings}
  end

  defp do_handle_pattern(%AST.Match{} = match, type, env) do
    {value, value_t, value_bindings} =
      do_handle_pattern(
        match.value,
        type,
        env
      )

    unless Subtyping.subtype_of?(type, value_t) do
      raise Deft.UnreachableBranchError, expected: type, actual: value_t
    end

    {pattern, pattern_t, pattern_bindings} =
      do_handle_pattern(
        match.pattern,
        value_t,
        env
      )

    match = {:=, match.meta, [pattern, value]}
    bindings = value_bindings ++ pattern_bindings

    {match, pattern_t, bindings}
  end

  defp do_handle_pattern(%AST.Tuple{} = tuple, value_t, env) do
    # TODO: handle case where value_t isn't a tuple, but pattern is
    elements = tuple.elements
    element_types = Type.Tuple.elements(value_t)

    {elements, types, inner_bindings} =
      Enum.zip(elements, element_types)
      |> Enum.reduce({[], [], []}, fn {element, type}, {elements, types, inner_bindings} ->
        {element, element_type, element_bindings} = do_handle_pattern(element, type, env)

        {elements ++ [element], types ++ [element_type], inner_bindings ++ element_bindings}
      end)

    tuple = {:{}, tuple.meta, elements}
    tuple_t = Type.tuple(types)

    unless Subtyping.subtype_of?(value_t, tuple_t) do
      raise Deft.UnreachableBranchError, expected: value_t, actual: tuple_t
    end

    {tuple, tuple_t, inner_bindings}
  end

  defp do_handle_pattern(%AST.Pair{} = pair, value_t, env) do
    do_handle_pattern(AST.Tuple.new([pair.fst, pair.snd]), value_t, env)
  end

  defp do_handle_pattern(%AST.List{} = list, value_t, env) do
    # TODO: handle case where value_t isn't a list, but pattern is
    #
    # TODO: should each element in the pattern take on the type
    # at that position?
    contents_t = Type.List.contents(value_t)

    {elements, element_types, inner_bindings} =
      Enum.reduce(list.elements, {[], [], []}, fn
        element, {elements, element_types, inner_bindings} ->
          {element, element_t, element_bindings} =
            do_handle_pattern(
              element,
              contents_t,
              env
            )

          {
            elements ++ [element],
            element_types ++ [element_t],
            inner_bindings ++ element_bindings
          }
      end)

    elements_t = Type.list(Type.union(element_types))

    {elements, elements_t, inner_bindings}
  end

  defp do_handle_pattern(%AST.Cons{} = cons, value_t, env) do
    list_t = Type.list(value_t)

    {head, _, head_bindings} = do_handle_pattern(cons.head, value_t, env)
    {rest, _, rest_bindings} = do_handle_pattern(cons.rest, list_t, env)

    cons = {:|, cons.meta, [head, rest]}

    {cons, value_t, head_bindings ++ rest_bindings}
  end
end
