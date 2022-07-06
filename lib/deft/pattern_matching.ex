defmodule Deft.PatternMatching do
  import Deft.Helpers

  alias Deft.AST
  alias Deft.Subtyping
  alias Deft.Type

  def handle_pattern(pattern, value_t, env) do
    do_handle_pattern(
      pattern,
      value_t,
      env
    )
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
    # TODO: Handle conflicting rebinds:
    # {x, x} = {1, 2}
    local_bindings = [{local, value_t}]

    {local, local_t, inner_bindings} =
      compute_and_erase_type_in_context(
        local,
        local_bindings,
        env
      )

    {local, local_t, inner_bindings ++ local_bindings}
  end

  defp do_handle_pattern(%AST.Match{} = match, rhs_t, env) do
    {value, lhs_t, value_bindings} =
      do_handle_pattern(
        match.value,
        rhs_t,
        env
      )

    unless Subtyping.subtype_of?(rhs_t, lhs_t) do
      raise Deft.UnreachableBranchError, expected: rhs_t, actual: lhs_t
    end

    {pattern, _, pattern_bindings} =
      do_handle_pattern(
        match.pattern,
        lhs_t,
        env
      )

    match = {:=, match.meta, [pattern, value]}
    bindings = value_bindings ++ pattern_bindings

    {match, lhs_t, bindings}
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
    {head, head_t, head_bindings} = do_handle_pattern(cons.head, value_t, env)
    {rest, rest_t, rest_bindings} = do_handle_pattern(cons.rest, Type.list(value_t), env)

    cons = {:|, cons.meta, [head, rest]}
    cons_t = Type.list(Type.union([head_t, rest_t]))

    {cons, cons_t, head_bindings ++ rest_bindings}
  end
end