defmodule Deft.PatternMatching do
  import Deft.Helpers

  alias Deft.AST
  alias Deft.Subtyping
  alias Deft.Type

  def handle_pattern(pattern, type, env, opts) do
    case do_handle_pattern(pattern, type, env, opts) do
      {:ok, result} ->
        result

      {:error, pattern_type} ->
        raise Deft.UnreachableBranchError, expected: type, actual: pattern_type
    end
  end

  defp do_handle_pattern(pattern, %Type.Union{} = type, env, opts) do
    fst = do_handle_pattern(pattern, type.fst, env, opts)
    snd = do_handle_pattern(pattern, type.snd, env, opts)

    case {fst, snd} do
      {{:error, fst_type}, {:error, snd_type}} ->
        pattern_type = Type.union(fst_type, snd_type)

        {:error, pattern_type}

      {{:ok, fst_result}, {:error, _}} ->
        {:ok, fst_result}

      {{:error, _}, {:ok, snd_result}} ->
        {:ok, snd_result}

      {{:ok, {erased, fst_type, fst_bindings}}, {:ok, {_, snd_type, snd_bindings}}} ->
        pattern_type = Type.union(fst_type, snd_type)
        bindings = fst_bindings ++ snd_bindings

        bindings =
          bindings
          |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
          |> Enum.map(fn {local, types} -> {local, Enum.reduce(types, &Type.union/2)} end)

        {:ok, {erased, pattern_type, bindings}}
    end
  end

  defp do_handle_pattern(%AST.Pair{} = pair, type, env, opts) do
    do_handle_pattern(AST.Tuple.new([pair.fst, pair.snd]), type, env, opts)
  end

  defp do_handle_pattern(%AST.Literal{} = literal, type, env, opts)
       when is_literal_type(type) do
    {pattern, pattern_type, pattern_bindings} =
      compute_and_erase_types(
        literal,
        env,
        opts
      )

    if Subtyping.subtype_of?(type, pattern_type) do
      {:ok, {pattern, pattern_type, pattern_bindings}}
    else
      {:error, pattern_type}
    end
  end

  defp do_handle_pattern(%AST.Local{} = local, type, env, opts) do
    local_bindings =
      if local.name == :_ do
        []
      else
        [{local, type}]
      end

    {pattern, pattern_type, pattern_bindings} =
      compute_and_erase_types(
        local,
        env,
        opts
      )

    # TODO: This seems a bit hacky
    pattern_type =
      if is_nil(pattern_type) do
        type
      else
        pattern_type
      end

    {:ok, {pattern, pattern_type, pattern_bindings ++ local_bindings}}
  end

  defp do_handle_pattern(%AST.Pin{} = pin, type, env, opts) do
    case do_handle_pattern(pin.expr, type, env, opts) do
      {:error, pattern_type} ->
        {:error, pattern_type}

      {:ok, {pattern, pattern_type, pattern_bindings}} ->
        # Is this correct? Should the local be an intersection of pattern_type and type?
        if Subtyping.subtype_of?(type, pattern_type) do
          {:ok, {{:^, pin.meta, [pattern]}, pattern_type, pattern_bindings}}
        else
          {:error, pattern_type}
        end
    end
  end

  defp do_handle_pattern(%AST.Match{} = match, type, env, opts) do
    case do_handle_pattern(match.value, type, env, opts) do
      {:error, pattern_type} ->
        {:error, pattern_type}

      {:ok, {value, value_type, value_bindings}} ->
        if Subtyping.subtype_of?(type, value_type) do
          {:ok, {pattern, pattern_type, pattern_bindings}} =
            do_handle_pattern(
              match.pattern,
              type,
              env,
              opts
            )

          pattern = {:=, match.meta, [pattern, value]}
          bindings = value_bindings ++ pattern_bindings

          {:ok, {pattern, pattern_type, bindings}}
        else
          {:error, value_type}
        end
    end
  end

  defp do_handle_pattern(%AST.Tuple{} = tuple, %Type.FixedTuple{} = type, env, opts) do
    {elements, types, inner_bindings} =
      Enum.zip(tuple.elements, Type.FixedTuple.elements(type))
      |> Enum.reduce({[], [], []}, fn {element, type}, {elements, types, inner_bindings} ->
        {:ok, {element, element_type, element_bindings}} =
          do_handle_pattern(element, type, env, opts)

        {elements ++ [element], types ++ [element_type], inner_bindings ++ element_bindings}
      end)

    tuple = {:{}, tuple.meta, elements}
    tuple_t = Type.fixed_tuple(types)

    bindings =
      inner_bindings
      |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
      |> Enum.map(fn {local, types} ->
        type = Enum.reduce(types, Type.bottom(), &Type.intersection/2)

        {local, type}
      end)

    {:ok, {tuple, tuple_t, bindings}}
  end

  defp do_handle_pattern(%AST.List{} = list, %Type.FixedList{} = type, env, opts) do
    # TODO: should each element in the pattern take on the type
    # at that position?
    {elements, element_types, inner_bindings} =
      Enum.reduce(list.elements, {[], [], []}, fn
        element, {elements, element_types, inner_bindings} ->
          {:ok, {element, element_t, element_bindings}} =
            do_handle_pattern(
              element,
              Type.FixedList.contents(type),
              env,
              opts
            )

          {
            elements ++ [element],
            element_types ++ [element_t],
            inner_bindings ++ element_bindings
          }
      end)

    elements_t =
      element_types
      |> Enum.reduce(Type.bottom(), &Type.union/2)
      |> Type.fixed_list()

    {:ok, {elements, elements_t, inner_bindings}}
  end

  defp do_handle_pattern(%AST.Cons{} = cons, type, env, opts) do
    {:ok, {head, _, head_bindings}} = do_handle_pattern(cons.head, type, env, opts)

    {:ok, {rest, _, rest_bindings}} =
      do_handle_pattern(cons.rest, Type.fixed_list(type), env, opts)

    cons = {:|, cons.meta, [head, rest]}

    {:ok, {cons, type, head_bindings ++ rest_bindings}}
  end

  defp do_handle_pattern(%AST.TypeConstructorCall{} = constructor, %Type.ADT{}, env, opts) do
    variant = constructor.variant

    {columns, types, inner_bindings} =
      Enum.zip(constructor.args, variant.columns)
      |> Enum.reduce({[], [], []}, fn
        {column, column_type}, {columns, types, inner_bindings} ->
          {:ok, {column, column_type, column_bindings}} =
            do_handle_pattern(column, column_type, env, opts)

          {columns ++ [column], types ++ [column_type], inner_bindings ++ column_bindings}
      end)

    if length(types) == length(variant.columns) and
         Subtyping.subtypes_of?(variant.columns, types) do
      columns =
        Enum.reduce(columns, [constructor.name], fn
          arg, acc ->
            acc ++ [arg]
        end)

      data = {:{}, constructor.meta, columns}
      data_t = variant

      bindings =
        inner_bindings
        |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
        |> Enum.map(fn {local, types} ->
          type = Enum.reduce(types, Type.bottom(), &Type.intersection/2)

          {local, type}
        end)

      {:ok, {data, data_t, bindings}}
    end
  end
end
