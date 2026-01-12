defmodule Deft.PatternMatching do
  @moduledoc """
  Bidirectional pattern matching for type checking.

  Pattern matching is a key part of bidirectional type checking where we
  check patterns against an expected type and produce variable bindings.
  """

  alias Deft.AST
  alias Deft.Context
  alias Deft.Error
  alias Deft.Helpers
  alias Deft.Span
  alias Deft.Subtyping
  alias Deft.Type

  import Helpers, only: [is_literal_type: 1]

  @type binding :: {AST.Local.t(), Type.t()}
  @type result :: {erased :: term(), pattern_type :: Type.t(), bindings :: [binding()]}

  @doc """
  Handles pattern matching against a type, returning erased pattern, type, and bindings.

  Raises `CompileError` if the pattern cannot match the given type.

  ## Options

  - `:branch_meta` - Metadata from the case branch, used as fallback location for literals
  - `:subject` - The case subject AST, for extracting location in error messages
  - `:subject_type` - The type of the case subject, for better error messages
  """
  @spec handle_pattern(term(), Type.t(), Context.t(), keyword()) :: result()
  def handle_pattern(pattern, type, %Context{} = ctx, opts \\ []) do
    case do_handle_pattern(pattern, type, ctx) do
      {:ok, result} ->
        result

      {:error, pattern_type} ->
        branch_meta = Keyword.get(opts, :branch_meta)
        subject = Keyword.get(opts, :subject)
        subject_type = Keyword.get(opts, :subject_type, type)

        pattern_location = Span.extract(pattern) || Span.extract(branch_meta)
        subject_location = if subject, do: Span.extract(subject), else: nil

        # Build spans for multi-span display
        # Subject is context (secondary), pattern is the error (primary)
        spans =
          Span.filter([
            Span.secondary(subject_location, "subject has type", subject_type),
            Span.primary(pattern_location, "pattern matches", pattern_type)
          ])

        Error.raise!(
          Error.unreachable_branch(
            subject_type: subject_type,
            pattern_type: pattern_type,
            location: pattern_location,
            expression: pattern,
            spans: spans
          ),
          ctx
        )
    end
  end

  # Union types: try both branches
  defp do_handle_pattern(pattern, %Type.Union{} = type, ctx) do
    fst = do_handle_pattern(pattern, type.fst, ctx)
    snd = do_handle_pattern(pattern, type.snd, ctx)

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

        # Merge duplicate bindings with union types
        bindings =
          bindings
          |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
          |> Enum.map(fn {local, types} -> {local, Enum.reduce(types, &Type.union/2)} end)

        {:ok, {erased, pattern_type, bindings}}
    end
  end

  # Pair patterns: convert to tuple
  defp do_handle_pattern(%AST.Pair{} = pair, type, ctx) do
    do_handle_pattern(AST.Tuple.new([pair.fst, pair.snd]), type, ctx)
  end

  # Literal patterns: check against literal types
  defp do_handle_pattern(%AST.Literal{value: value}, type, _ctx)
       when is_literal_type(type) do
    pattern_type = literal_type(value)

    if Subtyping.subtype_of?(type, pattern_type) do
      {:ok, {value, pattern_type, []}}
    else
      {:error, pattern_type}
    end
  end

  # Literal patterns against Top: always match, infer type from literal
  defp do_handle_pattern(%AST.Literal{value: value}, %Type.Top{}, _ctx) do
    pattern_type = literal_type(value)
    {:ok, {value, pattern_type, []}}
  end

  # Local (variable) patterns: bind the variable to the type
  defp do_handle_pattern(%AST.Local{} = local, type, _ctx) do
    erased = {local.name, local.meta, local.context}

    bindings =
      if local.name == :_ do
        []
      else
        [{local, type}]
      end

    # Check if the local already has a type annotation
    pattern_type =
      case Keyword.get(local.meta, :__deft_type__) do
        nil -> type
        existing_type -> existing_type
      end

    {:ok, {erased, pattern_type, bindings}}
  end

  # Pin patterns: force value comparison
  defp do_handle_pattern(%AST.Pin{} = pin, type, ctx) do
    case do_handle_pattern(pin.expr, type, ctx) do
      {:error, pattern_type} ->
        {:error, pattern_type}

      {:ok, {pattern, pattern_type, pattern_bindings}} ->
        if Subtyping.subtype_of?(type, pattern_type) do
          {:ok, {{:^, pin.meta, [pattern]}, pattern_type, pattern_bindings}}
        else
          {:error, pattern_type}
        end
    end
  end

  # Nested match patterns (x = value)
  defp do_handle_pattern(%AST.Match{} = match, type, ctx) do
    case do_handle_pattern(match.value, type, ctx) do
      {:error, pattern_type} ->
        {:error, pattern_type}

      {:ok, {value, value_type, value_bindings}} ->
        if Subtyping.subtype_of?(type, value_type) do
          case do_handle_pattern(match.pattern, type, ctx) do
            {:error, _} = error ->
              error

            {:ok, {pattern, pattern_type, pattern_bindings}} ->
              erased = {:=, match.meta, [pattern, value]}
              bindings = value_bindings ++ pattern_bindings

              {:ok, {erased, pattern_type, bindings}}
          end
        else
          {:error, value_type}
        end
    end
  end

  # Tuple patterns
  defp do_handle_pattern(%AST.Tuple{} = tuple, %Type.FixedTuple{} = type, ctx) do
    results =
      tuple.elements
      |> Enum.zip(Type.FixedTuple.elements(type))
      |> Enum.map(fn {element, elem_type} ->
        do_handle_pattern(element, elem_type, ctx)
      end)

    # Check if any element pattern failed.
    case Enum.find(results, &match?({:error, _}, &1)) do
      {:error, _} = error ->
        error

      nil ->
        {elements, types, inner_bindings} =
          Enum.reduce(results, {[], [], []}, fn {:ok, {element, element_type, element_bindings}},
                                                {elements, types, inner_bindings} ->
            {elements ++ [element], types ++ [element_type], inner_bindings ++ element_bindings}
          end)

        erased = {:{}, tuple.meta, elements}
        tuple_type = Type.fixed_tuple(types)

        # Merge duplicate bindings with intersection types.
        # Use top() as initial value since intersection(top, x) = x.
        bindings =
          inner_bindings
          |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
          |> Enum.map(fn {local, types} ->
            merged_type = Enum.reduce(types, Type.top(), &Type.intersection/2)
            {local, merged_type}
          end)

        {:ok, {erased, tuple_type, bindings}}
    end
  end

  # List patterns
  defp do_handle_pattern(%AST.List{} = list, %Type.FixedList{} = type, ctx) do
    elem_type = Type.FixedList.contents(type)

    results = Enum.map(list.elements, fn element ->
      do_handle_pattern(element, elem_type, ctx)
    end)

    # Check if any element pattern failed.
    case Enum.find(results, &match?({:error, _}, &1)) do
      {:error, _} = error ->
        error

      nil ->
        {elements, element_types, inner_bindings} =
          Enum.reduce(results, {[], [], []}, fn {:ok, {element, element_t, element_bindings}},
                                                {elements, element_types, inner_bindings} ->
            {
              elements ++ [element],
              element_types ++ [element_t],
              inner_bindings ++ element_bindings
            }
          end)

        elements_type =
          element_types
          |> Enum.reduce(Type.bottom(), &Type.union/2)
          |> Type.fixed_list()

        {:ok, {elements, elements_type, inner_bindings}}
    end
  end

  # Cons patterns ([head | rest])
  defp do_handle_pattern(%AST.Cons{} = cons, type, ctx) do
    case do_handle_pattern(cons.head, type, ctx) do
      {:error, _} = error ->
        error

      {:ok, {head, _, head_bindings}} ->
        case do_handle_pattern(cons.rest, Type.fixed_list(type), ctx) do
          {:error, _} = error ->
            error

          {:ok, {rest, _, rest_bindings}} ->
            erased = {:|, cons.meta, [head, rest]}
            {:ok, {erased, type, head_bindings ++ rest_bindings}}
        end
    end
  end

  # Type constructor patterns (ADT matching)
  defp do_handle_pattern(%AST.TypeConstructorCall{} = constructor, %Type.ADT{}, ctx) do
    variant = constructor.variant

    results =
      constructor.args
      |> Enum.zip(variant.columns)
      |> Enum.map(fn {column, column_type} ->
        do_handle_pattern(column, column_type, ctx)
      end)

    # Check if any column pattern failed.
    case Enum.find(results, &match?({:error, _}, &1)) do
      {:error, _} = error ->
        error

      nil ->
        {columns, types, inner_bindings} =
          Enum.reduce(results, {[], [], []}, fn {:ok, {column, col_type, column_bindings}},
                                                {columns, types, inner_bindings} ->
            {columns ++ [column], types ++ [col_type], inner_bindings ++ column_bindings}
          end)

        if length(types) == length(variant.columns) and
             Subtyping.subtypes_of?(variant.columns, types) do
          columns = [constructor.name | columns]
          erased = {:{}, constructor.meta, columns}

          # Merge duplicate bindings with intersection types.
          # Use top() as initial value since intersection(top, x) = x.
          bindings =
            inner_bindings
            |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
            |> Enum.map(fn {local, types} ->
              merged_type = Enum.reduce(types, Type.top(), &Type.intersection/2)
              {local, merged_type}
            end)

          {:ok, {erased, variant, bindings}}
        else
          # Column types don't match variant's expected types.
          {:error, variant}
        end
    end
  end

  # Catch-all for unhandled pattern/type combinations.
  defp do_handle_pattern(pattern, type, ctx) do
    {notes, suggestions} = unhandled_pattern_hints(pattern, type)

    error =
      Error.unsupported_pattern(
        expression: pattern,
        location: Span.extract(pattern),
        suggestions: suggestions,
        notes: notes
      )

    Error.raise!(error, ctx)
  end

  # ============================================================================
  # Error Helpers
  # ============================================================================

  defp unhandled_pattern_hints(pattern, type) do
    pattern_desc = describe_pattern(pattern)
    type_desc = Deft.Error.format_type(type)

    notes = ["Cannot match #{pattern_desc} against type `#{type_desc}`."]

    suggestions =
      case {pattern, type} do
        {%AST.Tuple{}, %Type.FixedList{}} ->
          ["Tuple patterns can only match tuple types, not list types."]

        {%AST.List{}, %Type.FixedTuple{}} ->
          ["List patterns can only match list types, not tuple types."]

        {%AST.TypeConstructorCall{}, type} when not is_struct(type, Type.ADT) ->
          ["ADT constructor patterns can only match ADT types."]

        _ ->
          ["Check that the pattern structure matches the expected type."]
      end

    {notes, suggestions}
  end

  defp describe_pattern(%AST.Literal{value: v}) when is_atom(v), do: "atom pattern `#{v}`"
  defp describe_pattern(%AST.Literal{value: v}) when is_integer(v), do: "integer pattern `#{v}`"
  defp describe_pattern(%AST.Literal{value: v}) when is_float(v), do: "float pattern `#{v}`"
  defp describe_pattern(%AST.Literal{}), do: "literal pattern"
  defp describe_pattern(%AST.Local{name: name}), do: "variable pattern `#{name}`"
  defp describe_pattern(%AST.Tuple{}), do: "tuple pattern"
  defp describe_pattern(%AST.List{}), do: "list pattern"
  defp describe_pattern(%AST.Cons{}), do: "cons pattern [h|t]"
  defp describe_pattern(%AST.Pin{expr: %AST.Local{name: name}}), do: "pin pattern `^#{name}`"
  defp describe_pattern(%AST.Pin{}), do: "pin pattern"
  defp describe_pattern(%AST.Match{}), do: "match pattern (=)"
  defp describe_pattern(%AST.TypeConstructorCall{name: name}), do: "constructor pattern `#{name}`"
  defp describe_pattern(_), do: "pattern"

  # Helper to get the type of a literal value
  defp literal_type(v) when is_boolean(v), do: Type.boolean()
  defp literal_type(v) when is_atom(v), do: Type.atom()
  defp literal_type(v) when is_binary(v), do: Type.binary()
  defp literal_type(v) when is_integer(v), do: Type.integer()
  defp literal_type(v) when is_float(v), do: Type.float()
  defp literal_type(_), do: Type.top()
end
