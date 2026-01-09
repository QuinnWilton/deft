defmodule Deft.Rules.Core do
  @moduledoc """
  Core typing rules for the base language.

  These rules implement bidirectional type checking for:
  - Literals (atoms, booleans, numbers)
  - Variables (locals)
  - Annotations (type ascriptions)
  - Blocks (sequential expressions)
  - Tuples and Lists
  - Pairs
  """

  alias Deft.AST
  alias Deft.Type
  alias Deft.TypeChecker

  # Literal rule - synthesizes types for literal values
  defmodule Literal do
    @behaviour Deft.Rule

    @impl true
    def name, do: :literal

    @impl true
    def judgment, do: :synth

    @impl true
    def matches?(%AST.Literal{}), do: true
    def matches?(_), do: false

    @impl true
    def apply(%AST.Literal{value: value}, _expected, ctx) do
      type = type_of_literal(value)
      {:ok, value, type, [], ctx}
    end

    defp type_of_literal(v) when is_boolean(v), do: Type.boolean()
    defp type_of_literal(v) when is_atom(v), do: Type.atom()
    defp type_of_literal(v) when is_integer(v), do: Type.integer()
    defp type_of_literal(v) when is_float(v), do: Type.float()
  end

  # Local variable rule - looks up type in context or returns untyped
  defmodule Local do
    @behaviour Deft.Rule

    @impl true
    def name, do: :local

    @impl true
    def judgment, do: :synth

    @impl true
    def matches?(%AST.Local{}), do: true
    def matches?(_), do: false

    @impl true
    def apply(%AST.Local{} = local, _expected, ctx) do
      erased = {local.name, local.meta, local.context}
      # Type may be in metadata if already bound
      type = Keyword.get(local.meta, :__deft_type__)
      {:ok, erased, type, [], ctx}
    end
  end

  # Annotation rule - validates pattern against declared type
  defmodule Annotation do
    @behaviour Deft.Rule

    @impl true
    def name, do: :annotation

    @impl true
    def judgment, do: :synth

    @impl true
    def matches?(%AST.Annotation{}), do: true
    def matches?(_), do: false

    @impl true
    def apply(%AST.Annotation{pattern: pattern, type: type}, _expected, ctx) do
      case pattern do
        %AST.Local{} = local ->
          erased = {local.name, local.meta, local.context}
          bindings = [{local, type}]
          {:ok, erased, type, bindings, ctx}

        _ ->
          # For more complex patterns, delegate to pattern matching
          {:ok, pattern, type, [], ctx}
      end
    end
  end

  # Block rule - sequences expressions, threading bindings
  defmodule Block do
    @behaviour Deft.Rule

    @impl true
    def name, do: :block

    @impl true
    def judgment, do: :synth

    @impl true
    def matches?(%AST.Block{}), do: true
    def matches?(_), do: false

    @impl true
    def apply(%AST.Block{exprs: exprs, meta: meta}, _expected, ctx) do
      {erased_exprs, final_type, ctx} = check_exprs(exprs, ctx)
      erased = {:__block__, meta, erased_exprs}
      {:ok, erased, final_type, [], ctx}
    end

    defp check_exprs(exprs, ctx) do
      {erased_exprs, final_type, _accumulated_bindings, ctx} =
        Enum.reduce(exprs, {[], nil, [], ctx}, fn expr,
                                                  {acc_exprs, _prev_type, acc_bindings, acc_ctx} ->
          case expr do
            %AST.DefData{} = defdata ->
              # Inject previous ADT bindings so Type.Alias references are resolved
              defdata = Deft.Helpers.inject_bindings(defdata, acc_bindings)

              # Handle ADT definitions - they don't produce erased code but bind types
              type = handle_def_data(defdata)

              adt_bindings =
                Enum.reduce(type.variants, [{:adt, defdata.name, type}], fn
                  %Type.Variant{} = variant, acc ->
                    acc ++ [{:adt_variant, variant.name, type, variant}]
                end)

              new_bindings = acc_bindings ++ adt_bindings
              {acc_exprs, nil, new_bindings, acc_ctx}

            _ ->
              # Inject accumulated bindings into this expression before checking
              {:ok, erased, expr_type, bindings, acc_ctx} =
                TypeChecker.check_in_context(expr, acc_bindings, acc_ctx)

              new_bindings = acc_bindings ++ bindings
              {acc_exprs ++ [erased], expr_type, new_bindings, acc_ctx}
          end
        end)

      {erased_exprs, final_type, ctx}
    end

    defp handle_def_data(%AST.DefData{} = node) do
      variants =
        Enum.map(node.variants, fn %AST.Variant{} = variant ->
          Type.variant(variant.name, node.name, variant.columns)
        end)

      Type.adt(node.name, variants)
    end
  end

  # Tuple rule - type checks each element
  defmodule Tuple do
    @behaviour Deft.Rule

    @impl true
    def name, do: :tuple

    @impl true
    def judgment, do: :synth

    @impl true
    def matches?(%AST.Tuple{}), do: true
    def matches?(_), do: false

    @impl true
    def apply(%AST.Tuple{elements: elements, meta: meta}, _expected, ctx) do
      {erased_elements, types, bindings, ctx} = check_elements(elements, ctx)
      type = Type.fixed_tuple(types)
      erased = {:{}, meta, erased_elements}
      {:ok, erased, type, bindings, ctx}
    end

    defp check_elements(elements, ctx) do
      {erased, types, bindings} =
        Enum.reduce(elements, {[], [], []}, fn elem, {acc_erased, acc_types, acc_bindings} ->
          {:ok, erased, type, elem_bindings, _ctx} = TypeChecker.check(elem, ctx)
          {acc_erased ++ [erased], acc_types ++ [type], acc_bindings ++ elem_bindings}
        end)

      {erased, types, bindings, ctx}
    end
  end

  # Pair rule - converts to tuple
  defmodule Pair do
    @behaviour Deft.Rule

    @impl true
    def name, do: :pair

    @impl true
    def judgment, do: :synth

    @impl true
    def matches?(%AST.Pair{}), do: true
    def matches?(_), do: false

    @impl true
    def apply(%AST.Pair{fst: fst, snd: snd}, _expected, ctx) do
      {:ok, erased_fst, fst_type, fst_bindings, ctx} = TypeChecker.check(fst, ctx)
      {:ok, erased_snd, snd_type, snd_bindings, ctx} = TypeChecker.check(snd, ctx)

      type = Type.fixed_tuple([fst_type, snd_type])
      erased = {erased_fst, erased_snd}
      bindings = fst_bindings ++ snd_bindings

      {:ok, erased, type, bindings, ctx}
    end
  end

  # List rule - type checks elements
  defmodule List do
    @behaviour Deft.Rule

    @impl true
    def name, do: :list

    @impl true
    def judgment, do: :synth

    @impl true
    def matches?(%AST.List{}), do: true
    def matches?(_), do: false

    @impl true
    def apply(%AST.List{elements: elements}, _expected, ctx) do
      {erased_elements, types, bindings} =
        Enum.reduce(elements, {[], [], []}, fn elem, {acc_erased, acc_types, acc_bindings} ->
          {:ok, erased, type, elem_bindings, _ctx} = TypeChecker.check(elem, ctx)
          {acc_erased ++ [erased], acc_types ++ [type], acc_bindings ++ elem_bindings}
        end)

      # Compute element type as union of all element types
      elem_type = Enum.reduce(types, Type.bottom(), &Type.union/2)
      type = Type.fixed_list(elem_type)

      {:ok, erased_elements, type, bindings, ctx}
    end
  end

  @doc """
  Returns all core rules in order.
  """
  def rules do
    [
      Literal,
      Local,
      Annotation,
      Block,
      Tuple,
      Pair,
      List
    ]
  end
end
