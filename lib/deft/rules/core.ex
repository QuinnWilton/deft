defmodule Deft.Rules.Core do
  @moduledoc """
  Core typing rules implemented using the declarative DSL.

  These rules handle the base language constructs:
  - Literals (atoms, booleans, numbers)
  - Variables (locals)
  - Annotations (type ascriptions)
  - Blocks (sequential expressions)
  - Tuples and Lists
  - Pairs
  """

  use Deft.Rules.DSL

  alias Deft.AST
  alias Deft.AST.Erased
  alias Deft.Helpers
  alias Deft.Type
  alias Deft.TypeChecker

  # ============================================================================
  # Literal Rule
  # ============================================================================

  defrule :literal, %AST.Literal{value: value} do
    conclude(value ~> type_of_literal(value))
  end

  # ============================================================================
  # Local Variable Rule
  # ============================================================================

  defrule :local, %AST.Local{name: name, meta: meta, context: context} do
    type = Keyword.get(meta, :__deft_type__)

    conclude({name, meta, context} ~> type)
  end

  # ============================================================================
  # Annotation Rule
  # ============================================================================

  defrule :annotation, %AST.Annotation{pattern: pattern, type: type} do
    compute {erased, result_bindings} do
      case pattern do
        %AST.Local{name: name, meta: meta, context: context} = local ->
          {{name, meta, context}, [{local, type}]}

        _ ->
          {pattern, []}
      end
    end

    conclude(erased ~> type, bind: result_bindings)
  end

  # ============================================================================
  # Block Rule
  # ============================================================================

  defrule :block, %AST.Block{exprs: exprs, meta: meta} do
    compute {erased_exprs, final_type} do
      Deft.Rules.Core.check_block_exprs(exprs, ctx)
    end

    conclude(Erased.block(meta, erased_exprs) ~> final_type)
  end

  @doc false
  def check_block_exprs(exprs, ctx) do
    {erased_exprs, final_type, _accumulated_bindings, _ctx} =
      Enum.reduce(exprs, {[], nil, [], ctx}, fn expr,
                                                {acc_exprs, _prev_type, acc_bindings, acc_ctx} ->
        case expr do
          %AST.DefData{} = defdata ->
            defdata = Helpers.inject_bindings(defdata, acc_bindings)
            type = handle_def_data(defdata)

            adt_bindings =
              Enum.reduce(type.variants, [{:adt, defdata.name, type}], fn
                %Type.Variant{} = variant, acc ->
                  acc ++ [{:adt_variant, variant.name, type, variant}]
              end)

            new_bindings = acc_bindings ++ adt_bindings
            {acc_exprs, nil, new_bindings, acc_ctx}

          _ ->
            case TypeChecker.check_in_context(expr, acc_bindings, acc_ctx) do
              {:ok, erased, expr_type, bindings, acc_ctx} ->
                new_bindings = acc_bindings ++ bindings
                {acc_exprs ++ [erased], expr_type, new_bindings, acc_ctx}

              {:error, %Deft.Error{} = error} ->
                # Re-raise the error with full context.
                Deft.Error.raise!(error)
            end
        end
      end)

    {erased_exprs, final_type}
  end

  defp handle_def_data(%AST.DefData{} = node) do
    variants =
      Enum.map(node.variants, fn %AST.Variant{} = variant ->
        Type.variant(variant.name, node.name, variant.columns)
      end)

    Type.adt(node.name, variants)
  end

  # ============================================================================
  # Tuple Rule
  # ============================================================================

  defrule :tuple, %AST.Tuple{elements: elements, meta: meta} do
    elements ~>> {erased_elements, types}

    conclude(Erased.tuple(meta, erased_elements) ~> Type.fixed_tuple(types))
  end

  # ============================================================================
  # Pair Rule
  # ============================================================================

  defrule :pair, %AST.Pair{fst: fst, snd: snd} do
    fst ~> {erased_fst, fst_type}
    snd ~> {erased_snd, snd_type}

    conclude(Erased.pair(erased_fst, erased_snd) ~> Type.fixed_tuple([fst_type, snd_type]))
  end

  # ============================================================================
  # List Rule
  # ============================================================================

  defrule :list, %AST.List{elements: elements} do
    elements ~>> {erased_elements, types}

    elem_type = union_types(types)

    conclude(erased_elements ~> Type.fixed_list(elem_type))
  end

  # ============================================================================
  # Cons Rule (list construction [head | tail])
  # ============================================================================

  defrule :cons, %AST.Cons{head: head, rest: rest, meta: meta} do
    head ~> {erased_head, head_type}
    rest ~> {erased_rest, rest_type}

    compute result_type do
      # The rest must be a list type.
      case rest_type do
        %Type.FixedList{} ->
          elem_type = Type.FixedList.contents(rest_type)
          Type.fixed_list(Type.union(head_type, elem_type))

        %Type.List{} ->
          Type.list()

        _ ->
          Deft.Error.raise!(
            Deft.Error.type_mismatch(
              expected: Type.list(),
              actual: rest_type
            )
          )
      end
    end

    conclude(Erased.cons(meta, erased_head, erased_rest) ~> result_type)
  end
end
