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
    compute {pat_e, bs} do
      case pattern do
        %AST.Local{name: name, meta: meta, context: context} = local ->
          {{name, meta, context}, [{local, type}]}

        _ ->
          {pattern, []}
      end
    end

    conclude(pat_e ~> type, bind: bs)
  end

  # ============================================================================
  # Block Rule
  # ============================================================================

  defrule :block, %AST.Block{exprs: exprs, meta: meta} do
    compute {exprs_e, result_t} do
      Deft.Rules.Core.check_block_exprs(exprs, ctx)
    end

    conclude(Erased.block(meta, exprs_e) ~> result_t)
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

  defrule :tuple, %AST.Tuple{elements: elems, meta: meta} do
    elems ~>> {elems_e, elems_ts}

    conclude(Erased.tuple(meta, elems_e) ~> Type.fixed_tuple(elems_ts))
  end

  # ============================================================================
  # Pair Rule
  # ============================================================================

  defrule :pair, %AST.Pair{fst: fst, snd: snd} do
    fst ~> {fst_e, fst_t}
    snd ~> {snd_e, snd_t}

    conclude(Erased.pair(fst_e, snd_e) ~> Type.fixed_tuple([fst_t, snd_t]))
  end

  # ============================================================================
  # List Rule
  # ============================================================================

  defrule :list, %AST.List{elements: elems} do
    elems ~>> {elems_e, elems_ts}

    elem_t = union_types(elems_ts)

    conclude(elems_e ~> Type.fixed_list(elem_t))
  end

  # ============================================================================
  # Cons Rule (list construction [head | tail])
  # ============================================================================

  defrule :cons, %AST.Cons{head: head, rest: rest, meta: meta} do
    head ~> {head_e, head_t}
    rest ~> {rest_e, rest_t}

    compute result_t do
      # The rest must be a list type.
      case rest_t do
        %Type.FixedList{} ->
          elem_t = Type.FixedList.contents(rest_t)
          Type.fixed_list(Type.union(head_t, elem_t))

        %Type.List{} ->
          Type.list()

        _ ->
          Deft.Error.raise!(
            Deft.Error.type_mismatch(
              expected: Type.list(),
              actual: rest_t
            )
          )
      end
    end

    conclude(Erased.cons(meta, head_e, rest_e) ~> result_t)
  end
end
