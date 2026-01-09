defmodule Deft.Rules.Declarative.Core do
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

  use Deft.Rule.DSL

  alias Deft.AST
  alias Deft.Helpers
  alias Deft.Type
  alias Deft.TypeChecker

  # ============================================================================
  # Literal Rule
  # ============================================================================

  defrule(:literal,
    match: %AST.Literal{},
    judgment: :synth,
    do:
      (
        %AST.Literal{value: value} = ast
        type = type_of_literal(value)
        emit(value, type)
      )
  )

  # ============================================================================
  # Local Variable Rule
  # ============================================================================

  defrule(:local,
    match: %AST.Local{},
    judgment: :synth,
    do:
      (
        %AST.Local{name: name, meta: meta, context: context} = ast
        erased = {name, meta, context}
        # Type may be in metadata if already bound via inject_bindings
        type = Keyword.get(meta, :__deft_type__)
        emit(erased, type)
      )
  )

  # ============================================================================
  # Annotation Rule
  # ============================================================================

  defrule(:annotation,
    match: %AST.Annotation{},
    judgment: :synth,
    do:
      (
        %AST.Annotation{pattern: pattern, type: type} = ast

        case pattern do
          %AST.Local{name: name, meta: meta, context: context} = local ->
            erased = {name, meta, context}
            bindings = [{local, type}]
            emit(erased, type, bindings)

          _ ->
            # For more complex patterns, delegate to pattern matching
            emit(pattern, type)
        end
      )
  )

  # ============================================================================
  # Block Rule
  # ============================================================================

  defrule(:block,
    match: %AST.Block{},
    judgment: :synth,
    do:
      (
        %AST.Block{exprs: exprs, meta: meta} = ast

        {erased_exprs, final_type, new_ctx} =
          Deft.Rules.Declarative.Core.check_block_exprs(exprs, ctx)

        erased = {:__block__, meta, erased_exprs}
        emit_with_ctx(erased, final_type, [], new_ctx)
      )
  )

  @doc false
  def check_block_exprs(exprs, ctx) do
    {erased_exprs, final_type, _accumulated_bindings, ctx} =
      Enum.reduce(exprs, {[], nil, [], ctx}, fn expr,
                                                {acc_exprs, _prev_type, acc_bindings, acc_ctx} ->
        case expr do
          %AST.DefData{} = defdata ->
            # Inject previous ADT bindings so Type.Alias references are resolved
            defdata = Helpers.inject_bindings(defdata, acc_bindings)

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

  # ============================================================================
  # Tuple Rule
  # ============================================================================

  defrule(:tuple,
    match: %AST.Tuple{},
    judgment: :synth,
    do:
      (
        %AST.Tuple{elements: elements, meta: meta} = ast
        {erased_elements, types, bindings} = check_all!(elements, ctx)
        type = Type.fixed_tuple(types)
        erased = {:{}, meta, erased_elements}
        emit(erased, type, bindings)
      )
  )

  # ============================================================================
  # Pair Rule
  # ============================================================================

  defrule(:pair,
    match: %AST.Pair{},
    judgment: :synth,
    do:
      (
        %AST.Pair{fst: fst, snd: snd} = ast
        {erased_fst, fst_type, fst_bindings} = synth!(fst, ctx)
        {erased_snd, snd_type, snd_bindings} = synth!(snd, ctx)
        type = Type.fixed_tuple([fst_type, snd_type])
        erased = {erased_fst, erased_snd}
        bindings = fst_bindings ++ snd_bindings
        emit(erased, type, bindings)
      )
  )

  # ============================================================================
  # List Rule
  # ============================================================================

  defrule(:list,
    match: %AST.List{},
    judgment: :synth,
    do:
      (
        %AST.List{elements: elements} = ast
        {erased_elements, types, bindings} = check_all!(elements, ctx)
        # Compute element type as union of all element types
        elem_type = union_types(types)
        type = Type.fixed_list(elem_type)
        emit(erased_elements, type, bindings)
      )
  )
end
