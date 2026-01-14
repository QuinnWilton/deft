defmodule Deft.Rules.DSL.Codegen do
  @moduledoc """
  Code generation helpers for the typing rule DSL.

  This module contains functions that transform DSL syntax (premises and
  conclusions) into executable Elixir code during macro expansion.
  """

  @doc """
  Generates code for a list of premises.
  """
  def generate_premises_code(premises) do
    Enum.map(premises, &generate_premise_code/1)
  end

  # ============================================================================
  # Premise Code Generation
  # ============================================================================

  @doc """
  Generates code for a single premise.

  Handles all DSL judgment forms: synthesis, checking, context extension,
  pattern judgments, and escape hatches (compute, if_feature).
  """
  # Synthesis with context extension: bindings +++ expr ~> {erased, type}
  # MUST come before the generic 2-tuple synth pattern
  def generate_premise_code({:~>, _, [{:+++, _, [extra_bindings, expr]}, {erased_var, type_var}]}) do
    generate_synth_with_context_code(extra_bindings, expr, erased_var, type_var)
  end

  # Synthesis with context extension: {:synth, {:extend_ctx, bindings, expr}, result} (expanded form)
  def generate_premise_code({:synth, {:extend_ctx, extra_bindings, expr}, {erased_var, type_var}}) do
    generate_synth_with_context_code(extra_bindings, expr, erased_var, type_var)
  end

  # Synthesis: expr ~> {erased, type} (unexpanded form)
  def generate_premise_code({:~>, _, [expr, {:{}, _, [erased_var, type_var]}]}) do
    generate_synth_code(expr, erased_var, type_var)
  end

  def generate_premise_code({:~>, _, [expr, {erased_var, type_var}]})
      when is_tuple(erased_var) do
    generate_synth_code(expr, erased_var, type_var)
  end

  # Synth all: exprs ~>> {erased_list, types_list}
  def generate_premise_code({:~>>, _, [exprs, {erased_var, types_var}]}) do
    generate_synth_all_code(exprs, erased_var, types_var)
  end

  # Scoped synth all with 3-element binding: (exprs &&& [k: v]) ~>> {erased, a, b}
  # Auto-unzips {a, b} tuple results into separate lists
  # NOTE: Must come before the general 3-element pattern below
  def generate_premise_code(
        {:~>>, _,
         [
           {:&&&, _, [exprs, scope_attrs]},
           {:{}, _, [erased_var, a_var, b_var]}
         ]}
      ) do
    generate_scoped_synth_all_3(exprs, scope_attrs, erased_var, a_var, b_var)
  end

  # Synth all with explicit bindings: exprs ~>> {erased_list, types_list, bindings_list}
  def generate_premise_code({:~>>, _, [exprs, {:{}, _, [erased_var, types_var, bindings_var]}]}) do
    generate_synth_all_with_bindings_code(exprs, erased_var, types_var, bindings_var)
  end

  # Context extension: extra_bindings +++ premise
  def generate_premise_code({:+++, _, [extra_bindings, premise]}) do
    generate_context_extension_code(extra_bindings, premise)
  end

  # Compute escape hatch: compute pattern do ... end
  # Return raw AST directly - no quote needed
  def generate_premise_code({:compute, _, [pattern, [do: body]]}) do
    {:=, [], [pattern, body]}
  end

  # Feature check: if_feature :name do ... end
  def generate_premise_code({:if_feature, _, [feature, [do: body]]}) do
    ctx_var = Macro.var(:ctx, nil)

    quote do
      if Deft.Context.feature_enabled?(unquote(ctx_var), unquote(feature)) do
        unquote(body)
      end
    end
  end

  # Scoped attribute read: var = scoped(:key)
  # Generates: var = Deft.Context.get_scoped(ctx, :key)
  def generate_premise_code({:=, _, [var, {:scoped, _, [key]}]}) do
    ctx_var = Macro.var(:ctx, nil)

    quote do
      unquote(var) = Deft.Context.get_scoped(unquote(ctx_var), unquote(key))
    end
  end

  # Pattern judgment: pattern <~> expected_type >>> {erased, type, bindings}
  # Generates: {erased, type, bindings} = PatternMatching.handle_pattern(pattern, expected_type, ctx)
  def generate_premise_code(
        {:>>>, _,
         [
           {:<~>, _, [pattern, expected_type]},
           {:{}, _, [erased_var, type_var, bindings_var]}
         ]}
      ) do
    ctx_var = Macro.var(:ctx, nil)

    quote do
      {unquote(erased_var), unquote(type_var), unquote(bindings_var)} =
        Deft.PatternMatching.handle_pattern(
          unquote(pattern),
          unquote(expected_type),
          unquote(ctx_var)
        )
    end
  end

  # Checking with type binding: expr <~ expected_type >>> {erased, type}
  # Checks expression against expected type, binds erased form and actual type.
  # NOTE: More specific patterns must come BEFORE the general `>>> erased_var` pattern.
  def generate_premise_code({:>>>, _, [{:<~, _, [expr, expected_type]}, {erased_var, type_var}]}) do
    generate_check_with_type_code(expr, expected_type, erased_var, type_var)
  end

  def generate_premise_code(
        {:>>>, _, [{:<~, _, [expr, expected_type]}, {:{}, _, [erased_var, type_var]}]}
      ) do
    generate_check_with_type_code(expr, expected_type, erased_var, type_var)
  end

  # Checking: expr <~ expected_type >>> erased
  # Checks expression against expected type, binds erased form.
  def generate_premise_code({:>>>, _, [{:<~, _, [expr, expected_type]}, erased_var]}) do
    generate_check_code(expr, expected_type, erased_var)
  end

  # Check all: exprs <<~ expected_types >>> erased_list
  # Checks list of expressions against list of expected types.
  def generate_premise_code({:>>>, _, [{:<<~, _, [exprs, expected_types]}, erased_var]}) do
    generate_check_all_code(exprs, expected_types, erased_var)
  end

  # Plain assignment or pattern match (for destructuring, etc.)
  def generate_premise_code({:=, _, [_left, _right]} = assignment) do
    assignment
  end

  # Any other expression (like function calls for side effects)
  def generate_premise_code(other) do
    other
  end

  # ============================================================================
  # Conclusion Code Generation
  # ============================================================================

  @doc """
  Generates code for the rule conclusion.
  """
  # conclude erased ~> type
  def generate_conclusion_code({:conclude, _, [{:~>, _, [erased, type]}]}) do
    bindings_var = Macro.var(:bindings, nil)
    ctx_var = Macro.var(:ctx, nil)

    quote do
      {:ok, unquote(erased), unquote(type), unquote(bindings_var), unquote(ctx_var)}
    end
  end

  # conclude erased ~> type, bind: explicit_bindings
  def generate_conclusion_code(
        {:conclude, _, [{:~>, _, [erased, type]}, [bind: explicit_bindings]]}
      ) do
    bindings_var = Macro.var(:bindings, nil)
    ctx_var = Macro.var(:ctx, nil)

    quote do
      # Suppress unused variable warning for accumulated bindings
      _ = unquote(bindings_var)
      {:ok, unquote(erased), unquote(type), unquote(explicit_bindings), unquote(ctx_var)}
    end
  end

  # conclude erased ~> type, bindings: explicit_bindings (alias)
  def generate_conclusion_code(
        {:conclude, _, [{:~>, _, [erased, type]}, [bindings: explicit_bindings]]}
      ) do
    bindings_var = Macro.var(:bindings, nil)
    ctx_var = Macro.var(:ctx, nil)

    quote do
      # Suppress unused variable warning for accumulated bindings
      _ = unquote(bindings_var)
      {:ok, unquote(erased), unquote(type), unquote(explicit_bindings), unquote(ctx_var)}
    end
  end

  # ============================================================================
  # Helper functions for premise code generation
  # ============================================================================

  # Helper for scoped synth with 3-element binding (auto-unzip)
  defp generate_scoped_synth_all_3(exprs, scope_attrs, erased_var, a_var, b_var) do
    bindings_var = Macro.var(:bindings, nil)
    ctx_var = Macro.var(:ctx, nil)

    quote do
      __scoped_ctx__ = Deft.Context.with_scoped(unquote(ctx_var), unquote(scope_attrs))

      {unquote(erased_var), __results__, _} =
        Deft.Rules.DSL.Helpers.synth_all_with_ctx!(
          unquote(exprs),
          unquote(bindings_var),
          __scoped_ctx__
        )

      {unquote(a_var), unquote(b_var)} = Enum.unzip(__results__)
    end
  end

  defp generate_synth_with_context_code(extra_bindings, expr, erased_var, type_var) do
    new_bindings_var = Macro.var(:__new_bindings__, __MODULE__)
    bindings_var = Macro.var(:bindings, nil)
    old_bindings_var = Macro.var(:__old_bindings__, __MODULE__)
    ctx_var = Macro.var(:ctx, nil)

    quote do
      unquote(old_bindings_var) = unquote(bindings_var)
      unquote(bindings_var) = unquote(bindings_var) ++ unquote(extra_bindings)

      {unquote(erased_var), unquote(type_var), unquote(new_bindings_var)} =
        Deft.Rules.DSL.Helpers.synth!(unquote(expr), unquote(bindings_var), unquote(ctx_var))

      unquote(bindings_var) = unquote(old_bindings_var) ++ unquote(new_bindings_var)
    end
  end

  defp generate_synth_code(expr, erased_var, type_var) do
    # Build: {erased_var, type_var, __new_bindings__} = synth!(expr, bindings, ctx)
    #        bindings = bindings ++ __new_bindings__
    new_bindings_var = Macro.var(:__new_bindings__, __MODULE__)
    bindings_var = Macro.var(:bindings, nil)
    ctx_var = Macro.var(:ctx, nil)

    quote do
      {unquote(erased_var), unquote(type_var), unquote(new_bindings_var)} =
        Deft.Rules.DSL.Helpers.synth!(unquote(expr), unquote(bindings_var), unquote(ctx_var))

      unquote(bindings_var) = unquote(bindings_var) ++ unquote(new_bindings_var)
    end
  end

  defp generate_synth_all_code(exprs, erased_var, types_var) do
    new_bindings_var = Macro.var(:__new_bindings__, __MODULE__)
    bindings_var = Macro.var(:bindings, nil)
    ctx_var = Macro.var(:ctx, nil)

    quote do
      {unquote(erased_var), unquote(types_var), unquote(new_bindings_var)} =
        Deft.Rules.DSL.Helpers.synth_all!(unquote(exprs), unquote(ctx_var))

      unquote(bindings_var) = unquote(bindings_var) ++ unquote(new_bindings_var)
    end
  end

  # Synth all with explicit bindings - does not auto-merge
  defp generate_synth_all_with_bindings_code(exprs, erased_var, types_var, bindings_var) do
    ctx_var = Macro.var(:ctx, nil)

    quote do
      {unquote(erased_var), unquote(types_var), unquote(bindings_var)} =
        Deft.Rules.DSL.Helpers.synth_all!(unquote(exprs), unquote(ctx_var))
    end
  end

  defp generate_context_extension_code(extra_bindings, premise) do
    inner_code = generate_premise_code(premise)
    bindings_var = Macro.var(:bindings, nil)
    old_bindings_var = Macro.var(:__old_bindings__, __MODULE__)

    quote do
      unquote(old_bindings_var) = unquote(bindings_var)
      unquote(bindings_var) = unquote(bindings_var) ++ unquote(extra_bindings)
      unquote(inner_code)
      unquote(bindings_var) = unquote(old_bindings_var)
    end
  end

  defp generate_check_code(expr, expected_type, erased_var) do
    new_bindings_var = Macro.var(:__new_bindings__, __MODULE__)
    bindings_var = Macro.var(:bindings, nil)
    ctx_var = Macro.var(:ctx, nil)

    quote do
      {unquote(erased_var), _, unquote(new_bindings_var)} =
        Deft.Rules.DSL.Helpers.check!(
          unquote(expr),
          unquote(expected_type),
          unquote(bindings_var),
          unquote(ctx_var)
        )

      unquote(bindings_var) = unquote(bindings_var) ++ unquote(new_bindings_var)
    end
  end

  defp generate_check_with_type_code(expr, expected_type, erased_var, type_var) do
    new_bindings_var = Macro.var(:__new_bindings__, __MODULE__)
    bindings_var = Macro.var(:bindings, nil)
    ctx_var = Macro.var(:ctx, nil)

    quote do
      {unquote(erased_var), unquote(type_var), unquote(new_bindings_var)} =
        Deft.Rules.DSL.Helpers.check!(
          unquote(expr),
          unquote(expected_type),
          unquote(bindings_var),
          unquote(ctx_var)
        )

      unquote(bindings_var) = unquote(bindings_var) ++ unquote(new_bindings_var)
    end
  end

  defp generate_check_all_code(exprs, expected_types, erased_var) do
    new_bindings_var = Macro.var(:__new_bindings__, __MODULE__)
    bindings_var = Macro.var(:bindings, nil)
    ctx_var = Macro.var(:ctx, nil)

    quote do
      {unquote(erased_var), unquote(new_bindings_var)} =
        Deft.Rules.DSL.Helpers.check_all!(
          unquote(exprs),
          unquote(expected_types),
          unquote(bindings_var),
          unquote(ctx_var)
        )

      unquote(bindings_var) = unquote(bindings_var) ++ unquote(new_bindings_var)
    end
  end
end
