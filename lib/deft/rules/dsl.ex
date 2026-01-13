defmodule Deft.Rules.DSL do
  @moduledoc """
  Declarative DSL for defining typing rules in the style of Turnstile.

  This module provides a declarative syntax for typing rules using
  judgment forms that closely mirror how typing rules are written
  in type theory.

  ## Judgment Forms

  ### Synthesis

      expr ~> {erased, type}

  Synthesizes (infers) the type of an expression. Returns the erased form
  and the inferred type.

  ### Synth All

      exprs ~>> {erased_list, types_list}

  Synthesizes types for a list of expressions. Returns parallel lists of
  erased forms and types.

  ### Context Extension

      bindings +++ expr ~> {erased, type}

  Extends the typing context with additional bindings before synthesizing.
  Bindings are scoped to the premise only.

  ### Scoped Context (Parent-to-Child)

      (exprs &&& [key: value]) ~>> {erased, types_a, types_b}

  Passes scoped attributes to child rules and auto-unzips tuple results.
  Each child rule returns `{a, b}`, collected into `{[a...], [b...]}`.

  ### Checking

      expr <~ expected_type >>> erased
      expr <~ expected_type >>> {erased, actual_type}

  Checks an expression against an expected type. The expression's type
  must be a subtype of the expected type.

  ### Check All

      exprs <<~ expected_type >>> erased_list   # homogeneous
      exprs <<~ expected_types >>> erased_list  # heterogeneous

  Two modes: if the right side is a single type, checks all expressions against
  that type. If it's a list of types, checks each expression against its
  corresponding type.

  ### Pattern Judgment

      pattern <~> expected_type >>> {erased, type, bindings}

  Checks a pattern against an expected type. Returns the erased pattern,
  the pattern's type, and any bindings produced.

  ## Escape Hatches

  ### Compute Block

      compute pattern do
        # arbitrary Elixir code
      end

  For complex logic that doesn't fit the declarative model.

  ### Feature Check

      if_feature :feature_name do
        # code executed only if feature is enabled
      end

  Conditionally executes code based on feature flags.

  ### Scoped Attribute Read

      subj_t = scoped(:subj_t)

  Reads a scoped attribute set by a parent rule via `&&&`.

  ## Conclusion Forms

      conclude erased ~> type
      conclude erased ~> type, bind: bindings

  The `bind:` option overrides accumulated bindings with explicit ones.

  ## Rule Requirements

      defrule :name, pattern do
        # premises
        conclude erased ~> type

        requires([:feature_a, :feature_b])
      end

  The `requires` clause declares features that must be enabled for the
  rule to apply. If features are missing, returns `{:error, {:missing_features, [...]}}`.

  ## Example

      defrule :literal, %AST.Literal{value: value} do
        conclude value ~> type_of_literal(value)
      end

      defrule :fn, %AST.Fn{args: args, body: body, fn_meta: fn_meta, arrow_meta: arrow_meta} do
        args ~>> {args_e, input_ts, arg_bs}
        (arg_bs +++ body) ~> {body_e, output_t}

        conclude Erased.fn_expr(fn_meta, arrow_meta, args_e, body_e)
              ~> Type.fun(input_ts, output_t)
      end

  ## Binding Flow

  Bindings from each premise automatically flow to subsequent premises,
  following Turnstile's lexically-scoped binding semantics.

  ## Naming Conventions

  - `_e` suffix: erased forms (e.g., `expr_e`, `args_e`)
  - `_t` suffix: types (e.g., `expr_t`, `output_t`)
  - `_ts` suffix: type lists (e.g., `elems_ts`, `input_ts`)
  - `_bs` suffix: bindings (e.g., `arg_bs`, `pat_bs`)
  """

  @doc """
  Enables the declarative Rule DSL in a module.
  """
  defmacro __using__(_opts) do
    quote do
      import Deft.Rules.DSL, only: [defrule: 3]
      import Deft.Rules.DSL.Operators
      import Deft.Rules.DSL.Helpers
      Module.register_attribute(__MODULE__, :deft_rules, accumulate: true)

      @before_compile Deft.Rules.DSL
    end
  end

  @doc false
  defmacro __before_compile__(env) do
    rules = Module.get_attribute(env.module, :deft_rules) || []

    quote do
      @doc """
      Returns all rules defined in this module.
      """
      def rules do
        unquote(Enum.reverse(rules))
      end
    end
  end

  @doc """
  Defines a typing rule with declarative syntax.

  The second argument is the pattern to match against.
  The block contains premises (judgment forms) and a `conclude` expression.
  """
  defmacro defrule(name, pattern, do: block) do
    pattern = unhygienize_block(pattern)
    # For matches?/1, we only care about structure, not the bound values
    matches_pattern = anonymize_pattern(pattern)
    {premises, conclusion, options} = parse_rule_block(block)

    requires = Keyword.get(options, :requires, [])
    rule_module_suffix = :"Rule_#{name}"

    # Generate the premises code
    premises_code = generate_premises_code(premises)

    # Generate the conclusion code
    conclusion_code = generate_conclusion_code(conclusion)

    # Generate the apply body, conditionally including feature check
    apply_body = generate_apply_body(premises_code, conclusion_code, requires)

    quote do
      @__rule_module_name__ Module.concat(__MODULE__, unquote(rule_module_suffix))

      defmodule @__rule_module_name__ do
        @behaviour Deft.Rules

        alias Deft.Context
        alias Deft.Subtyping
        alias Deft.Type
        alias Deft.TypeChecker
        alias Deft.AST
        alias Deft.Helpers, as: DeftHelpers

        import Deft.Rules.DSL.Helpers

        @impl true
        def name, do: unquote(name)

        @impl true
        def judgment, do: :synth

        @impl true
        def matches?(unquote(matches_pattern)), do: true
        def matches?(_), do: false

        @impl true
        def apply(unquote(pattern) = _ast, _expected, var!(ctx, nil)) do
          # Initialize bindings accumulator
          var!(bindings, nil) = []

          unquote(apply_body)
        end

        unquote(generate_feature_helpers(requires))
      end

      @deft_rules @__rule_module_name__
    end
  end

  # Generate apply body - only include feature check if there are required features
  defp generate_apply_body(premises_code, conclusion_code, []) do
    # No required features - execute directly
    quote do
      # Execute premises
      unquote_splicing(premises_code)

      # Execute conclusion
      unquote(conclusion_code)
    end
  end

  defp generate_apply_body(premises_code, conclusion_code, requires) do
    # Has required features - wrap in feature check
    quote do
      if features_enabled?(var!(ctx, nil), unquote(requires)) do
        # Execute premises
        unquote_splicing(premises_code)

        # Execute conclusion
        unquote(conclusion_code)
      else
        {:error, {:missing_features, unquote(requires)}}
      end
    end
  end

  # Generate feature helper functions only when needed
  defp generate_feature_helpers([]) do
    # No features required - no helper functions needed
    quote do
    end
  end

  defp generate_feature_helpers(_requires) do
    quote do
      defp features_enabled?(ctx, features) do
        Enum.all?(features, &Context.feature_enabled?(ctx, &1))
      end
    end
  end

  # Transform all variables in an AST to use nil context (unhygienic)
  # This ensures variables in the rule block can flow between premises and conclusion
  defp unhygienize_block(ast) do
    Macro.prewalk(ast, fn
      {name, meta, context} when is_atom(name) and is_atom(context) and context != nil ->
        # Variable with a specific context - convert to nil context
        {name, meta, nil}

      other ->
        other
    end)
  end

  # Transform all variables in a pattern to use underscore prefix
  # This is used for matches?/1 where we only check structure, not bound values
  defp anonymize_pattern(ast) do
    Macro.prewalk(ast, fn
      {name, meta, context} when is_atom(name) and is_atom(context) ->
        name_str = Atom.to_string(name)

        # Skip if already underscore-prefixed or is just underscore
        if String.starts_with?(name_str, "_") do
          {name, meta, context}
        else
          {:"_#{name}", meta, context}
        end

      other ->
        other
    end)
  end

  # Parse the rule block into premises, conclusion, and options
  # Pattern is now passed separately to defrule
  defp parse_rule_block({:__block__, _, exprs}) do
    exprs = Enum.map(exprs, &unhygienize_block/1)
    split_premises_and_conclusion(exprs)
  end

  defp parse_rule_block(single_expr) do
    split_premises_and_conclusion([unhygienize_block(single_expr)])
  end

  defp split_premises_and_conclusion(exprs) do
    # Find the conclude expression
    {premises, conclude_and_rest} =
      Enum.split_while(exprs, fn
        {:conclude, _, _} -> false
        _ -> true
      end)

    case conclude_and_rest do
      [{:conclude, _, _} = conclusion | rest] ->
        options = parse_options(rest)
        {premises, conclusion, options}

      [] ->
        raise ArgumentError, "defrule requires a `conclude` expression"
    end
  end

  defp parse_options([]), do: []

  defp parse_options(rest) do
    # Parse any options after conclude (like requires: [...])
    Enum.flat_map(rest, fn
      {:requires, _, [features]} -> [requires: features]
      _ -> []
    end)
  end

  # Generate code for premises
  # We build AST directly to avoid hygiene issues between quote blocks
  defp generate_premises_code(premises) do
    Enum.map(premises, &generate_premise_code/1)
  end

  # ============================================================================
  # Premise Code Generation - All generate_premise_code/1 clauses grouped together
  # ============================================================================

  # Synthesis with context extension: bindings +++ expr ~> {erased, type}
  # MUST come before the generic 2-tuple synth pattern
  defp generate_premise_code(
         {:~>, _, [{:+++, _, [extra_bindings, expr]}, {erased_var, type_var}]}
       ) do
    generate_synth_with_context_code(extra_bindings, expr, erased_var, type_var)
  end

  # Synthesis with context extension: {:synth, {:extend_ctx, bindings, expr}, result} (expanded form)
  defp generate_premise_code(
         {:synth, {:extend_ctx, extra_bindings, expr}, {erased_var, type_var}}
       ) do
    generate_synth_with_context_code(extra_bindings, expr, erased_var, type_var)
  end

  # Synthesis: expr ~> {erased, type} (unexpanded form)
  defp generate_premise_code({:~>, _, [expr, {:{}, _, [erased_var, type_var]}]}) do
    generate_synth_code(expr, erased_var, type_var)
  end

  defp generate_premise_code({:~>, _, [expr, {erased_var, type_var}]})
       when is_tuple(erased_var) do
    generate_synth_code(expr, erased_var, type_var)
  end

  # Synth all: exprs ~>> {erased_list, types_list}
  defp generate_premise_code({:~>>, _, [exprs, {erased_var, types_var}]}) do
    generate_synth_all_code(exprs, erased_var, types_var)
  end

  # Scoped synth all with 3-element binding: (exprs &&& [k: v]) ~>> {erased, a, b}
  # Auto-unzips {a, b} tuple results into separate lists
  # NOTE: Must come before the general 3-element pattern below
  defp generate_premise_code(
         {:~>>, _,
          [
            {:&&&, _, [exprs, scope_attrs]},
            {:{}, _, [erased_var, a_var, b_var]}
          ]}
       ) do
    generate_scoped_synth_all_3(exprs, scope_attrs, erased_var, a_var, b_var)
  end

  # Synth all with explicit bindings: exprs ~>> {erased_list, types_list, bindings_list}
  defp generate_premise_code({:~>>, _, [exprs, {:{}, _, [erased_var, types_var, bindings_var]}]}) do
    generate_synth_all_with_bindings_code(exprs, erased_var, types_var, bindings_var)
  end

  # Context extension: extra_bindings +++ premise
  defp generate_premise_code({:+++, _, [extra_bindings, premise]}) do
    generate_context_extension_code(extra_bindings, premise)
  end

  # Compute escape hatch: compute pattern do ... end
  # Return raw AST directly - no quote needed
  defp generate_premise_code({:compute, _, [pattern, [do: body]]}) do
    {:=, [], [pattern, body]}
  end

  # Feature check: if_feature :name do ... end
  defp generate_premise_code({:if_feature, _, [feature, [do: body]]}) do
    ctx_var = Macro.var(:ctx, nil)

    quote do
      if Deft.Context.feature_enabled?(unquote(ctx_var), unquote(feature)) do
        unquote(body)
      end
    end
  end

  # Scoped attribute read: var = scoped(:key)
  # Generates: var = Deft.Context.get_scoped(ctx, :key)
  defp generate_premise_code({:=, _, [var, {:scoped, _, [key]}]}) do
    ctx_var = Macro.var(:ctx, nil)

    quote do
      unquote(var) = Deft.Context.get_scoped(unquote(ctx_var), unquote(key))
    end
  end

  # Pattern judgment: pattern <~> expected_type >>> {erased, type, bindings}
  # Generates: {erased, type, bindings} = PatternMatching.handle_pattern(pattern, expected_type, ctx)
  defp generate_premise_code(
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
  defp generate_premise_code(
         {:>>>, _, [{:<~, _, [expr, expected_type]}, {erased_var, type_var}]}
       ) do
    generate_check_with_type_code(expr, expected_type, erased_var, type_var)
  end

  defp generate_premise_code(
         {:>>>, _, [{:<~, _, [expr, expected_type]}, {:{}, _, [erased_var, type_var]}]}
       ) do
    generate_check_with_type_code(expr, expected_type, erased_var, type_var)
  end

  # Checking: expr <~ expected_type >>> erased
  # Checks expression against expected type, binds erased form.
  defp generate_premise_code({:>>>, _, [{:<~, _, [expr, expected_type]}, erased_var]}) do
    generate_check_code(expr, expected_type, erased_var)
  end

  # Check all: exprs <<~ expected_types >>> erased_list
  # Checks list of expressions against list of expected types.
  defp generate_premise_code({:>>>, _, [{:<<~, _, [exprs, expected_types]}, erased_var]}) do
    generate_check_all_code(exprs, expected_types, erased_var)
  end

  # Plain assignment or pattern match (for destructuring, etc.)
  defp generate_premise_code({:=, _, [_left, _right]} = assignment) do
    assignment
  end

  # Any other expression (like function calls for side effects)
  defp generate_premise_code(other) do
    other
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

  # ============================================================================
  # Conclusion Code Generation
  # ============================================================================

  # Generate code for conclusion
  # conclude erased ~> type
  defp generate_conclusion_code({:conclude, _, [{:~>, _, [erased, type]}]}) do
    bindings_var = Macro.var(:bindings, nil)
    ctx_var = Macro.var(:ctx, nil)

    quote do
      {:ok, unquote(erased), unquote(type), unquote(bindings_var), unquote(ctx_var)}
    end
  end

  # conclude erased ~> type, bind: explicit_bindings
  defp generate_conclusion_code(
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
  defp generate_conclusion_code(
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
end

defmodule Deft.Rules.DSL.Operators do
  @moduledoc """
  Custom operators for the declarative typing rule DSL.

  ## Synthesis Operators

  - `~>` - Synthesis judgment: `expr ~> {erased, type}`
  - `~>>` - Synth all: `exprs ~>> {erased_list, types_list}`

  ## Checking Operators

  - `<~` - Checking judgment: `expr <~ expected >>> erased` or `expr <~ expected >>> {erased, type}`
  - `<<~` - Check all: `exprs <<~ type(s)` (homogeneous if single type, heterogeneous if list)

  ## Other Operators

  - `+++` - Context extension: `bindings +++ premise`
  - `&&&` - Scoped context: `(exprs &&& [k: v]) ~>> {erased, a, b}`
  - `<~>` - Pattern judgment: `pattern <~> type >>> {erased, type, bindings}`
  - `>>>` - Elaborates to (binds result of `<~`, `<~>`)
  """

  # These are just markers - the actual transformation happens in the DSL macro
  # We define them here so they're valid syntax when imported

  @doc "Synthesis: expr ~> {erased, type}"
  defmacro left ~> right do
    quote do: {:synth, unquote(left), unquote(right)}
  end

  @doc """
  Checking judgment: checks expression against expected type.

  Must be followed by `>>>` to bind the result:

      expr <~ expected_type >>> erased
      expr <~ expected_type >>> {erased, actual_type}
  """
  defmacro left <~ right do
    quote do: {:check, unquote(left), unquote(right)}
  end

  @doc "Elaborates to: binds result of `<~` or `<~>` judgment"
  defmacro left >>> right do
    quote do: {:elaborate, unquote(left), unquote(right)}
  end

  @doc "Context extension: bindings +++ premise"
  defmacro left +++ right do
    quote do: {:extend_ctx, unquote(left), unquote(right)}
  end

  @doc "Synth all: exprs ~>> {erased_list, types_list}"
  defmacro left ~>> right do
    quote do: {:synth_all, unquote(left), unquote(right)}
  end

  @doc """
  Check all: checks list of expressions against expected type(s).

  Two modes based on the right-hand side:
  - **Homogeneous** (single type): `exprs <<~ type` - all expressions checked against same type
  - **Heterogeneous** (list of types): `exprs <<~ types` - each expression checked against corresponding type

  Must be followed by `>>>` to bind the result:

      # Homogeneous: check all list elements are integers
      elements <<~ Type.integer() >>> elements_e

      # Heterogeneous: check function args against parameter types
      args <<~ input_types >>> args_e
  """
  defmacro left <<~ right do
    quote do: {:check_all, unquote(left), unquote(right)}
  end

  @doc """
  Scoped context: adds scoped attributes for child rules.

  Used in conjunction with ~>> to pass context from parent rules to child rules.
  For example, a :case rule can pass subj_t to :case_branch rules:

      (branches &&& [subj_t: subj_t]) ~>> {branches_e, pat_ts, body_ts}

  Note: Due to operator precedence, parentheses are required around `exprs &&& [...]`.
  """
  defmacro left &&& right do
    quote do: {:scoped_ctx, unquote(left), unquote(right)}
  end

  @doc """
  Pattern judgment: pattern <~> expected_type checks pattern against type.

  Returns erased pattern, pattern type, and bindings produced by pattern matching.
  Must be followed by >>> to bind results:

      pattern <~> subject_type >>> {erased_pattern, pattern_type, pattern_bindings}
  """
  defmacro left <~> right do
    quote do: {:pattern_judgment, unquote(left), unquote(right)}
  end
end

defmodule Deft.Rules.DSL.Helpers do
  @moduledoc """
  Helper functions available within defrule blocks.
  """

  alias Deft.AST
  alias Deft.Context
  alias Deft.Error
  alias Deft.Subtyping
  alias Deft.Type
  alias Deft.TypeChecker
  alias Deft.Helpers, as: DeftHelpers

  @doc """
  Synthesizes a type for an expression with bindings injected.
  Returns {erased, type, new_bindings}.
  """
  def synth!(expr, bindings, ctx) do
    expr_with_bindings = DeftHelpers.inject_bindings(expr, bindings)

    case TypeChecker.check(expr_with_bindings, ctx) do
      {:ok, erased, type, new_bindings, _ctx} ->
        {erased, type, new_bindings}

      {:error, {:no_matching_rule, ast}} ->
        error =
          Error.no_matching_rule(
            ast: ast,
            location: Error.extract_location(ast)
          )

        raise_or_accumulate(ctx, error)

      {:error, reason} ->
        raise "Synthesis failed: #{inspect(reason)}"
    end
  end

  @doc """
  Checks an expression against an expected type with bindings injected.

  Synthesizes the expression's type, then verifies it is a subtype of the
  expected type. Returns {erased, actual_type, new_bindings}.
  """
  def check!(expr, expected_type, bindings, ctx) do
    # First synthesize the expression's type
    {erased, actual_type, new_bindings} = synth!(expr, bindings, ctx)

    # Then verify it's a subtype of expected
    unless Subtyping.subtype_of?(expected_type, actual_type) do
      error =
        Error.type_mismatch(
          expected: expected_type,
          actual: actual_type,
          expression: expr,
          location: Error.extract_location(expr)
        )

      raise_or_accumulate(ctx, error)
    end

    {erased, actual_type, new_bindings}
  end

  @doc """
  Asserts that actual is a subtype of expected.
  Raises a TypecheckingError if the assertion fails.
  """
  def require_subtype!(actual, expected) do
    require_subtype!(actual, expected, nil, nil)
  end

  @doc """
  Asserts that actual is a subtype of expected with context.

  Uses detailed subtype checking to provide element-specific error messages
  for compound types like tuples.
  """
  def require_subtype!(actual, expected, expr, ctx) do
    # Extract sub-expressions for element-level span tracking.
    sub_exprs = extract_sub_exprs(expr)

    case Subtyping.check_subtype(expected, actual, sub_exprs) do
      :ok ->
        :ok

      {:mismatch, %{expected: exp_elem, actual: act_elem, expr: fail_expr}} ->
        # Use the failing element's expression for location if available.
        location_expr = fail_expr || expr

        error =
          Error.type_mismatch(
            expected: exp_elem,
            actual: act_elem,
            expression: location_expr,
            location: if(location_expr, do: Error.extract_location(location_expr), else: nil)
          )

        if ctx do
          raise_or_accumulate(ctx, error)
        else
          Error.raise!(error)
        end
    end
  end

  @doc """
  Extracts child expressions from an AST node for element-level span tracking.

  Returns a list of sub-expressions for compound types, or nil for simple types.
  """
  def extract_sub_exprs(%AST.Tuple{elements: elements}), do: elements
  def extract_sub_exprs(%AST.TypeConstructorCall{args: args}), do: args
  def extract_sub_exprs(%AST.List{elements: elements}), do: elements
  def extract_sub_exprs(%AST.Pair{fst: fst, snd: snd}), do: [fst, snd]
  def extract_sub_exprs(_), do: nil

  # Handles error based on context's error_mode.
  defp raise_or_accumulate(%Context{error_mode: :accumulate} = ctx, error) do
    # In accumulate mode, store errors in process dictionary since context
    # isn't threaded back through the rule system.
    enriched = enrich_error(error, ctx)
    errors = Process.get(:deft_accumulated_errors, [])
    Process.put(:deft_accumulated_errors, errors ++ [enriched])
    {nil, Type.bottom(), []}
  end

  defp raise_or_accumulate(_ctx, %Error{} = error) do
    # In fail_fast mode or no context, raise using the new error system.
    Error.raise!(error)
  end

  # Enriches an error with context information (file, etc.)
  defp enrich_error(%Error{location: nil} = error, %Context{env: env}) when not is_nil(env) do
    %{error | location: Error.extract_location_from_env(env)}
  end

  # If location has line but no file, add file from env.
  defp enrich_error(
         %Error{location: {nil, line, col}} = error,
         %Context{env: %Macro.Env{file: file}}
       )
       when not is_nil(file) do
    %{error | location: {file, line, col}}
  end

  defp enrich_error(error, _ctx), do: error

  @doc """
  Computes the type of a literal value.
  """
  def type_of_literal(v) when is_boolean(v), do: Type.boolean()
  def type_of_literal(v) when is_atom(v), do: Type.atom()
  def type_of_literal(v) when is_binary(v), do: Type.binary()
  def type_of_literal(v) when is_integer(v), do: Type.integer()
  def type_of_literal(v) when is_float(v), do: Type.float()

  @doc """
  Unions a list of types together.
  """
  def union_types([]), do: Type.bottom()
  def union_types([t]), do: t
  def union_types([t | rest]), do: Type.union(t, union_types(rest))

  @doc """
  Checks if a feature is enabled in the context.
  """
  def feature_enabled?(ctx, feature), do: Context.feature_enabled?(ctx, feature)

  @doc """
  Synthesizes types for a list of expressions.
  Returns {erased_list, types_list, bindings_list}.
  """
  def synth_all!(exprs, ctx) do
    {erased_list, types_list, bindings_list} =
      Enum.reduce(exprs, {[], [], []}, fn expr, {acc_erased, acc_types, acc_bindings} ->
        {erased, type, bindings} = synth!(expr, acc_bindings, ctx)
        {acc_erased ++ [erased], acc_types ++ [type], acc_bindings ++ bindings}
      end)

    {erased_list, types_list, bindings_list}
  end

  @doc """
  Synthesizes types for a list of expressions with a custom context.

  This is used by the `&&&` operator to pass scoped attributes to child rules.
  Returns {erased_list, types_list, bindings_list}.
  """
  def synth_all_with_ctx!(exprs, bindings, ctx) do
    {erased_list, types_list, bindings_list} =
      Enum.reduce(exprs, {[], [], bindings}, fn expr, {acc_erased, acc_types, acc_bindings} ->
        {erased, type, new_bindings} = synth!(expr, acc_bindings, ctx)
        {acc_erased ++ [erased], acc_types ++ [type], acc_bindings ++ new_bindings}
      end)

    {erased_list, types_list, bindings_list}
  end

  @doc """
  Checks a list of expressions against expected type(s).

  Two modes based on the second argument:
  - **Homogeneous** (single type): Each expression is checked against the same type
  - **Heterogeneous** (list of types): Each expression is checked against its corresponding type

  Returns {erased_list, bindings_list}.

  ## Examples

      # Homogeneous: all elements must be integers
      check_all!(elements, Type.integer(), bindings, ctx)

      # Heterogeneous: check args[i] against param_types[i]
      check_all!(args, param_types, bindings, ctx)
  """
  def check_all!(exprs, expected, bindings, ctx) when is_list(expected) do
    # Heterogeneous mode: check each expr against corresponding type
    if length(exprs) != length(expected) do
      # Use type_mismatch with tuple types to show arity difference
      error =
        Error.type_mismatch(
          expected: Type.fixed_tuple(expected),
          actual: Type.fixed_tuple(List.duplicate(Type.top(), length(exprs))),
          notes: [
            "Expected #{length(expected)} argument(s), got #{length(exprs)}"
          ]
        )

      Error.raise!(error, ctx)
    end

    {erased_list, bindings_list} =
      exprs
      |> Enum.zip(expected)
      |> Enum.reduce({[], bindings}, fn {expr, expected_type}, {acc_erased, acc_bindings} ->
        {erased, _type, new_bindings} = check!(expr, expected_type, acc_bindings, ctx)
        {acc_erased ++ [erased], acc_bindings ++ new_bindings}
      end)

    {erased_list, bindings_list}
  end

  def check_all!(exprs, expected_type, bindings, ctx) do
    # Homogeneous mode: check all exprs against the same type
    {erased_list, bindings_list} =
      Enum.reduce(exprs, {[], bindings}, fn expr, {acc_erased, acc_bindings} ->
        {erased, _type, new_bindings} = check!(expr, expected_type, acc_bindings, ctx)
        {acc_erased ++ [erased], acc_bindings ++ new_bindings}
      end)

    {erased_list, bindings_list}
  end

end
