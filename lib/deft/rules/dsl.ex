defmodule Deft.Rules.DSL do
  @moduledoc """
  Declarative DSL for defining typing rules in the style of Turnstile.

  This module provides a declarative syntax for typing rules using
  judgment forms that closely mirror how typing rules are written
  in type theory.

  ## Judgment Forms

  - `expr ~> {erased, type}` - Synthesis: infer type of expr
  - `expr <~ expected >>> erased` - Checking: check expr against expected type
  - `actual <<< expected` - Subtyping: assert actual is subtype of expected
  - `bindings +++ premise` - Context extension: add bindings for premise

  ## Example

      defrule :literal, %AST.Literal{value: value} do
        conclude value ~> type_of_literal(value)
      end

      defrule :fn, %AST.Fn{args: args, body: body, fn_meta: fn_meta, arrow_meta: arrow_meta} do
        args ~> {erased_args, input_types}
        body ~> {erased_body, output_type}

        conclude {:fn, fn_meta, [{:->, arrow_meta, [erased_args, erased_body]}]}
              ~> Type.fun(input_types, output_type)
      end

  ## Binding Flow

  Bindings from each premise automatically flow to subsequent premises,
  following Turnstile's lexically-scoped binding semantics.

  ## Escape Hatch

  For complex logic that doesn't fit the declarative model, use `compute`:

      compute {result, type} do
        # arbitrary Elixir code
        Enum.map(items, fn item -> ... end)
      end
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

        @required_features unquote(requires)

        @impl true
        def matches?(unquote(matches_pattern)), do: true
        def matches?(_), do: false

        @impl true
        def apply(unquote(pattern) = _ast, _expected, var!(ctx, nil)) do
          # Initialize bindings accumulator
          var!(bindings, nil) = []

          if features_enabled?(var!(ctx, nil), @required_features) do
            # Execute premises
            unquote_splicing(premises_code)

            # Execute conclusion
            unquote(conclusion_code)
          else
            {:error, {:missing_features, @required_features}}
          end
        end

        defp features_enabled?(_ctx, []), do: true

        defp features_enabled?(ctx, features) do
          Enum.all?(features, &Context.feature_enabled?(ctx, &1))
        end
      end

      @deft_rules @__rule_module_name__
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

  # Synthesis with pattern match on type: expr ~> {erased, %Type.Fn{...} = type_var}
  defp generate_premise_code({:~>, _, [expr, {:=, _, [type_pattern, {erased_var, type_var}]}]}) do
    synth_code = generate_synth_code(expr, erased_var, type_var)
    pattern_match = {:=, [], [type_pattern, type_var]}
    {:__block__, [], [synth_code, pattern_match]}
  end

  # Synthesis: {:synth, expr, {erased, type}} (expanded form from operator macro)
  defp generate_premise_code({:synth, expr, {erased_var, type_var}}) do
    generate_synth_code(expr, erased_var, type_var)
  end

  # Checking: expr <~ expected >>> erased (unexpanded form)
  defp generate_premise_code({:>>>, _, [{:<~, _, [expr, expected]}, erased_var]}) do
    generate_check_code(expr, expected, erased_var)
  end

  # Checking: {:elaborate, {:check, expr, expected}, erased} (expanded form)
  defp generate_premise_code({:elaborate, {:check, expr, expected}, erased_var}) do
    generate_check_code(expr, expected, erased_var)
  end

  # Checking without erased binding: expr <~ expected (unexpanded form)
  defp generate_premise_code({:<~, _, [expr, expected]}) do
    generate_check_no_erased_code(expr, expected)
  end

  # Checking without erased: {:check, expr, expected} (expanded form)
  defp generate_premise_code({:check, expr, expected}) do
    generate_check_no_erased_code(expr, expected)
  end

  # Subtyping: actual <<< expected (unexpanded form)
  defp generate_premise_code({:<<<, _, [actual, expected]}) do
    generate_subtype_code(actual, expected)
  end

  # Subtyping: {:subtype, actual, expected} (expanded form)
  defp generate_premise_code({:subtype, actual, expected}) do
    generate_subtype_code(actual, expected)
  end

  # Synth all: exprs ~>> {erased_list, types_list} (unexpanded form)
  defp generate_premise_code({:~>>, _, [exprs, {erased_var, types_var}]}) do
    generate_synth_all_code(exprs, erased_var, types_var)
  end

  # Synth all: {:synth_all, exprs, {erased_list, types_list}} (expanded form)
  defp generate_premise_code({:synth_all, exprs, {erased_var, types_var}}) do
    generate_synth_all_code(exprs, erased_var, types_var)
  end

  # Check all with erased binding: exprs <<~ expected_types >>> erased_list (unexpanded form)
  defp generate_premise_code({:>>>, _, [{:<<~, _, [exprs, expected]}, erased_var]}) do
    generate_check_all_code(exprs, expected, erased_var)
  end

  # Check all with erased binding: {:elaborate, {:check_all, exprs, expected}, erased} (expanded form)
  defp generate_premise_code({:elaborate, {:check_all, exprs, expected}, erased_var}) do
    generate_check_all_code(exprs, expected, erased_var)
  end

  # Context extension: extra_bindings +++ premise (unexpanded form)
  defp generate_premise_code({:+++, _, [extra_bindings, premise]}) do
    generate_context_extension_code(extra_bindings, premise)
  end

  # Context extension: {:extend_ctx, bindings, premise} (expanded form from operator macro)
  defp generate_premise_code({:extend_ctx, extra_bindings, premise}) do
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

  defp generate_check_code(expr, expected, erased_var) do
    new_bindings_var = Macro.var(:__new_bindings__, __MODULE__)
    bindings_var = Macro.var(:bindings, nil)
    ctx_var = Macro.var(:ctx, nil)

    quote do
      {unquote(erased_var), _, unquote(new_bindings_var)} =
        Deft.Rules.DSL.Helpers.check!(
          unquote(expr),
          unquote(expected),
          unquote(bindings_var),
          unquote(ctx_var)
        )

      unquote(bindings_var) = unquote(bindings_var) ++ unquote(new_bindings_var)
    end
  end

  defp generate_check_no_erased_code(expr, expected) do
    new_bindings_var = Macro.var(:__new_bindings__, __MODULE__)
    bindings_var = Macro.var(:bindings, nil)
    ctx_var = Macro.var(:ctx, nil)

    quote do
      {_, _, unquote(new_bindings_var)} =
        Deft.Rules.DSL.Helpers.check!(
          unquote(expr),
          unquote(expected),
          unquote(bindings_var),
          unquote(ctx_var)
        )

      unquote(bindings_var) = unquote(bindings_var) ++ unquote(new_bindings_var)
    end
  end

  defp generate_subtype_code(actual, expected) do
    quote do
      Deft.Rules.DSL.Helpers.require_subtype!(unquote(actual), unquote(expected))
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

  defp generate_check_all_code(exprs, expected, erased_var) do
    new_bindings_var = Macro.var(:__new_bindings__, __MODULE__)
    bindings_var = Macro.var(:bindings, nil)
    ctx_var = Macro.var(:ctx, nil)

    quote do
      {unquote(erased_var), unquote(new_bindings_var)} =
        Deft.Rules.DSL.Helpers.check_all!(
          unquote(exprs),
          unquote(expected),
          unquote(bindings_var),
          unquote(ctx_var)
        )

      unquote(bindings_var) = unquote(bindings_var) ++ unquote(new_bindings_var)
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

  These operators are used in premise and conclusion forms:
  - `~>` - Synthesis judgment (single)
  - `~>>` - Synthesis judgment (all)
  - `<~` - Checking judgment (single)
  - `<<~` - Checking judgment (all)
  - `>>>` - Elaborates to (binds erased form)
  - `<<<` - Subtyping assertion
  - `+++` - Context extension
  """

  # These are just markers - the actual transformation happens in the DSL macro
  # We define them here so they're valid syntax when imported

  @doc "Synthesis: expr ~> {erased, type}"
  defmacro left ~> right do
    quote do: {:synth, unquote(left), unquote(right)}
  end

  @doc "Checking: expr <~ expected"
  defmacro left <~ right do
    quote do: {:check, unquote(left), unquote(right)}
  end

  @doc "Elaborates to: (expr <~ type) >>> erased"
  defmacro left >>> right do
    quote do: {:elaborate, unquote(left), unquote(right)}
  end

  @doc "Subtyping: actual <<< expected"
  defmacro left <<< right do
    quote do: {:subtype, unquote(left), unquote(right)}
  end

  @doc "Context extension: bindings +++ premise"
  defmacro left +++ right do
    quote do: {:extend_ctx, unquote(left), unquote(right)}
  end

  @doc "Synth all: exprs ~>> {erased_list, types_list}"
  defmacro left ~>> right do
    quote do: {:synth_all, unquote(left), unquote(right)}
  end

  @doc "Check all: exprs <<~ expected_types"
  defmacro left <<~ right do
    quote do: {:check_all, unquote(left), unquote(right)}
  end
end

defmodule Deft.Rules.DSL.Helpers do
  @moduledoc """
  Helper functions available within defrule blocks.
  """

  alias Deft.Context
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

      {:error, reason} ->
        raise "Synthesis failed: #{inspect(reason)}"
    end
  end

  @doc """
  Checks an expression against an expected type with bindings injected.
  Returns {erased, type, new_bindings}.
  """
  def check!(expr, expected_type, bindings, ctx) do
    expr_with_bindings = DeftHelpers.inject_bindings(expr, bindings)

    case TypeChecker.check_against(expr_with_bindings, expected_type, ctx) do
      {:ok, erased, type, new_bindings, _ctx} ->
        {erased, type, new_bindings}

      {:error, reason} ->
        raise "Check failed: #{inspect(reason)}"
    end
  end

  @doc """
  Asserts that actual is a subtype of expected.
  """
  def require_subtype!(actual, expected) do
    unless Subtyping.subtype_of?(expected, actual) do
      raise Deft.TypecheckingError, expected: expected, actual: actual
    end
  end

  @doc """
  Computes the type of a literal value.
  """
  def type_of_literal(v) when is_boolean(v), do: Type.boolean()
  def type_of_literal(v) when is_atom(v), do: Type.atom()
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
  Checks a list of expressions against expected types.
  Returns {erased_list, bindings_list}.
  """
  def check_all!(exprs, expected_types, bindings, ctx) do
    if length(exprs) != length(expected_types) do
      raise "check_all!: mismatched lengths - #{length(exprs)} exprs vs #{length(expected_types)} types"
    end

    {erased_list, bindings_list} =
      Enum.zip(exprs, expected_types)
      |> Enum.reduce({[], bindings}, fn {expr, expected}, {acc_erased, acc_bindings} ->
        {erased, _type, new_bindings} = check!(expr, expected, acc_bindings, ctx)
        {acc_erased ++ [erased], acc_bindings ++ new_bindings}
      end)

    {erased_list, bindings_list}
  end
end
