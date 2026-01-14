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

  alias Deft.Rules.DSL.Codegen

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
    premises_code = Codegen.generate_premises_code(premises)

    # Generate the conclusion code
    conclusion_code = Codegen.generate_conclusion_code(conclusion)

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

  # ============================================================================
  # Apply Body Generation
  # ============================================================================

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

  # ============================================================================
  # AST Transformation Helpers
  # ============================================================================

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

  # ============================================================================
  # Rule Block Parsing
  # ============================================================================

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
end
