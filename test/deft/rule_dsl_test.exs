defmodule Deft.RulesDSLTest do
  use ExUnit.Case

  alias Deft.AST
  alias Deft.Context
  alias Deft.Type

  # ============================================================================
  # Basic defrule Macro
  # ============================================================================

  describe "defrule macro with declarative syntax" do
    test "defines a rule with the correct name" do
      defmodule TestNameRule do
        use Deft.Rules.DSL

        defrule :test_name, %{test: _} do
          conclude(:ok ~> Type.atom())
        end
      end

      assert apply(TestNameRule.Rule_test_name, :name, []) == :test_name
    end

    test "defines a rule with synth judgment by default" do
      defmodule TestJudgmentRule do
        use Deft.Rules.DSL

        defrule :synth_rule, %{synth: _} do
          conclude(:ok ~> Type.atom())
        end
      end

      assert apply(TestJudgmentRule.Rule_synth_rule, :judgment, []) == :synth
    end

    test "matches? returns true for matching patterns" do
      defmodule TestMatchRule do
        use Deft.Rules.DSL

        defrule :match_test, %{value: 42} do
          conclude(:ok ~> Type.integer())
        end
      end

      assert TestMatchRule.Rule_match_test.matches?(%{value: 42})
      refute TestMatchRule.Rule_match_test.matches?(%{value: 43})
      refute TestMatchRule.Rule_match_test.matches?(%{other: 42})
    end

    test "apply returns successful result" do
      defmodule TestApplyRule do
        use Deft.Rules.DSL

        defrule :apply_test, %{value: value} do
          conclude(value ~> Type.integer())
        end
      end

      ctx = Context.new(__ENV__)

      {:ok, erased, type, bindings, _ctx} =
        apply(TestApplyRule.Rule_apply_test, :apply, [%{value: 42}, nil, ctx])

      assert erased == 42
      assert %Type.Integer{} = type
      assert bindings == []
    end

    test "rules/0 returns all defined rules" do
      defmodule TestRulesCollection do
        use Deft.Rules.DSL

        defrule :first_rule, %{first: _} do
          conclude(:first ~> Type.atom())
        end

        defrule :second_rule, %{second: _} do
          conclude(:second ~> Type.atom())
        end
      end

      rules = TestRulesCollection.rules()
      assert length(rules) == 2
      assert TestRulesCollection.Rule_first_rule in rules
      assert TestRulesCollection.Rule_second_rule in rules
    end
  end

  # ============================================================================
  # Synth All Operator (~>>)
  # ============================================================================

  describe "synth all operator (~>>)" do
    test "synthesizes types for list of literals" do
      defmodule TestSynthAllRule do
        use Deft.Rules.DSL

        defrule :synth_all_test, %{elements: elems} do
          elems ~>> {elems_e, elems_ts}

          conclude({:tuple, elems_e} ~> Type.fixed_tuple(elems_ts))
        end
      end

      ctx = Context.new(__ENV__)
      ast = %{elements: [%AST.Literal{value: 1, meta: []}, %AST.Literal{value: :atom, meta: []}]}

      {:ok, {:tuple, [1, :atom]}, type, _, _} =
        apply(TestSynthAllRule.Rule_synth_all_test, :apply, [ast, nil, ctx])

      assert %Type.FixedTuple{elements: [%Type.Integer{}, %Type.Atom{}]} = type
    end

    test "3-element binding captures explicit bindings" do
      defmodule TestSynthAllBindingsRule do
        use Deft.Rules.DSL

        defrule :synth_all_bindings_test, %{annotations: annots, body: body} do
          # 3-element binding: {erased, types, bindings}
          annots ~>> {annots_e, input_ts, arg_bs}

          # Use bindings in body synthesis
          (arg_bs +++ body) ~> {body_e, body_t}

          conclude({:fn, annots_e, body_e} ~> Type.fun(input_ts, body_t))
        end
      end

      ctx = Context.new(__ENV__)

      # Create annotation that produces a binding
      x_local = %AST.Local{name: :x, meta: [], context: nil}
      x_annot = %AST.Annotation{pattern: x_local, type: Type.integer(), meta: []}

      # Body references x
      body = %AST.Local{name: :x, meta: [], context: nil}

      ast = %{annotations: [x_annot], body: body}

      {:ok, {:fn, [_], _}, type, _, _} =
        apply(TestSynthAllBindingsRule.Rule_synth_all_bindings_test, :apply, [ast, nil, ctx])

      assert %Type.Fn{inputs: [%Type.Integer{}], output: %Type.Integer{}} = type
    end
  end

  # ============================================================================
  # Context Extension Operator (+++)
  # ============================================================================

  describe "context extension operator (+++)" do
    test "extends context with bindings for nested synthesis" do
      defmodule TestContextExtRule do
        use Deft.Rules.DSL

        defrule :context_ext_test, %{binding: binding, body: body} do
          bs = [{binding, Type.integer()}]
          (bs +++ body) ~> {body_e, body_t}

          conclude(body_e ~> body_t)
        end
      end

      ctx = Context.new(__ENV__)
      binding = %AST.Local{name: :x, meta: [], context: nil}
      body = %AST.Local{name: :x, meta: [], context: nil}
      ast = %{binding: binding, body: body}

      {:ok, {:x, _, nil}, %Type.Integer{}, [], _} =
        apply(TestContextExtRule.Rule_context_ext_test, :apply, [ast, nil, ctx])
    end
  end

  # ============================================================================
  # Compute Escape Hatch
  # ============================================================================

  describe "compute escape hatch" do
    test "allows arbitrary computation" do
      defmodule TestComputeRule do
        use Deft.Rules.DSL

        defrule :compute_test, %{values: values} do
          compute result do
            Enum.sum(values)
          end

          conclude(result ~> Type.integer())
        end
      end

      ctx = Context.new(__ENV__)

      {:ok, 6, %Type.Integer{}, [], _} =
        apply(TestComputeRule.Rule_compute_test, :apply, [%{values: [1, 2, 3]}, nil, ctx])
    end

    test "compute can destructure results" do
      defmodule TestComputeDestructureRule do
        use Deft.Rules.DSL

        defrule :compute_destructure, %{pair: {a, b}} do
          compute {sum, product} do
            {a + b, a * b}
          end

          conclude({sum, product} ~> Type.fixed_tuple([Type.integer(), Type.integer()]))
        end
      end

      ctx = Context.new(__ENV__)

      {:ok, {7, 12}, type, [], _} =
        apply(TestComputeDestructureRule.Rule_compute_destructure, :apply, [
          %{pair: {3, 4}},
          nil,
          ctx
        ])

      assert %Type.FixedTuple{elements: [%Type.Integer{}, %Type.Integer{}]} = type
    end
  end

  # ============================================================================
  # if_feature Conditional
  # ============================================================================

  describe "if_feature conditional" do
    test "executes block when feature is enabled" do
      defmodule TestIfFeatureEnabledRule do
        use Deft.Rules.DSL

        defrule :if_feature_enabled, %{value: value} do
          if_feature :test_feature do
            Process.put(:feature_executed, true)
          end

          conclude(value ~> Type.integer())
        end
      end

      Process.delete(:feature_executed)
      ctx = Context.new(__ENV__, features: [:test_feature])

      {:ok, 42, _, _, _} =
        apply(TestIfFeatureEnabledRule.Rule_if_feature_enabled, :apply, [%{value: 42}, nil, ctx])

      assert Process.get(:feature_executed) == true
    end

    test "skips block when feature is disabled" do
      defmodule TestIfFeatureDisabledRule do
        use Deft.Rules.DSL

        defrule :if_feature_disabled, %{value: value} do
          if_feature :missing_feature do
            Process.put(:feature_executed, true)
          end

          conclude(value ~> Type.integer())
        end
      end

      Process.delete(:feature_executed)
      ctx = Context.new(__ENV__, features: [])

      {:ok, 42, _, _, _} =
        apply(TestIfFeatureDisabledRule.Rule_if_feature_disabled, :apply, [
          %{value: 42},
          nil,
          ctx
        ])

      assert Process.get(:feature_executed) == nil
    end
  end

  # ============================================================================
  # Scoped Context (&&&) and scoped() Helper
  # ============================================================================

  describe "scoped context operator (&&&) and scoped() helper" do
    test "passes scoped attributes to child rules" do
      # This test verifies the mechanism works by testing the Context functions directly
      ctx = Context.new(__ENV__)
      scoped_ctx = Deft.Context.with_scoped(ctx, subj_t: Type.integer())

      assert Deft.Context.get_scoped(scoped_ctx, :subj_t) == Type.integer()
      assert Deft.Context.get_scoped(scoped_ctx, :missing) == nil
    end
  end

  # ============================================================================
  # Explicit Bindings (conclude with bind:)
  # ============================================================================

  describe "conclude with explicit bindings" do
    test "bind: option overrides accumulated bindings" do
      defmodule TestExplicitBindRule do
        use Deft.Rules.DSL

        defrule :explicit_bind, %{value: value, binding: binding} do
          explicit_bs = [{binding, Type.atom()}]

          conclude(value ~> Type.integer(), bind: explicit_bs)
        end
      end

      ctx = Context.new(__ENV__)
      binding = %AST.Local{name: :y, meta: [], context: nil}

      {:ok, 42, %Type.Integer{}, bindings, _} =
        apply(TestExplicitBindRule.Rule_explicit_bind, :apply, [
          %{value: 42, binding: binding},
          nil,
          ctx
        ])

      assert [{^binding, %Type.Atom{}}] = bindings
    end
  end

  # ============================================================================
  # Pattern Judgment (<~> and >>>)
  # ============================================================================

  describe "pattern judgment operator (<~> >>>)" do
    test "pattern judgment produces bindings" do
      # Test that PatternMatching.handle_pattern is called correctly
      # by using a local variable pattern
      defmodule TestPatternJudgmentRule do
        use Deft.Rules.DSL

        defrule :pattern_judgment, %{pattern: pat, expected_type: exp_t} do
          pat <~> exp_t >>> {pat_e, pat_t, pat_bs}

          conclude(pat_e ~> pat_t, bind: pat_bs)
        end
      end

      ctx = Context.new(__ENV__)
      pattern = %AST.Local{name: :x, meta: [], context: nil}

      {:ok, {:x, _, nil}, %Type.Integer{}, bindings, _} =
        apply(TestPatternJudgmentRule.Rule_pattern_judgment, :apply, [
          %{pattern: pattern, expected_type: Type.integer()},
          nil,
          ctx
        ])

      # Pattern matching on a local produces a binding
      assert [{%AST.Local{name: :x}, %Type.Integer{}}] = bindings
    end
  end

  # ============================================================================
  # Checking Operator (<~ >>>)
  # ============================================================================

  describe "checking operator (<~ >>>)" do
    test "checks expression against expected type and binds erased form" do
      defmodule TestCheckRule do
        use Deft.Rules.DSL

        defrule :check_test, %{expr: expr, expected: expected} do
          expr <~ expected >>> expr_e

          conclude(expr_e ~> expected)
        end
      end

      ctx = Context.new(__ENV__)
      expr = %AST.Literal{value: 42, meta: []}

      {:ok, 42, %Type.Number{}, [], _} =
        apply(TestCheckRule.Rule_check_test, :apply, [
          %{expr: expr, expected: Type.number()},
          nil,
          ctx
        ])
    end

    test "checks expression and binds both erased form and actual type" do
      defmodule TestCheckWithTypeRule do
        use Deft.Rules.DSL

        defrule :check_with_type, %{expr: expr, expected: expected} do
          expr <~ expected >>> {expr_e, actual_t}

          conclude(expr_e ~> actual_t)
        end
      end

      ctx = Context.new(__ENV__)
      expr = %AST.Literal{value: 42, meta: []}

      # Integer is subtype of Number, so checking against Number succeeds
      # and actual_t is Integer
      {:ok, 42, %Type.Integer{}, [], _} =
        apply(TestCheckWithTypeRule.Rule_check_with_type, :apply, [
          %{expr: expr, expected: Type.number()},
          nil,
          ctx
        ])
    end

    test "raises when expression type is not subtype of expected" do
      defmodule TestCheckFailRule do
        use Deft.Rules.DSL

        defrule :check_fail, %{expr: expr, expected: expected} do
          expr <~ expected >>> expr_e

          conclude(expr_e ~> expected)
        end
      end

      ctx = Context.new(__ENV__)
      expr = %AST.Literal{value: :atom, meta: []}

      assert_raise CompileError, fn ->
        apply(TestCheckFailRule.Rule_check_fail, :apply, [
          %{expr: expr, expected: Type.integer()},
          nil,
          ctx
        ])
      end
    end
  end

  # ============================================================================
  # Check All Operator (<<~ >>>)
  # ============================================================================

  describe "check all operator (<<~ >>>)" do
    test "homogeneous mode: checks all expressions against single type" do
      defmodule TestCheckAllHomogRule do
        use Deft.Rules.DSL

        defrule :check_all_homog, %{exprs: exprs, expected: expected} do
          exprs <<~ expected >>> exprs_e

          conclude(exprs_e ~> Type.fixed_list(expected))
        end
      end

      ctx = Context.new(__ENV__)
      exprs = [%AST.Literal{value: 42, meta: []}, %AST.Literal{value: 3.14, meta: []}]

      {:ok, [42, 3.14], %Type.FixedList{}, [], _} =
        apply(TestCheckAllHomogRule.Rule_check_all_homog, :apply, [
          %{exprs: exprs, expected: Type.number()},
          nil,
          ctx
        ])
    end

    test "heterogeneous mode: checks expressions against corresponding types" do
      defmodule TestCheckAllHeteroRule do
        use Deft.Rules.DSL

        defrule :check_all_hetero, %{exprs: exprs, types: types} do
          exprs <<~ types >>> exprs_e

          conclude(exprs_e ~> Type.fixed_tuple(types))
        end
      end

      ctx = Context.new(__ENV__)
      exprs = [%AST.Literal{value: 42, meta: []}, %AST.Literal{value: :ok, meta: []}]
      types = [Type.integer(), Type.atom()]

      {:ok, [42, :ok], %Type.FixedTuple{}, [], _} =
        apply(TestCheckAllHeteroRule.Rule_check_all_hetero, :apply, [
          %{exprs: exprs, types: types},
          nil,
          ctx
        ])
    end

    test "homogeneous mode: raises when any expression fails" do
      defmodule TestCheckAllHomogFailRule do
        use Deft.Rules.DSL

        defrule :check_all_homog_fail, %{exprs: exprs, expected: expected} do
          exprs <<~ expected >>> exprs_e

          conclude(exprs_e ~> Type.fixed_list(expected))
        end
      end

      ctx = Context.new(__ENV__)
      exprs = [%AST.Literal{value: 42, meta: []}, %AST.Literal{value: :atom, meta: []}]

      assert_raise CompileError, fn ->
        apply(TestCheckAllHomogFailRule.Rule_check_all_homog_fail, :apply, [
          %{exprs: exprs, expected: Type.integer()},
          nil,
          ctx
        ])
      end
    end

    test "heterogeneous mode: raises when any expression fails its type" do
      defmodule TestCheckAllHeteroFailRule do
        use Deft.Rules.DSL

        defrule :check_all_hetero_fail, %{exprs: exprs, types: types} do
          exprs <<~ types >>> exprs_e

          conclude(exprs_e ~> Type.fixed_tuple(types))
        end
      end

      ctx = Context.new(__ENV__)
      exprs = [%AST.Literal{value: 42, meta: []}, %AST.Literal{value: :atom, meta: []}]
      types = [Type.integer(), Type.integer()]  # second should fail

      assert_raise CompileError, fn ->
        apply(TestCheckAllHeteroFailRule.Rule_check_all_hetero_fail, :apply, [
          %{exprs: exprs, types: types},
          nil,
          ctx
        ])
      end
    end
  end

  # ============================================================================
  # DSL Helpers
  # ============================================================================

  describe "DSL helpers" do
    test "type_of_literal returns correct types" do
      import Deft.Rules.DSL.Helpers

      assert %Type.Boolean{} = type_of_literal(true)
      assert %Type.Boolean{} = type_of_literal(false)
      assert %Type.Atom{} = type_of_literal(:hello)
      assert %Type.Integer{} = type_of_literal(42)
      assert %Type.Float{} = type_of_literal(3.14)
    end

    test "union_types combines types correctly" do
      import Deft.Rules.DSL.Helpers

      assert %Type.Bottom{} = union_types([])
      assert %Type.Integer{} = union_types([Type.integer()])
      assert %Type.Union{} = union_types([Type.integer(), Type.boolean()])
    end
  end

  describe "feature flags in rules" do
    test "rule with required features fails if feature not enabled" do
      defmodule TestFeatureRule do
        use Deft.Rules.DSL

        defrule :needs_feature, %{featured: _} do
          conclude(:ok ~> Type.atom())

          requires([:special_feature])
        end
      end

      ctx = Context.new(__ENV__, features: [])
      result = apply(TestFeatureRule.Rule_needs_feature, :apply, [%{featured: true}, nil, ctx])

      assert {:error, {:missing_features, [:special_feature]}} = result
    end

    test "rule with required features succeeds if feature enabled" do
      defmodule TestFeatureEnabledRule do
        use Deft.Rules.DSL

        defrule :needs_feature, %{featured: _} do
          conclude(:ok ~> Type.atom())

          requires([:special_feature])
        end
      end

      ctx = Context.new(__ENV__, features: [:special_feature])

      {:ok, erased, _type, _bindings, _ctx} =
        apply(TestFeatureEnabledRule.Rule_needs_feature, :apply, [%{featured: true}, nil, ctx])

      assert erased == :ok
    end
  end
end
