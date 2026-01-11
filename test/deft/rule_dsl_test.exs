defmodule Deft.RulesDSLTest do
  use ExUnit.Case

  alias Deft.Context
  alias Deft.Type

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
