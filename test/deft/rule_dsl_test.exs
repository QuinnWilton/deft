defmodule Deft.Rule.DSLTest do
  use ExUnit.Case

  alias Deft.Context
  alias Deft.Type

  describe "defrule macro" do
    test "defines a rule with the correct name" do
      defmodule TestNameRule do
        use Deft.Rule.DSL

        defrule(:test_name,
          match: %{test: _},
          do: emit(:ok, Type.atom())
        )
      end

      assert TestNameRule.Rule_test_name.name() == :test_name
    end

    test "defines a rule with the correct judgment" do
      defmodule TestJudgmentRule do
        use Deft.Rule.DSL

        defrule(:synth_rule,
          match: %{synth: _},
          judgment: :synth,
          do: emit(:ok, Type.atom())
        )

        defrule(:check_rule,
          match: %{check: _},
          judgment: :check,
          do: emit(:ok, Type.atom())
        )
      end

      assert TestJudgmentRule.Rule_synth_rule.judgment() == :synth
      assert TestJudgmentRule.Rule_check_rule.judgment() == :check
    end

    test "matches? returns true for matching patterns" do
      defmodule TestMatchRule do
        use Deft.Rule.DSL

        defrule(:match_test,
          match: %{value: 42},
          do: emit(:ok, Type.integer())
        )
      end

      assert TestMatchRule.Rule_match_test.matches?(%{value: 42})
      refute TestMatchRule.Rule_match_test.matches?(%{value: 43})
      refute TestMatchRule.Rule_match_test.matches?(%{other: 42})
    end

    test "apply returns successful result" do
      defmodule TestApplyRule do
        use Deft.Rule.DSL

        defrule(:apply_test,
          match: %{value: value},
          do: emit(value, Type.integer())
        )
      end

      ctx = Context.new(__ENV__)

      {:ok, erased, type, bindings, _ctx} =
        TestApplyRule.Rule_apply_test.apply(%{value: 42}, nil, ctx)

      assert erased == 42
      assert %Type.Integer{} = type
      assert bindings == []
    end

    test "rules/0 returns all defined rules" do
      defmodule TestRulesCollection do
        use Deft.Rule.DSL

        defrule(:first_rule,
          match: %{first: _},
          do: emit(:first, Type.atom())
        )

        defrule(:second_rule,
          match: %{second: _},
          do: emit(:second, Type.atom())
        )
      end

      rules = TestRulesCollection.rules()
      assert length(rules) == 2
      assert TestRulesCollection.Rule_first_rule in rules
      assert TestRulesCollection.Rule_second_rule in rules
    end
  end

  describe "DSL helpers" do
    test "type_of_literal returns correct types" do
      import Deft.Rule.DSL.Helpers

      assert %Type.Boolean{} = type_of_literal(true)
      assert %Type.Boolean{} = type_of_literal(false)
      assert %Type.Atom{} = type_of_literal(:hello)
      assert %Type.Integer{} = type_of_literal(42)
      assert %Type.Float{} = type_of_literal(3.14)
    end

    test "union_types combines types correctly" do
      import Deft.Rule.DSL.Helpers

      assert %Type.Bottom{} = union_types([])
      assert %Type.Integer{} = union_types([Type.integer()])
      assert %Type.Union{} = union_types([Type.integer(), Type.boolean()])
    end

    test "bind creates binding tuple" do
      import Deft.Rule.DSL.Helpers

      var = :x
      type = Type.integer()

      assert {^var, ^type} = bind(var, type)
    end
  end

  describe "feature flags in rules" do
    test "rule with required features fails if feature not enabled" do
      defmodule TestFeatureRule do
        use Deft.Rule.DSL

        defrule(:needs_feature,
          match: %{featured: _},
          requires: [:special_feature],
          do: emit(:ok, Type.atom())
        )
      end

      ctx = Context.new(__ENV__, features: [])
      result = TestFeatureRule.Rule_needs_feature.apply(%{featured: true}, nil, ctx)

      assert {:error, {:missing_features, [:special_feature]}} = result
    end

    test "rule with required features succeeds if feature enabled" do
      defmodule TestFeatureEnabledRule do
        use Deft.Rule.DSL

        defrule(:needs_feature,
          match: %{featured: _},
          requires: [:special_feature],
          do: emit(:ok, Type.atom())
        )
      end

      ctx = Context.new(__ENV__, features: [:special_feature])

      {:ok, erased, _type, _bindings, _ctx} =
        TestFeatureEnabledRule.Rule_needs_feature.apply(%{featured: true}, nil, ctx)

      assert erased == :ok
    end
  end
end
