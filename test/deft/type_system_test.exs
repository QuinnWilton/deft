defmodule Deft.TypeSystemTest do
  use ExUnit.Case

  alias Deft.Context
  alias Deft.Rule.Registry
  alias Deft.Type

  describe "use Deft.TypeSystem" do
    test "creates a module with features/0" do
      defmodule TestFeaturesTypeSystem do
        use Deft.TypeSystem

        features([:test_feature, :another_feature])
      end

      assert TestFeaturesTypeSystem.features() == [:test_feature, :another_feature]
    end

    test "creates a module with rule_modules/0" do
      defmodule TestRuleModulesTypeSystem do
        use Deft.TypeSystem

        include(Deft.Rules.Declarative.Core)
        include(Deft.Rules.Declarative.Functions)
      end

      modules = TestRuleModulesTypeSystem.rule_modules()
      assert Deft.Rules.Declarative.Core in modules
      assert Deft.Rules.Declarative.Functions in modules
    end

    test "creates a module with all_rules/0" do
      defmodule TestAllRulesTypeSystem do
        use Deft.TypeSystem

        include(Deft.Rules.Declarative.Core)
      end

      rules = TestAllRulesTypeSystem.all_rules()
      assert is_list(rules)
      assert length(rules) > 0
      # Should include Literal rule from Declarative.Core
      assert Deft.Rules.Declarative.Core.Rule_literal in rules
    end

    test "creates a module with registry/0" do
      defmodule TestRegistryTypeSystem do
        use Deft.TypeSystem

        include(Deft.Rules.Declarative.Core)
      end

      registry = TestRegistryTypeSystem.registry()
      assert %Registry{} = registry
    end

    test "creates a module with context/1" do
      defmodule TestContextTypeSystem do
        use Deft.TypeSystem

        features([:my_feature])
      end

      ctx = TestContextTypeSystem.context(__ENV__)
      assert %Context{} = ctx
      assert Context.feature_enabled?(ctx, :my_feature)
    end
  end

  describe "include ordering" do
    test "rules are included in order" do
      defmodule TestOrderingTypeSystem do
        use Deft.TypeSystem

        include(Deft.Rules.Declarative.Core)
        include(Deft.Rules.Declarative.Functions)
      end

      modules = TestOrderingTypeSystem.rule_modules()
      assert modules == [Deft.Rules.Declarative.Core, Deft.Rules.Declarative.Functions]
    end

    test "include_first adds rules at higher priority" do
      defmodule PriorityRules do
        def rules, do: [:priority_rule]
      end

      defmodule TestPriorityTypeSystem do
        use Deft.TypeSystem

        include(Deft.Rules.Declarative.Core)
        include_first(PriorityRules)
      end

      modules = TestPriorityTypeSystem.rule_modules()
      # PriorityRules should come first
      assert hd(modules) == PriorityRules
    end
  end

  describe "Deft.TypeSystem.Default" do
    test "includes standard rule sets" do
      modules = Deft.TypeSystem.Default.rule_modules()

      assert Deft.Rules.Declarative.Core in modules
      assert Deft.Rules.Declarative.Functions in modules
      assert Deft.Rules.Declarative.ControlFlow in modules
      assert Deft.Rules.Declarative.Builtins in modules
    end

    test "has exhaustiveness_checking enabled" do
      features = Deft.TypeSystem.Default.features()
      assert :exhaustiveness_checking in features
    end

    test "can type check expressions" do
      # A simple literal
      ast = %Deft.AST.Literal{value: 42, meta: []}

      {:ok, erased, type, _bindings, _ctx} = Deft.TypeSystem.Default.check(ast, __ENV__)

      assert erased == 42
      assert %Type.Integer{} = type
    end
  end
end
