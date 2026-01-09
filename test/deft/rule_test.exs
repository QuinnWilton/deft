defmodule Deft.RuleTest do
  use ExUnit.Case

  alias Deft.AST
  alias Deft.Context
  alias Deft.Rules.Registry
  alias Deft.Type

  # A simple test rule for testing
  defmodule TestLiteralRule do
    @behaviour Deft.Rules

    @impl true
    def name, do: :test_literal

    @impl true
    def judgment, do: :synth

    @impl true
    def matches?(%AST.Literal{}), do: true
    def matches?(_), do: false

    @impl true
    def apply(%AST.Literal{value: value}, _expected, ctx) do
      type =
        cond do
          is_boolean(value) -> Type.boolean()
          is_atom(value) -> Type.atom()
          is_integer(value) -> Type.integer()
          is_float(value) -> Type.float()
          true -> Type.top()
        end

      {:ok, value, type, [], ctx}
    end
  end

  defmodule TestLocalRule do
    @behaviour Deft.Rules

    @impl true
    def name, do: :test_local

    @impl true
    def judgment, do: :synth

    @impl true
    def matches?(%AST.Local{}), do: true
    def matches?(_), do: false

    @impl true
    def apply(%AST.Local{} = local, _expected, ctx) do
      erased = {local.name, local.meta, local.context}
      {:ok, erased, nil, [], ctx}
    end
  end

  describe "Registry.new/0" do
    test "creates empty registry" do
      registry = Registry.new()

      assert %Registry{rules: []} = registry
    end
  end

  describe "Registry.new/1" do
    test "creates registry with rules" do
      registry = Registry.new([TestLiteralRule, TestLocalRule])

      assert %Registry{rules: [TestLiteralRule, TestLocalRule]} = registry
    end
  end

  describe "Registry.add/2" do
    test "appends rule to registry" do
      registry =
        Registry.new()
        |> Registry.add(TestLiteralRule)
        |> Registry.add(TestLocalRule)

      assert registry.rules == [TestLiteralRule, TestLocalRule]
    end
  end

  describe "Registry.prepend/2" do
    test "prepends rule to registry (higher priority)" do
      registry =
        Registry.new([TestLocalRule])
        |> Registry.prepend(TestLiteralRule)

      assert registry.rules == [TestLiteralRule, TestLocalRule]
    end
  end

  describe "Registry.find_rule/2" do
    test "finds matching rule for AST node" do
      registry = Registry.new([TestLiteralRule, TestLocalRule])
      literal = AST.Literal.new(42)

      assert {:ok, TestLiteralRule} = Registry.find_rule(registry, literal)
    end

    test "returns error when no rule matches" do
      registry = Registry.new([TestLiteralRule])
      local = AST.Local.new(:x, nil, [])

      assert :error = Registry.find_rule(registry, local)
    end

    test "returns first matching rule" do
      # Both rules could match, but first wins
      defmodule CatchAllRule do
        @behaviour Deft.Rules

        def name, do: :catch_all
        def judgment, do: :synth
        def matches?(_), do: true
        def apply(_, _, ctx), do: {:ok, nil, Type.top(), [], ctx}
      end

      registry = Registry.new([TestLiteralRule, CatchAllRule])
      literal = AST.Literal.new(42)

      assert {:ok, TestLiteralRule} = Registry.find_rule(registry, literal)
    end
  end

  describe "Registry.apply_rule/4" do
    test "applies matching rule and returns result" do
      registry = Registry.new([TestLiteralRule])
      ctx = Context.new(__ENV__)
      literal = AST.Literal.new(42)

      assert {:ok, 42, %Type.Integer{}, [], ^ctx} =
               Registry.apply_rule(registry, literal, nil, ctx)
    end

    test "returns error when no rule matches" do
      registry = Registry.new([TestLiteralRule])
      ctx = Context.new(__ENV__)
      local = AST.Local.new(:x, nil, [])

      assert {:error, {:no_matching_rule, ^local}} =
               Registry.apply_rule(registry, local, nil, ctx)
    end

    test "handles boolean literals" do
      registry = Registry.new([TestLiteralRule])
      ctx = Context.new(__ENV__)
      literal = AST.Literal.new(true)

      assert {:ok, true, %Type.Boolean{}, [], ^ctx} =
               Registry.apply_rule(registry, literal, nil, ctx)
    end

    test "handles atom literals" do
      registry = Registry.new([TestLiteralRule])
      ctx = Context.new(__ENV__)
      literal = AST.Literal.new(:hello)

      assert {:ok, :hello, %Type.Atom{}, [], ^ctx} =
               Registry.apply_rule(registry, literal, nil, ctx)
    end

    test "handles float literals" do
      registry = Registry.new([TestLiteralRule])
      ctx = Context.new(__ENV__)
      literal = AST.Literal.new(3.14)

      assert {:ok, 3.14, %Type.Float{}, [], ^ctx} =
               Registry.apply_rule(registry, literal, nil, ctx)
    end
  end

  describe "Rule behaviour implementation" do
    test "rule returns correct name" do
      assert TestLiteralRule.name() == :test_literal
    end

    test "rule returns correct judgment" do
      assert TestLiteralRule.judgment() == :synth
    end

    test "rule matches correctly" do
      literal = AST.Literal.new(42)
      local = AST.Local.new(:x, nil, [])

      assert TestLiteralRule.matches?(literal)
      refute TestLiteralRule.matches?(local)
    end
  end
end
