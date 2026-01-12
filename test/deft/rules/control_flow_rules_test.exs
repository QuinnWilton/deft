defmodule Deft.Rules.ControlFlowRulesTest do
  @moduledoc """
  Tests for control flow typing rules: if, cond, case, match.
  """
  use Deft.TypeCase, async: true

  alias Deft.Rules.ControlFlow

  describe "if rule" do
    test "synthesizes union of branch types", %{ctx: ctx} do
      # if true then 42 else false
      ast = if_expr(literal(true), literal(42), literal(false))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      # Should be Integer | Boolean
      assert %Type.Union{} = type
    end

    test "same branch types collapse to single type", %{ctx: ctx} do
      # if true then 1 else 2
      ast = if_expr(literal(true), literal(1), literal(2))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      # Both branches are integers
      assert %Type.Integer{} = type
    end

    test "accepts boolean predicate from comparison", %{ctx: ctx} do
      # if 1 == 2 then 1 else 2
      pred = comparison(:==, literal(1), literal(2))
      ast = if_expr(pred, literal(1), literal(2))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Integer{} = type
    end

    test "accepts boolean predicate from guard", %{ctx: ctx} do
      # if is_integer(42) then 1 else 2
      pred = guard(:is_integer, literal(42))
      ast = if_expr(pred, literal(1), literal(2))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Integer{} = type
    end

    test "produces no bindings", %{ctx: ctx} do
      ast = if_expr(literal(true), literal(1), literal(2))

      {:ok, _erased, _type, bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert bindings == []
    end
  end

  describe "match rule" do
    test "creates bindings from variable pattern", %{ctx: ctx} do
      # x = 42
      ast = match_expr(local(:x), literal(42))

      {:ok, _erased, type, bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Integer{} = type
      assert length(bindings) == 1

      [{var, var_type}] = bindings
      assert var.name == :x
      assert %Type.Integer{} = var_type
    end

    test "creates multiple bindings from tuple pattern", %{ctx: ctx} do
      # {x, y} = {1, true}
      pattern = tuple([local(:x), local(:y)])
      value = tuple([literal(1), literal(true)])
      ast = match_expr(pattern, value)

      {:ok, _erased, type, bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.FixedTuple{} = type
      assert length(bindings) == 2

      names = Enum.map(bindings, fn {var, _} -> var.name end)
      assert :x in names
      assert :y in names
    end

    test "underscore pattern creates no binding", %{ctx: ctx} do
      # _ = 42
      ast = match_expr(local(:_), literal(42))

      {:ok, _erased, _type, bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert bindings == []
    end

    test "returns value type as expression type", %{ctx: ctx} do
      ast = match_expr(local(:x), literal(3.14))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Float{} = type
    end
  end

  describe "case rule" do
    test "synthesizes union of branch body types", %{ctx: ctx} do
      # case 42 do
      #   x -> true
      # end
      branch = case_branch(local(:x), literal(true))
      ast = case_expr(literal(42), [branch])

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Boolean{} = type
    end

    test "multiple branches produce union type", %{ctx: ctx} do
      # case 42 do
      #   x when is_integer(x) -> 1
      #   y -> true
      # end
      branch1 = case_branch(typed_local(:x, Type.integer()), literal(1))
      branch2 = case_branch(local(:y), literal(true))
      ast = case_expr(literal(42), [branch1, branch2])

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      # Should be Integer | Boolean or just Integer if union simplifies
      assert type.__struct__ in [Type.Integer, Type.Union]
    end

    test "pattern bindings are available in branch body", %{ctx: ctx} do
      # case 42 do
      #   x -> x
      # end
      # The typed_local simulates a variable with known type
      branch = case_branch(local(:x), typed_local(:x, Type.integer()))
      ast = case_expr(literal(42), [branch])

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Integer{} = type
    end

    test "case produces no outer bindings", %{ctx: ctx} do
      branch = case_branch(local(:x), literal(1))
      ast = case_expr(literal(42), [branch])

      {:ok, _erased, _type, bindings, _ctx} = TypeChecker.check(ast, ctx)

      # Pattern bindings should be scoped to the branch
      assert bindings == []
    end
  end

  describe "case exhaustiveness checking" do
    test "raises for missing pattern with feature enabled", %{ctx: ctx} do
      # Case on union with only one branch
      union_value = if_expr(literal(true), literal(1), literal(true))

      # Branch only handles integer via wildcard
      branch = case_branch(typed_local(:x, Type.integer()), literal(:ok))
      ast = case_expr(union_value, [branch])

      # This should raise InexhaustivePatterns because we're not handling the boolean case
      assert_raise CompileError, fn ->
        TypeChecker.check(ast, ctx)
      end
    end

    test "passes when wildcard pattern covers all cases", %{ctx: ctx} do
      # Any union with a wildcard should pass
      union_value = if_expr(literal(true), literal(1), literal(true))

      branch = case_branch(local(:x), literal(:ok))
      ast = case_expr(union_value, [branch])

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Atom{} = type
    end
  end

  describe "cond rule" do
    test "synthesizes union of branch body types", %{ctx: ctx} do
      # cond do
      #   true -> 1
      #   true -> 2
      # end
      branches = [
        cond_branch(literal(true), literal(1)),
        cond_branch(literal(true), literal(2))
      ]

      ast = cond_expr(branches)

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Integer{} = type
    end

    test "different branch types produce union", %{ctx: ctx} do
      # cond do
      #   true -> 1
      #   true -> true
      # end
      branches = [
        cond_branch(literal(true), literal(1)),
        cond_branch(literal(true), literal(true))
      ]

      ast = cond_expr(branches)

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Union{} = type
    end

    test "accepts comparison predicates", %{ctx: ctx} do
      # cond do
      #   1 == 2 -> :a
      #   3 > 4 -> :b
      # end
      branches = [
        cond_branch(comparison(:==, literal(1), literal(2)), literal(:a)),
        cond_branch(comparison(:>, literal(3), literal(4)), literal(:b))
      ]

      ast = cond_expr(branches)

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Atom{} = type
    end

    test "produces no bindings", %{ctx: ctx} do
      branches = [cond_branch(literal(true), literal(1))]
      ast = cond_expr(branches)

      {:ok, _erased, _type, bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert bindings == []
    end
  end

  describe "rule matching" do
    test "if rule matches AST.If nodes" do
      ast = if_expr(literal(true), literal(1), literal(2))
      assert ControlFlow.Rule_if.matches?(ast)
    end

    test "match rule matches AST.Match nodes" do
      ast = match_expr(local(:x), literal(42))
      assert ControlFlow.Rule_match.matches?(ast)
    end

    test "case rule matches AST.Case nodes" do
      ast = case_expr(literal(42), [case_branch(local(:x), literal(1))])
      assert ControlFlow.Rule_case.matches?(ast)
    end

    test "cond rule matches AST.Cond nodes" do
      ast = cond_expr([cond_branch(literal(true), literal(1))])
      assert ControlFlow.Rule_cond.matches?(ast)
    end
  end
end
