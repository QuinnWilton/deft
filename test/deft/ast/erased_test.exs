defmodule Deft.AST.ErasedTest do
  use ExUnit.Case, async: true

  alias Deft.AST.Erased

  describe "control flow builders" do
    test "if_expr/4 builds correct AST" do
      result = Erased.if_expr([line: 1], :pred, :do_body, :else_body)
      assert {:if, [line: 1], [:pred, [do: :do_body, else: :else_body]]} = result
    end

    test "case_expr/3 builds correct AST" do
      branches = [:branch1, :branch2]
      result = Erased.case_expr([line: 2], :subject, branches)
      assert {:case, [line: 2], [:subject, [do: [:branch1, :branch2]]]} = result
    end

    test "cond_expr/2 builds correct AST" do
      branches = [:branch1, :branch2]
      result = Erased.cond_expr([line: 3], branches)
      assert {:cond, [line: 3], [[do: [:branch1, :branch2]]]} = result
    end

    test "branch/3 builds arrow clause" do
      result = Erased.branch([line: 4], :pattern, :body)
      assert {:->, [line: 4], [[:pattern], :body]} = result
    end
  end

  describe "data structure builders" do
    test "tuple/2 builds tuple AST" do
      result = Erased.tuple([line: 5], [:a, :b, :c])
      assert {:{}, [line: 5], [:a, :b, :c]} = result
    end

    test "pair/2 builds 2-tuple" do
      result = Erased.pair(:a, :b)
      assert {:a, :b} = result
    end

    test "block/2 builds block AST" do
      result = Erased.block([line: 6], [:expr1, :expr2])
      assert {:__block__, [line: 6], [:expr1, :expr2]} = result
    end
  end

  describe "function builders" do
    test "fn_expr/4 builds anonymous function AST" do
      result = Erased.fn_expr([line: 7], [line: 8], [:x], :body)
      assert {:fn, [line: 7], [{:->, [line: 8], [[:x], :body]}]} = result
    end

    test "fn_apply/4 builds function application AST" do
      result = Erased.fn_apply([line: 9], [line: 10], :fun, [:arg1, :arg2])
      assert {{:., [line: 9], [:fun]}, [line: 10], [:arg1, :arg2]} = result
    end

    test "local_call/3 builds local function call AST" do
      result = Erased.local_call([line: 11], :foo, [:arg1, :arg2])
      assert {:foo, [line: 11], [:arg1, :arg2]} = result
    end
  end

  describe "pattern matching builders" do
    test "match/3 builds match expression AST" do
      result = Erased.match([line: 12], :pattern, :value)
      assert {:=, [line: 12], [:pattern, :value]} = result
    end
  end
end
