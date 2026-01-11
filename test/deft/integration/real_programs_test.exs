defmodule Deft.Integration.RealProgramsTest do
  @moduledoc """
  Integration tests for real-world programming patterns.

  These tests verify that common programming idioms work correctly
  with Deft's type system.
  """
  use Deft.TypeCase, async: true

  describe "recursive patterns" do
    test "factorial-like pattern with explicit types", %{ctx: ctx} do
      # Test the structure of a factorial-like function
      # fn n :: integer -> n end (simplified - no recursion in base AST)
      ast = fn_expr([annotation(:n, Type.integer())], typed_local(:n, Type.integer()))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Fn{inputs: [%Type.Integer{}], output: %Type.Integer{}} = type
    end
  end

  describe "higher-order function patterns" do
    test "identity function", %{ctx: ctx} do
      # fn x :: integer -> x end
      ast = fn_expr([annotation(:x, Type.integer())], typed_local(:x, Type.integer()))

      {:ok, erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Fn{inputs: [%Type.Integer{}], output: %Type.Integer{}} = type
      # Verify the erased code is a function
      assert {:fn, _, _} = erased
    end

    test "constant function", %{ctx: ctx} do
      # fn _x :: integer -> true end
      ast = fn_expr([annotation(:_x, Type.integer())], literal(true))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Fn{inputs: [%Type.Integer{}], output: %Type.Boolean{}} = type
    end

    test "composition structure", %{ctx: ctx} do
      # fn f :: (integer -> boolean) -> f end
      fn_type = Type.fun([Type.integer()], Type.boolean())
      ast = fn_expr([annotation(:f, fn_type)], typed_local(:f, fn_type))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Fn{inputs: [%Type.Fn{}], output: %Type.Fn{}} = type
    end

    test "function that returns a function", %{ctx: ctx} do
      # fn x :: integer -> fn y :: boolean -> x end end
      inner = fn_expr([annotation(:y, Type.boolean())], typed_local(:x, Type.integer()))
      outer = fn_expr([annotation(:x, Type.integer())], inner)

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(outer, ctx)

      assert %Type.Fn{inputs: [%Type.Integer{}], output: inner_type} = type
      assert %Type.Fn{inputs: [%Type.Boolean{}], output: %Type.Integer{}} = inner_type
    end
  end

  describe "data structure patterns" do
    test "pair construction", %{ctx: ctx} do
      # {1, true}
      ast = pair(literal(1), literal(true))

      {:ok, erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.FixedTuple{elements: [%Type.Integer{}, %Type.Boolean{}]} = type
      assert {1, true} = erased
    end

    test "nested tuple construction", %{ctx: ctx} do
      # {{1, 2}, {true, false}}
      left = tuple([literal(1), literal(2)])
      right = tuple([literal(true), literal(false)])
      ast = tuple([left, right])

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.FixedTuple{elements: [left_type, right_type]} = type
      assert %Type.FixedTuple{elements: [%Type.Integer{}, %Type.Integer{}]} = left_type
      assert %Type.FixedTuple{elements: [%Type.Boolean{}, %Type.Boolean{}]} = right_type
    end

    test "list of same type", %{ctx: ctx} do
      # [1, 2, 3]
      ast = list([literal(1), literal(2), literal(3)])

      {:ok, erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.FixedList{} = type
      assert %Type.Integer{} = Type.FixedList.contents(type)
      assert [1, 2, 3] = erased
    end

    test "list with mixed types creates union", %{ctx: ctx} do
      # [1, true, :ok]
      ast = list([literal(1), literal(true), literal(:ok)])

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.FixedList{} = type
      elem_type = Type.FixedList.contents(type)
      assert %Type.Union{} = elem_type
    end
  end

  describe "control flow patterns" do
    test "if-then-else with same type branches", %{ctx: ctx} do
      # if true then 1 else 2
      ast = if_expr(literal(true), literal(1), literal(2))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Integer{} = type
    end

    test "if-then-else with different type branches", %{ctx: ctx} do
      # if true then 1 else true
      ast = if_expr(literal(true), literal(1), literal(true))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Union{} = type
    end

    test "nested if expressions", %{ctx: ctx} do
      # if true then (if false then 1 else 2) else 3
      inner = if_expr(literal(false), literal(1), literal(2))
      ast = if_expr(literal(true), inner, literal(3))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Integer{} = type
    end

    test "case with single branch", %{ctx: ctx} do
      # case 42 do x -> x end
      branch = case_branch(local(:x), typed_local(:x, Type.integer()))
      ast = case_expr(literal(42), [branch])

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Integer{} = type
    end

    test "cond with multiple branches", %{ctx: ctx} do
      # cond do
      #   1 == 2 -> :a
      #   true -> :b
      # end
      branches = [
        cond_branch(comparison(:==, literal(1), literal(2)), literal(:a)),
        cond_branch(literal(true), literal(:b))
      ]

      ast = cond_expr(branches)

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Atom{} = type
    end
  end

  describe "binding patterns" do
    test "simple variable binding", %{ctx: ctx} do
      # x = 42
      ast = match_expr(local(:x), literal(42))

      {:ok, _erased, type, bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Integer{} = type
      assert [{var, %Type.Integer{}}] = bindings
      assert var.name == :x
    end

    test "tuple pattern binding", %{ctx: ctx} do
      # {x, y} = {1, true}
      pattern = tuple([local(:x), local(:y)])
      value = tuple([literal(1), literal(true)])
      ast = match_expr(pattern, value)

      {:ok, _erased, type, bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.FixedTuple{} = type
      assert length(bindings) == 2

      x_binding = Enum.find(bindings, fn {var, _} -> var.name == :x end)
      y_binding = Enum.find(bindings, fn {var, _} -> var.name == :y end)

      assert {_, %Type.Integer{}} = x_binding
      assert {_, %Type.Boolean{}} = y_binding
    end

    test "underscore does not create binding", %{ctx: ctx} do
      # _ = 42
      ast = match_expr(local(:_), literal(42))

      {:ok, _erased, _type, bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert bindings == []
    end

    test "block with sequential bindings", %{ctx: ctx} do
      # {
      #   x :: integer;
      #   y :: boolean;
      #   {x, y}
      # }
      ast =
        block([
          annotation(:x, Type.integer()),
          annotation(:y, Type.boolean()),
          tuple([typed_local(:x, Type.integer()), typed_local(:y, Type.boolean())])
        ])

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.FixedTuple{elements: [%Type.Integer{}, %Type.Boolean{}]} = type
    end
  end

  describe "complex expressions" do
    test "function application in if branches", %{ctx: ctx} do
      # if true then (fn x :: integer -> x end).(42) else 0
      fun = fn_expr([annotation(:x, Type.integer())], typed_local(:x, Type.integer()))
      app = fn_apply(fun, [literal(42)])
      ast = if_expr(literal(true), app, literal(0))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Integer{} = type
    end

    test "comparison in if predicate", %{ctx: ctx} do
      # if 1 < 2 then :less else :not_less
      pred = comparison(:<, literal(1), literal(2))
      ast = if_expr(pred, literal(:less), literal(:not_less))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Atom{} = type
    end

    test "guard check in if predicate", %{ctx: ctx} do
      # if is_integer(42) then 1 else 0
      pred = guard(:is_integer, literal(42))
      ast = if_expr(pred, literal(1), literal(0))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Integer{} = type
    end

    test "arithmetic in function body", %{ctx: ctx} do
      # fn x :: integer -> x + 1 end
      body = arithmetic(:+, typed_local(:x, Type.integer()), literal(1))
      ast = fn_expr([annotation(:x, Type.integer())], body)

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Fn{inputs: [%Type.Integer{}], output: %Type.Integer{}} = type
    end

    test "tuple in function body", %{ctx: ctx} do
      # fn x :: integer -> {x, x + 1} end
      plus_one = arithmetic(:+, typed_local(:x, Type.integer()), literal(1))
      body = tuple([typed_local(:x, Type.integer()), plus_one])
      ast = fn_expr([annotation(:x, Type.integer())], body)

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Fn{inputs: [%Type.Integer{}], output: output} = type
      assert %Type.FixedTuple{elements: [%Type.Integer{}, %Type.Integer{}]} = output
    end
  end
end
