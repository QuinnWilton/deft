defmodule Deft.Rules.FunctionRulesTest do
  @moduledoc """
  Tests for function typing rules: fn (anonymous functions) and fn_application.
  """
  use Deft.TypeCase, async: true

  alias Deft.Rules.Functions

  describe "fn rule" do
    test "matches AST.Fn nodes" do
      ast = fn_expr([annotation(:x, Type.integer())], typed_local(:x, Type.integer()))
      assert Functions.Rule_fn.matches?(ast)
    end

    test "synthesizes function type from annotated args and body", %{ctx: ctx} do
      # fn x :: integer -> x end
      ast = fn_expr([annotation(:x, Type.integer())], typed_local(:x, Type.integer()))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Fn{inputs: [%Type.Integer{}], output: %Type.Integer{}} = type
    end

    test "multi-argument function", %{ctx: ctx} do
      # fn (x :: integer, y :: boolean) -> x end
      args = [annotation(:x, Type.integer()), annotation(:y, Type.boolean())]
      body = typed_local(:x, Type.integer())
      ast = fn_expr(args, body)

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Fn{inputs: inputs, output: %Type.Integer{}} = type
      assert [%Type.Integer{}, %Type.Boolean{}] = inputs
    end

    test "function body can use argument bindings", %{ctx: ctx} do
      # fn x :: integer -> x end where body references x
      ast = fn_expr([annotation(:x, Type.integer())], typed_local(:x, Type.integer()))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Fn{output: %Type.Integer{}} = type
    end

    test "function produces no outer bindings", %{ctx: ctx} do
      ast = fn_expr([annotation(:x, Type.integer())], typed_local(:x, Type.integer()))

      {:ok, _erased, _type, bindings, _ctx} = TypeChecker.check(ast, ctx)

      # Function arguments should be scoped to the body, not leaked
      assert bindings == []
    end

    test "constant function", %{ctx: ctx} do
      # fn _x :: integer -> true end
      ast = fn_expr([annotation(:_x, Type.integer())], literal(true))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Fn{inputs: [%Type.Integer{}], output: %Type.Boolean{}} = type
    end

    test "function erases to fn expression", %{ctx: ctx} do
      ast = fn_expr([annotation(:x, Type.integer())], typed_local(:x, Type.integer()))

      {:ok, erased, _type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert {:fn, _, [{:->, _, _}]} = erased
    end
  end

  describe "fn_application rule" do
    test "matches AST.FnApplication nodes" do
      fun = fn_expr([annotation(:x, Type.integer())], typed_local(:x, Type.integer()))
      ast = fn_apply(fun, [literal(42)])
      assert Functions.Rule_fn_application.matches?(ast)
    end

    test "synthesizes output type of applied function", %{ctx: ctx} do
      # (fn x :: integer -> true end).(42)
      fun = fn_expr([annotation(:x, Type.integer())], literal(true))
      ast = fn_apply(fun, [literal(42)])

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Boolean{} = type
    end

    test "validates argument count matches parameter count", %{ctx: ctx} do
      # (fn x :: integer -> x end).() - missing argument
      fun = fn_expr([annotation(:x, Type.integer())], typed_local(:x, Type.integer()))
      ast = fn_apply(fun, [])

      assert_raise Deft.Error.Exception, fn ->
        TypeChecker.check(ast, ctx)
      end
    end

    test "validates argument types are subtypes of parameter types", %{ctx: ctx} do
      # (fn x :: integer -> x end).(true) - wrong type
      fun = fn_expr([annotation(:x, Type.integer())], typed_local(:x, Type.integer()))
      ast = fn_apply(fun, [literal(true)])

      assert_raise Deft.Error.Exception, fn ->
        TypeChecker.check(ast, ctx)
      end
    end

    test "accepts subtype arguments", %{ctx: ctx} do
      # (fn x :: number -> x end).(42) - integer is subtype of number
      fun = fn_expr([annotation(:x, Type.number())], typed_local(:x, Type.number()))
      ast = fn_apply(fun, [literal(42)])

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Number{} = type
    end

    test "multi-argument application", %{ctx: ctx} do
      # (fn (x :: integer, y :: boolean) -> y end).(42, true)
      args = [annotation(:x, Type.integer()), annotation(:y, Type.boolean())]
      fun = fn_expr(args, typed_local(:y, Type.boolean()))
      ast = fn_apply(fun, [literal(42), literal(true)])

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Boolean{} = type
    end

    test "application produces no bindings", %{ctx: ctx} do
      fun = fn_expr([annotation(:x, Type.integer())], typed_local(:x, Type.integer()))
      ast = fn_apply(fun, [literal(42)])

      {:ok, _erased, _type, bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert bindings == []
    end

    test "erases to function application syntax", %{ctx: ctx} do
      fun = fn_expr([annotation(:x, Type.integer())], typed_local(:x, Type.integer()))
      ast = fn_apply(fun, [literal(42)])

      {:ok, erased, _type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      # Should be f.(args) syntax
      assert {{:., _, _}, _, _} = erased
    end
  end

  describe "higher-order functions" do
    test "function returning function", %{ctx: ctx} do
      # fn x :: integer -> fn y :: boolean -> x end end
      inner_fn = fn_expr([annotation(:y, Type.boolean())], typed_local(:x, Type.integer()))
      outer_fn = fn_expr([annotation(:x, Type.integer())], inner_fn)

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(outer_fn, ctx)

      assert %Type.Fn{inputs: [%Type.Integer{}], output: inner_type} = type
      assert %Type.Fn{inputs: [%Type.Boolean{}], output: %Type.Integer{}} = inner_type
    end

    test "function taking function as argument", %{ctx: ctx} do
      # fn f :: (integer -> boolean) -> f end
      fn_type = Type.fun([Type.integer()], Type.boolean())
      ast = fn_expr([annotation(:f, fn_type)], typed_local(:f, fn_type))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Fn{inputs: [input_fn], output: output_fn} = type
      assert %Type.Fn{} = input_fn
      assert %Type.Fn{} = output_fn
    end
  end
end
