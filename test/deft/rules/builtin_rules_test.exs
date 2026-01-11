defmodule Deft.Rules.BuiltinRulesTest do
  @moduledoc """
  Tests for built-in typing rules: local_call (guards, operators) and type_constructor_call.
  """
  use Deft.TypeCase, async: true

  alias Deft.Rules.Builtins

  describe "local_call rule - guards" do
    test "matches AST.LocalCall nodes" do
      ast = guard(:is_integer, literal(42))
      assert Builtins.Rule_local_call.matches?(ast)
    end

    test "is_integer returns boolean", %{ctx: ctx} do
      ast = guard(:is_integer, literal(42))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Boolean{} = type
    end

    test "is_float returns boolean", %{ctx: ctx} do
      ast = guard(:is_float, literal(3.14))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Boolean{} = type
    end

    test "is_boolean returns boolean", %{ctx: ctx} do
      ast = guard(:is_boolean, literal(true))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Boolean{} = type
    end

    test "is_atom returns boolean", %{ctx: ctx} do
      ast = guard(:is_atom, literal(:ok))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Boolean{} = type
    end

    test "is_number returns boolean", %{ctx: ctx} do
      ast = guard(:is_number, literal(42))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Boolean{} = type
    end

    test "is_list returns boolean", %{ctx: ctx} do
      ast = guard(:is_list, list([literal(1)]))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Boolean{} = type
    end

    test "is_tuple returns boolean", %{ctx: ctx} do
      ast = guard(:is_tuple, tuple([literal(1), literal(2)]))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Boolean{} = type
    end

    test "guard erases to function call", %{ctx: ctx} do
      ast = guard(:is_integer, literal(42))

      {:ok, erased, _type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert {:is_integer, _, [42]} = erased
    end
  end

  describe "local_call rule - arithmetic operators" do
    test "+ with integers returns integer", %{ctx: ctx} do
      ast = arithmetic(:+, literal(1), literal(2))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Integer{} = type
    end

    test "+ with floats returns float", %{ctx: ctx} do
      ast = arithmetic(:+, literal(1.0), literal(2.0))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Float{} = type
    end

    test "- with integers returns integer", %{ctx: ctx} do
      ast = arithmetic(:-, literal(5), literal(3))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Integer{} = type
    end

    test "* with integers returns integer", %{ctx: ctx} do
      ast = arithmetic(:*, literal(3), literal(4))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Integer{} = type
    end

    test "/ with integers returns float", %{ctx: ctx} do
      ast = arithmetic(:/, literal(10), literal(3))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Float{} = type
    end

    test "arithmetic erases to operator call", %{ctx: ctx} do
      ast = arithmetic(:+, literal(1), literal(2))

      {:ok, erased, _type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert {:+, _, [1, 2]} = erased
    end
  end

  describe "local_call rule - comparison operators" do
    test "== returns boolean", %{ctx: ctx} do
      ast = comparison(:==, literal(1), literal(2))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Boolean{} = type
    end

    test "!= returns boolean", %{ctx: ctx} do
      ast = comparison(:!=, literal(1), literal(2))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Boolean{} = type
    end

    test "< returns boolean", %{ctx: ctx} do
      ast = comparison(:<, literal(1), literal(2))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Boolean{} = type
    end

    test "> returns boolean", %{ctx: ctx} do
      ast = comparison(:>, literal(1), literal(2))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Boolean{} = type
    end

    test "<= returns boolean", %{ctx: ctx} do
      ast = comparison(:<=, literal(1), literal(2))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Boolean{} = type
    end

    test ">= returns boolean", %{ctx: ctx} do
      ast = comparison(:>=, literal(1), literal(2))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Boolean{} = type
    end

    test "=== (strict equality) returns boolean", %{ctx: ctx} do
      ast = comparison(:===, literal(1), literal(1))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Boolean{} = type
    end

    test "!== (strict inequality) returns boolean", %{ctx: ctx} do
      ast = comparison(:!==, literal(1), literal(1.0))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Boolean{} = type
    end

    test "comparison erases to operator call", %{ctx: ctx} do
      ast = comparison(:==, literal(1), literal(2))

      {:ok, erased, _type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert {:==, _, [1, 2]} = erased
    end
  end

  describe "local_call rule - unsupported calls" do
    test "raises for unsupported function" do
      ast = local_call(:unsupported_function, [literal(42)])
      ctx = Context.new(__ENV__)

      assert_raise Deft.UnsupportedLocalCall, fn ->
        TypeChecker.check(ast, ctx)
      end
    end

    test "error includes function name and arity" do
      ast = local_call(:my_unknown_fn, [literal(1), literal(2)])
      ctx = Context.new(__ENV__)

      error =
        assert_raise Deft.UnsupportedLocalCall, fn ->
          TypeChecker.check(ast, ctx)
        end

      assert error.name == :my_unknown_fn
      assert error.arity == 2
    end
  end

  describe "type_constructor_call rule" do
    # Note: Type constructor calls require ADT context setup
    # These tests use the full Deft.compile macro to properly establish ADT bindings

    test "matches AST.TypeConstructorCall nodes" do
      variant = Type.variant(:circle, :shape, [Type.float()])
      adt = Type.adt(:shape, [variant])

      ast = %AST.TypeConstructorCall{
        name: :circle,
        args: [literal(1.0)],
        type: adt,
        variant: variant,
        meta: []
      }

      assert Builtins.Rule_type_constructor_call.matches?(ast)
    end
  end

  describe "local_call produces no bindings" do
    test "guards produce no bindings", %{ctx: ctx} do
      ast = guard(:is_integer, literal(42))

      {:ok, _erased, _type, bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert bindings == []
    end

    test "arithmetic produces no bindings", %{ctx: ctx} do
      ast = arithmetic(:+, literal(1), literal(2))

      {:ok, _erased, _type, bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert bindings == []
    end

    test "comparisons produce no bindings", %{ctx: ctx} do
      ast = comparison(:==, literal(1), literal(2))

      {:ok, _erased, _type, bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert bindings == []
    end
  end
end
