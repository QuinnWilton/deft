defmodule Deft.Rules.CoreRulesTest do
  @moduledoc """
  Tests for core typing rules: literals, locals, annotations, blocks, tuples, pairs, lists.
  """
  use Deft.TypeCase, async: true

  alias Deft.Rules.Core

  describe "literal rule" do
    test "matches AST.Literal nodes" do
      ast = literal(42)
      assert Core.Rule_literal.matches?(ast)
    end

    test "synthesizes integer type for integers", %{ctx: ctx} do
      ast = literal(42)

      {:ok, erased, type, bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert erased == 42
      assert %Type.Integer{} = type
      assert bindings == []
    end

    test "synthesizes float type for floats", %{ctx: ctx} do
      ast = literal(3.14)

      {:ok, erased, type, bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert erased == 3.14
      assert %Type.Float{} = type
      assert bindings == []
    end

    test "synthesizes boolean type for true", %{ctx: ctx} do
      ast = literal(true)

      {:ok, erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert erased == true
      assert %Type.Boolean{} = type
    end

    test "synthesizes boolean type for false", %{ctx: ctx} do
      ast = literal(false)

      {:ok, erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert erased == false
      assert %Type.Boolean{} = type
    end

    test "synthesizes atom type for atoms", %{ctx: ctx} do
      ast = literal(:ok)

      {:ok, erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert erased == :ok
      assert %Type.Atom{} = type
    end

    test "synthesizes atom type for nil (nil is an atom)", %{ctx: ctx} do
      ast = literal(nil)

      {:ok, erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert erased == nil
      # In Elixir, nil is an atom
      assert %Type.Atom{} = type
    end
  end

  describe "local rule" do
    test "matches AST.Local nodes" do
      ast = local(:x)
      assert Core.Rule_local.matches?(ast)
    end

    test "synthesizes type from metadata", %{ctx: ctx} do
      ast = typed_local(:x, Type.integer())

      {:ok, erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert {:x, _, _} = erased
      assert %Type.Integer{} = type
    end

    test "synthesizes nil type for untyped local", %{ctx: ctx} do
      ast = local(:x)

      {:ok, erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert {:x, _, _} = erased
      assert type == nil
    end
  end

  describe "annotation rule" do
    test "matches AST.Annotation nodes" do
      ast = annotation(:x, Type.integer())
      assert Core.Rule_annotation.matches?(ast)
    end

    test "synthesizes annotated type", %{ctx: ctx} do
      ast = annotation(:x, Type.integer())

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Integer{} = type
    end

    test "creates binding for variable", %{ctx: ctx} do
      ast = annotation(:x, Type.integer())

      {:ok, _erased, _type, bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert length(bindings) == 1
      [{var, var_type}] = bindings
      assert var.name == :x
      assert %Type.Integer{} = var_type
    end

    test "annotation erases to plain variable", %{ctx: ctx} do
      ast = annotation(:x, Type.boolean())

      {:ok, erased, _type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert {:x, _, _} = erased
    end
  end

  describe "block rule" do
    test "matches AST.Block nodes" do
      ast = block([literal(1), literal(2)])
      assert Core.Rule_block.matches?(ast)
    end

    test "synthesizes type of last expression", %{ctx: ctx} do
      ast = block([literal(1), literal(true)])

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Boolean{} = type
    end

    test "single expression block", %{ctx: ctx} do
      ast = block([literal(42)])

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Integer{} = type
    end

    test "bindings from earlier expressions available in later ones", %{ctx: ctx} do
      # {x :: integer; x}
      ast =
        block([
          annotation(:x, Type.integer()),
          typed_local(:x, Type.integer())
        ])

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.Integer{} = type
    end

    test "erases to block of erased expressions", %{ctx: ctx} do
      ast = block([literal(1), literal(2)])

      {:ok, erased, _type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert {:__block__, _, [1, 2]} = erased
    end
  end

  describe "tuple rule" do
    test "matches AST.Tuple nodes" do
      ast = tuple([literal(1), literal(2)])
      assert Core.Rule_tuple.matches?(ast)
    end

    test "synthesizes fixed tuple type", %{ctx: ctx} do
      ast = tuple([literal(1), literal(true)])

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.FixedTuple{elements: elements} = type
      assert length(elements) == 2
      assert [%Type.Integer{}, %Type.Boolean{}] = elements
    end

    test "empty tuple", %{ctx: ctx} do
      ast = tuple([])

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.FixedTuple{elements: []} = type
    end

    test "nested tuples", %{ctx: ctx} do
      inner = tuple([literal(1), literal(2)])
      ast = tuple([inner, literal(true)])

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.FixedTuple{elements: [inner_type, %Type.Boolean{}]} = type
      assert %Type.FixedTuple{elements: [%Type.Integer{}, %Type.Integer{}]} = inner_type
    end

    test "erases to tuple expression", %{ctx: ctx} do
      ast = tuple([literal(1), literal(2)])

      {:ok, erased, _type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert {:{}, _, [1, 2]} = erased
    end
  end

  describe "pair rule" do
    test "matches AST.Pair nodes" do
      ast = pair(literal(1), literal(2))
      assert Core.Rule_pair.matches?(ast)
    end

    test "synthesizes two-element fixed tuple type", %{ctx: ctx} do
      ast = pair(literal(1), literal(true))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.FixedTuple{elements: [%Type.Integer{}, %Type.Boolean{}]} = type
    end

    test "erases to two-element tuple", %{ctx: ctx} do
      ast = pair(literal(1), literal(2))

      {:ok, erased, _type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert {1, 2} = erased
    end
  end

  describe "list rule" do
    test "matches AST.List nodes" do
      ast = list([literal(1), literal(2)])
      assert Core.Rule_list.matches?(ast)
    end

    test "synthesizes fixed list type with union of element types", %{ctx: ctx} do
      ast = list([literal(1), literal(2), literal(3)])

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.FixedList{} = type
      assert %Type.Integer{} = Type.FixedList.contents(type)
    end

    test "empty list", %{ctx: ctx} do
      ast = list([])

      {:ok, erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert erased == []
      assert %Type.FixedList{} = type
    end

    test "mixed type list creates union", %{ctx: ctx} do
      ast = list([literal(1), literal(true)])

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.FixedList{} = type
      elem_type = Type.FixedList.contents(type)
      assert %Type.Union{} = elem_type
    end

    test "erases to list expression", %{ctx: ctx} do
      ast = list([literal(1), literal(2)])

      {:ok, erased, _type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert [1, 2] = erased
    end
  end

  describe "cons rule" do
    test "matches AST.Cons nodes" do
      ast = cons(literal(1), list([]))
      assert Core.Rule_cons.matches?(ast)
    end

    test "synthesizes list type from cons expression", %{ctx: ctx} do
      ast = cons(literal(1), list([]))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.FixedList{} = type
      assert %Type.Integer{} = Type.FixedList.contents(type)
    end

    test "unions head type with tail element type", %{ctx: ctx} do
      # [1 | [true, false]] should produce [integer | boolean]
      ast = cons(literal(1), list([literal(true), literal(false)]))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.FixedList{} = type
      elem_type = Type.FixedList.contents(type)
      assert %Type.Union{} = elem_type
    end

    test "cons with empty tail", %{ctx: ctx} do
      ast = cons(literal(:ok), list([]))

      {:ok, _erased, type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert %Type.FixedList{} = type
      # Empty list has bottom type, union with atom is atom.
      elem_type = Type.FixedList.contents(type)
      assert %Type.Atom{} = elem_type
    end

    test "erases to cons expression", %{ctx: ctx} do
      ast = cons(literal(1), list([literal(2)]))

      {:ok, erased, _type, _bindings, _ctx} = TypeChecker.check(ast, ctx)

      assert {:|, _, [1, [2]]} = erased
    end

    test "raises error when tail is not a list", %{ctx: ctx} do
      # [1 | 2] - tail is integer, not a list
      ast = cons(literal(1), literal(2))

      error =
        assert_raise Deft.Error.Exception, fn ->
          TypeChecker.check(ast, ctx)
        end

      assert error.error.code == :type_mismatch
    end
  end
end
