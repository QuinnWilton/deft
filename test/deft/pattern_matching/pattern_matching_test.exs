defmodule Deft.PatternMatchingTest do
  @moduledoc """
  Tests for the pattern matching module.

  Pattern matching in Deft is bidirectional - patterns are checked against
  expected types and produce variable bindings.
  """
  use Deft.TypeCase, async: true

  alias Deft.PatternMatching

  describe "literal patterns" do
    test "integer literal matches integer type", %{ctx: ctx} do
      pattern = literal(42)
      {erased, type, bindings} = PatternMatching.handle_pattern(pattern, Type.integer(), ctx)

      assert erased == 42
      assert %Type.Integer{} = type
      assert bindings == []
    end

    test "boolean literal matches boolean type", %{ctx: ctx} do
      pattern = literal(true)
      {erased, type, bindings} = PatternMatching.handle_pattern(pattern, Type.boolean(), ctx)

      assert erased == true
      assert %Type.Boolean{} = type
      assert bindings == []
    end

    test "atom literal matches atom type", %{ctx: ctx} do
      pattern = literal(:ok)
      {erased, type, bindings} = PatternMatching.handle_pattern(pattern, Type.atom(), ctx)

      assert erased == :ok
      assert %Type.Atom{} = type
      assert bindings == []
    end

    test "float literal matches float type", %{ctx: ctx} do
      pattern = literal(3.14)
      {erased, type, bindings} = PatternMatching.handle_pattern(pattern, Type.float(), ctx)

      assert erased == 3.14
      assert %Type.Float{} = type
      assert bindings == []
    end

    test "integer literal matches number type", %{ctx: ctx} do
      # Integer <: Number, so an integer pattern should match Number
      pattern = literal(42)
      {erased, type, bindings} = PatternMatching.handle_pattern(pattern, Type.number(), ctx)

      assert erased == 42
      assert %Type.Integer{} = type
      assert bindings == []
    end

    test "literal pattern fails for incompatible type", %{ctx: ctx} do
      pattern = literal(42)

      assert_raise CompileError, fn ->
        PatternMatching.handle_pattern(pattern, Type.boolean(), ctx)
      end
    end
  end

  describe "variable patterns" do
    test "variable binds to subject type", %{ctx: ctx} do
      pattern = local(:x)
      {erased, type, bindings} = PatternMatching.handle_pattern(pattern, Type.integer(), ctx)

      assert {:x, _, _} = erased
      assert %Type.Integer{} = type
      assert [{^pattern, %Type.Integer{}}] = bindings
    end

    test "underscore pattern does not create binding", %{ctx: ctx} do
      pattern = local(:_)
      {erased, type, bindings} = PatternMatching.handle_pattern(pattern, Type.integer(), ctx)

      assert {:_, _, _} = erased
      assert %Type.Integer{} = type
      assert bindings == []
    end

    test "variable binds to union type", %{ctx: ctx} do
      pattern = local(:x)
      union_type = Type.Union.new(Type.integer(), Type.boolean())
      {_erased, type, bindings} = PatternMatching.handle_pattern(pattern, union_type, ctx)

      assert %Type.Union{} = type
      assert [{^pattern, %Type.Union{}}] = bindings
    end

    test "variable binds to function type", %{ctx: ctx} do
      pattern = local(:f)
      fn_type = Type.fun([Type.integer()], Type.boolean())
      {_erased, type, bindings} = PatternMatching.handle_pattern(pattern, fn_type, ctx)

      assert %Type.Fn{} = type
      assert [{^pattern, %Type.Fn{}}] = bindings
    end
  end

  describe "tuple patterns" do
    test "tuple pattern matches fixed tuple type", %{ctx: ctx} do
      pattern = tuple([local(:x), local(:y)])
      tuple_type = Type.fixed_tuple([Type.integer(), Type.boolean()])

      {erased, type, bindings} = PatternMatching.handle_pattern(pattern, tuple_type, ctx)

      assert {:{}, _, [_, _]} = erased
      assert %Type.FixedTuple{} = type
      assert length(bindings) == 2
    end

    test "tuple pattern extracts correct element types", %{ctx: ctx} do
      x = local(:x)
      y = local(:y)
      pattern = tuple([x, y])
      tuple_type = Type.fixed_tuple([Type.integer(), Type.boolean()])

      {_erased, _type, bindings} = PatternMatching.handle_pattern(pattern, tuple_type, ctx)

      x_binding = Enum.find(bindings, fn {var, _} -> var.name == :x end)
      y_binding = Enum.find(bindings, fn {var, _} -> var.name == :y end)

      assert {_, %Type.Integer{}} = x_binding
      assert {_, %Type.Boolean{}} = y_binding
    end

    test "nested tuple patterns", %{ctx: ctx} do
      inner = tuple([local(:a), local(:b)])
      pattern = tuple([inner, local(:c)])

      tuple_type =
        Type.fixed_tuple([
          Type.fixed_tuple([Type.integer(), Type.boolean()]),
          Type.atom()
        ])

      {_erased, type, bindings} = PatternMatching.handle_pattern(pattern, tuple_type, ctx)

      assert %Type.FixedTuple{} = type
      assert length(bindings) == 3
    end

    test "empty tuple pattern matches empty tuple type", %{ctx: ctx} do
      pattern = tuple([])
      tuple_type = Type.fixed_tuple([])

      {erased, type, bindings} = PatternMatching.handle_pattern(pattern, tuple_type, ctx)

      assert {:{}, _, []} = erased
      assert %Type.FixedTuple{elements: []} = type
      assert bindings == []
    end
  end

  describe "list patterns" do
    test "list pattern matches fixed list type", %{ctx: ctx} do
      pattern = list([local(:x), local(:y)])
      list_type = Type.fixed_list(Type.integer())

      {erased, type, bindings} = PatternMatching.handle_pattern(pattern, list_type, ctx)

      assert is_list(erased)
      assert %Type.FixedList{} = type
      assert length(bindings) == 2
    end

    test "empty list pattern matches fixed list type", %{ctx: ctx} do
      pattern = list([])
      list_type = Type.fixed_list(Type.integer())

      {erased, type, bindings} = PatternMatching.handle_pattern(pattern, list_type, ctx)

      assert erased == []
      assert %Type.FixedList{} = type
      assert bindings == []
    end
  end

  describe "pair patterns" do
    test "pair pattern matches two-element tuple", %{ctx: ctx} do
      pattern = pair(local(:x), local(:y))
      tuple_type = Type.fixed_tuple([Type.integer(), Type.boolean()])

      {_erased, type, bindings} = PatternMatching.handle_pattern(pattern, tuple_type, ctx)

      assert %Type.FixedTuple{} = type
      assert length(bindings) == 2
    end
  end

  describe "union type patterns" do
    test "variable pattern matches union and binds to union type", %{ctx: ctx} do
      pattern = local(:x)
      union_type = Type.Union.new(Type.integer(), Type.boolean())

      {_erased, type, bindings} = PatternMatching.handle_pattern(pattern, union_type, ctx)

      # The binding should be to the union type
      assert %Type.Union{} = type
      [{_, bound_type}] = bindings
      assert %Type.Union{} = bound_type
    end

    test "literal pattern can match one branch of union", %{ctx: ctx} do
      pattern = literal(42)
      union_type = Type.Union.new(Type.integer(), Type.boolean())

      {erased, type, bindings} = PatternMatching.handle_pattern(pattern, union_type, ctx)

      assert erased == 42
      assert %Type.Integer{} = type
      assert bindings == []
    end
  end

  describe "nested match patterns" do
    test "match pattern (x = value) creates binding", %{ctx: ctx} do
      # Pattern like: x = 42
      value = literal(42)
      pattern = match_expr(local(:x), value)

      {erased, type, bindings} = PatternMatching.handle_pattern(pattern, Type.integer(), ctx)

      assert {:=, _, _} = erased
      assert %Type.Integer{} = type
      assert length(bindings) == 1
    end

    test "match pattern with tuple", %{ctx: ctx} do
      # Pattern like: x = {1, 2}
      value = tuple([literal(1), literal(2)])
      pattern = match_expr(local(:x), value)
      tuple_type = Type.fixed_tuple([Type.integer(), Type.integer()])

      {erased, type, bindings} = PatternMatching.handle_pattern(pattern, tuple_type, ctx)

      assert {:=, _, _} = erased
      assert %Type.FixedTuple{} = type
      assert length(bindings) == 1
    end
  end

  describe "error cases" do
    test "incompatible literal type raises exception", %{ctx: ctx} do
      pattern = literal(42)

      assert_raise CompileError, fn ->
        PatternMatching.handle_pattern(pattern, Type.boolean(), ctx)
      end
    end

    test "error includes subject and pattern types in message", %{ctx: ctx} do
      pattern = literal(42)

      error =
        assert_raise CompileError, fn ->
          PatternMatching.handle_pattern(pattern, Type.boolean(), ctx)
        end

      assert error.description =~ "E0006"
      # Types are communicated via message and notes, not expected/actual fields
      assert error.description =~ "boolean"
      assert error.description =~ "integer"
    end
  end

  # ============================================================================
  # Unsupported Pattern/Type Combinations
  # ============================================================================

  describe "unsupported pattern/type combinations" do
    test "tuple pattern against list type raises descriptive error", %{ctx: ctx} do
      pattern = tuple([local(:a), local(:b)])

      error =
        assert_raise CompileError, fn ->
          PatternMatching.handle_pattern(pattern, Type.fixed_list(Type.integer()), ctx)
        end

      assert error.description =~ "E0011"
      assert error.description =~ "tuple pattern"
      assert error.description =~ "Tuple patterns can only match tuple types"
    end

    test "list pattern against tuple type raises descriptive error", %{ctx: ctx} do
      pattern = list([local(:a), local(:b)])

      error =
        assert_raise CompileError, fn ->
          PatternMatching.handle_pattern(pattern, Type.fixed_tuple([Type.integer(), Type.integer()]), ctx)
        end

      assert error.description =~ "E0011"
      assert error.description =~ "list pattern"
      assert error.description =~ "List patterns can only match list types"
    end

    test "error message includes type description", %{ctx: ctx} do
      pattern = tuple([local(:a)])

      error =
        assert_raise CompileError, fn ->
          PatternMatching.handle_pattern(pattern, Type.integer(), ctx)
        end

      assert error.description =~ "E0011"
      assert error.description =~ "integer"
    end
  end
end
