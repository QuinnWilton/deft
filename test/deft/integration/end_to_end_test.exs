defmodule Deft.Integration.EndToEndTest do
  @moduledoc """
  End-to-end integration tests that exercise the full Deft compilation pipeline.

  These tests verify that complete programs:
  1. Type check correctly
  2. Execute to produce expected values
  3. Infer the expected types
  """
  use Deft.IntegrationCase, async: true

  describe "basic expressions" do
    test "literals" do
      {result, type} = Deft.compile(do: 42)
      assert result == 42
      assert %Type.Integer{} = type

      {result, type} = Deft.compile(do: 3.14)
      assert result == 3.14
      assert %Type.Float{} = type

      {result, type} = Deft.compile(do: true)
      assert result == true
      assert %Type.Boolean{} = type

      {result, type} = Deft.compile(do: :hello)
      assert result == :hello
      assert %Type.Atom{} = type
    end

    test "arithmetic" do
      {result, type} = Deft.compile(do: 1 + 2)
      assert result == 3
      assert %Type.Integer{} = type

      {result, type} = Deft.compile(do: 5 - 3)
      assert result == 2
      assert %Type.Integer{} = type

      {result, type} = Deft.compile(do: 2 * 3)
      assert result == 6
      assert %Type.Integer{} = type
    end

    test "comparisons" do
      {result, type} = Deft.compile(do: 1 == 2)
      assert result == false
      assert %Type.Boolean{} = type

      {result, type} = Deft.compile(do: 3 > 2)
      assert result == true
      assert %Type.Boolean{} = type
    end
  end

  describe "variable binding" do
    test "simple binding with match" do
      {result, type} =
        Deft.compile do
          x = 42
          x
        end

      assert result == 42
      assert %Type.Integer{} = type
    end

    test "multiple bindings" do
      {result, type} =
        Deft.compile do
          x = 1
          y = 2
          x + y
        end

      assert result == 3
      assert %Type.Integer{} = type
    end

    test "tuple destructuring" do
      {result, type} =
        Deft.compile do
          {x, y} = {1, 2}
          x + y
        end

      assert result == 3
      assert %Type.Integer{} = type
    end
  end

  describe "functions" do
    test "simple anonymous function" do
      {result, type} =
        Deft.compile do
          f = fn x :: integer -> x + 1 end
          f.(5)
        end

      assert result == 6
      assert %Type.Integer{} = type
    end

    test "multi-argument function" do
      {result, type} =
        Deft.compile do
          add = fn x :: integer, y :: integer -> x + y end
          add.(3, 4)
        end

      assert result == 7
      assert %Type.Integer{} = type
    end

    test "higher-order function" do
      {result, type} =
        Deft.compile do
          apply_twice = fn f :: (integer -> integer), x :: integer -> f.(f.(x)) end
          inc = fn x :: integer -> x + 1 end
          apply_twice.(inc, 5)
        end

      assert result == 7
      assert %Type.Integer{} = type
    end

    test "function returning function" do
      {result, type} =
        Deft.compile do
          make_adder = fn n :: integer ->
            fn x :: integer -> x + n end
          end

          add5 = make_adder.(5)
          add5.(10)
        end

      assert result == 15
      assert %Type.Integer{} = type
    end
  end

  describe "control flow" do
    test "if expression with same type branches" do
      {result, type} =
        Deft.compile do
          if true do
            1
          else
            2
          end
        end

      assert result == 1
      assert %Type.Integer{} = type
    end

    test "if expression with different type branches" do
      {result, type} =
        Deft.compile do
          if true do
            1
          else
            :none
          end
        end

      assert result == 1
      # Union of Integer and Atom
      assert type.__struct__ in [Type.Integer, Type.Union]
    end

    test "cond expression" do
      {result, type} =
        Deft.compile do
          cond do
            false -> :a
            true -> :b
          end
        end

      assert result == :b
      assert %Type.Atom{} = type
    end

    test "case expression with wildcard" do
      {result, type} =
        Deft.compile do
          case 42 do
            x -> x + 1
          end
        end

      assert result == 43
      assert %Type.Integer{} = type
    end
  end

  describe "data structures" do
    test "tuple construction" do
      {result, type} =
        Deft.compile do
          {1, true, :hello}
        end

      assert result == {1, true, :hello}
      assert %Type.FixedTuple{} = type
      assert length(type.elements) == 3
    end

    test "list construction" do
      {result, type} =
        Deft.compile do
          [1, 2, 3]
        end

      assert result == [1, 2, 3]
      assert %Type.FixedList{} = type
    end

    test "nested structures" do
      {result, type} =
        Deft.compile do
          {{1, 2}, {3, 4}}
        end

      assert result == {{1, 2}, {3, 4}}
      assert %Type.FixedTuple{} = type
    end
  end

  describe "guards" do
    test "is_integer guard" do
      {result, type} =
        Deft.compile do
          is_integer(42)
        end

      assert result == true
      assert %Type.Boolean{} = type
    end

    test "is_atom guard" do
      {result, type} =
        Deft.compile do
          is_atom(:hello)
        end

      assert result == true
      assert %Type.Boolean{} = type
    end

    test "not operation" do
      {result, type} =
        Deft.compile do
          not true
        end

      assert result == false
      assert %Type.Boolean{} = type
    end
  end

  describe "type annotations" do
    test "function parameter annotation" do
      {result, type} =
        Deft.compile do
          f = fn x :: integer -> x + 1 end
          f.(10)
        end

      assert result == 11
      assert %Type.Integer{} = type
    end

    test "tuple type annotation" do
      {result, type} =
        Deft.compile do
          f = fn p :: {integer, boolean} -> elem(p, 0) end
          f.({42, true})
        end

      assert result == 42
      # elem returns union of tuple element types
      assert type.__struct__ in [Type.Integer, Type.Union]
    end
  end

  describe "complex examples" do
    test "list operations" do
      {result, type} =
        Deft.compile do
          xs = [1, 2, 3]
          {length(xs), hd(xs), tl(xs)}
        end

      assert result == {3, 1, [2, 3]}
      assert %Type.FixedTuple{} = type
    end

    test "tuple operations" do
      {result, type} =
        Deft.compile do
          t = {1, true, :x}
          {tuple_size(t), elem(t, 0), elem(t, 1)}
        end

      assert result == {3, 1, true}
      assert %Type.FixedTuple{} = type
    end
  end
end
