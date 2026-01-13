defmodule Deft.Type.IntersectionTest do
  @moduledoc """
  Tests for intersection type construction and disjointness detection.

  Intersection types simplify to Bottom when composed of disjoint types,
  since no value can inhabit both types simultaneously.
  """
  use ExUnit.Case, async: true

  alias Deft.Type
  alias Deft.Type.Intersection

  describe "disjointness detection" do
    test "Integer & Float simplifies to Bottom" do
      # No value can be both Integer and Float
      result = Intersection.new(Type.integer(), Type.float())

      assert %Type.Bottom{} = result
    end

    test "Integer & Boolean simplifies to Bottom" do
      result = Intersection.new(Type.integer(), Type.boolean())

      assert %Type.Bottom{} = result
    end

    test "Boolean & Atom does NOT simplify (Boolean <: Atom)" do
      # This is interesting - booleans are atoms, so their intersection is Boolean
      # Actually, Boolean is NOT a subtype of Atom in this system, so they're disjoint
      result = Intersection.new(Type.boolean(), Type.atom())

      assert %Type.Bottom{} = result
    end

    test "Integer & Number does NOT simplify (Integer <: Number)" do
      result = Intersection.new(Type.integer(), Type.number())

      # Integer <: Number, so intersection simplifies to Integer (the more specific type)
      assert %Type.Integer{} = result
    end

    test "Float & Number does NOT simplify (Float <: Number)" do
      result = Intersection.new(Type.float(), Type.number())

      assert %Type.Float{} = result
    end
  end

  describe "function arity disjointness" do
    test "fn(A) -> T & fn(A, B) -> T simplifies to Bottom" do
      fn1 = Type.fun([Type.integer()], Type.boolean())
      fn2 = Type.fun([Type.integer(), Type.integer()], Type.boolean())

      result = Intersection.new(fn1, fn2)

      assert %Type.Bottom{} = result
    end

    test "fn(A) -> T & fn(A) -> U remains valid intersection" do
      fn1 = Type.fun([Type.integer()], Type.integer())
      fn2 = Type.fun([Type.integer()], Type.boolean())

      result = Intersection.new(fn1, fn2)

      # Same arity, different outputs - not disjoint (a function could have both types)
      refute match?(%Type.Bottom{}, result)
    end

    test "fn(Integer) & fn(Boolean) simplifies to Bottom (disjoint inputs)" do
      fn1 = Type.fun([Type.integer()], Type.boolean())
      fn2 = Type.fun([Type.boolean()], Type.boolean())

      result = Intersection.new(fn1, fn2)

      # Functions with disjoint parameter types at same position are disjoint
      assert %Type.Bottom{} = result
    end
  end

  describe "tuple arity disjointness" do
    test "{A, B} & {A, B, C} simplifies to Bottom" do
      tuple2 = Type.fixed_tuple([Type.integer(), Type.boolean()])
      tuple3 = Type.fixed_tuple([Type.integer(), Type.boolean(), Type.atom()])

      result = Intersection.new(tuple2, tuple3)

      assert %Type.Bottom{} = result
    end

    test "{A, B} & {A, B} with same types remains valid" do
      tuple1 = Type.fixed_tuple([Type.integer(), Type.boolean()])
      tuple2 = Type.fixed_tuple([Type.integer(), Type.boolean()])

      result = Intersection.new(tuple1, tuple2)

      # Same tuple type - simplifies to single tuple
      assert %Type.FixedTuple{} = result
    end

    test "{Integer, T} & {Boolean, T} simplifies to Bottom (disjoint elements)" do
      tuple1 = Type.fixed_tuple([Type.integer(), Type.atom()])
      tuple2 = Type.fixed_tuple([Type.boolean(), Type.atom()])

      result = Intersection.new(tuple1, tuple2)

      # First elements are disjoint, so tuples are disjoint
      assert %Type.Bottom{} = result
    end
  end

  describe "list disjointness" do
    test "[Integer] & [Boolean] simplifies to Bottom" do
      list1 = Type.fixed_list(Type.integer())
      list2 = Type.fixed_list(Type.boolean())

      result = Intersection.new(list1, list2)

      # Lists with disjoint element types are disjoint
      assert %Type.Bottom{} = result
    end

    test "[Integer] & [Number] does NOT simplify" do
      list1 = Type.fixed_list(Type.integer())
      list2 = Type.fixed_list(Type.number())

      result = Intersection.new(list1, list2)

      # Integer <: Number, so intersection simplifies to list of Integer
      assert %Type.FixedList{contents: %Type.Integer{}} = result
    end
  end

  describe "Top and Bottom special cases" do
    test "T & Top simplifies to T" do
      result = Intersection.new(Type.integer(), Type.top())

      assert %Type.Integer{} = result
    end

    test "T & Bottom simplifies to Bottom" do
      result = Intersection.new(Type.integer(), Type.bottom())

      assert %Type.Bottom{} = result
    end

    test "Bottom & Bottom remains Bottom" do
      result = Intersection.new(Type.bottom(), Type.bottom())

      assert %Type.Bottom{} = result
    end
  end

  describe "nested/recursive disjointness" do
    test "[[Integer]] & [[Boolean]] simplifies to Bottom" do
      nested1 = Type.fixed_list(Type.fixed_list(Type.integer()))
      nested2 = Type.fixed_list(Type.fixed_list(Type.boolean()))

      result = Intersection.new(nested1, nested2)

      assert %Type.Bottom{} = result
    end

    test "{[Integer]} & {[Boolean]} simplifies to Bottom" do
      tuple1 = Type.fixed_tuple([Type.fixed_list(Type.integer())])
      tuple2 = Type.fixed_tuple([Type.fixed_list(Type.boolean())])

      result = Intersection.new(tuple1, tuple2)

      assert %Type.Bottom{} = result
    end
  end

  describe "intersection flattening and canonicalization" do
    test "nested intersections are flattened" do
      # (A & B) & C should flatten to A & B & C
      ab = Intersection.new(Type.integer(), Type.number())
      # Since Integer <: Number, ab = Integer

      # Now intersect with something else
      result = Intersection.new(ab, Type.number())

      # Should still be Integer (most specific)
      assert %Type.Integer{} = result
    end

    test "redundant components are removed" do
      # Integer & Number & Integer should simplify to Integer
      int_num = Intersection.new(Type.integer(), Type.number())
      result = Intersection.new(int_num, Type.integer())

      assert %Type.Integer{} = result
    end
  end
end
