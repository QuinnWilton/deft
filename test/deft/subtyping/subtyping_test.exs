defmodule Deft.SubtypingTest do
  @moduledoc """
  Tests for the subtyping lattice.

  Note: `subtype_of?(t1, t2)` returns true if t2 is a subtype of t1.
  The lattice is standard:
  - Bottom <: T for all T (Bottom is the universal subtype)
  - T <: Top for all T (Top is the universal supertype)
  - Integer <: Number, Float <: Number (specific numeric types are subtypes of Number)
  """
  use Deft.TypeCase, async: true

  alias Deft.Type

  describe "reflexivity" do
    test "primitive types are subtypes of themselves" do
      for type <- [
            Type.integer(),
            Type.float(),
            Type.boolean(),
            Type.atom(),
            Type.number(),
            Type.top(),
            Type.bottom()
          ] do
        assert_subtype(type, type)
      end
    end

    test "function types are subtypes of themselves" do
      fn_type = Type.fun([Type.integer()], Type.boolean())
      assert_subtype(fn_type, fn_type)
    end

    test "tuple types are subtypes of themselves" do
      tuple_type = Type.fixed_tuple([Type.integer(), Type.boolean()])
      assert_subtype(tuple_type, tuple_type)
    end

    test "list types are subtypes of themselves" do
      list_type = Type.fixed_list(Type.integer())
      assert_subtype(list_type, list_type)
    end

    test "union types are subtypes of themselves" do
      union_type = Type.Union.new(Type.integer(), Type.boolean())
      assert_subtype(union_type, union_type)
    end
  end

  describe "Top type" do
    test "any type is a subtype of Top" do
      for type <- [
            Type.integer(),
            Type.float(),
            Type.boolean(),
            Type.atom(),
            Type.number(),
            Type.bottom()
          ] do
        assert_subtype(type, Type.top())
      end
    end

    test "compound types are subtypes of Top" do
      assert_subtype(Type.fun([Type.integer()], Type.boolean()), Type.top())
      assert_subtype(Type.fixed_tuple([Type.integer()]), Type.top())
      assert_subtype(Type.fixed_list(Type.integer()), Type.top())
    end

    test "Top is NOT a subtype of primitive types" do
      refute_subtype(Type.top(), Type.integer())
      refute_subtype(Type.top(), Type.boolean())
    end
  end

  describe "Bottom type" do
    test "Bottom is a subtype of any type" do
      for type <- [
            Type.integer(),
            Type.float(),
            Type.boolean(),
            Type.atom(),
            Type.number(),
            Type.top()
          ] do
        assert_subtype(Type.bottom(), type)
      end
    end

    test "Bottom is a subtype of compound types" do
      assert_subtype(Type.bottom(), Type.fun([Type.integer()], Type.boolean()))
      assert_subtype(Type.bottom(), Type.fixed_tuple([Type.integer()]))
      assert_subtype(Type.bottom(), Type.fixed_list(Type.integer()))
    end

    test "primitive types are NOT subtypes of Bottom" do
      refute_subtype(Type.integer(), Type.bottom())
      refute_subtype(Type.boolean(), Type.bottom())
    end
  end

  describe "number hierarchy" do
    test "Integer is a subtype of Number" do
      assert_subtype(Type.integer(), Type.number())
    end

    test "Float is a subtype of Number" do
      assert_subtype(Type.float(), Type.number())
    end

    test "Number is NOT a subtype of Integer" do
      refute_subtype(Type.number(), Type.integer())
    end

    test "Number is NOT a subtype of Float" do
      refute_subtype(Type.number(), Type.float())
    end

    test "Integer is NOT a subtype of Float" do
      refute_subtype(Type.integer(), Type.float())
    end

    test "Float is NOT a subtype of Integer" do
      refute_subtype(Type.float(), Type.integer())
    end
  end

  describe "primitive type distinctness" do
    test "Integer is NOT a subtype of Boolean" do
      refute_subtype(Type.integer(), Type.boolean())
    end

    test "Boolean is NOT a subtype of Atom" do
      refute_subtype(Type.boolean(), Type.atom())
    end

    test "Atom is NOT a subtype of Integer" do
      refute_subtype(Type.atom(), Type.integer())
    end

    test "Float is NOT a subtype of Boolean" do
      refute_subtype(Type.float(), Type.boolean())
    end
  end

  describe "function subtyping" do
    test "inputs are contravariant" do
      # fn(Number) -> Boolean <: fn(Integer) -> Boolean
      # because Integer <: Number (we can pass Integer where Number expected)
      fn_number_in = Type.fun([Type.number()], Type.boolean())
      fn_integer_in = Type.fun([Type.integer()], Type.boolean())

      assert_subtype(fn_number_in, fn_integer_in)
      refute_subtype(fn_integer_in, fn_number_in)
    end

    test "outputs are covariant" do
      # fn(Integer) -> Integer <: fn(Integer) -> Number
      # because Integer <: Number
      fn_integer_out = Type.fun([Type.integer()], Type.integer())
      fn_number_out = Type.fun([Type.integer()], Type.number())

      assert_subtype(fn_integer_out, fn_number_out)
      refute_subtype(fn_number_out, fn_integer_out)
    end

    test "arity must match" do
      fn_one_arg = Type.fun([Type.integer()], Type.boolean())
      fn_two_args = Type.fun([Type.integer(), Type.integer()], Type.boolean())

      refute_subtype(fn_one_arg, fn_two_args)
      refute_subtype(fn_two_args, fn_one_arg)
    end

    test "combined variance" do
      # fn(Number) -> Integer <: fn(Integer) -> Number
      # - Inputs: contravariant, need Integer <: Number (true)
      # - Outputs: covariant, need Integer <: Number (true)
      fn_num_in_int_out = Type.fun([Type.number()], Type.integer())
      fn_int_in_num_out = Type.fun([Type.integer()], Type.number())

      assert_subtype(fn_num_in_int_out, fn_int_in_num_out)
    end
  end

  describe "union types" do
    test "components are subtypes of their union" do
      union = Type.Union.new(Type.integer(), Type.boolean())

      assert_subtype(Type.integer(), union)
      assert_subtype(Type.boolean(), union)
    end

    test "union is NOT a subtype of its components" do
      union = Type.Union.new(Type.integer(), Type.boolean())

      refute_subtype(union, Type.integer())
      refute_subtype(union, Type.boolean())
    end

    test "unrelated type is NOT a subtype of union" do
      union = Type.Union.new(Type.integer(), Type.boolean())

      refute_subtype(Type.float(), union)
      refute_subtype(Type.atom(), union)
    end

    test "union subtyping is symmetric for identical unions" do
      union1 = Type.Union.new(Type.integer(), Type.boolean())
      union2 = Type.Union.new(Type.boolean(), Type.integer())

      assert_subtype(union1, union1)
      assert_subtype(union2, union2)
    end
  end

  describe "intersection types" do
    test "intersection is a subtype of both components" do
      # (A & B) <: A and (A & B) <: B
      intersection = Type.Intersection.new(Type.integer(), Type.boolean())

      assert_subtype(intersection, Type.integer())
      assert_subtype(intersection, Type.boolean())
    end

    test "component is NOT a subtype of intersection" do
      intersection = Type.Intersection.new(Type.integer(), Type.boolean())

      refute_subtype(Type.integer(), intersection)
      refute_subtype(Type.boolean(), intersection)
    end
  end

  describe "fixed tuple types" do
    test "element-wise subtyping" do
      # (Integer, Boolean) <: (Number, Boolean) because Integer <: Number
      tuple_int = Type.fixed_tuple([Type.integer(), Type.boolean()])
      tuple_num = Type.fixed_tuple([Type.number(), Type.boolean()])

      assert_subtype(tuple_int, tuple_num)
    end

    test "reverse direction does not hold" do
      tuple_int = Type.fixed_tuple([Type.integer(), Type.boolean()])
      tuple_num = Type.fixed_tuple([Type.number(), Type.boolean()])

      refute_subtype(tuple_num, tuple_int)
    end

    test "arity must match" do
      tuple2 = Type.fixed_tuple([Type.integer(), Type.boolean()])
      tuple3 = Type.fixed_tuple([Type.integer(), Type.boolean(), Type.atom()])

      refute_subtype(tuple2, tuple3)
      refute_subtype(tuple3, tuple2)
    end

    test "empty tuples are subtypes of each other" do
      empty1 = Type.fixed_tuple([])
      empty2 = Type.fixed_tuple([])

      assert_subtype(empty1, empty2)
    end
  end

  describe "fixed list types" do
    test "element-wise subtyping" do
      # [Integer] <: [Number] because Integer <: Number
      list_int = Type.fixed_list(Type.integer())
      list_num = Type.fixed_list(Type.number())

      assert_subtype(list_int, list_num)
    end

    test "reverse direction does not hold" do
      list_int = Type.fixed_list(Type.integer())
      list_num = Type.fixed_list(Type.number())

      refute_subtype(list_num, list_int)
    end

    test "different element types are not subtypes" do
      list_int = Type.fixed_list(Type.integer())
      list_bool = Type.fixed_list(Type.boolean())

      refute_subtype(list_int, list_bool)
      refute_subtype(list_bool, list_int)
    end
  end

  describe "unbounded types" do
    test "FixedTuple is subtype of unbounded Tuple" do
      # Specific FixedTuple <: general Tuple
      assert_subtype(Type.fixed_tuple([Type.integer()]), Type.tuple())
      assert_subtype(Type.fixed_tuple([]), Type.tuple())
    end

    test "FixedList is subtype of unbounded List" do
      # Specific FixedList <: general List
      assert_subtype(Type.fixed_list(Type.integer()), Type.list())
    end

    test "unbounded Tuple is NOT subtype of FixedTuple" do
      refute_subtype(Type.tuple(), Type.fixed_tuple([Type.integer()]))
    end

    test "unbounded List is NOT subtype of FixedList" do
      refute_subtype(Type.list(), Type.fixed_list(Type.integer()))
    end
  end

  describe "ADT and Variant types" do
    setup do
      circle = Type.variant(:circle, :shape, [Type.float()])
      rectangle = Type.variant(:rectangle, :shape, [Type.float(), Type.float()])
      shape_adt = Type.adt(:shape, [circle, rectangle])

      %{circle: circle, rectangle: rectangle, shape: shape_adt}
    end

    test "variant is a subtype of its ADT", %{circle: circle, rectangle: rectangle, shape: shape} do
      assert_subtype(circle, shape)
      assert_subtype(rectangle, shape)
    end

    test "ADT is NOT a subtype of its variants", %{circle: circle, shape: shape} do
      refute_subtype(shape, circle)
    end

    test "variants from different ADTs are not related" do
      circle1 = Type.variant(:circle, :shape1, [Type.float()])
      circle2 = Type.variant(:circle, :shape2, [Type.float()])

      refute_subtype(circle1, circle2)
      refute_subtype(circle2, circle1)
    end
  end

  describe "transitivity" do
    test "Integer -> Number -> Top chain" do
      # Integer <: Number and Number <: Top
      assert_subtype(Type.integer(), Type.number())
      assert_subtype(Type.number(), Type.top())
      # Therefore Integer <: Top
      assert_subtype(Type.integer(), Type.top())
    end

    test "Bottom -> Integer -> Number chain" do
      # Bottom <: Integer and Integer <: Number
      assert_subtype(Type.bottom(), Type.integer())
      assert_subtype(Type.integer(), Type.number())
      # Therefore Bottom <: Number
      assert_subtype(Type.bottom(), Type.number())
    end
  end
end
