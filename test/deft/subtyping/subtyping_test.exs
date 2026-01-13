defmodule Deft.SubtypingTest do
  @moduledoc """
  Tests for the subtyping lattice.

  Note: `subtype_of?(t1, t2)` returns true if t2 is a subtype of t1.
  The lattice is standard:
  - Bottom <: T for all T (Bottom is the universal subtype)
  - T <: Top for all T (Top is the universal supertype)
  - Integer <: Number, Float <: Number (specific numeric types are subtypes of Number)

  The lattice is defined declaratively in each type module using Deft.Subtyping.DSL.
  These tests verify both the declared relationships and derived properties.
  """
  use Deft.TypeCase, async: true

  alias Deft.Subtyping.Lattice
  alias Deft.Type

  # ============================================================================
  # Auto-generated tests from lattice introspection
  # ============================================================================

  describe "lattice structure" do
    test "all declared edges are valid subtype relationships" do
      for {sub_mod, super_mod} <- Lattice.edges() do
        sub = sample_instance(sub_mod)
        super = sample_instance(super_mod)
        assert_subtype(sub, super, "declared edge #{inspect(sub_mod)} <: #{inspect(super_mod)}")
      end
    end

    # Create sample instances for types, handling parametric types.
    defp sample_instance(Type.FixedTuple), do: Type.fixed_tuple([Type.integer()])
    defp sample_instance(Type.FixedList), do: Type.fixed_list(Type.integer())
    defp sample_instance(Type.Fn), do: Type.fun([Type.integer()], Type.boolean())
    defp sample_instance(Type.Union), do: Type.Union.new(Type.integer(), Type.boolean())

    defp sample_instance(Type.Intersection),
      do: Type.Intersection.new(Type.integer(), Type.boolean())

    defp sample_instance(mod), do: mod.new()

    test "supertypes_of returns correct transitive closure" do
      # Integer's supertypes should include Number and Top
      supers = Lattice.supertypes_of(Type.Integer)
      assert Type.Number in supers
      assert Type.Top in supers
    end

    test "subtypes_of returns correct transitive closure" do
      # Number's subtypes should include Integer, Float, and Bottom
      subs = Lattice.subtypes_of(Type.Number)
      assert Type.Integer in subs
      assert Type.Float in subs
      assert Type.Bottom in subs
    end

    test "variance_of returns correct parameter variance" do
      # Fn has contravariant inputs, covariant output
      fn_variance = Lattice.variance_of(Type.Fn)
      assert fn_variance[:inputs] == :contravariant
      assert fn_variance[:output] == :covariant

      # FixedTuple has covariant elements
      tuple_variance = Lattice.variance_of(Type.FixedTuple)
      assert tuple_variance[:elements] == :covariant
    end

    test "has_structural_rule? identifies types with structural rules" do
      assert Lattice.has_structural_rule?(Type.Fn)
      assert Lattice.has_structural_rule?(Type.FixedTuple)
      assert Lattice.has_structural_rule?(Type.FixedList)
      refute Lattice.has_structural_rule?(Type.Integer)
      refute Lattice.has_structural_rule?(Type.Boolean)
    end
  end

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

  # ============================================================================
  # check_subtype/3 Detailed Error Reporting
  # ============================================================================

  alias Deft.Subtyping

  describe "check_subtype/3 path tracking" do
    test "returns :ok for valid subtype" do
      assert :ok = Subtyping.check_subtype(Type.number(), Type.integer(), nil)
    end

    test "returns mismatch info for invalid subtype" do
      assert {:mismatch, info} = Subtyping.check_subtype(Type.integer(), Type.boolean(), :expr)
      assert info.expected == Type.integer()
      assert info.actual == Type.boolean()
      assert info.expr == :expr
      assert info.path == []
    end

    test "FixedList mismatch includes element type info" do
      super = Type.fixed_list(Type.integer())
      sub = Type.fixed_list(Type.boolean())

      assert {:mismatch, info} = Subtyping.check_subtype(super, sub, :expr)
      assert info.expected == Type.integer()
      assert info.actual == Type.boolean()
    end

    test "FixedTuple mismatch includes path with element index" do
      super = Type.fixed_tuple([Type.integer(), Type.float()])
      sub = Type.fixed_tuple([Type.integer(), Type.boolean()])

      exprs = [:expr0, :expr1]
      assert {:mismatch, info} = Subtyping.check_subtype(super, sub, exprs)

      # Path should include index of mismatching element
      assert 1 in info.path
      assert info.expected == Type.float()
      assert info.actual == Type.boolean()
      assert info.expr == :expr1
    end

    test "FixedTuple first element mismatch" do
      super = Type.fixed_tuple([Type.float(), Type.integer()])
      sub = Type.fixed_tuple([Type.boolean(), Type.integer()])

      exprs = [:expr0, :expr1]
      assert {:mismatch, info} = Subtyping.check_subtype(super, sub, exprs)

      assert 0 in info.path
      assert info.expected == Type.float()
      assert info.actual == Type.boolean()
      assert info.expr == :expr0
    end

    test "Fn input mismatch includes :input in path" do
      # fn(Number) -> Boolean expected, fn(Integer) -> Boolean actual
      # This is a mismatch because inputs are contravariant:
      # fn(Integer) can only accept Integer, but fn(Number) must accept any Number
      super = Type.fun([Type.number()], Type.boolean())
      sub = Type.fun([Type.integer()], Type.boolean())

      exprs = [:input_expr]
      assert {:mismatch, info} = Subtyping.check_subtype(super, sub, exprs)

      # Path should include :input marker and index
      assert :input in info.path
    end

    test "Fn output mismatch" do
      super = Type.fun([Type.integer()], Type.integer())
      sub = Type.fun([Type.integer()], Type.boolean())

      assert {:mismatch, info} = Subtyping.check_subtype(super, sub, nil)
      assert info.expected == Type.integer()
      assert info.actual == Type.boolean()
    end

    test "Fn arity mismatch returns full type info" do
      super = Type.fun([Type.integer()], Type.boolean())
      sub = Type.fun([Type.integer(), Type.integer()], Type.boolean())

      assert {:mismatch, info} = Subtyping.check_subtype(super, sub, nil)
      assert info.expected == super
      assert info.actual == sub
    end

    test "nested tuple path tracking" do
      # Tuple containing a tuple: {Integer, {Float, Boolean}}
      # vs {Integer, {Float, Integer}}
      inner_super = Type.fixed_tuple([Type.float(), Type.boolean()])
      inner_sub = Type.fixed_tuple([Type.float(), Type.integer()])
      outer_super = Type.fixed_tuple([Type.integer(), inner_super])
      outer_sub = Type.fixed_tuple([Type.integer(), inner_sub])

      assert {:mismatch, info} = Subtyping.check_subtype(outer_super, outer_sub, nil)

      # Path should track nested indices
      assert 1 in info.path
      assert info.expected == Type.boolean()
      assert info.actual == Type.integer()
    end
  end
end
