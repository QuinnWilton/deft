defmodule Deft.Property.SubtypingLawsTest do
  @moduledoc """
  Property-based tests for subtyping lattice laws.

  These tests verify that the subtyping relation satisfies the expected
  algebraic properties of a lattice.

  Note: `subtype_of?(t1, t2)` returns true if t2 is a subtype of t1.
  """
  use Deft.PropertyCase, async: true

  alias Deft.Generators.Types

  describe "reflexivity" do
    property "every type is a subtype of itself" do
      check all(type <- Types.type(), max_shrinking_steps: 0) do
        assert Subtyping.subtype_of?(type, type)
      end
    end

    property "every primitive type is a subtype of itself" do
      check all(type <- Types.primitive_type(), max_shrinking_steps: 0) do
        assert Subtyping.subtype_of?(type, type)
      end
    end

    property "every compound type is a subtype of itself" do
      check all(type <- Types.compound_type(), max_shrinking_steps: 0) do
        assert Subtyping.subtype_of?(type, type)
      end
    end
  end

  describe "Top type laws" do
    property "every type is a subtype of Top" do
      check all(type <- Types.type(), max_shrinking_steps: 0) do
        # t <: Top for all t
        assert Subtyping.subtype_of?(Type.top(), type)
      end
    end

    property "Top is only a subtype of itself" do
      check all(type <- Types.primitive_type(), max_shrinking_steps: 0) do
        # Top is not a subtype of any type except Top
        refute Subtyping.subtype_of?(type, Type.top())
      end
    end
  end

  describe "Bottom type laws" do
    property "Bottom is a subtype of every type" do
      check all(type <- Types.type(), max_shrinking_steps: 0) do
        # Bottom <: t for all t
        assert Subtyping.subtype_of?(type, Type.bottom())
      end
    end

    property "no type except Bottom is a subtype of Bottom" do
      check all(type <- Types.primitive_type(), max_shrinking_steps: 0) do
        # No primitive type is a subtype of Bottom
        refute Subtyping.subtype_of?(Type.bottom(), type)
      end
    end
  end

  describe "number hierarchy" do
    property "Integer and Float are subtypes of Number" do
      # Integer <: Number
      assert Subtyping.subtype_of?(Type.number(), Type.integer())
      # Float <: Number
      assert Subtyping.subtype_of?(Type.number(), Type.float())
    end

    property "Number is not a subtype of Integer or Float" do
      # Number is NOT <: Integer
      refute Subtyping.subtype_of?(Type.integer(), Type.number())
      # Number is NOT <: Float
      refute Subtyping.subtype_of?(Type.float(), Type.number())
    end
  end

  describe "function subtyping laws" do
    property "function types preserve reflexivity" do
      check all(fn_type <- Types.fn_type(), max_shrinking_steps: 0) do
        assert Subtyping.subtype_of?(fn_type, fn_type)
      end
    end

    property "function types with more general input are subtypes" do
      # fn(Number) -> T <: fn(Integer) -> T
      # because Integer <: Number (contravariance)
      check all(output <- Types.primitive_type(), max_shrinking_steps: 0) do
        fn_number_in = Type.fun([Type.number()], output)
        fn_integer_in = Type.fun([Type.integer()], output)

        assert Subtyping.subtype_of?(fn_integer_in, fn_number_in)
      end
    end

    property "function types with more specific output are subtypes" do
      # fn(T) -> Integer <: fn(T) -> Number
      # because Integer <: Number (covariance)
      check all(input <- Types.primitive_type(), max_shrinking_steps: 0) do
        fn_integer_out = Type.fun([input], Type.integer())
        fn_number_out = Type.fun([input], Type.number())

        assert Subtyping.subtype_of?(fn_number_out, fn_integer_out)
      end
    end
  end

  describe "tuple subtyping laws" do
    property "tuple types preserve reflexivity" do
      check all(tuple_type <- Types.fixed_tuple_type(), max_shrinking_steps: 0) do
        assert Subtyping.subtype_of?(tuple_type, tuple_type)
      end
    end

    property "fixed tuple is subtype of unbounded tuple" do
      check all(tuple_type <- Types.fixed_tuple_type(), max_shrinking_steps: 0) do
        assert Subtyping.subtype_of?(Type.tuple(), tuple_type)
      end
    end
  end

  describe "list subtyping laws" do
    property "list types preserve reflexivity" do
      check all(list_type <- Types.fixed_list_type(), max_shrinking_steps: 0) do
        assert Subtyping.subtype_of?(list_type, list_type)
      end
    end

    property "fixed list is subtype of unbounded list" do
      check all(list_type <- Types.fixed_list_type(), max_shrinking_steps: 0) do
        assert Subtyping.subtype_of?(Type.list(), list_type)
      end
    end
  end

  describe "union type laws" do
    property "union types preserve reflexivity" do
      check all(union_type <- Types.union_type(), max_shrinking_steps: 0) do
        assert Subtyping.subtype_of?(union_type, union_type)
      end
    end

    property "components are subtypes of their union" do
      check all(
              t1 <- Types.primitive_type(),
              t2 <- Types.primitive_type(),
              max_shrinking_steps: 0
            ) do
        union = Type.Union.new(t1, t2)

        # t1 <: (t1 | t2) and t2 <: (t1 | t2)
        # Using subtype_of?(union, t1) which checks if t1 <: union
        assert Subtyping.subtype_of?(union, t1)
        assert Subtyping.subtype_of?(union, t2)
      end
    end
  end

  describe "well-formedness" do
    property "generated types are well-formed" do
      check all(type <- Types.type(), max_shrinking_steps: 0) do
        assert Type.well_formed?(type)
      end
    end

    property "generated primitive types are well-formed" do
      check all(type <- Types.primitive_type(), max_shrinking_steps: 0) do
        assert Type.well_formed?(type)
      end
    end

    property "generated compound types are well-formed" do
      check all(type <- Types.compound_type(), max_shrinking_steps: 0) do
        assert Type.well_formed?(type)
      end
    end
  end

  describe "transitivity (limited)" do
    # Full transitivity is expensive to test with generators,
    # so we test specific chains that we know should hold.

    property "Integer -> Number -> Top transitivity" do
      # We know Integer <: Number and Number <: Top
      # Therefore Integer <: Top should hold
      assert Subtyping.subtype_of?(Type.number(), Type.integer())
      assert Subtyping.subtype_of?(Type.top(), Type.number())
      assert Subtyping.subtype_of?(Type.top(), Type.integer())
    end

    property "Bottom -> T -> Top transitivity" do
      check all(type <- Types.primitive_type(), max_shrinking_steps: 0) do
        # Bottom <: T and T <: Top
        assert Subtyping.subtype_of?(type, Type.bottom())
        assert Subtyping.subtype_of?(Type.top(), type)
        # Therefore Bottom <: Top
        assert Subtyping.subtype_of?(Type.top(), Type.bottom())
      end
    end
  end
end
