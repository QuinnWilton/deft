defmodule Deft.Property.TypeAlgebraTest do
  @moduledoc """
  Property-based tests for type algebra operations.

  These tests verify that type constructors (union, intersection) satisfy
  expected algebraic properties.
  """
  use Deft.PropertyCase, async: true

  alias Deft.Generators.Types

  describe "union idempotence" do
    property "T | T = T" do
      check all(type <- Types.primitive_type(), max_shrinking_steps: 0) do
        union = Type.union(type, type)
        assert union == type
      end
    end
  end

  describe "union with bounds" do
    property "T | Bottom = T" do
      check all(type <- Types.primitive_type(), max_shrinking_steps: 0) do
        # Bottom is the identity for union
        union = Type.union(type, Type.bottom())
        # Should simplify since Bottom <: T for all T
        assert Subtyping.subtype_of?(union, type)
      end
    end

    property "T | Top = Top" do
      check all(type <- Types.primitive_type(), max_shrinking_steps: 0) do
        # Top is the absorbing element for union
        union = Type.union(type, Type.top())
        # Should simplify since T <: Top for all T
        assert Subtyping.subtype_of?(Type.top(), union)
      end
    end
  end

  describe "intersection idempotence" do
    property "T & T = T" do
      check all(type <- Types.primitive_type(), max_shrinking_steps: 0) do
        intersection = Type.intersection(type, type)
        assert intersection == type
      end
    end
  end

  describe "intersection with bounds" do
    property "T & Top = T" do
      check all(type <- Types.primitive_type(), max_shrinking_steps: 0) do
        # Top is the identity for intersection
        intersection = Type.intersection(type, Type.top())
        # Should simplify since T <: Top for all T
        assert Subtyping.subtype_of?(intersection, type)
      end
    end

    property "T & Bottom = Bottom (intersection with empty type)" do
      check all(type <- Types.primitive_type(), max_shrinking_steps: 0) do
        # Bottom is the empty type (no values). Intersection with empty = empty.
        # This is dual to union: T | Bottom = T, but T & Bottom = Bottom.
        intersection = Type.intersection(type, Type.bottom())
        assert intersection == Type.bottom()
      end
    end
  end

  describe "type construction consistency" do
    property "fixed_tuple preserves element types" do
      check all(
              types <- list_of(Types.primitive_type(), min_length: 1, max_length: 5),
              max_shrinking_steps: 0
            ) do
        tuple = Type.fixed_tuple(types)

        assert tuple.elements == types
      end
    end

    property "fun preserves input and output types" do
      check all(
              inputs <- list_of(Types.primitive_type(), max_length: 3),
              output <- Types.primitive_type(),
              max_shrinking_steps: 0
            ) do
        fn_type = Type.fun(inputs, output)

        assert fn_type.inputs == inputs
        assert fn_type.output == output
      end
    end

    property "fixed_list preserves element type" do
      check all(elem_type <- Types.primitive_type(), max_shrinking_steps: 0) do
        list = Type.fixed_list(elem_type)

        assert Type.FixedList.contents(list) == elem_type
      end
    end
  end

  describe "union normalization" do
    property "union of subtypes simplifies" do
      # Integer | Number should simplify to Number (since Integer <: Number)
      assert Type.union(Type.integer(), Type.number()) == Type.number()
      assert Type.union(Type.float(), Type.number()) == Type.number()
    end

    property "union is stable under subtyping" do
      check all(
              t1 <- Types.primitive_type(),
              t2 <- Types.primitive_type(),
              max_shrinking_steps: 0
            ) do
        union = Type.union(t1, t2)

        # Both components should be "covered" by the union
        assert Subtyping.subtype_of?(union, t1)
        assert Subtyping.subtype_of?(union, t2)
      end
    end
  end

end
