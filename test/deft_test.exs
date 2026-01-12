defmodule DeftTest do
  @moduledoc """
  Property-based tests for the Deft type system.

  These tests verify core invariants of the type system using randomly
  generated types and expressions.
  """

  use ExUnit.Case
  use ExUnitProperties

  alias Deft.Context
  alias Deft.Generators
  alias Deft.Subtyping
  alias Deft.Type
  alias Deft.TypeChecker

  # ============================================================================
  # Expression Type Checking
  # ============================================================================

  property "computes the type for an expression" do
    env = __ENV__

    check all({expr, expected} <- Generators.Code.expression(), max_shrinking_steps: 0) do
      ctx = Context.new(env)
      {:ok, _erased, actual, _bindings, _ctx} = TypeChecker.check(expr, ctx)
      assert Type.well_formed?(actual)
      assert Subtyping.subtype_of?(expected, actual)
    end
  end

  # ============================================================================
  # Subtyping Properties
  # ============================================================================

  property "subtyping is reflexive (a <: a)" do
    check all(type <- Generators.Types.type()) do
      assert Subtyping.subtype_of?(type, type)
    end
  end

  property "subtyping is transitive (a <: b and b <: c implies a <: c)" do
    check all(
            a <- Generators.Types.type(),
            b <- Generators.Types.type(),
            c <- Generators.Types.type()
          ) do
      if Subtyping.subtype_of?(a, b) and Subtyping.subtype_of?(b, c) do
        assert Subtyping.subtype_of?(a, c),
               "Transitivity violated: #{inspect(a)} <: #{inspect(b)} and #{inspect(b)} <: #{inspect(c)} but not #{inspect(a)} <: #{inspect(c)}"
      end
    end
  end

  property "bottom is subtype of everything" do
    check all(type <- Generators.Types.type()) do
      # subtype_of?(super, sub) - is sub a subtype of super?
      assert Subtyping.subtype_of?(type, Type.bottom())
    end
  end

  property "everything is subtype of top" do
    check all(type <- Generators.Types.type()) do
      # subtype_of?(super, sub) - is sub a subtype of super?
      assert Subtyping.subtype_of?(Type.top(), type)
    end
  end

  # ============================================================================
  # Union Properties
  # ============================================================================

  property "union is commutative (a | b = b | a)" do
    check all(
            a <- Generators.Types.type(),
            b <- Generators.Types.type()
          ) do
      ab = Type.union(a, b)
      ba = Type.union(b, a)

      # Two types are equivalent if each is a subtype of the other.
      assert Subtyping.subtype_of?(ab, ba),
             "Union not commutative: #{inspect(ab)} not <: #{inspect(ba)}"

      assert Subtyping.subtype_of?(ba, ab),
             "Union not commutative: #{inspect(ba)} not <: #{inspect(ab)}"
    end
  end

  property "union is associative ((a | b) | c = a | (b | c))" do
    check all(
            a <- Generators.Types.type(),
            b <- Generators.Types.type(),
            c <- Generators.Types.type()
          ) do
      left = Type.union(Type.union(a, b), c)
      right = Type.union(a, Type.union(b, c))

      assert Subtyping.subtype_of?(left, right),
             "Union not associative: #{inspect(left)} not <: #{inspect(right)}"

      assert Subtyping.subtype_of?(right, left),
             "Union not associative: #{inspect(right)} not <: #{inspect(left)}"
    end
  end

  property "union with bottom is identity (a | never = a)" do
    check all(type <- Generators.Types.type()) do
      with_bottom = Type.union(type, Type.bottom())

      assert Subtyping.subtype_of?(with_bottom, type),
             "Union with bottom not identity: #{inspect(with_bottom)} not <: #{inspect(type)}"

      assert Subtyping.subtype_of?(type, with_bottom),
             "Union with bottom not identity: #{inspect(type)} not <: #{inspect(with_bottom)}"
    end
  end

  property "components are subtypes of union (a <: a | b)" do
    check all(
            a <- Generators.Types.type(),
            b <- Generators.Types.type()
          ) do
      union = Type.union(a, b)
      # subtype_of?(super, sub) - is sub a subtype of super?
      assert Subtyping.subtype_of?(union, a)
      assert Subtyping.subtype_of?(union, b)
    end
  end

  # ============================================================================
  # Intersection Properties
  # ============================================================================

  property "intersection is commutative (a & b = b & a)" do
    check all(
            a <- Generators.Types.type(),
            b <- Generators.Types.type()
          ) do
      ab = Type.intersection(a, b)
      ba = Type.intersection(b, a)

      assert Subtyping.subtype_of?(ab, ba),
             "Intersection not commutative: #{inspect(ab)} not <: #{inspect(ba)}"

      assert Subtyping.subtype_of?(ba, ab),
             "Intersection not commutative: #{inspect(ba)} not <: #{inspect(ab)}"
    end
  end

  property "intersection is associative ((a & b) & c = a & (b & c))" do
    check all(
            a <- Generators.Types.type(),
            b <- Generators.Types.type(),
            c <- Generators.Types.type()
          ) do
      left = Type.intersection(Type.intersection(a, b), c)
      right = Type.intersection(a, Type.intersection(b, c))

      assert Subtyping.subtype_of?(left, right),
             "Intersection not associative: #{inspect(left)} not <: #{inspect(right)}"

      assert Subtyping.subtype_of?(right, left),
             "Intersection not associative: #{inspect(right)} not <: #{inspect(left)}"
    end
  end

  property "intersection with top is identity (a & unknown = a)" do
    check all(type <- Generators.Types.type()) do
      with_top = Type.intersection(type, Type.top())

      # subtype_of?(super, sub) - is sub a subtype of super?
      # a & Top <: a (intersection is subtype of each component)
      assert Subtyping.subtype_of?(type, with_top),
             "Intersection with top not identity: #{inspect(with_top)} not <: #{inspect(type)}"

      # a <: a & Top (a is subtype of a & Top because a <: a and a <: Top)
      assert Subtyping.subtype_of?(with_top, type),
             "Intersection with top not identity: #{inspect(type)} not <: #{inspect(with_top)}"
    end
  end

  property "intersection is subtype of components (a & b <: a)" do
    check all(
            a <- Generators.Types.type(),
            b <- Generators.Types.type()
          ) do
      intersection = Type.intersection(a, b)
      # subtype_of?(super, sub) - is sub a subtype of super?
      assert Subtyping.subtype_of?(a, intersection)
      assert Subtyping.subtype_of?(b, intersection)
    end
  end

  # ============================================================================
  # Type Well-formedness
  # ============================================================================

  property "generated types are well-formed" do
    check all(type <- Generators.Types.type()) do
      assert Type.well_formed?(type)
    end
  end

  property "unions of well-formed types are well-formed" do
    check all(
            a <- Generators.Types.type(),
            b <- Generators.Types.type()
          ) do
      union = Type.union(a, b)
      assert Type.well_formed?(union)
    end
  end

  property "intersections of well-formed types are well-formed" do
    check all(
            a <- Generators.Types.type(),
            b <- Generators.Types.type()
          ) do
      intersection = Type.intersection(a, b)
      assert Type.well_formed?(intersection)
    end
  end
end
