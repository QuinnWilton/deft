defmodule Deft.Subtyping.PolymorphicTest do
  @moduledoc """
  Tests for subtyping of polymorphic types (Type.Var and Type.Forall).
  """
  use ExUnit.Case, async: true

  alias Deft.Type
  alias Deft.Subtyping

  describe "Type.Var subtyping" do
    test "type variable is subtype of itself" do
      a = Type.var(:a)

      assert Subtyping.subtype_of?(a, a)
    end

    test "different type variables are not subtypes" do
      a = Type.var(:a)
      b = Type.var(:b)

      refute Subtyping.subtype_of?(a, b)
      refute Subtyping.subtype_of?(b, a)
    end

    test "type variable is subtype of Top" do
      a = Type.var(:a)

      assert Subtyping.subtype_of?(Type.top(), a)
    end

    test "Bottom is subtype of type variable" do
      a = Type.var(:a)

      assert Subtyping.subtype_of?(a, Type.bottom())
    end
  end

  describe "Type.Forall subtyping (same type)" do
    test "identical forall types are subtypes" do
      a = Type.var(:a)
      forall1 = Type.forall([:a], Type.fun([a], a))
      forall2 = Type.forall([:a], Type.fun([a], a))

      assert Subtyping.subtype_of?(forall1, forall2)
      assert Subtyping.subtype_of?(forall2, forall1)
    end

    test "alpha-equivalent forall types are subtypes" do
      # forall a. a -> a  and  forall b. b -> b
      a = Type.var(:a)
      b = Type.var(:b)
      forall_a = Type.forall([:a], Type.fun([a], a))
      forall_b = Type.forall([:b], Type.fun([b], b))

      assert Subtyping.subtype_of?(forall_a, forall_b)
      assert Subtyping.subtype_of?(forall_b, forall_a)
    end

    test "forall types with different variable counts are not subtypes" do
      a = Type.var(:a)
      b = Type.var(:b)
      forall1 = Type.forall([:a], Type.fun([a], a))
      forall2 = Type.forall([:a, :b], Type.fun([a], b))

      refute Subtyping.subtype_of?(forall1, forall2)
      refute Subtyping.subtype_of?(forall2, forall1)
    end

    test "forall types with incompatible bodies are not subtypes" do
      a = Type.var(:a)
      # forall a. a -> a  vs  forall a. a -> integer
      forall1 = Type.forall([:a], Type.fun([a], a))
      forall2 = Type.forall([:a], Type.fun([a], Type.integer()))

      # These are NOT subtypes because the bodies differ structurally
      refute Subtyping.subtype_of?(forall1, forall2)
      refute Subtyping.subtype_of?(forall2, forall1)
    end
  end

  describe "Type.Forall with Top and Bottom" do
    test "forall is subtype of Top" do
      a = Type.var(:a)
      forall = Type.forall([:a], Type.fun([a], a))

      assert Subtyping.subtype_of?(Type.top(), forall)
    end

    test "Bottom is subtype of forall" do
      a = Type.var(:a)
      forall = Type.forall([:a], Type.fun([a], a))

      assert Subtyping.subtype_of?(forall, Type.bottom())
    end
  end

  describe "nested polymorphic types" do
    test "forall with list body" do
      # forall a. [a]  vs  forall b. [b]
      a = Type.var(:a)
      b = Type.var(:b)
      forall_a = Type.forall([:a], Type.fixed_list(a))
      forall_b = Type.forall([:b], Type.fixed_list(b))

      assert Subtyping.subtype_of?(forall_a, forall_b)
      assert Subtyping.subtype_of?(forall_b, forall_a)
    end

    test "forall with tuple body" do
      # forall a, b. {a, b}  vs  forall x, y. {x, y}
      a = Type.var(:a)
      b = Type.var(:b)
      x = Type.var(:x)
      y = Type.var(:y)
      forall_ab = Type.forall([:a, :b], Type.fixed_tuple([a, b]))
      forall_xy = Type.forall([:x, :y], Type.fixed_tuple([x, y]))

      assert Subtyping.subtype_of?(forall_ab, forall_xy)
      assert Subtyping.subtype_of?(forall_xy, forall_ab)
    end
  end
end
