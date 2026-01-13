defmodule Deft.Type.ForallTest do
  @moduledoc """
  Tests for universal quantification (forall types).
  """
  use ExUnit.Case, async: true

  alias Deft.Type
  alias Deft.Subtyping

  describe "construction" do
    test "creates a forall type with vars and body" do
      # forall a. a -> a
      a = Type.var(:a)
      body = Type.fun([a], a)
      forall = Type.forall([:a], body)

      assert %Type.Forall{vars: [:a], body: ^body} = forall
    end

    test "inspects with forall syntax" do
      a = Type.var(:a)
      forall = Type.forall([:a], Type.fun([a], a))

      assert inspect(forall) =~ "forall(a)"
    end
  end

  describe "instantiation" do
    test "substitutes type variables with concrete types" do
      # forall a. a -> a
      a = Type.var(:a)
      forall = Type.forall([:a], Type.fun([a], a))

      # Instantiate with integer
      result = Type.Forall.instantiate(forall, [Type.integer()])

      assert result == Type.fun([Type.integer()], Type.integer())
    end

    test "handles multiple type variables" do
      # forall a, b. a -> b
      a = Type.var(:a)
      b = Type.var(:b)
      forall = Type.forall([:a, :b], Type.fun([a], b))

      result = Type.Forall.instantiate(forall, [Type.integer(), Type.boolean()])

      assert result == Type.fun([Type.integer()], Type.boolean())
    end

    test "handles nested types" do
      # forall a. [a] -> a
      a = Type.var(:a)
      forall = Type.forall([:a], Type.fun([Type.fixed_list(a)], a))

      result = Type.Forall.instantiate(forall, [Type.integer()])

      assert result == Type.fun([Type.fixed_list(Type.integer())], Type.integer())
    end
  end

  describe "subtyping" do
    test "identical forall types are subtypes" do
      a = Type.var(:a)
      forall1 = Type.forall([:a], Type.fun([a], a))
      forall2 = Type.forall([:a], Type.fun([a], a))

      assert Subtyping.subtype_of?(forall1, forall2)
      assert Subtyping.subtype_of?(forall2, forall1)
    end

    test "alpha-equivalent forall types are subtypes" do
      # forall a. a -> a  <:  forall b. b -> b
      a = Type.var(:a)
      b = Type.var(:b)
      forall_a = Type.forall([:a], Type.fun([a], a))
      forall_b = Type.forall([:b], Type.fun([b], b))

      assert Subtyping.subtype_of?(forall_a, forall_b)
      assert Subtyping.subtype_of?(forall_b, forall_a)
    end

    test "forall types with different arities are not subtypes" do
      a = Type.var(:a)
      b = Type.var(:b)
      forall1 = Type.forall([:a], Type.fun([a], a))
      forall2 = Type.forall([:a, :b], Type.fun([a], b))

      refute Subtyping.subtype_of?(forall1, forall2)
      refute Subtyping.subtype_of?(forall2, forall1)
    end
  end

  describe "walkable" do
    test "body is the only child" do
      a = Type.var(:a)
      body = Type.fun([a], a)
      forall = Type.forall([:a], body)

      assert Deft.Walkable.children(forall) == [body]
    end

    test "rebuild updates the body" do
      a = Type.var(:a)
      forall = Type.forall([:a], Type.fun([a], a))
      new_body = Type.fun([Type.integer()], Type.integer())

      rebuilt = Deft.Walkable.rebuild(forall, [new_body])

      assert rebuilt.body == new_body
      assert rebuilt.vars == [:a]
    end
  end
end
