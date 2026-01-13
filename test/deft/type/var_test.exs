defmodule Deft.Type.VarTest do
  @moduledoc """
  Tests for type variables.
  """
  use ExUnit.Case, async: true

  alias Deft.Type
  alias Deft.Subtyping

  describe "construction" do
    test "creates a type variable with the given name" do
      var = Type.var(:a)

      assert %Type.Var{name: :a} = var
    end

    test "inspects as the variable name" do
      var = Type.var(:a)

      assert inspect(var) == "a"
    end
  end

  describe "subtyping" do
    test "a type variable is a subtype of itself" do
      a = Type.var(:a)

      assert Subtyping.subtype_of?(a, a)
    end

    test "different type variables are not subtypes" do
      a = Type.var(:a)
      b = Type.var(:b)

      refute Subtyping.subtype_of?(a, b)
      refute Subtyping.subtype_of?(b, a)
    end

    test "type variables are not subtypes of concrete types" do
      a = Type.var(:a)

      refute Subtyping.subtype_of?(Type.integer(), a)
      refute Subtyping.subtype_of?(Type.boolean(), a)
    end
  end

  describe "walkable" do
    test "type variables have no children" do
      a = Type.var(:a)

      assert Deft.Walkable.children(a) == []
    end

    test "rebuild returns the same node" do
      a = Type.var(:a)

      assert Deft.Walkable.rebuild(a, []) == a
    end
  end
end
