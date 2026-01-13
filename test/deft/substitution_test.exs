defmodule Deft.SubstitutionTest do
  @moduledoc """
  Tests for type variable substitution.

  Substitution replaces type variables with concrete types throughout
  a type expression using the Walkable protocol for traversal.
  """
  use ExUnit.Case, async: true

  alias Deft.Type
  alias Deft.Substitution

  describe "substitute/2" do
    test "returns type unchanged when substitution is empty" do
      type = Type.fun([Type.var(:a)], Type.var(:b))

      assert Substitution.substitute(type, %{}) == type
    end

    test "replaces a single type variable" do
      type = Type.var(:a)
      subst = %{a: Type.integer()}

      assert Substitution.substitute(type, subst) == Type.integer()
    end

    test "leaves unbound variables unchanged" do
      type = Type.var(:a)
      subst = %{b: Type.integer()}

      assert Substitution.substitute(type, subst) == Type.var(:a)
    end

    test "replaces variables in function types" do
      # (a, b) -> a
      type = Type.fun([Type.var(:a), Type.var(:b)], Type.var(:a))
      subst = %{a: Type.integer(), b: Type.boolean()}

      result = Substitution.substitute(type, subst)

      assert result == Type.fun([Type.integer(), Type.boolean()], Type.integer())
    end

    test "replaces variables in list types" do
      # [a]
      type = Type.fixed_list(Type.var(:a))
      subst = %{a: Type.integer()}

      result = Substitution.substitute(type, subst)

      assert result == Type.fixed_list(Type.integer())
    end

    test "replaces variables in tuple types" do
      # {a, b}
      type = Type.fixed_tuple([Type.var(:a), Type.var(:b)])
      subst = %{a: Type.integer(), b: Type.boolean()}

      result = Substitution.substitute(type, subst)

      assert result == Type.fixed_tuple([Type.integer(), Type.boolean()])
    end

    test "replaces variables in nested types" do
      # [[a]] -> [a]
      type =
        Type.fun(
          [Type.fixed_list(Type.fixed_list(Type.var(:a)))],
          Type.fixed_list(Type.var(:a))
        )

      subst = %{a: Type.integer()}

      result = Substitution.substitute(type, subst)

      expected =
        Type.fun(
          [Type.fixed_list(Type.fixed_list(Type.integer()))],
          Type.fixed_list(Type.integer())
        )

      assert result == expected
    end

    test "handles multiple variables with some unbound" do
      # (a, b, c) -> a
      type =
        Type.fun(
          [Type.var(:a), Type.var(:b), Type.var(:c)],
          Type.var(:a)
        )

      # Only substitute a and c
      subst = %{a: Type.integer(), c: Type.boolean()}

      result = Substitution.substitute(type, subst)

      expected =
        Type.fun(
          [Type.integer(), Type.var(:b), Type.boolean()],
          Type.integer()
        )

      assert result == expected
    end

    test "does not substitute inside forall body vars list" do
      # forall a. a -> a  with subst {a: integer}
      # The vars [:a] should remain unchanged, body gets substituted
      a = Type.var(:a)
      forall = Type.forall([:a], Type.fun([a], a))
      subst = %{a: Type.integer()}

      result = Substitution.substitute(forall, subst)

      # The forall's vars list is atoms, not Type.Var, so unaffected.
      # The body IS substituted since we're doing a full traversal.
      assert result.vars == [:a]
      assert result.body == Type.fun([Type.integer()], Type.integer())
    end
  end
end
