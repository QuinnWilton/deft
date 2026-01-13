defmodule Deft.UnificationTest do
  @moduledoc """
  Tests for type unification.

  Unification infers type variable bindings by matching polymorphic
  signatures against concrete argument types.
  """
  use ExUnit.Case, async: true

  alias Deft.Type
  alias Deft.Unification

  describe "infer/3 with simple types" do
    test "infers a single type variable from a direct match" do
      # Match 'a' against 'integer'
      {:ok, subst} = Unification.infer([:a], [Type.var(:a)], [Type.integer()])

      assert subst[:a] == Type.integer()
    end

    test "infers multiple type variables" do
      # Match (a, b) against (integer, boolean)
      templates = [Type.var(:a), Type.var(:b)]
      actuals = [Type.integer(), Type.boolean()]

      {:ok, subst} = Unification.infer([:a, :b], templates, actuals)

      assert subst[:a] == Type.integer()
      assert subst[:b] == Type.boolean()
    end

    test "returns error on arity mismatch" do
      templates = [Type.var(:a)]
      actuals = [Type.integer(), Type.boolean()]

      assert {:error, {:arity_mismatch, 1, 2}} = Unification.infer([:a], templates, actuals)
    end
  end

  describe "infer/3 with list types" do
    test "infers type variable from list contents" do
      # Match [a] against [integer]
      templates = [Type.fixed_list(Type.var(:a))]
      actuals = [Type.fixed_list(Type.integer())]

      {:ok, subst} = Unification.infer([:a], templates, actuals)

      assert subst[:a] == Type.integer()
    end

    test "infers type variable from nested lists" do
      # Match [[a]] against [[integer]]
      templates = [Type.fixed_list(Type.fixed_list(Type.var(:a)))]
      actuals = [Type.fixed_list(Type.fixed_list(Type.integer()))]

      {:ok, subst} = Unification.infer([:a], templates, actuals)

      assert subst[:a] == Type.integer()
    end

    test "binds to top when matching FixedList against generic List" do
      # Match [a] against list()
      templates = [Type.fixed_list(Type.var(:a))]
      actuals = [Type.list()]

      {:ok, subst} = Unification.infer([:a], templates, actuals)

      assert subst[:a] == Type.top()
    end
  end

  describe "infer/3 with function types" do
    test "infers type variables from function inputs" do
      # Match (a -> b) against (integer -> boolean)
      templates = [Type.fun([Type.var(:a)], Type.var(:b))]
      actuals = [Type.fun([Type.integer()], Type.boolean())]

      {:ok, subst} = Unification.infer([:a, :b], templates, actuals)

      assert subst[:a] == Type.integer()
      assert subst[:b] == Type.boolean()
    end

    test "returns error on function arity mismatch" do
      # Match (a -> b) against ((x, y) -> z)
      templates = [Type.fun([Type.var(:a)], Type.var(:b))]
      actuals = [Type.fun([Type.integer(), Type.boolean()], Type.atom())]

      assert {:error, _} = Unification.infer([:a, :b], templates, actuals)
    end
  end

  describe "infer/3 with tuple types" do
    test "infers type variables from tuple elements" do
      # Match {a, b} against {integer, boolean}
      templates = [Type.fixed_tuple([Type.var(:a), Type.var(:b)])]
      actuals = [Type.fixed_tuple([Type.integer(), Type.boolean()])]

      {:ok, subst} = Unification.infer([:a, :b], templates, actuals)

      assert subst[:a] == Type.integer()
      assert subst[:b] == Type.boolean()
    end

    test "returns error on tuple arity mismatch" do
      templates = [Type.fixed_tuple([Type.var(:a)])]
      actuals = [Type.fixed_tuple([Type.integer(), Type.boolean()])]

      assert {:error, _} = Unification.infer([:a], templates, actuals)
    end
  end

  describe "infer/3 with multiple occurrences" do
    test "accepts consistent bindings" do
      # Match (a, a) against (integer, integer)
      templates = [Type.var(:a), Type.var(:a)]
      actuals = [Type.integer(), Type.integer()]

      {:ok, subst} = Unification.infer([:a], templates, actuals)

      assert subst[:a] == Type.integer()
    end

    test "computes union for inconsistent bindings" do
      # Match (a, a) against (integer, boolean)
      templates = [Type.var(:a), Type.var(:a)]
      actuals = [Type.integer(), Type.boolean()]

      {:ok, subst} = Unification.infer([:a], templates, actuals)

      # Should be a union of integer and boolean
      assert %Type.Union{} = subst[:a]
    end
  end

  describe "infer/3 with subtyping fallback" do
    test "accepts when actual is subtype of template" do
      # Match 'number' against 'integer' (integer <: number)
      templates = [Type.number()]
      actuals = [Type.integer()]

      {:ok, _subst} = Unification.infer([], templates, actuals)
    end

    test "returns error when types are incompatible" do
      # Match 'integer' against 'boolean'
      templates = [Type.integer()]
      actuals = [Type.boolean()]

      assert {:error, {:type_mismatch, _, _}} = Unification.infer([], templates, actuals)
    end
  end

  describe "real-world scenarios" do
    test "hd: infer element type from list" do
      # forall a. [a] -> a
      # Called with [integer]
      templates = [Type.fixed_list(Type.var(:a))]
      actuals = [Type.fixed_list(Type.integer())]

      {:ok, subst} = Unification.infer([:a], templates, actuals)

      assert subst[:a] == Type.integer()
    end

    test "map: infer both input and output types" do
      # forall a, b. ([a], (a -> b)) -> [b]
      # Called with ([integer], (integer -> boolean))
      a = Type.var(:a)
      b = Type.var(:b)
      templates = [Type.fixed_list(a), Type.fun([a], b)]
      actuals = [
        Type.fixed_list(Type.integer()),
        Type.fun([Type.integer()], Type.boolean())
      ]

      {:ok, subst} = Unification.infer([:a, :b], templates, actuals)

      assert subst[:a] == Type.integer()
      assert subst[:b] == Type.boolean()
    end

    test "filter: same type variable in list and predicate" do
      # forall a. ([a], (a -> boolean)) -> [a]
      # Called with ([integer], (integer -> boolean))
      a = Type.var(:a)
      templates = [Type.fixed_list(a), Type.fun([a], Type.boolean())]
      actuals = [
        Type.fixed_list(Type.integer()),
        Type.fun([Type.integer()], Type.boolean())
      ]

      {:ok, subst} = Unification.infer([:a], templates, actuals)

      assert subst[:a] == Type.integer()
    end
  end
end
