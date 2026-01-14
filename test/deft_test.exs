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

  # Helper to create a context with default signatures for type checking.
  defp context_with_signatures(env) do
    signatures = Deft.TypeSystem.Default.all_signatures()

    Context.new(env)
    |> Context.with_signatures(signatures)
  end

  # ============================================================================
  # Expression Type Checking
  # ============================================================================

  property "computes the type for an expression" do
    env = __ENV__

    check all({expr, expected} <- Generators.Code.expression(), max_shrinking_steps: 0) do
      ctx = context_with_signatures(env)
      {:ok, _erased, actual, _bindings, _ctx} = TypeChecker.check(expr, ctx)
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
  # Program Metamorphic Properties
  # ============================================================================
  #
  # These tests verify that semantics-preserving program transformations
  # also preserve the inferred type. Each test generates a well-typed
  # expression, applies a transformation, and checks that the type is unchanged.

  alias Deft.AST

  # Transformation: e → (x = e; x)
  defp bind_and_return(expr, type) do
    x = AST.Local.new(:__meta_x__, __MODULE__, [])
    typed_x = AST.Local.new(:__meta_x__, __MODULE__, __deft_type__: type)
    match = AST.Match.new(x, expr)
    AST.Block.new([match, typed_x])
  end

  # Transformation: e → (_ = nil; e)
  defp prepend_dead_code(expr) do
    discard =
      AST.Match.new(
        AST.Local.new(:_, nil, []),
        AST.Literal.new(nil)
      )

    AST.Block.new([discard, expr])
  end

  # Transformation: e → (nil; e)
  defp prepend_nil(expr) do
    AST.Block.new([AST.Literal.new(nil), expr])
  end

  # Transformation: e → case e do x -> x end
  defp case_identity(expr, type) do
    x = AST.Local.new(:__meta_x__, __MODULE__, [])
    typed_x = AST.Local.new(:__meta_x__, __MODULE__, __deft_type__: type)
    branch = AST.CaseBranch.new(x, typed_x)
    AST.Case.new(expr, [branch])
  end

  # Transformation: e → if true do e else e end
  defp if_identical_branches(expr) do
    AST.If.new(AST.Literal.new(true), expr, expr)
  end

  # Transformation: e → (fn () -> e end).()
  defp thunk_and_force(expr) do
    thunk = AST.Fn.new(expr, [])
    AST.FnApplication.new(thunk, [])
  end

  # Transformation: e → (fn x :: T -> x end).(e)
  defp apply_identity(expr, type) do
    x = AST.Local.new(:__meta_x__, __MODULE__, [])
    typed_x = AST.Local.new(:__meta_x__, __MODULE__, __deft_type__: type)
    identity = AST.Fn.new(typed_x, [AST.Annotation.new(x, type)])
    AST.FnApplication.new(identity, [expr])
  end

  # Helper to assert type equivalence (mutual subtyping)
  defp assert_type_equivalent(type_a, type_b, message) do
    assert Subtyping.subtype_of?(type_a, type_b),
           "#{message}: #{inspect(type_a)} not <: #{inspect(type_b)}"

    assert Subtyping.subtype_of?(type_b, type_a),
           "#{message}: #{inspect(type_b)} not <: #{inspect(type_a)}"
  end

  property "binding and returning preserves type (e → x = e; x)" do
    env = __ENV__

    check all({expr, _} <- Generators.Code.expression(), max_shrinking_steps: 0) do
      ctx = context_with_signatures(env)

      {:ok, _, original_type, _, _} = TypeChecker.check(expr, ctx)

      transformed = bind_and_return(expr, original_type)
      {:ok, _, transformed_type, _, _} = TypeChecker.check(transformed, ctx)

      assert_type_equivalent(original_type, transformed_type, "bind_and_return")
    end
  end

  property "dead code does not affect type (e → _ = nil; e)" do
    env = __ENV__

    check all({expr, _} <- Generators.Code.expression(), max_shrinking_steps: 0) do
      ctx = context_with_signatures(env)

      {:ok, _, original_type, _, _} = TypeChecker.check(expr, ctx)

      transformed = prepend_dead_code(expr)
      {:ok, _, transformed_type, _, _} = TypeChecker.check(transformed, ctx)

      assert_type_equivalent(original_type, transformed_type, "prepend_dead_code")
    end
  end

  property "sequencing with nil preserves type (e → nil; e)" do
    env = __ENV__

    check all({expr, _} <- Generators.Code.expression(), max_shrinking_steps: 0) do
      ctx = context_with_signatures(env)

      {:ok, _, original_type, _, _} = TypeChecker.check(expr, ctx)

      transformed = prepend_nil(expr)
      {:ok, _, transformed_type, _, _} = TypeChecker.check(transformed, ctx)

      assert_type_equivalent(original_type, transformed_type, "prepend_nil")
    end
  end

  property "case with catch-all identity preserves type (e → case e do x -> x end)" do
    env = __ENV__

    check all({expr, _} <- Generators.Code.expression(), max_shrinking_steps: 0) do
      ctx = context_with_signatures(env)

      {:ok, _, original_type, _, _} = TypeChecker.check(expr, ctx)

      transformed = case_identity(expr, original_type)
      {:ok, _, transformed_type, _, _} = TypeChecker.check(transformed, ctx)

      assert_type_equivalent(original_type, transformed_type, "case_identity")
    end
  end

  property "if with identical branches preserves type (e → if true do e else e end)" do
    env = __ENV__

    check all({expr, _} <- Generators.Code.expression(), max_shrinking_steps: 0) do
      ctx = context_with_signatures(env)

      {:ok, _, original_type, _, _} = TypeChecker.check(expr, ctx)

      transformed = if_identical_branches(expr)
      {:ok, _, transformed_type, _, _} = TypeChecker.check(transformed, ctx)

      assert_type_equivalent(original_type, transformed_type, "if_identical_branches")
    end
  end

  property "thunk and force preserves type (e → (fn -> e end).())" do
    env = __ENV__

    check all({expr, _} <- Generators.Code.expression(), max_shrinking_steps: 0) do
      ctx = context_with_signatures(env)

      {:ok, _, original_type, _, _} = TypeChecker.check(expr, ctx)

      transformed = thunk_and_force(expr)
      {:ok, _, transformed_type, _, _} = TypeChecker.check(transformed, ctx)

      assert_type_equivalent(original_type, transformed_type, "thunk_and_force")
    end
  end

  property "identity function application preserves type (e → (fn x :: T -> x end).(e))" do
    env = __ENV__

    check all({expr, _} <- Generators.Code.expression(), max_shrinking_steps: 0) do
      ctx = context_with_signatures(env)

      {:ok, _, original_type, _, _} = TypeChecker.check(expr, ctx)

      transformed = apply_identity(expr, original_type)
      {:ok, _, transformed_type, _, _} = TypeChecker.check(transformed, ctx)

      assert_type_equivalent(original_type, transformed_type, "apply_identity")
    end
  end
end
