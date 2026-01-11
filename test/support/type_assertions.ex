defmodule Deft.TestHelpers.TypeAssertions do
  @moduledoc """
  Custom assertions for type checking tests.

  Provides convenient macros and functions for asserting type relationships,
  type checking results, and error conditions.
  """
  import ExUnit.Assertions

  alias Deft.Context
  alias Deft.Subtyping
  alias Deft.Type
  alias Deft.TypeChecker

  @doc """
  Asserts that type checking succeeds and returns the expected type.

  ## Examples

      assert_synthesizes(literal(42), Type.integer())
      assert_synthesizes(literal(42), Type.integer(), ctx)
  """
  defmacro assert_synthesizes(ast, expected_type, ctx \\ nil) do
    quote do
      ctx = unquote(ctx) || Context.new(__ENV__)
      result = TypeChecker.check(unquote(ast), ctx)

      assert {:ok, _erased, actual_type, _bindings, _ctx} = result,
             "Expected type checking to succeed, got: #{inspect(result)}"

      assert Subtyping.subtype_of?(unquote(expected_type), actual_type),
             "Expected type to be subtype of #{inspect(unquote(expected_type))}, got #{inspect(actual_type)}"

      actual_type
    end
  end

  @doc """
  Asserts that type checking succeeds and returns an exact type match.

  Unlike `assert_synthesizes/3`, this checks for exact equality rather than subtyping.
  """
  defmacro assert_synthesizes_exactly(ast, expected_type, ctx \\ nil) do
    quote do
      ctx = unquote(ctx) || Context.new(__ENV__)
      result = TypeChecker.check(unquote(ast), ctx)

      assert {:ok, _erased, actual_type, _bindings, _ctx} = result,
             "Expected type checking to succeed, got: #{inspect(result)}"

      assert unquote(expected_type) == actual_type,
             "Expected exact type #{inspect(unquote(expected_type))}, got #{inspect(actual_type)}"

      actual_type
    end
  end

  @doc """
  Asserts that type checking fails with the expected error type.

  ## Examples

      assert_type_error(bad_ast, Deft.TypecheckingError)
  """
  defmacro assert_type_error(ast, error_module, ctx \\ nil) do
    quote do
      ctx = unquote(ctx) || Context.new(__ENV__)

      assert_raise unquote(error_module), fn ->
        TypeChecker.check(unquote(ast), ctx)
      end
    end
  end

  @doc """
  Asserts that type checking returns an error tuple.

  ## Examples

      assert_check_error(bad_ast, :no_matching_rule)
  """
  defmacro assert_check_error(ast, error_tag, ctx \\ nil) do
    quote do
      ctx = unquote(ctx) || Context.new(__ENV__)
      result = TypeChecker.check(unquote(ast), ctx)

      assert {:error, {unquote(error_tag), _}} = result,
             "Expected error with tag #{inspect(unquote(error_tag))}, got: #{inspect(result)}"

      result
    end
  end

  @doc """
  Asserts that `sub` is a subtype of `super`.

  ## Examples

      assert_subtype(Type.integer(), Type.number())
      assert_subtype(Type.bottom(), Type.integer())
      assert_subtype(Type.integer(), Type.number(), "custom message")
  """
  def assert_subtype(sub, super, message \\ nil) do
    assert Subtyping.subtype_of?(super, sub),
           message || "Expected #{inspect(sub)} <: #{inspect(super)}, but subtyping does not hold"
  end

  @doc """
  Asserts that `sub` is NOT a subtype of `super`.

  ## Examples

      refute_subtype(Type.integer(), Type.boolean())
  """
  def refute_subtype(sub, super) do
    refute Subtyping.subtype_of?(super, sub),
           "Expected #{inspect(sub)} NOT <: #{inspect(super)}, but subtyping holds"
  end

  @doc """
  Asserts that two types are equal (mutual subtyping).

  ## Examples

      assert_type_equal(Type.integer(), Type.integer())
  """
  def assert_type_equal(t1, t2) do
    assert_subtype(t1, t2)
    assert_subtype(t2, t1)
  end

  @doc """
  Asserts that two types are NOT equal.
  """
  def refute_type_equal(t1, t2) do
    is_mutual_subtype =
      Subtyping.subtype_of?(t2, t1) and Subtyping.subtype_of?(t1, t2)

    refute is_mutual_subtype,
           "Expected #{inspect(t1)} != #{inspect(t2)}, but they are mutually subtypes"
  end

  @doc """
  Asserts that a type is well-formed.

  ## Examples

      assert_well_formed(Type.integer())
  """
  def assert_well_formed(type) do
    assert Type.well_formed?(type),
           "Expected #{inspect(type)} to be well-formed"
  end

  @doc """
  Asserts that type checking produces specific bindings.

  ## Examples

      assert_produces_bindings(ast, [{:x, Type.integer()}], ctx)
  """
  defmacro assert_produces_bindings(ast, expected_bindings, ctx \\ nil) do
    quote do
      ctx = unquote(ctx) || Context.new(__ENV__)
      result = TypeChecker.check(unquote(ast), ctx)

      assert {:ok, _erased, _type, actual_bindings, _ctx} = result,
             "Expected type checking to succeed, got: #{inspect(result)}"

      expected = unquote(expected_bindings)

      for {var, expected_type} <- expected do
        binding = Enum.find(actual_bindings, fn {v, _} -> v == var end)

        assert binding != nil,
               "Expected binding for #{inspect(var)}, but not found in #{inspect(actual_bindings)}"

        {_, actual_type} = binding

        assert Subtyping.subtype_of?(expected_type, actual_type),
               "Expected binding #{inspect(var)} to have type #{inspect(expected_type)}, got #{inspect(actual_type)}"
      end

      actual_bindings
    end
  end

  @doc """
  Asserts that type checking produces no bindings.
  """
  defmacro assert_no_bindings(ast, ctx \\ nil) do
    quote do
      ctx = unquote(ctx) || Context.new(__ENV__)
      result = TypeChecker.check(unquote(ast), ctx)

      assert {:ok, _erased, _type, [], _ctx} = result,
             "Expected no bindings, got: #{inspect(result)}"
    end
  end
end
