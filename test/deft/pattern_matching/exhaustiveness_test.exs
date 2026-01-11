defmodule Deft.ExhaustivenessTest do
  @moduledoc """
  Tests for exhaustiveness checking in case expressions.

  Exhaustiveness checking ensures that all possible values of the subject type
  are handled by at least one pattern.
  """
  use Deft.TypeCase, async: true

  alias Deft.Rules.ControlFlow

  describe "scalar type exhaustiveness" do
    test "single wildcard pattern is exhaustive for scalar type" do
      subject_type = Type.integer()
      pattern_types = [Type.integer()]

      # Should not raise
      ControlFlow.exhaustive_check!(subject_type, pattern_types)
    end

    test "matching pattern is exhaustive" do
      subject_type = Type.boolean()
      pattern_types = [Type.boolean()]

      # Should not raise
      ControlFlow.exhaustive_check!(subject_type, pattern_types)
    end

    test "subtype pattern is exhaustive" do
      # If we have a pattern for Number, it should cover Integer
      subject_type = Type.integer()
      pattern_types = [Type.number()]

      # Should not raise - Number covers Integer
      ControlFlow.exhaustive_check!(subject_type, pattern_types)
    end

    test "missing pattern raises InexhaustivePatterns" do
      subject_type = Type.integer()
      # Wrong type!
      pattern_types = [Type.boolean()]

      assert_raise Deft.Error.Exception, fn ->
        ControlFlow.exhaustive_check!(subject_type, pattern_types)
      end
    end

    test "error includes missing type" do
      subject_type = Type.integer()
      pattern_types = [Type.boolean()]

      error =
        assert_raise Deft.Error.Exception, fn ->
          ControlFlow.exhaustive_check!(subject_type, pattern_types)
        end

      assert error.error.code == :inexhaustive_patterns
      assert Enum.any?(error.error.suggestions, &(&1 =~ "integer"))
    end
  end

  describe "union type exhaustiveness" do
    test "all components covered is exhaustive" do
      union = Type.Union.new(Type.integer(), Type.boolean())
      pattern_types = [Type.integer(), Type.boolean()]

      # Should not raise
      ControlFlow.exhaustive_check!(union, pattern_types)
    end

    test "single pattern covering both components is exhaustive" do
      # A Number pattern should cover both Integer and Float
      union = Type.Union.new(Type.integer(), Type.float())
      pattern_types = [Type.number()]

      # Should not raise - Number covers both
      ControlFlow.exhaustive_check!(union, pattern_types)
    end

    test "missing component raises InexhaustivePatterns" do
      union = Type.Union.new(Type.integer(), Type.boolean())
      # Missing boolean
      pattern_types = [Type.integer()]

      assert_raise Deft.Error.Exception, fn ->
        ControlFlow.exhaustive_check!(union, pattern_types)
      end
    end

    test "error includes missing type from union" do
      union = Type.Union.new(Type.integer(), Type.boolean())
      pattern_types = [Type.integer()]

      error =
        assert_raise Deft.Error.Exception, fn ->
          ControlFlow.exhaustive_check!(union, pattern_types)
        end

      assert error.error.code == :inexhaustive_patterns
      assert Enum.any?(error.error.suggestions, &(&1 =~ "boolean"))
    end

    test "nested union requires coverage of all leaves" do
      # (Integer | Boolean) | Atom
      inner_union = Type.Union.new(Type.integer(), Type.boolean())
      outer_union = Type.Union.new(inner_union, Type.atom())

      # Cover all three
      pattern_types = [Type.integer(), Type.boolean(), Type.atom()]

      # Should not raise
      ControlFlow.exhaustive_check!(outer_union, pattern_types)
    end

    test "nested union missing one component raises error" do
      inner_union = Type.Union.new(Type.integer(), Type.boolean())
      outer_union = Type.Union.new(inner_union, Type.atom())

      # Missing atom
      pattern_types = [Type.integer(), Type.boolean()]

      assert_raise Deft.Error.Exception, fn ->
        ControlFlow.exhaustive_check!(outer_union, pattern_types)
      end
    end
  end

  describe "ADT exhaustiveness" do
    setup do
      circle = Type.variant(:circle, :shape, [Type.float()])
      rectangle = Type.variant(:rectangle, :shape, [Type.float(), Type.float()])
      triangle = Type.variant(:triangle, :shape, [Type.float(), Type.float(), Type.float()])
      shape_adt = Type.adt(:shape, [circle, rectangle, triangle])

      %{
        circle: circle,
        rectangle: rectangle,
        triangle: triangle,
        shape: shape_adt
      }
    end

    test "all variants covered is exhaustive", %{
      circle: circle,
      rectangle: rectangle,
      triangle: triangle,
      shape: shape
    } do
      pattern_types = [circle, rectangle, triangle]

      # Should not raise
      ControlFlow.exhaustive_check!(shape, pattern_types)
    end

    test "subset of variants covered raises error", %{circle: circle, shape: shape} do
      # Missing rectangle and triangle
      pattern_types = [circle]

      assert_raise Deft.Error.Exception, fn ->
        ControlFlow.exhaustive_check!(shape, pattern_types)
      end
    end

    test "error includes missing variant", %{
      circle: circle,
      rectangle: rectangle,
      triangle: _triangle,
      shape: shape
    } do
      # Missing triangle
      pattern_types = [circle, rectangle]

      error =
        assert_raise Deft.Error.Exception, fn ->
          ControlFlow.exhaustive_check!(shape, pattern_types)
        end

      assert error.error.code == :inexhaustive_patterns
      assert Enum.any?(error.error.suggestions, &(&1 =~ "triangle"))
    end

    test "order of patterns doesn't matter", %{
      circle: circle,
      rectangle: rectangle,
      triangle: triangle,
      shape: shape
    } do
      # Different order
      pattern_types = [triangle, circle, rectangle]

      # Should not raise
      ControlFlow.exhaustive_check!(shape, pattern_types)
    end

    test "duplicate patterns are okay", %{
      circle: circle,
      rectangle: rectangle,
      triangle: triangle,
      shape: shape
    } do
      # Duplicates
      pattern_types = [circle, circle, rectangle, triangle]

      # Should not raise
      ControlFlow.exhaustive_check!(shape, pattern_types)
    end
  end

  describe "feature gating" do
    test "exhaustiveness checking is enabled when feature is set", %{ctx: ctx} do
      assert Context.feature_enabled?(ctx, :exhaustiveness_checking)
    end

    test "context without feature has checking disabled" do
      ctx_without = Context.new(__ENV__, features: [])
      refute Context.feature_enabled?(ctx_without, :exhaustiveness_checking)
    end
  end
end
