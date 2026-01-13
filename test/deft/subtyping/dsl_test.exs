defmodule Deft.Subtyping.DSLTest do
  @moduledoc """
  Tests for the subtyping DSL, including manual structural rules and variance handling.
  """
  use ExUnit.Case, async: true

  alias Deft.Subtyping

  # ============================================================================
  # Test Types for Manual Structural Rule
  # ============================================================================

  defmodule ManualStructuralType do
    @moduledoc """
    A test type that uses a manual structural_rule/1 escape hatch.
    This simulates a type with custom subtyping logic that cannot be expressed
    via simple variance annotations.
    """
    use Deft.Subtyping.DSL

    # Custom structural rule: subtyping holds if values are within a threshold
    structural_rule(fn sub, super ->
      abs(sub.value - super.value) <= super.tolerance
    end)

    defstruct [:value, :tolerance]
  end

  # ============================================================================
  # Test Types for Invariant Variance
  # ============================================================================

  defmodule InvariantContainer do
    @moduledoc """
    A test type with an invariant parameter.
    Invariant means the parameter types must match exactly for subtyping.
    This models types like mutable references where both reading and writing require exact types.
    """
    use Deft.Subtyping.DSL

    parameter(:contents, variance: :invariant)

    defstruct [:contents]
  end

  describe "structural_rule/1 manual escape hatch" do
    test "accepts valid subtype relationship" do
      sub = %ManualStructuralType{value: 5, tolerance: 0}
      super = %ManualStructuralType{value: 7, tolerance: 3}

      # sub.value (5) is within super.tolerance (3) of super.value (7)
      # |5 - 7| = 2 <= 3, so subtyping holds
      assert ManualStructuralType.structural_subtype?(sub, super)
      assert Subtyping.subtype_of?(super, sub)
    end

    test "rejects invalid subtype relationship" do
      sub = %ManualStructuralType{value: 5, tolerance: 0}
      super = %ManualStructuralType{value: 10, tolerance: 2}

      # sub.value (5) is NOT within super.tolerance (2) of super.value (10)
      # |5 - 10| = 5 > 2, so subtyping does not hold
      refute ManualStructuralType.structural_subtype?(sub, super)
      refute Subtyping.subtype_of?(super, sub)
    end

    test "metadata indicates structural rule presence" do
      meta = ManualStructuralType.__subtyping_metadata__()

      assert meta.has_structural_rule? == true
      # Manual rules don't generate check_subtype
      assert meta.has_check_subtype? == false
    end
  end

  describe "invariant variance" do
    test "identical inner types are subtypes of each other" do
      container1 = %InvariantContainer{contents: Deft.Type.integer()}
      container2 = %InvariantContainer{contents: Deft.Type.integer()}

      assert Subtyping.subtype_of?(container1, container2)
      assert Subtyping.subtype_of?(container2, container1)
    end

    test "different inner types are NOT subtypes even with sub/super relationship" do
      int_container = %InvariantContainer{contents: Deft.Type.integer()}
      num_container = %InvariantContainer{contents: Deft.Type.number()}

      # Even though Integer <: Number, invariant means exact match required
      refute Subtyping.subtype_of?(num_container, int_container)
      refute Subtyping.subtype_of?(int_container, num_container)
    end

    test "metadata shows invariant variance" do
      meta = InvariantContainer.__subtyping_metadata__()

      assert meta.parameters == [contents: :invariant]
      assert meta.has_structural_rule? == true
      assert meta.has_check_subtype? == true
    end

    test "check_subtype reports mismatch for invariant parameter" do
      int_container = %InvariantContainer{contents: Deft.Type.integer()}
      num_container = %InvariantContainer{contents: Deft.Type.number()}

      result = Subtyping.check_subtype(num_container, int_container, :test_expr)

      assert {:mismatch, info} = result
      assert info.expected == Deft.Type.number()
      assert info.actual == Deft.Type.integer()
    end
  end
end
