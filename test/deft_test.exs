defmodule DeftTest do
  use ExUnit.Case
  use ExUnitProperties

  alias Deft.Generators
  alias Deft.Subtyping
  alias Deft.Type

  property "computes the type for an expression" do
    check all({expr, expected} <- Generators.Code.expression()) do
      assert actual = Deft.Helpers.compute_types(expr, __ENV__)
      assert Type.well_formed?(actual)

      # HACK: It isn't enough to check subtyping.
      # Consider this case:
      #
      # if true do
      #   5
      # else
      #   :foo
      # end
      #
      # If the type checker only checked one branch,
      # and returned `integer` as the type, then
      # this would pass a simple subtype test.
      #
      # Instead, we need to verify that each type
      # in `expected` has at least one subtype in
      # `actual`
      if is_struct(expected, Type.Union) and is_struct(actual, Type.Union) do
        for type <- Type.Union.types(expected) do
          actual_types = Type.Union.types(actual)

          assert Enum.any?(actual_types, &Subtyping.subtype_of?(type, &1))
        end
      end

      assert Subtyping.subtype_of?(expected, actual)
    end
  end
end
