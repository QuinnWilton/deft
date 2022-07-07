defmodule DeftTest do
  use ExUnit.Case
  use ExUnitProperties

  alias Deft.Generators
  alias Deft.Subtyping
  alias Deft.Type

  property "computes the type for an expression" do
    check all({expr, expected} <- Generators.Code.expression(), max_shrinking_steps: 0) do
      assert actual = Deft.Helpers.compute_types(expr, __ENV__)
      assert Type.well_formed?(actual)
      assert Subtyping.subtype_of?(expected, actual)
    end
  end
end
