defmodule DeftTest do
  use ExUnit.Case
  use ExUnitProperties

  alias Deft.Context
  alias Deft.Generators
  alias Deft.Subtyping
  alias Deft.Type
  alias Deft.TypeChecker

  property "computes the type for an expression" do
    env = __ENV__

    check all({expr, expected} <- Generators.Code.expression(), max_shrinking_steps: 0) do
      ctx = Context.new(env)
      {:ok, _erased, actual, _bindings, _ctx} = TypeChecker.check(expr, ctx)
      assert Type.well_formed?(actual)
      assert Subtyping.subtype_of?(expected, actual)
    end
  end
end
