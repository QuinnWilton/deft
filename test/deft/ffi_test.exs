defmodule Deft.FFITest do
  @moduledoc """
  Unit tests for the FFI conversion module.
  """
  use ExUnit.Case, async: true

  alias Deft.AST
  alias Deft.FFI
  alias Deft.Type

  describe "conversion_kind/1" do
    test "recognizes option Type.Alias" do
      type = Type.Alias.new(:option, nil, [Type.var(:a)])
      assert :option = FFI.conversion_kind(type)
    end

    test "recognizes result Type.Alias (tuple error)" do
      type = Type.Alias.new(:result, nil, [Type.var(:t), Type.var(:e)])
      assert {:result, :tuple_error} = FFI.conversion_kind(type)
    end

    test "recognizes fetch_result Type.Alias (bare error)" do
      type = Type.Alias.new(:fetch_result, nil, [Type.var(:t)])
      assert {:result, :bare_error} = FFI.conversion_kind(type)
    end

    test "recognizes option Type.ADT" do
      type = %Type.ADT{
        name: AST.Local.new(:option, nil),
        params: [Type.var(:a)],
        variants: []
      }

      assert :option = FFI.conversion_kind(type)
    end

    test "recognizes result Type.ADT (tuple error)" do
      adt_name = AST.Local.new(:result, nil)

      type = %Type.ADT{
        name: adt_name,
        params: [Type.var(:t), Type.var(:e)],
        variants: [
          Type.Variant.new(:ok, adt_name, [Type.var(:t)]),
          Type.Variant.new(:err, adt_name, [Type.var(:e)])
        ]
      }

      assert {:result, :tuple_error} = FFI.conversion_kind(type)
    end

    test "recognizes fetch_result Type.ADT (bare error)" do
      adt_name = AST.Local.new(:fetch_result, nil)

      type = %Type.ADT{
        name: adt_name,
        params: [Type.var(:t)],
        variants: [
          Type.Variant.new(:ok, adt_name, [Type.var(:t)]),
          Type.Variant.new(:error, adt_name, [])
        ]
      }

      assert {:result, :bare_error} = FFI.conversion_kind(type)
    end

    test "returns :none for integer type" do
      assert :none = FFI.conversion_kind(Type.integer())
    end

    test "returns :none for list type" do
      assert :none = FFI.conversion_kind(Type.list())
    end

    test "returns :none for other Type.Alias names" do
      type = Type.Alias.new(:other, nil, [])
      assert :none = FFI.conversion_kind(type)
    end

    test "returns :none for function type" do
      type = Type.fun([Type.integer()], Type.integer())
      assert :none = FFI.conversion_kind(type)
    end
  end

  describe "maybe_wrap_conversion/3" do
    test "wraps option return type in case expression" do
      call = quote(do: Enum.find([1, 2], fn x -> x > 1 end))
      type = Type.Alias.new(:option, nil, [Type.integer()])

      wrapped = FFI.maybe_wrap_conversion(call, type, [])
      assert {:case, _, _} = wrapped
    end

    test "wraps result return type in case expression" do
      call = quote(do: Enum.fetch([1, 2], 0))
      type = Type.Alias.new(:result, nil, [Type.integer(), Type.atom()])

      wrapped = FFI.maybe_wrap_conversion(call, type, [])
      assert {:case, _, _} = wrapped
    end

    test "leaves non-FFI types unwrapped" do
      call = quote(do: String.length("hello"))

      wrapped = FFI.maybe_wrap_conversion(call, Type.integer(), [])
      assert wrapped == call
    end

    test "leaves list type unwrapped" do
      call = quote(do: Enum.map([1, 2], fn x -> x + 1 end))

      wrapped = FFI.maybe_wrap_conversion(call, Type.list(), [])
      assert wrapped == call
    end
  end
end
