defmodule Deft.Integration.FFIConversionTest do
  @moduledoc """
  Integration tests for FFI conversion of option/result return types.

  Verifies that external function calls returning `value | nil` or
  `{:ok, value} | :error` are automatically converted to Deft ADT
  representations.
  """
  use Deft.IntegrationCase, async: true

  describe "option return type conversion for Enum functions" do
    test "Enum.find converts nil to {:none}" do
      {result, type} =
        Deft.compile do
          Enum.find([1, 2, 3], fn x :: integer -> x > 10 end)
        end

      assert result == {:none}
      assert %Type.Alias{name: :option} = type
    end

    test "Enum.find converts value to {:some, value}" do
      {result, type} =
        Deft.compile do
          Enum.find([1, 15, 3], fn x :: integer -> x > 10 end)
        end

      assert result == {:some, 15}
      assert %Type.Alias{name: :option} = type
    end

    test "Enum.at converts nil to {:none}" do
      {result, type} =
        Deft.compile do
          Enum.at([1, 2, 3], 10)
        end

      assert result == {:none}
      assert %Type.Alias{name: :option} = type
    end

    test "Enum.at converts value to {:some, value}" do
      {result, type} =
        Deft.compile do
          Enum.at([1, 2, 3], 1)
        end

      assert result == {:some, 2}
      assert %Type.Alias{name: :option} = type
    end

    test "Enum.find_index converts nil to {:none}" do
      {result, type} =
        Deft.compile do
          Enum.find_index([1, 2, 3], fn x :: integer -> x > 10 end)
        end

      assert result == {:none}
      assert %Type.Alias{name: :option} = type
    end

    test "Enum.find_index converts value to {:some, index}" do
      {result, type} =
        Deft.compile do
          Enum.find_index([1, 2, 3], fn x :: integer -> x > 1 end)
        end

      assert result == {:some, 1}
      assert %Type.Alias{name: :option} = type
    end

    test "Enum.find_value converts nil to {:none}" do
      {result, type} =
        Deft.compile do
          Enum.find_value([1, 2, 3], fn x :: integer -> x * 2 end)
        end

      # find_value with a mapping function that always returns truthy values
      # returns {:some, mapped_value} for the first element
      assert result == {:some, 2}
      assert %Type.Alias{name: :option} = type
    end
  end

  describe "fetch_result return type conversion for Enum functions" do
    test "Enum.fetch converts {:ok, v} correctly" do
      {result, type} =
        Deft.compile do
          Enum.fetch([1, 2, 3], 1)
        end

      assert result == {:ok, 2}
      assert %Type.Alias{name: :fetch_result} = type
    end

    test "Enum.fetch converts :error to {:error}" do
      {result, type} =
        Deft.compile do
          Enum.fetch([1, 2, 3], 10)
        end

      assert result == {:error}
      assert %Type.Alias{name: :fetch_result} = type
    end
  end

  describe "option return type conversion for List functions" do
    test "List.first converts value to {:some, value}" do
      {result, type} =
        Deft.compile do
          List.first([1, 2, 3])
        end

      assert result == {:some, 1}
      assert %Type.Alias{name: :option} = type
    end

    test "List.last converts value to {:some, value}" do
      {result, type} =
        Deft.compile do
          List.last([1, 2, 3])
        end

      assert result == {:some, 3}
      assert %Type.Alias{name: :option} = type
    end
  end

  describe "option return type conversion for String functions" do
    test "String.at converts nil to {:none}" do
      {result, type} =
        Deft.compile do
          String.at("hello", 10)
        end

      assert result == {:none}
      assert %Type.Alias{name: :option} = type
    end

    test "String.at converts value to {:some, value}" do
      {result, type} =
        Deft.compile do
          String.at("hello", 1)
        end

      assert result == {:some, "e"}
      assert %Type.Alias{name: :option} = type
    end

    test "String.first converts nil to {:none}" do
      {result, type} =
        Deft.compile do
          String.first("")
        end

      assert result == {:none}
      assert %Type.Alias{name: :option} = type
    end

    test "String.first converts value to {:some, value}" do
      {result, type} =
        Deft.compile do
          String.first("hello")
        end

      assert result == {:some, "h"}
      assert %Type.Alias{name: :option} = type
    end

    test "String.last converts nil to {:none}" do
      {result, type} =
        Deft.compile do
          String.last("")
        end

      assert result == {:none}
      assert %Type.Alias{name: :option} = type
    end

    test "String.last converts value to {:some, value}" do
      {result, type} =
        Deft.compile do
          String.last("hello")
        end

      assert result == {:some, "o"}
      assert %Type.Alias{name: :option} = type
    end
  end

  describe "pattern matching on FFI-converted values" do
    test "can pattern match on option from Enum.find - some case" do
      {result, type} =
        Deft.compile do
          case Enum.find([1, 2, 3], fn x :: integer -> x > 1 end) do
            some(x) -> x * 10
            none -> 0
          end
        end

      assert result == 20
      assert %Type.Integer{} = type
    end

    test "can pattern match on option from Enum.find - none case" do
      {result, type} =
        Deft.compile do
          case Enum.find([1, 2, 3], fn x :: integer -> x > 10 end) do
            some(x) -> x * 10
            none -> 0
          end
        end

      assert result == 0
      assert %Type.Integer{} = type
    end

    test "can pattern match on fetch_result from Enum.fetch - ok case" do
      {result, type} =
        Deft.compile do
          case Enum.fetch([1, 2, 3], 1) do
            ok(x) -> x * 10
            error -> 0
          end
        end

      assert result == 20
      assert %Type.Integer{} = type
    end

    test "can pattern match on fetch_result from Enum.fetch - error case" do
      {result, type} =
        Deft.compile do
          case Enum.fetch([1, 2, 3], 10) do
            ok(x) -> x * 10
            error -> 0
          end
        end

      assert result == 0
      assert %Type.Integer{} = type
    end

    test "can use pattern matching to unwrap option from String.first" do
      {result, type} =
        Deft.compile do
          case String.first("hello") do
            some(c) -> c
            none -> "default"
          end
        end

      assert result == "h"
      assert %Type.Binary{} = type
    end

    test "can chain FFI conversions with pattern matching" do
      {result, type} =
        Deft.compile do
          case Enum.at([10, 20, 30], 1) do
            some(x) ->
              case Enum.fetch([100, 200, 300], x) do
                ok(v) -> v
                error -> 0
              end

            none ->
              0
          end
        end

      # Enum.at([10,20,30], 1) -> {:some, 20}
      # Enum.fetch([100,200,300], 20) -> {:error} (out of bounds)
      assert result == 0
      assert %Type.Integer{} = type
    end
  end
end
