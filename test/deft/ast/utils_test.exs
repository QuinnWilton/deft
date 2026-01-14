defmodule Deft.AST.UtilsTest do
  use ExUnit.Case, async: true

  alias Deft.AST.Utils

  describe "map_ok/2" do
    test "returns ok with mapped values on success" do
      result = Utils.map_ok([1, 2, 3], fn x -> {:ok, x * 2} end)
      assert {:ok, [2, 4, 6]} = result
    end

    test "returns first error and stops processing" do
      calls = :counters.new(1, [:atomics])

      result =
        Utils.map_ok([1, 2, 3], fn x ->
          :counters.add(calls, 1, 1)
          if x == 2, do: {:error, :failed}, else: {:ok, x}
        end)

      assert {:error, :failed} = result
      # Stopped at 2, didn't process 3
      assert :counters.get(calls, 1) == 2
    end

    test "handles empty list" do
      assert {:ok, []} = Utils.map_ok([], fn _ -> {:ok, :never_called} end)
    end

    test "preserves order of results" do
      result = Utils.map_ok([:a, :b, :c], fn x -> {:ok, {x, 1}} end)
      assert {:ok, [{:a, 1}, {:b, 1}, {:c, 1}]} = result
    end

    test "propagates error reason" do
      result = Utils.map_ok([1], fn _ -> {:error, {:custom, :reason}} end)
      assert {:error, {:custom, :reason}} = result
    end
  end

  describe "find_similar/3" do
    test "finds similar name above default threshold" do
      assert :option = Utils.find_similar(:optin, [:option, :result, :either])
    end

    test "finds similar name with typo" do
      assert :result = Utils.find_similar(:resutl, [:option, :result, :either])
    end

    test "returns nil when no similar name" do
      assert nil == Utils.find_similar(:xyz, [:option, :result, :either])
    end

    test "returns nil for empty list" do
      assert nil == Utils.find_similar(:option, [])
    end

    test "respects custom threshold - stricter" do
      # "op" and "option" have ~0.72 similarity
      assert nil == Utils.find_similar(:op, [:option], 0.8)
    end

    test "respects custom threshold - looser" do
      assert :option = Utils.find_similar(:op, [:option], 0.7)
    end

    test "returns first match when multiple similar names exist" do
      # Both :option and :options are similar to :optio
      result = Utils.find_similar(:optio, [:option, :options])
      assert result in [:option, :options]
    end
  end
end
