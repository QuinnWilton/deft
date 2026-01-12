defmodule Deft.SpanTest do
  use ExUnit.Case, async: true

  alias Deft.AST
  alias Deft.Span

  describe "extract/1" do
    test "extracts from raw Elixir AST tuple" do
      ast = {:foo, [line: 10, column: 5, file: "test.ex"], []}
      assert {"test.ex", 10, 5} = Span.extract(ast)
    end

    test "extracts from raw meta keyword list" do
      assert {"test.ex", 10, 5} = Span.extract([line: 10, column: 5, file: "test.ex"])
    end

    test "handles missing file" do
      assert {nil, 10, 5} = Span.extract([line: 10, column: 5])
    end

    test "handles missing column" do
      assert {"test.ex", 10, nil} = Span.extract([line: 10, file: "test.ex"])
    end

    test "returns nil for missing line" do
      assert nil == Span.extract([column: 5])
    end

    test "returns nil for empty meta" do
      assert nil == Span.extract([])
    end

    test "returns nil for non-meta values" do
      assert nil == Span.extract("not meta")
      assert nil == Span.extract(123)
      assert nil == Span.extract(%{foo: :bar})
    end

    # AST node types with meta field
    test "extracts from AST.Literal" do
      node = AST.Literal.new(42, [line: 1, column: 1])
      assert {nil, 1, 1} = Span.extract(node)
    end

    test "extracts from AST.Local" do
      node = AST.Local.new(:x, nil, [line: 10, column: 5, file: "test.ex"])
      assert {"test.ex", 10, 5} = Span.extract(node)
    end

    test "extracts from AST.Tuple" do
      node = AST.Tuple.new([], [line: 5, column: 3])
      assert {nil, 5, 3} = Span.extract(node)
    end

    test "extracts from AST.List" do
      node = AST.List.new([], [line: 5, column: 3])
      assert {nil, 5, 3} = Span.extract(node)
    end

    test "extracts from AST.Pair" do
      node = AST.Pair.new(nil, nil, [line: 7, column: 2])
      assert {nil, 7, 2} = Span.extract(node)
    end

    test "extracts from AST.If" do
      node = AST.If.new(nil, nil, nil, [line: 3, column: 5])
      assert {nil, 3, 5} = Span.extract(node)
    end

    test "extracts from AST.Match" do
      node = AST.Match.new(nil, nil, [line: 8, column: 1])
      assert {nil, 8, 1} = Span.extract(node)
    end

    test "extracts from AST.Block" do
      node = AST.Block.new([], [line: 1, column: 1])
      assert {nil, 1, 1} = Span.extract(node)
    end

    test "extracts from AST.LocalCall" do
      node = AST.LocalCall.new(:foo, [], [line: 15, column: 10])
      assert {nil, 15, 10} = Span.extract(node)
    end

    test "extracts from AST.Case" do
      node = AST.Case.new(nil, [], [line: 20, column: 5])
      assert {nil, 20, 5} = Span.extract(node)
    end

    test "extracts from AST.CaseBranch" do
      node = AST.CaseBranch.new(nil, nil, [line: 21, column: 7])
      assert {nil, 21, 7} = Span.extract(node)
    end

    test "extracts from AST.Cond" do
      node = AST.Cond.new([], [line: 25, column: 3])
      assert {nil, 25, 3} = Span.extract(node)
    end

    test "extracts from AST.CondBranch" do
      node = AST.CondBranch.new(nil, nil, [line: 26, column: 5])
      assert {nil, 26, 5} = Span.extract(node)
    end

    # Note: AST.Fn and AST.FnApplication use different field names (fn_meta, arrow_meta,
    # fun_meta, args_meta) instead of the standard `meta` field. The Span module works
    # with the standard `meta` convention; these special cases should use their own
    # location extraction if needed.

    test "extracts from AST.Pin" do
      inner = AST.Local.new(:x, nil, [])
      node = AST.Pin.new(inner, [line: 40, column: 2])
      assert {nil, 40, 2} = Span.extract(node)
    end

    test "extracts from AST.Cons" do
      node = AST.Cons.new(nil, nil, [line: 45, column: 1])
      assert {nil, 45, 1} = Span.extract(node)
    end

    test "extracts from AST.Annotation" do
      node = AST.Annotation.new(nil, nil, [line: 50, column: 3])
      assert {nil, 50, 3} = Span.extract(node)
    end
  end

  describe "from_meta/1" do
    test "extracts location from keyword list with all fields" do
      assert {"test.ex", 10, 5} = Span.from_meta([line: 10, column: 5, file: "test.ex"])
    end

    test "handles extra metadata fields" do
      meta = [line: 10, column: 5, file: "test.ex", foo: :bar, baz: 123]
      assert {"test.ex", 10, 5} = Span.from_meta(meta)
    end

    test "returns nil when line is missing" do
      assert nil == Span.from_meta([column: 5, file: "test.ex"])
    end
  end

  describe "leftmost/2" do
    test "returns first node's location when both have locations" do
      node1 = AST.Local.new(:x, nil, [line: 10, column: 5])
      node2 = AST.Local.new(:y, nil, [line: 20, column: 10])

      assert {nil, 10, 5} = Span.leftmost(node1, node2)
    end

    test "returns second node's location when first has none" do
      node1 = AST.Local.new(:x, nil, [])
      node2 = AST.Local.new(:y, nil, [line: 20, column: 10])

      assert {nil, 20, 10} = Span.leftmost(node1, node2)
    end

    test "returns first node's location when second has none" do
      node1 = AST.Local.new(:x, nil, [line: 10, column: 5])
      node2 = AST.Local.new(:y, nil, [])

      assert {nil, 10, 5} = Span.leftmost(node1, node2)
    end

    test "returns nil when both have no location" do
      node1 = AST.Local.new(:x, nil, [])
      node2 = AST.Local.new(:y, nil, [])

      assert nil == Span.leftmost(node1, node2)
    end
  end

  describe "primary/3" do
    test "builds labeled span with primary kind" do
      loc = {"test.ex", 10, 5}

      assert %{
               location: {"test.ex", 10, 5},
               label: "error here",
               type: nil,
               kind: :primary
             } = Span.primary(loc, "error here", nil)
    end

    test "includes type when provided" do
      loc = {"test.ex", 10, 5}
      type = :some_type

      span = Span.primary(loc, "error", type)
      assert span.type == :some_type
    end

    test "returns nil for nil location" do
      assert nil == Span.primary(nil, "error here", nil)
    end
  end

  describe "secondary/3" do
    test "builds labeled span with secondary kind" do
      loc = {"test.ex", 10, 5}

      assert %{
               location: {"test.ex", 10, 5},
               label: "defined here",
               type: nil,
               kind: :secondary
             } = Span.secondary(loc, "defined here", nil)
    end

    test "includes type when provided" do
      loc = {"test.ex", 10, 5}
      type = :context_type

      span = Span.secondary(loc, "context", type)
      assert span.type == :context_type
    end

    test "returns nil for nil location" do
      assert nil == Span.secondary(nil, "defined here", nil)
    end
  end

  describe "filter/1" do
    test "removes nil values from list" do
      spans = [
        %{location: {nil, 1, 1}, label: "a", type: nil, kind: :primary},
        nil,
        %{location: {nil, 2, 2}, label: "b", type: nil, kind: :secondary},
        nil
      ]

      result = Span.filter(spans)
      assert length(result) == 2
      assert Enum.at(result, 0).label == "a"
      assert Enum.at(result, 1).label == "b"
    end

    test "returns empty list when all are nil" do
      assert [] == Span.filter([nil, nil, nil])
    end

    test "returns all items when none are nil" do
      spans = [
        %{location: {nil, 1, 1}, label: "a", type: nil, kind: :primary},
        %{location: {nil, 2, 2}, label: "b", type: nil, kind: :secondary}
      ]

      assert spans == Span.filter(spans)
    end

    test "handles empty list" do
      assert [] == Span.filter([])
    end
  end

  describe "integration with span builders" do
    test "filter works with primary/secondary builders" do
      loc1 = {"test.ex", 10, 5}
      loc2 = nil
      loc3 = {"test.ex", 20, 3}

      spans =
        Span.filter([
          Span.primary(loc1, "error", nil),
          Span.secondary(loc2, "context", nil),
          Span.secondary(loc3, "another", nil)
        ])

      assert length(spans) == 2
      assert Enum.at(spans, 0).kind == :primary
      assert Enum.at(spans, 1).kind == :secondary
    end
  end
end
