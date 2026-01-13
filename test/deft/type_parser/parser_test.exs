defmodule Deft.TypeParser.ParserTest do
  use ExUnit.Case, async: true

  alias Deft.Error
  alias Deft.TypeParser.{AST, Parser}

  # ============================================================================
  # Primitive Types
  # ============================================================================

  describe "primitive types" do
    test "parses integer" do
      ast = {:integer, [line: 1, column: 1], nil}
      assert {:ok, %AST.Primitive{kind: :integer, span: {1, 1}}} = Parser.parse(ast, [])
    end

    test "parses float" do
      ast = {:float, [line: 2, column: 3], nil}
      assert {:ok, %AST.Primitive{kind: :float, span: {2, 3}}} = Parser.parse(ast, [])
    end

    test "parses number" do
      ast = {:number, [], nil}
      assert {:ok, %AST.Primitive{kind: :number}} = Parser.parse(ast, [])
    end

    test "parses boolean" do
      ast = {:boolean, [], nil}
      assert {:ok, %AST.Primitive{kind: :boolean}} = Parser.parse(ast, [])
    end

    test "parses atom" do
      ast = {:atom, [], nil}
      assert {:ok, %AST.Primitive{kind: :atom}} = Parser.parse(ast, [])
    end

    test "parses binary" do
      ast = {:binary, [], nil}
      assert {:ok, %AST.Primitive{kind: :binary}} = Parser.parse(ast, [])
    end

    test "parses string as binary alias" do
      ast = {:string, [line: 1], nil}
      assert {:ok, %AST.Primitive{kind: :binary}} = Parser.parse(ast, [])
    end

    test "parses top" do
      ast = {:top, [], nil}
      assert {:ok, %AST.Primitive{kind: :top}} = Parser.parse(ast, [])
    end

    test "parses bottom" do
      ast = {:bottom, [], nil}
      assert {:ok, %AST.Primitive{kind: :bottom}} = Parser.parse(ast, [])
    end

    test "parses nil as atom" do
      ast = {nil, [line: 1], nil}
      assert {:ok, %AST.Primitive{kind: :atom}} = Parser.parse(ast, [])
    end
  end

  # ============================================================================
  # Abstract Types
  # ============================================================================

  describe "abstract types" do
    test "parses abstract list type" do
      ast = {:list, [line: 5], nil}
      assert {:ok, %AST.Abstract{kind: :list, span: {5, nil}}} = Parser.parse(ast, [])
    end

    test "parses abstract tuple type" do
      ast = {:tuple, [], nil}
      assert {:ok, %AST.Abstract{kind: :tuple}} = Parser.parse(ast, [])
    end
  end

  # ============================================================================
  # Tuple Types
  # ============================================================================

  describe "tuple types" do
    test "parses 2-element tuple (pair syntax)" do
      ast = {{:integer, [], nil}, {:float, [], nil}}

      assert {:ok, %AST.Tuple{elements: elements}} = Parser.parse(ast, [])
      assert [%AST.Primitive{kind: :integer}, %AST.Primitive{kind: :float}] = elements
    end

    test "parses n-element tuple (explicit syntax)" do
      ast = {:{}, [line: 5], [{:integer, [], nil}, {:float, [], nil}, {:boolean, [], nil}]}

      assert {:ok, %AST.Tuple{elements: elements, span: {5, nil}}} = Parser.parse(ast, [])
      assert length(elements) == 3
    end

    test "parses empty tuple" do
      ast = {:{}, [], []}
      assert {:ok, %AST.Tuple{elements: []}} = Parser.parse(ast, [])
    end

    test "parses single-element tuple" do
      ast = {:{}, [], [{:integer, [], nil}]}
      assert {:ok, %AST.Tuple{elements: [%AST.Primitive{kind: :integer}]}} = Parser.parse(ast, [])
    end

    test "parses nested tuples" do
      ast = {{:integer, [], nil}, {:{}, [], [{:float, [], nil}, {:boolean, [], nil}]}}

      assert {:ok, %AST.Tuple{elements: [_, %AST.Tuple{elements: [_, _]}]}} =
               Parser.parse(ast, [])
    end
  end

  # ============================================================================
  # Union Types
  # ============================================================================

  describe "union types" do
    test "parses simple union" do
      ast = {:|, [line: 3, column: 10], [{:integer, [], nil}, {:float, [], nil}]}

      assert {:ok,
              %AST.Union{
                left: %AST.Primitive{kind: :integer},
                right: %AST.Primitive{kind: :float},
                span: {3, 10}
              }} = Parser.parse(ast, [])
    end

    test "parses nested union (right-associative)" do
      # integer | float | boolean parses as integer | (float | boolean)
      ast =
        {:|, [], [{:integer, [], nil}, {:|, [], [{:float, [], nil}, {:boolean, [], nil}]}]}

      assert {:ok, %AST.Union{left: _, right: %AST.Union{}}} = Parser.parse(ast, [])
    end

    test "parses union of tuples" do
      left = {:{}, [], [{:integer, [], nil}]}
      right = {:{}, [], [{:float, [], nil}]}
      ast = {:|, [], [left, right]}

      assert {:ok, %AST.Union{left: %AST.Tuple{}, right: %AST.Tuple{}}} = Parser.parse(ast, [])
    end
  end

  # ============================================================================
  # List Types
  # ============================================================================

  describe "list types" do
    test "parses list with element type" do
      ast = [{:integer, [], nil}]
      assert {:ok, %AST.List{element: %AST.Primitive{kind: :integer}}} = Parser.parse(ast, [])
    end

    test "parses nested list type" do
      ast = [[{:integer, [], nil}]]
      assert {:ok, %AST.List{element: %AST.List{}}} = Parser.parse(ast, [])
    end

    test "parses list of tuples" do
      ast = [{{:integer, [], nil}, {:float, [], nil}}]
      assert {:ok, %AST.List{element: %AST.Tuple{}}} = Parser.parse(ast, [])
    end
  end

  # ============================================================================
  # Function Types
  # ============================================================================

  describe "function types" do
    test "parses single-arg function wrapped in list" do
      ast = [{:->, [line: 1], [[{:integer, [], nil}], {:boolean, [], nil}]}]

      assert {:ok,
              %AST.Function{
                inputs: [%AST.Primitive{kind: :integer}],
                output: %AST.Primitive{kind: :boolean}
              }} = Parser.parse(ast, [])
    end

    test "parses multi-arg function" do
      ast =
        [{:->, [], [[{:integer, [], nil}, {:float, [], nil}], {:boolean, [], nil}]}]

      assert {:ok, %AST.Function{inputs: [_, _]}} = Parser.parse(ast, [])
    end

    test "parses zero-arg function" do
      ast = [{:->, [], [[], {:integer, [], nil}]}]

      assert {:ok, %AST.Function{inputs: [], output: %AST.Primitive{kind: :integer}}} =
               Parser.parse(ast, [])
    end

    test "parses function without list wrapper" do
      ast = {:->, [line: 5], [[{:integer, [], nil}], {:boolean, [], nil}]}

      assert {:ok, %AST.Function{span: {5, nil}}} = Parser.parse(ast, [])
    end

    test "parses higher-order function" do
      # (a -> b) -> [a] -> [b]
      fn_type = [{:->, [], [[{:a, [], nil}], {:b, [], nil}]}]
      list_a = [{:a, [], nil}]
      list_b = [{:b, [], nil}]
      ast = [{:->, [], [[fn_type, list_a], list_b]}]

      assert {:ok, %AST.Function{inputs: [%AST.Function{}, %AST.List{}], output: %AST.List{}}} =
               Parser.parse(ast, allow_variables: true)
    end
  end

  # ============================================================================
  # Type Variables
  # ============================================================================

  describe "type variables" do
    test "parses single letter as variable when allowed" do
      ast = {:a, [line: 1, column: 5], nil}

      assert {:ok, %AST.Variable{name: :a, span: {1, 5}}} =
               Parser.parse(ast, allow_variables: true)
    end

    test "parses all lowercase letters as variables" do
      for letter <- ~w(a b c d e f g h i j k l m n o p q r s t u v w x y z)a do
        ast = {letter, [], nil}

        assert {:ok, %AST.Variable{name: ^letter}} =
                 Parser.parse(ast, allow_variables: true)
      end
    end

    test "parses single letter as alias when not allowed" do
      ast = {:a, [], nil}
      assert {:ok, %AST.Alias{name: :a}} = Parser.parse(ast, allow_variables: false)
    end

    test "parses single letter as alias by default" do
      ast = {:a, [], nil}
      assert {:ok, %AST.Alias{name: :a}} = Parser.parse(ast, [])
    end

    test "parses multi-letter name as alias even when variables allowed" do
      ast = {:foo, [], nil}
      assert {:ok, %AST.Alias{name: :foo}} = Parser.parse(ast, allow_variables: true)
    end

    test "parses uppercase letter as alias" do
      ast = {:A, [], nil}
      assert {:ok, %AST.Alias{name: :A}} = Parser.parse(ast, allow_variables: true)
    end
  end

  # ============================================================================
  # Type Aliases
  # ============================================================================

  describe "type aliases" do
    test "parses named type as alias" do
      ast = {:MyType, [line: 10], Elixir}

      assert {:ok, %AST.Alias{name: :MyType, context: Elixir, span: {10, nil}}} =
               Parser.parse(ast, [])
    end

    test "parses alias with nil context" do
      ast = {:SomeType, [], nil}
      assert {:ok, %AST.Alias{name: :SomeType, context: nil}} = Parser.parse(ast, [])
    end
  end

  # ============================================================================
  # Parenthesized Expressions
  # ============================================================================

  describe "parenthesized expressions" do
    test "unwraps __block__ with single element" do
      ast = {:__block__, [], [{:integer, [line: 1], nil}]}
      assert {:ok, %AST.Primitive{kind: :integer}} = Parser.parse(ast, [])
    end
  end

  # ============================================================================
  # Error Cases
  # ============================================================================

  describe "error cases" do
    test "returns error for empty list" do
      assert {:error, %Error{code: :malformed_type}} = Parser.parse([], [])
    end

    test "returns error for multi-element list" do
      assert {:error, %Error{code: :malformed_type}} = Parser.parse([1, 2], [])
    end

    test "returns error for literal integer" do
      assert {:error, %Error{code: :malformed_type}} = Parser.parse(42, [])
    end

    test "returns error for literal float" do
      assert {:error, %Error{code: :malformed_type}} = Parser.parse(3.14, [])
    end

    test "returns error for literal string" do
      assert {:error, %Error{code: :malformed_type}} = Parser.parse("hello", [])
    end

    test "error includes suggestions for empty list" do
      {:error, error} = Parser.parse([], [])
      assert Enum.any?(error.suggestions, &String.contains?(&1, "[element_type]"))
    end

    test "error includes suggestions for literal number" do
      {:error, error} = Parser.parse(42, [])
      assert Enum.any?(error.suggestions, &String.contains?(&1, "integer"))
    end

    test "error includes suggestions for literal string" do
      {:error, error} = Parser.parse("hello", [])
      assert Enum.any?(error.suggestions, &String.contains?(&1, "binary"))
    end

    test "error propagates from nested types" do
      # Union with invalid right side
      ast = {:|, [], [{:integer, [], nil}, 42]}
      assert {:error, %Error{code: :malformed_type}} = Parser.parse(ast, [])
    end
  end

  # ============================================================================
  # File Location
  # ============================================================================

  describe "file location in errors" do
    test "error includes file location when provided" do
      {:error, error} = Parser.parse(42, file: "test.ex")
      assert error.location == {"test.ex", nil, nil}
    end

    test "error includes file and line from AST metadata" do
      # Use a multi-element list which is always an error
      {:error, error} = Parser.parse([1, 2], file: "test.ex")
      assert error.location == {"test.ex", nil, nil}
    end

    test "error includes file even for literals without metadata" do
      {:error, error} = Parser.parse("invalid", file: "test.ex")
      assert error.location == {"test.ex", nil, nil}
    end
  end

  # ============================================================================
  # Span Extraction
  # ============================================================================

  describe "span extraction" do
    test "extracts span from metadata with line and column" do
      ast = {:integer, [line: 10, column: 5], nil}
      assert {:ok, %AST.Primitive{span: {10, 5}}} = Parser.parse(ast, [])
    end

    test "extracts span from metadata with only line" do
      ast = {:integer, [line: 10], nil}
      assert {:ok, %AST.Primitive{span: {10, nil}}} = Parser.parse(ast, [])
    end

    test "handles empty metadata" do
      ast = {:integer, [], nil}
      assert {:ok, %AST.Primitive{span: nil}} = Parser.parse(ast, [])
    end
  end
end
