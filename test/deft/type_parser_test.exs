defmodule Deft.TypeParserTest do
  use ExUnit.Case, async: true

  alias Deft.Error
  alias Deft.Type
  alias Deft.TypeParser
  alias Deft.TypeParser.AST

  # ============================================================================
  # parse/2 with output: :type (default)
  # ============================================================================

  describe "parse/2 with output: :type" do
    test "parses primitive types" do
      assert {:ok, %Type.Integer{}} = TypeParser.parse({:integer, [], nil})
      assert {:ok, %Type.Float{}} = TypeParser.parse({:float, [], nil})
      assert {:ok, %Type.Boolean{}} = TypeParser.parse({:boolean, [], nil})
    end

    test "parses composite types" do
      # Tuple
      ast = {{:integer, [], nil}, {:float, [], nil}}
      assert {:ok, %Type.FixedTuple{}} = TypeParser.parse(ast)

      # Union
      ast = {:|, [], [{:integer, [], nil}, {:float, [], nil}]}
      assert {:ok, %Type.Union{}} = TypeParser.parse(ast)

      # List
      ast = [{:integer, [], nil}]
      assert {:ok, %Type.FixedList{}} = TypeParser.parse(ast)

      # Function
      ast = [{:->, [], [[{:integer, [], nil}], {:boolean, [], nil}]}]
      assert {:ok, %Type.Fn{}} = TypeParser.parse(ast)
    end

    test "returns error for invalid input" do
      assert {:error, %Error{code: :malformed_type}} = TypeParser.parse(42)
      assert {:error, %Error{}} = TypeParser.parse([])
      assert {:error, %Error{}} = TypeParser.parse("not a type")
    end

    test "parses type alias when variables not allowed" do
      ast = {:a, [], nil}
      assert {:ok, %Type.Alias{name: :a}} = TypeParser.parse(ast, allow_variables: false)
    end

    test "parses type variable when allowed" do
      ast = {:a, [], nil}
      assert {:ok, %Type.Var{name: :a}} = TypeParser.parse(ast, allow_variables: true)
    end
  end

  # ============================================================================
  # parse/2 with output: :quoted
  # ============================================================================

  describe "parse/2 with output: :quoted" do
    test "returns quoted AST that evaluates to Type struct" do
      ast = {:integer, [], nil}
      assert {:ok, quoted} = TypeParser.parse(ast, output: :quoted)

      {result, _} = Code.eval_quoted(quoted)
      assert %Type.Integer{} = result
    end

    test "handles complex types" do
      # Function: (integer, float) -> boolean
      ast = [{:->, [], [[{:integer, [], nil}, {:float, [], nil}], {:boolean, [], nil}]}]
      assert {:ok, quoted} = TypeParser.parse(ast, output: :quoted)

      {result, _} = Code.eval_quoted(quoted)
      assert %Type.Fn{inputs: [%Type.Integer{}, %Type.Float{}], output: %Type.Boolean{}} = result
    end

    test "handles type variables" do
      ast = {:a, [], nil}
      assert {:ok, quoted} = TypeParser.parse(ast, output: :quoted, allow_variables: true)

      {result, _} = Code.eval_quoted(quoted)
      assert %Type.Var{name: :a} = result
    end
  end

  # ============================================================================
  # parse/2 with output: :ast
  # ============================================================================

  describe "parse/2 with output: :ast" do
    test "returns intermediate AST struct" do
      ast = {:integer, [line: 5, column: 3], nil}

      assert {:ok, %AST.Primitive{kind: :integer, span: {5, 3}}} =
               TypeParser.parse(ast, output: :ast)
    end

    test "preserves span information" do
      ast = {:|, [line: 10, column: 5], [{:integer, [], nil}, {:float, [], nil}]}

      assert {:ok, %AST.Union{span: {10, 5}}} = TypeParser.parse(ast, output: :ast)
    end
  end

  # ============================================================================
  # parse!/2
  # ============================================================================

  describe "parse!/2" do
    test "returns result on success" do
      assert %Type.Integer{} = TypeParser.parse!({:integer, [], nil})
    end

    test "raises CompileError on failure" do
      assert_raise CompileError, fn ->
        TypeParser.parse!(42)
      end
    end

    test "supports all options" do
      assert %Type.Var{name: :a} = TypeParser.parse!({:a, [], nil}, allow_variables: true)
    end
  end

  # ============================================================================
  # parse_with_variables/2
  # ============================================================================

  describe "parse_with_variables/2" do
    test "returns AST and empty variables for monomorphic type" do
      ast = {:integer, [], nil}
      assert {:ok, %AST.Primitive{kind: :integer}, []} = TypeParser.parse_with_variables(ast)
    end

    test "returns AST and variables for polymorphic type" do
      # [a] -> a
      ast = [{:->, [], [[{:a, [], nil}], {:a, [], nil}]}]

      assert {:ok, %AST.Function{}, [:a]} =
               TypeParser.parse_with_variables(ast, allow_variables: true)
    end

    test "collects multiple variables in order" do
      # (a, b) -> {b, a}
      ast = [
        {:->, [],
         [
           [{:a, [], nil}, {:b, [], nil}],
           {{:b, [], nil}, {:a, [], nil}}
         ]}
      ]

      assert {:ok, _, vars} = TypeParser.parse_with_variables(ast, allow_variables: true)
      assert vars == [:a, :b]
    end

    test "deduplicates variables" do
      # a -> a -> a
      ast = [
        {:->, [],
         [
           [{:a, [], nil}, {:a, [], nil}],
           {:a, [], nil}
         ]}
      ]

      assert {:ok, _, [:a]} = TypeParser.parse_with_variables(ast, allow_variables: true)
    end

    test "returns error for invalid input" do
      assert {:error, %Error{}} = TypeParser.parse_with_variables(42)
    end
  end

  # ============================================================================
  # File/Env Options
  # ============================================================================

  describe "file and env options" do
    test "extracts file from :file option" do
      {:error, error} = TypeParser.parse(42, file: "test.ex")
      assert {"test.ex", nil, nil} == error.location
    end

    test "extracts file from Macro.Env" do
      env = %Macro.Env{file: "from_env.ex", line: 10}
      {:error, error} = TypeParser.parse(42, env: env)
      assert {"from_env.ex", nil, nil} == error.location
    end

    test "env takes precedence over file option" do
      env = %Macro.Env{file: "env_file.ex", line: 10}
      {:error, error} = TypeParser.parse(42, env: env, file: "explicit_file.ex")
      assert {"env_file.ex", nil, nil} == error.location
    end
  end

  # ============================================================================
  # Real-World Type Syntax Tests
  # ============================================================================

  describe "real-world type syntax" do
    test "parses Kernel.hd signature type: [a] -> a" do
      # This is how `sig hd([a]) :: a` is quoted
      # The input is [[{:a, [], nil}]] - a list containing element type 'a'
      list_of_a = [{:a, [], nil}]
      ast = [{:->, [], [[list_of_a], {:a, [], nil}]}]

      {:ok, result} = TypeParser.parse(ast, allow_variables: true)
      assert %Type.Fn{} = result
      assert [%Type.FixedList{contents: %Type.Var{name: :a}}] = result.inputs
      assert %Type.Var{name: :a} = result.output
    end

    test "parses Kernel.++ signature type: [a], [b] -> [a | b]" do
      ast = [
        {:->, [],
         [
           [[{:a, [], nil}], [{:b, [], nil}]],
           [{:|, [], [{:a, [], nil}, {:b, [], nil}]}]
         ]}
      ]

      assert {:ok, %Type.Fn{inputs: [_, _], output: %Type.FixedList{}}} =
               TypeParser.parse(ast, allow_variables: true)
    end

    test "parses Enum.map signature type: [a], (a -> b) -> [b]" do
      fn_type = [{:->, [], [[{:a, [], nil}], {:b, [], nil}]}]
      list_a = [{:a, [], nil}]
      list_b = [{:b, [], nil}]
      ast = [{:->, [], [[list_a, fn_type], list_b]}]

      assert {:ok, %Type.Fn{inputs: [%Type.FixedList{}, %Type.Fn{}], output: %Type.FixedList{}}} =
               TypeParser.parse(ast, allow_variables: true)
    end

    test "parses ADT variant type: circle(float)" do
      # This would be parsed as an alias in annotations
      ast = {:circle, [], Elixir}
      assert {:ok, %Type.Alias{name: :circle}} = TypeParser.parse(ast)
    end
  end

  # ============================================================================
  # Edge Cases
  # ============================================================================

  describe "edge cases" do
    test "handles deeply nested types" do
      # [[[integer]]]
      ast = [[[{:integer, [], nil}]]]

      assert {:ok, %Type.FixedList{contents: %Type.FixedList{contents: %Type.FixedList{}}}} =
               TypeParser.parse(ast)
    end

    test "handles empty tuple" do
      ast = {:{}, [], []}
      assert {:ok, %Type.FixedTuple{elements: []}} = TypeParser.parse(ast)
    end

    test "handles zero-arg function" do
      ast = [{:->, [], [[], {:integer, [], nil}]}]
      assert {:ok, %Type.Fn{inputs: []}} = TypeParser.parse(ast)
    end

    test "handles union of unions" do
      # (integer | float) | boolean
      inner_union = {:|, [], [{:integer, [], nil}, {:float, [], nil}]}
      ast = {:|, [], [inner_union, {:boolean, [], nil}]}

      assert {:ok, %Type.Union{fst: %Type.Union{}, snd: %Type.Boolean{}}} =
               TypeParser.parse(ast)
    end
  end
end
