defmodule Deft.AnnotationsTest do
  use ExUnit.Case

  alias Deft.Annotations
  alias Deft.Type

  # ============================================================================
  # Primitive Types
  # ============================================================================

  describe "primitive type parsing" do
    test "parses boolean annotation" do
      ast = {:boolean, [], nil}

      assert %Type.Boolean{} = Annotations.parse(ast)
    end

    test "parses atom annotation" do
      ast = {:atom, [], nil}

      assert %Type.Atom{} = Annotations.parse(ast)
    end

    test "parses binary annotation" do
      ast = {:binary, [], nil}

      assert %Type.Binary{} = Annotations.parse(ast)
    end

    test "parses float annotation" do
      ast = {:float, [], nil}

      assert %Type.Float{} = Annotations.parse(ast)
    end

    test "parses integer annotation" do
      ast = {:integer, [], nil}

      assert %Type.Integer{} = Annotations.parse(ast)
    end

    test "parses number annotation" do
      ast = {:number, [], nil}

      assert %Type.Number{} = Annotations.parse(ast)
    end

    test "parses top annotation" do
      ast = {:top, [], nil}

      assert %Type.Top{} = Annotations.parse(ast)
    end

    test "parses bottom annotation" do
      ast = {:bottom, [], nil}

      assert %Type.Bottom{} = Annotations.parse(ast)
    end
  end

  # ============================================================================
  # Tuple Types
  # ============================================================================

  describe "tuple type parsing" do
    test "parses two-element tuple using pair syntax" do
      # This is how Elixir represents {integer, float} in AST
      ast = {{:integer, [], nil}, {:float, [], nil}}

      result = Annotations.parse(ast)

      assert %Type.FixedTuple{elements: [%Type.Integer{}, %Type.Float{}]} = result
    end

    test "parses three-element tuple using explicit tuple syntax" do
      # {integer, float, boolean} in AST form
      ast = {:{}, [], [{:integer, [], nil}, {:float, [], nil}, {:boolean, [], nil}]}

      result = Annotations.parse(ast)

      assert %Type.FixedTuple{elements: [%Type.Integer{}, %Type.Float{}, %Type.Boolean{}]} =
               result
    end

    test "parses single-element tuple" do
      ast = {:{}, [], [{:integer, [], nil}]}

      result = Annotations.parse(ast)

      assert %Type.FixedTuple{elements: [%Type.Integer{}]} = result
    end

    test "parses empty tuple" do
      ast = {:{}, [], []}

      result = Annotations.parse(ast)

      assert %Type.FixedTuple{elements: []} = result
    end

    test "parses nested tuples" do
      # {{integer, float}, boolean}
      inner = {{:integer, [], nil}, {:float, [], nil}}
      ast = {inner, {:boolean, [], nil}}

      result = Annotations.parse(ast)

      assert %Type.FixedTuple{
               elements: [
                 %Type.FixedTuple{elements: [%Type.Integer{}, %Type.Float{}]},
                 %Type.Boolean{}
               ]
             } = result
    end
  end

  # ============================================================================
  # Union Types
  # ============================================================================

  describe "union type parsing" do
    test "parses simple union" do
      # integer | float
      ast = {:|, [], [{:integer, [], nil}, {:float, [], nil}]}

      result = Annotations.parse(ast)

      assert %Type.Union{fst: %Type.Integer{}, snd: %Type.Float{}} = result
    end

    test "parses nested unions (right-associative)" do
      # integer | float | boolean parses as integer | (float | boolean)
      inner = {:|, [], [{:float, [], nil}, {:boolean, [], nil}]}
      ast = {:|, [], [{:integer, [], nil}, inner]}

      result = Annotations.parse(ast)

      # The union constructor may simplify based on subtyping, but structure should be preserved.
      assert %Type.Union{} = result
    end

    test "parses union with complex types" do
      # {integer, float} | boolean
      tuple = {{:integer, [], nil}, {:float, [], nil}}
      ast = {:|, [], [tuple, {:boolean, [], nil}]}

      result = Annotations.parse(ast)

      assert %Type.Union{
               fst: %Type.FixedTuple{elements: [%Type.Integer{}, %Type.Float{}]},
               snd: %Type.Boolean{}
             } = result
    end
  end

  # ============================================================================
  # List Types
  # ============================================================================

  describe "list type parsing" do
    test "parses list type using bracket syntax" do
      # [integer]
      ast = [{:integer, [], nil}]

      result = Annotations.parse(ast)

      assert %Type.FixedList{contents: %Type.Integer{}} = result
    end

    test "parses list type using explicit list/1 syntax" do
      # list(integer)
      ast = {:list, [], [{:integer, [], nil}]}

      result = Annotations.parse(ast)

      assert %Type.FixedList{contents: %Type.Integer{}} = result
    end

    test "parses nested list types" do
      # [[integer]]
      inner = [{:integer, [], nil}]
      ast = [inner]

      result = Annotations.parse(ast)

      assert %Type.FixedList{contents: %Type.FixedList{contents: %Type.Integer{}}} = result
    end

    test "parses list of tuples" do
      # [{integer, float}]
      tuple = {{:integer, [], nil}, {:float, [], nil}}
      ast = [tuple]

      result = Annotations.parse(ast)

      assert %Type.FixedList{
               contents: %Type.FixedTuple{elements: [%Type.Integer{}, %Type.Float{}]}
             } = result
    end
  end

  # ============================================================================
  # Function Types
  # ============================================================================

  describe "function type parsing" do
    test "parses single-argument function type" do
      # (integer -> float)
      ast = [{:->, [], [[{:integer, [], nil}], {:float, [], nil}]}]

      result = Annotations.parse(ast)

      assert %Type.Fn{inputs: [%Type.Integer{}], output: %Type.Float{}} = result
    end

    test "parses multi-argument function type" do
      # (integer, float -> boolean)
      ast = [{:->, [], [[{:integer, [], nil}, {:float, [], nil}], {:boolean, [], nil}]}]

      result = Annotations.parse(ast)

      assert %Type.Fn{inputs: [%Type.Integer{}, %Type.Float{}], output: %Type.Boolean{}} = result
    end

    test "parses zero-argument function type" do
      # (-> integer)
      ast = [{:->, [], [[], {:integer, [], nil}]}]

      result = Annotations.parse(ast)

      assert %Type.Fn{inputs: [], output: %Type.Integer{}} = result
    end

    test "parses function returning function (higher-order)" do
      # (integer -> (float -> boolean))
      inner_fn = [{:->, [], [[{:float, [], nil}], {:boolean, [], nil}]}]
      ast = [{:->, [], [[{:integer, [], nil}], inner_fn]}]

      result = Annotations.parse(ast)

      assert %Type.Fn{
               inputs: [%Type.Integer{}],
               output: %Type.Fn{inputs: [%Type.Float{}], output: %Type.Boolean{}}
             } = result
    end

    test "parses function with tuple argument" do
      # ({integer, float} -> boolean)
      tuple = {{:integer, [], nil}, {:float, [], nil}}
      ast = [{:->, [], [[tuple], {:boolean, [], nil}]}]

      result = Annotations.parse(ast)

      assert %Type.Fn{
               inputs: [%Type.FixedTuple{elements: [%Type.Integer{}, %Type.Float{}]}],
               output: %Type.Boolean{}
             } = result
    end
  end

  # ============================================================================
  # Type Aliases
  # ============================================================================

  describe "type alias parsing" do
    test "parses simple type alias" do
      # MyType (module-local)
      ast = {:MyType, [], nil}

      result = Annotations.parse(ast)

      assert %Type.Alias{name: :MyType, context: nil} = result
    end

    test "parses type alias with context" do
      # my_type with Elixir context
      ast = {:my_type, [], Elixir}

      result = Annotations.parse(ast)

      assert %Type.Alias{name: :my_type, context: Elixir} = result
    end
  end

  # ============================================================================
  # Complex Combinations
  # ============================================================================

  describe "complex type combinations" do
    test "parses function returning union" do
      # (integer -> integer | float)
      union = {:|, [], [{:integer, [], nil}, {:float, [], nil}]}
      ast = [{:->, [], [[{:integer, [], nil}], union]}]

      result = Annotations.parse(ast)

      assert %Type.Fn{inputs: [%Type.Integer{}], output: %Type.Union{}} = result
    end

    test "parses list of unions" do
      # [integer | float]
      union = {:|, [], [{:integer, [], nil}, {:float, [], nil}]}
      ast = [union]

      result = Annotations.parse(ast)

      assert %Type.FixedList{contents: %Type.Union{}} = result
    end

    test "parses tuple containing function" do
      # {integer, (float -> boolean)}
      fn_type = [{:->, [], [[{:float, [], nil}], {:boolean, [], nil}]}]
      ast = {{:integer, [], nil}, fn_type}

      result = Annotations.parse(ast)

      assert %Type.FixedTuple{
               elements: [
                 %Type.Integer{},
                 %Type.Fn{inputs: [%Type.Float{}], output: %Type.Boolean{}}
               ]
             } = result
    end
  end

  # ============================================================================
  # Error Cases
  # ============================================================================

  describe "error handling" do
    test "raises on unrecognized annotation" do
      # An unrecognized AST form should raise a malformed type error.
      ast = {:unknown_construct, [], [1, 2, 3]}

      assert_raise CompileError, fn ->
        Annotations.parse(ast)
      end
    end

    test "raises on empty list annotation" do
      # Empty list [] is not a valid type annotation.
      ast = []

      assert_raise CompileError, fn ->
        Annotations.parse(ast)
      end
    end

    test "raises on non-AST input" do
      # A plain integer is not a valid type annotation.
      ast = 42

      assert_raise CompileError, fn ->
        Annotations.parse(ast)
      end
    end

    test "raises on multi-element list that is not a function type" do
      # [integer, float] is ambiguous - not a valid type syntax.
      ast = [{:integer, [], nil}, {:float, [], nil}]

      assert_raise CompileError, fn ->
        Annotations.parse(ast)
      end
    end

    test "error message includes the malformed expression" do
      # Use something that won't match any pattern - a map is not valid type syntax.
      ast = %{invalid: :type}

      error =
        assert_raise CompileError, fn ->
          Annotations.parse(ast)
        end

      # CompileError has description field with formatted error message
      assert error.description =~ "E0003"
      assert error.description =~ "Malformed type"
    end
  end
end
