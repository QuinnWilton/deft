defmodule Deft.CompilerTest do
  use ExUnit.Case

  alias Deft.Compiler

  # ============================================================================
  # Unsupported Expression Syntax
  # ============================================================================

  describe "compile/1 unsupported syntax" do
    test "raises on map literal" do
      ast = quote do: %{a: 1, b: 2}

      error =
        assert_raise Deft.Error.Exception, fn ->
          Compiler.compile(ast)
        end

      assert error.error.code == :unsupported_syntax
      assert Enum.any?(error.error.notes, &String.contains?(&1, "Map literals are not supported"))
    end

    test "raises on struct literal" do
      ast = {:%, [], [{:__aliases__, [], [:User]}, {:%{}, [], [name: "test"]}]}

      error =
        assert_raise Deft.Error.Exception, fn ->
          Compiler.compile(ast)
        end

      assert error.error.code == :unsupported_syntax
      assert Enum.any?(error.error.notes, &String.contains?(&1, "Struct literals are not supported"))
    end

    test "raises on comprehension" do
      ast = quote do: for(x <- [1, 2, 3], do: x * 2)

      error =
        assert_raise Deft.Error.Exception, fn ->
          Compiler.compile(ast)
        end

      assert error.error.code == :unsupported_syntax
      assert Enum.any?(error.error.notes, &String.contains?(&1, "Comprehensions are not supported"))
    end

    test "raises on with expression" do
      ast = quote do: with({:ok, x} <- {:ok, 1}, do: x)

      error =
        assert_raise Deft.Error.Exception, fn ->
          Compiler.compile(ast)
        end

      assert error.error.code == :unsupported_syntax
      assert Enum.any?(error.error.notes, &String.contains?(&1, "`with` construct is not supported"))
    end

    test "raises on receive" do
      ast = quote do: receive(do: (x -> x))

      error =
        assert_raise Deft.Error.Exception, fn ->
          Compiler.compile(ast)
        end

      assert error.error.code == :unsupported_syntax
      assert Enum.any?(error.error.notes, &String.contains?(&1, "`receive` construct is not supported"))
    end

    test "raises on try" do
      ast = quote do: try(do: 1, rescue: (e -> e))

      error =
        assert_raise Deft.Error.Exception, fn ->
          Compiler.compile(ast)
        end

      assert error.error.code == :unsupported_syntax
      assert Enum.any?(error.error.notes, &String.contains?(&1, "`try` construct is not supported"))
    end

    test "raises on import inside block" do
      ast = quote do: import(Enum)

      error =
        assert_raise Deft.Error.Exception, fn ->
          Compiler.compile(ast)
        end

      assert error.error.code == :unsupported_syntax
      assert Enum.any?(error.error.notes, &String.contains?(&1, "`import` directive is not supported"))
    end

    test "raises on require inside block" do
      ast = quote do: require(Logger)

      error =
        assert_raise Deft.Error.Exception, fn ->
          Compiler.compile(ast)
        end

      assert error.error.code == :unsupported_syntax
      assert Enum.any?(error.error.notes, &String.contains?(&1, "`require` directive is not supported"))
    end

    test "raises on alias inside block" do
      ast = quote do: alias(My.Module)

      error =
        assert_raise Deft.Error.Exception, fn ->
          Compiler.compile(ast)
        end

      assert error.error.code == :unsupported_syntax
      assert Enum.any?(error.error.notes, &String.contains?(&1, "`alias` directive is not supported"))
    end

    test "provides generic error for unknown syntax" do
      # Use a map since it's the most reliable way to get to the catch-all.
      ast = %{unknown: "structure"}

      error =
        assert_raise Deft.Error.Exception, fn ->
          Compiler.compile(ast)
        end

      assert error.error.code == :unsupported_syntax
      assert Enum.any?(error.error.notes, &String.contains?(&1, "syntax is not recognized"))
    end
  end

  # ============================================================================
  # Unsupported Pattern Syntax
  # ============================================================================

  describe "compile_pattern/1 unsupported syntax" do
    test "raises on map pattern" do
      ast = quote do: %{a: x}

      error =
        assert_raise Deft.Error.Exception, fn ->
          Compiler.compile_pattern(ast)
        end

      assert error.error.code == :unsupported_pattern
      assert Enum.any?(error.error.notes, &String.contains?(&1, "Map patterns are not supported"))
    end

    test "raises on struct pattern" do
      ast = {:%, [], [{:__aliases__, [], [:User]}, {:%{}, [], [name: {:x, [], nil}]}]}

      error =
        assert_raise Deft.Error.Exception, fn ->
          Compiler.compile_pattern(ast)
        end

      assert error.error.code == :unsupported_pattern
      assert Enum.any?(error.error.notes, &String.contains?(&1, "Struct patterns are not supported"))
    end

    test "provides generic error for unknown pattern" do
      # A pattern that doesn't match any clause.
      ast = %{weird: "structure"}

      error =
        assert_raise Deft.Error.Exception, fn ->
          Compiler.compile_pattern(ast)
        end

      assert error.error.code == :unsupported_pattern
      assert Enum.any?(error.error.notes, &String.contains?(&1, "pattern is not recognized"))
    end
  end

  # ============================================================================
  # Valid Compilation (sanity checks)
  # ============================================================================

  describe "compile/1 valid syntax" do
    test "compiles literals" do
      assert %Deft.AST.Literal{value: 42} = Compiler.compile(42)
      assert %Deft.AST.Literal{value: :atom} = Compiler.compile(:atom)
      assert %Deft.AST.Literal{value: "string"} = Compiler.compile("string")
      assert %Deft.AST.Literal{value: true} = Compiler.compile(true)
      assert %Deft.AST.Literal{value: 3.14} = Compiler.compile(3.14)
    end

    test "compiles variables" do
      ast = quote do: x

      assert %Deft.AST.Local{name: :x} = Compiler.compile(ast)
    end

    test "compiles tuples" do
      ast = quote do: {1, 2, 3}

      assert %Deft.AST.Tuple{} = Compiler.compile(ast)
    end

    test "compiles lists" do
      ast = quote do: [1, 2, 3]

      assert %Deft.AST.List{} = Compiler.compile(ast)
    end

    test "compiles if expressions" do
      ast = quote do: if(true, do: 1, else: 2)

      assert %Deft.AST.If{} = Compiler.compile(ast)
    end

    test "compiles case expressions" do
      ast = quote do: case(x, do: (1 -> :one))

      assert %Deft.AST.Case{} = Compiler.compile(ast)
    end

    test "compiles function calls" do
      ast = quote do: foo(1, 2)

      assert %Deft.AST.LocalCall{name: :foo} = Compiler.compile(ast)
    end
  end

  # ============================================================================
  # ADT Variant Error Context
  # ============================================================================

  describe "compile_adt_variant error context" do
    test "error includes variant name and column index" do
      # An ADT variant with an invalid type annotation.
      # variant_ast represents: my_variant(integer, [invalid])
      variant_ast = {:my_variant, [], [{:integer, [], nil}, []]}
      adt_name = %Deft.AST.Local{name: :my_adt, context: nil, meta: []}

      error =
        assert_raise Deft.Error.Exception, fn ->
          Compiler.compile_adt_variant(variant_ast, adt_name)
        end

      # Error should include context about which variant and column failed.
      assert Enum.any?(error.error.notes, &String.contains?(&1, "my_variant"))
      assert Enum.any?(error.error.notes, &String.contains?(&1, "my_adt"))
      assert Enum.any?(error.error.notes, &String.contains?(&1, "Column 2"))
    end
  end

  describe "compile_pattern/1 valid patterns" do
    test "compiles literal patterns" do
      assert %Deft.AST.Literal{value: 42} = Compiler.compile_pattern(42)
      assert %Deft.AST.Literal{value: :atom} = Compiler.compile_pattern(:atom)
    end

    test "compiles variable patterns" do
      ast = quote do: x

      assert %Deft.AST.Local{name: :x} = Compiler.compile_pattern(ast)
    end

    test "compiles tuple patterns" do
      ast = quote do: {a, b, c}

      assert %Deft.AST.Tuple{} = Compiler.compile_pattern(ast)
    end

    test "compiles list patterns" do
      ast = quote do: [a, b]

      assert %Deft.AST.List{} = Compiler.compile_pattern(ast)
    end

    test "compiles cons patterns" do
      # The cons pattern [h | t] is represented as {:|, meta, [h, t]} in AST.
      ast = {:|, [], [{:h, [], nil}, {:t, [], nil}]}

      assert %Deft.AST.Cons{} = Compiler.compile_pattern(ast)
    end

    test "compiles pin patterns" do
      ast = quote do: ^x

      assert %Deft.AST.Pin{} = Compiler.compile_pattern(ast)
    end
  end
end
