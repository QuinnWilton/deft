defmodule Deft.TypeParser.EmitterTest do
  use ExUnit.Case, async: true

  alias Deft.Type
  alias Deft.TypeParser.{AST, Emitter}

  # ============================================================================
  # to_type/1 - Primitive Types
  # ============================================================================

  describe "to_type/1 with primitives" do
    test "converts integer" do
      assert %Type.Integer{} = Emitter.to_type(%AST.Primitive{kind: :integer})
    end

    test "converts float" do
      assert %Type.Float{} = Emitter.to_type(%AST.Primitive{kind: :float})
    end

    test "converts number" do
      assert %Type.Number{} = Emitter.to_type(%AST.Primitive{kind: :number})
    end

    test "converts boolean" do
      assert %Type.Boolean{} = Emitter.to_type(%AST.Primitive{kind: :boolean})
    end

    test "converts atom" do
      assert %Type.Atom{} = Emitter.to_type(%AST.Primitive{kind: :atom})
    end

    test "converts binary" do
      assert %Type.Binary{} = Emitter.to_type(%AST.Primitive{kind: :binary})
    end

    test "converts top" do
      assert %Type.Top{} = Emitter.to_type(%AST.Primitive{kind: :top})
    end

    test "converts bottom" do
      assert %Type.Bottom{} = Emitter.to_type(%AST.Primitive{kind: :bottom})
    end
  end

  # ============================================================================
  # to_type/1 - Abstract Types
  # ============================================================================

  describe "to_type/1 with abstract types" do
    test "converts abstract list" do
      assert %Type.List{} = Emitter.to_type(%AST.Abstract{kind: :list})
    end

    test "converts abstract tuple" do
      assert %Type.Tuple{} = Emitter.to_type(%AST.Abstract{kind: :tuple})
    end
  end

  # ============================================================================
  # to_type/1 - Composite Types
  # ============================================================================

  describe "to_type/1 with composite types" do
    test "converts tuple" do
      ast = %AST.Tuple{
        elements: [
          %AST.Primitive{kind: :integer},
          %AST.Primitive{kind: :float}
        ]
      }

      result = Emitter.to_type(ast)
      assert %Type.FixedTuple{elements: [%Type.Integer{}, %Type.Float{}]} = result
    end

    test "converts empty tuple" do
      ast = %AST.Tuple{elements: []}
      assert %Type.FixedTuple{elements: []} = Emitter.to_type(ast)
    end

    test "converts union" do
      ast = %AST.Union{
        left: %AST.Primitive{kind: :integer},
        right: %AST.Primitive{kind: :float}
      }

      result = Emitter.to_type(ast)
      assert %Type.Union{fst: %Type.Integer{}, snd: %Type.Float{}} = result
    end

    test "converts list" do
      ast = %AST.List{element: %AST.Primitive{kind: :integer}}
      assert %Type.FixedList{contents: %Type.Integer{}} = Emitter.to_type(ast)
    end

    test "converts function" do
      ast = %AST.Function{
        inputs: [%AST.Primitive{kind: :integer}],
        output: %AST.Primitive{kind: :boolean}
      }

      result = Emitter.to_type(ast)
      assert %Type.Fn{inputs: [%Type.Integer{}], output: %Type.Boolean{}} = result
    end

    test "converts zero-arg function" do
      ast = %AST.Function{
        inputs: [],
        output: %AST.Primitive{kind: :integer}
      }

      result = Emitter.to_type(ast)
      assert %Type.Fn{inputs: [], output: %Type.Integer{}} = result
    end

    test "converts type variable" do
      ast = %AST.Variable{name: :a}
      assert %Type.Var{name: :a} = Emitter.to_type(ast)
    end

    test "converts type alias" do
      ast = %AST.Alias{name: :MyType, context: Elixir}
      assert %Type.Alias{name: :MyType, context: Elixir} = Emitter.to_type(ast)
    end
  end

  # ============================================================================
  # to_type/1 - Nested Types
  # ============================================================================

  describe "to_type/1 with nested types" do
    test "converts nested tuples" do
      ast = %AST.Tuple{
        elements: [
          %AST.Primitive{kind: :integer},
          %AST.Tuple{
            elements: [
              %AST.Primitive{kind: :float},
              %AST.Primitive{kind: :boolean}
            ]
          }
        ]
      }

      result = Emitter.to_type(ast)

      assert %Type.FixedTuple{
               elements: [
                 %Type.Integer{},
                 %Type.FixedTuple{elements: [%Type.Float{}, %Type.Boolean{}]}
               ]
             } = result
    end

    test "converts list of functions" do
      ast = %AST.List{
        element: %AST.Function{
          inputs: [%AST.Variable{name: :a}],
          output: %AST.Variable{name: :b}
        }
      }

      result = Emitter.to_type(ast)

      assert %Type.FixedList{
               contents: %Type.Fn{
                 inputs: [%Type.Var{name: :a}],
                 output: %Type.Var{name: :b}
               }
             } = result
    end
  end

  # ============================================================================
  # to_quoted/1 - Basic Types
  # ============================================================================

  describe "to_quoted/1 with basic types" do
    test "generates quoted integer constructor" do
      ast = %AST.Primitive{kind: :integer}
      quoted = Emitter.to_quoted(ast)

      {result, _} = Code.eval_quoted(quoted)
      assert %Type.Integer{} = result
    end

    test "generates quoted float constructor" do
      ast = %AST.Primitive{kind: :float}
      quoted = Emitter.to_quoted(ast)

      {result, _} = Code.eval_quoted(quoted)
      assert %Type.Float{} = result
    end

    test "generates quoted for all primitives" do
      for {kind, expected_struct} <- [
            {:integer, Type.Integer},
            {:float, Type.Float},
            {:number, Type.Number},
            {:boolean, Type.Boolean},
            {:atom, Type.Atom},
            {:binary, Type.Binary},
            {:top, Type.Top},
            {:bottom, Type.Bottom}
          ] do
        ast = %AST.Primitive{kind: kind}
        quoted = Emitter.to_quoted(ast)

        {result, _} = Code.eval_quoted(quoted)
        assert result.__struct__ == expected_struct
      end
    end

    test "generates quoted for abstract types" do
      ast = %AST.Abstract{kind: :list}
      quoted = Emitter.to_quoted(ast)

      {result, _} = Code.eval_quoted(quoted)
      assert %Type.List{} = result
    end
  end

  # ============================================================================
  # to_quoted/1 - Composite Types
  # ============================================================================

  describe "to_quoted/1 with composite types" do
    test "generates quoted tuple constructor" do
      ast = %AST.Tuple{
        elements: [
          %AST.Primitive{kind: :integer},
          %AST.Primitive{kind: :float}
        ]
      }

      quoted = Emitter.to_quoted(ast)
      {result, _} = Code.eval_quoted(quoted)

      assert %Type.FixedTuple{elements: [%Type.Integer{}, %Type.Float{}]} = result
    end

    test "generates quoted union constructor" do
      ast = %AST.Union{
        left: %AST.Primitive{kind: :integer},
        right: %AST.Primitive{kind: :float}
      }

      quoted = Emitter.to_quoted(ast)
      {result, _} = Code.eval_quoted(quoted)

      assert %Type.Union{fst: %Type.Integer{}, snd: %Type.Float{}} = result
    end

    test "generates quoted list constructor" do
      ast = %AST.List{element: %AST.Primitive{kind: :integer}}

      quoted = Emitter.to_quoted(ast)
      {result, _} = Code.eval_quoted(quoted)

      assert %Type.FixedList{contents: %Type.Integer{}} = result
    end

    test "generates quoted function constructor" do
      ast = %AST.Function{
        inputs: [%AST.Primitive{kind: :integer}],
        output: %AST.Primitive{kind: :boolean}
      }

      quoted = Emitter.to_quoted(ast)
      {result, _} = Code.eval_quoted(quoted)

      assert %Type.Fn{inputs: [%Type.Integer{}], output: %Type.Boolean{}} = result
    end

    test "generates quoted variable constructor" do
      ast = %AST.Variable{name: :a}

      quoted = Emitter.to_quoted(ast)
      {result, _} = Code.eval_quoted(quoted)

      assert %Type.Var{name: :a} = result
    end

    test "generates quoted alias constructor" do
      ast = %AST.Alias{name: :MyType, context: Elixir}

      quoted = Emitter.to_quoted(ast)
      {result, _} = Code.eval_quoted(quoted)

      assert %Type.Alias{name: :MyType, context: Elixir} = result
    end
  end

  # ============================================================================
  # collect_variables/1
  # ============================================================================

  describe "collect_variables/1" do
    test "returns empty list for primitive" do
      ast = %AST.Primitive{kind: :integer}
      assert [] = Emitter.collect_variables(ast)
    end

    test "returns empty list for abstract type" do
      ast = %AST.Abstract{kind: :list}
      assert [] = Emitter.collect_variables(ast)
    end

    test "returns empty list for alias" do
      ast = %AST.Alias{name: :MyType}
      assert [] = Emitter.collect_variables(ast)
    end

    test "collects single variable" do
      ast = %AST.Variable{name: :a}
      assert [:a] = Emitter.collect_variables(ast)
    end

    test "collects variables from tuple" do
      ast = %AST.Tuple{
        elements: [
          %AST.Variable{name: :a},
          %AST.Primitive{kind: :integer},
          %AST.Variable{name: :b}
        ]
      }

      assert [:a, :b] = Emitter.collect_variables(ast)
    end

    test "collects variables from union" do
      ast = %AST.Union{
        left: %AST.Variable{name: :a},
        right: %AST.Variable{name: :b}
      }

      assert [:a, :b] = Emitter.collect_variables(ast)
    end

    test "collects variables from list" do
      ast = %AST.List{element: %AST.Variable{name: :a}}
      assert [:a] = Emitter.collect_variables(ast)
    end

    test "collects variables from function" do
      ast = %AST.Function{
        inputs: [%AST.Variable{name: :a}],
        output: %AST.Variable{name: :b}
      }

      assert [:a, :b] = Emitter.collect_variables(ast)
    end

    test "collects variables from multi-arg function" do
      ast = %AST.Function{
        inputs: [%AST.Variable{name: :a}, %AST.Variable{name: :b}],
        output: %AST.Variable{name: :c}
      }

      assert [:a, :b, :c] = Emitter.collect_variables(ast)
    end

    test "deduplicates repeated variables" do
      ast = %AST.Function{
        inputs: [%AST.Variable{name: :a}],
        output: %AST.Variable{name: :a}
      }

      assert [:a] = Emitter.collect_variables(ast)
    end

    test "preserves order of first appearance" do
      ast = %AST.Tuple{
        elements: [
          %AST.Variable{name: :b},
          %AST.Variable{name: :a},
          %AST.Variable{name: :b}
        ]
      }

      assert [:b, :a] = Emitter.collect_variables(ast)
    end

    test "collects from deeply nested structure" do
      # ([a] -> b) -> [a] -> [b]
      ast = %AST.Function{
        inputs: [
          %AST.Function{
            inputs: [%AST.List{element: %AST.Variable{name: :a}}],
            output: %AST.Variable{name: :b}
          },
          %AST.List{element: %AST.Variable{name: :a}}
        ],
        output: %AST.List{element: %AST.Variable{name: :b}}
      }

      assert [:a, :b] = Emitter.collect_variables(ast)
    end
  end

  # ============================================================================
  # Round-trip Tests
  # ============================================================================

  describe "to_type and to_quoted equivalence" do
    test "both produce equivalent results for primitives" do
      for kind <- [:integer, :float, :number, :boolean, :atom, :binary, :top, :bottom] do
        ast = %AST.Primitive{kind: kind}

        type_result = Emitter.to_type(ast)
        {quoted_result, _} = Code.eval_quoted(Emitter.to_quoted(ast))

        assert type_result == quoted_result
      end
    end

    test "both produce equivalent results for composite types" do
      ast = %AST.Function{
        inputs: [
          %AST.List{element: %AST.Variable{name: :a}}
        ],
        output: %AST.Variable{name: :a}
      }

      type_result = Emitter.to_type(ast)
      {quoted_result, _} = Code.eval_quoted(Emitter.to_quoted(ast))

      assert type_result == quoted_result
    end
  end

  # ============================================================================
  # to_type/2 - Location Preservation
  # ============================================================================

  describe "to_type/2 with file context" do
    test "preserves location on Type.Alias" do
      ast = %AST.Alias{name: :my_type, context: nil, span: {10, 5}}

      type = Emitter.to_type(ast, "lib/example.ex")

      assert %Type.Alias{
               name: :my_type,
               location: {"lib/example.ex", 10, 5}
             } = type
    end

    test "preserves location on Type.Alias from Application" do
      ast = %AST.Application{
        name: :option,
        args: [%AST.Primitive{kind: :integer, span: nil}],
        span: {15, 3}
      }

      type = Emitter.to_type(ast, "lib/example.ex")

      assert %Type.Alias{
               name: :option,
               args: [%Type.Integer{}],
               location: {"lib/example.ex", 15, 3}
             } = type
    end

    test "handles nil file gracefully" do
      ast = %AST.Alias{name: :my_type, context: nil, span: {10, 5}}

      type = Emitter.to_type(ast, nil)

      # Location is still set, but file is nil
      assert %Type.Alias{location: {nil, 10, 5}} = type
    end

    test "handles nil span gracefully" do
      ast = %AST.Alias{name: :my_type, context: nil, span: nil}

      type = Emitter.to_type(ast, "lib/example.ex")

      assert %Type.Alias{name: :my_type, location: nil} = type
    end

    test "to_type/1 without file defaults to nil location" do
      ast = %AST.Alias{name: :my_type, context: nil, span: {10, 5}}

      type = Emitter.to_type(ast)

      # Without file, location has nil for file
      assert %Type.Alias{location: {nil, 10, 5}} = type
    end

    test "threads file through nested structures" do
      # A union containing an alias should preserve its location
      ast = %AST.Union{
        left: %AST.Alias{name: :my_type, context: nil, span: {10, 5}},
        right: %AST.Primitive{kind: :integer, span: nil}
      }

      type = Emitter.to_type(ast, "lib/example.ex")

      assert %Type.Union{
               fst: %Type.Alias{location: {"lib/example.ex", 10, 5}}
             } = type
    end

    test "threads file through function types" do
      ast = %AST.Function{
        inputs: [%AST.Alias{name: :my_input, context: nil, span: {5, 10}}],
        output: %AST.Alias{name: :my_output, context: nil, span: {5, 25}}
      }

      type = Emitter.to_type(ast, "lib/example.ex")

      assert %Type.Fn{
               inputs: [%Type.Alias{location: {"lib/example.ex", 5, 10}}],
               output: %Type.Alias{location: {"lib/example.ex", 5, 25}}
             } = type
    end
  end
end
