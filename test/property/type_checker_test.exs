defmodule Deft.Property.TypeCheckerTest do
  @moduledoc """
  Property-based tests for the type checker.

  These tests verify fundamental properties that should hold for any
  well-typed expression in the language.
  """
  use Deft.PropertyCase, async: true

  alias Deft.Generators.Types

  describe "well-formedness" do
    property "synthesized types are always well-formed" do
      check all(type <- Types.primitive_type(), max_shrinking_steps: 0) do
        assert Type.well_formed?(type)
      end
    end

    property "compound types are well-formed" do
      check all(type <- Types.compound_type(), max_shrinking_steps: 0) do
        assert Type.well_formed?(type)
      end
    end

    property "union types are well-formed" do
      check all(type <- Types.union_type(), max_shrinking_steps: 0) do
        assert Type.well_formed?(type)
      end
    end
  end

  describe "literal type synthesis" do
    property "integer literals have Integer type" do
      check all(n <- integer(), max_shrinking_steps: 0) do
        ast = Deft.AST.Literal.new(n)
        ctx = Deft.Context.new(__ENV__)

        {:ok, _erased, type, _bindings, _ctx} = Deft.TypeChecker.check(ast, ctx)

        assert %Type.Integer{} = type
      end
    end

    property "float literals have Float type" do
      check all(f <- float(), max_shrinking_steps: 0) do
        ast = Deft.AST.Literal.new(f)
        ctx = Deft.Context.new(__ENV__)

        {:ok, _erased, type, _bindings, _ctx} = Deft.TypeChecker.check(ast, ctx)

        assert %Type.Float{} = type
      end
    end

    property "boolean literals have Boolean type" do
      check all(b <- boolean(), max_shrinking_steps: 0) do
        ast = Deft.AST.Literal.new(b)
        ctx = Deft.Context.new(__ENV__)

        {:ok, _erased, type, _bindings, _ctx} = Deft.TypeChecker.check(ast, ctx)

        assert %Type.Boolean{} = type
      end
    end

    property "atom literals have Atom type" do
      check all(
              a <- atom(:alphanumeric),
              a not in [true, false],
              max_shrinking_steps: 0
            ) do
        ast = Deft.AST.Literal.new(a)
        ctx = Deft.Context.new(__ENV__)

        {:ok, _erased, type, _bindings, _ctx} = Deft.TypeChecker.check(ast, ctx)

        assert %Type.Atom{} = type
      end
    end
  end

  describe "erasure" do
    property "literal erasure equals original value" do
      check all(n <- integer(), max_shrinking_steps: 0) do
        ast = Deft.AST.Literal.new(n)
        ctx = Deft.Context.new(__ENV__)

        {:ok, erased, _type, _bindings, _ctx} = Deft.TypeChecker.check(ast, ctx)

        assert erased == n
      end
    end

    property "boolean erasure equals original value" do
      check all(b <- boolean(), max_shrinking_steps: 0) do
        ast = Deft.AST.Literal.new(b)
        ctx = Deft.Context.new(__ENV__)

        {:ok, erased, _type, _bindings, _ctx} = Deft.TypeChecker.check(ast, ctx)

        assert erased == b
      end
    end
  end

  describe "tuple synthesis" do
    property "tuple element count matches synthesized type" do
      check all(
              types <- list_of(Types.primitive_type(), min_length: 0, max_length: 5),
              max_shrinking_steps: 0
            ) do
        elements = Enum.map(types, &type_to_literal/1)
        ast = Deft.AST.Tuple.new(elements)
        ctx = Deft.Context.new(__ENV__)

        {:ok, _erased, type, _bindings, _ctx} = Deft.TypeChecker.check(ast, ctx)

        assert %Type.FixedTuple{elements: elem_types} = type
        assert length(elem_types) == length(types)
      end
    end
  end

  describe "function synthesis" do
    property "function input count matches arg count" do
      check all(
              arg_types <- list_of(Types.primitive_type(), min_length: 1, max_length: 3),
              max_shrinking_steps: 0
            ) do
        args =
          Enum.with_index(arg_types, fn type, i ->
            name = String.to_atom("x#{i}")
            Deft.AST.Annotation.new(Deft.AST.Local.new(name, nil, []), type)
          end)

        body = Deft.AST.Literal.new(42)
        ast = Deft.AST.Fn.new(body, args)
        ctx = Deft.Context.new(__ENV__)

        {:ok, _erased, type, _bindings, _ctx} = Deft.TypeChecker.check(ast, ctx)

        assert %Type.Fn{inputs: inputs} = type
        assert length(inputs) == length(arg_types)
      end
    end
  end

  describe "determinism" do
    property "same input produces same output" do
      check all(n <- integer(), max_shrinking_steps: 0) do
        ast = Deft.AST.Literal.new(n)
        ctx = Deft.Context.new(__ENV__)

        {:ok, erased1, type1, bindings1, _ctx1} = Deft.TypeChecker.check(ast, ctx)
        {:ok, erased2, type2, bindings2, _ctx2} = Deft.TypeChecker.check(ast, ctx)

        assert erased1 == erased2
        assert type1 == type2
        assert bindings1 == bindings2
      end
    end
  end

  # Helper to create a literal of a given type
  defp type_to_literal(%Type.Integer{}), do: Deft.AST.Literal.new(42)
  defp type_to_literal(%Type.Float{}), do: Deft.AST.Literal.new(3.14)
  defp type_to_literal(%Type.Boolean{}), do: Deft.AST.Literal.new(true)
  defp type_to_literal(%Type.Atom{}), do: Deft.AST.Literal.new(:ok)
  defp type_to_literal(%Type.Number{}), do: Deft.AST.Literal.new(42)
  defp type_to_literal(_), do: Deft.AST.Literal.new(nil)
end
