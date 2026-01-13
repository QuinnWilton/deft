defmodule Deft.ErrorTest do
  use ExUnit.Case, async: true

  alias Deft.Error
  alias Deft.Type

  describe "Error struct" do
    test "has all required fields" do
      error = %Error{
        code: :type_mismatch,
        message: "test message"
      }

      assert error.code == :type_mismatch
      assert error.message == "test message"
      assert error.expected == nil
      assert error.actual == nil
      assert error.location == nil
      assert error.expression == nil
      assert error.suggestions == []
      assert error.notes == []
      assert error.severity == :error
    end
  end

  describe "type_mismatch/1" do
    test "creates error with expected and actual types" do
      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float()
        )

      assert error.code == :type_mismatch
      assert error.expected == Type.integer()
      assert error.actual == Type.float()
      assert error.message =~ "expected"
      assert error.message =~ "integer"
      assert error.message =~ "float"
    end

    test "includes location when provided" do
      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float(),
          location: {"test.ex", 10, 5}
        )

      assert error.location == {"test.ex", 10, 5}
    end

    test "includes suggestions when provided" do
      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float(),
          suggestions: ["Use trunc/1", "Change type to number"]
        )

      assert error.suggestions == ["Use trunc/1", "Change type to number"]
    end
  end

  describe "missing_annotation/1" do
    test "creates error with expression" do
      expr = quote do: x + 1

      error = Error.missing_annotation(expression: expr)

      assert error.code == :missing_annotation
      assert error.expression == expr
      assert error.message == "Missing type annotation on expression"
      assert Enum.any?(error.suggestions, &String.contains?(&1, "Add a type annotation"))
    end
  end

  describe "unsupported_call/1" do
    test "creates error with function name and arity" do
      error =
        Error.unsupported_call(
          name: :foo,
          arity: 2
        )

      assert error.code == :unsupported_call
      assert error.message =~ "foo/2"
    end
  end

  describe "unsupported_function/1" do
    test "creates error with module, function, arity, and reason" do
      error =
        Error.unsupported_function(
          module: MyModule,
          function: :my_func,
          arity: 2,
          reason: "Returns a map type which Deft cannot represent"
        )

      assert error.code == :unsupported_function
      assert error.message =~ "MyModule.my_func/2"
      assert error.message =~ "cannot be typed"
      assert Enum.any?(error.notes, &(&1 == "Returns a map type which Deft cannot represent"))
    end

    test "includes reason in notes" do
      error =
        Error.unsupported_function(
          module: Enum,
          function: :group_by,
          arity: 2,
          reason: "Map types are not supported"
        )

      assert ["Map types are not supported"] = error.notes
    end
  end

  describe "inexhaustive_patterns/1" do
    test "creates error with missing patterns" do
      missing = Type.variant(:some, nil, [Type.integer()])

      error = Error.inexhaustive_patterns(missing: missing)

      assert error.code == :inexhaustive_patterns
      assert error.message == "Non-exhaustive pattern matching"
      assert length(error.suggestions) == 1
      assert hd(error.suggestions) =~ "some"
    end

    test "handles list of missing patterns" do
      missing = [
        Type.variant(:some, nil, []),
        Type.variant(:none, nil, [])
      ]

      error = Error.inexhaustive_patterns(missing: missing)

      assert length(error.suggestions) == 2
    end
  end

  describe "unreachable_branch/1" do
    test "creates error with subject and pattern types" do
      error =
        Error.unreachable_branch(
          subject_type: Type.integer(),
          pattern_type: Type.float()
        )

      assert error.code == :unreachable_branch
      assert error.message =~ "Pattern `float` can never match subject of type `integer`"
      assert "Remove this unreachable branch" in error.suggestions
    end

    test "falls back to expected/actual for backwards compatibility" do
      error =
        Error.unreachable_branch(
          expected: Type.integer(),
          actual: Type.float()
        )

      assert error.code == :unreachable_branch
      assert error.message =~ "Pattern `float` can never match subject of type `integer`"
    end
  end

  describe "no_matching_rule/1" do
    test "creates error with AST" do
      ast = {:foo, [], nil}

      error = Error.no_matching_rule(ast: ast)

      assert error.code == :no_matching_rule
      assert error.expression == ast
    end
  end

  describe "missing_features/1" do
    test "creates error with feature list" do
      error = Error.missing_features(features: [:polymorphism, :effects])

      assert error.code == :missing_features
      assert length(error.suggestions) == 2
      assert Enum.any?(error.suggestions, &String.contains?(&1, "polymorphism"))
    end
  end

  describe "subtype_violation/1" do
    test "creates error with expected and actual types" do
      error =
        Error.subtype_violation(
          expected: Type.number(),
          actual: Type.atom()
        )

      assert error.code == :subtype_violation
      assert error.expected == Type.number()
      assert error.actual == Type.atom()
    end
  end

  describe "error_code_string/1" do
    test "returns correct code for each error type" do
      assert Error.error_code_string(:type_mismatch) == "E0001"
      assert Error.error_code_string(:missing_annotation) == "E0002"
      assert Error.error_code_string(:malformed_type) == "E0003"
      assert Error.error_code_string(:unsupported_call) == "E0004"
      assert Error.error_code_string(:inexhaustive_patterns) == "E0005"
      assert Error.error_code_string(:unreachable_branch) == "E0006"
      assert Error.error_code_string(:no_matching_rule) == "E0007"
      assert Error.error_code_string(:missing_features) == "E0008"
      assert Error.error_code_string(:subtype_violation) == "E0009"
      assert Error.error_code_string(:unsupported_syntax) == "E0010"
      assert Error.error_code_string(:unsupported_pattern) == "E0011"
      assert Error.error_code_string(:unsupported_function) == "E0012"
    end

    test "returns E0000 for unknown codes" do
      assert Error.error_code_string(:unknown) == "E0000"
    end
  end

  describe "extract_location/1" do
    test "extracts location from 3-tuple AST" do
      ast = {:foo, [line: 10, column: 5], []}

      assert Error.extract_location(ast) == {nil, 10, 5}
    end

    test "extracts location with file" do
      ast = {:foo, [line: 10, column: 5, file: "test.ex"], []}

      assert Error.extract_location(ast) == {"test.ex", 10, 5}
    end

    test "extracts location from struct with meta" do
      struct = %{meta: [line: 10, column: 5]}

      assert Error.extract_location(struct) == {nil, 10, 5}
    end

    test "returns nil for AST without location" do
      assert Error.extract_location({:foo, [], []}) == nil
      assert Error.extract_location(42) == nil
    end
  end

  describe "extract_location_from_env/1" do
    test "extracts location from Macro.Env" do
      env = %Macro.Env{file: "test.ex", line: 10}

      assert Error.extract_location_from_env(env) == {"test.ex", 10, nil}
    end

    test "returns nil for nil env" do
      assert Error.extract_location_from_env(nil) == nil
    end
  end

  describe "format_type/1" do
    test "formats primitive types" do
      assert Error.format_type(Type.integer()) == "integer"
      assert Error.format_type(Type.float()) == "float"
      assert Error.format_type(Type.number()) == "number"
      assert Error.format_type(Type.boolean()) == "boolean"
      assert Error.format_type(Type.atom()) == "atom"
      assert Error.format_type(Type.top()) == "any"
      assert Error.format_type(Type.bottom()) == "never"
    end

    test "formats function types" do
      fn_type = Type.fun([Type.integer(), Type.integer()], Type.integer())

      assert Error.format_type(fn_type) == "(integer, integer -> integer)"
    end

    test "formats union types" do
      union = Type.union(Type.integer(), Type.float())

      assert Error.format_type(union) == "integer | float"
    end

    test "formats tuple types" do
      tuple = Type.fixed_tuple([Type.integer(), Type.atom()])

      assert Error.format_type(tuple) == "{integer, atom}"
    end

    test "formats list types" do
      list = Type.fixed_list(Type.integer())

      assert Error.format_type(list) == "[integer]"
    end

    test "formats nil as unknown" do
      assert Error.format_type(nil) == "unknown"
    end
  end
end
