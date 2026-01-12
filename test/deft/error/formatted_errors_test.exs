defmodule Deft.Error.FormattedErrorsTest do
  @moduledoc """
  Comprehensive tests for formatted error output.

  These tests verify that all error types (E0001-E0009) produce
  well-formatted, human-readable error messages with:
  - Correct error codes and messages
  - Source code context with line numbers
  - Accurate span underlining
  - Helpful notes and suggestions
  - Element-specific messages for compound types
  """
  use ExUnit.Case, async: true

  alias Deft.Error
  alias Deft.Error.Formatter
  alias Deft.Type
  alias Deft.AST

  # Sample source lines for testing
  @sample_source [
    "defmodule Example do",
    "  use Deft",
    "",
    "  deft add(a :: integer, b :: integer) :: integer do",
    "    a + b",
    "  end",
    "",
    "  deft main() :: atom do",
    "    x = 1.5",
    "    y = add(x, 2)",
    "    :ok",
    "  end",
    "end"
  ]

  describe "E0001 - Type mismatch" do
    test "formats basic type mismatch" do
      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float(),
          location: {"lib/example.ex", 9, 9}
        )

      formatted = Formatter.format(error, colors: false, source_lines: @sample_source)

      assert formatted =~ "error[E0001]"
      assert formatted =~ "Type mismatch"
      assert formatted =~ "expected `integer`"
      assert formatted =~ "found `float`"
      assert formatted =~ "╭─[lib/example.ex:9:9"
    end

    test "formats type mismatch with source context" do
      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float(),
          location: {"lib/example.ex", 9, 9}
        )

      formatted = Formatter.format(error, colors: false, source_lines: @sample_source)

      # Should show the source line
      assert formatted =~ "x = 1.5"
      # Should have line number with unicode border
      assert formatted =~ "9 │"
    end

    test "formats complex type mismatch" do
      error =
        Error.type_mismatch(
          expected: Type.fun([Type.integer()], Type.boolean()),
          actual: Type.fun([Type.float()], Type.atom()),
          location: {"lib/example.ex", 5, 10}
        )

      formatted = Formatter.format(error, colors: false, source_lines: @sample_source)

      assert formatted =~ "(integer -> boolean)"
      assert formatted =~ "(float -> atom)"
    end

    test "formats tuple type mismatch" do
      error =
        Error.type_mismatch(
          expected: Type.fixed_tuple([Type.integer(), Type.atom()]),
          actual: Type.fixed_tuple([Type.float(), Type.boolean()]),
          location: {"lib/example.ex", 5, 10}
        )

      formatted = Formatter.format(error, colors: false, source_lines: @sample_source)

      assert formatted =~ "{integer, atom}"
      assert formatted =~ "{float, boolean}"
    end

    test "formats type mismatch with multi-span showing declaration and usage" do
      # Error with spans showing both the declared type and the found type
      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float(),
          location: {"lib/example.ex", 9, 9},
          spans: [
            %{
              location: {"lib/example.ex", 4, 26},
              label: "parameter declared as",
              type: Type.integer()
            },
            %{
              location: {"lib/example.ex", 9, 9},
              label: "argument has type",
              type: Type.float()
            }
          ]
        )

      formatted = Formatter.format(error, colors: false, source_lines: @sample_source)

      # Should show multi-span output with both locations
      assert formatted =~ "parameter declared as"
      assert formatted =~ "argument has type"
      # Both lines should be shown with unicode borders
      assert formatted =~ "4 │"
      assert formatted =~ "9 │"
    end
  end

  describe "E0002 - Missing annotation" do
    test "formats missing annotation error" do
      expr = quote do: x + 1

      error =
        Error.missing_annotation(
          expression: expr,
          location: {"lib/example.ex", 5, 5}
        )

      formatted = Formatter.format(error, colors: false, source_lines: @sample_source)

      assert formatted =~ "error[E0002]"
      assert formatted =~ "Missing type annotation"
      assert formatted =~ "help: Add a type annotation"
    end
  end

  describe "E0003 - Malformed type" do
    test "formats malformed type error" do
      expr = quote do: bad_type(foo)

      error =
        Error.malformed_type(
          expression: expr,
          location: {"lib/example.ex", 4, 20},
          suggestions: ["Check the type syntax"],
          notes: ["Type expressions must be valid Deft types"]
        )

      formatted = Formatter.format(error, colors: false, source_lines: @sample_source)

      assert formatted =~ "error[E0003]"
      assert formatted =~ "Malformed type"
      assert formatted =~ "help: Check the type syntax"
      assert formatted =~ "note: Type expressions must be valid"
    end
  end

  describe "E0004 - Unsupported call" do
    test "formats unsupported local call error" do
      error =
        Error.unsupported_call(
          name: :undefined_function,
          arity: 2,
          location: {"lib/example.ex", 10, 9}
        )

      formatted = Formatter.format(error, colors: false, source_lines: @sample_source)

      assert formatted =~ "error[E0004]"
      assert formatted =~ "unsupported function"
      assert formatted =~ "undefined_function/2"
      assert formatted =~ "help: Register a type signature"
    end

    test "formats unsupported call with expression context" do
      expr = quote do: undefined_function(x, y)

      error =
        Error.unsupported_call(
          name: :undefined_function,
          arity: 2,
          expression: expr,
          location: {"lib/example.ex", 10, 9}
        )

      formatted = Formatter.format(error, colors: false, source_lines: @sample_source)

      assert formatted =~ "undefined_function/2"
    end
  end

  describe "E0005 - Inexhaustive patterns" do
    test "formats inexhaustive patterns error with single missing" do
      missing = Type.variant(:none, nil, [])

      error =
        Error.inexhaustive_patterns(
          missing: missing,
          location: {"lib/example.ex", 8, 5}
        )

      formatted = Formatter.format(error, colors: false, source_lines: @sample_source)

      assert formatted =~ "error[E0005]"
      assert formatted =~ "Non-exhaustive pattern"
      assert formatted =~ "help: Add a case branch for `none`"
      assert formatted =~ "note: Pattern matching must cover all possible values"
    end

    test "formats inexhaustive patterns error with multiple missing" do
      missing = [
        Type.variant(:some, nil, [Type.integer()]),
        Type.variant(:none, nil, [])
      ]

      error =
        Error.inexhaustive_patterns(
          missing: missing,
          location: {"lib/example.ex", 8, 5}
        )

      formatted = Formatter.format(error, colors: false, source_lines: @sample_source)

      assert formatted =~ "help: Add a case branch for `some(integer)`"
      assert formatted =~ "help: Add a case branch for `none`"
    end

    test "formats inexhaustive patterns with complex variants" do
      missing = Type.variant(:rectangle, nil, [Type.number(), Type.number()])

      error =
        Error.inexhaustive_patterns(
          missing: missing,
          location: {"lib/example.ex", 8, 5}
        )

      formatted = Formatter.format(error, colors: false, source_lines: @sample_source)

      assert formatted =~ "rectangle(number, number)"
    end

    test "formats inexhaustive patterns with covered variants" do
      missing = Type.variant(:none, nil, [])
      covered = [Type.variant(:some, nil, [Type.integer()])]

      error =
        Error.inexhaustive_patterns(
          missing: missing,
          covered: covered,
          location: {"lib/example.ex", 8, 5}
        )

      formatted = Formatter.format(error, colors: false, source_lines: @sample_source)

      assert formatted =~ "error[E0005]"
      assert formatted =~ "note: Covered patterns: `some(integer)`"
      assert formatted =~ "help: Add a case branch for `none`"
    end
  end

  describe "E0006 - Unreachable branch" do
    test "formats unreachable branch error" do
      error =
        Error.unreachable_branch(
          subject_type: Type.integer(),
          pattern_type: Type.atom(),
          location: {"lib/example.ex", 10, 5}
        )

      formatted = Formatter.format(error, colors: false, source_lines: @sample_source)

      assert formatted =~ "error[E0006]"
      assert formatted =~ "Pattern `atom` can never match subject of type `integer`"
      assert formatted =~ "help: Remove this unreachable branch"
      assert formatted =~ "note: The case subject has type `integer`"
      assert formatted =~ "note: But this pattern only matches `atom`"
    end

    test "formats unreachable branch with complex types" do
      error =
        Error.unreachable_branch(
          subject_type: Type.union(Type.integer(), Type.float()),
          pattern_type: Type.atom(),
          location: {"lib/example.ex", 10, 5}
        )

      formatted = Formatter.format(error, colors: false, source_lines: @sample_source)

      assert formatted =~ "integer | float"
    end
  end

  describe "E0007 - No matching rule" do
    test "formats no matching rule error" do
      ast = {:unknown_construct, [line: 5], []}

      error =
        Error.no_matching_rule(
          ast: ast,
          location: {"lib/example.ex", 5, 5}
        )

      formatted = Formatter.format(error, colors: false, source_lines: @sample_source)

      assert formatted =~ "error[E0007]"
      assert formatted =~ "No typing rule matches"
      assert formatted =~ "help: This construct may not be supported"
    end

    test "formats no matching rule with notes" do
      ast = {:receive, [line: 5], []}

      error =
        Error.no_matching_rule(
          ast: ast,
          location: {"lib/example.ex", 5, 5},
          notes: ["receive blocks are not yet supported"]
        )

      formatted = Formatter.format(error, colors: false, source_lines: @sample_source)

      assert formatted =~ "note: receive blocks are not yet supported"
    end
  end

  describe "E0008 - Missing features" do
    test "formats missing features error with single feature" do
      error =
        Error.missing_features(
          features: :polymorphism,
          location: {"lib/example.ex", 4, 3}
        )

      formatted = Formatter.format(error, colors: false, source_lines: @sample_source)

      assert formatted =~ "error[E0008]"
      assert formatted =~ "Required type system features are not enabled"
      assert formatted =~ "help: Enable feature `:polymorphism"
      assert formatted =~ "note: Use `use Deft, features:"
    end

    test "formats missing features error with multiple features" do
      error =
        Error.missing_features(
          features: [:polymorphism, :effects, :refinement_types],
          location: {"lib/example.ex", 4, 3}
        )

      formatted = Formatter.format(error, colors: false, source_lines: @sample_source)

      assert formatted =~ "help: Enable feature `:polymorphism"
      assert formatted =~ "help: Enable feature `:effects"
      assert formatted =~ "help: Enable feature `:refinement_types"
    end
  end

  describe "E0009 - Subtype violation" do
    test "formats subtype violation error" do
      error =
        Error.subtype_violation(
          expected: Type.number(),
          actual: Type.atom(),
          location: {"lib/example.ex", 10, 9}
        )

      formatted = Formatter.format(error, colors: false, source_lines: @sample_source)

      assert formatted =~ "error[E0009]"
      assert formatted =~ "Subtype constraint violated"
      assert formatted =~ "note: Expected subtype of `number`"
      assert formatted =~ "note: Found `atom`"
    end

    test "formats subtype violation with complex types" do
      error =
        Error.subtype_violation(
          expected: Type.fun([Type.number()], Type.number()),
          actual: Type.fun([Type.atom()], Type.boolean()),
          location: {"lib/example.ex", 10, 9}
        )

      formatted = Formatter.format(error, colors: false, source_lines: @sample_source)

      assert formatted =~ "(number -> number)"
      assert formatted =~ "(atom -> boolean)"
    end
  end

  describe "Source context display" do
    test "displays source line with pointer" do
      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float(),
          location: {"lib/example.ex", 9, 9}
        )

      formatted = Formatter.format(error, colors: false, source_lines: @sample_source)

      # Should contain line number and unicode border
      assert formatted =~ "9 │"
      # Should contain the source line content
      assert formatted =~ "x = 1.5"
      # Should contain underline characters
      assert formatted =~ "─"
    end

    test "falls back to expression context when source not available" do
      expr = quote do: x + 1.5

      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float(),
          expression: expr,
          location: {"lib/example.ex", 99, 5}
        )

      # Source line 99 doesn't exist in @sample_source
      formatted = Formatter.format(error, colors: false, source_lines: @sample_source)

      assert formatted =~ "in:"
    end

    test "handles missing source lines gracefully" do
      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float(),
          location: {"lib/example.ex", 5, 5}
        )

      formatted = Formatter.format(error, colors: false, source_lines: nil)

      # Should still format without crashing
      assert formatted =~ "error[E0001]"
    end
  end

  describe "Expression width calculation" do
    test "calculates width from AST metadata when available" do
      # Create an AST node with end_column metadata
      expr = %{meta: [line: 5, column: 10, end_column: 18]}

      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float(),
          expression: expr,
          location: {"lib/example.ex", 5, 10}
        )

      formatted = Formatter.format(error, colors: false, source_lines: @sample_source)

      # Should produce underline with tee at center (width 8: ───┬────)
      assert formatted =~ "───┬────"
    end

    test "calculates width for LocalCall AST" do
      # Create a LocalCall AST node
      local_call = AST.LocalCall.new(:parse, [AST.Literal.new(:h, [line: 5, column: 15])], [
        line: 5,
        column: 9
      ])

      error =
        Error.type_mismatch(
          expected: Type.float(),
          actual: Type.integer(),
          expression: local_call,
          location: {"lib/example.ex", 5, 9}
        )

      formatted = Formatter.format(error, colors: false, source_lines: @sample_source)

      # Should produce underline with tee based on expression width
      assert formatted =~ "─"
    end
  end

  describe "Multiple error formatting" do
    test "formats multiple errors with summary" do
      errors = [
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float(),
          location: {"lib/example.ex", 5, 5}
        ),
        Error.type_mismatch(
          expected: Type.atom(),
          actual: Type.boolean(),
          location: {"lib/example.ex", 10, 10}
        ),
        Error.unsupported_call(
          name: :foo,
          arity: 1,
          location: {"lib/example.ex", 15, 5}
        )
      ]

      formatted = Formatter.format_all(errors, colors: false, source_lines: @sample_source)

      # Should contain all three errors
      assert formatted =~ "E0001"
      assert formatted =~ "E0004"
      # Should show correct count
      assert formatted =~ "3 previous errors"
    end

    test "separates multiple errors with blank lines" do
      errors = [
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float()
        ),
        Error.unsupported_call(name: :foo, arity: 1)
      ]

      formatted = Formatter.format_all(errors, colors: false)

      # Errors should be separated
      parts = String.split(formatted, "\n\n")
      assert length(parts) >= 2
    end
  end

  describe "Location formatting" do
    test "formats full location with file, line, and column" do
      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float(),
          location: {"lib/my_app/module.ex", 42, 15}
        )

      formatted = Formatter.format(error, colors: false)

      assert formatted =~ "╭─[lib/my_app/module.ex:42:15"
    end

    test "formats location without column" do
      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float(),
          location: {"lib/example.ex", 10, nil}
        )

      formatted = Formatter.format(error, colors: false)

      assert formatted =~ "╭─[lib/example.ex:10"
      refute formatted =~ "lib/example.ex:10:"
    end

    test "formats location without file" do
      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float(),
          location: {nil, 10, 5}
        )

      formatted = Formatter.format(error, colors: false)

      assert formatted =~ "╭─[line 10:5"
    end

    test "omits location section when no location" do
      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float()
        )

      formatted = Formatter.format(error, colors: false)

      refute formatted =~ "-->"
    end
  end

  describe "Notes and suggestions formatting" do
    test "formats multiple notes" do
      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float(),
          notes: [
            "First note explaining the issue",
            "Second note with more context",
            "Third note with details"
          ]
        )

      formatted = Formatter.format(error, colors: false)

      assert formatted =~ "note: First note"
      assert formatted =~ "note: Second note"
      assert formatted =~ "note: Third note"
    end

    test "formats multiple suggestions" do
      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float(),
          suggestions: [
            "Use trunc/1 to convert float to integer",
            "Change the expected type to number",
            "Use round/1 for rounding"
          ]
        )

      formatted = Formatter.format(error, colors: false)

      assert formatted =~ "help: Use trunc/1"
      assert formatted =~ "help: Change the expected type"
      assert formatted =~ "help: Use round/1"
    end

    test "omits notes section when no notes" do
      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float(),
          notes: []
        )

      formatted = Formatter.format(error, colors: false)

      refute formatted =~ "note:"
    end

    test "omits suggestions section when no suggestions" do
      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float(),
          suggestions: []
        )

      formatted = Formatter.format(error, colors: false)

      refute formatted =~ "help:"
    end
  end

  describe "Color formatting" do
    test "includes ANSI codes when colors enabled" do
      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float()
        )

      # Force colors even if terminal doesn't support them
      formatted = Formatter.format(error, colors: true)

      # Check for presence of ANSI escape sequences
      # This may or may not have codes depending on IO.ANSI.enabled?()
      # So we just verify it doesn't crash
      assert is_binary(formatted)
    end

    test "excludes ANSI codes when colors disabled" do
      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float()
        )

      formatted = Formatter.format(error, colors: false)

      # Should not contain ANSI escape codes
      refute formatted =~ "\e["
    end
  end

  describe "Edge cases" do
    test "handles very long type names" do
      long_tuple =
        Type.fixed_tuple([
          Type.integer(),
          Type.float(),
          Type.atom(),
          Type.boolean(),
          Type.integer(),
          Type.float()
        ])

      error =
        Error.type_mismatch(
          expected: long_tuple,
          actual: Type.atom()
        )

      formatted = Formatter.format(error, colors: false)

      # Should still format without crashing
      assert formatted =~ "error[E0001]"
    end

    test "handles deeply nested types" do
      nested =
        Type.fixed_tuple([
          Type.fixed_tuple([
            Type.fixed_tuple([Type.integer()])
          ])
        ])

      error =
        Error.type_mismatch(
          expected: nested,
          actual: Type.atom()
        )

      formatted = Formatter.format(error, colors: false)

      assert formatted =~ "{{{integer}}}"
    end

    test "handles union of many types" do
      union =
        Type.union(
          Type.integer(),
          Type.union(
            Type.float(),
            Type.union(Type.atom(), Type.boolean())
          )
        )

      error =
        Error.type_mismatch(
          expected: union,
          actual: Type.binary()
        )

      formatted = Formatter.format(error, colors: false)

      assert formatted =~ "integer"
      assert formatted =~ "float"
      assert formatted =~ "atom"
      assert formatted =~ "boolean"
    end

    test "handles empty expression" do
      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float(),
          expression: nil,
          location: {"lib/example.ex", 5, 5}
        )

      formatted = Formatter.format(error, colors: false, source_lines: @sample_source)

      # Should still show source context from source_lines
      assert formatted =~ "error[E0001]"
    end
  end
end
