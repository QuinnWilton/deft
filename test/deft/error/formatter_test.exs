defmodule Deft.Error.FormatterTest do
  use ExUnit.Case, async: true

  alias Deft.Error
  alias Deft.Error.Formatter
  alias Deft.Type

  describe "format/2" do
    test "formats error with header" do
      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float()
        )

      formatted = Formatter.format(error, colors: false)

      assert formatted =~ "error[E0001]"
      assert formatted =~ "Type mismatch"
    end

    test "formats error with location" do
      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float(),
          location: {"lib/my_app.ex", 15, 10}
        )

      formatted = Formatter.format(error, colors: false)

      assert formatted =~ "--> lib/my_app.ex:15:10"
    end

    test "formats error with notes" do
      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float(),
          notes: ["This is a note", "Another note"]
        )

      formatted = Formatter.format(error, colors: false)

      assert formatted =~ "= note: This is a note"
      assert formatted =~ "= note: Another note"
    end

    test "formats error with suggestions" do
      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float(),
          suggestions: ["Use trunc/1", "Change type"]
        )

      formatted = Formatter.format(error, colors: false)

      assert formatted =~ "= help: Use trunc/1"
      assert formatted =~ "= help: Change type"
    end

    test "formats error with expression context" do
      expr = quote do: x + 1.5

      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float(),
          location: {nil, 15, 10},
          expression: expr
        )

      formatted = Formatter.format(error, colors: false)

      assert formatted =~ "in:"
    end

    test "respects colors: false option" do
      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float()
        )

      formatted = Formatter.format(error, colors: false)

      # Should not contain ANSI codes
      refute formatted =~ "\e["
    end

    test "shows surrounding context lines for multi-span errors" do
      # Create source with multiple lines
      source_lines = [
        "defmodule Foo do",
        "  def bar(x) do",
        "    x + 1",
        "  end",
        "end"
      ]

      # Error with spans on different lines
      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float(),
          location: {"test.ex", 3, 5},
          spans: [
            %{location: {"test.ex", 3, 5}, label: "error here", type: nil, kind: :primary}
          ]
        )

      # With context_lines: 2, should show lines 1-5 (2 before line 3, 2 after)
      formatted = Formatter.format(error, colors: false, source_lines: source_lines, context_lines: 2)

      # Should contain the surrounding context lines
      assert formatted =~ "defmodule Foo do"
      assert formatted =~ "def bar(x) do"
      assert formatted =~ "x + 1"
      assert formatted =~ "end"
    end

    test "context_lines: 0 shows only the span line" do
      source_lines = [
        "line 1",
        "line 2",
        "line 3",
        "line 4",
        "line 5"
      ]

      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float(),
          location: {"test.ex", 3, 1},
          spans: [
            %{location: {"test.ex", 3, 1}, label: "error", type: nil, kind: :primary}
          ]
        )

      formatted = Formatter.format(error, colors: false, source_lines: source_lines, context_lines: 0)

      # Should contain only line 3
      assert formatted =~ "line 3"
      refute formatted =~ "line 1"
      refute formatted =~ "line 2"
      refute formatted =~ "line 4"
      refute formatted =~ "line 5"
    end
  end

  describe "format_all/2" do
    test "formats multiple errors" do
      errors = [
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float()
        ),
        Error.unsupported_call(name: :foo, arity: 2)
      ]

      formatted = Formatter.format_all(errors, colors: false)

      assert formatted =~ "E0001"
      assert formatted =~ "E0004"
      assert formatted =~ "2 previous errors"
    end

    test "shows singular 'error' for single error" do
      errors = [
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float()
        )
      ]

      formatted = Formatter.format_all(errors, colors: false)

      assert formatted =~ "1 previous error"
      refute formatted =~ "errors"
    end

    test "handles empty error list" do
      formatted = Formatter.format_all([], colors: false)

      assert formatted == ""
    end
  end
end
