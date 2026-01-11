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
