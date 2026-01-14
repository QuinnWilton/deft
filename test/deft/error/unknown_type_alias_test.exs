defmodule Deft.Error.UnknownTypeAliasTest do
  use ExUnit.Case, async: true

  alias Deft.Error

  describe "unknown_type_alias/1" do
    test "creates error with correct code and message" do
      error = Error.unknown_type_alias(name: :foo)

      assert error.code == :unknown_type_alias
      assert error.message == "Unknown type `foo`"
    end

    test "includes location when provided" do
      error =
        Error.unknown_type_alias(
          name: :foo,
          location: {"test.ex", 10, 5}
        )

      assert error.location == {"test.ex", 10, 5}
    end

    test "includes default note about type not being defined" do
      error = Error.unknown_type_alias(name: :foo)

      assert "The type `foo` is not defined in this scope." in error.notes
    end

    test "suggests similar type name when provided" do
      error =
        Error.unknown_type_alias(
          name: :optin,
          similar: :option
        )

      assert "Did you mean `option`?" in error.suggestions
    end

    test "lists available types when no similar match but available provided" do
      error =
        Error.unknown_type_alias(
          name: :foo,
          available: [:option, :result, :either]
        )

      assert Enum.any?(error.suggestions, &String.contains?(&1, "option"))
      assert Enum.any?(error.suggestions, &String.contains?(&1, "result"))
    end

    test "provides default suggestion when no similar or available" do
      error = Error.unknown_type_alias(name: :foo)

      assert "Define the type with `defdata` or check for typos." in error.suggestions
    end

    test "prefers similar over available when both provided" do
      error =
        Error.unknown_type_alias(
          name: :optin,
          similar: :option,
          available: [:option, :result]
        )

      # Should suggest the similar name, not list available
      assert "Did you mean `option`?" in error.suggestions
      refute Enum.any?(error.suggestions, &String.contains?(&1, "Available types"))
    end

    test "allows custom notes to override default" do
      error =
        Error.unknown_type_alias(
          name: :foo,
          notes: ["Custom note about foo."]
        )

      assert "Custom note about foo." in error.notes
      refute "The type `foo` is not defined in this scope." in error.notes
    end
  end
end
