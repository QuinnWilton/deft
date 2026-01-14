defmodule Deft.TypeAliasResolutionTest do
  use ExUnit.Case, async: true

  alias Deft.AST.Utils, as: ASTUtils
  alias Deft.Context
  alias Deft.Error
  alias Deft.Type

  describe "unknown_type_alias error construction" do
    test "includes type name in message" do
      error = Error.unknown_type_alias(name: :foo)
      assert error.message == "Unknown type `foo`"
    end

    test "includes location when provided" do
      error =
        Error.unknown_type_alias(
          name: :bar,
          location: {"lib/test.ex", 10, 5}
        )

      assert error.location == {"lib/test.ex", 10, 5}
    end

    test "suggests similar type when available" do
      error =
        Error.unknown_type_alias(
          name: :optin,
          similar: :option
        )

      assert "Did you mean `option`?" in error.suggestions
    end

    test "lists available types when no similar match" do
      error =
        Error.unknown_type_alias(
          name: :xyz,
          available: [:option, :result]
        )

      suggestion = Enum.find(error.suggestions, &String.contains?(&1, "Available types"))
      assert suggestion
      assert String.contains?(suggestion, "option")
    end
  end

  describe "find_similar integration" do
    test "finds similar ADT names" do
      available = [:option, :result, :either, :maybe]

      assert :option = ASTUtils.find_similar(:optin, available)
      assert :result = ASTUtils.find_similar(:resutl, available)
      assert nil == ASTUtils.find_similar(:xyz, available)
    end
  end

  describe "Type.Alias location preservation" do
    test "Type.Alias stores location" do
      alias_type = Type.Alias.new(:foo, nil, [], {"lib/test.ex", 10, 5})

      assert alias_type.name == :foo
      assert alias_type.location == {"lib/test.ex", 10, 5}
    end

    test "Type.Alias works without location" do
      alias_type = Type.Alias.new(:foo, nil)

      assert alias_type.name == :foo
      assert alias_type.location == nil
    end
  end

  describe "Context.available_adt_names" do
    test "returns empty list for empty context" do
      ctx = Context.new(%Macro.Env{})
      assert [] = Context.available_adt_names(ctx)
    end

    test "returns local ADT names" do
      ctx = Context.new(%Macro.Env{})

      # Simulate adding an ADT
      adt_type = %Type.ADT{
        name: Deft.AST.Local.new(:option, nil),
        variants: []
      }

      ctx = Context.bind_adt(ctx, :option, adt_type)

      names = Context.available_adt_names(ctx)
      assert :option in names
    end
  end

  describe "end-to-end error flow" do
    test "error has all expected fields" do
      # Simulate what happens when pattern matching fails to resolve an alias
      alias_type = Type.Alias.new(:unknown_type, nil, [], {"lib/test.ex", 15, 3})
      available = [:option, :result]
      similar = ASTUtils.find_similar(:unknown_type, available)

      error =
        Error.unknown_type_alias(
          name: :unknown_type,
          location: alias_type.location,
          similar: similar,
          available: available
        )

      assert error.code == :unknown_type_alias
      assert error.message == "Unknown type `unknown_type`"
      assert error.location == {"lib/test.ex", 15, 3}
      assert "The type `unknown_type` is not defined in this scope." in error.notes
    end
  end
end
