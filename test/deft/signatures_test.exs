defmodule Deft.SignaturesTest do
  use ExUnit.Case, async: false

  alias Deft.Signatures
  alias Deft.Type

  setup do
    # Reset signatures before each test
    Signatures.reset()
    :ok
  end

  describe "init/0" do
    test "initializes process-local registry with builtins" do
      # Registry should be initialized from reset
      assert Process.get(:deft_signatures) != nil
    end

    test "builtins are registered after init" do
      assert {:ok, _} = Signatures.lookup({Kernel, :+, 2})
      assert {:ok, _} = Signatures.lookup({Kernel, :length, 1})
    end
  end

  describe "builtins/0" do
    test "returns map of built-in signatures" do
      builtins = Signatures.builtins()

      assert is_map(builtins)
      assert map_size(builtins) > 0
    end

    test "includes arithmetic operators" do
      builtins = Signatures.builtins()

      assert %Type.Fn{} = builtins[{Kernel, :+, 2}]
      assert %Type.Fn{} = builtins[{Kernel, :-, 2}]
      assert %Type.Fn{} = builtins[{Kernel, :*, 2}]
      assert %Type.Fn{} = builtins[{Kernel, :/, 2}]
    end

    test "includes comparison operators" do
      builtins = Signatures.builtins()

      assert %Type.Fn{output: %Type.Boolean{}} = builtins[{Kernel, :==, 2}]
      assert %Type.Fn{output: %Type.Boolean{}} = builtins[{Kernel, :<, 2}]
    end

    test "includes type guards" do
      builtins = Signatures.builtins()

      assert %Type.Fn{output: %Type.Boolean{}} = builtins[{Kernel, :is_integer, 1}]
      assert %Type.Fn{output: %Type.Boolean{}} = builtins[{Kernel, :is_list, 1}]
    end

    test "includes list operations" do
      builtins = Signatures.builtins()

      assert %Type.Fn{output: %Type.Integer{}} = builtins[{Kernel, :length, 1}]
    end
  end

  describe "register/2" do
    test "registers a new signature" do
      sig = Type.fun([Type.integer(), Type.integer()], Type.boolean())

      assert :ok = Signatures.register({MyModule, :my_fun, 2}, sig)
      assert {:ok, ^sig} = Signatures.lookup({MyModule, :my_fun, 2})
    end

    test "overwrites existing signature" do
      sig1 = Type.fun([Type.integer()], Type.integer())
      sig2 = Type.fun([Type.integer()], Type.boolean())

      Signatures.register({MyModule, :overwrite, 1}, sig1)
      Signatures.register({MyModule, :overwrite, 1}, sig2)

      assert {:ok, ^sig2} = Signatures.lookup({MyModule, :overwrite, 1})
    end
  end

  describe "lookup/1" do
    test "returns {:ok, type} for registered signature" do
      assert {:ok, %Type.Fn{}} = Signatures.lookup({Kernel, :+, 2})
    end

    test "returns :error for unregistered signature" do
      assert :error = Signatures.lookup({NonExistent, :foo, 1})
    end
  end

  describe "get/1" do
    test "returns type for registered signature" do
      assert %Type.Fn{} = Signatures.get({Kernel, :+, 2})
    end

    test "returns nil for unregistered signature" do
      assert nil == Signatures.get({NonExistent, :foo, 1})
    end
  end

  describe "registered?/1" do
    test "returns true for registered signature" do
      assert Signatures.registered?({Kernel, :+, 2})
    end

    test "returns false for unregistered signature" do
      refute Signatures.registered?({NonExistent, :foo, 1})
    end
  end

  describe "unregister/1" do
    test "removes a registered signature" do
      sig = Type.fun([Type.integer()], Type.integer())
      Signatures.register({MyModule, :to_remove, 1}, sig)

      assert Signatures.registered?({MyModule, :to_remove, 1})

      Signatures.unregister({MyModule, :to_remove, 1})

      refute Signatures.registered?({MyModule, :to_remove, 1})
    end
  end

  describe "all/0" do
    test "returns all registered signatures" do
      sigs = Signatures.all()

      assert is_list(sigs)
      assert length(sigs) > 0
      assert Enum.all?(sigs, fn {mfa, type} -> is_tuple(mfa) and is_struct(type) end)
    end
  end

  describe "reset/0" do
    test "clears custom signatures and reloads builtins" do
      sig = Type.fun([Type.integer()], Type.integer())
      Signatures.register({MyModule, :custom, 1}, sig)

      Signatures.reset()

      refute Signatures.registered?({MyModule, :custom, 1})
      assert Signatures.registered?({Kernel, :+, 2})
    end
  end

  describe "signature types" do
    test "arithmetic operators have number inputs and outputs" do
      {:ok, plus} = Signatures.lookup({Kernel, :+, 2})

      assert [%Type.Number{}, %Type.Number{}] = plus.inputs
      assert %Type.Number{} = plus.output
    end

    test "division returns float" do
      {:ok, div} = Signatures.lookup({Kernel, :/, 2})

      assert %Type.Float{} = div.output
    end

    test "integer division returns integer" do
      {:ok, div} = Signatures.lookup({Kernel, :div, 2})

      assert %Type.Integer{} = div.output
    end

    test "rounding functions return integer" do
      {:ok, ceil} = Signatures.lookup({Kernel, :ceil, 1})
      {:ok, floor} = Signatures.lookup({Kernel, :floor, 1})
      {:ok, round} = Signatures.lookup({Kernel, :round, 1})
      {:ok, trunc} = Signatures.lookup({Kernel, :trunc, 1})

      assert %Type.Integer{} = ceil.output
      assert %Type.Integer{} = floor.output
      assert %Type.Integer{} = round.output
      assert %Type.Integer{} = trunc.output
    end
  end
end
