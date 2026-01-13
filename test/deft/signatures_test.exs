defmodule Deft.SignaturesTest do
  use ExUnit.Case, async: true

  alias Deft.Context
  alias Deft.Signatures
  alias Deft.Type

  describe "TypeSystem.Default signatures" do
    test "all_signatures returns map of built-in signatures" do
      sigs = Deft.TypeSystem.Default.all_signatures()

      assert is_map(sigs)
      assert map_size(sigs) > 0
    end

    test "includes arithmetic operators" do
      sigs = Deft.TypeSystem.Default.all_signatures()

      assert %Type.Fn{} = sigs[{Kernel, :+, 2}]
      assert %Type.Fn{} = sigs[{Kernel, :-, 2}]
      assert %Type.Fn{} = sigs[{Kernel, :*, 2}]
      assert %Type.Fn{} = sigs[{Kernel, :/, 2}]
    end

    test "includes comparison operators" do
      sigs = Deft.TypeSystem.Default.all_signatures()

      assert %Type.Fn{output: %Type.Boolean{}} = sigs[{Kernel, :==, 2}]
      assert %Type.Fn{output: %Type.Boolean{}} = sigs[{Kernel, :<, 2}]
    end

    test "includes type guards" do
      sigs = Deft.TypeSystem.Default.all_signatures()

      assert %Type.Fn{output: %Type.Boolean{}} = sigs[{Kernel, :is_integer, 1}]
      assert %Type.Fn{output: %Type.Boolean{}} = sigs[{Kernel, :is_list, 1}]
    end

    test "includes list operations" do
      sigs = Deft.TypeSystem.Default.all_signatures()

      # length is polymorphic: forall(a). fn ([a]) -> integer
      assert %Type.Forall{body: %Type.Fn{output: %Type.Integer{}}} = sigs[{Kernel, :length, 1}]
    end
  end

  describe "Context.lookup_signature/2" do
    test "returns {:ok, type} for registered signature" do
      sigs = Deft.TypeSystem.Default.all_signatures()
      ctx = Context.new(nil) |> Context.with_signatures(sigs)

      assert {:ok, %Type.Fn{}} = Context.lookup_signature(ctx, {Kernel, :+, 2})
    end

    test "returns :error for unregistered signature" do
      sigs = Deft.TypeSystem.Default.all_signatures()
      ctx = Context.new(nil) |> Context.with_signatures(sigs)

      assert :error = Context.lookup_signature(ctx, {NonExistent, :foo, 1})
    end
  end

  describe "Signatures.with_signatures/2" do
    test "creates context with given signatures" do
      custom_sig = Type.fun([Type.integer()], Type.boolean())
      custom_sigs = %{{CustomModule, :custom_fn, 1} => custom_sig}

      Signatures.with_signatures(custom_sigs, fn ctx ->
        assert {:ok, ^custom_sig} = Context.lookup_signature(ctx, {CustomModule, :custom_fn, 1})
        assert :error = Context.lookup_signature(ctx, {Kernel, :+, 2})
      end)
    end
  end

  describe "Signatures.with_default_signatures/1" do
    test "creates context with default type system signatures" do
      Signatures.with_default_signatures(fn ctx ->
        assert {:ok, %Type.Fn{}} = Context.lookup_signature(ctx, {Kernel, :+, 2})
        assert {:ok, %Type.Fn{}} = Context.lookup_signature(ctx, {Kernel, :is_integer, 1})
      end)
    end
  end

  describe "Signatures.merge/1" do
    test "merges multiple signature maps" do
      sigs1 = %{{Mod1, :f, 1} => Type.fun([Type.integer()], Type.integer())}
      sigs2 = %{{Mod2, :g, 1} => Type.fun([Type.boolean()], Type.boolean())}

      merged = Signatures.merge([sigs1, sigs2])

      assert Map.has_key?(merged, {Mod1, :f, 1})
      assert Map.has_key?(merged, {Mod2, :g, 1})
    end

    test "later maps override earlier ones" do
      sig1 = Type.fun([Type.integer()], Type.integer())
      sig2 = Type.fun([Type.boolean()], Type.boolean())

      sigs1 = %{{Mod, :f, 1} => sig1}
      sigs2 = %{{Mod, :f, 1} => sig2}

      merged = Signatures.merge([sigs1, sigs2])

      assert merged[{Mod, :f, 1}] == sig2
    end
  end

  describe "Signatures.build_fn/2" do
    test "builds a function type" do
      sig = Signatures.build_fn([Type.integer(), Type.integer()], Type.boolean())

      assert %Type.Fn{} = sig
      assert [%Type.Integer{}, %Type.Integer{}] = sig.inputs
      assert %Type.Boolean{} = sig.output
    end
  end

  describe "Signatures.build_forall/3" do
    test "builds a polymorphic function type" do
      sig = Signatures.build_forall([:a], [Type.fixed_list(Type.var(:a))], Type.var(:a))

      assert %Type.Forall{vars: [:a]} = sig
      assert %Type.Fn{} = sig.body
    end
  end

  describe "signature types from Deft.Signatures.Kernel" do
    test "arithmetic operators have number inputs and outputs" do
      sigs = Deft.Signatures.Kernel.signatures()
      plus = sigs[{Kernel, :+, 2}]

      assert [%Type.Number{}, %Type.Number{}] = plus.inputs
      assert %Type.Number{} = plus.output
    end

    test "division returns float" do
      sigs = Deft.Signatures.Kernel.signatures()
      div = sigs[{Kernel, :/, 2}]

      assert %Type.Float{} = div.output
    end

    test "integer division returns integer" do
      sigs = Deft.Signatures.Kernel.signatures()
      div = sigs[{Kernel, :div, 2}]

      assert %Type.Integer{} = div.output
    end

    test "rounding functions return integer" do
      sigs = Deft.Signatures.Kernel.signatures()

      assert %Type.Integer{} = sigs[{Kernel, :ceil, 1}].output
      assert %Type.Integer{} = sigs[{Kernel, :floor, 1}].output
      assert %Type.Integer{} = sigs[{Kernel, :round, 1}].output
      assert %Type.Integer{} = sigs[{Kernel, :trunc, 1}].output
    end

    test "polymorphic list operations" do
      sigs = Deft.Signatures.Kernel.signatures()

      hd_sig = sigs[{Kernel, :hd, 1}]
      assert %Type.Forall{vars: [:a]} = hd_sig

      concat_sig = sigs[{Kernel, :++, 2}]
      assert %Type.Forall{vars: [:a, :b]} = concat_sig
    end
  end
end
