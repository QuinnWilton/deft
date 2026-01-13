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

  # ============================================================================
  # Unsupported Function Signatures (Type.Unsupported)
  # ============================================================================

  describe "sig_unsupported DSL macro" do
    defmodule TestUnsupportedSignatures do
      use Deft.Signatures.DSL, for: TestModule

      # Standard function syntax
      sig_unsupported(group_by([integer], (integer -> boolean)) :: top,
        reason: "Returns a map type which Deft cannot represent"
      )

      # Zero-arg function
      sig_unsupported(fetch_all() :: top,
        reason: "Returns value | nil but Deft has no nil type"
      )

      # Multi-arg function
      sig_unsupported(into([integer], top, (integer -> top)) :: top,
        reason: "Target collection type cannot be determined statically"
      )
    end

    test "creates Type.Unsupported struct with correct fields" do
      sigs = TestUnsupportedSignatures.signatures()

      unsupported = sigs[{TestModule, :group_by, 2}]
      assert %Type.Unsupported{} = unsupported
      assert unsupported.module == TestModule
      assert unsupported.function == :group_by
      assert unsupported.arity == 2
      assert unsupported.reason == "Returns a map type which Deft cannot represent"
    end

    test "handles zero-arg functions" do
      sigs = TestUnsupportedSignatures.signatures()

      unsupported = sigs[{TestModule, :fetch_all, 0}]
      assert %Type.Unsupported{} = unsupported
      assert unsupported.arity == 0
    end

    test "handles multi-arg functions" do
      sigs = TestUnsupportedSignatures.signatures()

      unsupported = sigs[{TestModule, :into, 3}]
      assert %Type.Unsupported{} = unsupported
      assert unsupported.arity == 3
    end
  end

  describe "sig_unsupported with operator syntax" do
    defmodule TestUnsupportedOperators do
      use Deft.Signatures.DSL, for: TestOperatorModule

      sig_unsupported(:"~~~", [integer, integer] :: top, reason: "Custom operator not supported")
    end

    test "handles operator syntax" do
      sigs = TestUnsupportedOperators.signatures()

      unsupported = sigs[{TestOperatorModule, :"~~~", 2}]
      assert %Type.Unsupported{} = unsupported
      assert unsupported.function == :"~~~"
      assert unsupported.reason == "Custom operator not supported"
    end
  end

  describe "unsupported function type checking" do
    defmodule TestUnsupportedForTypeChecking do
      use Deft.Signatures.DSL, for: UnsupportedTestModule

      sig_unsupported(bad_function(integer) :: top,
        reason: "This function returns a map which cannot be typed"
      )
    end

    test "raises CompileError with reason when calling unsupported function" do
      alias Deft.TypeChecker
      alias Deft.AST

      sigs = TestUnsupportedForTypeChecking.signatures()
      ctx = Context.new(__ENV__) |> Context.with_signatures(sigs)

      # Build AST for UnsupportedTestModule.bad_function(42)
      ast = %AST.RemoteCall{
        module: UnsupportedTestModule,
        function: :bad_function,
        args: [AST.Literal.new(42)],
        meta: []
      }

      error =
        assert_raise CompileError, fn ->
          TypeChecker.check(ast, ctx)
        end

      # Check that the error includes the reason
      assert error.description =~ "E0012"
      assert error.description =~ "UnsupportedTestModule.bad_function/1"
      assert error.description =~ "cannot be typed"
      assert error.description =~ "returns a map which cannot be typed"
    end

    test "raises CompileError when capturing unsupported function" do
      alias Deft.TypeChecker
      alias Deft.AST

      sigs = TestUnsupportedForTypeChecking.signatures()
      ctx = Context.new(__ENV__) |> Context.with_signatures(sigs)

      # Build AST for &UnsupportedTestModule.bad_function/1
      ast = %AST.Capture{
        module: UnsupportedTestModule,
        function: :bad_function,
        arity: 1,
        meta: []
      }

      error =
        assert_raise CompileError, fn ->
          TypeChecker.check(ast, ctx)
        end

      assert error.description =~ "E0012"
      assert error.description =~ "returns a map which cannot be typed"
    end
  end

  describe "mixing sig and sig_unsupported in same module" do
    defmodule MixedSignatures do
      use Deft.Signatures.DSL, for: MixedModule

      # Supported function
      sig good_function(integer) :: integer

      # Unsupported function
      sig_unsupported(bad_function(integer) :: top,
        reason: "Cannot be typed"
      )
    end

    test "both signature types are present" do
      sigs = MixedSignatures.signatures()

      assert %Type.Fn{} = sigs[{MixedModule, :good_function, 1}]
      assert %Type.Unsupported{} = sigs[{MixedModule, :bad_function, 1}]
    end
  end
end
