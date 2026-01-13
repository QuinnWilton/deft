defmodule Deft.Signatures do
  @moduledoc """
  Utilities for working with function type signatures.

  Signatures are now scoped to type systems rather than stored in a global
  registry. Use `Deft.Context.lookup_signature/2` to look up signatures
  during type checking.

  ## Defining Signatures

  Use `Deft.Signatures.DSL` to define signature modules:

      defmodule MyApp.Signatures.MyModule do
        use Deft.Signatures.DSL, for: MyModule

        sig my_function(integer) :: boolean
        sig polymorphic_fn([a]) :: a
      end

  ## Including Signatures in Type Systems

  Include signature modules in your type system:

      defmodule MyApp.TypeSystem do
        use Deft.TypeSystem

        include Deft.Rules.Core
        # ... other rules ...

        signatures Deft.Signatures.Kernel
        signatures MyApp.Signatures.MyModule

        features [:exhaustiveness_checking]
      end

  ## Test Helpers

  For tests, use `with_signatures/2` to temporarily scope signatures:

      Deft.Signatures.with_signatures(signatures, fn ctx ->
        # Type checking uses these signatures
      end)
  """

  alias Deft.Context
  alias Deft.Type

  @doc """
  Creates a context with the given signatures and invokes the function.

  This is primarily useful for testing, allowing you to define ad-hoc
  signatures without creating full signature modules or type systems.

  ## Example

      signatures = %{
        {MyModule, :foo, 1} => Type.fun([Type.integer()], Type.boolean())
      }

      Deft.Signatures.with_signatures(signatures, fn ctx ->
        # ctx has these signatures available
        {:ok, _, type, _, _} = Deft.TypeChecker.check(ast, ctx)
      end)

  You can also merge with default signatures:

      default_sigs = Deft.TypeSystem.Default.all_signatures()
      custom_sigs = Map.merge(default_sigs, %{...})

      Deft.Signatures.with_signatures(custom_sigs, fn ctx ->
        # Has both default and custom signatures
      end)
  """
  @spec with_signatures(map(), (Context.t() -> result)) :: result when result: any()
  def with_signatures(signatures, fun) when is_map(signatures) and is_function(fun, 1) do
    ctx =
      Context.new(nil)
      |> Context.with_signatures(signatures)

    fun.(ctx)
  end

  @doc """
  Creates a context with default signatures and invokes the function.

  This is a convenience for tests that want to use the default type
  system's signatures.

  ## Example

      Deft.Signatures.with_default_signatures(fn ctx ->
        {:ok, _, type, _, _} = Deft.TypeChecker.check(ast, ctx)
      end)
  """
  @spec with_default_signatures((Context.t() -> result)) :: result when result: any()
  def with_default_signatures(fun) when is_function(fun, 1) do
    signatures = Deft.TypeSystem.Default.all_signatures()
    with_signatures(signatures, fun)
  end

  @doc """
  Merges multiple signature maps together.

  Later signatures override earlier ones for the same MFA.

  ## Example

      merged = Deft.Signatures.merge([
        Deft.Signatures.Kernel.signatures(),
        my_custom_signatures
      ])
  """
  @spec merge([map()]) :: map()
  def merge(signature_maps) when is_list(signature_maps) do
    Enum.reduce(signature_maps, %{}, &Map.merge(&2, &1))
  end

  @doc """
  Builds a signature type from components.

  This is a convenience function for building signature types in tests.

  ## Examples

      # Monomorphic function
      sig = Deft.Signatures.build_fn([Type.integer()], Type.boolean())

      # Polymorphic function
      sig = Deft.Signatures.build_forall([:a], [Type.fixed_list(Type.var(:a))], Type.var(:a))
  """
  @spec build_fn([Type.t()], Type.t()) :: Type.Fn.t()
  def build_fn(inputs, output) when is_list(inputs) do
    Type.fun(inputs, output)
  end

  @doc """
  Builds a polymorphic signature type.

  ## Example

      sig = Deft.Signatures.build_forall([:a], [Type.fixed_list(Type.var(:a))], Type.var(:a))
  """
  @spec build_forall([atom()], [Type.t()], Type.t()) :: Type.Forall.t()
  def build_forall(vars, inputs, output) when is_list(vars) and is_list(inputs) do
    Type.forall(vars, Type.fun(inputs, output))
  end
end
