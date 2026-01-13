defmodule Deft.ModuleTest do
  use ExUnit.Case, async: true

  alias Deft.Type

  describe "use Deft" do
    test "imports compile macro" do
      defmodule UseDeftImports do
        use Deft

        def test do
          compile do
            42
          end
        end
      end

      assert {42, %Type.Integer{}} = UseDeftImports.test()
    end

    test "registers @before_compile hook" do
      defmodule UseDeftHook do
        use Deft

        deft add(a :: integer, b :: integer) :: integer do
          a + b
        end
      end

      # Module should have __deft__ function
      assert function_exported?(UseDeftHook, :__deft__, 1)
    end

    test "stores type system in module attributes" do
      defmodule UseDeftTypeSystem do
        use Deft, type_system: Deft.TypeSystem.Default

        deft dummy(x :: integer) :: integer do
          x
        end
      end

      assert Deft.TypeSystem.Default = UseDeftTypeSystem.__deft__(:type_system)
    end
  end

  describe "deft macro" do
    test "defines a function with the given name and arity" do
      defmodule DeftDefinesFunction do
        use Deft

        deft add(a :: integer, b :: integer) :: integer do
          a + b
        end
      end

      assert function_exported?(DeftDefinesFunction, :add, 2)
      assert 3 = DeftDefinesFunction.add(1, 2)
    end

    test "function type checks correctly" do
      defmodule DeftRegistersSignature do
        use Deft

        deft multiply(a :: integer, b :: integer) :: integer do
          a * b
        end
      end

      # The function should work correctly (type checking passed at compile time)
      assert 6 = DeftRegistersSignature.multiply(2, 3)
    end

    test "handles zero-arity functions" do
      defmodule DeftZeroArity do
        use Deft

        deft forty_two() :: integer do
          42
        end
      end

      assert function_exported?(DeftZeroArity, :forty_two, 0)
      assert 42 = DeftZeroArity.forty_two()
    end

    test "handles single argument functions" do
      defmodule DeftSingleArg do
        use Deft

        deft negate(x :: integer) :: integer do
          -x
        end
      end

      assert -5 = DeftSingleArg.negate(5)
    end

    test "handles multiple functions in same module" do
      defmodule DeftMultipleFunctions do
        use Deft

        deft add(a :: integer, b :: integer) :: integer do
          a + b
        end

        deft subtract(a :: integer, b :: integer) :: integer do
          a - b
        end
      end

      assert 5 = DeftMultipleFunctions.add(2, 3)
      assert 1 = DeftMultipleFunctions.subtract(3, 2)
    end

    test "stores signature info in __deft__(:signatures)" do
      defmodule DeftStoresSignatures do
        use Deft

        deft foo(x :: integer) :: boolean do
          x > 0
        end
      end

      sigs = DeftStoresSignatures.__deft__(:signatures)
      assert [{:foo, 1, _, _}] = sigs
    end
  end

  describe "type parsing in deft" do
    test "parses basic types" do
      defmodule DeftBasicTypes do
        use Deft

        deft int_fun(x :: integer) :: integer do
          x
        end

        deft float_fun(x :: float) :: float do
          x
        end

        deft number_fun(x :: number) :: number do
          x
        end

        deft bool_fun(x :: boolean) :: boolean do
          x
        end

        deft atom_fun(x :: atom) :: atom do
          x
        end
      end

      # Functions compile and work correctly (type checking passed)
      assert 42 = DeftBasicTypes.int_fun(42)
      assert 3.14 = DeftBasicTypes.float_fun(3.14)
      assert 123 = DeftBasicTypes.number_fun(123)
      assert true = DeftBasicTypes.bool_fun(true)
      assert :hello = DeftBasicTypes.atom_fun(:hello)
    end

    test "parses list types" do
      defmodule DeftListTypes do
        use Deft

        deft list_fun(xs :: [integer]) :: [integer] do
          xs
        end
      end

      # Function compiles and works correctly
      assert [1, 2, 3] = DeftListTypes.list_fun([1, 2, 3])
    end

    test "parses tuple types" do
      defmodule DeftTupleTypes do
        use Deft

        deft pair_fun(p :: {integer, boolean}) :: {integer, boolean} do
          p
        end
      end

      # Function compiles and works correctly
      assert {42, true} = DeftTupleTypes.pair_fun({42, true})
    end

    test "parses union types" do
      defmodule DeftUnionTypes do
        use Deft

        deft union_fun(x :: integer | boolean) :: integer | boolean do
          x
        end
      end

      # Function compiles and works correctly
      assert 42 = DeftUnionTypes.union_fun(42)
      assert true = DeftUnionTypes.union_fun(true)
    end

    test "parses function types" do
      defmodule DeftFunctionTypes do
        use Deft

        deft higher_order(f :: (integer -> boolean)) :: (integer -> boolean) do
          f
        end
      end

      is_positive = fn x -> x > 0 end
      # Function compiles and works correctly
      returned_fn = DeftFunctionTypes.higher_order(is_positive)
      assert is_function(returned_fn, 1)
    end
  end

  describe "cross-module calls" do
    test "can call deft function from another deft module" do
      # Define the callee module first
      defmodule DeftMathHelper do
        use Deft

        deft double(x :: integer) :: integer do
          x * 2
        end
      end

      # Define a caller module that uses the helper
      defmodule DeftMathCaller do
        use Deft

        deft quadruple(x :: integer) :: integer do
          DeftMathHelper.double(DeftMathHelper.double(x))
        end
      end

      # Verify the call works at runtime
      assert 8 = DeftMathCaller.quadruple(2)
      assert 20 = DeftMathCaller.quadruple(5)
    end

    test "type checks cross-module calls" do
      # Define a module with a function that returns boolean
      defmodule DeftPredicates do
        use Deft

        deft is_positive(x :: integer) :: boolean do
          x > 0
        end
      end

      # Define a caller that uses the result correctly
      defmodule DeftPredicateCaller do
        use Deft

        deft check_positive(x :: integer) :: boolean do
          DeftPredicates.is_positive(x)
        end
      end

      assert true == DeftPredicateCaller.check_positive(5)
      assert false == DeftPredicateCaller.check_positive(-1)
    end
  end

  describe "module-level signatures" do
    test "can include signature modules scoped to the module" do
      # Define a signature module for an "external" library
      defmodule ExternalLibSignatures do
        use Deft.Signatures.DSL, for: Deft.ModuleTest.ExternalLib

        sig(double(integer) :: integer)
        sig(stringify(integer) :: atom)
      end

      # Define the "external" library (normal Elixir module)
      defmodule ExternalLib do
        def double(x), do: x * 2
        def stringify(x), do: String.to_atom("num_#{x}")
      end

      # Define a deft module that uses the external library
      defmodule UsesExternalLib do
        use Deft

        # Include signatures for the external library
        signatures(Deft.ModuleTest.ExternalLibSignatures)

        deft process(x :: integer) :: integer do
          ExternalLib.double(x)
        end

        deft convert(x :: integer) :: atom do
          ExternalLib.stringify(x)
        end
      end

      # Verify runtime behavior
      assert 10 == UsesExternalLib.process(5)
      assert :num_42 == UsesExternalLib.convert(42)
    end

    test "module signatures take precedence over type system signatures" do
      # Define a signature module that overrides a Kernel function
      defmodule OverrideSignatures do
        use Deft.Signatures.DSL, for: Kernel

        # Override + to only accept integers (not numbers)
        sig(:+, [integer, integer] :: integer)
      end

      # This module uses the overridden signature
      defmodule UsesOverride do
        use Deft

        signatures(Deft.ModuleTest.OverrideSignatures)

        deft add_ints(a :: integer, b :: integer) :: integer do
          a + b
        end
      end

      assert 5 == UsesOverride.add_ints(2, 3)
    end
  end

  describe "error handling" do
    test "raises on invalid deft syntax" do
      assert_raise ArgumentError, ~r/Invalid deft syntax/, fn ->
        Code.compile_quoted(
          quote do
            defmodule InvalidDeftSyntax do
              use Deft

              deft invalid_fun(x) do
                x
              end
            end
          end
        )
      end
    end
  end
end
