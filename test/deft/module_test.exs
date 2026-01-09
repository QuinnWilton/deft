defmodule Deft.ModuleTest do
  use ExUnit.Case, async: false

  alias Deft.Signatures
  alias Deft.Type

  setup do
    Signatures.reset()
    :ok
  end

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

    test "stores features in module attributes" do
      defmodule UseDeftFeatures do
        use Deft, features: [:strict_subtyping]

        deft dummy(x :: integer) :: integer do
          x
        end
      end

      assert [:strict_subtyping] = UseDeftFeatures.__deft__(:features)
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

    test "registers signature in registry" do
      defmodule DeftRegistersSignature do
        use Deft

        deft multiply(a :: integer, b :: integer) :: integer do
          a * b
        end
      end

      assert {:ok, sig} = Signatures.lookup({DeftRegistersSignature, :multiply, 2})
      assert [%Type.Integer{}, %Type.Integer{}] = sig.inputs
      assert %Type.Integer{} = sig.output
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

      assert {:ok, %Type.Fn{inputs: [%Type.Integer{}], output: %Type.Integer{}}} =
               Signatures.lookup({DeftBasicTypes, :int_fun, 1})

      assert {:ok, %Type.Fn{inputs: [%Type.Float{}], output: %Type.Float{}}} =
               Signatures.lookup({DeftBasicTypes, :float_fun, 1})

      assert {:ok, %Type.Fn{inputs: [%Type.Number{}], output: %Type.Number{}}} =
               Signatures.lookup({DeftBasicTypes, :number_fun, 1})

      assert {:ok, %Type.Fn{inputs: [%Type.Boolean{}], output: %Type.Boolean{}}} =
               Signatures.lookup({DeftBasicTypes, :bool_fun, 1})

      assert {:ok, %Type.Fn{inputs: [%Type.Atom{}], output: %Type.Atom{}}} =
               Signatures.lookup({DeftBasicTypes, :atom_fun, 1})
    end

    test "parses list types" do
      defmodule DeftListTypes do
        use Deft

        deft list_fun(xs :: [integer]) :: [integer] do
          xs
        end
      end

      {:ok, sig} = Signatures.lookup({DeftListTypes, :list_fun, 1})
      assert [%Type.FixedList{contents: %Type.Integer{}}] = sig.inputs
      assert %Type.FixedList{contents: %Type.Integer{}} = sig.output
    end

    test "parses tuple types" do
      defmodule DeftTupleTypes do
        use Deft

        deft pair_fun(p :: {integer, boolean}) :: {integer, boolean} do
          p
        end
      end

      {:ok, sig} = Signatures.lookup({DeftTupleTypes, :pair_fun, 1})
      assert [%Type.FixedTuple{elements: [%Type.Integer{}, %Type.Boolean{}]}] = sig.inputs
    end

    test "parses union types" do
      defmodule DeftUnionTypes do
        use Deft

        deft union_fun(x :: integer | boolean) :: integer | boolean do
          x
        end
      end

      {:ok, sig} = Signatures.lookup({DeftUnionTypes, :union_fun, 1})
      assert [%Type.Union{}] = sig.inputs
      assert %Type.Union{} = sig.output
    end

    test "parses function types" do
      defmodule DeftFunctionTypes do
        use Deft

        deft higher_order(f :: (integer -> boolean)) :: (integer -> boolean) do
          f
        end
      end

      {:ok, sig} = Signatures.lookup({DeftFunctionTypes, :higher_order, 1})
      assert [%Type.Fn{inputs: [%Type.Integer{}], output: %Type.Boolean{}}] = sig.inputs
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
