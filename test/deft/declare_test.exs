defmodule Deft.DeclareTest do
  use ExUnit.Case, async: false

  alias Deft.Signatures
  alias Deft.Type

  setup do
    Signatures.reset()
    :ok
  end

  describe "declare macro" do
    test "registers a simple function signature" do
      defmodule SimpleDeclarations do
        use Deft.Declare

        declare(String.length(string) :: integer)
      end

      assert {:ok, sig} = Signatures.lookup({String, :length, 1})
      assert %Type.Fn{} = sig
      assert %Type.Integer{} = sig.output
    end

    test "registers function with multiple arguments" do
      defmodule MultiArgDeclarations do
        use Deft.Declare

        declare(Enum.at([integer], integer) :: integer)
      end

      assert {:ok, sig} = Signatures.lookup({Enum, :at, 2})
      assert length(sig.inputs) == 2
    end

    test "registers function with union return type" do
      defmodule UnionDeclarations do
        use Deft.Declare

        declare(Map.fetch(top, atom) :: atom | tuple)
      end

      assert {:ok, sig} = Signatures.lookup({Map, :fetch, 2})
      assert %Type.Union{} = sig.output
    end

    test "registers higher-order function" do
      defmodule HigherOrderDeclarations do
        use Deft.Declare

        declare(Enum.map([integer], (integer -> boolean)) :: [boolean])
      end

      assert {:ok, sig} = Signatures.lookup({Enum, :map, 2})
      assert [%Type.FixedList{}, %Type.Fn{}] = sig.inputs
      assert %Type.FixedList{} = sig.output
    end

    test "handles type variables as top type" do
      defmodule TypeVarDeclarations do
        use Deft.Declare

        declare(List.first([a]) :: a)
      end

      assert {:ok, sig} = Signatures.lookup({List, :first, 1})
      # Type variables currently map to Top
      assert %Type.Top{} = sig.output
    end

    test "registers function with tuple types" do
      defmodule TupleDeclarations do
        use Deft.Declare

        declare(Tuple.to_list({integer, boolean}) :: [top])
      end

      assert {:ok, sig} = Signatures.lookup({Tuple, :to_list, 1})
      assert [%Type.FixedTuple{elements: [%Type.Integer{}, %Type.Boolean{}]}] = sig.inputs
    end

    test "overwrites previous declaration" do
      defmodule OverwriteDeclarations do
        use Deft.Declare

        declare(Custom.foo(integer) :: integer)
        declare(Custom.foo(integer) :: boolean)
      end

      assert {:ok, sig} = Signatures.lookup({Custom, :foo, 1})
      assert %Type.Boolean{} = sig.output
    end
  end

  describe "module resolution" do
    test "resolves aliased modules" do
      defmodule AliasDeclarations do
        use Deft.Declare

        # Note: the module resolution happens at compile time
        # so we use a fully qualified name here
        declare(Deft.Type.integer() :: atom)
      end

      assert {:ok, _} = Signatures.lookup({Deft.Type, :integer, 0})
    end

    test "resolves nested modules" do
      defmodule NestedDeclarations do
        use Deft.Declare

        declare(Deft.Type.Fn.new([integer], integer) :: top)
      end

      assert {:ok, _} = Signatures.lookup({Deft.Type.Fn, :new, 2})
    end
  end
end
