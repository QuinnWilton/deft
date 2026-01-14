defmodule Deft.Type.TypesTest do
  @moduledoc """
  Unit tests for Type module constructors and Walkable implementations.

  Tests type construction, inspection output, and the Walkable protocol
  for traversing type structures.
  """
  use ExUnit.Case, async: true

  alias Deft.Type
  alias Deft.Walkable

  describe "primitive types" do
    test "integer/0 creates Integer type" do
      result = Type.integer()
      assert %Type.Integer{} = result
    end

    test "float/0 creates Float type" do
      result = Type.float()
      assert %Type.Float{} = result
    end

    test "number/0 creates Number type" do
      result = Type.number()
      assert %Type.Number{} = result
    end

    test "boolean/0 creates Boolean type" do
      result = Type.boolean()
      assert %Type.Boolean{} = result
    end

    test "atom/0 creates Atom type" do
      result = Type.atom()
      assert %Type.Atom{} = result
    end

    test "binary/0 creates Binary type" do
      result = Type.binary()
      assert %Type.Binary{} = result
    end

    test "primitive types have no Walkable children" do
      assert Walkable.children(Type.integer()) == []
      assert Walkable.children(Type.float()) == []
      assert Walkable.children(Type.number()) == []
      assert Walkable.children(Type.boolean()) == []
      assert Walkable.children(Type.atom()) == []
      assert Walkable.children(Type.binary()) == []
    end
  end

  describe "Top and Bottom" do
    test "top/0 creates Top type" do
      result = Type.top()
      assert %Type.Top{} = result
    end

    test "bottom/0 creates Bottom type" do
      result = Type.bottom()
      assert %Type.Bottom{} = result
    end

    test "Top inspects as special symbol" do
      assert inspect(Type.top()) == "\u22A4"
    end

    test "Bottom inspects as special symbol" do
      assert inspect(Type.bottom()) == "\u22A5"
    end
  end

  describe "FixedList" do
    test "fixed_list/1 creates list type with contents" do
      result = Type.fixed_list(Type.integer())

      assert %Type.FixedList{contents: %Type.Integer{}} = result
    end

    test "contents/1 extracts element type" do
      list = Type.fixed_list(Type.boolean())

      assert %Type.Boolean{} = Type.FixedList.contents(list)
    end

    test "Walkable children returns contents" do
      list = Type.fixed_list(Type.integer())

      assert Walkable.children(list) == [Type.integer()]
    end

    test "Walkable rebuild with new contents" do
      list = Type.fixed_list(Type.integer())
      rebuilt = Walkable.rebuild(list, [Type.boolean()])

      assert %Type.FixedList{contents: %Type.Boolean{}} = rebuilt
    end

    test "inspects with brackets" do
      list = Type.fixed_list(Type.integer())
      assert inspect(list) == "[integer]"
    end
  end

  describe "FixedTuple" do
    test "fixed_tuple/1 creates tuple type with elements" do
      result = Type.fixed_tuple([Type.integer(), Type.boolean()])

      assert %Type.FixedTuple{elements: [%Type.Integer{}, %Type.Boolean{}]} = result
    end

    test "elements/1 extracts element types" do
      tuple = Type.fixed_tuple([Type.integer(), Type.boolean()])

      assert [%Type.Integer{}, %Type.Boolean{}] = Type.FixedTuple.elements(tuple)
    end

    test "empty tuple" do
      result = Type.fixed_tuple([])
      assert %Type.FixedTuple{elements: []} = result
    end

    test "Walkable children returns elements" do
      tuple = Type.fixed_tuple([Type.integer(), Type.boolean()])

      assert Walkable.children(tuple) == [[Type.integer(), Type.boolean()]]
    end

    test "inspects with braces" do
      tuple = Type.fixed_tuple([Type.integer(), Type.boolean()])
      assert inspect(tuple) == "{integer, boolean}"
    end
  end

  describe "Union" do
    test "union/2 creates union type" do
      result = Type.union(Type.integer(), Type.boolean())

      assert %Type.Union{fst: %Type.Integer{}, snd: %Type.Boolean{}} = result
    end

    test "Walkable children returns both components" do
      union = Type.union(Type.integer(), Type.boolean())

      assert Walkable.children(union) == [Type.integer(), Type.boolean()]
    end

    test "Walkable rebuild with new components" do
      union = Type.union(Type.integer(), Type.boolean())
      rebuilt = Walkable.rebuild(union, [Type.float(), Type.atom()])

      assert %Type.Union{fst: %Type.Float{}, snd: %Type.Atom{}} = rebuilt
    end

    test "inspects with pipe" do
      union = Type.union(Type.integer(), Type.boolean())
      assert inspect(union) == "integer | boolean"
    end
  end

  describe "Fn (function type)" do
    test "fun/2 creates function type" do
      result = Type.fun([Type.integer(), Type.boolean()], Type.atom())

      assert %Type.Fn{
               inputs: [%Type.Integer{}, %Type.Boolean{}],
               output: %Type.Atom{}
             } = result
    end

    test "nullary function" do
      result = Type.fun([], Type.integer())

      assert %Type.Fn{inputs: [], output: %Type.Integer{}} = result
    end

    test "Walkable children returns inputs and output" do
      fn_type = Type.fun([Type.integer()], Type.boolean())

      assert Walkable.children(fn_type) == [[Type.integer()], Type.boolean()]
    end

    test "inspects with arrow" do
      fn_type = Type.fun([Type.integer()], Type.boolean())
      assert inspect(fn_type) =~ "fn"
      assert inspect(fn_type) =~ "->"
    end
  end

  describe "List (generic)" do
    test "list/0 creates generic list type" do
      result = Type.List.new()
      assert %Type.List{} = result
    end

    test "Walkable children returns empty list" do
      list = Type.List.new()
      assert Walkable.children(list) == []
    end

    test "inspects as 'list'" do
      assert inspect(Type.List.new()) == "list"
    end
  end

  describe "Tuple (generic)" do
    test "tuple/0 creates generic tuple type" do
      result = Type.Tuple.new()
      assert %Type.Tuple{} = result
    end

    test "Walkable children returns empty list" do
      tuple = Type.Tuple.new()
      assert Walkable.children(tuple) == []
    end

    test "inspects as 'tuple'" do
      assert inspect(Type.Tuple.new()) == "tuple"
    end
  end

  describe "Variant (type)" do
    test "new/3 creates variant type" do
      result = Type.Variant.new(:some, :option, [Type.integer()])

      assert result.name == :some
      assert result.adt_name == :option
      assert result.columns == [Type.integer()]
    end

    test "Walkable children returns columns" do
      variant = Type.Variant.new(:some, :option, [Type.integer()])

      assert Walkable.children(variant) == [[Type.integer()]]
    end
  end

  describe "ADT" do
    test "new/3 creates ADT with variants" do
      some = Type.Variant.new(:some, :option, [Type.integer()])
      none = Type.Variant.new(:none, :option, [])

      result = Type.ADT.new(:option, [some, none])

      assert result.name == :option
      assert result.variants == [some, none]
      assert result.params == []
    end

    test "new/3 with type parameters" do
      some = Type.Variant.new(:some, :option, [Type.Var.new(:a)])
      none = Type.Variant.new(:none, :option, [])

      result = Type.ADT.new(:option, [some, none], [:a])

      assert result.params == [:a]
    end

    test "polymorphic?/1 checks for type parameters" do
      monomorphic = Type.ADT.new(:unit, [Type.Variant.new(:unit, :unit, [])])
      polymorphic = Type.ADT.new(:option, [], [:a])

      refute Type.ADT.polymorphic?(monomorphic)
      assert Type.ADT.polymorphic?(polymorphic)
    end
  end

  describe "Alias" do
    test "new/2 creates simple alias" do
      result = Type.Alias.new(:my_type, nil)

      assert result.name == :my_type
      assert result.args == []
    end

    test "new/4 creates parameterized alias" do
      result = Type.Alias.new(:option, nil, [Type.integer()])

      assert result.name == :option
      assert result.args == [Type.integer()]
    end

    test "inspects parameterized alias with args" do
      alias_type = Type.Alias.new(:option, nil, [Type.integer()])
      assert inspect(alias_type) == "option(integer)"
    end
  end

  describe "Unsupported" do
    test "new/4 creates unsupported type marker" do
      result = Type.Unsupported.new(Enum, :group_by, 2, "Returns a map")

      assert result.module == Enum
      assert result.function == :group_by
      assert result.arity == 2
      assert result.reason == "Returns a map"
    end
  end
end
