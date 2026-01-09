defmodule Deft.Rules.DeclarativeTest do
  use ExUnit.Case

  alias Deft.AST
  alias Deft.Context
  alias Deft.Rules.Declarative
  alias Deft.Type

  describe "declarative rules registry" do
    test "includes all rule modules" do
      modules = Declarative.rule_modules()

      assert Declarative.Core in modules
      assert Declarative.Functions in modules
      assert Declarative.ControlFlow in modules
      assert Declarative.Builtins in modules
    end

    test "all_rules returns rules from all modules" do
      rules = Declarative.all_rules()

      assert is_list(rules)
      assert length(rules) > 0
    end

    test "registry returns a valid registry" do
      registry = Declarative.registry()
      assert %Deft.Rule.Registry{} = registry
    end
  end

  describe "Core declarative rules" do
    setup do
      %{ctx: Context.new(__ENV__)}
    end

    test "literal rule synthesizes integer type", %{ctx: ctx} do
      ast = %AST.Literal{value: 42, meta: []}
      rule = Declarative.Core.Rule_literal

      {:ok, erased, type, bindings, _ctx} = rule.apply(ast, nil, ctx)

      assert erased == 42
      assert %Type.Integer{} = type
      assert bindings == []
    end

    test "literal rule synthesizes boolean type", %{ctx: ctx} do
      ast = %AST.Literal{value: true, meta: []}
      rule = Declarative.Core.Rule_literal

      {:ok, erased, type, _bindings, _ctx} = rule.apply(ast, nil, ctx)

      assert erased == true
      assert %Type.Boolean{} = type
    end

    test "literal rule synthesizes float type", %{ctx: ctx} do
      ast = %AST.Literal{value: 3.14, meta: []}
      rule = Declarative.Core.Rule_literal

      {:ok, erased, type, _bindings, _ctx} = rule.apply(ast, nil, ctx)

      assert erased == 3.14
      assert %Type.Float{} = type
    end

    test "literal rule synthesizes atom type", %{ctx: ctx} do
      ast = %AST.Literal{value: :hello, meta: []}
      rule = Declarative.Core.Rule_literal

      {:ok, erased, type, _bindings, _ctx} = rule.apply(ast, nil, ctx)

      assert erased == :hello
      assert %Type.Atom{} = type
    end

    test "local rule synthesizes from metadata", %{ctx: ctx} do
      ast = %AST.Local{
        name: :x,
        meta: [__deft_type__: Type.integer()],
        context: nil
      }

      rule = Declarative.Core.Rule_local

      {:ok, erased, type, bindings, _ctx} = rule.apply(ast, nil, ctx)

      assert erased == {:x, [__deft_type__: Type.integer()], nil}
      assert %Type.Integer{} = type
      assert bindings == []
    end

    test "annotation rule creates binding", %{ctx: ctx} do
      local = %AST.Local{name: :x, meta: [], context: nil}
      ast = %AST.Annotation{pattern: local, type: Type.boolean(), meta: []}

      rule = Declarative.Core.Rule_annotation

      {:ok, erased, type, bindings, _ctx} = rule.apply(ast, nil, ctx)

      assert erased == {:x, [], nil}
      assert %Type.Boolean{} = type
      assert [{^local, %Type.Boolean{}}] = bindings
    end

    test "tuple rule synthesizes fixed tuple type", %{ctx: ctx} do
      ast = %AST.Tuple{
        elements: [
          %AST.Literal{value: 1, meta: []},
          %AST.Literal{value: true, meta: []}
        ],
        meta: []
      }

      rule = Declarative.Core.Rule_tuple

      {:ok, _erased, type, _bindings, _ctx} = rule.apply(ast, nil, ctx)

      assert %Type.FixedTuple{elements: [%Type.Integer{}, %Type.Boolean{}]} = type
    end

    test "pair rule synthesizes 2-tuple type", %{ctx: ctx} do
      ast = %AST.Pair{
        fst: %AST.Literal{value: :ok, meta: []},
        snd: %AST.Literal{value: 42, meta: []}
      }

      rule = Declarative.Core.Rule_pair

      {:ok, erased, type, _bindings, _ctx} = rule.apply(ast, nil, ctx)

      assert {:ok, 42} = erased
      assert %Type.FixedTuple{elements: [%Type.Atom{}, %Type.Integer{}]} = type
    end

    test "list rule synthesizes fixed list type", %{ctx: ctx} do
      ast = %AST.List{
        elements: [
          %AST.Literal{value: 1, meta: []},
          %AST.Literal{value: 2, meta: []},
          %AST.Literal{value: 3, meta: []}
        ]
      }

      rule = Declarative.Core.Rule_list

      {:ok, erased, type, _bindings, _ctx} = rule.apply(ast, nil, ctx)

      assert erased == [1, 2, 3]
      assert %Type.FixedList{contents: %Type.Integer{}} = type
    end
  end

  describe "Function declarative rules" do
    setup do
      %{ctx: Context.new(__ENV__)}
    end

    test "fn rule synthesizes function type", %{ctx: ctx} do
      x = %AST.Local{name: :x, meta: [], context: nil}

      ast = %AST.Fn{
        args: [%AST.Annotation{pattern: x, type: Type.integer(), meta: []}],
        body: %AST.Literal{value: true, meta: []},
        fn_meta: [],
        arrow_meta: []
      }

      rule = Declarative.Functions.Rule_fn

      {:ok, _erased, type, bindings, _ctx} = rule.apply(ast, nil, ctx)

      assert %Type.Fn{inputs: [%Type.Integer{}], output: %Type.Boolean{}} = type
      assert bindings == []
    end
  end

  describe "ControlFlow declarative rules" do
    setup do
      %{ctx: Context.new(__ENV__, features: [:exhaustiveness_checking])}
    end

    test "if rule unions branch types", %{ctx: ctx} do
      ast = %AST.If{
        predicate: %AST.Literal{value: true, meta: []},
        do: %AST.Literal{value: 42, meta: []},
        else: %AST.Literal{value: 3.14, meta: []},
        meta: []
      }

      rule = Declarative.ControlFlow.Rule_if

      {:ok, _erased, type, _bindings, _ctx} = rule.apply(ast, nil, ctx)

      # Should be union of integer and float
      assert %Type.Union{} = type
    end

    test "match rule creates bindings from pattern", %{ctx: ctx} do
      x = %AST.Local{name: :x, meta: [], context: nil}

      ast = %AST.Match{
        pattern: x,
        value: %AST.Literal{value: 42, meta: []},
        meta: []
      }

      rule = Declarative.ControlFlow.Rule_match

      {:ok, _erased, type, bindings, _ctx} = rule.apply(ast, nil, ctx)

      assert %Type.Integer{} = type
      assert [{^x, %Type.Integer{}}] = bindings
    end
  end

  describe "Builtins declarative rules" do
    setup do
      %{ctx: Context.new(__ENV__)}
    end

    test "local_call rule handles arithmetic", %{ctx: ctx} do
      ast = %AST.LocalCall{
        name: :+,
        args: [%AST.Literal{value: 1, meta: []}, %AST.Literal{value: 2, meta: []}],
        meta: []
      }

      rule = Declarative.Builtins.Rule_local_call

      {:ok, _erased, type, _bindings, _ctx} = rule.apply(ast, nil, ctx)

      assert %Type.Integer{} = type
    end

    test "local_call rule handles comparisons", %{ctx: ctx} do
      ast = %AST.LocalCall{
        name: :>,
        args: [%AST.Literal{value: 1, meta: []}, %AST.Literal{value: 2, meta: []}],
        meta: []
      }

      rule = Declarative.Builtins.Rule_local_call

      {:ok, _erased, type, _bindings, _ctx} = rule.apply(ast, nil, ctx)

      assert %Type.Boolean{} = type
    end
  end

  describe "declarative vs imperative parity" do
    test "literal rule produces same results" do
      ctx = Context.new(__ENV__)
      ast = %AST.Literal{value: 42, meta: []}

      {:ok, erased1, type1, bind1, _} = Deft.Rules.Core.Literal.apply(ast, nil, ctx)
      {:ok, erased2, type2, bind2, _} = Declarative.Core.Rule_literal.apply(ast, nil, ctx)

      assert erased1 == erased2
      assert type1 == type2
      assert bind1 == bind2
    end

    test "annotation rule produces same results" do
      ctx = Context.new(__ENV__)
      local = %AST.Local{name: :x, meta: [], context: nil}
      ast = %AST.Annotation{pattern: local, type: Type.integer(), meta: []}

      {:ok, erased1, type1, bind1, _} = Deft.Rules.Core.Annotation.apply(ast, nil, ctx)
      {:ok, erased2, type2, bind2, _} = Declarative.Core.Rule_annotation.apply(ast, nil, ctx)

      assert erased1 == erased2
      assert type1 == type2
      assert bind1 == bind2
    end
  end
end
