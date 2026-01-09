defmodule Deft.RulesTest do
  use ExUnit.Case

  alias Deft.AST
  alias Deft.Context
  alias Deft.Rules
  alias Deft.Type

  describe "rules registry" do
    test "includes all rule modules" do
      modules = Rules.rule_modules()

      assert Rules.Core in modules
      assert Rules.Functions in modules
      assert Rules.ControlFlow in modules
      assert Rules.Builtins in modules
    end

    test "all_rules returns rules from all modules" do
      rules = Rules.all_rules()

      assert is_list(rules)
      assert length(rules) > 0
    end

    test "registry returns a valid registry" do
      registry = Rules.registry()
      assert %Deft.Rule.Registry{} = registry
    end
  end

  describe "Core rules" do
    setup do
      %{ctx: Context.new(__ENV__)}
    end

    test "literal rule synthesizes integer type", %{ctx: ctx} do
      ast = %AST.Literal{value: 42, meta: []}
      rule = Rules.Core.Rule_literal

      {:ok, erased, type, bindings, _ctx} = rule.apply(ast, nil, ctx)

      assert erased == 42
      assert %Type.Integer{} = type
      assert bindings == []
    end

    test "literal rule synthesizes boolean type", %{ctx: ctx} do
      ast = %AST.Literal{value: true, meta: []}
      rule = Rules.Core.Rule_literal

      {:ok, erased, type, _bindings, _ctx} = rule.apply(ast, nil, ctx)

      assert erased == true
      assert %Type.Boolean{} = type
    end

    test "literal rule synthesizes float type", %{ctx: ctx} do
      ast = %AST.Literal{value: 3.14, meta: []}
      rule = Rules.Core.Rule_literal

      {:ok, erased, type, _bindings, _ctx} = rule.apply(ast, nil, ctx)

      assert erased == 3.14
      assert %Type.Float{} = type
    end

    test "literal rule synthesizes atom type", %{ctx: ctx} do
      ast = %AST.Literal{value: :hello, meta: []}
      rule = Rules.Core.Rule_literal

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

      rule = Rules.Core.Rule_local

      {:ok, erased, type, bindings, _ctx} = rule.apply(ast, nil, ctx)

      assert erased == {:x, [__deft_type__: Type.integer()], nil}
      assert %Type.Integer{} = type
      assert bindings == []
    end

    test "annotation rule creates binding", %{ctx: ctx} do
      local = %AST.Local{name: :x, meta: [], context: nil}
      ast = %AST.Annotation{pattern: local, type: Type.boolean(), meta: []}

      rule = Rules.Core.Rule_annotation

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

      rule = Rules.Core.Rule_tuple

      {:ok, _erased, type, _bindings, _ctx} = rule.apply(ast, nil, ctx)

      assert %Type.FixedTuple{elements: [%Type.Integer{}, %Type.Boolean{}]} = type
    end

    test "pair rule synthesizes 2-tuple type", %{ctx: ctx} do
      ast = %AST.Pair{
        fst: %AST.Literal{value: :ok, meta: []},
        snd: %AST.Literal{value: 42, meta: []}
      }

      rule = Rules.Core.Rule_pair

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

      rule = Rules.Core.Rule_list

      {:ok, erased, type, _bindings, _ctx} = rule.apply(ast, nil, ctx)

      assert erased == [1, 2, 3]
      assert %Type.FixedList{contents: %Type.Integer{}} = type
    end
  end

  describe "Function rules" do
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

      rule = Rules.Functions.Rule_fn

      {:ok, _erased, type, bindings, _ctx} = rule.apply(ast, nil, ctx)

      assert %Type.Fn{inputs: [%Type.Integer{}], output: %Type.Boolean{}} = type
      assert bindings == []
    end
  end

  describe "ControlFlow rules" do
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

      rule = Rules.ControlFlow.Rule_if

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

      rule = Rules.ControlFlow.Rule_match

      {:ok, _erased, type, bindings, _ctx} = rule.apply(ast, nil, ctx)

      assert %Type.Integer{} = type
      assert [{^x, %Type.Integer{}}] = bindings
    end
  end

  describe "Builtins rules" do
    setup do
      %{ctx: Context.new(__ENV__)}
    end

    test "local_call rule handles arithmetic", %{ctx: ctx} do
      ast = %AST.LocalCall{
        name: :+,
        args: [%AST.Literal{value: 1, meta: []}, %AST.Literal{value: 2, meta: []}],
        meta: []
      }

      rule = Rules.Builtins.Rule_local_call

      {:ok, _erased, type, _bindings, _ctx} = rule.apply(ast, nil, ctx)

      assert %Type.Integer{} = type
    end

    test "local_call rule handles comparisons", %{ctx: ctx} do
      ast = %AST.LocalCall{
        name: :>,
        args: [%AST.Literal{value: 1, meta: []}, %AST.Literal{value: 2, meta: []}],
        meta: []
      }

      rule = Rules.Builtins.Rule_local_call

      {:ok, _erased, type, _bindings, _ctx} = rule.apply(ast, nil, ctx)

      assert %Type.Boolean{} = type
    end
  end
end
