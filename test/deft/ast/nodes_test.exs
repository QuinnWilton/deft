defmodule Deft.AST.NodesTest do
  @moduledoc """
  Unit tests for AST node constructors and Walkable implementations.

  Tests each AST node type's constructor and verifies the Walkable protocol
  implementation correctly identifies children and supports rebuilding.
  """
  use ExUnit.Case, async: true

  alias Deft.AST
  alias Deft.Walkable

  describe "Annotation" do
    test "new/3 creates annotation with pattern and type" do
      pattern = AST.Local.new(:x, nil)
      type = Deft.Type.integer()

      result = AST.Annotation.new(pattern, type)

      assert result.pattern == pattern
      assert result.type == type
    end

    test "Walkable children returns pattern and type" do
      pattern = AST.Local.new(:x, nil)
      type = Deft.Type.integer()
      annotation = AST.Annotation.new(pattern, type)

      assert Walkable.children(annotation) == [pattern, type]
    end

    test "Walkable rebuild reconstructs with new children" do
      pattern = AST.Local.new(:x, nil)
      type = Deft.Type.integer()
      annotation = AST.Annotation.new(pattern, type)

      new_pattern = AST.Local.new(:y, nil)
      new_type = Deft.Type.boolean()
      rebuilt = Walkable.rebuild(annotation, [new_pattern, new_type])

      assert rebuilt.pattern == new_pattern
      assert rebuilt.type == new_type
    end
  end

  describe "Block" do
    test "new/2 creates block with expressions" do
      expr1 = AST.Literal.new(1)
      expr2 = AST.Literal.new(2)

      result = AST.Block.new([expr1, expr2])

      assert result.exprs == [expr1, expr2]
    end

    test "Walkable children returns expressions list" do
      expr1 = AST.Literal.new(1)
      expr2 = AST.Literal.new(2)
      block = AST.Block.new([expr1, expr2])

      assert Walkable.children(block) == [[expr1, expr2]]
    end
  end

  describe "Case" do
    test "new/3 creates case with subject and branches" do
      subject = AST.Local.new(:x, nil)
      branch = AST.CaseBranch.new(AST.Local.new(:y, nil), AST.Literal.new(1))

      result = AST.Case.new(subject, [branch])

      assert result.subject == subject
      assert result.branches == [branch]
    end

    test "Walkable children returns subject and branches" do
      subject = AST.Local.new(:x, nil)
      branch = AST.CaseBranch.new(AST.Local.new(:y, nil), AST.Literal.new(1))
      case_expr = AST.Case.new(subject, [branch])

      assert Walkable.children(case_expr) == [subject, [branch]]
    end
  end

  describe "CaseBranch" do
    test "new/3 creates branch with pattern and body" do
      pattern = AST.Local.new(:x, nil)
      body = AST.Literal.new(42)

      result = AST.CaseBranch.new(pattern, body)

      assert result.pattern == pattern
      assert result.body == body
    end

    test "Walkable children returns pattern and body" do
      pattern = AST.Local.new(:x, nil)
      body = AST.Literal.new(42)
      branch = AST.CaseBranch.new(pattern, body)

      assert Walkable.children(branch) == [pattern, body]
    end
  end

  describe "Cond" do
    test "new/2 creates cond with branches" do
      branch = AST.CondBranch.new(AST.Literal.new(true), AST.Literal.new(1))

      result = AST.Cond.new([branch])

      assert result.branches == [branch]
    end
  end

  describe "CondBranch" do
    test "new/3 creates branch with predicate and body" do
      predicate = AST.Literal.new(true)
      body = AST.Literal.new(42)

      result = AST.CondBranch.new(predicate, body)

      assert result.predicate == predicate
      assert result.body == body
    end
  end

  describe "Cons" do
    test "new/3 creates cons with head and rest" do
      head = AST.Literal.new(1)
      rest = AST.Local.new(:xs, nil)

      result = AST.Cons.new(head, rest)

      assert result.head == head
      assert result.rest == rest
    end

    test "Walkable children returns head and rest" do
      head = AST.Literal.new(1)
      rest = AST.Local.new(:xs, nil)
      cons = AST.Cons.new(head, rest)

      assert Walkable.children(cons) == [head, rest]
    end
  end

  describe "Fn" do
    test "new/4 creates anonymous function" do
      body = AST.Local.new(:x, nil)
      arg = AST.Annotation.new(AST.Local.new(:x, nil), Deft.Type.integer())

      result = AST.Fn.new(body, [arg])

      assert result.body == body
      assert result.args == [arg]
    end
  end

  describe "FnApplication" do
    test "new/4 creates function application" do
      fun = AST.Local.new(:f, nil)
      args = [AST.Literal.new(1), AST.Literal.new(2)]

      result = AST.FnApplication.new(fun, args)

      assert result.fun == fun
      assert result.args == args
    end
  end

  describe "If" do
    test "new/4 creates if expression" do
      predicate = AST.Literal.new(true)
      do_branch = AST.Literal.new(1)
      else_branch = AST.Literal.new(2)

      result = AST.If.new(predicate, do_branch, else_branch)

      assert result.predicate == predicate
      assert result.do == do_branch
      assert result.else == else_branch
    end

    test "Walkable children returns predicate and both branches" do
      predicate = AST.Literal.new(true)
      do_branch = AST.Literal.new(1)
      else_branch = AST.Literal.new(2)
      if_expr = AST.If.new(predicate, do_branch, else_branch)

      assert Walkable.children(if_expr) == [predicate, do_branch, else_branch]
    end
  end

  describe "List" do
    test "new/2 creates list with elements" do
      elements = [AST.Literal.new(1), AST.Literal.new(2)]

      result = AST.List.new(elements)

      assert result.elements == elements
    end

    test "empty list" do
      result = AST.List.new([])

      assert result.elements == []
    end
  end

  describe "Literal" do
    test "new/2 creates literal with integer" do
      result = AST.Literal.new(42)
      assert result.value == 42
    end

    test "new/2 creates literal with float" do
      result = AST.Literal.new(3.14)
      assert result.value == 3.14
    end

    test "new/2 creates literal with boolean" do
      result = AST.Literal.new(true)
      assert result.value == true
    end

    test "new/2 creates literal with atom" do
      result = AST.Literal.new(:foo)
      assert result.value == :foo
    end

    test "new/2 creates literal with string" do
      result = AST.Literal.new("hello")
      assert result.value == "hello"
    end

    test "Walkable children returns empty list (leaf node)" do
      literal = AST.Literal.new(42)
      assert Walkable.children(literal) == []
    end
  end

  describe "Local" do
    test "new/3 creates local variable reference" do
      result = AST.Local.new(:x, nil, line: 1)

      assert result.name == :x
      assert result.context == nil
      assert result.meta == [line: 1]
    end

    test "Walkable children returns empty list (leaf node)" do
      local = AST.Local.new(:x, nil)
      assert Walkable.children(local) == []
    end
  end

  describe "LocalCall" do
    test "new/3 creates local function call" do
      args = [AST.Literal.new(1), AST.Literal.new(2)]

      result = AST.LocalCall.new(:add, args)

      assert result.name == :add
      assert result.args == args
    end

    test "Walkable children returns args" do
      args = [AST.Literal.new(1), AST.Literal.new(2)]
      call = AST.LocalCall.new(:add, args)

      assert Walkable.children(call) == [args]
    end
  end

  describe "Match" do
    test "new/3 creates match expression" do
      pattern = AST.Local.new(:x, nil)
      value = AST.Literal.new(42)

      result = AST.Match.new(pattern, value)

      assert result.pattern == pattern
      assert result.value == value
    end
  end

  describe "Pair" do
    test "new/3 creates two-element tuple" do
      fst = AST.Literal.new(:ok)
      snd = AST.Literal.new(42)

      result = AST.Pair.new(fst, snd)

      assert result.fst == fst
      assert result.snd == snd
    end
  end

  describe "Pin" do
    test "new/2 creates pin expression" do
      expr = AST.Local.new(:x, nil)

      result = AST.Pin.new(expr)

      assert result.expr == expr
    end

    test "Walkable children returns pinned expression" do
      expr = AST.Local.new(:x, nil)
      pin = AST.Pin.new(expr)

      assert Walkable.children(pin) == [expr]
    end
  end

  describe "Tuple" do
    test "new/2 creates tuple with elements" do
      elements = [AST.Literal.new(1), AST.Literal.new(2), AST.Literal.new(3)]

      result = AST.Tuple.new(elements)

      assert result.elements == elements
    end

    test "empty tuple" do
      result = AST.Tuple.new([])
      assert result.elements == []
    end
  end

  describe "TypeConstructorCall" do
    test "new/5 creates type constructor call" do
      args = [AST.Literal.new(42)]
      type = Deft.Type.integer()
      variant = Deft.Type.Variant.new(:some, :option, [Deft.Type.integer()])

      result = AST.TypeConstructorCall.new(:some, args, type, variant)

      assert result.name == :some
      assert result.args == args
      assert result.type == type
      assert result.variant == variant
    end
  end

  describe "Variant (AST)" do
    test "new/4 creates variant definition" do
      columns = [Deft.Type.integer()]

      result = AST.Variant.new(:some, :option, columns)

      assert result.name == :some
      assert result.adt_name == :option
      assert result.columns == columns
    end
  end
end
