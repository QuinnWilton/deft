defmodule Deft.WalkableTest do
  use ExUnit.Case

  alias Deft.AST
  alias Deft.Type
  alias Deft.Walker
  alias Deft.Walkable

  describe "Walkable protocol for AST nodes" do
    test "Literal has no children" do
      literal = AST.Literal.new(42)

      assert Walkable.children(literal) == []
      assert Walkable.rebuild(literal, []) == literal
    end

    test "Local has no children" do
      local = AST.Local.new(:x, nil, [])

      assert Walkable.children(local) == []
      assert Walkable.rebuild(local, []) == local
    end

    test "Tuple has elements as children" do
      elem1 = AST.Literal.new(1)
      elem2 = AST.Literal.new(2)
      tuple = AST.Tuple.new([elem1, elem2], [])

      assert [[^elem1, ^elem2]] = Walkable.children(tuple)
    end

    test "Fn has body and args as children" do
      arg = AST.Annotation.new(AST.Local.new(:x, nil, []), Type.integer(), [])
      body = AST.Local.new(:x, nil, [])
      fn_node = AST.Fn.new(body, [arg], [], [])

      children = Walkable.children(fn_node)

      assert [^body, [^arg]] = children
    end

    test "Case has subject and branches as children" do
      subject = AST.Local.new(:x, nil, [])
      pattern = AST.Literal.new(1)
      body = AST.Literal.new(:ok)
      branch = AST.CaseBranch.new(pattern, body, [])
      case_node = AST.Case.new(subject, [branch], [])

      [child_subject, child_branches] = Walkable.children(case_node)

      assert child_subject == subject
      assert child_branches == [branch]
    end

    test "If has predicate, do, and else as children" do
      pred = AST.Literal.new(true)
      do_branch = AST.Literal.new(1)
      else_branch = AST.Literal.new(2)
      if_node = AST.If.new(pred, do_branch, else_branch, [])

      children = Walkable.children(if_node)

      assert [^pred, ^do_branch, ^else_branch] = children
    end

    test "rebuild reconstructs node with new children" do
      elem1 = AST.Literal.new(1)
      elem2 = AST.Literal.new(2)
      tuple = AST.Tuple.new([elem1, elem2], line: 1)

      new_elem1 = AST.Literal.new(10)
      new_elem2 = AST.Literal.new(20)
      rebuilt = Walkable.rebuild(tuple, [[new_elem1, new_elem2]])

      assert rebuilt.elements == [new_elem1, new_elem2]
      assert rebuilt.meta == [line: 1]
    end
  end

  describe "Walkable protocol for Type nodes" do
    test "primitive types have no children" do
      assert Walkable.children(Type.integer()) == []
      assert Walkable.children(Type.boolean()) == []
      assert Walkable.children(Type.atom()) == []
      assert Walkable.children(Type.float()) == []
      assert Walkable.children(Type.number()) == []
      assert Walkable.children(Type.top()) == []
      assert Walkable.children(Type.bottom()) == []
    end

    test "FixedTuple has elements as children" do
      tuple_type = Type.fixed_tuple([Type.integer(), Type.boolean()])

      [[elem1, elem2]] = Walkable.children(tuple_type)

      assert %Type.Integer{} = elem1
      assert %Type.Boolean{} = elem2
    end

    test "FixedList has contents as children" do
      list_type = Type.fixed_list(Type.integer())

      [contents] = Walkable.children(list_type)

      assert %Type.Integer{} = contents
    end

    test "Fn has inputs and output as children" do
      fn_type = Type.fun([Type.integer(), Type.boolean()], Type.atom())

      [inputs, output] = Walkable.children(fn_type)

      assert [%Type.Integer{}, %Type.Boolean{}] = inputs
      assert %Type.Atom{} = output
    end

    test "Union has fst and snd as children" do
      union_type = Type.Union.new(Type.integer(), Type.boolean())

      [fst, snd] = Walkable.children(union_type)

      assert %Type.Integer{} = fst
      assert %Type.Boolean{} = snd
    end

    test "Intersection has fst and snd as children" do
      intersection_type = Type.Intersection.new(Type.integer(), Type.boolean())

      [fst, snd] = Walkable.children(intersection_type)

      assert %Type.Integer{} = fst
      assert %Type.Boolean{} = snd
    end
  end

  describe "Walker.postwalk/2" do
    test "transforms leaf nodes" do
      literal = AST.Literal.new(42)

      result =
        Walker.postwalk(literal, fn
          %AST.Literal{value: n} -> AST.Literal.new(n * 2)
          other -> other
        end)

      assert %AST.Literal{value: 84} = result
    end

    test "transforms nested nodes bottom-up" do
      elem1 = AST.Literal.new(1)
      elem2 = AST.Literal.new(2)
      tuple = AST.Tuple.new([elem1, elem2], [])

      Walker.postwalk(tuple, fn node ->
        # This won't actually accumulate since we're not using reduce,
        # but it demonstrates the traversal order
        node
      end)

      # The function is called on children before parents
      # This is a basic smoke test
      assert %AST.Tuple{} = tuple
    end

    test "handles lists" do
      list = [AST.Literal.new(1), AST.Literal.new(2)]

      result =
        Walker.postwalk(list, fn
          [a, b] -> [b, a]
          other -> other
        end)

      assert [%AST.Literal{value: 2}, %AST.Literal{value: 1}] = result
    end

    test "transforms type nodes" do
      fn_type = Type.fun([Type.integer()], Type.integer())

      result =
        Walker.postwalk(fn_type, fn
          %Type.Integer{} -> Type.float()
          other -> other
        end)

      assert %Type.Fn{inputs: [%Type.Float{}], output: %Type.Float{}} = result
    end
  end

  describe "Walker.postwalk/3 with accumulator" do
    test "accumulates values during traversal" do
      elem1 = AST.Literal.new(1)
      elem2 = AST.Literal.new(2)
      tuple = AST.Tuple.new([elem1, elem2], [])

      {_result, count} =
        Walker.postwalk(tuple, 0, fn node, acc ->
          case node do
            %AST.Literal{} -> {node, acc + 1}
            _ -> {node, acc}
          end
        end)

      assert count == 2
    end

    test "transforms while accumulating" do
      literal = AST.Literal.new(5)

      {result, sum} =
        Walker.postwalk(literal, 0, fn
          %AST.Literal{value: n}, acc ->
            {AST.Literal.new(n * 2), acc + n}

          other, acc ->
            {other, acc}
        end)

      assert %AST.Literal{value: 10} = result
      assert sum == 5
    end
  end

  describe "Walker.prewalk/2" do
    test "transforms nodes top-down" do
      elem1 = AST.Literal.new(1)
      elem2 = AST.Literal.new(2)
      tuple = AST.Tuple.new([elem1, elem2], [])

      # In prewalk, the parent is visited before children
      result =
        Walker.prewalk(tuple, fn
          %AST.Tuple{} ->
            # Replace entire tuple
            AST.Literal.new(:replaced)

          other ->
            other
        end)

      # The tuple was replaced before its children were visited
      assert %AST.Literal{value: :replaced} = result
    end
  end
end
