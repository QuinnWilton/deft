defmodule Deft.TestHelpers.ASTBuilders do
  @moduledoc """
  Convenient builders for constructing test AST nodes.

  These builders provide a concise way to create AST nodes for testing
  without needing to specify all metadata fields.
  """

  alias Deft.AST

  # Literals

  @doc """
  Creates a literal AST node.

  ## Examples

      literal(42)
      literal(true)
      literal(:ok)
  """
  def literal(value) do
    AST.Literal.new(value)
  end

  # Variables

  @doc """
  Creates a local variable AST node.

  ## Examples

      local(:x)
      local(:my_var)
  """
  def local(name) when is_atom(name) do
    AST.Local.new(name, nil, [])
  end

  @doc """
  Creates a local variable AST node with a pre-attached type in metadata.

  This is useful for testing when you need a variable that already
  has type information (simulating a previously bound variable).

  ## Examples

      typed_local(:x, Type.integer())
  """
  def typed_local(name, type) when is_atom(name) do
    AST.Local.new(name, nil, __deft_type__: type)
  end

  # Annotations

  @doc """
  Creates a type annotation AST node.

  ## Examples

      annotation(:x, Type.integer())
  """
  def annotation(name, type) when is_atom(name) do
    AST.Annotation.new(local(name), type)
  end

  @doc """
  Creates a type annotation from an existing AST node.

  ## Examples

      annotate(local(:x), Type.integer())
  """
  def annotate(ast, type) do
    AST.Annotation.new(ast, type)
  end

  # Functions

  @doc """
  Creates an anonymous function AST node.

  Args should be annotation nodes (use `annotation/2` to create them).

  ## Examples

      fn_expr([annotation(:x, Type.integer())], typed_local(:x, Type.integer()))
  """
  def fn_expr(args, body) when is_list(args) do
    AST.Fn.new(body, args)
  end

  @doc """
  Creates a function application AST node.

  ## Examples

      fn_apply(some_fn, [literal(42)])
  """
  def fn_apply(fun, args) when is_list(args) do
    AST.FnApplication.new(fun, args)
  end

  # Control flow

  @doc """
  Creates an if expression AST node.

  ## Examples

      if_expr(literal(true), literal(1), literal(0))
  """
  def if_expr(predicate, do_branch, else_branch) do
    AST.If.new(predicate, do_branch, else_branch)
  end

  @doc """
  Creates a case expression AST node.

  ## Examples

      case_expr(subject, [
        case_branch(local(:x), literal(:matched))
      ])
  """
  def case_expr(subject, branches) when is_list(branches) do
    AST.Case.new(subject, branches)
  end

  @doc """
  Creates a case branch AST node.

  ## Examples

      case_branch(local(:x), typed_local(:x, Type.integer()))
  """
  def case_branch(pattern, body) do
    AST.CaseBranch.new(pattern, body)
  end

  @doc """
  Creates a cond expression AST node.

  ## Examples

      cond_expr([
        cond_branch(literal(true), literal(1)),
        cond_branch(literal(true), literal(2))
      ])
  """
  def cond_expr(branches) when is_list(branches) do
    AST.Cond.new(branches)
  end

  @doc """
  Creates a cond branch AST node.

  ## Examples

      cond_branch(local_call(:is_integer, [typed_local(:x, Type.integer())]), literal(1))
  """
  def cond_branch(predicate, body) do
    AST.CondBranch.new(predicate, body)
  end

  @doc """
  Creates a match expression AST node.

  ## Examples

      match_expr(local(:x), literal(42))
  """
  def match_expr(pattern, value) do
    AST.Match.new(pattern, value)
  end

  # Data structures

  @doc """
  Creates a tuple AST node.

  ## Examples

      tuple([literal(1), literal(2), literal(3)])
  """
  def tuple(elements) when is_list(elements) do
    AST.Tuple.new(elements)
  end

  @doc """
  Creates a pair (2-tuple) AST node.

  ## Examples

      pair(literal(1), literal(2))
  """
  def pair(fst, snd) do
    AST.Pair.new(fst, snd)
  end

  @doc """
  Creates a list AST node.

  ## Examples

      list([literal(1), literal(2), literal(3)])
  """
  def list(elements) when is_list(elements) do
    AST.List.new(elements)
  end

  # Blocks

  @doc """
  Creates a block AST node.

  ## Examples

      block([
        match_expr(annotation(:x, Type.integer()), literal(1)),
        typed_local(:x, Type.integer())
      ])
  """
  def block(exprs) when is_list(exprs) do
    AST.Block.new(exprs)
  end

  # Local calls (guards, operators, etc.)

  @doc """
  Creates a local call AST node.

  ## Examples

      local_call(:is_integer, [typed_local(:x, Type.integer())])
      local_call(:+, [literal(1), literal(2)])
  """
  def local_call(name, args) when is_atom(name) and is_list(args) do
    AST.LocalCall.new(name, args)
  end

  # Helpers for common patterns

  @doc """
  Creates an identity function for a given type.

  ## Examples

      identity_fn(Type.integer())
      # => fn x :: integer -> x end
  """
  def identity_fn(type) do
    var = local(:x)
    typed_var = typed_local(:x, type)

    fn_expr([AST.Annotation.new(var, type)], typed_var)
  end

  @doc """
  Creates a constant function that always returns a literal.

  ## Examples

      const_fn(Type.integer(), 42)
      # => fn _x :: integer -> 42 end
  """
  def const_fn(arg_type, return_value) do
    var = local(:_x)

    fn_expr([AST.Annotation.new(var, arg_type)], literal(return_value))
  end

  @doc """
  Creates an arithmetic expression.

  ## Examples

      arithmetic(:+, literal(1), literal(2))
  """
  def arithmetic(op, left, right) when op in [:+, :-, :*, :/] do
    local_call(op, [left, right])
  end

  @doc """
  Creates a comparison expression.

  ## Examples

      comparison(:==, literal(1), literal(2))
  """
  def comparison(op, left, right) when op in [:==, :!=, :===, :!==, :<, :>, :<=, :>=] do
    local_call(op, [left, right])
  end

  @doc """
  Creates a guard check expression.

  ## Examples

      guard(:is_integer, typed_local(:x, Type.integer()))
  """
  def guard(name, arg)
      when name in [
             :is_integer,
             :is_float,
             :is_number,
             :is_boolean,
             :is_atom,
             :is_list,
             :is_tuple,
             :is_function
           ] do
    local_call(name, [arg])
  end
end
