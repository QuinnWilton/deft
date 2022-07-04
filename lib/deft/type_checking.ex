defmodule Deft.TypeChecking do
  import Deft.Helpers

  alias Deft.AST
  alias Deft.TypeChecking

  @supported_guards [
    !=: 2,
    !==: 2,
    *: 2,
    +: 1,
    +: 2,
    -: 1,
    -: 2,
    /: 2,
    <: 2,
    <=: 2,
    ==: 2,
    ===: 2,
    >: 2,
    >=: 2,
    abs: 1,
    # binary_part: 3,
    # bit_size: 1,
    # byte_size: 1,
    ceil: 1,
    div: 2,
    elem: 2,
    floor: 1,
    hd: 1,
    is_atom: 1,
    # is_binary: 1,
    # is_bitstring: 1,
    is_boolean: 1,
    is_float: 1,
    is_function: 1,
    is_function: 2,
    is_integer: 1,
    is_list: 1,
    # is_map: 1,
    # is_map_key: 2,
    is_number: 1,
    # is_pid: 1,
    # is_port: 1,
    # is_reference: 1,
    is_tuple: 1,
    length: 1,
    # map_size: 1,
    # node: 0,
    # node: 1,
    not: 1,
    rem: 2,
    round: 1,
    # self: 0,
    tl: 1,
    trunc: 1,
    tuple_size: 1
  ]

  def type_check(%AST.Annotation{} = ast, env) do
    TypeChecking.Annotation.type_check(ast, env)
  end

  def type_check(%AST.Block{} = ast, env) do
    TypeChecking.Block.type_check(ast, env)
  end

  def type_check(%AST.Fn{} = ast, env) do
    TypeChecking.Fn.type_check(ast, env)
  end

  def type_check(%AST.FnApplication{} = ast, env) do
    TypeChecking.FnApplication.type_check(ast, env)
  end

  def type_check(%AST.If{} = ast, env) do
    TypeChecking.If.type_check(ast, env)
  end

  def type_check(%AST.Cond{} = ast, env) do
    TypeChecking.Cond.type_check(ast, env)
  end

  def type_check(%AST.Case{} = ast, env) do
    TypeChecking.Case.type_check(ast, env)
  end

  def type_check(%AST.Tuple{} = ast, env) do
    TypeChecking.Tuple.type_check(ast, env)
  end

  def type_check(%AST.Pair{} = ast, env) do
    fst = type_check(ast.fst, env)
    snd = type_check(ast.snd, env)

    {fst, snd}
  end

  def type_check(%AST.List{} = ast, env) do
    Enum.map(ast.elements, &type_check(&1, env))
  end

  def type_check(%AST.LocalCall{} = ast, env) do
    if Enum.member?(@supported_guards, {ast.name, length(ast.args)}) do
      {args, t} = TypeChecking.Guards.handle_guard(ast.name, ast.args, env)

      annotate({ast.name, ast.meta, args}, t)
    else
      # TODO: Maybe raise because of an unsupported function call?
      {ast.name, ast.meta, ast.args}
    end
  end

  def type_check(%AST.Local{} = ast, _env) do
    {ast.name, ast.meta, ast.context}
  end

  def type_check(%AST.Literal{} = ast, _env) do
    ast.value
  end
end
