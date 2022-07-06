defmodule Deft.TypeChecking do
  import Deft.Helpers

  alias Deft.AST
  alias Deft.TypeChecking

  def type_check(%AST.Annotation{} = ast, env) do
    TypeChecking.Annotation.type_check(ast, env)
  end

  def type_check(%AST.Block{} = ast, env) do
    TypeChecking.Block.type_check(ast, env)
  end

  def type_check(%AST.Match{} = ast, env) do
    TypeChecking.Match.type_check(ast, env)
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
    if TypeChecking.Guards.supported?(ast.name, length(ast.args)) do
      {args, t, bindings} = TypeChecking.Guards.handle_guard(ast.name, ast.args, env)

      {ast.name, ast.meta, args}
      |> annotate_type(t)
      |> annotate_bindings(bindings)
    else
      raise Deft.UnsupportedLocalCall, name: ast.name, arity: length(ast.args)
    end
  end

  def type_check(%AST.Local{} = ast, _env) do
    {ast.name, ast.meta, ast.context}
  end

  def type_check(%AST.Literal{} = ast, _env) do
    ast.value
  end
end
