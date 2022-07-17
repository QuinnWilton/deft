defmodule Deft.TypeChecking do
  import Deft.Helpers

  alias Deft.AST
  alias Deft.TypeChecking

  def type_check(%AST.Annotation{} = ast, env, opts) do
    TypeChecking.Annotation.type_check(ast, env, opts)
  end

  def type_check(%AST.Block{} = ast, env, opts) do
    TypeChecking.Block.type_check(ast, env, opts)
  end

  def type_check(%AST.Match{} = ast, env, opts) do
    TypeChecking.Match.type_check(ast, env, opts)
  end

  def type_check(%AST.Fn{} = ast, env, opts) do
    TypeChecking.Fn.type_check(ast, env, opts)
  end

  def type_check(%AST.FnApplication{} = ast, env, opts) do
    TypeChecking.FnApplication.type_check(ast, env, opts)
  end

  def type_check(%AST.If{} = ast, env, opts) do
    TypeChecking.If.type_check(ast, env, opts)
  end

  def type_check(%AST.Cond{} = ast, env, opts) do
    TypeChecking.Cond.type_check(ast, env, opts)
  end

  def type_check(%AST.Case{} = ast, env, opts) do
    TypeChecking.Case.type_check(ast, env, opts)
  end

  def type_check(%AST.Tuple{} = ast, env, opts) do
    TypeChecking.Tuple.type_check(ast, env, opts)
  end

  def type_check(%AST.Pair{} = ast, env, opts) do
    fst = type_check(ast.fst, env, opts)
    snd = type_check(ast.snd, env, opts)

    {fst, snd}
  end

  def type_check(%AST.List{} = ast, env, opts) do
    Enum.map(ast.elements, &type_check(&1, env, opts))
  end

  def type_check(%AST.LocalCall{} = ast, env, opts) do
    if TypeChecking.Guards.supported?(ast.name, length(ast.args)) do
      {args, t, bindings} = TypeChecking.Guards.handle_guard(ast.name, ast.args, env, opts)

      {ast.name, ast.meta, args}
      |> annotate_type(t)
      |> annotate_bindings(bindings)
    else
      raise Deft.UnsupportedLocalCall, name: ast.name, arity: length(ast.args)
    end
  end

  def type_check(%AST.Local{} = ast, _env, _opts) do
    {ast.name, ast.meta, ast.context}
  end

  def type_check(%AST.Literal{} = ast, _env, _opts) do
    ast.value
  end
end
