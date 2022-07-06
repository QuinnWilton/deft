defmodule Deft.AST do
  alias Deft.AST

  def postwalk(%AST.Annotation{} = ast, f) do
    pattern = postwalk(ast.pattern, f)
    ast = %{ast | pattern: pattern}

    f.(ast)
  end

  def postwalk(%AST.Block{} = ast, f) do
    exprs = postwalk(ast.exprs, f)
    ast = %{ast | exprs: exprs}

    f.(ast)
  end

  def postwalk(%AST.CaseBranch{} = ast, f) do
    pattern = postwalk(ast.pattern, f)
    body = postwalk(ast.body, f)
    ast = %{ast | pattern: pattern, body: body}

    f.(ast)
  end

  def postwalk(%AST.Case{} = ast, f) do
    subject = postwalk(ast.subject, f)
    branches = postwalk(ast.branches, f)
    ast = %{ast | subject: subject, branches: branches}

    f.(ast)
  end

  def postwalk(%AST.CondBranch{} = ast, f) do
    predicate = postwalk(ast.predicate, f)
    body = postwalk(ast.body, f)
    ast = %{ast | predicate: predicate, body: body}

    f.(ast)
  end

  def postwalk(%AST.Cond{} = ast, f) do
    branches = postwalk(ast.branches, f)
    ast = %{ast | branches: branches}

    f.(ast)
  end

  def postwalk(%AST.FnApplication{} = ast, f) do
    fun = postwalk(ast.fun, f)
    args = postwalk(ast.args, f)
    ast = %{ast | fun: fun, args: args}

    f.(ast)
  end

  def postwalk(%AST.Fn{} = ast, f) do
    body = postwalk(ast.body, f)
    args = postwalk(ast.args, f)
    ast = %{ast | body: body, args: args}

    f.(ast)
  end

  def postwalk(%AST.If{} = ast, f) do
    predicate = postwalk(ast.predicate, f)
    do_branch = postwalk(ast.do, f)
    else_branch = postwalk(ast.else, f)

    ast = %{
      ast
      | predicate: predicate,
        do: do_branch,
        else: else_branch
    }

    f.(ast)
  end

  def postwalk(%AST.List{} = ast, f) do
    elements = postwalk(ast.elements, f)
    ast = %{ast | elements: elements}

    f.(ast)
  end

  def postwalk(%AST.LocalCall{} = ast, f) do
    args = postwalk(ast.args, f)
    ast = %{ast | args: args}

    f.(ast)
  end

  def postwalk(%AST.Local{} = ast, f) do
    f.(ast)
  end

  def postwalk(%AST.Match{} = ast, f) do
    pattern = postwalk(ast.pattern, f)
    value = postwalk(ast.value, f)
    ast = %{ast | pattern: pattern, value: value}

    f.(ast)
  end

  def postwalk(%AST.Pair{} = ast, f) do
    fst = postwalk(ast.fst, f)
    snd = postwalk(ast.snd, f)
    ast = %{ast | fst: fst, snd: snd}

    f.(ast)
  end

  def postwalk(%AST.Tuple{} = ast, f) do
    elements = postwalk(ast.elements, f)
    ast = %{ast | elements: elements}

    f.(ast)
  end

  def postwalk(ast, f) when is_list(ast) do
    ast = Enum.map(ast, &postwalk(&1, f))

    f.(ast)
  end

  def postwalk(ast, f) do
    f.(ast)
  end
end
