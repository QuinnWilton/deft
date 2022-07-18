defmodule Deft.Macro do
  alias Deft.AST
  alias Deft.Type

  def to_string(node) do
    raw_ast = AST.to_raw_ast(node)

    Macro.to_string(raw_ast)
  end

  def postwalk(%AST.Annotation{} = node, f) do
    pattern = postwalk(node.pattern, f)
    type = postwalk(node.type, f)
    node = %{node | pattern: pattern, type: type}

    f.(node)
  end

  def postwalk(%AST.Pin{} = node, f) do
    expr = postwalk(node.expr, f)
    node = %{node | expr: expr}

    f.(node)
  end

  def postwalk(%AST.Block{} = node, f) do
    exprs = postwalk(node.exprs, f)
    node = %{node | exprs: exprs}

    f.(node)
  end

  def postwalk(%AST.CaseBranch{} = node, f) do
    pattern = postwalk(node.pattern, f)
    body = postwalk(node.body, f)
    node = %{node | pattern: pattern, body: body}

    f.(node)
  end

  def postwalk(%AST.Case{} = node, f) do
    subject = postwalk(node.subject, f)
    branches = postwalk(node.branches, f)
    node = %{node | subject: subject, branches: branches}

    f.(node)
  end

  def postwalk(%AST.CondBranch{} = node, f) do
    predicate = postwalk(node.predicate, f)
    body = postwalk(node.body, f)
    node = %{node | predicate: predicate, body: body}

    f.(node)
  end

  def postwalk(%AST.Cond{} = node, f) do
    branches = postwalk(node.branches, f)
    node = %{node | branches: branches}

    f.(node)
  end

  def postwalk(%AST.FnApplication{} = node, f) do
    fun = postwalk(node.fun, f)
    args = postwalk(node.args, f)
    node = %{node | fun: fun, args: args}

    f.(node)
  end

  def postwalk(%AST.Fn{} = node, f) do
    body = postwalk(node.body, f)
    args = postwalk(node.args, f)
    node = %{node | body: body, args: args}

    f.(node)
  end

  def postwalk(%AST.If{} = node, f) do
    predicate = postwalk(node.predicate, f)
    do_branch = postwalk(node.do, f)
    else_branch = postwalk(node.else, f)

    node = %{
      node
      | predicate: predicate,
        do: do_branch,
        else: else_branch
    }

    f.(node)
  end

  def postwalk(%AST.List{} = node, f) do
    elements = postwalk(node.elements, f)
    node = %{node | elements: elements}

    f.(node)
  end

  def postwalk(%AST.LocalCall{} = node, f) do
    args = postwalk(node.args, f)
    node = %{node | args: args}

    f.(node)
  end

  def postwalk(%AST.Local{} = node, f) do
    f.(node)
  end

  def postwalk(%AST.Match{} = node, f) do
    pattern = postwalk(node.pattern, f)
    value = postwalk(node.value, f)
    node = %{node | pattern: pattern, value: value}

    f.(node)
  end

  def postwalk(%AST.Pair{} = node, f) do
    fst = postwalk(node.fst, f)
    snd = postwalk(node.snd, f)
    node = %{node | fst: fst, snd: snd}

    f.(node)
  end

  def postwalk(%AST.Tuple{} = node, f) do
    elements = postwalk(node.elements, f)
    node = %{node | elements: elements}

    f.(node)
  end

  def postwalk(%AST.DefData{} = node, f) do
    name = postwalk(node.name, f)
    variants = postwalk(node.variants, f)
    node = %{node | name: name, variants: variants}

    f.(node)
  end

  def postwalk(%AST.TypeConstructorCall{} = node, f) do
    name = postwalk(node.name, f)
    args = postwalk(node.args, f)
    node = %{node | name: name, args: args}

    f.(node)
  end

  def postwalk(%AST.Variant{} = node, f) do
    name = postwalk(node.name, f)
    columns = postwalk(node.columns, f)
    node = %{node | name: name, columns: columns}

    f.(node)
  end

  def postwalk(%Type.ADT{} = node, f) do
    variants = f.(node.variants)
    node = %{node | variants: variants}

    f.(node)
  end

  def postwalk(%Type.Alias{} = node, f) do
    name = f.(node.name)
    node = %{node | name: name}

    f.(node)
  end

  def postwalk(%Type.Fn{} = node, f) do
    inputs = Enum.map(node.inputs, &postwalk(&1, f))
    output = f.(node.output)
    node = %{node | inputs: inputs, output: output}

    f.(node)
  end

  def postwalk(%Type.Union{} = node, f) do
    fst = f.(node.fst)
    snd = f.(node.snd)
    node = %{node | fst: fst, snd: snd}

    f.(node)
  end

  def postwalk(%Type.Intersection{} = node, f) do
    fst = f.(node.fst)
    snd = f.(node.snd)
    node = %{node | fst: fst, snd: snd}

    f.(node)
  end

  def postwalk(%Type.FixedList{} = node, f) do
    contents = f.(node.contents)
    node = %{node | contents: contents}

    f.(node)
  end

  def postwalk(%Type.FixedTuple{} = node, f) do
    elements = f.(node.elements)
    node = %{node | elements: elements}

    f.(node)
  end

  def postwalk(node, f) when is_list(node) do
    node = Enum.map(node, &postwalk(&1, f))

    f.(node)
  end

  def postwalk(node, f) do
    f.(node)
  end
end
