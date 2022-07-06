defmodule Deft.Compiler do
  alias Deft.AST
  alias Deft.Annotations

  def compile({:__block__, meta, exprs}) do
    exprs = Enum.map(exprs, &compile/1)

    AST.Block.new(exprs, meta)
  end

  def compile({:fn, fn_meta, [{:->, arrow_meta, [args, body]}]}) do
    args = Enum.map(args, &compile_fn_arg/1)
    body = compile(body)

    AST.Fn.new(body, args, fn_meta, arrow_meta)
  end

  def compile({{:., fun_meta, [fun]}, args_meta, args}) do
    fun = compile(fun)
    args = Enum.map(args, &compile/1)

    AST.FnApplication.new(fun, args, fun_meta, args_meta)
  end

  def compile({:if, meta, [predicate, branches]}) do
    predicate = compile(predicate)
    branches = Keyword.map(branches, &compile(elem(&1, 1)))

    AST.If.new(
      predicate,
      branches[:do],
      branches[:else],
      meta
    )
  end

  def compile({:cond, cond_meta, [[do: branches]]}) do
    branches =
      Enum.map(branches, fn
        {:->, branch_meta, [[predicate], body]} ->
          predicate = compile(predicate)
          body = compile(body)

          AST.CondBranch.new(predicate, body, branch_meta)
      end)

    AST.Cond.new(branches, cond_meta)
  end

  def compile({:case, case_meta, [subject, [do: branches]]}) do
    subject = compile(subject)

    branches =
      Enum.map(branches, fn
        {:->, branch_meta, [[pattern], body]} ->
          pattern = compile_pattern(pattern)
          body = compile(body)

          AST.CaseBranch.new(pattern, body, branch_meta)
      end)

    AST.Case.new(subject, branches, case_meta)
  end

  def compile({:=, meta, [pattern, value]}) do
    pattern = compile_pattern(pattern)
    value = compile(value)

    AST.Match.new(pattern, value, meta)
  end

  def compile({:{}, meta, elements}) do
    elements = Enum.map(elements, &compile/1)

    AST.Tuple.new(elements, meta)
  end

  def compile({fst, snd}) do
    fst = compile(fst)
    snd = compile(snd)

    AST.Pair.new(fst, snd)
  end

  def compile(elements) when is_list(elements) do
    elements = Enum.map(elements, &compile/1)

    AST.List.new(elements)
  end

  def compile({name, meta, args}) when is_list(args) do
    args = Enum.map(args, &compile/1)

    AST.LocalCall.new(name, args, meta)
  end

  def compile({name, meta, context}) when is_atom(context) do
    AST.Local.new(name, context, meta)
  end

  def compile(literal)
      when is_atom(literal)
      when is_boolean(literal)
      when is_float(literal)
      when is_integer(literal)
      when is_number(literal) do
    AST.Literal.new(literal)
  end

  def compile_fn_arg({:"::", meta, [pattern, annotation]}) do
    pattern = compile_pattern(pattern)
    type = Annotations.parse(annotation)

    AST.Annotation.new(pattern, type, meta)
  end

  def compile_pattern(literal)
      when is_atom(literal)
      when is_boolean(literal)
      when is_float(literal)
      when is_integer(literal)
      when is_number(literal) do
    AST.Literal.new(literal)
  end

  def compile_pattern({:^, meta, [expr]}) do
    expr = compile_pattern(expr)

    AST.Pin.new(expr, meta)
  end

  def compile_pattern({:|, meta, [head, rest]}) do
    head = compile_pattern(head)
    rest = compile_pattern(rest)

    AST.Cons.new(head, rest, meta)
  end

  def compile_pattern({:{}, meta, elements}) do
    elements = Enum.map(elements, &compile_pattern/1)

    AST.Tuple.new(elements, meta)
  end

  def compile_pattern({:=, meta, [pattern, value]}) do
    pattern = compile_pattern(pattern)
    value = compile_pattern(value)

    AST.Match.new(pattern, value, meta)
  end

  def compile_pattern({name, meta, context}) when is_atom(context) do
    AST.Local.new(name, context, meta)
  end

  def compile_pattern(elements) when is_list(elements) do
    elements = Enum.map(elements, &compile_pattern/1)

    AST.List.new(elements)
  end

  def compile_pattern({fst, snd}) do
    fst = compile_pattern(fst)
    snd = compile_pattern(snd)

    AST.Pair.new(fst, snd)
  end
end
