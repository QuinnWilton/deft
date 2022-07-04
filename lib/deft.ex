defmodule Deft do
  alias Deft.AST
  alias Deft.Annotations

  defmacro compile(do: block) do
    block = rewrite(block)

    {block, _type} =
      Deft.Helpers.compute_and_erase_types(
        block,
        __CALLER__
      )

    block
  end

  defmacro type(do: block) do
    block = rewrite(block)

    {_block, type} =
      Deft.Helpers.compute_and_erase_types(
        block,
        __CALLER__
      )

    Macro.escape(type)
  end

  def rewrite({:__block__, meta, exprs}) do
    exprs = Enum.map(exprs, &rewrite/1)

    AST.Block.new(exprs, meta)
  end

  def rewrite({:fn, fn_meta, [{:->, arrow_meta, [args, body]}]}) do
    args = Enum.map(args, &rewrite_fn_arg/1)
    body = rewrite(body)

    AST.Fn.new(body, args, fn_meta, arrow_meta)
  end

  def rewrite({{:., fun_meta, [fun]}, args_meta, args}) do
    fun = rewrite(fun)
    args = Enum.map(args, &rewrite/1)

    AST.FnApplication.new(fun, args, fun_meta, args_meta)
  end

  def rewrite({:if, meta, [predicate, branches]}) do
    predicate = rewrite(predicate)
    branches = Keyword.map(branches, &rewrite(elem(&1, 1)))

    AST.If.new(
      predicate,
      branches[:do],
      branches[:else],
      meta
    )
  end

  def rewrite({:cond, cond_meta, [[do: branches]]}) do
    branches =
      Enum.map(branches, fn
        {:->, branch_meta, [[predicate], body]} ->
          predicate = rewrite(predicate)
          body = rewrite(body)

          AST.CondBranch.new(predicate, body, branch_meta)
      end)

    AST.Cond.new(branches, cond_meta)
  end

  def rewrite({:case, case_meta, [subject, [do: branches]]}) do
    subject = rewrite(subject)

    branches =
      Enum.map(branches, fn
        {:->, branch_meta, [[pattern], body]} ->
          pattern = rewrite_pattern(pattern)

          AST.CaseBranch.new(pattern, body, branch_meta)
      end)

    AST.Case.new(subject, branches, case_meta)
  end

  def rewrite({:=, meta, [pattern, value]}) do
    pattern = rewrite_pattern(pattern)
    value = rewrite(value)

    AST.Match.new(pattern, value, meta)
  end

  def rewrite({:{}, meta, elements}) do
    elements = Enum.map(elements, &rewrite/1)

    AST.Tuple.new(elements, meta)
  end

  def rewrite({fst, snd}) do
    fst = rewrite(fst)
    snd = rewrite(snd)

    AST.Pair.new(fst, snd)
  end

  def rewrite(elements) when is_list(elements) do
    elements = Enum.map(elements, &rewrite/1)

    AST.List.new(elements)
  end

  def rewrite({name, meta, args}) when is_list(args) do
    args = Enum.map(args, &rewrite/1)

    AST.LocalCall.new(name, args, meta)
  end

  def rewrite({name, meta, context}) when is_atom(context) do
    AST.Local.new(name, context, meta)
  end

  def rewrite(literal) do
    literal
  end

  def rewrite_fn_arg({:"::", meta, [name, annotation]}) do
    # TODO: handle literals in function heads
    type = Annotations.parse(annotation)
    name = rewrite(name)

    AST.Annotation.new(name, type, meta)
  end

  def rewrite_pattern({name, meta, context}) when is_atom(context) do
    AST.Local.new(name, context, meta)
  end
end
