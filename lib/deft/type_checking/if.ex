defmodule Deft.TypeChecking.If do
  import Deft.Helpers

  alias Deft.AST
  alias Deft.Subtyping
  alias Deft.Type

  def type_check(%AST.If{} = if_ast, env) do
    {predicate, predicate_t} = compute_and_erase_types(if_ast.predicate, env)
    {do_branch, do_branch_t} = compute_and_erase_types(if_ast.do, env)
    {else_branch, else_branch_t} = compute_and_erase_types(if_ast.else, env)

    unless Subtyping.subtype_of?(Type.boolean(), predicate_t) do
      raise Deft.TypecheckingError, expected: Type.boolean(), actual: predicate_t
    end

    type = Type.Union.new([do_branch_t, else_branch_t])

    annotate({:if, if_ast.meta, [predicate, [do: do_branch, else: else_branch]]}, type)
  end
end
