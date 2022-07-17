defmodule Deft.TypeChecking.If do
  import Deft.Helpers

  alias Deft.AST
  alias Deft.Subtyping
  alias Deft.Type

  def type_check(%AST.If{} = if_ast, env, opts) do
    {predicate, predicate_t, bindings} =
      compute_and_erase_types(
        if_ast.predicate,
        env,
        opts
      )

    {do_branch, do_branch_t, _} =
      compute_and_erase_type_in_context(
        if_ast.do,
        bindings,
        env,
        opts
      )

    {else_branch, else_branch_t, _} =
      compute_and_erase_type_in_context(
        if_ast.else,
        bindings,
        env,
        opts
      )

    unless Subtyping.subtype_of?(Type.boolean(), predicate_t) do
      raise Deft.TypecheckingError, expected: Type.boolean(), actual: predicate_t
    end

    type = Type.union(do_branch_t, else_branch_t)

    {:if, if_ast.meta, [predicate, [do: do_branch, else: else_branch]]}
    |> annotate_type(type)
    |> annotate_bindings(bindings)
  end
end
