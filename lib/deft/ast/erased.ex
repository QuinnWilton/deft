defmodule Deft.AST.Erased do
  @moduledoc """
  Builder functions for constructing erased Elixir AST.

  These functions provide a cleaner API for building raw Elixir AST tuples
  in typing rule conclusions, replacing manual tuple construction like:

      {:if, meta, [pred, [do: do_branch, else: else_branch]]}

  With readable builder calls:

      Erased.if_expr(meta, pred, do_branch, else_branch)

  ## Usage

  In rule files, alias this module and use builders in conclusions:

      alias Deft.AST.Erased

      defrule :if, %AST.If{...} do
        ...
        conclude(
          Erased.if_expr(meta, erased_pred, erased_do, erased_else)
          ~> Type.union(do_type, else_type)
        )
      end
  """

  # ============================================================================
  # Control Flow
  # ============================================================================

  @doc """
  Builds an if expression AST.

      iex> Erased.if_expr([], :pred, :do_body, :else_body)
      {:if, [], [:pred, [do: :do_body, else: :else_body]]}
  """
  @spec if_expr(keyword(), term(), term(), term()) :: Macro.t()
  def if_expr(meta, pred, do_branch, else_branch) do
    {:if, meta, [pred, [do: do_branch, else: else_branch]]}
  end

  @doc """
  Builds a case expression AST.

      iex> Erased.case_expr([], :subject, [:branch1, :branch2])
      {:case, [], [:subject, [do: [:branch1, :branch2]]]}
  """
  @spec case_expr(keyword(), term(), [term()]) :: Macro.t()
  def case_expr(meta, subject, branches) do
    {:case, meta, [subject, [do: branches]]}
  end

  @doc """
  Builds a cond expression AST.

      iex> Erased.cond_expr([], [:branch1, :branch2])
      {:cond, [], [[do: [:branch1, :branch2]]]}
  """
  @spec cond_expr(keyword(), [term()]) :: Macro.t()
  def cond_expr(meta, branches) do
    {:cond, meta, [[do: branches]]}
  end

  @doc """
  Builds a branch (arrow clause) for case/cond expressions.

      iex> Erased.branch([], :pattern, :body)
      {:->, [], [[:pattern], :body]}
  """
  @spec branch(keyword(), term(), term()) :: Macro.t()
  def branch(meta, pattern, body) do
    {:->, meta, [[pattern], body]}
  end

  # ============================================================================
  # Data Structures
  # ============================================================================

  @doc """
  Builds a tuple AST (3+ elements use `:{}`).

      iex> Erased.tuple([], [:a, :b, :c])
      {:{}, [], [:a, :b, :c]}
  """
  @spec tuple(keyword(), [term()]) :: Macro.t()
  def tuple(meta, elements) do
    {:{}, meta, elements}
  end

  @doc """
  Builds a 2-tuple (pair) AST. Two-element tuples don't need `:{}`}.

      iex> Erased.pair(:a, :b)
      {:a, :b}
  """
  @spec pair(term(), term()) :: {term(), term()}
  def pair(fst, snd) do
    {fst, snd}
  end

  @doc """
  Builds a block expression AST.

      iex> Erased.block([], [:expr1, :expr2])
      {:__block__, [], [:expr1, :expr2]}
  """
  @spec block(keyword(), [term()]) :: Macro.t()
  def block(meta, exprs) do
    {:__block__, meta, exprs}
  end

  # ============================================================================
  # Functions
  # ============================================================================

  @doc """
  Builds an anonymous function AST.

      iex> Erased.fn_expr([], [], [:x], :body)
      {:fn, [], [{:->, [], [[:x], :body]}]}
  """
  @spec fn_expr(keyword(), keyword(), [term()], term()) :: Macro.t()
  def fn_expr(fn_meta, arrow_meta, args, body) do
    {:fn, fn_meta, [{:->, arrow_meta, [args, body]}]}
  end

  @doc """
  Builds a function application AST (f.(args)).

      iex> Erased.fn_apply([], [], :fun, [:arg1, :arg2])
      {{:., [], [:fun]}, [], [:arg1, :arg2]}
  """
  @spec fn_apply(keyword(), keyword(), term(), [term()]) :: Macro.t()
  def fn_apply(fun_meta, args_meta, fun, args) do
    {{:., fun_meta, [fun]}, args_meta, args}
  end

  @doc """
  Builds a local function call AST.

      iex> Erased.local_call([], :foo, [:arg1, :arg2])
      {:foo, [], [:arg1, :arg2]}
  """
  @spec local_call(keyword(), atom(), [term()]) :: Macro.t()
  def local_call(meta, name, args) do
    {name, meta, args}
  end

  # ============================================================================
  # Pattern Matching
  # ============================================================================

  @doc """
  Builds a match/assignment expression AST.

      iex> Erased.match([], :pattern, :value)
      {:=, [], [:pattern, :value]}
  """
  @spec match(keyword(), term(), term()) :: Macro.t()
  def match(meta, pattern, value) do
    {:=, meta, [pattern, value]}
  end
end
