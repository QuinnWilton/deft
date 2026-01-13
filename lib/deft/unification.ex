defmodule Deft.Unification do
  @moduledoc """
  Type unification for polymorphic instantiation.

  ## What is Unification?

  Unification finds a substitution that makes two types equal (or compatible).
  In our case, we use a simplified one-way matching: we match a polymorphic
  signature (containing type variables) against concrete argument types.

  ## How It Works

  Given:
  - A polymorphic signature like `forall a. [a] -> a`
  - Concrete argument types like `[integer]`

  Unification:
  1. Walks the signature and argument types in parallel
  2. When encountering a `Type.Var` in the signature, binds it to the
     corresponding concrete type (or checks consistency if already bound)
  3. For compound types (lists, tuples, functions), recurses into children
  4. Returns a substitution map: `%{a: integer}`

  This substitution can then be applied to the return type to get
  the concrete result type.

  ## Example

      # Signature: forall a. [a] -> a
      sig_inputs = [Type.fixed_list(Type.Var.new(:a))]
      sig_output = Type.Var.new(:a)

      # Actual argument: [integer]
      actual_args = [Type.fixed_list(Type.integer())]

      # Unify to get substitution
      {:ok, subst} = Unification.infer([:a], sig_inputs, actual_args)
      # => %{a: Type.integer()}

      # Apply to output
      result = Substitution.substitute(sig_output, subst)
      # => Type.integer()

  ## Handling Multiple Occurrences

  If a type variable appears multiple times and binds to different types,
  we compute their union:

      # forall a. (a, a) -> a  with arguments (integer, boolean)
      # => %{a: integer | boolean}

  ## Edge Cases

  - Generic `list()` vs `FixedList`: When matching `[a]` against generic
    `list()`, we bind `a` to `top()` (unknown element type)
  - Arity mismatches: Functions/tuples must have matching arities
  - Subtyping fallback: Non-variable types use subtyping for compatibility
  """

  alias Deft.Type
  alias Deft.Subtyping

  @type substitution :: %{atom() => Type.t() | nil}

  @doc """
  Infer type variable bindings by matching templates against actuals.

  Returns `{:ok, substitution}` or `{:error, reason}`.

  ## Parameters

  - `type_vars`: List of type variable names to solve for
  - `templates`: List of types from the polymorphic signature (may contain Type.Var)
  - `actuals`: List of concrete types to match against

  ## Example

      # Match [a] against [integer] to infer a = integer
      {:ok, subst} = Unification.infer([:a], [Type.fixed_list(Type.var(:a))], [Type.fixed_list(Type.integer())])
      # => %{a: Type.integer()}
  """
  @spec infer([atom()], [Type.t()], [Type.t()]) :: {:ok, substitution()} | {:error, term()}
  def infer(type_vars, templates, actuals) when length(templates) == length(actuals) do
    # Initialize all type variables as unbound (nil).
    initial = Map.new(type_vars, &{&1, nil})

    # Match each template against its corresponding actual type.
    Enum.zip(templates, actuals)
    |> Enum.reduce_while({:ok, initial}, fn {template, actual}, {:ok, subst} ->
      case unify(template, actual, subst) do
        {:ok, new_subst} -> {:cont, {:ok, new_subst}}
        {:error, _} = err -> {:halt, err}
      end
    end)
  end

  def infer(_type_vars, templates, actuals) do
    {:error, {:arity_mismatch, length(templates), length(actuals)}}
  end

  # --- Core Unification Logic ---

  # Type variable: bind or merge with existing binding.
  defp unify(%Type.Var{name: name}, actual, subst) do
    case Map.fetch(subst, name) do
      {:ok, nil} ->
        # Unbound - bind to actual type.
        {:ok, Map.put(subst, name, actual)}

      {:ok, ^actual} ->
        # Already bound to same type - consistent.
        {:ok, subst}

      {:ok, existing} ->
        # Bound to different type - compute union.
        {:ok, Map.put(subst, name, Type.union(existing, actual))}

      :error ->
        # Variable not in the set we're solving for - treat as opaque.
        if Subtyping.subtype_of?(actual, %Type.Var{name: name}) do
          {:ok, subst}
        else
          {:error, {:unknown_type_var, name}}
        end
    end
  end

  # FixedList: recurse on contents.
  defp unify(%Type.FixedList{contents: tc}, %Type.FixedList{contents: ac}, subst) do
    unify(tc, ac, subst)
  end

  # FixedList template vs generic List: bind element var to top.
  defp unify(%Type.FixedList{contents: %Type.Var{name: n}}, %Type.List{}, subst) do
    case Map.fetch(subst, n) do
      {:ok, nil} -> {:ok, Map.put(subst, n, Type.top())}
      {:ok, _existing} -> {:ok, subst}
      :error -> {:ok, subst}
    end
  end

  # Function types: unify inputs and output.
  defp unify(%Type.Fn{inputs: ti, output: to}, %Type.Fn{inputs: ai, output: ao}, subst)
       when length(ti) == length(ai) do
    with {:ok, subst} <- unify_all(ti, ai, subst) do
      unify(to, ao, subst)
    end
  end

  # Tuple types: unify elements pairwise.
  defp unify(%Type.FixedTuple{elements: te}, %Type.FixedTuple{elements: ae}, subst)
       when length(te) == length(ae) do
    unify_all(te, ae, subst)
  end

  # Union types: try to unify with each alternative.
  defp unify(template, %Type.Union{fst: fst, snd: snd}, subst) do
    # Try to unify with both branches of the union.
    case unify(template, fst, subst) do
      {:ok, subst1} ->
        case unify(template, snd, subst1) do
          {:ok, subst2} -> {:ok, subst2}
          {:error, _} -> {:ok, subst1}
        end

      {:error, _} ->
        unify(template, snd, subst)
    end
  end

  # Fallback: if actual is subtype of template, accept.
  defp unify(template, actual, subst) do
    if Subtyping.subtype_of?(template, actual) do
      {:ok, subst}
    else
      {:error, {:type_mismatch, template, actual}}
    end
  end

  defp unify_all(templates, actuals, subst) do
    Enum.zip(templates, actuals)
    |> Enum.reduce_while({:ok, subst}, fn {t, a}, {:ok, s} ->
      case unify(t, a, s) do
        {:ok, s2} -> {:cont, {:ok, s2}}
        err -> {:halt, err}
      end
    end)
  end
end
