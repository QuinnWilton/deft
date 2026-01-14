defmodule Deft.Rules.DSL.Helpers do
  @moduledoc """
  Runtime helper functions available within defrule blocks.

  These functions are used by the generated rule code to perform
  type synthesis, checking, and subtype assertions.
  """

  alias Deft.AST
  alias Deft.Context
  alias Deft.Error
  alias Deft.Subtyping
  alias Deft.Type
  alias Deft.TypeChecker
  alias Deft.Helpers, as: DeftHelpers

  @doc """
  Synthesizes a type for an expression with bindings injected.
  Returns {erased, type, new_bindings}.
  """
  def synth!(expr, bindings, ctx) do
    expr_with_bindings = DeftHelpers.inject_bindings(expr, bindings)

    case TypeChecker.check(expr_with_bindings, ctx) do
      {:ok, erased, type, new_bindings, _ctx} ->
        {erased, type, new_bindings}

      {:error, {:no_matching_rule, ast}} ->
        error =
          Error.no_matching_rule(
            ast: ast,
            location: Error.extract_location(ast)
          )

        raise_or_accumulate(ctx, error)

      {:error, reason} ->
        raise "Synthesis failed: #{inspect(reason)}"
    end
  end

  @doc """
  Checks an expression against an expected type with bindings injected.

  Synthesizes the expression's type, then verifies it is a subtype of the
  expected type. Returns {erased, actual_type, new_bindings}.
  """
  def check!(expr, expected_type, bindings, ctx) do
    # First synthesize the expression's type
    {erased, actual_type, new_bindings} = synth!(expr, bindings, ctx)

    # Then verify it's a subtype of expected
    unless Subtyping.subtype_of?(expected_type, actual_type) do
      error =
        Error.type_mismatch(
          expected: expected_type,
          actual: actual_type,
          expression: expr,
          location: Error.extract_location(expr)
        )

      raise_or_accumulate(ctx, error)
    end

    {erased, actual_type, new_bindings}
  end

  @doc """
  Asserts that actual is a subtype of expected.
  Raises a TypecheckingError if the assertion fails.
  """
  def require_subtype!(actual, expected) do
    require_subtype!(actual, expected, nil, nil)
  end

  @doc """
  Asserts that actual is a subtype of expected with context.

  Uses detailed subtype checking to provide element-specific error messages
  for compound types like tuples.
  """
  def require_subtype!(actual, expected, expr, ctx) do
    # Extract sub-expressions for element-level span tracking.
    sub_exprs = extract_sub_exprs(expr)

    case Subtyping.check_subtype(expected, actual, sub_exprs) do
      :ok ->
        :ok

      {:mismatch, %{expected: exp_elem, actual: act_elem, expr: fail_expr}} ->
        # Use the failing element's expression for location if available.
        location_expr = fail_expr || expr

        error =
          Error.type_mismatch(
            expected: exp_elem,
            actual: act_elem,
            expression: location_expr,
            location: if(location_expr, do: Error.extract_location(location_expr), else: nil)
          )

        if ctx do
          raise_or_accumulate(ctx, error)
        else
          Error.raise!(error)
        end
    end
  end

  @doc """
  Extracts child expressions from an AST node for element-level span tracking.

  Returns a list of sub-expressions for compound types, or nil for simple types.
  """
  def extract_sub_exprs(%AST.Tuple{elements: elements}), do: elements
  def extract_sub_exprs(%AST.TypeConstructorCall{args: args}), do: args
  def extract_sub_exprs(%AST.List{elements: elements}), do: elements
  def extract_sub_exprs(%AST.Pair{fst: fst, snd: snd}), do: [fst, snd]
  def extract_sub_exprs(_), do: nil

  # Handles error based on context's error_mode.
  defp raise_or_accumulate(%Context{error_mode: :accumulate} = ctx, error) do
    # In accumulate mode, store errors in process dictionary since context
    # isn't threaded back through the rule system.
    enriched = enrich_error(error, ctx)
    errors = Process.get(:deft_accumulated_errors, [])
    Process.put(:deft_accumulated_errors, errors ++ [enriched])
    {nil, Type.bottom(), []}
  end

  defp raise_or_accumulate(_ctx, %Error{} = error) do
    # In fail_fast mode or no context, raise using the new error system.
    Error.raise!(error)
  end

  # Enriches an error with context information (file, etc.)
  defp enrich_error(%Error{location: nil} = error, %Context{env: env}) when not is_nil(env) do
    %{error | location: Error.extract_location_from_env(env)}
  end

  # If location has line but no file, add file from env.
  defp enrich_error(
         %Error{location: {nil, line, col}} = error,
         %Context{env: %Macro.Env{file: file}}
       )
       when not is_nil(file) do
    %{error | location: {file, line, col}}
  end

  defp enrich_error(error, _ctx), do: error

  @doc """
  Computes the type of a literal value.
  """
  def type_of_literal(v) when is_boolean(v), do: Type.boolean()
  def type_of_literal(v) when is_atom(v), do: Type.atom()
  def type_of_literal(v) when is_binary(v), do: Type.binary()
  def type_of_literal(v) when is_integer(v), do: Type.integer()
  def type_of_literal(v) when is_float(v), do: Type.float()

  @doc """
  Unions a list of types together.
  """
  def union_types([]), do: Type.bottom()
  def union_types([t]), do: t
  def union_types([t | rest]), do: Type.union(t, union_types(rest))

  @doc """
  Checks if a feature is enabled in the context.
  """
  def feature_enabled?(ctx, feature), do: Context.feature_enabled?(ctx, feature)

  @doc """
  Synthesizes types for a list of expressions.
  Returns {erased_list, types_list, bindings_list}.
  """
  def synth_all!(exprs, ctx) do
    {erased_list, types_list, bindings_list} =
      Enum.reduce(exprs, {[], [], []}, fn expr, {acc_erased, acc_types, acc_bindings} ->
        {erased, type, bindings} = synth!(expr, acc_bindings, ctx)
        {acc_erased ++ [erased], acc_types ++ [type], acc_bindings ++ bindings}
      end)

    {erased_list, types_list, bindings_list}
  end

  @doc """
  Synthesizes types for a list of expressions with a custom context.

  This is used by the `&&&` operator to pass scoped attributes to child rules.
  Returns {erased_list, types_list, bindings_list}.
  """
  def synth_all_with_ctx!(exprs, bindings, ctx) do
    {erased_list, types_list, bindings_list} =
      Enum.reduce(exprs, {[], [], bindings}, fn expr, {acc_erased, acc_types, acc_bindings} ->
        {erased, type, new_bindings} = synth!(expr, acc_bindings, ctx)
        {acc_erased ++ [erased], acc_types ++ [type], acc_bindings ++ new_bindings}
      end)

    {erased_list, types_list, bindings_list}
  end

  @doc """
  Checks a list of expressions against expected type(s).

  Two modes based on the second argument:
  - **Homogeneous** (single type): Each expression is checked against the same type
  - **Heterogeneous** (list of types): Each expression is checked against its corresponding type

  Returns {erased_list, bindings_list}.

  ## Examples

      # Homogeneous: all elements must be integers
      check_all!(elements, Type.integer(), bindings, ctx)

      # Heterogeneous: check args[i] against param_types[i]
      check_all!(args, param_types, bindings, ctx)
  """
  def check_all!(exprs, expected, bindings, ctx) when is_list(expected) do
    # Heterogeneous mode: check each expr against corresponding type
    if length(exprs) != length(expected) do
      # Use type_mismatch with tuple types to show arity difference
      error =
        Error.type_mismatch(
          expected: Type.fixed_tuple(expected),
          actual: Type.fixed_tuple(List.duplicate(Type.top(), length(exprs))),
          notes: [
            "Expected #{length(expected)} argument(s), got #{length(exprs)}"
          ]
        )

      Error.raise!(error, ctx)
    end

    {erased_list, bindings_list} =
      exprs
      |> Enum.zip(expected)
      |> Enum.reduce({[], bindings}, fn {expr, expected_type}, {acc_erased, acc_bindings} ->
        {erased, _type, new_bindings} = check!(expr, expected_type, acc_bindings, ctx)
        {acc_erased ++ [erased], acc_bindings ++ new_bindings}
      end)

    {erased_list, bindings_list}
  end

  def check_all!(exprs, expected_type, bindings, ctx) do
    # Homogeneous mode: check all exprs against the same type
    {erased_list, bindings_list} =
      Enum.reduce(exprs, {[], bindings}, fn expr, {acc_erased, acc_bindings} ->
        {erased, _type, new_bindings} = check!(expr, expected_type, acc_bindings, ctx)
        {acc_erased ++ [erased], acc_bindings ++ new_bindings}
      end)

    {erased_list, bindings_list}
  end
end
