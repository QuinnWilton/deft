defmodule Deft.Rules.DSL.Operators do
  @moduledoc """
  Custom operators for the declarative typing rule DSL.

  ## Synthesis Operators

  - `~>` - Synthesis judgment: `expr ~> {erased, type}`
  - `~>>` - Synth all: `exprs ~>> {erased_list, types_list}`

  ## Checking Operators

  - `<~` - Checking judgment: `expr <~ expected >>> erased` or `expr <~ expected >>> {erased, type}`
  - `<<~` - Check all: `exprs <<~ type(s)` (homogeneous if single type, heterogeneous if list)

  ## Other Operators

  - `+++` - Context extension: `bindings +++ premise`
  - `&&&` - Scoped context: `(exprs &&& [k: v]) ~>> {erased, a, b}`
  - `<~>` - Pattern judgment: `pattern <~> type >>> {erased, type, bindings}`
  - `>>>` - Elaborates to (binds result of `<~`, `<~>`)
  """

  # These are just markers - the actual transformation happens in the DSL macro
  # We define them here so they're valid syntax when imported

  @doc "Synthesis: expr ~> {erased, type}"
  defmacro left ~> right do
    quote do: {:synth, unquote(left), unquote(right)}
  end

  @doc """
  Checking judgment: checks expression against expected type.

  Must be followed by `>>>` to bind the result:

      expr <~ expected_type >>> erased
      expr <~ expected_type >>> {erased, actual_type}
  """
  defmacro left <~ right do
    quote do: {:check, unquote(left), unquote(right)}
  end

  @doc "Elaborates to: binds result of `<~` or `<~>` judgment"
  defmacro left >>> right do
    quote do: {:elaborate, unquote(left), unquote(right)}
  end

  @doc "Context extension: bindings +++ premise"
  defmacro left +++ right do
    quote do: {:extend_ctx, unquote(left), unquote(right)}
  end

  @doc "Synth all: exprs ~>> {erased_list, types_list}"
  defmacro left ~>> right do
    quote do: {:synth_all, unquote(left), unquote(right)}
  end

  @doc """
  Check all: checks list of expressions against expected type(s).

  Two modes based on the right-hand side:
  - **Homogeneous** (single type): `exprs <<~ type` - all expressions checked against same type
  - **Heterogeneous** (list of types): `exprs <<~ types` - each expression checked against corresponding type

  Must be followed by `>>>` to bind the result:

      # Homogeneous: check all list elements are integers
      elements <<~ Type.integer() >>> elements_e

      # Heterogeneous: check function args against parameter types
      args <<~ input_types >>> args_e
  """
  defmacro left <<~ right do
    quote do: {:check_all, unquote(left), unquote(right)}
  end

  @doc """
  Scoped context: adds scoped attributes for child rules.

  Used in conjunction with ~>> to pass context from parent rules to child rules.
  For example, a :case rule can pass subj_t to :case_branch rules:

      (branches &&& [subj_t: subj_t]) ~>> {branches_e, pat_ts, body_ts}

  Note: Due to operator precedence, parentheses are required around `exprs &&& [...]`.
  """
  defmacro left &&& right do
    quote do: {:scoped_ctx, unquote(left), unquote(right)}
  end

  @doc """
  Pattern judgment: pattern <~> expected_type checks pattern against type.

  Returns erased pattern, pattern type, and bindings produced by pattern matching.
  Must be followed by >>> to bind results:

      pattern <~> subject_type >>> {erased_pattern, pattern_type, pattern_bindings}
  """
  defmacro left <~> right do
    quote do: {:pattern_judgment, unquote(left), unquote(right)}
  end
end
