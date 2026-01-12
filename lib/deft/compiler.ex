defmodule Deft.Compiler do
  @moduledoc """
  Compiles Elixir AST to Deft AST nodes.

  This module transforms standard Elixir AST (as produced by `quote`) into
  Deft's internal AST representation for type checking.

  Raises `CompileError` if the input contains unsupported syntax.
  """

  alias Deft.AST
  alias Deft.Annotations
  alias Deft.Error

  def compile({:__block__, meta, exprs}) do
    exprs = Enum.map(exprs, &compile/1)

    AST.Block.new(exprs, meta)
  end

  def compile({:defdata, def_meta, [{:"::", variants_meta, [{name, name_meta, ctx}, variants]}]}) do
    name = AST.Local.new(name, ctx, name_meta)
    variants = compile_adt_variants(variants, name)

    AST.DefData.new(name, variants, def_meta, variants_meta)
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

    branches =
      Keyword.new(branches, fn {key, node} ->
        {key, compile(node)}
      end)

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

  # Remote function calls: Module.function(args) with aliased module
  def compile({{:., _, [{:__aliases__, _, module_parts}, function]}, meta, args})
      when is_atom(function) do
    module = Module.concat(module_parts)
    args = Enum.map(args, &compile/1)

    AST.RemoteCall.new(module, function, args, meta)
  end

  # Remote function calls: Module.function(args) with atom module
  def compile({{:., _, [module, function]}, meta, args})
      when is_atom(module) and is_atom(function) do
    args = Enum.map(args, &compile/1)

    AST.RemoteCall.new(module, function, args, meta)
  end

  # Remote function capture: &Module.function/arity (aliased module)
  def compile(
        {:&, meta,
         [{:/, _, [{{:., _, [{:__aliases__, _, module_parts}, function]}, _, _}, arity]}]}
      )
      when is_atom(function) and is_integer(arity) do
    module = Module.concat(module_parts)
    AST.Capture.new(module, function, arity, meta)
  end

  # Remote function capture: &Module.function/arity (atom module)
  def compile({:&, meta, [{:/, _, [{{:., _, [module, function]}, _, _}, arity]}]})
      when is_atom(module) and is_atom(function) and is_integer(arity) do
    AST.Capture.new(module, function, arity, meta)
  end

  # Local function capture: &function/arity
  def compile({:&, meta, [{:/, _, [{function, _, context}, arity]}]})
      when is_atom(function) and is_atom(context) and is_integer(arity) do
    AST.Capture.new(nil, function, arity, meta)
  end

  # ============================================================================
  # Unsupported Syntax (explicit rejections before generic patterns)
  # ============================================================================

  # Map literals
  def compile({:%{}, _, _} = ast) do
    raise_unsupported_syntax(ast)
  end

  # Struct literals
  def compile({:%, _, _} = ast) do
    raise_unsupported_syntax(ast)
  end

  # Comprehensions
  def compile({:for, _, _} = ast) do
    raise_unsupported_syntax(ast)
  end

  # With expressions
  def compile({:with, _, _} = ast) do
    raise_unsupported_syntax(ast)
  end

  # Receive
  def compile({:receive, _, _} = ast) do
    raise_unsupported_syntax(ast)
  end

  # Try/rescue/catch
  def compile({:try, _, _} = ast) do
    raise_unsupported_syntax(ast)
  end

  # Raise
  def compile({:raise, _, _} = ast) do
    raise_unsupported_syntax(ast)
  end

  # Throw
  def compile({:throw, _, _} = ast) do
    raise_unsupported_syntax(ast)
  end

  # Import
  def compile({:import, _, _} = ast) do
    raise_unsupported_syntax(ast)
  end

  # Require
  def compile({:require, _, _} = ast) do
    raise_unsupported_syntax(ast)
  end

  # Alias
  def compile({:alias, _, _} = ast) do
    raise_unsupported_syntax(ast)
  end

  # Binary/bitstring syntax
  def compile({:<<>>, _, _} = ast) do
    raise_unsupported_syntax(ast)
  end

  # ============================================================================
  # Generic patterns (after specific rejections)
  # ============================================================================

  def compile({name, meta, args}) when is_list(args) do
    args = Enum.map(args, &compile/1)

    AST.LocalCall.new(name, args, meta)
  end

  def compile({name, meta, context}) when is_atom(context) do
    AST.Local.new(name, context, meta)
  end

  def compile(literal)
      when is_atom(literal)
      when is_binary(literal)
      when is_boolean(literal)
      when is_float(literal)
      when is_integer(literal)
      when is_number(literal) do
    AST.Literal.new(literal)
  end

  # Catch-all for unsupported syntax.
  def compile(ast) do
    {notes, suggestions} = unsupported_syntax_hints(ast)

    error =
      Error.unsupported_syntax(
        expression: ast,
        kind: "expression",
        location: Error.extract_location(ast),
        suggestions: suggestions,
        notes: notes
      )

    Error.raise!(error)
  end

  def compile_fn_arg({:"::", meta, [pattern, annotation]}) do
    pattern = compile_pattern(pattern)
    type = Annotations.parse(annotation)

    AST.Annotation.new(pattern, type, meta)
  end

  def compile_pattern(literal)
      when is_atom(literal)
      when is_binary(literal)
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

  # ============================================================================
  # Unsupported Pattern Syntax (explicit rejections before generic patterns)
  # ============================================================================

  # Map patterns
  def compile_pattern({:%{}, _, _} = ast) do
    raise_unsupported_pattern(ast)
  end

  # Struct patterns
  def compile_pattern({:%, _, _} = ast) do
    raise_unsupported_pattern(ast)
  end

  # Binary/bitstring patterns
  def compile_pattern({:<<>>, _, _} = ast) do
    raise_unsupported_pattern(ast)
  end

  # ============================================================================
  # Generic pattern clauses (after specific rejections)
  # ============================================================================

  def compile_pattern({name, meta, context}) when is_atom(context) do
    AST.Local.new(name, context, meta)
  end

  def compile_pattern({name, meta, args}) when is_list(args) do
    # TODO: Risky to allow LocalCall nodes in patterns, even though
    #       they should get rewritten in the case of type constructors.
    args = Enum.map(args, &compile_pattern/1)

    AST.LocalCall.new(name, args, meta)
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

  # Catch-all for unsupported patterns.
  def compile_pattern(ast) do
    {notes, suggestions} = unsupported_pattern_hints(ast)

    error =
      Error.unsupported_pattern(
        expression: ast,
        location: Error.extract_location(ast),
        suggestions: suggestions,
        notes: notes
      )

    Error.raise!(error)
  end

  def compile_adt_variant({name, meta, columns}, adt_name) do
    columns = parse_adt_columns(columns, name, adt_name)

    AST.Variant.new(name, adt_name, columns, meta)
  end

  def compile_adt_variants({:|, _, [first, rest]}, adt_name) do
    first = compile_adt_variant(first, adt_name)
    rest = compile_adt_variants(rest, adt_name)

    [first | rest]
  end

  def compile_adt_variants({name, meta, columns}, adt_name) do
    columns = parse_adt_columns(columns, name, adt_name)
    variant = AST.Variant.new(name, adt_name, columns, meta)

    [variant]
  end

  defp parse_adt_columns(columns, _variant_name, _adt_name) do
    Enum.map(columns, &Annotations.parse/1)
  end

  # ============================================================================
  # Error Helpers
  # ============================================================================

  defp raise_unsupported_syntax(ast) do
    {notes, suggestions} = unsupported_syntax_hints(ast)

    error =
      Error.unsupported_syntax(
        expression: ast,
        kind: "expression",
        location: Error.extract_location(ast),
        suggestions: suggestions,
        notes: notes
      )

    Error.raise!(error)
  end

  defp raise_unsupported_pattern(ast) do
    {notes, suggestions} = unsupported_pattern_hints(ast)

    error =
      Error.unsupported_pattern(
        expression: ast,
        location: Error.extract_location(ast),
        suggestions: suggestions,
        notes: notes
      )

    Error.raise!(error)
  end

  # ============================================================================
  # Error Hints
  # ============================================================================

  defp unsupported_syntax_hints(ast) do
    case ast do
      {:%{}, _, _} ->
        {["Map literals are not supported in Deft."],
         ["Consider using a tuple or defining an ADT instead."]}

      {:%, _, _} ->
        {["Struct literals are not supported in Deft."],
         ["Consider defining an ADT with defdata instead."]}

      {:for, _, _} ->
        {["Comprehensions are not supported in Deft."],
         ["Use Enum.map/2 or Enum.filter/2 instead."]}

      {:with, _, _} ->
        {["The `with` construct is not supported in Deft."],
         ["Use nested case expressions instead."]}

      {:receive, _, _} ->
        {["The `receive` construct is not supported in Deft."], []}

      {:try, _, _} ->
        {["The `try` construct is not supported in Deft."], []}

      {:raise, _, _} ->
        {["The `raise` construct is not supported in Deft."], []}

      {:throw, _, _} ->
        {["The `throw` construct is not supported in Deft."], []}

      {:import, _, _} ->
        {["The `import` directive is not supported inside Deft blocks."],
         ["Move imports outside the deft block."]}

      {:require, _, _} ->
        {["The `require` directive is not supported inside Deft blocks."],
         ["Move requires outside the deft block."]}

      {:alias, _, _} ->
        {["The `alias` directive is not supported inside Deft blocks."],
         ["Move aliases outside the deft block."]}

      {op, _, _} when op in [:<<>>, :"::", :".."] ->
        {["Binary/bitstring syntax is not supported in Deft."], []}

      _ ->
        {["This syntax is not recognized by the Deft compiler."],
         ["Deft supports: literals, variables, tuples, lists, case, cond, if, fn, and function calls."]}
    end
  end

  defp unsupported_pattern_hints(ast) do
    case ast do
      {:%{}, _, _} ->
        {["Map patterns are not supported in Deft."],
         ["Consider using tuple patterns or ADT matching instead."]}

      {:%, _, _} ->
        {["Struct patterns are not supported in Deft."],
         ["Define an ADT with defdata and pattern match on its variants."]}

      {op, _, _} when op in [:<<>>, :"::", :".."] ->
        {["Binary/bitstring patterns are not supported in Deft."], []}

      _ ->
        {["This pattern is not recognized by the Deft compiler."],
         ["Deft supports: literals, variables, tuples, lists, cons ([h|t]), pins (^x), and ADT constructors."]}
    end
  end
end
