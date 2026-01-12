defmodule Deft.Error.Formatter do
  @moduledoc """
  Pretty-prints Deft type errors in a compiler-quality format.

  This module formats errors similar to Rust's error messages, with:
  - Error code and severity
  - Source location with file, line, and column
  - Highlighted source context
  - Pointing arrows to the error location
  - Suggestions for fixes
  - Additional notes

  ## Example Output

      error[E0001]: type mismatch
        --> lib/my_app.ex:15:10
         |
      14 |   add = fn x :: integer, y :: integer ->
      15 |     x + y + 1.5
         |             ^^^ expected `integer`, found `float`
         |
         = note: in expression `x + y + 1.5`
         = note: `+` with integer arguments returns integer
         = help: consider using `trunc(1.5)` or change return type to `number`
  """

  alias Deft.Error
  alias Deft.Context

  @type format_options :: [
          colors: boolean(),
          context_lines: non_neg_integer(),
          source_lines: [String.t()] | nil
        ]

  @default_options [
    colors: true,
    context_lines: 2
  ]

  # ANSI color codes
  @colors %{
    error: IO.ANSI.red(),
    warning: IO.ANSI.yellow(),
    note: IO.ANSI.cyan(),
    help: IO.ANSI.green(),
    bold: IO.ANSI.bright(),
    reset: IO.ANSI.reset(),
    dim: IO.ANSI.faint(),
    underline: IO.ANSI.underline()
  }

  @doc """
  Formats a single error for display.
  """
  @spec format(Error.t(), format_options()) :: String.t()
  def format(%Error{} = error, opts \\ []) do
    opts = Keyword.merge(@default_options, opts)
    use_colors = Keyword.get(opts, :colors, true) and IO.ANSI.enabled?()
    source_lines = Keyword.get(opts, :source_lines)

    [
      format_header(error, use_colors),
      format_location(error, use_colors),
      format_source_context(error, source_lines, opts, use_colors),
      format_notes(error, use_colors),
      format_suggestions(error, use_colors)
    ]
    |> Enum.reject(&is_nil/1)
    |> Enum.join("\n")
  end

  @doc """
  Formats multiple errors for display.
  """
  @spec format_all([Error.t()], format_options()) :: String.t()
  def format_all(errors, opts \\ []) when is_list(errors) do
    errors
    |> Enum.map(&format(&1, opts))
    |> Enum.join("\n\n")
    |> then(fn formatted ->
      count = length(errors)

      if count > 0 do
        summary = format_summary(count, opts)
        formatted <> "\n\n" <> summary
      else
        formatted
      end
    end)
  end

  @doc """
  Formats errors from a context.
  """
  @spec format_context_errors(Context.t(), format_options()) :: String.t()
  def format_context_errors(%Context{} = ctx, opts \\ []) do
    format_all(Context.get_errors(ctx), opts)
  end

  # ============================================================================
  # Header Formatting
  # ============================================================================

  defp format_header(%Error{code: code, message: message, severity: severity}, use_colors) do
    code_str = Error.error_code_string(code)
    severity_str = Atom.to_string(severity)

    if use_colors do
      color = severity_color(severity)
      "#{color}#{@colors.bold}#{severity_str}[#{code_str}]#{@colors.reset}: #{message}"
    else
      "#{severity_str}[#{code_str}]: #{message}"
    end
  end

  # ============================================================================
  # Location Formatting
  # ============================================================================

  defp format_location(%Error{location: nil}, _use_colors), do: nil

  defp format_location(%Error{location: {file, line, column}}, use_colors) do
    location_str = format_location_string(file, line, column)

    if use_colors do
      "  #{@colors.dim}-->#{@colors.reset} #{location_str}"
    else
      "  --> #{location_str}"
    end
  end

  defp format_location_string(nil, line, nil), do: "line #{line}"
  defp format_location_string(nil, line, col), do: "line #{line}:#{col}"
  defp format_location_string(file, line, nil), do: "#{file}:#{line}"
  defp format_location_string(file, line, col), do: "#{file}:#{line}:#{col}"

  # ============================================================================
  # Source Context Formatting
  # ============================================================================

  defp format_source_context(%Error{location: nil}, _source_lines, _opts, _use_colors), do: nil

  defp format_source_context(
         %Error{location: {_file, line, column}, expression: expr} = error,
         source_lines,
         _opts,
         use_colors
       ) do
    # Try to get source line from provided source_lines list, or fall back to expression
    source_line = get_source_line(source_lines, line) || get_source_line_from_expr(expr)

    if source_line do
      format_source_with_pointer(line, column, source_line, error, use_colors)
    else
      format_expression_context(expr, error, use_colors)
    end
  end

  defp format_source_context(_error, _source_lines, _opts, _use_colors), do: nil

  defp format_source_with_pointer(line, column, source_line, error, use_colors) do
    line_num_width = String.length(Integer.to_string(line))
    padding = String.duplicate(" ", line_num_width)

    # Format the separator line
    separator =
      if use_colors do
        "#{padding} #{@colors.dim}|#{@colors.reset}"
      else
        "#{padding} |"
      end

    # Format the source line with line number
    source =
      if use_colors do
        "#{@colors.dim}#{line}#{@colors.reset} #{@colors.dim}|#{@colors.reset} #{source_line}"
      else
        "#{line} | #{source_line}"
      end

    # Format the pointer line
    col = column || 1
    pointer_padding = String.duplicate(" ", max(0, col - 1))

    # Determine pointer width based on expression
    pointer_width = get_expression_width(error.expression) || 1
    pointer = String.duplicate("^", pointer_width)

    pointer_line =
      if use_colors do
        error_color = severity_color(error.severity)

        "#{padding} #{@colors.dim}|#{@colors.reset} #{pointer_padding}#{error_color}#{pointer}#{@colors.reset} #{format_pointer_message(error, use_colors)}"
      else
        "#{padding} | #{pointer_padding}#{pointer} #{format_pointer_message(error, false)}"
      end

    [separator, source, pointer_line, separator]
    |> Enum.join("\n")
  end

  defp format_expression_context(nil, _error, _use_colors), do: nil

  defp format_expression_context(expr, _error, use_colors) do
    expr_str =
      try do
        Macro.to_string(expr)
      rescue
        _ -> inspect(expr)
      end

    # Truncate very long expressions
    expr_str =
      if String.length(expr_str) > 60 do
        String.slice(expr_str, 0, 57) <> "..."
      else
        expr_str
      end

    if use_colors do
      "     #{@colors.dim}|#{@colors.reset}\n     #{@colors.dim}|#{@colors.reset} in: #{expr_str}\n     #{@colors.dim}|#{@colors.reset}"
    else
      "     |\n     | in: #{expr_str}\n     |"
    end
  end

  defp format_pointer_message(%Error{expected: expected, actual: actual}, use_colors)
       when not is_nil(expected) and not is_nil(actual) do
    expected_str = Error.format_type(expected)
    actual_str = Error.format_type(actual)

    if use_colors do
      "expected `#{@colors.bold}#{expected_str}#{@colors.reset}`, found `#{@colors.bold}#{actual_str}#{@colors.reset}`"
    else
      "expected `#{expected_str}`, found `#{actual_str}`"
    end
  end

  defp format_pointer_message(_error, _use_colors), do: ""

  # ============================================================================
  # Notes and Suggestions Formatting
  # ============================================================================

  defp format_notes(%Error{notes: []}, _use_colors), do: nil

  defp format_notes(%Error{notes: notes}, use_colors) do
    notes
    |> Enum.map(&format_note(&1, use_colors))
    |> Enum.join("\n")
  end

  defp format_note(note, use_colors) do
    if use_colors do
      "     #{@colors.note}= note#{@colors.reset}: #{note}"
    else
      "     = note: #{note}"
    end
  end

  defp format_suggestions(%Error{suggestions: []}, _use_colors), do: nil

  defp format_suggestions(%Error{suggestions: suggestions}, use_colors) do
    suggestions
    |> Enum.map(&format_suggestion(&1, use_colors))
    |> Enum.join("\n")
  end

  defp format_suggestion(suggestion, use_colors) do
    if use_colors do
      "     #{@colors.help}= help#{@colors.reset}: #{suggestion}"
    else
      "     = help: #{suggestion}"
    end
  end

  # ============================================================================
  # Summary Formatting
  # ============================================================================

  defp format_summary(count, opts) do
    use_colors = Keyword.get(opts, :colors, true) and IO.ANSI.enabled?()
    plural = if count == 1, do: "error", else: "errors"

    if use_colors do
      "#{@colors.error}#{@colors.bold}error#{@colors.reset}: could not compile due to #{count} previous #{plural}"
    else
      "error: could not compile due to #{count} previous #{plural}"
    end
  end

  # ============================================================================
  # Helper Functions
  # ============================================================================

  defp severity_color(:error), do: @colors.error
  defp severity_color(:warning), do: @colors.warning

  # Get source line from provided source_lines list (1-indexed line numbers)
  defp get_source_line(source_lines, line) when is_list(source_lines) and is_integer(line) do
    Enum.at(source_lines, line - 1)
  end

  defp get_source_line(_, _), do: nil

  defp get_source_line_from_expr({_, meta, _}) when is_list(meta) do
    # If the AST has source attached (from Code.string_to_quoted)
    nil
  end

  defp get_source_line_from_expr(_), do: nil

  defp get_expression_width(nil), do: nil

  defp get_expression_width(expr) do
    # First try to get width from metadata (most accurate)
    case get_width_from_meta(expr) do
      width when is_integer(width) and width > 0 ->
        width

      _ ->
        # Fall back to stringifying the expression
        str =
          try do
            expr
            |> to_raw_ast()
            |> Macro.to_string()
          rescue
            _ -> nil
          end

        if str do
          # Just use the first line if multi-line
          str
          |> String.split("\n")
          |> List.first()
          |> String.length()
          |> min(40)
        else
          nil
        end
    end
  end

  # Try to get width from expression metadata (column and end_column)
  defp get_width_from_meta(%{meta: meta}) when is_list(meta) do
    with col when is_integer(col) <- Keyword.get(meta, :column),
         end_col when is_integer(end_col) <- Keyword.get(meta, :end_column) do
      end_col - col
    else
      _ -> nil
    end
  end

  defp get_width_from_meta({_, meta, _}) when is_list(meta) do
    with col when is_integer(col) <- Keyword.get(meta, :column),
         end_col when is_integer(end_col) <- Keyword.get(meta, :end_column) do
      end_col - col
    else
      _ -> nil
    end
  end

  defp get_width_from_meta(_), do: nil

  # Convert Deft AST structs to raw Elixir AST for Macro.to_string
  defp to_raw_ast(%_mod{} = struct) do
    if function_exported?(Deft.AST, :to_raw_ast, 1) do
      Deft.AST.to_raw_ast(struct)
    else
      # Fallback: try common struct fields
      cond do
        Map.has_key?(struct, :name) and Map.has_key?(struct, :meta) ->
          {struct.name, struct.meta, nil}

        true ->
          struct
      end
    end
  end

  defp to_raw_ast(ast), do: ast
end
