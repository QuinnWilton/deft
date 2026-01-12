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
      format_source_context(error, source_lines, use_colors),
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
    formatted_message = bold_backtick_content(message, use_colors)

    if use_colors do
      color = severity_color(severity)
      "#{color}#{@colors.bold}#{severity_str}[#{code_str}]#{@colors.reset}: #{formatted_message}"
    else
      "#{severity_str}[#{code_str}]: #{formatted_message}"
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

  defp format_source_context(%Error{location: nil, spans: []}, _source_lines, _use_colors),
    do: nil

  # Multi-span display for errors with labeled spans
  defp format_source_context(%Error{spans: spans} = error, source_lines, use_colors)
       when spans != [] do
    format_multi_span_context(spans, source_lines, error, use_colors)
  end

  defp format_source_context(
         %Error{location: {_file, line, column} = location, expression: expr} = error,
         source_lines,
         use_colors
       ) do
    # Try to get source line from provided source_lines or read from file.
    source_line = get_source_line_from_location(source_lines, location)

    if source_line do
      format_source_with_pointer(line, column, source_line, error, use_colors)
    else
      format_expression_context(expr, error, use_colors)
    end
  end

  defp format_source_context(_error, _source_lines, _use_colors), do: nil

  # Format multiple labeled spans in miette style
  defp format_multi_span_context(spans, source_lines, error, use_colors) do
    # Sort spans by line number
    sorted_spans =
      spans
      |> Enum.filter(fn %{location: {_, line, _}} -> is_integer(line) end)
      |> Enum.sort_by(fn %{location: {_, line, _}} -> line end)

    if Enum.empty?(sorted_spans) do
      nil
    else
      # Calculate the max line number width for consistent formatting
      max_line =
        sorted_spans
        |> Enum.map(fn %{location: {_, line, _}} -> line end)
        |> Enum.max()

      line_num_width = String.length(Integer.to_string(max_line))
      padding = String.duplicate(" ", line_num_width)

      # Format each span with line numbers for gap detection
      spans_with_lines =
        sorted_spans
        |> Enum.map(fn %{location: {_, line, _}} = span ->
          {line, format_labeled_span(span, source_lines, line_num_width, error, use_colors)}
        end)
        |> Enum.reject(fn {_, block} -> is_nil(block) end)

      # Join all spans with separator lines, adding elision markers for gaps
      if Enum.empty?(spans_with_lines) do
        nil
      else
        separator =
          if use_colors do
            "#{padding} #{@colors.dim}|#{@colors.reset}"
          else
            "#{padding} |"
          end

        elision =
          if use_colors do
            "#{@colors.dim}...#{@colors.reset}"
          else
            "..."
          end

        # Build output with elision markers between non-consecutive lines
        {result, _} =
          spans_with_lines
          |> Enum.reduce({[], nil}, fn {line, block}, {acc, prev_line} ->
            # Add separator or elision marker between spans
            between =
              cond do
                # First span - just add opening separator
                prev_line == nil -> [separator]
                # Gap of more than 1 line - show elision
                line > prev_line + 1 -> [elision]
                # Consecutive lines - normal separator
                true -> [separator]
              end

            {acc ++ between ++ List.flatten([block]), line}
          end)

        (result ++ [separator])
        |> Enum.join("\n")
      end
    end
  end

  # Format a single labeled span
  defp format_labeled_span(
         %{location: {_file, line, column} = location, label: label, type: type},
         source_lines,
         line_num_width,
         error,
         use_colors
       ) do
    source_line = get_source_line_from_location(source_lines, location)

    if source_line do
      padding = String.duplicate(" ", line_num_width)

      # Format the source line with line number
      source =
        if use_colors do
          line_str = String.pad_leading(Integer.to_string(line), line_num_width)
          "#{@colors.dim}#{line_str}#{@colors.reset} #{@colors.dim}|#{@colors.reset} #{source_line}"
        else
          line_str = String.pad_leading(Integer.to_string(line), line_num_width)
          "#{line_str} | #{source_line}"
        end

      # Format the pointer line with label
      col = column || 1
      pointer_padding = String.duplicate(" ", max(0, col - 1))
      pointer_width = estimate_span_width(source_line, col, type)
      pointer = String.duplicate("^", pointer_width)

      # Build the label with type if present (bold type when colors enabled)
      type_str =
        if type do
          formatted = Error.format_type(type)

          if use_colors do
            " `#{@colors.bold}#{formatted}#{@colors.reset}`"
          else
            " `#{formatted}`"
          end
        else
          ""
        end

      full_label = "#{label}#{type_str}"

      pointer_line =
        if use_colors do
          error_color = severity_color(error.severity)

          "#{padding} #{@colors.dim}|#{@colors.reset} #{pointer_padding}#{error_color}#{pointer}#{@colors.reset} #{full_label}"
        else
          "#{padding} | #{pointer_padding}#{pointer} #{full_label}"
        end

      [source, pointer_line]
    else
      nil
    end
  end

  # Estimate the width of a span based on context.
  # For expressions, we try to highlight meaningful content.
  defp estimate_span_width(source_line, column, _type) do
    # Get the content from the column position to end of line
    remaining = String.slice(source_line, max(0, column - 1)..-1//1)

    # For body expressions, try to highlight up to a common statement terminator
    # or the entire remaining non-whitespace content
    trimmed = String.trim_trailing(remaining)

    # If this looks like an expression (not a definition), highlight the whole thing
    width =
      cond do
        # If line ends with "do" or contains "->", it's likely a declaration
        String.ends_with?(trimmed, " do") or String.contains?(trimmed, "->") ->
          # Just highlight the first token
          case Regex.run(~r/^(\S+)/, remaining) do
            [_, match] -> String.length(match)
            _ -> 1
          end

        # For regular expressions, highlight the trimmed content
        String.length(trimmed) > 0 ->
          String.length(trimmed)

        # Fallback to single token
        true ->
          case Regex.run(~r/^(\S+)/, remaining) do
            [_, match] -> String.length(match)
            _ -> 1
          end
      end

    min(max(width, 1), 50)
  end

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
    # Convert Deft AST structs to raw Elixir AST first
    raw_ast = to_raw_ast(expr)

    expr_str =
      try do
        Macro.to_string(raw_ast)
      rescue
        _ ->
          # Last resort: show a simplified representation
          format_expr_fallback(expr)
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

  # Fallback formatting for expressions that can't be converted
  defp format_expr_fallback(%{__struct__: mod, name: name}) when is_atom(name) do
    mod_name = mod |> Module.split() |> List.last()
    "#{mod_name}(#{name})"
  end

  defp format_expr_fallback(%{__struct__: mod}) do
    mod_name = mod |> Module.split() |> List.last()
    "#{mod_name}(...)"
  end

  defp format_expr_fallback(expr), do: inspect(expr, limit: 3)

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
    formatted_note = bold_backtick_content(note, use_colors)

    if use_colors do
      "     #{@colors.note}= note#{@colors.reset}: #{formatted_note}"
    else
      "     = note: #{formatted_note}"
    end
  end

  defp format_suggestions(%Error{suggestions: []}, _use_colors), do: nil

  defp format_suggestions(%Error{suggestions: suggestions}, use_colors) do
    suggestions
    |> Enum.map(&format_suggestion(&1, use_colors))
    |> Enum.join("\n")
  end

  defp format_suggestion(suggestion, use_colors) do
    formatted_suggestion = bold_backtick_content(suggestion, use_colors)

    if use_colors do
      "     #{@colors.help}= help#{@colors.reset}: #{formatted_suggestion}"
    else
      "     = help: #{formatted_suggestion}"
    end
  end

  # Bold content within backticks when colors are enabled
  defp bold_backtick_content(text, false), do: text

  defp bold_backtick_content(text, true) do
    Regex.replace(~r/`([^`]+)`/, text, "`#{@colors.bold}\\1#{@colors.reset}`")
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

  # Gets a source line from the provided source_lines list.
  # Lines are 1-indexed in error locations.
  defp get_source_line(source_lines, line)
       when is_list(source_lines) and is_integer(line) and line > 0 do
    Enum.at(source_lines, line - 1)
  end

  defp get_source_line(_, _), do: nil

  @doc """
  Reads source lines from a file.

  Returns a list of lines or nil if the file cannot be read.
  """
  @spec read_source_file(String.t()) :: [String.t()] | nil
  def read_source_file(file_path) when is_binary(file_path) do
    case File.read(file_path) do
      {:ok, content} -> String.split(content, "\n")
      {:error, _} -> nil
    end
  end

  def read_source_file(_), do: nil

  # Get source line, reading from file if needed
  defp get_source_line_from_location(source_lines, {file, line, _column})
       when is_binary(file) and is_integer(line) and line > 0 do
    case get_source_line(source_lines, line) do
      nil ->
        # Try to read from file
        case read_source_file(file) do
          nil -> nil
          lines -> Enum.at(lines, line - 1)
        end

      line_content ->
        line_content
    end
  end

  defp get_source_line_from_location(source_lines, {_file, line, _column}) do
    get_source_line(source_lines, line)
  end

  defp get_source_line_from_location(_, _), do: nil

  defp get_expression_width(nil), do: nil

  defp get_expression_width(expr) do
    # Try to use column metadata for accurate width.
    width_from_meta = get_width_from_meta(expr)

    if width_from_meta do
      width_from_meta
    else
      # Convert to raw AST if it's a Deft AST struct, then use Macro.to_string.
      raw_ast = to_raw_ast(expr)

      str =
        try do
          Macro.to_string(raw_ast)
        rescue
          _ -> nil
        end

      if str do
        # Just use the first line if multi-line.
        str
        |> String.split("\n")
        |> List.first()
        |> String.length()
        |> min(30)
        |> max(1)
      else
        1
      end
    end
  end

  # Convert Deft AST structs to raw Elixir AST for Macro.to_string.
  defp to_raw_ast(%{__struct__: _mod, meta: _meta} = ast) do
    # This looks like a Deft AST struct, try to convert it.
    try do
      Deft.AST.to_raw_ast(ast)
    rescue
      _ -> ast
    end
  end

  defp to_raw_ast(ast), do: ast

  # Try to get width from AST metadata if available.
  defp get_width_from_meta(%{meta: meta}) when is_list(meta) do
    get_width_from_meta_list(meta)
  end

  defp get_width_from_meta({_, meta, _}) when is_list(meta) do
    get_width_from_meta_list(meta)
  end

  defp get_width_from_meta(_), do: nil

  defp get_width_from_meta_list(meta) do
    start_col = Keyword.get(meta, :column)
    end_col = Keyword.get(meta, :end_column)

    if start_col && end_col && end_col > start_col do
      end_col - start_col
    else
      nil
    end
  end
end
