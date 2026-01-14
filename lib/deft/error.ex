defmodule Deft.Error do
  @moduledoc """
  Rich structured error type for Deft type checking.

  This module provides comprehensive error information including:
  - Error code for programmatic handling
  - Human-readable message
  - Expected and actual types
  - Source location (file, line, column)
  - Expression context
  - Suggestions for fixes
  - Additional notes

  ## Error Codes

  Error codes follow the pattern `E0XXX`:
  - `E0001` - Type mismatch (expected vs actual)
  - `E0002` - Missing type annotation
  - `E0003` - Malformed type expression
  - `E0004` - Unsupported local call
  - `E0005` - Inexhaustive patterns
  - `E0006` - Unreachable branch
  - `E0007` - No matching rule
  - `E0008` - Missing features
  - `E0009` - Subtype constraint violated

  ## Usage

      error = Deft.Error.type_mismatch(
        expected: Type.integer(),
        actual: Type.float(),
        location: {file, line, column},
        expression: expr
      )
  """

  alias Deft.Span
  alias Deft.Type

  @type error_code ::
          :type_mismatch
          | :missing_annotation
          | :malformed_type
          | :unknown_type_alias
          | :unsupported_call
          | :inexhaustive_patterns
          | :unreachable_branch
          | :no_matching_rule
          | :missing_features
          | :subtype_violation
          | :unsupported_syntax
          | :unsupported_pattern
          | :unsupported_function
          | :conflicting_definition

  @type location :: {String.t() | nil, non_neg_integer() | nil, non_neg_integer() | nil}

  @typedoc """
  A labeled span for multi-span error display.

  Contains a location tuple, a label describing what this span represents,
  optionally a type to display, and a kind indicating primary (error) or
  secondary (context) highlighting.

  - `:primary` spans are highlighted in red (the actual error)
  - `:secondary` spans are highlighted in yellow (additional context)
  """
  @type labeled_span :: %{
          required(:location) => location(),
          required(:label) => String.t(),
          optional(:type) => Type.t() | nil,
          optional(:kind) => :primary | :secondary
        }

  @type t :: %__MODULE__{
          code: error_code(),
          message: String.t(),
          expected: Type.t() | nil,
          actual: Type.t() | nil,
          location: location() | nil,
          expression: term(),
          context_before: String.t() | nil,
          context_after: String.t() | nil,
          suggestions: [String.t()],
          notes: [String.t()],
          severity: :error | :warning,
          spans: [labeled_span()]
        }

  defstruct [
    :code,
    :message,
    :expected,
    :actual,
    :location,
    :expression,
    :context_before,
    :context_after,
    suggestions: [],
    notes: [],
    severity: :error,
    spans: []
  ]

  @error_codes %{
    type_mismatch: "E0001",
    missing_annotation: "E0002",
    malformed_type: "E0003",
    unknown_type_alias: "E0014",
    unsupported_call: "E0004",
    inexhaustive_patterns: "E0005",
    unreachable_branch: "E0006",
    no_matching_rule: "E0007",
    missing_features: "E0008",
    subtype_violation: "E0009",
    unsupported_syntax: "E0010",
    unsupported_pattern: "E0011",
    unsupported_function: "E0012",
    conflicting_definition: "E0013"
  }

  # ============================================================================
  # Error Constructors
  # ============================================================================

  @doc """
  Creates a type mismatch error.

  ## Options

  - `:expected` - The expected type (required)
  - `:actual` - The actual type found (required)
  - `:location` - Source location tuple `{file, line, column}`
  - `:expression` - The AST expression that caused the error
  - `:suggestions` - List of possible fixes
  - `:notes` - Additional context notes
  - `:spans` - List of labeled spans for multi-span display
  """
  @spec type_mismatch(keyword()) :: t()
  def type_mismatch(opts) do
    expected = Keyword.fetch!(opts, :expected)
    actual = Keyword.fetch!(opts, :actual)

    %__MODULE__{
      code: :type_mismatch,
      message:
        "Type mismatch: expected `#{format_type(expected)}`, found `#{format_type(actual)}`",
      expected: expected,
      actual: actual,
      location: Keyword.get(opts, :location),
      expression: Keyword.get(opts, :expression),
      suggestions: Keyword.get(opts, :suggestions, []),
      notes: Keyword.get(opts, :notes, []),
      spans: Keyword.get(opts, :spans, [])
    }
  end

  @doc """
  Creates a return type mismatch error with multi-span display.

  This error type shows both the declared return type location and the
  body expression that produces the incompatible type, making it clear
  where the mismatch occurs.

  ## Options

  - `:name` - Function name (required)
  - `:arity` - Function arity (required)
  - `:declared_type` - The declared return type (required)
  - `:actual_type` - The actual type of the body (required)
  - `:declaration_location` - Location of the return type annotation `{file, line, column}`
  - `:body_location` - Location of the body's final expression `{file, line, column}`
  """
  @spec return_type_mismatch(keyword()) :: t()
  def return_type_mismatch(opts) do
    name = Keyword.fetch!(opts, :name)
    arity = Keyword.fetch!(opts, :arity)
    declared_type = Keyword.fetch!(opts, :declared_type)
    actual_type = Keyword.fetch!(opts, :actual_type)
    declaration_location = Keyword.get(opts, :declaration_location)
    body_location = Keyword.get(opts, :body_location)

    spans =
      [
        if declaration_location do
          %{
            location: declaration_location,
            label: "declared return type",
            type: declared_type,
            kind: :secondary
          }
        end,
        if body_location do
          %{
            location: body_location,
            label: "body has type",
            type: actual_type,
            kind: :primary
          }
        end
      ]
      |> Enum.reject(&is_nil/1)

    %__MODULE__{
      code: :type_mismatch,
      message: "Return type mismatch in `#{name}/#{arity}`",
      expected: declared_type,
      actual: actual_type,
      location: declaration_location,
      spans: spans,
      suggestions: [
        "Change the return type annotation to `#{format_type(actual_type)}`",
        "Or modify the function body to return `#{format_type(declared_type)}`"
      ],
      notes: []
    }
  end

  @doc """
  Creates a missing annotation error.

  ## Options

  - `:expression` - The expression requiring annotation (required)
  - `:location` - Source location
  - `:notes` - Additional context notes
  - `:spans` - List of labeled spans for multi-span display
  """
  @spec missing_annotation(keyword()) :: t()
  def missing_annotation(opts) do
    expr = Keyword.fetch!(opts, :expression)

    %__MODULE__{
      code: :missing_annotation,
      message: "Missing type annotation on expression",
      expression: expr,
      location: Keyword.get(opts, :location),
      spans: Keyword.get(opts, :spans, []),
      suggestions: ["Add a type annotation using the `::` operator"],
      notes: Keyword.get(opts, :notes, [])
    }
  end

  @doc """
  Creates a malformed type error.

  ## Options

  - `:expression` - The malformed type expression (required)
  - `:location` - Source location
  - `:suggestions` - List of suggested fixes
  - `:notes` - Additional context notes
  - `:spans` - List of labeled spans for multi-span display
  """
  @spec malformed_type(keyword()) :: t()
  def malformed_type(opts) do
    expr = Keyword.fetch!(opts, :expression)

    %__MODULE__{
      code: :malformed_type,
      message: "Malformed type expression",
      expression: expr,
      location: Keyword.get(opts, :location),
      spans: Keyword.get(opts, :spans, []),
      suggestions: Keyword.get(opts, :suggestions, []),
      notes: Keyword.get(opts, :notes, [])
    }
  end

  @doc """
  Creates an unknown type alias error.

  Used when a type annotation references a type alias that cannot be resolved.

  ## Options

  - `:name` - The unresolved type alias name (required)
  - `:location` - Source location where the alias was referenced
  - `:similar` - A similar type name for "did you mean" suggestions
  - `:available` - List of available type names in scope
  - `:notes` - Additional context notes
  """
  @spec unknown_type_alias(keyword()) :: t()
  def unknown_type_alias(opts) do
    name = Keyword.fetch!(opts, :name)
    similar = Keyword.get(opts, :similar)
    available = Keyword.get(opts, :available, [])
    location = Keyword.get(opts, :location)

    suggestions = build_alias_suggestions(similar, available)

    # Build a span for the type name if we have location
    spans =
      if location do
        [
          %{
            location: location,
            label: "Unknown type `#{name}`",
            type: nil,
            kind: :primary,
            # Provide explicit length for the type name
            length: String.length(Atom.to_string(name))
          }
        ]
      else
        []
      end

    %__MODULE__{
      code: :unknown_type_alias,
      message: "Unknown type `#{name}`",
      location: location,
      spans: spans,
      suggestions: suggestions,
      notes: Keyword.get(opts, :notes, ["The type `#{name}` is not defined in this scope."])
    }
  end

  defp build_alias_suggestions(similar, available) do
    cond do
      similar ->
        ["Did you mean `#{similar}`?"]

      available != [] ->
        ["Available types: #{Enum.join(available, ", ")}"]

      true ->
        ["Define the type with `defdata` or check for typos."]
    end
  end

  @doc """
  Creates an unsupported local call error.

  ## Options

  - `:name` - The function name (required)
  - `:arity` - The function arity (required)
  - `:expression` - The call expression AST
  - `:location` - Source location
  - `:suggestions` - List of suggested fixes
  - `:notes` - Additional context notes
  - `:spans` - List of labeled spans for multi-span display
  """
  @spec unsupported_call(keyword()) :: t()
  def unsupported_call(opts) do
    name = Keyword.fetch!(opts, :name)
    arity = Keyword.fetch!(opts, :arity)

    %__MODULE__{
      code: :unsupported_call,
      message: "Call to unsupported function `#{format_call_name(name)}/#{arity}`",
      expression: Keyword.get(opts, :expression),
      location: Keyword.get(opts, :location),
      spans: Keyword.get(opts, :spans, []),
      suggestions:
        Keyword.get(opts, :suggestions, [
          "Register a type signature for this function",
          "Use a supported built-in function"
        ]),
      notes: Keyword.get(opts, :notes, [])
    }
  end

  # Format a function name for display, handling module tuples
  defp format_call_name({module, function}), do: "#{inspect(module)}.#{function}"
  defp format_call_name(name), do: "#{name}"

  @doc """
  Creates an inexhaustive patterns error.

  ## Options

  - `:missing` - The missing type(s) that need to be handled (required)
  - `:location` - Source location of the case expression
  - `:expression` - The case expression AST
  - `:covered` - List of types that are covered (for display)
  - `:spans` - List of labeled spans for multi-span display
  """
  @spec inexhaustive_patterns(keyword()) :: t()
  def inexhaustive_patterns(opts) do
    missing = Keyword.fetch!(opts, :missing)
    missing_list = List.wrap(missing)
    covered = Keyword.get(opts, :covered, [])

    notes =
      if Enum.empty?(covered) do
        ["Pattern matching must cover all possible values"]
      else
        covered_str =
          covered
          |> Enum.map(&format_type/1)
          |> Enum.join(", ")

        ["Covered patterns: `#{covered_str}`"]
      end

    %__MODULE__{
      code: :inexhaustive_patterns,
      message: "Non-exhaustive pattern matching",
      expression: Keyword.get(opts, :expression),
      location: Keyword.get(opts, :location),
      spans: Keyword.get(opts, :spans, []),
      suggestions:
        Enum.map(missing_list, fn m ->
          "Add a case branch for `#{format_type(m)}`"
        end),
      notes: notes
    }
  end

  @doc """
  Creates an unreachable branch error.

  ## Options

  - `:subject_type` - The type of the case subject (required)
  - `:pattern_type` - The type the pattern matches (required)
  - `:location` - Source location of the pattern
  - `:expression` - The pattern AST
  - `:spans` - List of labeled spans for multi-span display
  """
  @spec unreachable_branch(keyword()) :: t()
  def unreachable_branch(opts) do
    # Support both old (:expected/:actual) and new (:subject_type/:pattern_type) keys
    subject_type = Keyword.get(opts, :subject_type) || Keyword.fetch!(opts, :expected)
    pattern_type = Keyword.get(opts, :pattern_type) || Keyword.fetch!(opts, :actual)

    %__MODULE__{
      code: :unreachable_branch,
      message:
        "Pattern `#{format_type(pattern_type)}` can never match subject of type `#{format_type(subject_type)}`",
      # Don't set expected/actual so the pointer line doesn't show "expected X, found Y"
      expected: nil,
      actual: nil,
      expression: Keyword.get(opts, :expression),
      location: Keyword.get(opts, :location),
      spans: Keyword.get(opts, :spans, []),
      suggestions: [
        "Remove this unreachable branch",
        "Change the pattern to match `#{format_type(subject_type)}`"
      ],
      notes: [
        "The case subject has type `#{format_type(subject_type)}`",
        "But this pattern only matches `#{format_type(pattern_type)}`"
      ]
    }
  end

  @doc """
  Creates a no matching rule error.

  ## Options

  - `:ast` - The AST expression with no matching rule (required)
  - `:location` - Source location
  - `:notes` - Additional context notes
  - `:spans` - List of labeled spans for multi-span display
  """
  @spec no_matching_rule(keyword()) :: t()
  def no_matching_rule(opts) do
    ast = Keyword.fetch!(opts, :ast)

    %__MODULE__{
      code: :no_matching_rule,
      message: "No typing rule matches this expression",
      expression: ast,
      location: Keyword.get(opts, :location),
      spans: Keyword.get(opts, :spans, []),
      suggestions: ["This construct may not be supported"],
      notes: Keyword.get(opts, :notes, [])
    }
  end

  @doc """
  Creates a missing features error.

  ## Options

  - `:features` - The required feature(s) (required)
  - `:expression` - The expression requiring the feature
  - `:location` - Source location
  - `:spans` - List of labeled spans for multi-span display
  """
  @spec missing_features(keyword()) :: t()
  def missing_features(opts) do
    features = Keyword.fetch!(opts, :features)
    features_list = List.wrap(features)

    %__MODULE__{
      code: :missing_features,
      message: "Required type system features are not enabled",
      expression: Keyword.get(opts, :expression),
      location: Keyword.get(opts, :location),
      spans: Keyword.get(opts, :spans, []),
      suggestions:
        Enum.map(features_list, fn f ->
          "Enable feature `#{inspect(f)}`"
        end),
      notes: ["Use `use Deft, features: #{inspect(features_list)}` to enable"]
    }
  end

  @doc """
  Creates a subtype constraint violation error.

  ## Options

  - `:expected` - The expected supertype (required)
  - `:actual` - The actual type that should be a subtype (required)
  - `:location` - Source location
  - `:expression` - The AST expression
  - `:spans` - List of labeled spans for multi-span display
  - `:suggestions` - List of suggested fixes
  """
  @spec subtype_violation(keyword()) :: t()
  def subtype_violation(opts) do
    expected = Keyword.fetch!(opts, :expected)
    actual = Keyword.fetch!(opts, :actual)

    %__MODULE__{
      code: :subtype_violation,
      message: "Subtype constraint violated",
      expected: expected,
      actual: actual,
      expression: Keyword.get(opts, :expression),
      location: Keyword.get(opts, :location),
      spans: Keyword.get(opts, :spans, []),
      suggestions: Keyword.get(opts, :suggestions, []),
      notes: [
        "Expected subtype of `#{format_type(expected)}`",
        "Found `#{format_type(actual)}`"
      ]
    }
  end

  @doc """
  Creates an unsupported syntax error.

  Used when the compiler encounters Elixir syntax that Deft doesn't support.

  ## Options

  - `:expression` - The unsupported expression (required)
  - `:kind` - The kind of unsupported syntax (default: "expression")
  - `:location` - Source location
  - `:suggestions` - List of suggested alternatives
  - `:notes` - Additional context notes
  - `:spans` - List of labeled spans for multi-span display
  """
  @spec unsupported_syntax(keyword()) :: t()
  def unsupported_syntax(opts) do
    expr = Keyword.fetch!(opts, :expression)
    kind = Keyword.get(opts, :kind, "expression")

    %__MODULE__{
      code: :unsupported_syntax,
      message: "Unsupported #{kind} in Deft",
      expression: expr,
      location: Keyword.get(opts, :location),
      spans: Keyword.get(opts, :spans, []),
      suggestions: Keyword.get(opts, :suggestions, []),
      notes: Keyword.get(opts, :notes, [])
    }
  end

  @doc """
  Creates an unsupported pattern error.

  Used when pattern matching encounters a pattern type that Deft doesn't support.

  ## Options

  - `:expression` - The unsupported pattern (required)
  - `:location` - Source location
  - `:suggestions` - List of suggested alternatives
  - `:notes` - Additional context notes
  - `:spans` - List of labeled spans for multi-span display
  """
  @spec unsupported_pattern(keyword()) :: t()
  def unsupported_pattern(opts) do
    expr = Keyword.fetch!(opts, :expression)

    %__MODULE__{
      code: :unsupported_pattern,
      message: "Unsupported pattern in Deft",
      expression: expr,
      location: Keyword.get(opts, :location),
      spans: Keyword.get(opts, :spans, []),
      suggestions: Keyword.get(opts, :suggestions, []),
      notes: Keyword.get(opts, :notes, [])
    }
  end

  @doc """
  Creates an error for calling an explicitly unsupported function.

  This error is raised when a function has been explicitly marked as
  unsupported via `sig_unsupported` in a signature module, with a reason
  explaining why the function cannot be typed.

  ## Options

  - `:module` - The module containing the function (required)
  - `:function` - The function name (required)
  - `:arity` - The function arity (required)
  - `:reason` - Why this function is unsupported (required)
  - `:location` - Source location
  - `:expression` - The call expression AST
  - `:spans` - List of labeled spans for multi-span display
  """
  @spec unsupported_function(keyword()) :: t()
  def unsupported_function(opts) do
    mod = Keyword.fetch!(opts, :module)
    func = Keyword.fetch!(opts, :function)
    arity = Keyword.fetch!(opts, :arity)
    reason = Keyword.fetch!(opts, :reason)

    %__MODULE__{
      code: :unsupported_function,
      message: "Function `#{inspect(mod)}.#{func}/#{arity}` cannot be typed",
      expression: Keyword.get(opts, :expression),
      location: Keyword.get(opts, :location),
      spans: Keyword.get(opts, :spans, []),
      suggestions: [],
      notes: [reason]
    }
  end

  @doc """
  Creates a conflicting definition error.

  Raised when multiple included modules define the same function signature
  or datatype.

  ## Options

  - `:key` - The conflicting key (e.g., `{Module, :function, arity}` or `:datatype_name`) (required)
  - `:kind` - The kind of definition (`:signature` or `:datatype`) (required)
  - `:definitions` - List of `{module, file, line, type}` tuples for each conflicting definition (required)
  """
  @spec conflicting_definition(keyword()) :: t()
  def conflicting_definition(opts) do
    key = Keyword.fetch!(opts, :key)
    kind = Keyword.fetch!(opts, :kind)
    definitions = Keyword.fetch!(opts, :definitions)

    key_str = format_conflict_key(key, kind)
    kind_str = if kind == :signature, do: "signature", else: "datatype"

    # Build spans for each conflicting definition
    spans =
      definitions
      |> Enum.with_index()
      |> Enum.map(fn {{mod, file, line, type}, idx} ->
        label = if idx == 0, do: "first defined here", else: "also defined here"
        span_kind = if idx == 0, do: :secondary, else: :primary

        %{
          location: {file, line, nil},
          label: "#{label} (from #{inspect(mod)})",
          type: type,
          kind: span_kind
        }
      end)

    notes =
      ["Multiple modules define the same #{kind_str}:"] ++
        Enum.map(definitions, fn {mod, file, line, type} ->
          "  #{inspect(mod)} at #{Path.basename(file)}:#{line} defines: #{format_type(type)}"
        end)

    # Get location from first span
    first_location =
      case spans do
        [%{location: loc} | _] -> loc
        _ -> nil
      end

    %__MODULE__{
      code: :conflicting_definition,
      message: "Conflicting #{kind_str} definitions for `#{key_str}`",
      location: first_location,
      spans: spans,
      suggestions: [
        "Remove one of the conflicting definitions",
        "Use only one module that defines #{key_str}"
      ],
      notes: notes
    }
  end

  defp format_conflict_key({mod, fun, arity}, :signature) do
    "#{inspect(mod)}.#{fun}/#{arity}"
  end

  defp format_conflict_key(name, :datatype) when is_atom(name) do
    "#{name}"
  end

  # ============================================================================
  # Error Code Lookup
  # ============================================================================

  @doc """
  Returns the error code string for a given error code atom.
  """
  @spec error_code_string(error_code()) :: String.t()
  def error_code_string(code) do
    Map.get(@error_codes, code, "E0000")
  end

  # ============================================================================
  # Location Helpers
  # ============================================================================

  @doc """
  Extracts source location from AST metadata or expression.

  Accepts:
  - Raw Elixir AST tuples `{name, meta, args}`
  - Deft AST structs with a `meta` field
  - Raw metadata keyword lists

  This function delegates to `Deft.Span.extract/1`.
  """
  @spec extract_location(term()) :: location() | nil
  defdelegate extract_location(term), to: Span, as: :extract

  @doc """
  Extracts location from a Macro.Env struct.
  """
  @spec extract_location_from_env(Macro.Env.t() | nil) :: location() | nil
  def extract_location_from_env(nil), do: nil

  def extract_location_from_env(%Macro.Env{file: file, line: line}) do
    {file, line, nil}
  end

  # ============================================================================
  # Type Formatting
  # ============================================================================

  @doc """
  Formats a type for display in error messages.
  """
  @spec format_type(Type.t() | nil) :: String.t()
  def format_type(nil), do: "unknown"
  def format_type(%Type.Top{}), do: "any"
  def format_type(%Type.Bottom{}), do: "never"
  def format_type(%Type.Integer{}), do: "integer"
  def format_type(%Type.Float{}), do: "float"
  def format_type(%Type.Number{}), do: "number"
  def format_type(%Type.Boolean{}), do: "boolean"
  def format_type(%Type.Atom{}), do: "atom"

  def format_type(%Type.Fn{inputs: inputs, output: output}) do
    input_strs = Enum.map(inputs, &format_type/1)
    "(#{Enum.join(input_strs, ", ")} -> #{format_type(output)})"
  end

  def format_type(%Type.Union{fst: fst, snd: snd}) do
    "#{format_type(fst)} | #{format_type(snd)}"
  end

  def format_type(%Type.Intersection{fst: fst, snd: snd}) do
    "#{format_type(fst)} & #{format_type(snd)}"
  end

  def format_type(%Type.FixedTuple{elements: elements}) do
    elem_strs = Enum.map(elements, &format_type/1)
    "{#{Enum.join(elem_strs, ", ")}}"
  end

  def format_type(%Type.FixedList{contents: contents}) do
    "[#{format_type(contents)}]"
  end

  def format_type(%Type.List{}), do: "list"
  def format_type(%Type.Tuple{}), do: "tuple"

  def format_type(%Type.ADT{name: name}) do
    format_adt_name(name)
  end

  def format_type(%Type.Variant{name: name, columns: columns}) do
    col_strs = Enum.map(columns, &format_type/1)

    if columns == [] do
      "#{name}"
    else
      "#{name}(#{Enum.join(col_strs, ", ")})"
    end
  end

  def format_type(%Type.Alias{name: name}) do
    "#{name}"
  end

  def format_type(other), do: inspect(other)

  defp format_adt_name(%{name: name}), do: "#{name}"
  defp format_adt_name(name) when is_atom(name), do: "#{name}"
  defp format_adt_name(other), do: inspect(other)

  # ============================================================================
  # Exception Protocol Implementation
  # ============================================================================

  @doc """
  Raises an error as a CompileError with a synthetic stacktrace.

  This provides a clean user-friendly stacktrace pointing to the error
  location in user code, rather than showing internal Deft stacktraces.

  Optionally accepts a Context to enrich the error with file location.
  """
  @spec raise!(t()) :: no_return()
  def raise!(%__MODULE__{} = error) do
    raise_with_synthetic_stacktrace(error)
  end

  @spec raise!(t(), Deft.Context.t()) :: no_return()
  def raise!(%__MODULE__{} = error, %Deft.Context{} = ctx) do
    enriched = Deft.Context.enrich_error(error, ctx)
    raise_with_synthetic_stacktrace(enriched)
  end

  # Raises a CompileError with a synthetic stacktrace pointing to user code.
  defp raise_with_synthetic_stacktrace(%__MODULE__{} = error) do
    # Read source lines if we have a file location
    source_lines = read_source_lines_for_error(error)

    # Format the error message
    formatted = Deft.Error.Formatter.format(error, colors: true, source_lines: source_lines)
    exception = %CompileError{description: formatted}

    # Build a synthetic stacktrace pointing to the error location
    stacktrace = build_synthetic_stacktrace(error)

    :erlang.raise(:error, exception, stacktrace)
  end

  # Build a synthetic stacktrace from the error's location.
  defp build_synthetic_stacktrace(%__MODULE__{location: {file, line, _col}})
       when is_binary(file) and is_integer(line) do
    [{:elixir_compiler, :__FILE__, 1, [file: String.to_charlist(file), line: line]}]
  end

  defp build_synthetic_stacktrace(_), do: []

  # Read source lines from the error's file location.
  defp read_source_lines_for_error(%__MODULE__{location: {file, _, _}}) when is_binary(file) do
    case File.read(file) do
      {:ok, content} -> String.split(content, "\n")
      {:error, _} -> nil
    end
  end

  defp read_source_lines_for_error(_), do: nil
end
