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

  alias Deft.Type

  @type error_code ::
          :type_mismatch
          | :missing_annotation
          | :malformed_type
          | :unsupported_call
          | :inexhaustive_patterns
          | :unreachable_branch
          | :no_matching_rule
          | :missing_features
          | :subtype_violation
          | :unsupported_syntax
          | :unsupported_pattern

  @type location :: {String.t() | nil, non_neg_integer() | nil, non_neg_integer() | nil}

  @typedoc """
  A labeled span for multi-span error display.

  Contains a location tuple, a label describing what this span represents,
  and optionally a type to display.
  """
  @type labeled_span :: %{
          location: location(),
          label: String.t(),
          type: Type.t() | nil
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
    unsupported_call: "E0004",
    inexhaustive_patterns: "E0005",
    unreachable_branch: "E0006",
    no_matching_rule: "E0007",
    missing_features: "E0008",
    subtype_violation: "E0009",
    unsupported_syntax: "E0010",
    unsupported_pattern: "E0011"
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
      message: "Type mismatch: expected `#{format_type(expected)}`, found `#{format_type(actual)}`",
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
            type: declared_type
          }
        end,
        if body_location do
          %{
            location: body_location,
            label: "body has type",
            type: actual_type
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

  - `:expected` - The type of the value being matched (required)
  - `:actual` - The type the pattern expects (required)
  - `:location` - Source location of the pattern
  - `:expression` - The pattern AST
  - `:spans` - List of labeled spans for multi-span display
  """
  @spec unreachable_branch(keyword()) :: t()
  def unreachable_branch(opts) do
    expected = Keyword.fetch!(opts, :expected)
    actual = Keyword.fetch!(opts, :actual)

    %__MODULE__{
      code: :unreachable_branch,
      message: "Branch is unreachable",
      expected: expected,
      actual: actual,
      expression: Keyword.get(opts, :expression),
      location: Keyword.get(opts, :location),
      spans: Keyword.get(opts, :spans, []),
      suggestions: ["Remove this branch or fix the pattern"],
      notes: [
        "Value has type `#{format_type(expected)}`",
        "Pattern expects type `#{format_type(actual)}`"
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
  """
  @spec extract_location(term()) :: location() | nil
  def extract_location({_, meta, _}) when is_list(meta) do
    extract_location_from_meta(meta)
  end

  def extract_location(%{meta: meta}) when is_list(meta) do
    extract_location_from_meta(meta)
  end

  def extract_location(_), do: nil

  defp extract_location_from_meta(meta) do
    line = Keyword.get(meta, :line)
    column = Keyword.get(meta, :column)
    file = Keyword.get(meta, :file)

    if line do
      {file, line, column}
    else
      nil
    end
  end

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
  Converts an Error struct to an exception that can be raised.

  This allows using `raise Deft.Error.to_exception(error)` or
  implementing the Exception protocol directly.

  The Exception module is defined in `lib/deft/error/exception.ex`.
  """
  @spec to_exception(t()) :: Exception.t()
  def to_exception(%__MODULE__{} = error) do
    %Deft.Error.Exception{error: error}
  end

  @doc """
  Raises an error as an exception.
  """
  @spec raise!(t()) :: no_return()
  def raise!(%__MODULE__{} = error) do
    raise to_exception(error)
  end
end
