defmodule Deft.Span do
  @moduledoc """
  Centralized span/location extraction for error messages.

  This module provides a single source of truth for extracting location
  information from AST nodes and building labeled spans for error display.
  """

  @type location :: {file :: String.t() | nil, line :: pos_integer(), column :: pos_integer() | nil}

  @type labeled_span :: %{
          required(:location) => location(),
          required(:label) => String.t(),
          required(:type) => term() | nil,
          required(:kind) => :primary | :secondary
        }

  # ============================================================================
  # Location Extraction
  # ============================================================================

  @doc """
  Extracts location from any AST node, raw Elixir AST tuple, or meta keyword list.

  Returns `{file, line, column}` tuple or `nil` if location cannot be extracted.

  ## Examples

      iex> Span.extract(%AST.Local{name: :x, meta: [line: 10, column: 5]})
      {nil, 10, 5}

      iex> Span.extract([line: 5, column: 3, file: "test.ex"])
      {"test.ex", 5, 3}

      iex> Span.extract("not an AST node")
      nil
  """
  @spec extract(term()) :: location() | nil
  def extract({_, meta, _}) when is_list(meta), do: from_meta(meta)
  def extract(%{meta: meta}) when is_list(meta), do: from_meta(meta)
  def extract(meta) when is_list(meta), do: from_meta(meta)
  def extract(_), do: nil

  @doc """
  Extracts location from a metadata keyword list.

  Returns `{file, line, column}` tuple or `nil` if `:line` is not present.
  """
  @spec from_meta(keyword()) :: location() | nil
  def from_meta(meta) when is_list(meta) do
    line = Keyword.get(meta, :line)
    column = Keyword.get(meta, :column)
    file = Keyword.get(meta, :file)

    if line, do: {file, line, column}, else: nil
  end

  @doc """
  Gets the leftmost (first available) location from two nodes.

  Useful for binary operators where the left operand typically has location info.
  """
  @spec leftmost(term(), term()) :: location() | nil
  def leftmost(node1, node2) do
    extract(node1) || extract(node2)
  end

  # ============================================================================
  # Labeled Span Construction
  # ============================================================================

  @doc """
  Builds a primary (error site) labeled span.

  Returns `nil` if location is `nil`, allowing safe use in list construction.

  ## Examples

      iex> Span.primary({"test.ex", 10, 5}, "error here", nil)
      %{location: {"test.ex", 10, 5}, label: "error here", type: nil, kind: :primary}

      iex> Span.primary(nil, "error here", nil)
      nil
  """
  @spec primary(location() | nil, String.t(), term()) :: labeled_span() | nil
  def primary(nil, _label, _type), do: nil

  def primary(location, label, type) do
    %{location: location, label: label, type: type, kind: :primary}
  end

  @doc """
  Builds a secondary (context) labeled span.

  Returns `nil` if location is `nil`, allowing safe use in list construction.

  ## Examples

      iex> Span.secondary({"test.ex", 10, 5}, "defined here", type)
      %{location: {"test.ex", 10, 5}, label: "defined here", type: type, kind: :secondary}

      iex> Span.secondary(nil, "defined here", type)
      nil
  """
  @spec secondary(location() | nil, String.t(), term()) :: labeled_span() | nil
  def secondary(nil, _label, _type), do: nil

  def secondary(location, label, type) do
    %{location: location, label: label, type: type, kind: :secondary}
  end

  @doc """
  Filters nil values from a list of spans.

  Useful after building spans with `primary/3` and `secondary/3` which may return nil.

  ## Examples

      iex> spans = [Span.primary(loc, "a", nil), nil, Span.secondary(nil, "b", nil)]
      iex> Span.filter(spans)
      [%{location: loc, label: "a", type: nil, kind: :primary}]
  """
  @spec filter([labeled_span() | nil]) :: [labeled_span()]
  def filter(spans), do: Enum.reject(spans, &is_nil/1)
end
