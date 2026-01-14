defmodule Deft.AST.Utils do
  @moduledoc """
  Shared utilities for AST parsing and compilation.

  This module provides common helper functions used by both `Deft.Compiler`
  and `Deft.TypeParser.Parser` for processing AST nodes.
  """

  @doc """
  Maps over a list with a function that returns `{:ok, value}` or `{:error, reason}`,
  returning early on the first error.

  ## Examples

      iex> AST.Utils.map_ok([1, 2, 3], fn x -> {:ok, x * 2} end)
      {:ok, [2, 4, 6]}

      iex> AST.Utils.map_ok([1, 2, 3], fn x ->
      ...>   if x == 2, do: {:error, :failed}, else: {:ok, x}
      ...> end)
      {:error, :failed}

  """
  @spec map_ok([term()], (term() -> {:ok, t} | {:error, e})) :: {:ok, [t]} | {:error, e}
        when t: term(), e: term()
  def map_ok(list, fun) do
    result =
      Enum.reduce_while(list, {:ok, []}, fn item, {:ok, acc} ->
        case fun.(item) do
          {:ok, value} -> {:cont, {:ok, [value | acc]}}
          {:error, _} = err -> {:halt, err}
        end
      end)

    case result do
      {:ok, reversed} -> {:ok, Enum.reverse(reversed)}
      error -> error
    end
  end

  @doc """
  Finds a similar name from a list of known names using Jaro distance.

  Returns the first name with similarity above the threshold, or `nil` if none found.

  ## Parameters

  - `name` - The name to find matches for
  - `known_names` - List of known names to search
  - `threshold` - Minimum Jaro distance to consider a match (default: 0.8)

  ## Examples

      iex> AST.Utils.find_similar(:optin, [:option, :result, :either])
      :option

      iex> AST.Utils.find_similar(:xyz, [:option, :result, :either])
      nil

  """
  @spec find_similar(atom(), [atom()], float()) :: atom() | nil
  def find_similar(name, known_names, threshold \\ 0.8) do
    name_str = Atom.to_string(name)

    Enum.find(known_names, fn known ->
      known_str = Atom.to_string(known)
      String.jaro_distance(name_str, known_str) > threshold
    end)
  end
end
