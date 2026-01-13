defmodule Deft.TypeSystem.Conflict do
  @moduledoc false
  # Internal module for detecting conflicts when merging signatures and datatypes
  # from multiple included modules.

  alias Deft.Error

  @doc """
  Merges signatures from multiple modules, raising on conflicts.

  Each module entry is a tuple: `{module, file, line}` where file and line
  indicate where the module was included.

  Raises a compile error if the same function signature is defined in
  multiple modules.
  """
  @spec merge_signatures([{module(), String.t(), non_neg_integer()}]) :: map()
  def merge_signatures(modules_with_loc) do
    # Build a map tracking which module defined each key
    # {key => [{module, file, line, type}, ...]}
    tracking =
      Enum.reduce(modules_with_loc, %{}, fn {mod, file, line}, acc ->
        Code.ensure_loaded(mod)
        sigs = mod.signatures()

        Enum.reduce(sigs, acc, fn {key, type}, inner_acc ->
          entry = {mod, file, line, type}
          Map.update(inner_acc, key, [entry], &[entry | &1])
        end)
      end)

    # Check for conflicts (multiple definitions for same key)
    conflicts =
      tracking
      |> Enum.filter(fn {_key, defs} -> length(defs) > 1 end)
      |> Enum.map(fn {key, defs} -> {key, Enum.reverse(defs)} end)

    if conflicts != [] do
      # Raise error for first conflict
      {key, definitions} = hd(conflicts)

      error =
        Error.conflicting_definition(
          key: key,
          kind: :signature,
          definitions: definitions
        )

      Error.raise!(error)
    end

    # No conflicts, build the final map
    Map.new(tracking, fn {key, [entry | _]} ->
      {_mod, _file, _line, type} = entry
      {key, type}
    end)
  end

  @doc """
  Merges datatypes from multiple modules, raising on conflicts.

  Each module entry is a tuple: `{module, file, line}` where file and line
  indicate where the module was included.

  Raises a compile error if the same datatype name is defined in
  multiple modules.
  """
  @spec merge_datatypes([{module(), String.t(), non_neg_integer()}]) :: map()
  def merge_datatypes(modules_with_loc) do
    tracking =
      Enum.reduce(modules_with_loc, %{}, fn {mod, file, line}, acc ->
        Code.ensure_loaded(mod)

        if function_exported?(mod, :datatypes, 0) do
          datatypes = mod.datatypes()

          Enum.reduce(datatypes, acc, fn {name, type}, inner_acc ->
            entry = {mod, file, line, type}
            Map.update(inner_acc, name, [entry], &[entry | &1])
          end)
        else
          acc
        end
      end)

    conflicts =
      tracking
      |> Enum.filter(fn {_key, defs} -> length(defs) > 1 end)
      |> Enum.map(fn {key, defs} -> {key, Enum.reverse(defs)} end)

    if conflicts != [] do
      {key, definitions} = hd(conflicts)

      error =
        Error.conflicting_definition(
          key: key,
          kind: :datatype,
          definitions: definitions
        )

      Error.raise!(error)
    end

    Map.new(tracking, fn {key, [entry | _]} ->
      {_mod, _file, _line, type} = entry
      {key, type}
    end)
  end
end
