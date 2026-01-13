defmodule Deft.FFI do
  @moduledoc """
  FFI conversion for ADT return types.

  Handles automatic conversion of native Elixir values to Deft ADT
  representations when calling external functions with `option(a)` or
  `result(t, e)` return types.

  ## Conversion Rules

  For `option(a)` return types:
  - `nil` → `{:none}`
  - `value` → `{:some, value}`

  For `result(t, e)` return types:
  - `{:ok, value}` → `{:ok, value}` (already matches)
  - `{:error, reason}` → `{:err, reason}`
  - `:error` → `{:err, :error}`

  ## Important

  FFI conversion is ONLY applied to external Elixir function calls (signatures
  from `Deft.Signatures.*` modules). Calls to other Deft modules are NOT
  wrapped, as they already return properly formatted ADT tuples.
  """

  alias Deft.AST
  alias Deft.AST.Erased
  alias Deft.Type

  @doc """
  Wraps an erased call AST with FFI conversion if the return type requires it.

  Returns the (possibly wrapped) erased AST.
  """
  @spec maybe_wrap_conversion(Macro.t(), Type.t(), keyword()) :: Macro.t()
  def maybe_wrap_conversion(erased_call, return_type, meta) do
    case conversion_kind(return_type) do
      :option -> wrap_option_conversion(erased_call, meta)
      {:result, error_kind} -> wrap_result_conversion(erased_call, error_kind, meta)
      :none -> erased_call
    end
  end

  @doc """
  Determines what kind of FFI conversion is needed for a return type.

  Returns:
  - `:option` for option types (value | nil → some/none)
  - `{:result, :bare_error}` for fetch_result types ({:ok, t} | :error)
  - `{:result, :tuple_error}` for result types ({:ok, t} | {:error, e})
  - `:none` for other types
  """
  @spec conversion_kind(Type.t()) :: :option | {:result, :bare_error | :tuple_error} | :none
  def conversion_kind(%Type.Alias{name: :option}), do: :option
  def conversion_kind(%Type.Alias{name: :fetch_result}), do: {:result, :bare_error}
  def conversion_kind(%Type.Alias{name: :result}), do: {:result, :tuple_error}

  def conversion_kind(%Type.ADT{name: %AST.Local{name: :option}}), do: :option
  def conversion_kind(%Type.ADT{name: %AST.Local{name: :fetch_result}}), do: {:result, :bare_error}
  def conversion_kind(%Type.ADT{name: %AST.Local{name: :result}}), do: {:result, :tuple_error}

  def conversion_kind(_), do: :none

  # Wrap: case call do nil -> {:none}; value -> {:some, value} end
  defp wrap_option_conversion(call, meta) do
    value_var = Macro.var(:__deft_ffi_value__, __MODULE__)

    branches = [
      Erased.branch(meta, nil, Erased.tuple(meta, [:none])),
      Erased.branch(meta, value_var, Erased.tuple(meta, [:some, value_var]))
    ]

    Erased.case_expr(meta, call, branches)
  end

  # Wrap result conversion based on error kind:
  # - :bare_error - function returns :error (like Enum.fetch) -> fetch_result with {:error}
  # - :tuple_error - function returns {:error, reason} (like File.read) -> result with {:err, reason}
  defp wrap_result_conversion(call, error_kind, meta) do
    value_var = Macro.var(:__deft_ffi_value__, __MODULE__)

    branches =
      case error_kind do
        :bare_error ->
          # Function returns :error -> convert to {:error} (nullary variant tuple)
          [
            Erased.branch(meta, {:ok, value_var}, {:ok, value_var}),
            Erased.branch(meta, :error, Erased.tuple(meta, [:error]))
          ]

        :tuple_error ->
          # Function returns {:error, reason} -> convert to {:err, reason}
          reason_var = Macro.var(:__deft_ffi_reason__, __MODULE__)

          [
            Erased.branch(meta, {:ok, value_var}, {:ok, value_var}),
            Erased.branch(meta, {:error, reason_var}, Erased.tuple(meta, [:err, reason_var]))
          ]
      end

    Erased.case_expr(meta, call, branches)
  end
end
