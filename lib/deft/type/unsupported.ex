defmodule Deft.Type.Unsupported do
  @moduledoc """
  Marker type for functions that cannot be accurately typed.

  When a function signature returns this type, the type checker will
  emit a compile-time error with the provided reason explaining why
  the function cannot be typed within Deft's type system.

  ## Usage

  This type is created by the `sig_unsupported` macro in `Deft.Signatures.DSL`:

      sig_unsupported group_by([a], (a -> b)) :: top,
        reason: "Returns a map type which Deft cannot represent"

  When the type checker encounters a call to this function, it raises
  an `unsupported_function` error with the stored reason.
  """

  @enforce_keys [:module, :function, :arity, :reason]
  defstruct [:module, :function, :arity, :reason]

  @type t :: %__MODULE__{
          module: module(),
          function: atom(),
          arity: non_neg_integer(),
          reason: String.t()
        }

  @doc """
  Creates a new unsupported type marker.

  ## Parameters

  - `module` - The module containing the function
  - `function` - The function name
  - `arity` - The function arity
  - `reason` - Human-readable explanation of why this function cannot be typed
  """
  @spec new(module(), atom(), non_neg_integer(), String.t()) :: t()
  def new(module, function, arity, reason)
      when is_atom(module) and is_atom(function) and is_integer(arity) and arity >= 0 and
             is_binary(reason) do
    %__MODULE__{
      module: module,
      function: function,
      arity: arity,
      reason: reason
    }
  end
end
