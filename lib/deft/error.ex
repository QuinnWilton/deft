defmodule Deft.MissingTypeError do
  defexception [:expr]

  @type t() :: %__MODULE__{
          expr: term()
        }

  @impl true
  def message(exception) do
    "Expected type annotation on expression: #{Macro.to_string(exception.expr)}"
  end
end
