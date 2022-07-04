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

defmodule Deft.MalformedTypedError do
  defexception [:expr]

  @type t() :: %__MODULE__{
          expr: term()
        }

  @impl true
  def message(exception) do
    "Malformed type: #{Macro.to_string(exception.expr)}"
  end
end

defmodule Deft.TypecheckingError do
  defexception [:expected, :actual]

  @type t() :: %__MODULE__{
          expected: term(),
          actual: term()
        }

  @impl true
  def message(exception) do
    "Typechecking failed: expected #{inspect(exception.expected)}, got #{inspect(exception.actual)}"
  end
end

defmodule Deft.UnreachableBranchError do
  defexception [:expected, :actual]

  @type t() :: %__MODULE__{
          expected: term(),
          actual: term()
        }

  @impl true
  def message(exception) do
    "Branch unreachable: value has type #{inspect(exception.expected)}, but pattern has type #{inspect(exception.actual)}"
  end
end
