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

defmodule Deft.UnsupportedLocalCall do
  defexception [:name, :arity]

  @type t() :: %__MODULE__{
          name: atom(),
          arity: arity()
        }

  @impl true
  def message(exception) do
    "Call to unsupported local function: #{exception.name}/#{exception.arity}"
  end
end

defmodule Deft.InexhaustivePatterns do
  defexception [:missing]

  @type t() :: %__MODULE__{
          missing: term()
        }

  @impl true
  def message(exception) do
    "Inexhaustive patterns: missing branch for #{inspect(exception.missing)}"
  end
end
