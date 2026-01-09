defmodule Deft.Type.FixedList do
  alias Deft.AST

  @type t :: %__MODULE__{}

  @enforce_keys [:contents]
  defstruct @enforce_keys

  def new(contents) do
    %__MODULE__{
      contents: contents
    }
  end

  def contents(%__MODULE__{} = list) do
    list.contents
  end

  defimpl AST do
    def to_raw_ast(type) do
      contents = Enum.map(type.contents, &@protocol.to_raw_ast/1)

      [contents]
    end
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(t, opts) do
      concat([
        string("["),
        Inspect.inspect(t.contents, opts),
        string("]")
      ])
    end
  end
end
