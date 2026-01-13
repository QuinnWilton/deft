defmodule Deft.Type.FixedList do
  use Deft.Subtyping.DSL

  subtype_of(Deft.Type.List)

  parameter(:contents, variance: :covariant)

  structural_rule(fn sub, super ->
    Deft.Subtyping.subtype_of?(super.contents, sub.contents)
  end)

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
      contents_ast = @protocol.to_raw_ast(type.contents)
      [contents_ast]
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
