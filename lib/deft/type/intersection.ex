defmodule Deft.Type.Intersection do
  alias Deft.AST

  @type t :: %__MODULE__{}

  @enforce_keys [:fst, :snd]
  defstruct @enforce_keys

  def new(fst, snd) do
    %__MODULE__{
      fst: fst,
      snd: snd
    }
  end

  defimpl AST do
    def to_raw_ast(type) do
      fst = @protocol.to_raw_ast(type.fst)
      snd = @protocol.to_raw_ast(type.snd)

      {:&, [], [fst, snd]}
    end
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(t, opts) do
      concat([
        Inspect.inspect(t.fst, opts),
        " & ",
        Inspect.inspect(t.snd, opts)
      ])
    end
  end
end
