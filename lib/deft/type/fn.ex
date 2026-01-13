defmodule Deft.Type.Fn do
  use Deft.Subtyping.DSL

  # Contravariant inputs with arity check: fn(Number) -> T <: fn(Integer) -> T
  # because a function accepting Number can be used where one accepting Integer is expected
  parameter(:inputs, variance: :contravariant, arity: :must_match)

  # Covariant output: fn(T) -> Integer <: fn(T) -> Number
  # because a function returning Integer can be used where one returning Number is expected
  parameter(:output, variance: :covariant)

  alias Deft.AST

  @type t :: %__MODULE__{}

  @enforce_keys [:inputs, :output]
  defstruct @enforce_keys

  def new(inputs, output) do
    %__MODULE__{
      inputs: inputs,
      output: output
    }
  end

  defimpl AST do
    def to_raw_ast(type) do
      inputs = Enum.map(type.inputs, &@protocol.to_raw_ast/1)
      output = @protocol.to_raw_ast(type.output)

      [{:->, [], [inputs, output]}]
    end
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(t, opts) do
      args =
        container_doc(" (", t.inputs, ") ", opts, fn i, _opts -> Inspect.inspect(i, opts) end,
          separator: ","
        )

      concat([
        string("fn"),
        args,
        string("-> "),
        Inspect.inspect(t.output, opts)
      ])
    end
  end
end
