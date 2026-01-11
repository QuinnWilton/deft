defmodule Deft.Type.Fn do
  use Deft.Subtyping.DSL

  parameter(:inputs, variance: :contravariant)
  parameter(:output, variance: :covariant)

  structural_rule(fn sub, super ->
    # Contravariant inputs: super's inputs must be subtypes of sub's inputs
    # Covariant output: sub's output must be subtype of super's output
    length(sub.inputs) == length(super.inputs) and
      Enum.zip(sub.inputs, super.inputs)
      |> Enum.all?(fn {sub_in, super_in} ->
        Deft.Subtyping.subtype_of?(sub_in, super_in)
      end) and
      Deft.Subtyping.subtype_of?(super.output, sub.output)
  end)

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
