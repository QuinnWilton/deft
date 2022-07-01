defmodule Deft.Type.Fn do
  alias Deft.Type

  @enforce_keys [:inputs, :output]
  defstruct @enforce_keys

  def new(inputs, output) do
    %__MODULE__{
      inputs: inputs,
      output: output
    }
  end

  defimpl Deft.Type do
    def subtype_of?(t1, t2) when is_struct(t2, Type.Fn) do
      inputs = Enum.zip(t1.inputs, t2.inputs)

      inputs_subtype? =
        Enum.all?(inputs, fn {ti1, ti2} ->
          subtype_of?(ti2, ti1)
        end)

      output_subtype? = subtype_of?(t1.output, t2.output)

      inputs_subtype? and output_subtype?
    end

    def subtype_of(_, _) do
      false
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
