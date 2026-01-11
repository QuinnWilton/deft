defmodule Deft.Type.Variant do
  use Deft.Subtyping.DSL

  @type t :: %__MODULE__{}

  @enforce_keys [:name, :adt_name, :columns]
  defstruct @enforce_keys

  def new(name, adt_name, columns) do
    %__MODULE__{
      name: name,
      adt_name: adt_name,
      columns: columns
    }
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(t, opts) do
      columns =
        container_doc("(", t.columns, ")", opts, fn i, _opts -> Inspect.inspect(i, opts) end,
          separator: ","
        )

      concat([
        Atom.to_string(t.name),
        columns
      ])
    end
  end
end
