defmodule Deft.Type.Variant do
  @moduledoc """
  Represents a single variant (constructor) of an algebraic data type.

  Each variant belongs to an ADT and carries zero or more typed columns. For example,
  in `defdata option(a) :: some(a) | none`, both `some` and `none` are variants:
  - `some` has columns `[a]`
  - `none` has columns `[]`

  Variants are created by the ADT compilation process and used for pattern matching
  and type checking of constructor calls.
  """

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

  defimpl Deft.Walkable do
    def children(node), do: [node.columns]
    def rebuild(node, [columns]), do: %{node | columns: columns}
  end
end
