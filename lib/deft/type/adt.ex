defmodule Deft.Type.ADT do
  use Deft.Subtyping.DSL

  # ADT/Variant subtyping is handled directly in Deft.Subtyping

  @type t :: %__MODULE__{}

  @enforce_keys [:name, :variants]
  defstruct @enforce_keys

  def new(name, variants) do
    %__MODULE__{
      name: name,
      variants: variants
    }
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(t, opts) do
      variants =
        container_doc("", t.variants, "", opts, fn i, _opts -> Inspect.inspect(i, opts) end,
          separator: " |"
        )

      concat([
        string("adt("),
        Inspect.inspect(t.name, opts),
        string(", "),
        variants,
        string(")")
      ])
    end
  end

  defimpl Deft.Walkable do
    def children(node), do: [node.variants]
    def rebuild(node, [variants]), do: %{node | variants: variants}
  end
end
