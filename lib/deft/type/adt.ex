defmodule Deft.Type.ADT do
  @moduledoc """
  Algebraic data type with optional type parameters.

  An ADT consists of:
  - `name` - The ADT name (as an AST.Local or atom)
  - `variants` - List of Type.Variant structs
  - `params` - List of type parameter names (atoms), empty for monomorphic ADTs

  ## Parameterized ADTs

  When `params` is non-empty, the ADT is polymorphic:

      defdata option(a) :: some(a) | none

  This creates an ADT with `params: [:a]`. When used as `option(integer)`,
  call `instantiate/2` to substitute `a` with `integer`.
  """

  use Deft.Subtyping.DSL

  alias Deft.Substitution
  alias Deft.Type

  # ADT/Variant subtyping is handled directly in Deft.Subtyping

  @type t :: %__MODULE__{
          name: term(),
          variants: [Type.Variant.t()],
          params: [atom()]
        }

  @enforce_keys [:name, :variants]
  defstruct [:name, :variants, params: []]

  @doc """
  Creates a new ADT type.

  ## Parameters

  - `name` - The ADT name (usually an AST.Local)
  - `variants` - List of Type.Variant structs
  - `params` - Optional list of type parameter names (default: [])
  """
  @spec new(term(), [Type.Variant.t()], [atom()]) :: t()
  def new(name, variants, params \\ []) do
    %__MODULE__{
      name: name,
      variants: variants,
      params: params
    }
  end

  @doc """
  Returns true if the ADT has type parameters.
  """
  @spec polymorphic?(t()) :: boolean()
  def polymorphic?(%__MODULE__{params: params}), do: params != []

  @doc """
  Instantiates a polymorphic ADT with concrete type arguments.

  Given an ADT like `option(a)` and type arguments `[integer]`,
  returns a monomorphic ADT with `a` replaced by `integer` in all variants.

  ## Parameters

  - `adt` - The polymorphic ADT to instantiate
  - `type_args` - List of concrete types to substitute for parameters

  ## Returns

  A new ADT with `params: []` and all type variables substituted.

  ## Examples

      # Given: defdata option(a) :: some(a) | none
      option_adt = %ADT{name: :option, params: [:a], variants: [...]}

      # Instantiate with integer:
      ADT.instantiate(option_adt, [Type.integer()])
      # => %ADT{name: :option, params: [], variants: [some(integer), none]}
  """
  @spec instantiate(t(), [Type.t()]) :: t()
  def instantiate(%__MODULE__{params: []} = adt, []), do: adt

  def instantiate(%__MODULE__{params: params, name: name, variants: variants}, type_args)
      when length(params) == length(type_args) do
    # Build substitution map: param_name => concrete_type
    subst = Enum.zip(params, type_args) |> Map.new()

    # Substitute in each variant's columns
    instantiated_variants =
      Enum.map(variants, fn %Type.Variant{} = variant ->
        new_columns = Enum.map(variant.columns, &Substitution.substitute(&1, subst))
        %{variant | columns: new_columns}
      end)

    %__MODULE__{name: name, variants: instantiated_variants, params: []}
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
