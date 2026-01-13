defmodule Deft.Type.Alias do
  @moduledoc """
  A type alias reference, optionally with type arguments.

  ## Fields

  - `name` - The alias name (atom)
  - `context` - The context from the AST (usually nil or Elixir)
  - `args` - List of type arguments for parameterized aliases (default: [])

  ## Usage

  For non-parameterized aliases:
      Type.Alias.new(:my_type, nil)

  For parameterized aliases like `option(integer)`:
      Type.Alias.new(:option, nil, [Type.integer()])
  """

  use Deft.Subtyping.DSL

  alias Deft.Type

  @type t :: %__MODULE__{
          name: atom(),
          context: atom() | nil,
          args: [Type.t()]
        }

  @enforce_keys [:name]
  defstruct [:name, :context, args: []]

  @doc """
  Creates a new type alias.

  ## Parameters

  - `name` - The alias name
  - `context` - The AST context (usually nil)
  - `args` - Optional list of type arguments for parameterized aliases
  """
  @spec new(atom(), atom() | nil, [Type.t()]) :: t()
  def new(name, context \\ nil, args \\ []) do
    %__MODULE__{
      name: name,
      context: context,
      args: args
    }
  end

  defimpl Inspect do
    def inspect(t, opts) do
      if t.args == [] do
        Atom.to_string(t.name)
      else
        args_str =
          t.args
          |> Enum.map(&Inspect.inspect(&1, opts))
          |> Enum.join(", ")

        "#{t.name}(#{args_str})"
      end
    end
  end

  defimpl Deft.Walkable do
    def children(%{args: args}), do: [args]
    def rebuild(node, [args]), do: %{node | args: args}
  end
end
