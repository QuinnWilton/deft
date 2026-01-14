defmodule Deft.Type.Alias do
  @moduledoc """
  A type alias reference, optionally with type arguments.

  ## Fields

  - `name` - The alias name (atom)
  - `context` - The context from the AST (usually nil or Elixir)
  - `args` - List of type arguments for parameterized aliases (default: [])
  - `location` - Source location where this alias was referenced (default: nil)

  ## Usage

  For non-parameterized aliases:
      Type.Alias.new(:my_type, nil)

  For parameterized aliases like `option(integer)`:
      Type.Alias.new(:option, nil, [Type.integer()])

  With location for error reporting:
      Type.Alias.new(:option, nil, [Type.integer()], {"lib/my_file.ex", 10, 5})
  """

  use Deft.Subtyping.DSL

  alias Deft.Type

  @type t :: %__MODULE__{
          name: atom(),
          context: atom() | nil,
          args: [Type.t()],
          location: Deft.Span.location() | nil
        }

  @enforce_keys [:name]
  defstruct [:name, :context, args: [], location: nil]

  @doc """
  Creates a new type alias.

  ## Parameters

  - `name` - The alias name
  - `context` - The AST context (usually nil)
  - `args` - Optional list of type arguments for parameterized aliases
  - `location` - Optional source location for error reporting
  """
  @spec new(atom(), atom() | nil, [Type.t()], Deft.Span.location() | nil) :: t()
  def new(name, context \\ nil, args \\ [], location \\ nil) do
    %__MODULE__{
      name: name,
      context: context,
      args: args,
      location: location
    }
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(t, opts) do
      if t.args == [] do
        string(Atom.to_string(t.name))
      else
        args_doc =
          container_doc("(", t.args, ")", opts, fn arg, _opts ->
            Inspect.inspect(arg, opts)
          end, separator: ",")

        concat([
          string(Atom.to_string(t.name)),
          args_doc
        ])
      end
    end
  end

  defimpl Deft.Walkable do
    def children(%{args: args}), do: [args]
    def rebuild(node, [args]), do: %{node | args: args}
  end
end
