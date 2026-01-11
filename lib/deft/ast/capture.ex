defmodule Deft.AST.Capture do
  @moduledoc """
  AST node for function capture expressions like &double/1 or &String.to_integer/1.
  """

  alias Deft.AST
  alias Deft.Walkable

  @type t :: %__MODULE__{
          module: module() | nil,
          function: atom(),
          arity: non_neg_integer(),
          meta: keyword()
        }

  @enforce_keys [:module, :function, :arity, :meta]
  defstruct @enforce_keys

  @doc """
  Creates a new Capture node.

  - `module` is nil for local captures, or the module for remote captures
  - `function` is the function name
  - `arity` is the function arity
  """
  def new(module, function, arity, meta \\ []) do
    %__MODULE__{
      module: module,
      function: function,
      arity: arity,
      meta: meta
    }
  end

  defimpl AST do
    def to_raw_ast(node) do
      if node.module do
        # Remote capture: &Module.function/arity
        dot = {:., [], [node.module, node.function]}
        call = {dot, [no_parens: true], []}
        slash = {:/, [], [call, node.arity]}
        {:&, node.meta, [slash]}
      else
        # Local capture: &function/arity
        name = {node.function, [], Elixir}
        slash = {:/, [], [name, node.arity]}
        {:&, node.meta, [slash]}
      end
    end
  end

  defimpl Walkable do
    def children(_node) do
      # Captures have no child nodes to walk
      []
    end

    def rebuild(node, []) do
      node
    end
  end
end
