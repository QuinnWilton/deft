defmodule Deft.AST.Capture do
  @moduledoc """
  AST node for function capture expressions like &double/1 or &String.to_integer/1.
  """

  use Deft.AST.Node, fields: [:module, :function, :arity], children: []

  alias Deft.AST

  @type t :: %__MODULE__{
          module: module() | nil,
          function: atom(),
          arity: non_neg_integer(),
          meta: keyword()
        }

  @doc """
  Creates a new Capture node.

  - `module` is nil for local captures, or the module for remote captures
  - `function` is the function name
  - `arity` is the function arity
  """
  def new(module, function, arity, meta \\ []) do
    %__MODULE__{module: module, function: function, arity: arity, meta: meta}
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
end
