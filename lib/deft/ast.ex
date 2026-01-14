defprotocol Deft.AST do
  @moduledoc """
  Protocol for converting Deft AST nodes back to raw Elixir AST.

  All AST node types implement this protocol to support type erasure,
  where typed Deft code is converted back to standard Elixir AST for
  compilation.

  ## Implementation

  Each AST module (e.g., `Deft.AST.Literal`, `Deft.AST.Fn`) implements
  this protocol via `use Deft.AST.Node`, which generates both the
  `Deft.AST` and `Deft.Walkable` protocol implementations.
  """

  @spec to_raw_ast(t()) :: term()
  def to_raw_ast(t)
end
