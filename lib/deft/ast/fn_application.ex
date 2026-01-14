defmodule Deft.AST.FnApplication do
  @moduledoc """
  Represents a function application (call) in the AST.

  Function applications invoke a function value with arguments.
  Type checking verifies that argument types match the function's
  parameter types.
  """

  use Deft.AST.Node,
    fields: [:fun, :args, :fun_meta, :args_meta],
    children: [:fun, :args],
    no_meta: true

  alias Deft.AST

  def new(fun, args, fun_meta \\ [], args_meta \\ []) do
    %__MODULE__{fun: fun, args: args, fun_meta: fun_meta, args_meta: args_meta}
  end

  defimpl AST do
    def to_raw_ast(node) do
      fun = @protocol.to_raw_ast(node.fun)
      args = Enum.map(node.args, &@protocol.to_raw_ast/1)

      {{:., node.fun_meta, [fun]}, node.args_meta, args}
    end
  end
end
