defmodule Deft.AST.Fn do
  use Deft.AST.Node,
    fields: [:body, :args, :fn_meta, :arrow_meta],
    children: [:body, :args],
    no_meta: true

  alias Deft.AST

  def new(body, args, fn_meta \\ [], arrow_meta \\ []) do
    %__MODULE__{body: body, args: args, fn_meta: fn_meta, arrow_meta: arrow_meta}
  end

  defimpl AST do
    def to_raw_ast(node) do
      body = @protocol.to_raw_ast(node.body)
      args = Enum.map(node.args, &@protocol.to_raw_ast/1)

      {:fn, node.fn_meta, [{:->, node.arrow_meta, [args, body]}]}
    end
  end
end
