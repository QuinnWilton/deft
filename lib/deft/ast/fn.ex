defmodule Deft.AST.Fn do
  alias Deft.AST

  @enforce_keys [:body, :args, :fn_meta, :arrow_meta]
  defstruct @enforce_keys

  def new(body, args, fn_meta \\ [], arrow_meta \\ []) do
    %__MODULE__{
      body: body,
      args: args,
      fn_meta: fn_meta,
      arrow_meta: arrow_meta
    }
  end

  defimpl AST do
    def to_raw_ast(node) do
      body = @protocol.to_raw_ast(node.body)
      args = Enum.map(node.args, &@protocol.to_raw_ast/1)

      {:fn, node.fn_meta, [{:->, node.arrow_meta, [args, body]}]}
    end
  end
end
