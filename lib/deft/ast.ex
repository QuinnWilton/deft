defprotocol Deft.AST do
  @spec to_raw_ast(t()) :: term()
  def to_raw_ast(t)
end
