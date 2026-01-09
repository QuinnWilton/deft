# Walkable protocol implementations for all AST and Type nodes.
# This file adds protocol implementations without modifying the original struct definitions.

alias Deft.AST
alias Deft.Type
alias Deft.Walkable

# AST Node Implementations

defimpl Walkable, for: AST.Annotation do
  def children(node), do: [node.pattern, node.type]
  def rebuild(node, [pattern, type]), do: %{node | pattern: pattern, type: type}
end

defimpl Walkable, for: AST.Block do
  def children(node), do: [node.exprs]
  def rebuild(node, [exprs]), do: %{node | exprs: exprs}
end

defimpl Walkable, for: AST.Case do
  def children(node), do: [node.subject, node.branches]
  def rebuild(node, [subject, branches]), do: %{node | subject: subject, branches: branches}
end

defimpl Walkable, for: AST.CaseBranch do
  def children(node), do: [node.pattern, node.body]
  def rebuild(node, [pattern, body]), do: %{node | pattern: pattern, body: body}
end

defimpl Walkable, for: AST.Cond do
  def children(node), do: [node.branches]
  def rebuild(node, [branches]), do: %{node | branches: branches}
end

defimpl Walkable, for: AST.CondBranch do
  def children(node), do: [node.predicate, node.body]
  def rebuild(node, [predicate, body]), do: %{node | predicate: predicate, body: body}
end

defimpl Walkable, for: AST.Cons do
  def children(node), do: [node.head, node.rest]
  def rebuild(node, [head, rest]), do: %{node | head: head, rest: rest}
end

defimpl Walkable, for: AST.DefData do
  def children(node), do: [node.name, node.variants]
  def rebuild(node, [name, variants]), do: %{node | name: name, variants: variants}
end

defimpl Walkable, for: AST.Fn do
  def children(node), do: [node.body, node.args]
  def rebuild(node, [body, args]), do: %{node | body: body, args: args}
end

defimpl Walkable, for: AST.FnApplication do
  def children(node), do: [node.fun, node.args]
  def rebuild(node, [fun, args]), do: %{node | fun: fun, args: args}
end

defimpl Walkable, for: AST.If do
  def children(node), do: [node.predicate, node.do, node.else]

  def rebuild(node, [predicate, do_branch, else_branch]),
    do: %{node | predicate: predicate, do: do_branch, else: else_branch}
end

defimpl Walkable, for: AST.List do
  def children(node), do: [node.elements]
  def rebuild(node, [elements]), do: %{node | elements: elements}
end

defimpl Walkable, for: AST.Literal do
  # Literals have no children to walk
  def children(_node), do: []
  def rebuild(node, []), do: node
end

defimpl Walkable, for: AST.Local do
  # Locals have no children to walk (name and context are atoms)
  def children(_node), do: []
  def rebuild(node, []), do: node
end

defimpl Walkable, for: AST.LocalCall do
  def children(node), do: [node.args]
  def rebuild(node, [args]), do: %{node | args: args}
end

defimpl Walkable, for: AST.Match do
  def children(node), do: [node.pattern, node.value]
  def rebuild(node, [pattern, value]), do: %{node | pattern: pattern, value: value}
end

defimpl Walkable, for: AST.Pair do
  def children(node), do: [node.fst, node.snd]
  def rebuild(node, [fst, snd]), do: %{node | fst: fst, snd: snd}
end

defimpl Walkable, for: AST.Pin do
  def children(node), do: [node.expr]
  def rebuild(node, [expr]), do: %{node | expr: expr}
end

defimpl Walkable, for: AST.Tuple do
  def children(node), do: [node.elements]
  def rebuild(node, [elements]), do: %{node | elements: elements}
end

defimpl Walkable, for: AST.TypeConstructorCall do
  def children(node), do: [node.args]
  def rebuild(node, [args]), do: %{node | args: args}
end

defimpl Walkable, for: AST.Variant do
  def children(node), do: [node.columns]
  def rebuild(node, [columns]), do: %{node | columns: columns}
end

# Type Node Implementations

defimpl Walkable, for: Type.ADT do
  def children(node), do: [node.variants]
  def rebuild(node, [variants]), do: %{node | variants: variants}
end

defimpl Walkable, for: Type.Alias do
  # Type aliases have no children to walk
  def children(_node), do: []
  def rebuild(node, []), do: node
end

defimpl Walkable, for: Type.Atom do
  def children(_node), do: []
  def rebuild(node, []), do: node
end

defimpl Walkable, for: Type.Boolean do
  def children(_node), do: []
  def rebuild(node, []), do: node
end

defimpl Walkable, for: Type.Bottom do
  def children(_node), do: []
  def rebuild(node, []), do: node
end

defimpl Walkable, for: Type.FixedList do
  def children(node), do: [node.contents]
  def rebuild(node, [contents]), do: %{node | contents: contents}
end

defimpl Walkable, for: Type.FixedTuple do
  def children(node), do: [node.elements]
  def rebuild(node, [elements]), do: %{node | elements: elements}
end

defimpl Walkable, for: Type.Float do
  def children(_node), do: []
  def rebuild(node, []), do: node
end

defimpl Walkable, for: Type.Fn do
  def children(node), do: [node.inputs, node.output]
  def rebuild(node, [inputs, output]), do: %{node | inputs: inputs, output: output}
end

defimpl Walkable, for: Type.Integer do
  def children(_node), do: []
  def rebuild(node, []), do: node
end

defimpl Walkable, for: Type.Intersection do
  def children(node), do: [node.fst, node.snd]
  def rebuild(node, [fst, snd]), do: %{node | fst: fst, snd: snd}
end

defimpl Walkable, for: Type.List do
  def children(_node), do: []
  def rebuild(node, []), do: node
end

defimpl Walkable, for: Type.Number do
  def children(_node), do: []
  def rebuild(node, []), do: node
end

defimpl Walkable, for: Type.Top do
  def children(_node), do: []
  def rebuild(node, []), do: node
end

defimpl Walkable, for: Type.Tuple do
  def children(_node), do: []
  def rebuild(node, []), do: node
end

defimpl Walkable, for: Type.Union do
  def children(node), do: [node.fst, node.snd]
  def rebuild(node, [fst, snd]), do: %{node | fst: fst, snd: snd}
end

defimpl Walkable, for: Type.Variant do
  def children(node), do: [node.columns]
  def rebuild(node, [columns]), do: %{node | columns: columns}
end
