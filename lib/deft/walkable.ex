defprotocol Deft.Walkable do
  @moduledoc """
  Protocol for walking AST and Type nodes.

  Implementing this protocol allows nodes to be traversed uniformly
  without manual pattern matching for each node type.
  """

  @doc """
  Returns a list of child nodes that should be recursively walked.
  """
  @spec children(t) :: [term()]
  def children(node)

  @doc """
  Rebuilds the node with new children after walking.
  The children list must be in the same order as returned by children/1.
  """
  @spec rebuild(t, [term()]) :: t
  def rebuild(node, new_children)
end

defmodule Deft.Walker do
  @moduledoc """
  Generic tree walking utilities using the Walkable protocol.
  """

  alias Deft.Walkable

  @doc """
  Performs a post-order traversal, applying the function after visiting children.
  """
  @spec postwalk(term(), (term() -> term())) :: term()
  def postwalk(node, fun) when is_struct(node) do
    if Deft.Walkable.impl_for(node) do
      children = Walkable.children(node)
      new_children = Enum.map(children, &postwalk(&1, fun))
      rebuilt = Walkable.rebuild(node, new_children)
      fun.(rebuilt)
    else
      fun.(node)
    end
  end

  def postwalk(nodes, fun) when is_list(nodes) do
    nodes
    |> Enum.map(&postwalk(&1, fun))
    |> fun.()
  end

  def postwalk(node, fun) do
    fun.(node)
  end

  @doc """
  Performs a pre-order traversal, applying the function before visiting children.
  """
  @spec prewalk(term(), (term() -> term())) :: term()
  def prewalk(node, fun) when is_struct(node) do
    node = fun.(node)

    if Deft.Walkable.impl_for(node) do
      children = Walkable.children(node)
      new_children = Enum.map(children, &prewalk(&1, fun))
      Walkable.rebuild(node, new_children)
    else
      node
    end
  end

  def prewalk(nodes, fun) when is_list(nodes) do
    nodes = fun.(nodes)
    Enum.map(nodes, &prewalk(&1, fun))
  end

  def prewalk(node, fun) do
    fun.(node)
  end

  @doc """
  Performs a post-order traversal with an accumulator.
  """
  @spec postwalk(term(), acc, (term(), acc -> {term(), acc})) :: {term(), acc} when acc: term()
  def postwalk(node, acc, fun) when is_struct(node) do
    if Deft.Walkable.impl_for(node) do
      children = Walkable.children(node)

      {new_children, acc} =
        Enum.map_reduce(children, acc, fn child, acc ->
          postwalk(child, acc, fun)
        end)

      rebuilt = Walkable.rebuild(node, new_children)
      fun.(rebuilt, acc)
    else
      fun.(node, acc)
    end
  end

  def postwalk(nodes, acc, fun) when is_list(nodes) do
    {new_nodes, acc} =
      Enum.map_reduce(nodes, acc, fn node, acc ->
        postwalk(node, acc, fun)
      end)

    fun.(new_nodes, acc)
  end

  def postwalk(node, acc, fun) do
    fun.(node, acc)
  end
end

defmodule Deft.AST.Node do
  @moduledoc """
  Macro for defining AST nodes with automatic Walkable protocol implementation.

  ## Usage

      defmodule Deft.AST.MyNode do
        use Deft.AST.Node,
          fields: [:foo, :bar],
          children: [:foo]  # Only :foo is walked, :bar is metadata
      end

  This generates:
  - A struct with the specified fields plus :meta (unless no_meta: true)
  - A Walkable protocol implementation

  ## Options

  - `fields` - List of struct fields (required)
  - `children` - List of fields to walk (defaults to all fields)
  - `meta_default` - Default value for :meta field (defaults to [])
  - `no_meta` - If true, don't add automatic :meta field (for nodes with custom meta)
  """

  defmacro __using__(opts) do
    fields = Keyword.get(opts, :fields, [])
    children = Keyword.get(opts, :children, fields)
    meta_default = Keyword.get(opts, :meta_default, [])
    no_meta = Keyword.get(opts, :no_meta, false)

    struct_fields =
      if no_meta do
        fields
      else
        fields ++ [meta: meta_default]
      end

    enforce_keys = fields -- [:meta]

    quote do
      @enforce_keys unquote(enforce_keys)
      defstruct unquote(struct_fields)

      @doc "Returns the list of child field names for walking."
      def __children_fields__, do: unquote(children)

      defimpl Deft.Walkable do
        def children(node) do
          unquote(__MODULE__).__get_children__(node, unquote(children))
        end

        def rebuild(node, new_children) do
          unquote(__MODULE__).__rebuild__(node, unquote(children), new_children)
        end
      end
    end
  end

  @doc false
  def __get_children__(node, fields) do
    Enum.map(fields, fn field ->
      Map.get(node, field)
    end)
  end

  @doc false
  def __rebuild__(node, fields, new_children) do
    fields
    |> Enum.zip(new_children)
    |> Enum.reduce(node, fn {field, value}, acc ->
      Map.put(acc, field, value)
    end)
  end
end
