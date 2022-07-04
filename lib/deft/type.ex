defmodule Deft.Type do
  alias Deft.Type

  def atom() do
    Type.Atom.new()
  end

  def boolean() do
    Type.Boolean.new()
  end

  def bottom() do
    Type.Bottom.new()
  end

  def float() do
    Type.Float.new()
  end

  def fun(inputs, outputs) do
    Type.Fn.new(inputs, outputs)
  end

  def integer() do
    Type.Integer.new()
  end

  def list(contents) do
    Type.List.new(contents)
  end

  def number() do
    Type.Number.new()
  end

  def top() do
    Type.Top.new()
  end

  def tuple(elements) do
    Type.Tuple.new(elements)
  end

  def union(elements) do
    Type.Union.new(elements)
  end
end
