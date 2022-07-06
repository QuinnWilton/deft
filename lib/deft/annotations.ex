defmodule Deft.Annotations do
  alias Deft.Type

  def parse({:boolean, _, _}) do
    Type.boolean()
  end

  def parse({:atom, _, _}) do
    Type.atom()
  end

  def parse({:float, _, _}) do
    Type.float()
  end

  def parse({:integer, _, _}) do
    Type.integer()
  end

  def parse({:number, _, _}) do
    Type.number()
  end

  def parse({:top, _, _}) do
    Type.top()
  end

  def parse({:bottom, _, _}) do
    Type.bottom()
  end

  def parse({elem0, elem1}) do
    elem0 = parse(elem0)
    elem1 = parse(elem1)

    Type.fixed_tuple([elem0, elem1])
  end

  def parse({:{}, _, elements}) do
    elements = Enum.map(elements, &parse/1)

    Type.fixed_tuple(elements)
  end

  def parse({:|, _, [t1, t2]}) do
    t1 = parse(t1)
    t2 = parse(t2)

    Type.union([t1, t2])
  end

  def parse([{:->, _, [inputs, output]}]) do
    inputs = Enum.map(inputs, &parse/1)
    output = parse(output)

    Type.fun(inputs, output)
  end

  def parse([type]) do
    Type.list(parse(type))
  end
end
