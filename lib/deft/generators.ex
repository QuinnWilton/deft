defmodule Deft.Generators do
  alias Deft.Generators

  def code() do
    Generators.Code.code()
  end

  def type() do
    Generators.Types.type()
  end

  def primitive_type() do
    Generators.Types.primitive_type()
  end

  def compound_type() do
    Generators.Types.compound_type()
  end
end
