defmodule Deft.Generators do
  @moduledoc """
  Entry point for Deft's property-based test generators.

  Provides generators for types and well-typed code expressions,
  used in property tests to verify type system invariants.
  """

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
