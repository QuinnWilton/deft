defmodule Deft do
  alias Deft.Compiler

  defmacro compile(do: block) do
    block = Compiler.compile(block)

    {block, _type} =
      Deft.TypeChecking.compute_and_erase_types(
        block,
        __CALLER__
      )

    block
  end

  defmacro type(do: block) do
    block = Compiler.compile(block)

    {_block, type} =
      Deft.TypeChecking.compute_and_erase_types(
        block,
        __CALLER__
      )

    Macro.escape(type)
  end
end
