defmodule Deft do
  alias Deft.Compiler

  defmacro compile(do: block) do
    block = Compiler.compile(block)

    {block, type, _bindings} =
      Deft.Helpers.compute_and_erase_types(
        block,
        __CALLER__
      )

    {block, Macro.escape(type)}
  end

  defmacro bindings(do: block) do
    block = Compiler.compile(block)

    {_block, type, bindings} =
      Deft.Helpers.compute_and_erase_types(
        block,
        __CALLER__
      )

    Macro.escape({type, bindings})
  end
end
