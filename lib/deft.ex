defmodule Deft do
  @moduledoc """
  Deft - Type Systems as Macros for Elixir.

  This module provides the main entry points for type-checked code blocks.
  It implements bidirectional type checking during macro expansion, then
  erases all type information to produce standard Elixir code.

  ## Usage

      require Deft

      {result, type} = Deft.compile do
        f = fn x :: integer -> x + 1 end
        f.(42)
      end

  ## Architecture

  Deft uses a rule-based extensible type system via `Deft.TypeChecker` and
  `Deft.Rule`. Custom typing rules can be added by implementing the `Deft.Rule`
  behaviour and registering them with the rule registry.
  """

  alias Deft.Compiler
  alias Deft.Context

  @doc """
  Compiles a typed code block, returning the result and its type.

  The block is type-checked at compile time, and all type annotations
  are erased before the code is executed.

  ## Example

      {result, type} = Deft.compile do
        x = 42
        x + 1
      end
      # result = 43
      # type = %Deft.Type.Integer{}
  """
  defmacro compile(do: block) do
    block = Compiler.compile(block)
    ctx = Context.new(__CALLER__)

    case Deft.TypeChecker.check(block, ctx) do
      {:ok, erased, type, _bindings, _ctx} ->
        {erased, Macro.escape(type)}

      {:error, reason} ->
        raise "Type checking failed: #{inspect(reason)}"
    end
  end

  @doc """
  Returns the type and bindings of a code block without executing it.

  Useful for inspecting the type structure of expressions.
  """
  defmacro bindings(do: block) do
    block = Compiler.compile(block)
    ctx = Context.new(__CALLER__)

    case Deft.TypeChecker.check(block, ctx) do
      {:ok, _erased, type, bindings, _ctx} ->
        Macro.escape({type, bindings})

      {:error, reason} ->
        raise "Type checking failed: #{inspect(reason)}"
    end
  end

  # ============================================================================
  # Module-level API (future)
  # ============================================================================

  @doc """
  Macro for enabling Deft in a module.

  This is a placeholder for future module-level typed function support.

  ## Future Usage

      defmodule MyMath do
        use Deft

        @deft add(integer, integer) :: integer
        def add(a, b), do: a + b
      end
  """
  defmacro __using__(_opts) do
    quote do
      import Deft, only: [compile: 1]
      require Deft
    end
  end
end
