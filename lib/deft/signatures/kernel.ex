defmodule Deft.Signatures.Kernel do
  @moduledoc """
  Type signatures for Kernel module functions.

  These are the type signatures for Elixir's standard library
  functions that are commonly used in typed code.
  """

  use Deft.Signatures.DSL, for: Kernel

  # ============================================================================
  # Arithmetic operators
  # ============================================================================

  sig :+, [number, number] :: number
  sig :-, [number, number] :: number
  sig :*, [number, number] :: number
  sig :/, [number, number] :: float

  # Unary arithmetic
  sig :+, [number] :: number
  sig :-, [number] :: number

  # Integer division and remainder
  sig div(integer, integer) :: integer
  sig rem(integer, integer) :: integer

  # ============================================================================
  # Comparison operators
  # ============================================================================

  sig :==, [top, top] :: boolean
  sig :!=, [top, top] :: boolean
  sig :===, [top, top] :: boolean
  sig :!==, [top, top] :: boolean
  sig :<, [top, top] :: boolean
  sig :<=, [top, top] :: boolean
  sig :>, [top, top] :: boolean
  sig :>=, [top, top] :: boolean

  # ============================================================================
  # Boolean operators
  # ============================================================================

  sig not boolean :: boolean
  sig :and, [boolean, boolean] :: boolean
  sig :or, [boolean, boolean] :: boolean

  # ============================================================================
  # Math functions
  # ============================================================================

  sig abs(number) :: number
  sig ceil(number) :: integer
  sig floor(number) :: integer
  sig round(number) :: integer
  sig trunc(number) :: integer
  sig max(number, number) :: number
  sig min(number, number) :: number

  # ============================================================================
  # Type guards
  # ============================================================================

  sig is_atom(top) :: boolean
  sig is_boolean(top) :: boolean
  sig is_integer(top) :: boolean
  sig is_float(top) :: boolean
  sig is_number(top) :: boolean
  sig is_list(top) :: boolean
  sig is_tuple(top) :: boolean
  sig is_function(top) :: boolean
  sig is_function(top, integer) :: boolean
  sig is_binary(top) :: boolean
  sig is_map(top) :: boolean
  sig is_nil(top) :: boolean

  # ============================================================================
  # Polymorphic list operations
  # ============================================================================

  sig hd([a]) :: a
  sig tl([a]) :: [a]
  sig length([a]) :: integer
  sig :++, [[a], [b]] :: [a | b]
  sig :--, [[a], [b]] :: [a]

  # ============================================================================
  # Tuple operations
  # ============================================================================

  sig elem(tuple, integer) :: top
  sig tuple_size(tuple) :: integer
  sig put_elem(tuple, integer, top) :: tuple
end
