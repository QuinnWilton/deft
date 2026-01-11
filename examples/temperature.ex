# Temperature conversion example using Deft type checking
# Demonstrates typed conversion functions

defmodule Examples.Temperature do
  @moduledoc """
  Demonstrates type-checked temperature conversions with the deft macro.
  Uses integer arithmetic (tenths of degrees) for precision without floats.
  """

  use Deft

  # Celsius to Fahrenheit: F = C * 9/5 + 32
  # Using tenths: input is C*10, output is F*10
  deft celsius_to_fahrenheit(celsius_tenths :: integer) :: integer do
    div(celsius_tenths * 9, 5) + 320
  end

  # Fahrenheit to Celsius: C = (F - 32) * 5/9
  # Using tenths: input is F*10, output is C*10
  deft fahrenheit_to_celsius(fahrenheit_tenths :: integer) :: integer do
    div((fahrenheit_tenths - 320) * 5, 9)
  end

  # Celsius to Kelvin: K = C + 273
  # Using tenths: input is C*10, output is K*10
  deft celsius_to_kelvin(celsius_tenths :: integer) :: integer do
    celsius_tenths + 2730
  end

  # Kelvin to Celsius: C = K - 273
  # Using tenths: input is K*10, output is C*10
  deft kelvin_to_celsius(kelvin_tenths :: integer) :: integer do
    kelvin_tenths - 2730
  end

  # Typed main functions for each conversion
  # Input is whole degrees, converted to tenths internally, output is whole degrees
  deft main_c_to_f(value :: integer) :: atom do
    result = celsius_to_fahrenheit(value * 10)
    IO.puts(div(result, 10))
  end

  deft main_f_to_c(value :: integer) :: atom do
    result = fahrenheit_to_celsius(value * 10)
    IO.puts(div(result, 10))
  end

  deft main_c_to_k(value :: integer) :: atom do
    result = celsius_to_kelvin(value * 10)
    IO.puts(div(result, 10))
  end

  deft main_k_to_c(value :: integer) :: atom do
    result = kelvin_to_celsius(value * 10)
    IO.puts(div(result, 10))
  end
end

# Only run CLI when invoked directly with valid arguments
case System.argv() do
  ["c_to_f", value] ->
    Examples.Temperature.main_c_to_f(String.to_integer(value))

  ["f_to_c", value] ->
    Examples.Temperature.main_f_to_c(String.to_integer(value))

  ["c_to_k", value] ->
    Examples.Temperature.main_c_to_k(String.to_integer(value))

  ["k_to_c", value] ->
    Examples.Temperature.main_k_to_c(String.to_integer(value))

  _ ->
    :ok
end
