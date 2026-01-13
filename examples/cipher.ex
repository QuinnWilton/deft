# Caesar cipher example using Deft type checking

defmodule Examples.Cipher do
  @moduledoc """
  A Caesar cipher encoder/decoder that demonstrates type-checked
  data processing through String, Enum, and List operations.

  The cipher shifts each letter by a fixed amount, wrapping around
  the alphabet.
  """

  use Deft

  # Encode a message by shifting each letter forward
  deft encode(message :: binary, shift :: integer) :: binary do
    chars = String.to_charlist(message)
    shifted = Enum.map(chars, fn c :: integer -> shift_char(c, shift) end)
    List.to_string(shifted)
  end

  # Decode by shifting backward (negative shift)
  deft decode(message :: binary, shift :: integer) :: binary do
    encode(message, 0 - shift)
  end

  # Shift a single character, preserving case and non-letters
  deft shift_char(char :: integer, shift :: integer) :: integer do
    cond do
      char >= 65 and char <= 90 ->
        # Uppercase A-Z: shift within 65-90
        base = char - 65
        shifted = rem(base + shift + 26, 26)
        shifted + 65

      char >= 97 and char <= 122 ->
        # Lowercase a-z: shift within 97-122
        base = char - 97
        shifted = rem(base + shift + 26, 26)
        shifted + 97

      true ->
        # Non-letter: preserve as-is (spaces, punctuation)
        char
    end
  end

  # ROT13 is a special case: shift by 13 (self-inverse)
  deft rot13(message :: binary) :: binary do
    encode(message, 13)
  end

  # Count letters in message (filtering non-letters)
  deft letter_count(message :: binary) :: integer do
    chars = String.to_charlist(message)

    letters =
      Enum.filter(chars, fn c :: integer ->
        (c >= 65 and c <= 90) or (c >= 97 and c <= 122)
      end)

    length(letters)
  end

  # Reverse word order in message
  deft reverse_words(message :: binary) :: binary do
    words = String.split(message)
    reversed = Enum.reverse(words)
    Enum.join(reversed, " ")
  end

  # Entry points for CLI
  deft main_encode(message :: binary, shift :: integer) :: atom do
    result = encode(message, shift)
    IO.puts(result)
  end

  deft main_decode(message :: binary, shift :: integer) :: atom do
    result = decode(message, shift)
    IO.puts(result)
  end

  deft main_rot13(message :: binary) :: atom do
    result = rot13(message)
    IO.puts(result)
  end
end

# CLI handler
case System.argv() do
  ["encode", message, shift] ->
    Examples.Cipher.main_encode(message, String.to_integer(shift))

  ["decode", message, shift] ->
    Examples.Cipher.main_decode(message, String.to_integer(shift))

  ["rot13", message] ->
    Examples.Cipher.main_rot13(message)

  _ ->
    :ok
end
