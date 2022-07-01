defmodule DeftTest do
  use ExUnit.Case
  doctest Deft

  test "greets the world" do
    assert Deft.hello() == :world
  end
end
