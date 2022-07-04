defmodule Deft.ExampleTest do
  use ExUnit.Case

  require Deft

  alias Deft.Type

  test "example type checks" do
    type =
      Deft.type do
        people = [
          {:alice, :f, 24},
          {:bob, :m, 15},
          {:eve, :f, 17}
        ]

        category = fn p :: {atom, atom, integer} ->
          cond do
            elem(p, 0) == :eve ->
              :hacker

            elem(p, 2) >= 18 ->
              if elem(p, 1) == :m do
                :male_adult
              else
                :female_adult
              end

            true ->
              :minor
          end
        end

        alice = hd(people)
        bob = hd(tl(people))
        eve = hd(tl(tl(people)))

        [
          {alice, category.(alice)},
          {bob, category.(bob)},
          {eve, category.(eve)}
        ]
      end

    assert Type.list(
             Type.union([
               Type.tuple([
                 Type.union([
                   Type.tuple([
                     Type.atom(),
                     Type.atom(),
                     Type.integer()
                   ])
                 ]),
                 Type.union([
                   Type.Atom.new()
                 ])
               ])
             ])
           ) == type
  end
end
