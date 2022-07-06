defmodule Deft.ExampleTest do
  use ExUnit.Case

  require Deft

  alias Deft.Type

  test "medium example" do
    {result, type} =
      Deft.compile do
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
        [_, bob | eve_list = [_]] = people
        eve = hd(eve_list)

        [
          {alice, category.(alice)},
          {bob, category.(bob)},
          {eve, category.(eve)}
        ]
      end

    assert result == [
             {{:alice, :f, 24}, :female_adult},
             {{:bob, :m, 15}, :minor},
             {{:eve, :f, 17}, :hacker}
           ]

    assert type ==
             Type.list(
               Type.tuple([
                 Type.tuple([
                   Type.atom(),
                   Type.atom(),
                   Type.integer()
                 ]),
                 Type.Atom.new()
               ])
             )
  end

  test "pattern matching" do
    {result, type} =
      Deft.compile do
        f = fn x :: [integer] ->
          case x do
            [] ->
              0

            [x] ->
              x

            [x, y] ->
              x + y

            [x, y | rest] ->
              x + y + length(rest)
          end
        end

        {f.([]), f.([1]), f.([1, 2]), f.([1, 2, 3])}
      end

    assert result == {0, 1, 3, 4}

    assert type ==
             Type.tuple([
               Type.integer(),
               Type.integer(),
               Type.integer(),
               Type.integer()
             ])
  end
end
