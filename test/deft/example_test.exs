defmodule Deft.ExampleTest do
  use ExUnit.Case

  require Deft

  alias Deft.AST
  alias Deft.Type

  test "medium example" do
    {result, type} =
      Deft.compile do
        [alice, bob, eve] = [
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
             Type.fixed_list(
               Type.fixed_tuple([
                 Type.fixed_tuple([
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
             Type.fixed_tuple([
               Type.integer(),
               Type.integer(),
               Type.integer(),
               Type.integer()
             ])
  end

  test "ADTs" do
    {result, type} =
      Deft.compile do
        defdata(
          shape ::
            circle(float)
            | rectangle(float, float)
        )

        defdata(scene :: scene([shape]))

        classify = fn x :: shape ->
          case x do
            circle(_) ->
              :circle

            rectangle(width, width) ->
              :square

            rectangle(_, _) ->
              :rectangle
          end
        end

        c = circle(5.0)
        r = rectangle(2.0, 3.0)
        s = rectangle(6.0, 6.0)

        classify.(c)
        classify.(r)
        classify.(s)

        scene([c, r, s])
      end

    assert result ==
             {
               :scene,
               [
                 {:circle, 5.0},
                 {:rectangle, 2.0, 3.0},
                 {:rectangle, 6.0, 6.0}
               ]
             }

    assert type ==
             Type.adt(%AST.Local{context: nil, meta: [line: 103, column: 17], name: :scene}, [
               Type.variant(
                 :scene,
                 %AST.Local{context: nil, meta: [line: 103, column: 17], name: :scene},
                 [
                   Type.fixed_list(
                     Type.adt(
                       %AST.Local{context: nil, meta: [line: 98, column: 11], name: :shape},
                       [
                         Type.variant(
                           :circle,
                           %AST.Local{context: nil, meta: [line: 98, column: 11], name: :shape},
                           [
                             Type.float()
                           ]
                         ),
                         Type.variant(
                           :rectangle,
                           %AST.Local{context: nil, meta: [line: 98, column: 11], name: :shape},
                           [
                             Type.float(),
                             Type.float()
                           ]
                         )
                       ]
                     )
                   )
                 ]
               )
             ])
  end
end
