defmodule DeftTest do
  use ExUnit.Case
  use ExUnitProperties

  import StreamData

  require Deft

  alias Deft.Type

  def compile(ast) do
    {result, _} =
      Code.eval_quoted(
        quote do
          require Deft
          Deft.compile(unquote(ast))
        end
      )

    result
  end

  def get_type(ast) do
    {type, _} =
      Code.eval_quoted(
        quote do
          require Deft
          Deft.type(unquote(ast))
        end
      )

    type
  end

  property "compile/1 succeeds when the AST is correctly typed" do
    check(
      all(
        fn_type <- fn_type(),
        fn_code <- inhabitant_of(fn_type),
        args <- fixed_list(Enum.map(fn_type.inputs, &inhabitant_of/1))
      ) do
        ast =
          quote do
            unquote(fn_code).(unquote_splicing(args))
          end

        compile(ast)
      end
    )
  end

  property "type/1 returns the type of an expression" do
    check all(
            type <- type(),
            ast <- inhabitant_of(type)
          ) do
      assert Type.subtype_of?(type, get_type(ast))
    end
  end

  def type() do
    one_of([
      primitive_type(),
      compound_type()
    ])
  end

  def primitive_type() do
    one_of([
      atom_type(),
      boolean_type(),
      float_type(),
      integer_type(),
      number_type(),
      top_type()
    ])
  end

  def compound_type() do
    one_of([
      fn_type(),
      tuple_type(),
      union_type()
    ])
  end

  def atom_type() do
    constant(Type.Atom.new())
  end

  def boolean_type() do
    constant(Type.Boolean.new())
  end

  def bottom_type() do
    constant(Type.Bottom.new())
  end

  def float_type() do
    constant(Type.Float.new())
  end

  def fn_type() do
    bind(list_of(primitive_type(), max_length: 8), fn inputs ->
      map(primitive_type(), fn output ->
        Type.Fn.new(inputs, output)
      end)
    end)
  end

  def tuple_type() do
    bind(list_of(primitive_type(), max_length: 8), fn elements ->
      constant(Type.Tuple.new(elements))
    end)
  end

  def union_type() do
    bind(list_of(primitive_type(), min_length: 1, max_length: 8), fn elements ->
      constant(Type.Union.new(elements))
    end)
  end

  def integer_type() do
    constant(Type.Integer.new())
  end

  def number_type() do
    constant(Type.Number.new())
  end

  def top_type() do
    constant(Type.Top.new())
  end

  def inhabitant_of(type) do
    case type do
      %Type.Atom{} ->
        atom_inhabitant(type)

      %Type.Boolean{} ->
        boolean_inhabitant(type)

      %Type.Float{} ->
        float_inhabitant(type)

      %Type.Fn{} ->
        fn_inhabitant(type)

      %Type.Integer{} ->
        integer_inhabitant(type)

      %Type.Number{} ->
        number_inhabitant(type)

      %Type.Top{} ->
        top_inhabitant(type)

      %Type.Tuple{} ->
        tuple_inhabitant(type)

      %Type.Union{} ->
        union_inhabitant(type)
    end
  end

  def atom_inhabitant(_) do
    filter(atom(:alphanumeric), &allowed_atom?/1)
  end

  def boolean_inhabitant(_) do
    boolean()
  end

  def float_inhabitant(_) do
    float()
  end

  def fn_inhabitant(type) do
    args =
      length(type.inputs)
      |> Macro.generate_unique_arguments(__MODULE__)
      |> Enum.zip(type.inputs)
      |> Enum.map(fn {x, t} ->
        {:"::", [], [x, annotation_for(t)]}
      end)

    map(inhabitant_of(type.output), fn body ->
      quote do
        fn unquote_splicing(args) ->
          unquote(body)
        end
      end
    end)
  end

  def integer_inhabitant(_) do
    integer()
  end

  def number_inhabitant(type) do
    one_of([
      float_inhabitant(type),
      integer_inhabitant(type)
    ])
  end

  def top_inhabitant(type) do
    one_of([
      atom_inhabitant(type),
      boolean_inhabitant(type),
      float_inhabitant(type),
      integer_inhabitant(type),
      number_inhabitant(type)
    ])
  end

  def tuple_inhabitant(type) do
    type.elements
    |> Enum.map(&inhabitant_of/1)
    |> List.to_tuple()
    |> tuple()
    |> map(fn tuple ->
      Macro.escape(tuple)
    end)
  end

  def union_inhabitant(type) do
    type.elements
    |> Enum.map(&inhabitant_of/1)
    |> one_of()
  end

  def annotation_for(type) do
    case type do
      %Type.Atom{} ->
        {:atom, [], nil}

      %Type.Boolean{} ->
        {:boolean, [], nil}

      %Type.Bottom{} ->
        {:bottom, [], nil}

      %Type.Float{} ->
        {:float, [], nil}

      %Type.Fn{} ->
        inputs =
          length(type.inputs)
          |> Macro.generate_unique_arguments(__MODULE__)
          |> Enum.zip(type.inputs)
          |> Enum.map(fn {x, t} ->
            {:"::", [], [x, annotation_for(t)]}
          end)

        output = annotation_for(type.output)

        quote do
          unquote_splicing(inputs) ->
            unquote(output)
        end

      %Type.Integer{} ->
        {:integer, [], nil}

      %Type.Number{} ->
        {:number, [], nil}

      %Type.Top{} ->
        {:top, [], nil}

      %Type.Tuple{} ->
        type.elements
        |> Enum.map(&annotation_for/1)
        |> List.to_tuple()
        |> Macro.escape()

      %Type.Union{} ->
        Enum.reduce(type.elements, fn t, acc ->
          {:|, [], [t, acc]}
        end)
    end
  end

  def allowed_atom?(atom) do
    atom not in [nil, true, false]
  end
end
