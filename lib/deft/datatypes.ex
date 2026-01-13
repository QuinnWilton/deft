defmodule Deft.Datatypes do
  @moduledoc """
  Base module for creating ADT libraries that can be shared across modules.

  ADT library modules define reusable algebraic data types that can be
  included in TypeSystems or individual deft modules.

  ## Usage

      defmodule MyApp.Datatypes do
        use Deft.Datatypes

        defdata option(a) :: some(a) | none
        defdata result(t, e) :: ok(t) | err(e)
      end

  Then include in a TypeSystem:

      defmodule MyApp.TypeSystem do
        use Deft.TypeSystem

        include_datatypes MyApp.Datatypes
      end

  Or include in an individual module:

      defmodule MyApp.Example do
        use Deft

        include_datatypes MyApp.Datatypes

        deft wrap(x :: integer) :: option(integer) do
          some(x)
        end
      end
  """

  alias Deft.AST
  alias Deft.Type

  @doc """
  Enables the Datatypes DSL in a module.

  Imports `defdata/1` for defining ADTs and sets up the module to export
  a `datatypes/0` function.
  """
  defmacro __using__(_opts) do
    quote do
      import Deft.Datatypes, only: [defdata: 1]
      Module.register_attribute(__MODULE__, :deft_datatype_definitions, accumulate: true)
      @before_compile Deft.Datatypes
    end
  end

  @doc """
  Define an ADT in a Datatypes module.

  Same syntax as in deft modules:

      defdata option(a) :: some(a) | none
      defdata result(t, e) :: ok(t) | err(e)
  """
  defmacro defdata({:"::", _, [name_with_params, variants]}) do
    {name, params} = extract_name_and_params(name_with_params)

    quote do
      @deft_datatype_definitions {
        unquote(name),
        unquote(params),
        unquote(Macro.escape(variants))
      }
    end
  end

  defp extract_name_and_params({name, _, params}) when is_list(params) do
    param_names = Enum.map(params, fn {p, _, _} -> p end)
    {name, param_names}
  end

  defp extract_name_and_params({name, _, _}) do
    {name, []}
  end

  @doc false
  defmacro __before_compile__(env) do
    defs = Module.get_attribute(env.module, :deft_datatype_definitions) || []

    quote do
      @doc """
      Returns all ADT definitions as a map of name => Type.ADT.
      """
      @spec datatypes() :: %{atom() => Deft.Type.t()}
      def datatypes do
        unquote(Macro.escape(defs))
        |> Enum.map(&Deft.Datatypes.build_adt/1)
        |> Map.new()
      end
    end
  end

  @doc false
  def build_adt({name, params, variants_ast}) do
    variants = build_variants(variants_ast, name, params)
    name_local = AST.Local.new(name, nil)
    adt = Type.adt(name_local, variants, params)
    {name, adt}
  end

  defp build_variants({:|, _, [left, right]}, adt_name, params) do
    [build_variant(left, adt_name, params) | build_variants(right, adt_name, params)]
  end

  defp build_variants(variant, adt_name, params) do
    [build_variant(variant, adt_name, params)]
  end

  defp build_variant({name, _, columns}, adt_name, params) do
    columns = columns || []

    column_types =
      Enum.map(columns, fn col ->
        {:ok, type} = Deft.TypeParser.parse(col, output: :type, allow_variables: params != [])
        type
      end)

    name_local = AST.Local.new(adt_name, nil)
    Type.variant(name, name_local, column_types)
  end
end
