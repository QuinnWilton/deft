defmodule Deft.Subtyping.DSL do
  @moduledoc """
  DSL for declaratively specifying subtyping relationships.

  ## Usage

      defmodule Deft.Type.Integer do
        use Deft.Subtyping.DSL

        subtype_of Deft.Type.Number

        defstruct []
      end

      defmodule Deft.Type.Fn do
        use Deft.Subtyping.DSL

        parameter :inputs, variance: :contravariant
        parameter :output, variance: :covariant

        structural_rule fn sub, super ->
          # Custom subtyping logic for parameterized types
          length(sub.inputs) == length(super.inputs) and
            Deft.Subtyping.subtype_of?(super.output, sub.output)
        end

        defstruct [:inputs, :output]
      end
  """

  defmacro __using__(_opts) do
    quote do
      import Deft.Subtyping.DSL, only: [subtype_of: 1, parameter: 2, structural_rule: 1]

      Module.register_attribute(__MODULE__, :deft_supertypes, accumulate: true)
      Module.register_attribute(__MODULE__, :deft_parameters, accumulate: true)
      Module.register_attribute(__MODULE__, :deft_structural_rule, accumulate: false)

      @before_compile Deft.Subtyping.DSL
    end
  end

  @doc """
  Declares that this type is a subtype of another type.

      subtype_of Deft.Type.Number
  """
  defmacro subtype_of(supertype) do
    quote do
      @deft_supertypes unquote(supertype)
    end
  end

  @doc """
  Declares a parameter with its variance.

      parameter :contents, variance: :covariant
      parameter :inputs, variance: :contravariant
  """
  defmacro parameter(name, opts) do
    variance = Keyword.get(opts, :variance, :invariant)

    quote do
      @deft_parameters {unquote(name), unquote(variance)}
    end
  end

  @doc """
  Defines a structural subtyping rule for this type.

  The function receives two arguments: the potential subtype and supertype.
  It should return true if the first is a subtype of the second.

      structural_rule fn sub, super ->
        Deft.Subtyping.subtype_of?(super.contents, sub.contents)
      end
  """
  defmacro structural_rule(fun) do
    quote do
      @deft_structural_rule unquote(Macro.escape(fun))
    end
  end

  defmacro __before_compile__(env) do
    supertypes = Module.get_attribute(env.module, :deft_supertypes) || []
    parameters = Module.get_attribute(env.module, :deft_parameters) || []
    structural_rule = Module.get_attribute(env.module, :deft_structural_rule)

    # Build parameter info as keyword list
    param_info =
      Enum.map(parameters, fn {name, variance} ->
        {name, variance}
      end)

    # Generate the structural_subtype? function if a rule was defined
    structural_fun =
      if structural_rule do
        quote do
          @doc false
          def structural_subtype?(sub, super) do
            fun = unquote(structural_rule)
            fun.(sub, super)
          end
        end
      else
        quote do
          @doc false
          def structural_subtype?(_sub, _super), do: nil
        end
      end

    quote do
      @doc false
      def __subtyping_metadata__ do
        %{
          module: __MODULE__,
          supertypes: unquote(supertypes),
          parameters: unquote(param_info),
          has_structural_rule?: unquote(structural_rule != nil)
        }
      end

      unquote(structural_fun)
    end
  end
end
