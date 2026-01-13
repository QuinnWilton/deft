defmodule Deft.Subtyping.DSL do
  @moduledoc """
  DSL for declaratively specifying subtyping relationships.

  ## Usage

      defmodule Deft.Type.Integer do
        use Deft.Subtyping.DSL

        subtype_of Deft.Type.Number

        defstruct []
      end

  ## Parameterized Types with Variance

  For parameterized types, declare parameters with their variance. A structural
  subtyping rule will be auto-generated:

      defmodule Deft.Type.FixedList do
        use Deft.Subtyping.DSL

        parameter :contents, variance: :covariant

        defstruct [:contents]
      end

  This generates:

      def structural_subtype?(sub, super) do
        Deft.Subtyping.subtype_of?(super.contents, sub.contents)
      end

  ## List Parameters with Arity Constraints

  For parameters that are lists (like tuple elements or function inputs), use
  `arity: :must_match` to require matching lengths with element-wise comparison:

      defmodule Deft.Type.Fn do
        use Deft.Subtyping.DSL

        parameter :inputs, variance: :contravariant, arity: :must_match
        parameter :output, variance: :covariant

        defstruct [:inputs, :output]
      end

  This generates:

      def structural_subtype?(sub, super) do
        length(sub.inputs) == length(super.inputs) and
          Enum.zip(sub.inputs, super.inputs)
          |> Enum.all?(fn {sub_elem, super_elem} ->
            Deft.Subtyping.subtype_of?(sub_elem, super_elem)  # contravariant
          end) and
          Deft.Subtyping.subtype_of?(super.output, sub.output)  # covariant
      end

  ## Manual Structural Rules

  For complex cases, you can still define a manual structural rule:

      structural_rule fn sub, super ->
        # Custom logic here
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

  ## Options

  - `:variance` - `:covariant`, `:contravariant`, or `:invariant` (default)
  - `:arity` - `:must_match` for list parameters that require matching lengths

  ## Examples

      parameter :contents, variance: :covariant
      parameter :inputs, variance: :contravariant, arity: :must_match
  """
  defmacro parameter(name, opts) do
    variance = Keyword.get(opts, :variance, :invariant)
    arity = Keyword.get(opts, :arity, nil)

    quote do
      @deft_parameters {unquote(name), unquote(variance), unquote(arity)}
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
    manual_structural_rule = Module.get_attribute(env.module, :deft_structural_rule)

    # Build parameter info for metadata (just name and variance)
    param_info =
      Enum.map(parameters, fn {name, variance, _arity} ->
        {name, variance}
      end)

    # Determine if we should auto-generate
    should_auto_generate = manual_structural_rule == nil and parameters != []
    has_structural_rule = manual_structural_rule != nil or should_auto_generate

    # Generate the structural_subtype? function
    structural_fun =
      cond do
        manual_structural_rule != nil ->
          quote do
            @doc false
            def structural_subtype?(sub, super) do
              fun = unquote(manual_structural_rule)
              fun.(sub, super)
            end
          end

        should_auto_generate ->
          generate_structural_fun(parameters)

        true ->
          nil
      end

    # Generate the check_subtype function for detailed error reporting
    check_subtype_fun =
      if should_auto_generate do
        generate_check_subtype_fun(parameters)
      end

    quote do
      @doc false
      def __subtyping_metadata__ do
        %{
          module: __MODULE__,
          supertypes: unquote(supertypes),
          parameters: unquote(param_info),
          has_structural_rule?: unquote(has_structural_rule),
          has_check_subtype?: unquote(should_auto_generate)
        }
      end

      unquote(structural_fun)
      unquote(check_subtype_fun)
    end
  end

  # ============================================================================
  # Structural Rule Generation
  # ============================================================================

  defp generate_structural_fun(parameters) do
    # Generate the body that checks all parameters
    checks =
      Enum.map(parameters, fn {name, variance, arity} ->
        generate_parameter_check(name, variance, arity)
      end)

    # Combine all checks with `and`
    body =
      case checks do
        [] ->
          quote do: true

        [single] ->
          single

        [first | rest] ->
          Enum.reduce(rest, first, fn check, acc ->
            quote do: unquote(acc) and unquote(check)
          end)
      end

    quote do
      @doc false
      def structural_subtype?(sub, super) do
        unquote(body)
      end
    end
  end

  # Single value parameter (no arity constraint)
  defp generate_parameter_check(name, variance, nil) do
    case variance do
      :covariant ->
        quote do
          Deft.Subtyping.subtype_of?(super.unquote(name), sub.unquote(name))
        end

      :contravariant ->
        quote do
          Deft.Subtyping.subtype_of?(sub.unquote(name), super.unquote(name))
        end

      :invariant ->
        quote do
          sub.unquote(name) == super.unquote(name)
        end
    end
  end

  # List parameter with arity constraint
  defp generate_parameter_check(name, variance, :must_match) do
    elementwise_check =
      case variance do
        :covariant ->
          quote do
            Deft.Subtyping.subtype_of?(super_elem, sub_elem)
          end

        :contravariant ->
          quote do
            Deft.Subtyping.subtype_of?(sub_elem, super_elem)
          end

        :invariant ->
          quote do
            sub_elem == super_elem
          end
      end

    quote do
      length(sub.unquote(name)) == length(super.unquote(name)) and
        Enum.zip(sub.unquote(name), super.unquote(name))
        |> Enum.all?(fn {sub_elem, super_elem} ->
          unquote(elementwise_check)
        end)
    end
  end

  # ============================================================================
  # Check Subtype Generation (Detailed Error Reporting)
  # ============================================================================

  defp generate_check_subtype_fun(parameters) do
    # For types with a single non-list parameter, expr is the expression
    # For types with list parameters, expr may be a list of expressions
    has_list_param = Enum.any?(parameters, fn {_, _, arity} -> arity == :must_match end)

    if has_list_param do
      generate_check_subtype_with_list_params(parameters)
    else
      generate_check_subtype_simple(parameters)
    end
  end

  # Simple case: all parameters are single values
  defp generate_check_subtype_simple(parameters) do
    # Chain checks, returning first mismatch
    checks = Enum.map(parameters, &generate_check_subtype_single_param/1)

    body =
      case checks do
        [] ->
          quote do: :ok

        [single] ->
          single

        checks ->
          # Chain with `with` to short-circuit on first mismatch
          Enum.reduce(Enum.reverse(checks), quote(do: :ok), fn check, acc ->
            quote do
              with :ok <- unquote(check) do
                unquote(acc)
              end
            end
          end)
      end

    quote do
      @doc false
      def check_subtype(sub, super, expr) do
        unquote(body)
      end
    end
  end

  defp generate_check_subtype_single_param({name, variance, nil}) do
    check_call =
      case variance do
        :covariant ->
          quote do
            Deft.Subtyping.check_subtype(super.unquote(name), sub.unquote(name), nil)
          end

        :contravariant ->
          quote do
            Deft.Subtyping.check_subtype(sub.unquote(name), super.unquote(name), nil)
          end

        :invariant ->
          quote do
            if sub.unquote(name) == super.unquote(name) do
              :ok
            else
              {:mismatch,
               %{
                 path: [unquote(name)],
                 expected: super.unquote(name),
                 actual: sub.unquote(name),
                 expr: nil
               }}
            end
          end
      end

    quote do
      case unquote(check_call) do
        :ok -> :ok
        {:mismatch, info} -> {:mismatch, %{info | expr: info.expr || expr}}
      end
    end
  end

  # Complex case: has list parameters that need element-wise checking with path tracking
  defp generate_check_subtype_with_list_params(parameters) do
    # Generate the main function and helper functions for list parameters
    {main_checks, helpers} =
      Enum.map_reduce(parameters, [], fn {name, variance, arity}, acc_helpers ->
        if arity == :must_match do
          helper_name = :"__check_#{name}_elements__"
          check = generate_list_param_check(name, variance, helper_name)
          helper = generate_list_param_helper(name, variance, helper_name)
          {check, [helper | acc_helpers]}
        else
          check = generate_check_subtype_single_param({name, variance, nil})
          {check, acc_helpers}
        end
      end)

    body =
      case main_checks do
        [] ->
          quote do: :ok

        [single] ->
          single

        checks ->
          Enum.reduce(Enum.reverse(checks), quote(do: :ok), fn check, acc ->
            quote do
              with :ok <- unquote(check) do
                unquote(acc)
              end
            end
          end)
      end

    quote do
      @doc false
      def check_subtype(sub, super, expr) do
        unquote(body)
      end

      unquote_splicing(helpers)
    end
  end

  defp generate_list_param_check(name, _variance, helper_name) do
    quote do
      if length(sub.unquote(name)) != length(super.unquote(name)) do
        {:mismatch,
         %{
           path: [],
           expected: super,
           actual: sub,
           expr: nil
         }}
      else
        exprs =
          case expr do
            list when is_list(list) -> list
            _ -> []
          end

        unquote(helper_name)(sub.unquote(name), super.unquote(name), exprs, 0)
      end
    end
  end

  defp generate_list_param_helper(name, variance, helper_name) do
    check_call =
      case variance do
        :covariant ->
          quote do
            Deft.Subtyping.check_subtype(super_elem, sub_elem, elem_expr)
          end

        :contravariant ->
          quote do
            Deft.Subtyping.check_subtype(sub_elem, super_elem, elem_expr)
          end

        :invariant ->
          quote do
            if sub_elem == super_elem do
              :ok
            else
              {:mismatch, %{path: [], expected: super_elem, actual: sub_elem, expr: elem_expr}}
            end
          end
      end

    # For function inputs, use :input in path; for tuple elements, just use index
    path_prefix =
      if name == :inputs do
        quote do: [:input, idx]
      else
        quote do: [idx]
      end

    quote do
      defp unquote(helper_name)([], [], _, _), do: :ok

      defp unquote(helper_name)(
             [sub_elem | sub_rest],
             [super_elem | super_rest],
             exprs,
             idx
           ) do
        {elem_expr, rest_exprs} =
          case exprs do
            [e | rest] -> {e, rest}
            [] -> {nil, []}
          end

        case unquote(check_call) do
          :ok ->
            unquote(helper_name)(sub_rest, super_rest, rest_exprs, idx + 1)

          {:mismatch, info} ->
            {:mismatch, %{info | path: unquote(path_prefix) ++ info.path}}
        end
      end
    end
  end
end
