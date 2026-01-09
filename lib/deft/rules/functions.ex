defmodule Deft.Rules.Functions do
  @moduledoc """
  Typing rules for function definitions and applications.
  """

  alias Deft.AST
  alias Deft.Subtyping
  alias Deft.Type
  alias Deft.TypeChecker

  # Anonymous function rule
  defmodule Fn do
    @behaviour Deft.Rule

    @impl true
    def name, do: :fn

    @impl true
    def judgment, do: :synth

    @impl true
    def matches?(%AST.Fn{}), do: true
    def matches?(_), do: false

    @impl true
    def apply(
          %AST.Fn{args: args, body: body, fn_meta: fn_meta, arrow_meta: arrow_meta},
          _expected,
          ctx
        ) do
      # Type check arguments (which should be annotations)
      {erased_args, input_types, arg_bindings, ctx} = check_args(args, ctx)

      # Type check body with argument bindings injected into the AST
      {:ok, erased_body, output_type, _body_bindings, _ctx} =
        TypeChecker.check_in_context(body, arg_bindings, ctx)

      type = Type.fun(input_types, output_type)
      erased = {:fn, fn_meta, [{:->, arrow_meta, [erased_args, erased_body]}]}

      {:ok, erased, type, [], ctx}
    end

    defp check_args(args, ctx) do
      {erased, types, bindings} =
        Enum.reduce(args, {[], [], []}, fn arg, {acc_erased, acc_types, acc_bindings} ->
          {:ok, erased, type, arg_bindings, _ctx} = TypeChecker.check(arg, ctx)
          {acc_erased ++ [erased], acc_types ++ [type], acc_bindings ++ arg_bindings}
        end)

      {erased, types, bindings, ctx}
    end
  end

  # Function application rule
  defmodule FnApplication do
    @behaviour Deft.Rule

    @impl true
    def name, do: :fn_application

    @impl true
    def judgment, do: :synth

    @impl true
    def matches?(%AST.FnApplication{}), do: true
    def matches?(_), do: false

    @impl true
    def apply(
          %AST.FnApplication{fun: fun, args: args, fun_meta: fun_meta, args_meta: args_meta},
          _expected,
          ctx
        ) do
      # Type check the function
      {:ok, erased_fun, fun_type, fun_bindings, ctx} = TypeChecker.check(fun, ctx)

      # Type check arguments
      {erased_args, arg_types, arg_bindings} = check_args(args, ctx)

      # Validate function type and arguments
      case fun_type do
        %Type.Fn{inputs: inputs, output: output} ->
          if length(inputs) == length(arg_types) and Subtyping.subtypes_of?(inputs, arg_types) do
            erased = {{:., fun_meta, [erased_fun]}, args_meta, erased_args}
            bindings = fun_bindings ++ arg_bindings
            {:ok, erased, output, bindings, ctx}
          else
            {:error, %Deft.TypecheckingError{expected: inputs, actual: arg_types}}
          end

        _ ->
          {:error, {:not_a_function, fun_type}}
      end
    end

    defp check_args(args, ctx) do
      {erased, types, bindings} =
        Enum.reduce(args, {[], [], []}, fn arg, {acc_erased, acc_types, acc_bindings} ->
          {:ok, erased, type, arg_bindings, _ctx} = TypeChecker.check(arg, ctx)
          {acc_erased ++ [erased], acc_types ++ [type], acc_bindings ++ arg_bindings}
        end)

      {erased, types, bindings}
    end
  end

  @doc """
  Returns all function rules.
  """
  def rules do
    [Fn, FnApplication]
  end
end
