defmodule Deft.Integration.CLITest do
  @moduledoc """
  Integration tests for example modules.

  Tests call the example module functions directly and capture IO output,
  avoiding the overhead of spawning separate VMs via `mix run`.
  """
  use ExUnit.Case, async: true

  import ExUnit.CaptureIO

  @examples_dir Path.expand("../../../examples", __DIR__)

  # Compile example modules once before all tests
  setup_all do
    # Compile each example file (suppress the CLI output from top-level code)
    for file <- ~w[arithmetic.ex temperature.ex control_flow.ex higher_order.ex geometry.ex] do
      ExUnit.CaptureIO.capture_io(fn ->
        Code.compile_file(Path.join(@examples_dir, file))
      end)
    end

    :ok
  end

  describe "arithmetic" do
    test "add" do
      assert run(fn -> Examples.Arithmetic.main("add", 10, 20) end) == "30"
      assert run(fn -> Examples.Arithmetic.main("add", -5, 3) end) == "-2"
    end

    test "subtract" do
      assert run(fn -> Examples.Arithmetic.main("sub", 100, 42) end) == "58"
      assert run(fn -> Examples.Arithmetic.main("sub", 5, 10) end) == "-5"
    end

    test "multiply" do
      assert run(fn -> Examples.Arithmetic.main("mul", 6, 7) end) == "42"
      assert run(fn -> Examples.Arithmetic.main("mul", -3, 4) end) == "-12"
    end

    test "divide" do
      assert run(fn -> Examples.Arithmetic.main("div", 100, 7) end) == "14"
      assert run(fn -> Examples.Arithmetic.main("div", 42, 6) end) == "7"
    end

    test "modulo" do
      assert run(fn -> Examples.Arithmetic.main("mod", 17, 5) end) == "2"
      assert run(fn -> Examples.Arithmetic.main("mod", 100, 10) end) == "0"
    end
  end

  describe "geometry" do
    test "rectangle area" do
      assert run(fn -> Examples.Geometry.main(["rectangle", "area", "5", "10"]) end) == "50"
      assert run(fn -> Examples.Geometry.main(["rectangle", "area", "3", "7"]) end) == "21"
    end

    test "rectangle perimeter" do
      assert run(fn -> Examples.Geometry.main(["rectangle", "perimeter", "5", "10"]) end) == "30"
      assert run(fn -> Examples.Geometry.main(["rectangle", "perimeter", "4", "6"]) end) == "20"
    end

    test "square area" do
      assert run(fn -> Examples.Geometry.main(["square", "area", "5"]) end) == "25"
      assert run(fn -> Examples.Geometry.main(["square", "area", "12"]) end) == "144"
    end

    test "square perimeter" do
      assert run(fn -> Examples.Geometry.main(["square", "perimeter", "8"]) end) == "32"
      assert run(fn -> Examples.Geometry.main(["square", "perimeter", "5"]) end) == "20"
    end

    test "triangle area" do
      assert run(fn -> Examples.Geometry.main(["triangle", "area", "10", "6"]) end) == "30.0"
      assert run(fn -> Examples.Geometry.main(["triangle", "area", "8", "4"]) end) == "16.0"
    end
  end

  describe "temperature" do
    test "celsius to fahrenheit" do
      assert run(fn -> Examples.Temperature.main_c_to_f(0) end) == "32"
      assert run(fn -> Examples.Temperature.main_c_to_f(100) end) == "212"
      assert run(fn -> Examples.Temperature.main_c_to_f(-40) end) == "-40"
    end

    test "fahrenheit to celsius" do
      assert run(fn -> Examples.Temperature.main_f_to_c(32) end) == "0"
      assert run(fn -> Examples.Temperature.main_f_to_c(212) end) == "100"
      assert run(fn -> Examples.Temperature.main_f_to_c(-40) end) == "-40"
    end

    test "celsius to kelvin" do
      assert run(fn -> Examples.Temperature.main_c_to_k(0) end) == "273"
      assert run(fn -> Examples.Temperature.main_c_to_k(100) end) == "373"
    end

    test "kelvin to celsius" do
      assert run(fn -> Examples.Temperature.main_k_to_c(273) end) == "0"
      assert run(fn -> Examples.Temperature.main_k_to_c(373) end) == "100"
    end
  end

  describe "higher_order" do
    test "twice (apply function twice)" do
      assert run(fn -> Examples.HigherOrder.main_twice(5) end) == "20"
      assert run(fn -> Examples.HigherOrder.main_twice(10) end) == "40"
      assert run(fn -> Examples.HigherOrder.main_twice(0) end) == "0"
    end

    test "compose (function composition)" do
      assert run(fn -> Examples.HigherOrder.main_compose(5) end) == "11"
      assert run(fn -> Examples.HigherOrder.main_compose(10) end) == "21"
      assert run(fn -> Examples.HigherOrder.main_compose(0) end) == "1"
    end

    test "map (apply to pair)" do
      assert run(fn -> Examples.HigherOrder.main_map(3, 4) end) == "6,8"
      assert run(fn -> Examples.HigherOrder.main_map(5, 10) end) == "10,20"
      assert run(fn -> Examples.HigherOrder.main_map(0, 7) end) == "0,14"
    end

    test "fold_add (fold pair with addition)" do
      assert run(fn -> Examples.HigherOrder.main_fold_add(10, 20) end) == "30"
      assert run(fn -> Examples.HigherOrder.main_fold_add(7, 8) end) == "15"
      assert run(fn -> Examples.HigherOrder.main_fold_add(100, 1) end) == "101"
    end

    test "fold_mul (fold pair with multiplication)" do
      assert run(fn -> Examples.HigherOrder.main_fold_mul(6, 7) end) == "42"
      assert run(fn -> Examples.HigherOrder.main_fold_mul(5, 5) end) == "25"
      assert run(fn -> Examples.HigherOrder.main_fold_mul(10, 0) end) == "0"
    end

    test "curry (curried addition)" do
      assert run(fn -> Examples.HigherOrder.main_curry(7, 8) end) == "15"
      assert run(fn -> Examples.HigherOrder.main_curry(100, 200) end) == "300"
      assert run(fn -> Examples.HigherOrder.main_curry(-5, 10) end) == "5"
    end
  end

  describe "control_flow" do
    test "abs (absolute value)" do
      assert run(fn -> Examples.ControlFlow.main_abs(42) end) == "42"
      assert run(fn -> Examples.ControlFlow.main_abs(-42) end) == "42"
      assert run(fn -> Examples.ControlFlow.main_abs(0) end) == "0"
    end

    test "clamp (clamp to range)" do
      assert run(fn -> Examples.ControlFlow.main_clamp(50, 0, 100) end) == "50"
      assert run(fn -> Examples.ControlFlow.main_clamp(-10, 0, 100) end) == "0"
      assert run(fn -> Examples.ControlFlow.main_clamp(150, 0, 100) end) == "100"
      assert run(fn -> Examples.ControlFlow.main_clamp(5, 10, 20) end) == "10"
    end

    test "max (maximum of two)" do
      assert run(fn -> Examples.ControlFlow.main_max(10, 20) end) == "20"
      assert run(fn -> Examples.ControlFlow.main_max(50, 30) end) == "50"
      assert run(fn -> Examples.ControlFlow.main_max(7, 7) end) == "7"
    end

    test "min (minimum of two)" do
      assert run(fn -> Examples.ControlFlow.main_min(10, 20) end) == "10"
      assert run(fn -> Examples.ControlFlow.main_min(50, 30) end) == "30"
      assert run(fn -> Examples.ControlFlow.main_min(7, 7) end) == "7"
    end
  end

  # Helper to capture IO output and trim it
  defp run(fun) do
    capture_io(fun) |> String.trim()
  end
end
