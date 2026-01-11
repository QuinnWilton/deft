defmodule Deft.ContextTest do
  use ExUnit.Case

  alias Deft.AST
  alias Deft.Context
  alias Deft.Type

  describe "new/1" do
    test "creates context from macro env" do
      env = __ENV__
      ctx = Context.new(env)

      assert %Context{} = ctx
      assert ctx.env == env
      assert ctx.type_env == []
      assert ctx.adt_env == []
      assert ctx.features == []
    end
  end

  describe "new/2" do
    test "creates context with options" do
      callback = fn _ast, _type -> :ok end
      ctx = Context.new(__ENV__, on_compute: callback, features: [:polymorphism])

      assert ctx.on_compute == callback
      assert ctx.features == [:polymorphism]
    end
  end

  describe "bind/3" do
    test "adds variable binding to context" do
      ctx = Context.new(__ENV__)
      local = AST.Local.new(:x, nil, [])

      ctx = Context.bind(ctx, local, Type.integer())

      assert [{^local, %Type.Integer{}}] = ctx.type_env
    end

    test "bindings are prepended (most recent first)" do
      ctx = Context.new(__ENV__)
      x = AST.Local.new(:x, nil, [])
      y = AST.Local.new(:y, nil, [])

      ctx =
        ctx
        |> Context.bind(x, Type.integer())
        |> Context.bind(y, Type.boolean())

      assert [{^y, %Type.Boolean{}}, {^x, %Type.Integer{}}] = ctx.type_env
    end
  end

  describe "bind_all/2" do
    test "adds multiple bindings" do
      ctx = Context.new(__ENV__)
      x = AST.Local.new(:x, nil, [])
      y = AST.Local.new(:y, nil, [])

      ctx = Context.bind_all(ctx, [{x, Type.integer()}, {y, Type.boolean()}])

      assert length(ctx.type_env) == 2
    end

    test "handles ADT bindings" do
      ctx = Context.new(__ENV__)
      name = AST.Local.new(:shape, nil, [])
      adt = Type.adt(name, [])

      ctx = Context.bind_all(ctx, [{:adt, name, adt}])

      assert [{:adt, ^name, ^adt}] = ctx.adt_env
    end

    test "handles ADT variant bindings" do
      ctx = Context.new(__ENV__)
      name = AST.Local.new(:shape, nil, [])
      adt = Type.adt(name, [])
      variant = Type.variant(:circle, name, [Type.float()])

      ctx = Context.bind_all(ctx, [{:adt_variant, :circle, adt, variant}])

      assert [{:adt_variant, :circle, ^adt, ^variant}] = ctx.adt_env
    end
  end

  describe "bind_adt/3" do
    test "adds ADT definition to context" do
      ctx = Context.new(__ENV__)
      name = AST.Local.new(:shape, nil, [])
      adt = Type.adt(name, [])

      ctx = Context.bind_adt(ctx, name, adt)

      assert [{:adt, ^name, ^adt}] = ctx.adt_env
    end
  end

  describe "lookup/2" do
    test "finds bound variable" do
      ctx = Context.new(__ENV__)
      local = AST.Local.new(:x, nil, [])

      ctx = Context.bind(ctx, local, Type.integer())

      assert {:ok, %Type.Integer{}} = Context.lookup(ctx, local)
    end

    test "returns error for unbound variable" do
      ctx = Context.new(__ENV__)
      local = AST.Local.new(:x, nil, [])

      assert :error = Context.lookup(ctx, local)
    end
  end

  describe "feature_enabled?/2" do
    test "returns true for enabled feature" do
      ctx = Context.new(__ENV__, features: [:polymorphism])

      assert Context.feature_enabled?(ctx, :polymorphism)
    end

    test "returns false for disabled feature" do
      ctx = Context.new(__ENV__, features: [])

      refute Context.feature_enabled?(ctx, :polymorphism)
    end
  end

  describe "notify_compute/3" do
    test "calls callback when present" do
      test_pid = self()
      callback = fn ast, type -> send(test_pid, {:computed, ast, type}) end
      ctx = Context.new(__ENV__, on_compute: callback)

      Context.notify_compute(ctx, :test_ast, Type.integer())

      assert_receive {:computed, :test_ast, %Type.Integer{}}
    end

    test "does nothing when callback is nil" do
      ctx = Context.new(__ENV__)

      assert :ok = Context.notify_compute(ctx, :test_ast, Type.integer())
    end
  end

  describe "all_bindings/1" do
    test "returns combined bindings" do
      ctx = Context.new(__ENV__)
      x = AST.Local.new(:x, nil, [])
      name = AST.Local.new(:shape, nil, [])
      adt = Type.adt(name, [])

      ctx =
        ctx
        |> Context.bind(x, Type.integer())
        |> Context.bind_adt(name, adt)

      bindings = Context.all_bindings(ctx)

      assert length(bindings) == 2
    end
  end

  describe "to_opts/1" do
    test "converts context to legacy opts format" do
      callback = fn _ast, _type -> :ok end
      ctx = Context.new(__ENV__, on_compute: callback)

      opts = Context.to_opts(ctx)

      assert opts[:on_compute] == callback
    end

    test "returns empty opts when no options set" do
      ctx = Context.new(__ENV__)

      opts = Context.to_opts(ctx)

      assert opts == []
    end
  end

  # ============================================================================
  # Error Accumulation Tests
  # ============================================================================

  alias Deft.Error

  describe "error_mode option" do
    test "defaults to :fail_fast" do
      ctx = Context.new(__ENV__)

      assert ctx.error_mode == :fail_fast
    end

    test "can be set to :accumulate" do
      ctx = Context.new(__ENV__, error_mode: :accumulate)

      assert ctx.error_mode == :accumulate
    end
  end

  describe "add_error/2 in fail_fast mode" do
    test "raises exception" do
      ctx = Context.new(__ENV__, error_mode: :fail_fast)

      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float()
        )

      assert_raise Deft.Error.Exception, fn ->
        Context.add_error(ctx, error)
      end
    end
  end

  describe "add_error/2 in accumulate mode" do
    test "stores error in context" do
      ctx = Context.new(__ENV__, error_mode: :accumulate)

      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float()
        )

      ctx = Context.add_error(ctx, error)

      assert length(ctx.errors) == 1
      assert hd(ctx.errors).code == :type_mismatch
    end

    test "accumulates multiple errors" do
      ctx = Context.new(__ENV__, error_mode: :accumulate)

      error1 =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float()
        )

      error2 =
        Error.unsupported_call(
          name: :foo,
          arity: 2
        )

      ctx =
        ctx
        |> Context.add_error(error1)
        |> Context.add_error(error2)

      assert length(ctx.errors) == 2
    end
  end

  describe "get_errors/1" do
    test "returns empty list when no errors" do
      ctx = Context.new(__ENV__)

      assert Context.get_errors(ctx) == []
    end

    test "returns accumulated errors" do
      ctx = Context.new(__ENV__, error_mode: :accumulate)

      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float()
        )

      ctx = Context.add_error(ctx, error)

      errors = Context.get_errors(ctx)
      assert length(errors) == 1
    end
  end

  describe "has_errors?/1" do
    test "returns false when no errors" do
      ctx = Context.new(__ENV__)

      refute Context.has_errors?(ctx)
    end

    test "returns true when errors exist" do
      ctx = Context.new(__ENV__, error_mode: :accumulate)

      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float()
        )

      ctx = Context.add_error(ctx, error)

      assert Context.has_errors?(ctx)
    end
  end

  describe "clear_errors/1" do
    test "removes all errors" do
      ctx = Context.new(__ENV__, error_mode: :accumulate)

      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float()
        )

      ctx =
        ctx
        |> Context.add_error(error)
        |> Context.clear_errors()

      refute Context.has_errors?(ctx)
    end
  end

  describe "record_location/3 and get_location/2" do
    test "records and retrieves source location" do
      ctx = Context.new(__ENV__)
      ast_node = {:foo, [], nil}
      location = {"test.ex", 10, 5}

      ctx = Context.record_location(ctx, ast_node, location)

      assert Context.get_location(ctx, ast_node) == location
    end

    test "returns nil for unrecorded node" do
      ctx = Context.new(__ENV__)
      ast_node = {:foo, [], nil}

      assert Context.get_location(ctx, ast_node) == nil
    end
  end

  describe "get_source_context/3" do
    test "returns nil tuple when no source lines" do
      ctx = Context.new(__ENV__)

      assert Context.get_source_context(ctx, 5, 2) == {nil, nil, nil}
    end

    test "extracts context lines around target" do
      source_lines = [
        "line 1",
        "line 2",
        "line 3",
        "line 4",
        "line 5"
      ]

      ctx = Context.new(__ENV__, source_lines: source_lines)

      {before, current, after_ctx} = Context.get_source_context(ctx, 3, 1)

      assert current == "line 3"
      assert before == "line 2"
      assert after_ctx == "line 4"
    end

    test "handles edge cases at start of file" do
      source_lines = [
        "line 1",
        "line 2",
        "line 3"
      ]

      ctx = Context.new(__ENV__, source_lines: source_lines)

      {before, current, _after} = Context.get_source_context(ctx, 1, 2)

      assert current == "line 1"
      assert before == nil
    end

    test "handles edge cases at end of file" do
      source_lines = [
        "line 1",
        "line 2",
        "line 3"
      ]

      ctx = Context.new(__ENV__, source_lines: source_lines)

      {_before, current, after_ctx} = Context.get_source_context(ctx, 3, 2)

      assert current == "line 3"
      assert after_ctx == nil
    end
  end

  describe "with_error_handling/3" do
    test "returns ok result in fail_fast mode" do
      ctx = Context.new(__ENV__, error_mode: :fail_fast)

      {:ok, result, _ctx} = Context.with_error_handling(ctx, fn -> {:ok, 42} end, nil)

      assert result == 42
    end

    test "raises on error in fail_fast mode" do
      ctx = Context.new(__ENV__, error_mode: :fail_fast)

      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float()
        )

      assert_raise Deft.Error.Exception, fn ->
        Context.with_error_handling(ctx, fn -> {:error, error} end, nil)
      end
    end

    test "accumulates error and returns recovery in accumulate mode" do
      ctx = Context.new(__ENV__, error_mode: :accumulate)

      error =
        Error.type_mismatch(
          expected: Type.integer(),
          actual: Type.float()
        )

      {:ok, result, ctx} =
        Context.with_error_handling(ctx, fn -> {:error, error} end, :recovered)

      assert result == :recovered
      assert Context.has_errors?(ctx)
    end
  end
end
