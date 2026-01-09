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
end
