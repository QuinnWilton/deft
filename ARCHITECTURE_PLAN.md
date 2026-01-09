# Architectural Review and Rearchitecture Plan for Deft

## Executive Summary

Deft is a promising implementation of "Type Systems as Macros" for Elixir, implementing bidirectional type checking with type erasure at compile time. The core ideas are sound, but the current architecture has several limitations that make it difficult to extend. This plan proposes a rearchitecture to support Turnstile-style declarative typing rules, per-module type system features, and better integration with the Elixir ecosystem.

---

## Current Architecture Analysis

### What Works Well

1. **Clean separation of AST and Type representations** - Custom struct-based nodes with protocol implementations (`Deft.AST`) provide a solid foundation.

2. **Bidirectional type checking pattern** - The `compute_and_erase_types/3` function correctly implements the synthesis/analysis duality.

3. **Subtyping lattice** (`lib/deft/subtyping.ex`) - Correctly handles covariance/contravariance for functions, union/intersection types, and the Top/Bottom bounds.

4. **ADT support with exhaustiveness checking** - `defdata` syntax and variant matching work correctly, though limited.

5. **Property-based testing** - The use of `stream_data` with custom generators for AST/type expressions is excellent for catching edge cases.

### Architectural Limitations

#### 1. Hardcoded Type Checking Logic

The current design has type checking rules spread across many files with no unifying abstraction:

```
lib/deft/type_checking/
├── annotation.ex    # 15 lines
├── block.ex         # 45 lines
├── case.ex          # 63 lines
├── cond.ex          # 28 lines
├── fn.ex            # 23 lines
├── fn_application.ex # 20 lines
├── guards.ex        # 255 lines (!)
├── if.ex            # 42 lines
├── match.ex         # 29 lines
└── tuple.ex         # 14 lines
```

Each module implements the same pattern manually: take AST node, compute types, erase annotations, return bindings. There's no declarative way to define new typing rules.

#### 2. Guards Handling is Brittle (`lib/deft/type_checking/guards.ex`)

The 255-line `guards.ex` hardcodes ~25 built-in functions with manual pattern matching:

```elixir
def handle_guard(name, [fst, snd], env, opts) when name in @comparisons do
  # ... manual type checking
end

def handle_guard(name, [term], env, opts) when name in @unary_math do
  # ... manual type checking
end
```

This approach doesn't scale. Adding new functions requires modifying this file directly.

#### 3. No Type Signature Registry

There's no centralized way to:
- Register type signatures for existing Elixir functions
- Import typed modules into `deft` blocks
- Define typed function specifications that can be validated

#### 4. Manual AST Walking (`lib/deft/macro.ex`)

The `postwalk/2` function has 222 lines of boilerplate:

```elixir
def postwalk(%AST.Annotation{} = node, f) do
  pattern = postwalk(node.pattern, f)
  type = postwalk(node.type, f)
  node = %{node | pattern: pattern, type: type}
  f.(node)
end

def postwalk(%AST.Pin{} = node, f) do
  # ... same pattern
end
# ... 20+ more clauses
```

This should be derivable from the AST struct definitions.

#### 5. Poor Error Messages

Error types in `lib/deft/error.ex` carry minimal context:

```elixir
defmodule Deft.TypecheckingError do
  defexception [:expected, :actual]

  def message(exception) do
    "Typechecking failed: expected #{inspect(exception.expected)}, got #{inspect(exception.actual)}"
  end
end
```

No source location, no expression context, no suggestion for fixes.

#### 6. No Module System

The `deft` macro only works on expression blocks. You cannot:
- Define typed modules
- Export typed functions
- Import typed functions from other modules
- Wrap untyped Elixir code with type annotations

#### 7. Binding Injection is Hacky (`lib/deft/helpers.ex:134-169`)

Variable bindings are tracked via metadata and injected using `postwalk`:

```elixir
def inject_bindings(ast, bindings, _env, _opts) do
  Enum.reduce(bindings, ast, fn
    {%AST.Local{name: name, context: context} = x, t}, acc ->
      Deft.Macro.postwalk(acc, fn
        %AST.Local{name: ^name, context: ^context} = local ->
          # HACK: Encapsulate this in Local
          if Keyword.get(local.meta, :counter) == Keyword.get(x.meta, :counter) do
            # ...
```

The comment "HACK" is revealing. This approach doesn't scale to more complex scoping.

#### 8. Type Alias Resolution is Incomplete

`Type.Alias` exists but is only "marked", never fully resolved:

```elixir
def parse({name, _, ctx}) when is_atom(name) and is_atom(ctx) do
  Type.alias(name, ctx)
end
```

There's no mechanism to resolve aliases to their underlying types during checking.

---

## Proposed Architecture

### Core Principle: Declarative Typing Rules

Following Turnstile, typing rules should be *declarative specifications* that the framework interprets, not imperative code. This enables:
- Composable type systems
- Per-module feature selection
- Easier verification
- Better error messages

### Phase 1: Foundation Refactoring

#### 1.1 Introduce Typing Rule Protocol

Create a protocol-based system for defining typing rules:

```elixir
defmodule Deft.Rule do
  @type judgment :: :synth | :check | :both
  @type result :: {:ok, erased_ast, type, bindings} | {:error, Error.t()}

  @callback judgment() :: judgment()
  @callback matches?(ast :: term()) :: boolean()
  @callback apply(ast, expected_type :: type() | nil, ctx :: Context.t()) :: result()
end
```

Each rule becomes a module implementing this protocol. Rules can be composed and selected per-module.

#### 1.2 Unified Context Module

Replace the scattered `env, opts` threading with a proper context:

```elixir
defmodule Deft.Context do
  defstruct [
    :env,                    # Elixir macro env
    :type_env,               # variable -> type mappings
    :adt_env,                # ADT name -> definition
    :signature_env,          # function name -> type signature
    :enabled_features,       # [:subtyping, :polymorphism, etc.]
    :error_handler,          # callback for error accumulation
    :source_map              # AST node -> source location
  ]
end
```

#### 1.3 Derivable AST Walking

Use a macro to generate `postwalk` from struct definitions:

```elixir
defmodule Deft.AST.Node do
  defmacro defnode(name, fields) do
    quote do
      defmodule unquote(name) do
        defstruct unquote(fields) ++ [:meta]

        # Auto-generate postwalk clause
        defimpl Deft.Walkable do
          def children(node) do
            # Extract child fields from struct
          end

          def rebuild(node, new_children) do
            # Rebuild with new children
          end
        end
      end
    end
  end
end
```

### Phase 2: Type Signature System

#### 2.1 Type Signature Registry

Create a registry for function type signatures:

```elixir
defmodule Deft.Signatures do
  use GenServer  # Or ETS-backed compile-time storage

  @spec register(mfa(), Type.t()) :: :ok
  @spec lookup(mfa()) :: {:ok, Type.t()} | :error

  # Built-in signatures
  @builtins %{
    {:Kernel, :+, 2} => Type.fun([Type.number(), Type.number()], Type.number()),
    {:Kernel, :length, 1} => Type.fun([Type.list()], Type.integer()),
    # ...
  }
end
```

#### 2.2 Typed Module Definitions

Introduce `deft` as a module-level construct:

```elixir
defmodule MyApp.Math do
  use Deft

  # Declare type signature (checked against implementation)
  @deft add(integer, integer) :: integer
  def add(a, b), do: a + b

  # Inline typed function
  deft safe_div(a :: number, b :: number) :: number | :error do
    if b == 0, do: :error, else: a / b
  end
end
```

#### 2.3 External Type Declarations

Allow typing existing Elixir code without modifying it:

```elixir
defmodule MyApp.TypeDeclarations do
  use Deft.Declare

  # Declare types for external functions
  declare Enum.map([a], (a -> b)) :: [b]
  declare Enum.reduce([a], b, (a, b -> b)) :: b
  declare String.length(string) :: integer
end
```

### Phase 3: Turnstile-Style DSL

#### 3.1 Typing Rule DSL

Create a macro DSL for defining typing rules similar to Turnstile:

```elixir
defmodule Deft.Rules.Fn do
  use Deft.Rule.DSL

  # Synthesis rule for anonymous functions
  defrule :fn_synth,
    matches: %AST.Fn{args: args, body: body},
    premises: [
      # Check each argument annotation
      {:each, args, fn %AST.Annotation{pattern: p, type: t} ->
        bind(p, t)
      end},
      # Synthesize body type with extended context
      synth(body) ~> body_type
    ],
    conclusion: Type.fun(arg_types(args), body_type),
    elaboration: fn ast, _ctx ->
      # Return erased AST
      {:fn, ast.fn_meta, [{:->, ast.arrow_meta, [erase(args), erase(body)]}]}
    end
end
```

#### 3.2 Rule Composition

Allow combining rules from multiple sources:

```elixir
defmodule MyApp.TypeSystem do
  use Deft.TypeSystem

  # Base rules
  include Deft.Rules.Core
  include Deft.Rules.ADT

  # Extensions
  include Deft.Rules.Polymorphism
  include MyApp.CustomRules

  # Per-module feature flags
  features [:exhaustiveness_checking, :strict_subtyping]
end
```

### Phase 4: Rich Error Reporting

#### 4.1 Structured Error Type

Replace simple exceptions with rich error objects:

```elixir
defmodule Deft.Error do
  defstruct [
    :code,           # :type_mismatch, :missing_annotation, etc.
    :message,        # Human-readable message
    :expected,       # Expected type
    :actual,         # Actual type
    :location,       # {file, line, column}
    :expression,     # Source expression
    :context,        # Surrounding code
    :suggestions,    # Possible fixes
    :notes           # Additional context
  ]
end
```

#### 4.2 Error Accumulation

Support multiple errors per compilation (don't fail on first):

```elixir
defmodule Deft.ErrorCollector do
  def collect(context, fun) do
    # Run type checking, collecting errors
    case fun.() do
      {:ok, result} -> {:ok, result}
      {:error, e} ->
        add_error(context, e)
        {:error, :recovered}  # Continue checking
    end
  end
end
```

#### 4.3 Pretty Error Formatting

Generate compiler-quality error messages:

```
error[E0308]: mismatched types
  --> lib/my_app.ex:15:10
   |
14 |   add = fn x :: integer, y :: integer ->
15 |     x + y + 1.5
   |           ^^^^^ expected `integer`, found `float`
   |
   = note: in expression `x + y + 1.5`
   = note: `+` with integer arguments returns integer
   = help: consider using `trunc(1.5)` or change return type to `number`
```

### Phase 5: Advanced Type System Features

#### 5.1 Polymorphism (Per-Module Opt-In)

```elixir
defmodule MyApp.Lists do
  use Deft, features: [:polymorphism]

  # Type variables with explicit quantification
  deft map(list :: [a], f :: (a -> b)) :: [b] do
    case list do
      [] -> []
      [h | t] -> [f.(h) | map(t, f)]
    end
  end
end
```

#### 5.2 Row Polymorphism for Records/Maps

```elixir
deft get_name(person :: {name: string | _}) :: string do
  person.name
end
```

#### 5.3 Effect Tracking (Optional)

```elixir
defmodule MyApp.IO do
  use Deft, features: [:effects]

  deft read_file(path :: string) :: string ! :io do
    File.read!(path)
  end
end
```

---

## Current Limitations Inventory

| Limitation | Impact | Proposed Solution |
|------------|--------|-------------------|
| No named functions | Severe | Phase 2.2 typed module definitions |
| No module imports | Severe | Phase 2.3 external declarations |
| No recursive types | Moderate | Phase 5 with proper type unification |
| No type parameters | Moderate | Phase 5.1 polymorphism |
| ~25 hardcoded guards | Moderate | Phase 2.1 signature registry |
| No process types | Moderate | Future: session types or behavioral types |
| No maps/structs | Moderate | Phase 5.2 row polymorphism |
| Poor error messages | Moderate | Phase 4 error reporting |
| No type inference | Minor | Phase 5 with constraint solving |
| No binary/bitstring | Minor | Extend signature registry |
| No guards in patterns | Minor | Extend pattern matching rules |
| Single expression blocks | Minor | Phase 2.2 module support |

---

## Implementation Roadmap

### Phase 1: Foundation (Groundwork)
1. Introduce `Deft.Context` to replace `env, opts`
2. Create `Deft.Rule` protocol
3. Derive AST walking from struct definitions
4. Migrate existing type checkers to rule-based system

### Phase 2: Signatures & Modules
1. Implement signature registry with built-in Elixir functions
2. Add `use Deft` module integration
3. Support `@deft` function annotations
4. Add external type declaration syntax

### Phase 3: Declarative Rules
1. Design and implement `defrule` macro
2. Port all existing rules to declarative syntax
3. Implement rule composition/selection
4. Add per-module feature flags

### Phase 4: Error Quality
1. Design structured error type
2. Add source location tracking
3. Implement error accumulation
4. Create pretty printer with suggestions

### Phase 5: Advanced Features
1. Implement type unification for polymorphism
2. Add constraint solving
3. Row polymorphism for maps
4. Effect tracking (optional)

---

## Files Requiring Major Changes

| File | Change Type | Notes |
|------|-------------|-------|
| `lib/deft.ex` | Rewrite | Becomes `use Deft` module |
| `lib/deft/compiler.ex` | Extend | Add module-level compilation |
| `lib/deft/type_checking.ex` | Rewrite | Becomes rule dispatcher |
| `lib/deft/type_checking/*.ex` | Rewrite | Convert to declarative rules |
| `lib/deft/helpers.ex` | Split | Context module + utilities |
| `lib/deft/macro.ex` | Auto-generate | Derive from AST definitions |
| `lib/deft/error.ex` | Rewrite | Rich error types |
| (new) `lib/deft/context.ex` | Create | Unified typing context |
| (new) `lib/deft/rule.ex` | Create | Rule protocol and DSL |
| (new) `lib/deft/signatures.ex` | Create | Function type registry |

---

## References

- [Type Systems as Macros (POPL 2017)](https://www.khoury.northeastern.edu/home/stchang/popl2017/) - Original paper
- [Implementing Type Systems as Macros](https://lambdaland.org/posts/2023-08-14_types_with_macros/) - Practical tutorial
- [Turnstile+ for Dependent Types](https://arxiv.org/abs/2107.01295) - Extended approach
- [ACM Paper](https://dl.acm.org/doi/10.1145/3093333.3009886) - Formal publication

---

This plan prioritizes extensibility and declarativeness while maintaining the existing strengths of the codebase. The phased approach allows incremental progress with working software at each stage.
