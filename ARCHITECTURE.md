# Architecture

This document describes the architecture of Deft, a "Type Systems as Macros" implementation for Elixir. It provides bidirectional type checking at compile time with complete type erasure at runtime.

## Core Concepts

### Bidirectional Type Checking

Deft implements bidirectional type checking with two modes:

- **Synthesis (inference)**: Infer the type of an expression from its structure
- **Checking (analysis)**: Verify an expression matches an expected type

This duality allows precise type inference while supporting explicit type annotations where needed.

### Type Erasure

All type information is removed during compilation. The output is standard Elixir code with no runtime type overhead. Types exist only at compile time for validation.

### Rule-Based Extensibility

Type checking rules are declarative modules implementing the `Deft.Rules` behaviour. This enables:

- Composable type systems
- Per-module feature selection
- Easy addition of new language constructs
- Clear separation of typing logic

## Module Organization

```
lib/deft/
├── deft.ex                    # Main entry point (compile/1, use Deft)
├── compiler.ex                # Macro-to-AST compilation
├── type_checker.ex            # Rule dispatcher and entry point
├── context.ex                 # Unified typing context
├── subtyping.ex               # Subtyping relation
├── annotations.ex             # Type annotation parsing
├── pattern_matching.ex        # Pattern matching type checking
├── signatures.ex              # Function type signature registry
├── helpers.ex                 # Utility functions
├── guards.ex                  # Guard function type checking
├── walkable.ex                # AST/Type traversal protocol
├── walkable_impl.ex           # Protocol implementations
├── error.ex                   # Rich error types
│
├── ast/                       # AST node definitions
│   ├── literal.ex             # Literal values (atoms, numbers, etc.)
│   ├── local.ex               # Variables
│   ├── local_call.ex          # Function calls
│   ├── annotation.ex          # Type annotations (x :: type)
│   ├── fn.ex                  # Anonymous functions
│   ├── fn_application.ex      # Function application (f.(args))
│   ├── tuple.ex, list.ex      # Data structures
│   ├── if.ex, cond.ex, case.ex # Control flow
│   ├── match.ex               # Pattern matching (=)
│   ├── def_data.ex, variant.ex # Algebraic data types
│   ├── erased.ex              # Type-erased AST generation
│   └── ...
│
├── type/                      # Type definitions
│   ├── top.ex, bottom.ex      # Lattice bounds
│   ├── integer.ex, float.ex   # Primitive types
│   ├── number.ex, boolean.ex  # Additional primitives
│   ├── fn.ex                  # Function types
│   ├── tuple.ex, fixed_tuple.ex # Tuple types
│   ├── list.ex, fixed_list.ex # List types
│   ├── union.ex, intersection.ex # Composite types
│   ├── adt.ex, variant.ex     # Algebraic data types
│   └── alias.ex               # Type aliases
│
├── rules/                     # Type checking rules
│   ├── dsl.ex                 # Declarative rule DSL
│   ├── core.ex                # Literals, variables, tuples, lists
│   ├── functions.ex           # Anonymous functions
│   ├── control_flow.ex        # If, cond, case expressions
│   └── builtins.ex            # Built-in function signatures
│
├── error/                     # Error handling
│   ├── exception.ex           # Exception definitions
│   └── formatter.ex           # Error formatting
│
├── declare.ex                 # External type declarations
├── type_system.ex             # Custom type system composition
└── generators/                # Property test generators
```

## Key Architectural Patterns

### 1. Compilation Pipeline

```
Elixir Source Code
       │
       ▼
┌──────────────────┐
│  Deft.Compiler   │  Macro expansion: Elixir AST → Deft AST
└──────────────────┘
       │
       ▼
┌──────────────────┐
│  Deft.TypeChecker│  Rule-based type checking
└──────────────────┘
       │
       ▼
┌──────────────────┐
│  Type Erasure    │  Deft AST → Standard Elixir AST
└──────────────────┘
       │
       ▼
   Runtime Code
```

### 2. Context Threading

The `Deft.Context` struct carries all state through type checking:

```elixir
%Context{
  env: __CALLER__,           # Elixir macro environment
  type_env: [],              # Variable → type bindings
  adt_env: [],               # ADT definitions
  features: [],              # Enabled features
  error_mode: :fail_fast,    # Error handling mode
  errors: [],                # Accumulated errors
  source_map: %{},           # AST → source location
  on_compute: nil            # Debug callback
}
```

This replaces scattered `env, opts` parameters with a single, immutable context that flows through all operations.

### 3. Rule Protocol and DSL

Rules implement the `Deft.Rules` behaviour:

```elixir
@callback name() :: atom()
@callback judgment() :: :synth | :check | :both
@callback matches?(ast :: term()) :: boolean()
@callback apply(ast, expected_type, ctx) :: result()
```

The DSL (`Deft.Rules.DSL`) provides declarative syntax:

```elixir
defrule :literal, %AST.Literal{value: value} do
  conclude value ~> type_of_literal(value)
end

defrule :fn, %AST.Fn{args: args, body: body, fn_meta: fn_meta, arrow_meta: arrow_meta} do
  args ~>> {args_e, input_ts, arg_bs}
  (arg_bs +++ body) ~> {body_e, output_t}

  conclude Erased.fn_expr(fn_meta, arrow_meta, args_e, body_e)
        ~> Type.fun(input_ts, output_t)
end
```

DSL operators:
- `~>` : Synthesis judgment (infer type)
- `~>>` : Synthesize all expressions in a list
- `<~` : Checking judgment (check against expected type)
- `<<~` : Check all expressions (homogeneous if single type, heterogeneous if list)
- `+++` : Context extension (add bindings for premise)
- `&&&` : Scoped context (pass context to child rules)
- `<~>` : Pattern judgment (check pattern against type)
- `>>>` : Elaborates to (binds result of `<~` or `<~>`)

### 4. Walkable Protocol

The `Deft.Walkable` protocol enables generic tree traversal:

```elixir
defprotocol Deft.Walkable do
  def children(node)
  def rebuild(node, new_children)
end
```

Implementations are in `walkable_impl.ex`. The `Deft.Walker` module provides `postwalk/2` and `prewalk/2` for transformation.

### 5. Type Lattice

```
              Top (any type)
           /   |   \   \   \
         Fn  Tuple List Union ...
         /\   /|\    |   /\
     Int Float Number ...
          \  |  /
          Bottom (no value)
```

Key relations:
- `Top` is supertype of all types
- `Bottom` is subtype of all types
- `Integer <: Number`, `Float <: Number`
- Functions are contravariant in inputs, covariant in output
- `FixedTuple <: Tuple`, `FixedList <: List`

### 6. Error Handling

The `Deft.Error` module provides structured errors:

```elixir
%Error{
  code: :type_mismatch,
  message: "...",
  expected: Type.integer(),
  actual: Type.float(),
  location: {"file.ex", 10, 5},
  expression: ast,
  suggestions: ["Try using trunc/1"],
  notes: ["..."]
}
```

Error modes:
- `:fail_fast` - Raise on first error (default)
- `:accumulate` - Collect all errors for batch reporting

## Design Decisions

### AST Representation

Custom struct-based AST nodes rather than raw Elixir AST because:
- Type-safe field access
- Protocol dispatch for operations
- Clear documentation of node structure
- Metadata preserved in `:meta` field

### Type Erasure Approach

`Deft.AST.Erased` provides builder functions that construct standard Elixir AST:

```elixir
Erased.fn_expr(fn_meta, arrow_meta, args, body)
Erased.local_call(meta, name, args)
Erased.tuple(meta, elements)
```

This maintains source locations while producing clean output.

### Signature Registry

`Deft.Signatures` uses ETS for efficient lookup of function signatures:

```elixir
Signatures.register({Kernel, :+, 2}, Type.fun([Type.number(), Type.number()], Type.number()))
Signatures.lookup({Kernel, :+, 2})  # => {:ok, %Type.Fn{...}}
```

Built-in signatures are loaded at startup.

### Binding Injection

Variable bindings are tracked via metadata annotations:

```elixir
%AST.Local{name: :x, meta: [__deft_type__: %Type.Integer{}]}
```

`Helpers.inject_bindings/2` walks the AST and annotates matching locals.

## Extension Points

### Custom Rules

Create a module using the DSL:

```elixir
defmodule MyRules do
  use Deft.Rules.DSL

  defrule :my_construct, %MyAST{...} do
    # premises
    conclude erased ~> type
  end
end
```

### Custom Type Systems

Compose rules with `Deft.TypeSystem`:

```elixir
defmodule MyTypeSystem do
  use Deft.TypeSystem

  include Deft.Rules.Core
  include Deft.Rules.Functions
  include MyRules

  features [:exhaustiveness_checking]
end
```

### External Type Declarations

Declare types for existing modules with `Deft.Declare`:

```elixir
defmodule MyDeclarations do
  use Deft.Declare

  declare Enum.map([a], (a -> b)) :: [b]
end
```

## Testing Strategy

- **Unit tests**: Per-module tests in `test/deft/`
- **Integration tests**: End-to-end type checking in `test/deft/integration/`
- **Property tests**: Type algebra laws in `test/property/`
- **Test support**: Builders and assertions in `test/support/`

Property tests verify:
- Subtyping transitivity and reflexivity
- Type algebra properties (union, intersection)
- Type checker invariants

## File Naming Conventions

- `lib/deft/<module>.ex` - Single module per file
- `lib/deft/ast/<node>.ex` - AST node definitions
- `lib/deft/type/<type>.ex` - Type definitions
- `lib/deft/rules/<category>.ex` - Rule modules
- `test/deft/<module>_test.exs` - Module tests
- `test/property/<property>_test.exs` - Property tests
