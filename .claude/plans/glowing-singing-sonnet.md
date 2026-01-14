# Deft Codebase Audit and Improvement Plan

## Executive Summary

Comprehensive audit of the Deft codebase revealed a well-structured project with strong type safety practices. The highest-impact improvements fall into three categories: documentation accuracy, removing unnecessary indirection, and completing generator functionality.

---

## Priority 1: Documentation Fixes

### 1.1 Update ARCHITECTURE.md to Match Reality
**Impact: High** | **Effort: Low**

ARCHITECTURE.md references files that don't exist:
- `guards.ex` - Guard handling is actually in `rules/builtins.ex`
- `walkable_impl.ex` - Protocol implementations live in AST modules via `use Deft.AST.Node`

**Files to modify:**
- `/Users/quinn/dev/deft/ARCHITECTURE.md` (lines 42-43)

### 1.2 Complete Error Code Documentation
**Impact: Medium** | **Effort: Low**

`lib/deft/error.ex` documents E0001-E0009 (lines 14-25) but `@error_codes` (lines 105-119) defines:
- E0010: `unsupported_syntax`
- E0011: `unsupported_pattern`
- E0012: `unsupported_function`
- E0013: `conflicting_definition`
- E0014: `unknown_type_alias`

**Files to modify:**
- `/Users/quinn/dev/deft/lib/deft/error.ex` (lines 14-25)

---

## Priority 2: Remove @types Hardcoded List

### 2.1 Eliminate `@types` Module Attribute
**Impact: Medium** | **Effort: Low**

`lib/deft/type.ex` has a hardcoded `@types` list (lines 28-46) used only by `well_formed?/1` (line 154). This conflicts with the goal of customizable types.

**Current usage:** Only in property tests to verify generated types are valid.

**Options:**
1. **Remove `well_formed?/1` entirely** - Property tests could use different assertions
2. **Replace with protocol** - Define `Deft.Type` protocol that all types implement
3. **Use struct check** - `well_formed?/1` could check `is_struct(type)` plus required fields

**Recommendation:** Option 3 is simplest - check for required struct behavior rather than maintaining a list.

**Files to modify:**
- `/Users/quinn/dev/deft/lib/deft/type.ex` (lines 28-46, 154-160)

---

## Priority 3: Address LocalCall Pattern TODO

### 3.1 LocalCall Pattern Validation
**Impact: Medium** | **Effort: Medium**

**File:** `lib/deft/compiler.ex` (lines 340-346)

```elixir
def compile_pattern({name, meta, args}) when is_list(args) do
  # TODO: Risky to allow LocalCall nodes in patterns, even though
  #       they should get rewritten in the case of type constructors.
  args = Enum.map(args, &compile_pattern/1)
  AST.LocalCall.new(name, args, meta)
end
```

**Deep Analysis:**

The issue: When compiling patterns like `some(x)`, the compiler creates `AST.LocalCall`. This is "risky" because LocalCall is normally for function calls, not pattern matching.

**Current safety mechanisms:**
1. `helpers.ex:96-102` - `inject_bindings/2` rewrites LocalCall → TypeConstructorCall when ADT variant bindings exist
2. `pattern_matching.ex:390-406` - `transform_adt_patterns/2` rewrites for registry types (FFI)

**The gap:** If a LocalCall pattern reaches `do_handle_pattern/3` without being rewritten (e.g., pattern matching against non-ADT type), there's no explicit handler for it. It would fall through to the catch-all error case.

**Validity:** Still valid - relying on implicit fall-through for error handling is fragile.

**Fix:** Add explicit LocalCall handler in `pattern_matching.ex` that produces a clear error:
```elixir
defp do_handle_pattern(%AST.LocalCall{name: name}, type, _ctx) do
  {:error, "Unknown pattern `#{name}(...)` - not a type constructor for #{inspect(type)}"}
end
```

**Files to modify:**
- `/Users/quinn/dev/deft/lib/deft/pattern_matching.ex`

---

## Priority 4: Generator TODOs (Deep Analysis)

### 4.1 TODO: propagate bindings (line 10)
**File:** `lib/deft/generators/code.ex`

```elixir
def code() do
  # TODO: propagate bindings
  map(nonempty(list_of(expression())), fn children ->
    ...
  end)
end
```

**Intent:** When generating a block of expressions, variables bound in earlier expressions should be available to later expressions. Currently, each expression is generated independently.

**Example of limitation:**
```elixir
# Current generator might produce:
x = 5
y + 1  # y is unbound - invalid!

# With binding propagation:
x = 5
x + 1  # uses bound x - valid!
```

**Validity:** Still valid but low priority - property tests work around this by using independent expressions.

**Fix:** Thread a binding environment through generators:
```elixir
def code() do
  bind(list_of(expression_with_bindings(%{})), fn children ->
    ...
  end)
end

defp expression_with_bindings(bindings) do
  # Include bound variables as possible sub-expressions
end
```

**Effort:** Medium-High - requires significant refactor of generator structure.

---

### 4.2 TODO: Generate non-literal arguments (line 246)
**File:** `lib/deft/generators/code.ex`

```elixir
def fn_application_node(child_data \\ literal_node()) do
  bind(fn_node(child_data), fn {fn_node, fn_type} ->
    # TODO: Generate non-literal arguments
    arg_nodes = Enum.map(fn_type.inputs, &argument_node/1)
    ...
  end)
end
```

**Intent:** Generate more complex expressions as function arguments, not just literals.

**Analysis:** Looking at `argument_node/1` (lines 316-388), it actually DOES handle non-literal types:
- `Type.Union` - picks a branch
- `Type.Fn` - generates lambda
- `Type.FixedList` - generates list
- `Type.FixedTuple` - generates tuple
- `Type.Intersection`, `Type.Top`, `Type.Bottom`

**Validity:** Likely OUTDATED - the `argument_node/1` function has been enhanced since this TODO was written.

**Fix:** Remove or update the TODO to specify what's actually missing (perhaps nested expressions like `f.(g.(x))`).

---

### 4.3 TODO: Limited predicate generation (line 270)
**File:** `lib/deft/generators/code.ex`

```elixir
def if_node(child_data \\ literal_node()) do
  # TODO: Limited predicate generation
  map(tuple({predicate(child_data), child_data, child_data}), ...
```

**Intent:** `predicate/1` only generates guard calls (`is_atom`, `is_integer`, etc.) and comparisons.

**Missing predicate forms:**
- Boolean literals: `true`, `false`
- Boolean operators: `and`, `or`, `not`
- Compound predicates: `is_integer(x) and x > 0`

**Validity:** Still valid - predicates are limited.

**Fix:** Extend `predicate/1`:
```elixir
def predicate(child_data \\ literal_node()) do
  one_of([
    literal_node(%Type.Boolean{}),  # true/false literals
    local_call_node(child_data),    # existing guards/comparisons
    boolean_operator(child_data)    # and/or combinations
  ])
end
```

**Effort:** Low-Medium

---

### 4.4 TODO: Generate more types of local calls (line 474)
**File:** `lib/deft/generators/code.ex`

```elixir
bind(arguments, fn
  # TODO: Generate more types of local calls
  {{fst, _}, {snd, _}} ->
    map(comparisons, fn comparison -> ...
```

**Intent:** Currently generates comparisons (2 args) or guards (1 arg). Could generate arithmetic, string ops, list ops.

**Missing call types:**
- Arithmetic: `+`, `-`, `*`, `/` for numbers
- List operations: `hd`, `tl`, `length`
- Tuple operations: `elem`, `tuple_size`

**Validity:** Still valid but lower priority - these are partially covered by `choose_fn/1-2`.

**Fix:** Add type-directed call generation based on argument types.

**Effort:** Medium

---

### 4.5 TODO: Slow. (line 534)
**File:** `lib/deft/generators/code.ex`

```elixir
def consume_exprs(exprs) do
  # TODO: Slow.
  exprs = Enum.shuffle(exprs)
  if length(exprs) > 2 do
    {[fst, snd], exprs} = Enum.split(exprs, 2)
    ...
    consume_exprs([expr | exprs])
  else
    ...
  end
end
```

**Intent:** Performance issue - this is O(n²) due to repeated `length/1` and `Enum.shuffle/1` calls.

**Validity:** Still valid - inefficient algorithm.

**Fix:** Use accumulator pattern with pre-computed length:
```elixir
def consume_exprs(exprs) do
  consume_exprs(Enum.shuffle(exprs), length(exprs))
end

defp consume_exprs(exprs, count) when count > 2 do
  [fst, snd | rest] = exprs
  expr = choose_fn(fst, snd)
  consume_exprs([expr | rest], count - 1)
end

defp consume_exprs(exprs, _count) do
  Enum.random(exprs) |> choose_fn()
end
```

**Effort:** Low

---

## Priority 5: Missing Module Documentation

39 modules lack `@moduledoc`. Highest priority are the core modules users interact with:

**High priority:**
- `lib/deft/type.ex` - Type factory module
- `lib/deft/ast.ex` - AST protocol

**Medium priority:**
- All `lib/deft/type/*.ex` files (14 files)
- All `lib/deft/ast/*.ex` files (23 files)

**Low priority:**
- `lib/deft/generators.ex`
- `lib/deft/generators/code.ex`

---

## Verification Plan

After implementing changes:
1. `mix compile` - No compilation errors
2. `mix test` - All tests pass
3. `mix format --check-formatted` - Code formatted
4. `mix dialyzer` - No new warnings (if configured)

---

## Recommended Implementation Order

1. **Quick wins (30 min):**
   - Fix ARCHITECTURE.md references
   - Add missing error code documentation
   - Remove/update outdated TODO (4.2)
   - Fix slow algorithm (4.5)

2. **Medium effort (2-3 hours):**
   - Remove `@types` list, refactor `well_formed?/1`
   - Add explicit LocalCall pattern error handling
   - Extend predicate generation (4.3)

3. **Larger effort (optional):**
   - Binding propagation in generators (4.1)
   - Add `@moduledoc` to all modules
