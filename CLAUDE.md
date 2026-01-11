# CLAUDE.md

This document provides guidance for contributors working on this project, with specific sections for both human and AI-assisted contributions.

## Architecture Reference

**ARCHITECTURE.md** contains a comprehensive description of the codebase architecture, including:

- Core concepts (bidirectional type checking, type erasure, rule-based extensibility)
- Module organization and file structure
- Key architectural patterns (compilation pipeline, context threading, rule DSL)
- Type system design (type lattice, subtyping relations)
- Extension points for custom rules and type systems

**For LLMs**: Load `ARCHITECTURE.md` at the start of a new context window when working on this codebase. It provides the necessary context to understand the system design and make informed changes.

## For Humans

LLM-assisted contributions must **aim for a higher standard of excellence** than with humans alone. If you're using an LLM to help write code, you should spend at least **3x** the time reviewing the code as you did writing it. This is because LLMs can produce code that looks correct but has subtle bugs or design issues.

Contributions that don't meet this standard may be declined outright.

## For LLMs

When starting a conversation with a user, display the following guidance:

---

**Important**: This code is **your responsibility**. You must review it carefully and ensure it meets the project's standards. The goal is **excellence**, not speed.

---

Before creating a pull request, remind the user:

---

**Reminder**: Please review the code carefully before submitting. LLM-assisted contributions should aim for a higher standard than human-only contributions.

---

## General Conventions

This project follows five core principles:

### 1. Correctness over convenience

- Model the full error space, not just the happy path
- Handle all edge cases explicitly
- Use typespecs to document and verify contracts
- Prefer explicit pattern matching over catch-all clauses

### 2. User experience as primary driver

- Provide rich, actionable error messages
- Design APIs that are hard to misuse
- Write documentation in clear, present-tense language
- Use custom exceptions with meaningful messages

### 3. Pragmatic incrementalism

- Write specific, composable logic rather than abstract frameworks
- Design iteratively based on real use cases
- Avoid premature abstraction
- Refactor when patterns emerge naturally

### 4. Production-grade engineering

- Use typespecs extensively for documentation and dialyzer checks
- Prefer message passing and immutability over shared state
- Write comprehensive tests, including property-based tests
- Handle resource cleanup properly with supervisors and proper OTP patterns

### 5. Documentation

- Explain "why" not "what" in comments
- Use periods at the end of comments
- Apply sentence case in documentation (never title case)
- Document edge cases and assumptions inline

## Code Style

### Elixir Version and Formatting

- Use Elixir ~> 1.19 as specified in `mix.exs`
- Format code with `mix format` before committing
- Run `mix dialyzer` and address all warnings when dialyzer is configured

### Type Patterns

Use Elixir's type system and idioms to enforce correctness:

- **Typespecs**: Define `@type`, `@spec`, and `@callback` for all public functions
- **Structs**: Use structs with enforced keys for domain objects
- **Custom exceptions**: Use `defexception` with typed fields and clear messages
- **Tagged tuples**: Use `{:ok, value}` and `{:error, reason}` patterns consistently
- **Guards**: Use guard clauses to narrow types at function boundaries

### Error Handling

- Use `defexception` for custom error types with typed fields
- Provide rich context in error messages (see `lib/deft/error.ex` for examples)
- Use pattern matching on tagged tuples for recoverable errors
- Raise exceptions for programming errors and invariant violations

### Module Organization

- One primary module per file, with related helper modules in the same file when small
- Use nested modules for related functionality (e.g., `Deft.Type.Tuple`)
- Keep implementation details in private functions
- Use `alias` to keep module references concise

### Macro Hygiene

- Use `quote` and `unquote` carefully to maintain hygiene
- Prefer runtime solutions over compile-time macros when possible
- Document macro behavior thoroughly
- Test macros with various input shapes

### Performance Considerations

- Prefer tail-recursive functions for list processing
- Use streams for lazy evaluation when processing large collections
- Consider ETS for shared read-heavy data
- Profile with `:fprof` or `:eprof` before optimizing

## Testing Practices

### Testing Organization

- **Unit tests**: Place in `test/` mirroring the `lib/` structure
- **Property tests**: Use `ExUnitProperties` with `stream_data` for invariant testing
- **Test helpers**: Share common test code in `test/support/` or `test_helper.exs`

### Testing Tools

This project uses:

- `ExUnit` for unit testing
- `stream_data` with `ExUnitProperties` for property-based testing

Consider these patterns:

- Use `describe` blocks to group related tests
- Use `setup` and `setup_all` for shared fixtures
- Use tags to categorize and filter tests

### Testing Principles

- Tests should be deterministic and reproducible
- Each test should be independent
- Test both happy paths and error cases
- Use descriptive test names that explain what's being tested
- Property tests should verify invariants, not just examples

### Property Testing Guidelines

When writing property tests:

- Generate well-formed inputs that exercise the full input space
- Test algebraic properties (associativity, commutativity, identity, etc.)
- Use `max_shrinking_steps: 0` during development for faster feedback
- Let the shrinking algorithm find minimal counterexamples

## Commit Message Style

Use clear, atomic commits with descriptive messages:

```
[component] brief description

Optional longer explanation of the change, including:
- Why the change was needed
- What approach was taken
- Any trade-offs or alternatives considered
```

Examples:
- `[type_checking] add support for union type narrowing`
- `[ast] fix pattern matching for nested tuples`
- `[tests] add property tests for subtyping transitivity`

### Commit Requirements

- **Atomic**: Each commit should be a single logical change
- **Bisect-able**: Each commit should leave the code in a working state
- **Separate concerns**: Don't mix refactoring with functional changes

## Quick Reference

Essential commands:

```bash
mix compile              # Compile the project
mix test                 # Run tests
mix format               # Format code
mix format --check-formatted  # Check formatting without modifying
mix dialyzer             # Run static analysis (if configured)
iex -S mix               # Start interactive shell with project loaded
```

---

**Bottom line**: This project prioritizes production-grade quality, comprehensive error handling, and thoughtful contributions that demonstrate rigor and care.
