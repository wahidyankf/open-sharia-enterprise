---
name: swe-programming-elixir
description: Elixir coding standards from authoritative docs/explanation/software-engineering/programming-languages/elixir/ documentation
---

# Elixir Coding Standards

## Purpose

Progressive disclosure of Elixir coding standards for agents writing Elixir code.

**Authoritative Source**: [docs/explanation/software-engineering/programming-languages/elixir/README.md](../../../docs/explanation/software-engineering/programming-languages/elixir/README.md)

**Usage**: Auto-loaded for agents when writing Elixir code. Provides quick reference to idioms, best practices, and antipatterns.

## Quick Standards Reference

### Naming Conventions

**Modules**: PascalCase

- `UserAccount`, `PaymentProcessor`

**Functions and Variables**: snake_case

- Functions: `calculate_total/1`, `find_user_by_id/1`
- Variables: `user_name`, `total_amount`

**Atoms**: lowercase with underscores

- `:ok`, `:error`, `:not_found`

**Private Functions**: Prefix with `def` (not `defp` for documentation)

- Use `@doc false` for private but need to document

### Modern Elixir Features (1.14+)

**Pattern Matching**: Use extensively

```elixir
case result do
  {:ok, value} -> process_value(value)
  {:error, reason} -> handle_error(reason)
  _ -> :unknown
end
```

**Pipe Operator**: Chain transformations

```elixir
data
|> parse()
|> validate()
|> process()
|> format()
```

**With Statement**: Handle multiple operations

```elixir
with {:ok, user} <- find_user(id),
     {:ok, account} <- find_account(user.account_id),
     {:ok, balance} <- get_balance(account) do
  {:ok, balance}
end
```

**Protocols**: Use for polymorphism

```elixir
defprotocol Validator do
  def validate(data)
end
```

### Error Handling

**Tagged Tuples**: Use for results

```elixir
{:ok, result} | {:error, reason}
```

**Exceptions**: Only for exceptional cases

```elixir
raise ArgumentError, "invalid input"
```

**With Else**: Handle error cases

```elixir
with {:ok, result} <- do_something() do
  result
else
  {:error, :not_found} -> default_value()
  error -> handle_error(error)
end
```

### Concurrency

**GenServer**: Use for stateful processes

```elixir
defmodule Counter do
  use GenServer

  def init(initial_value) do
    {:ok, initial_value}
  end

  def handle_call(:get, _from, state) do
    {:reply, state, state}
  end
end
```

**Task**: Use for async operations

```elixir
task = Task.async(fn -> expensive_operation() end)
result = Task.await(task)
```

**Supervision**: Always supervise processes

```elixir
children = [
  {Counter, 0},
  {Worker, []}
]

Supervisor.start_link(children, strategy: :one_for_one)
```

### Testing Standards

**ExUnit**: Built-in testing framework

```elixir
defmodule UserTest do
  use ExUnit.Case

  test "creates user with valid data" do
    assert {:ok, user} = User.create(%{name: "John"})
    assert user.name == "John"
  end
end
```

**Doctests**: Test examples in documentation

```elixir
@doc """
Doubles a number.

## Examples

    iex> double(5)
    10
"""
def double(n), do: n * 2
```

### Security Practices

**Input Validation**: Validate all external input

- Use Ecto changesets for data validation
- Check types and formats

**SQL Injection**: Use Ecto queries

```elixir
from(u in User, where: u.id == ^user_id)
```

**Secrets Management**: Use runtime configuration

```elixir
# config/runtime.exs
config :my_app, api_key: System.get_env("API_KEY")
```

## Comprehensive Documentation

For detailed guidance, refer to:

- **[Idioms](../../../docs/explanation/software-engineering/programming-languages/elixir/ex-soen-prla-ex__idioms.md)** - Elixir-specific patterns
- **[Best Practices](../../../docs/explanation/software-engineering/programming-languages/elixir/ex-soen-prla-ex__best-practices.md)** - Clean code standards
- **[Anti-Patterns](../../../docs/explanation/software-engineering/programming-languages/elixir/ex-soen-prla-ex__anti-patterns.md)** - Common mistakes

## Related Skills

- docs-applying-content-quality
- repo-practicing-trunk-based-development

## References

- [Elixir README](../../../docs/explanation/software-engineering/programming-languages/elixir/README.md)
- [Functional Programming](../../../governance/development/pattern/functional-programming.md)
