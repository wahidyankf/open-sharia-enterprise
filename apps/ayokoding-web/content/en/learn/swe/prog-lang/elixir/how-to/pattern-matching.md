---
title: "Pattern Matching Effectively"
date: 2025-12-21T10:00:00+07:00
draft: false
description: "Master Elixir pattern matching for data extraction, control flow, function dispatch, and destructuring complex structures"
weight: 1000002
tags: ["elixir", "pattern-matching", "destructuring", "guards"]
---

# Pattern Matching Effectively

**Struggling with nested data extraction?** This guide teaches pattern matching techniques for destructuring complex structures, dispatching functions, validating data, and controlling program flow in Elixir.

## Problem

Pattern matching is fundamental to Elixir, but beginners often:

- Overuse explicit conditionals instead of pattern matching
- Miss opportunities for function head dispatch
- Struggle with nested structure extraction
- Don't leverage guards for validation
- Write verbose code when patterns could be concise

## Prerequisites

- [Quick Start Tutorial](/en/learn/swe/prog-lang/elixir/tutorials/quick-start) - Basic pattern matching
- [Beginner Tutorial](/en/learn/swe/prog-lang/elixir/tutorials/beginner) - Data structures

## Solution Overview

Pattern matching in Elixir:

1. **Assignment**: Binds values to variables
2. **Assertion**: Verifies structure matches pattern
3. **Extraction**: Pulls data from complex structures
4. **Dispatch**: Routes to different code paths

## Basic Patterns

### Simple Value Matching

```elixir
# Match exact values
{:ok, value} = {:ok, 42}
# value is 42

{:error, reason} = {:error, :not_found}
# reason is :not_found

# Assertion: structure must match
{:ok, result} = {:error, "fail"}
# ** (MatchError) no match of right hand side value: {:error, "fail"}
```

### List Patterns

```elixir
# Head and tail
[head | tail] = [1, 2, 3, 4, 5]
# head = 1, tail = [2, 3, 4, 5]

# Multiple elements
[first, second | rest] = [1, 2, 3, 4]
# first = 1, second = 2, rest = [3, 4]

# Empty list
[] = []
# Matches

# Specific length
[a, b, c] = [1, 2, 3]
# a = 1, b = 2, c = 3

[x, y] = [1, 2, 3]
# ** (MatchError) - list too long
```

### Map Patterns

```elixir
# Extract specific keys
%{name: name, age: age} = %{name: "Alice", age: 30, city: "NYC"}
# name = "Alice", age = 30
# city is not extracted but doesn't cause error

# Map must contain matched keys
%{id: id} = %{name: "Bob"}
# ** (MatchError) - :id key not found

# Match specific values
%{status: :ok, data: data} = %{status: :ok, data: [1, 2, 3]}
# data = [1, 2, 3]
```

### Tuple Patterns

```elixir
# Fixed-size structures
{:ok, value, timestamp} = {:ok, 42, ~U[2024-12-21 10:00:00Z]}

# Nested tuples
{:user, {name, age}, {city, country}} =
  {:user, {"Alice", 30}, {"NYC", "USA"}}
# name = "Alice", age = 30, city = "NYC", country = "USA"
```

## Advanced Patterns

### Nested Structure Extraction

**Problem**: Extract data from deeply nested structures.

**Solution**:

```elixir
defmodule UserExtractor do
  # Extract from nested API response
  def extract_user_data(response) do
    %{
      status: 200,
      body: %{
        user: %{
          id: id,
          profile: %{
            name: name,
            contacts: %{email: email, phone: phone}
          },
          settings: %{theme: theme}
        }
      }
    } = response

    %{
      id: id,
      name: name,
      email: email,
      phone: phone,
      theme: theme
    }
  end

  # With validation
  def extract_valid_user(response) do
    case response do
      %{
        status: 200,
        body: %{user: %{id: id, profile: %{name: name}} = user}
      } when is_integer(id) and is_binary(name) ->
        {:ok, user}

      %{status: status} when status >= 400 ->
        {:error, :http_error}

      _ ->
        {:error, :invalid_response}
    end
  end
end

# Usage
response = %{
  status: 200,
  body: %{
    user: %{
      id: 123,
      profile: %{
        name: "Alice",
        contacts: %{email: "alice@example.com", phone: "555-1234"}
      },
      settings: %{theme: :dark}
    }
  }
}

UserExtractor.extract_user_data(response)
# => %{id: 123, name: "Alice", email: "alice@example.com",
#      phone: "555-1234", theme: :dark}
```

**How It Works**: Nested pattern matches entire structure. Binds only needed variables. Validates structure in one expression.

### Function Head Dispatch

**Problem**: Handle different input types without conditionals.

**Solution**:

```elixir
defmodule Calculator do
  # Pattern match on operation type
  def calculate({:add, a, b}), do: a + b
  def calculate({:subtract, a, b}), do: a - b
  def calculate({:multiply, a, b}), do: a * b
  def calculate({:divide, a, 0}), do: {:error, :division_by_zero}
  def calculate({:divide, a, b}), do: a / b

  # Handle errors
  def calculate(_), do: {:error, :unknown_operation}
end

# Usage
Calculator.calculate({:add, 5, 3})        # => 8
Calculator.calculate({:divide, 10, 2})    # => 5.0
Calculator.calculate({:divide, 10, 0})    # => {:error, :division_by_zero}
Calculator.calculate({:unknown, 1, 2})    # => {:error, :unknown_operation}

# More complex dispatch
defmodule EventHandler do
  # Different event types
  def handle_event(%{type: :user_created, data: %{name: name, email: email}}) do
    send_welcome_email(email, name)
  end

  def handle_event(%{type: :user_updated, data: %{id: id, changes: changes}}) do
    sync_user_changes(id, changes)
  end

  def handle_event(%{type: :user_deleted, data: %{id: id}}) do
    cleanup_user_data(id)
  end

  # Catch-all for unknown events
  def handle_event(event) do
    Logger.warn("Unknown event: #{inspect(event)}")
    :ok
  end

  defp send_welcome_email(_email, _name), do: :ok
  defp sync_user_changes(_id, _changes), do: :ok
  defp cleanup_user_data(_id), do: :ok
end
```

**How It Works**: Elixir tries function clauses top-to-bottom. First matching pattern executes. Specific patterns before general ones.

### Pin Operator for Exact Matching

**Problem**: Match against existing variable value, not rebind.

**Solution**:

```elixir
defmodule Validator do
  # Match expected value
  def validate_status(response, expected_status) do
    case response do
      %{status: ^expected_status, body: body} ->
        {:ok, body}

      %{status: other_status} ->
        {:error, "Expected #{expected_status}, got #{other_status}"}
    end
  end

  # Filter list by value
  def find_by_id(items, target_id) do
    Enum.filter(items, fn
      %{id: ^target_id} -> true
      _ -> false
    end)
  end

  # Validate matching fields
  def validate_passwords(%{password: pass, password_confirmation: pass}) do
    {:ok, pass}
  end

  def validate_passwords(_) do
    {:error, "Passwords do not match"}
  end
end

# Usage
response = %{status: 200, body: "Success"}
Validator.validate_status(response, 200)
# => {:ok, "Success"}

Validator.validate_status(response, 404)
# => {:error, "Expected 404, got 200"}

# Pin in comprehensions
target = :admin
users = [
  %{id: 1, role: :admin},
  %{id: 2, role: :user},
  %{id: 3, role: :admin}
]

for %{role: ^target} = user <- users, do: user
# => [%{id: 1, role: :admin}, %{id: 3, role: :admin}]
```

**How It Works**: `^var` uses variable's current value in pattern. Without pin, variable would rebind to new value.

### Guards for Additional Validation

**Problem**: Pattern match with conditional checks.

**Solution**:

```elixir
defmodule GuardExamples do
  # Type guards
  def process(value) when is_integer(value), do: {:int, value * 2}
  def process(value) when is_binary(value), do: {:string, String.upcase(value)}
  def process(value) when is_list(value), do: {:list, length(value)}

  # Range guards
  def categorize_age(age) when age < 13, do: :child
  def categorize_age(age) when age < 20, do: :teenager
  def categorize_age(age) when age < 65, do: :adult
  def categorize_age(_age), do: :senior

  # Multiple conditions
  def valid_user?(%{age: age, email: email})
      when is_integer(age) and age >= 18 and is_binary(email) do
    String.contains?(email, "@")
  end

  def valid_user?(_), do: false

  # Complex guards
  def can_vote?(%{age: age, country: country})
      when age >= 18 and country in ["USA", "UK", "Canada"] do
    true
  end

  def can_vote?(_), do: false

  # Guard with pattern
  def handle_result({:ok, value}) when is_integer(value) and value > 0 do
    {:valid, value}
  end

  def handle_result({:ok, value}) when is_integer(value) do
    {:invalid, "Value must be positive"}
  end

  def handle_result({:error, reason}) do
    {:error, reason}
  end
end

# Usage
GuardExamples.process(42)           # => {:int, 84}
GuardExamples.process("hello")      # => {:string, "HELLO"}
GuardExamples.process([1, 2, 3])    # => {:list, 3}

GuardExamples.categorize_age(10)    # => :child
GuardExamples.categorize_age(15)    # => :teenager
GuardExamples.categorize_age(30)    # => :adult

GuardExamples.valid_user?(%{age: 25, email: "user@example.com"})
# => true

GuardExamples.can_vote?(%{age: 21, country: "USA"})
# => true
```

**How It Works**: Guards execute after pattern match succeeds. Limited to safe expressions (no side effects). Multiple conditions with `and`, `or`.

## Pattern Matching in Control Flow

### Case Expressions

```elixir
defmodule CaseExamples do
  def process_response(response) do
    case response do
      {:ok, %{status: 200, body: body}} ->
        {:success, Jason.decode!(body)}

      {:ok, %{status: 404}} ->
        {:error, :not_found}

      {:ok, %{status: status}} when status >= 500 ->
        {:error, :server_error}

      {:error, :timeout} ->
        {:error, :request_timeout}

      {:error, reason} ->
        {:error, {:unexpected, reason}}

      _ ->
        {:error, :unknown_response}
    end
  end

  # Pattern match on computed values
  def describe_length(list) do
    case length(list) do
      0 -> "Empty list"
      1 -> "Single item"
      n when n < 10 -> "Few items (#{n})"
      n -> "Many items (#{n})"
    end
  end
end
```

### With Expressions

```elixir
defmodule WithExamples do
  def create_user(params) do
    with {:ok, validated} <- validate_params(params),
         {:ok, user} <- insert_user(validated),
         {:ok, _email} <- send_confirmation(user) do
      {:ok, user}
    else
      {:error, :invalid_params} = error ->
        error

      {:error, :db_error} = error ->
        error

      {:error, :email_failed} ->
        # User created, email failed - still success
        {:ok, user}

      error ->
        {:error, {:unexpected, error}}
    end
  end

  # Pattern match intermediate results
  def process_data(input) do
    with {:ok, %{id: id, data: data}} <- parse_input(input),
         {:ok, transformed} <- transform_data(data),
         {:ok, result} when result > 0 <- compute_result(transformed) do
      {:ok, %{id: id, result: result}}
    else
      {:error, reason} -> {:error, reason}
      {:ok, 0} -> {:error, :zero_result}
      _ -> {:error, :unknown}
    end
  end

  defp validate_params(%{name: name, email: email})
      when is_binary(name) and is_binary(email) do
    {:ok, %{name: name, email: email}}
  end
  defp validate_params(_), do: {:error, :invalid_params}

  defp insert_user(params), do: {:ok, Map.put(params, :id, 123)}
  defp send_confirmation(_user), do: {:ok, "sent"}
  defp parse_input(input), do: {:ok, input}
  defp transform_data(data), do: {:ok, data}
  defp compute_result(data), do: {:ok, data}
end
```

### Comprehensions

```elixir
defmodule ComprehensionPatterns do
  # Pattern match in generators
  def extract_successes(results) do
    for {:ok, value} <- results, do: value
  end

  # Multiple patterns
  def extract_users(data) do
    for %{type: :user, payload: %{name: name, email: email}} <- data do
      %{name: name, email: email}
    end
  end

  # With guards
  def filter_adults(users) do
    for %{age: age} = user <- users, age >= 18, do: user
  end

  # Nested patterns
  def flatten_nested(items) do
    for %{data: %{values: values}} <- items,
        value <- values,
        do: value
  end
end

# Usage
results = [{:ok, 1}, {:error, "fail"}, {:ok, 2}, {:ok, 3}]
ComprehensionPatterns.extract_successes(results)
# => [1, 2, 3]

data = [
  %{type: :user, payload: %{name: "Alice", email: "alice@example.com"}},
  %{type: :post, payload: %{title: "Hello"}},
  %{type: :user, payload: %{name: "Bob", email: "bob@example.com"}}
]
ComprehensionPatterns.extract_users(data)
# => [%{name: "Alice", email: "alice@example.com"},
#     %{name: "Bob", email: "bob@example.com"}]
```

## Common Patterns

### Optional Fields

```elixir
defmodule OptionalFields do
  # Match with or without field
  def get_name(%{name: name}), do: name
  def get_name(_), do: "Unknown"

  # Default values
  def get_config(opts) do
    %{
      timeout: timeout,
      retries: retries
    } = Map.merge(%{timeout: 5000, retries: 3}, opts)

    {timeout, retries}
  end

  # Using Map.get with default
  def extract_user_info(user) do
    %{
      name: Map.get(user, :name, "Anonymous"),
      age: Map.get(user, :age, 0),
      city: Map.get(user, :city, "Unknown")
    }
  end
end
```

### Result Tuples

```elixir
defmodule ResultHandler do
  # Standard {:ok, value} / {:error, reason} pattern
  def handle_result(operation) do
    case operation do
      {:ok, value} when is_integer(value) ->
        process_int(value)

      {:ok, value} when is_binary(value) ->
        process_string(value)

      {:error, :not_found} ->
        default_value()

      {:error, reason} ->
        log_error(reason)
        {:error, reason}
    end
  end

  # Chaining results
  def chain_operations(input) do
    with {:ok, step1} <- operation1(input),
         {:ok, step2} <- operation2(step1),
         {:ok, final} <- operation3(step2) do
      {:ok, final}
    end
  end

  defp process_int(n), do: {:ok, n * 2}
  defp process_string(s), do: {:ok, String.upcase(s)}
  defp default_value, do: {:ok, 0}
  defp log_error(reason), do: IO.puts("Error: #{inspect(reason)}")
  defp operation1(x), do: {:ok, x}
  defp operation2(x), do: {:ok, x}
  defp operation3(x), do: {:ok, x}
end
```

### Struct Patterns

```elixir
defmodule User do
  defstruct [:id, :name, :email, :role]
end

defmodule StructPatterns do
  # Match struct type
  def greet(%User{name: name}) do
    "Hello, #{name}!"
  end

  # Match specific field values
  def is_admin?(%User{role: :admin}), do: true
  def is_admin?(%User{}), do: false

  # Extract multiple fields
  def user_summary(%User{id: id, name: name, email: email}) do
    "User ##{id}: #{name} (#{email})"
  end

  # Update struct with pattern
  def promote_user(%User{role: :user} = user) do
    %{user | role: :admin}
  end

  def promote_user(user), do: user
end

# Usage
user = %User{id: 1, name: "Alice", email: "alice@example.com", role: :user}

StructPatterns.greet(user)
# => "Hello, Alice!"

StructPatterns.is_admin?(user)
# => false

StructPatterns.promote_user(user)
# => %User{id: 1, name: "Alice", email: "alice@example.com", role: :admin}
```

## Best Practices

### Do: Use Specific Patterns First

```elixir
# Good: Specific to general
def process({:ok, %{status: 200, data: data}}), do: {:success, data}
def process({:ok, %{status: status}}) when status >= 400, do: {:error, status}
def process({:ok, response}), do: {:unknown, response}
def process({:error, reason}), do: {:failed, reason}

# Bad: General pattern first (unreachable clauses)
def process({:ok, _}), do: :ok  # This matches everything!
def process({:ok, %{status: 200, data: data}}), do: {:success, data}  # Never reached
```

### Do: Keep Patterns Readable

```elixir
# Good: Clear structure
def extract_user(%{
  id: id,
  profile: %{name: name, email: email},
  settings: %{theme: theme}
}) do
  %{id: id, name: name, email: email, theme: theme}
end

# Bad: Dense and hard to read
def extract_user(%{id: id, profile: %{name: name, email: email}, settings: %{theme: theme}}), do: %{id: id, name: name, email: email, theme: theme}
```

### Do: Use Guards for Validation

```elixir
# Good: Guard expresses intent
def divide(a, b) when is_number(a) and is_number(b) and b != 0 do
  a / b
end

# Bad: Conditional inside function
def divide(a, b) do
  if is_number(a) and is_number(b) and b != 0 do
    a / b
  else
    {:error, :invalid_input}
  end
end
```

### Don't: Overuse Pattern Matching

```elixir
# Good: Simple access
def get_name(user), do: user.name

# Bad: Unnecessary pattern match
def get_name(%{name: name}), do: name
```

## Troubleshooting

### MatchError: No Match

**Problem**: Pattern doesn't match actual data.

**Solution**: Use `IO.inspect` to see actual structure.

```elixir
# Inspect data structure
data
|> IO.inspect(label: "Actual data")
|> process()

# Or in IEx
iex> response = get_data()
iex> IO.inspect(response)
```

### Variable Already Bound

**Problem**: Reusing variable name in same pattern.

```elixir
# Bad: x appears twice
{x, x} = {1, 2}
# ** (MatchError)

# Good: Use guard
{x, y} = {1, 2}
if x == y, do: :equal, else: :different

# Or pin operator
x = 1
{^x, y} = {1, 2}  # y = 2, x must be 1
```

## Performance Considerations

Pattern matching is optimized by the compiler:

1. **Function Dispatch**: Very fast, compiled to jump tables
2. **Guards**: Executed in order, keep simple for performance
3. **Nested Patterns**: No performance penalty vs manual extraction

```elixir
# Both equally performant
def extract1(%{user: %{name: name}}), do: name
def extract2(data), do: data.user.name
```

## See Also

- [Cookbook - Pattern Matching Recipes](/en/learn/swe/prog-lang/elixir/how-to/cookbook#pattern-matching)
- [Beginner Tutorial - Pattern Matching](/en/learn/swe/prog-lang/elixir/tutorials/beginner#pattern-matching)
- [Error Handling Guide](/en/learn/swe/prog-lang/elixir/how-to/error-handling)
- [Best Practices - Pattern Matching](/en/learn/swe/prog-lang/elixir/explanation/best-practices)
