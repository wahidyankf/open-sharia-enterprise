---
title: "Polymorphism with Protocols"
date: 2025-12-21T18:05:00+07:00
draft: false
description: "Implement polymorphic behavior in Elixir using protocols for type-based dispatch and extensible APIs without modifying existing code."
weight: 1000017
tags: ["elixir", "protocols", "polymorphism", "extensibility", "how-to"]
---

**Need polymorphic behavior across types?** Protocols enable type-based dispatch for extensible APIs without modifying existing code, providing powerful polymorphism in functional programming.

## Prerequisites

- Understanding of Elixir modules and structs
- Basic knowledge of data types
- Familiarity with function dispatch

## Problem

You want different types to respond to the same function differently without case statements or type checking. Traditional approaches require modifying existing code when adding new types.

**Challenges:**

- Implementing type-specific behavior without conditionals
- Extending functionality for new types without changing core code
- Creating flexible APIs that work across multiple types
- Avoiding tight coupling between implementations
- Maintaining type safety with custom behavior

## Solution

Use **protocols** to define a common interface that multiple types can implement independently.

## How It Works

### 1. Define Protocol

```elixir
defprotocol Serializable do
  @doc "Convert data to JSON string"
  def to_json(data)

  @doc "Convert data to XML string"
  def to_xml(data)
end
```

**Best Practices:**

- Use clear, descriptive protocol names
- Document all protocol functions
- Define one focused responsibility per protocol
- Consider return types and error cases

### 2. Implement for Built-in Types

```elixir
defimpl Serializable, for: List do
  def to_json(list) do
    Jason.encode!(list)
  end

  def to_xml(list) do
    items = Enum.map_join(list, "\n", fn item ->
      "  <item>#{item}</item>"
    end)
    "<list>\n#{items}\n</list>"
  end
end

defimpl Serializable, for: Map do
  def to_json(map) do
    Jason.encode!(map)
  end

  def to_xml(map) do
    entries = Enum.map_join(map, "\n", fn {k, v} ->
      "  <#{k}>#{v}</#{k}>"
    end)
    "<map>\n#{entries}\n</map>"
  end
end

defimpl Serializable, for: BitString do
  def to_json(string) do
    Jason.encode!(string)
  end

  def to_xml(string) do
    "<string>#{string}</string>"
  end
end
```

### 3. Implement for Custom Structs

```elixir
defmodule User do
  defstruct [:id, :name, :email]
end

defimpl Serializable, for: User do
  def to_json(%User{} = user) do
    Jason.encode!(%{
      id: user.id,
      name: user.name,
      email: user.email,
      type: "user"
    })
  end

  def to_xml(%User{} = user) do
    """
    <user>
      <id>#{user.id}</id>
      <name>#{user.name}</name>
      <email>#{user.email}</email>
    </user>
    """
  end
end
```

### 4. Protocol Usage

```elixir
# Works with any type that implements Serializable
user = %User{id: 1, name: "Alice", email: "alice@example.com"}
Serializable.to_json(user)
# => "{\"id\":1,\"name\":\"Alice\",\"email\":\"alice@example.com\",\"type\":\"user\"}"

Serializable.to_json([1, 2, 3])
# => "[1,2,3]"

Serializable.to_json(%{key: "value"})
# => "{\"key\":\"value\"}"

Serializable.to_xml(user)
# => "<user>\n  <id>1</id>\n  <name>Alice</name>\n  <email>alice@example.com</email>\n</user>"
```

## Advanced Patterns

### 1. Protocol Consolidation

```elixir
# In mix.exs for production performance
def project do
  [
    app: :my_app,
    version: "0.1.0",
    consolidate_protocols: Mix.env() != :test
  ]
end
```

Protocol consolidation happens at compile time, converting dynamic dispatch to static dispatch for better performance.

### 2. Fallback Implementation

```elixir
defprotocol Inspectable do
  @fallback_to_any true
  def inspect(data)
end

# Default implementation for any type
defimpl Inspectable, for: Any do
  def inspect(data) do
    "Unknown type: #{Kernel.inspect(data)}"
  end
end

# Specific implementation
defimpl Inspectable, for: User do
  def inspect(%User{name: name}) do
    "User: #{name}"
  end
end
```

### 3. Protocol Composition

```elixir
defprotocol Comparable do
  @doc "Compare two values, returns :lt, :eq, or :gt"
  def compare(a, b)
end

defprotocol Sortable do
  @doc "Sort a collection"
  def sort(collection)
end

defimpl Sortable, for: List do
  def sort(list) do
    Enum.sort(list, fn a, b ->
      Comparable.compare(a, b) != :gt
    end)
  end
end
```

### 4. Generic Protocol Functions

```elixir
defprotocol Size do
  @doc "Return the size of a data structure"
  def size(data)
end

defimpl Size, for: List do
  def size(list), do: length(list)
end

defimpl Size, for: Map do
  def size(map), do: map_size(map)
end

defimpl Size, for: Tuple do
  def size(tuple), do: tuple_size(tuple)
end

defimpl Size, for: BitString do
  def size(string), do: String.length(string)
end
```

## Real-World Examples

### 1. API Response Rendering

```elixir
defprotocol Renderable do
  @doc "Render data in requested format"
  def render(data, format)
end

defimpl Renderable, for: User do
  def render(user, :json) do
    %{
      id: user.id,
      name: user.name,
      email: user.email
    }
  end

  def render(user, :public) do
    %{
      name: user.name
    }
  end

  def render(user, :admin) do
    %{
      id: user.id,
      name: user.name,
      email: user.email,
      created_at: user.inserted_at,
      updated_at: user.updated_at
    }
  end
end
```

### 2. Event Handling

```elixir
defprotocol EventHandler do
  @doc "Handle an event and return new state"
  def handle_event(event, state)
end

defmodule UserCreatedEvent do
  defstruct [:user_id, :timestamp]
end

defmodule UserDeletedEvent do
  defstruct [:user_id, :timestamp]
end

defimpl EventHandler, for: UserCreatedEvent do
  def handle_event(%{user_id: id} = event, state) do
    Logger.info("User created: #{id}")
    %{state | user_count: state.user_count + 1}
  end
end

defimpl EventHandler, for: UserDeletedEvent do
  def handle_event(%{user_id: id} = event, state) do
    Logger.info("User deleted: #{id}")
    %{state | user_count: state.user_count - 1}
  end
end
```

### 3. Validation Protocol

```elixir
defprotocol Validator do
  @doc "Validate data and return {:ok, data} or {:error, reasons}"
  def validate(data)
end

defimpl Validator, for: User do
  def validate(%User{} = user) do
    with :ok <- validate_email(user.email),
         :ok <- validate_name(user.name) do
      {:ok, user}
    end
  end

  defp validate_email(email) when is_binary(email) do
    if String.contains?(email, "@") do
      :ok
    else
      {:error, "Invalid email format"}
    end
  end

  defp validate_name(name) when is_binary(name) and byte_size(name) > 0 do
    :ok
  end

  defp validate_name(_), do: {:error, "Name cannot be empty"}
end
```

## Common Pitfalls

### 1. Over-Engineering with Protocols

**Problem:**

```elixir
# Don't create protocols for single implementations
defprotocol SingleUse do
  def process(data)
end

defimpl SingleUse, for: User do
  def process(user), do: user.name
end
```

**Solution:**

```elixir
# Use regular functions instead
def process_user(%User{} = user), do: user.name
```

### 2. Forgetting @fallback_to_any

```elixir
# Without fallback - raises Protocol.UndefinedError
defprotocol Printer do
  def print(data)
end

# With fallback - graceful handling
defprotocol Printer do
  @fallback_to_any true
  def print(data)
end

defimpl Printer, for: Any do
  def print(data), do: Kernel.inspect(data)
end
```

### 3. Protocol Implementation Conflicts

```elixir
# Be careful with duplicate implementations
# This will raise a compilation error
defimpl MyProtocol, for: User do
  def my_func(user), do: :first
end

defimpl MyProtocol, for: User do
  def my_func(user), do: :second  # Error!
end
```

## Performance Considerations

### Protocol Dispatch Cost

```elixir
# Protocol dispatch (dynamic)
Serializable.to_json(data)  # ~50-100ns overhead

# Direct function call
User.to_json(data)  # No overhead

# Consolidation reduces protocol overhead in production
```

### When to Use Protocols

**Good Use Cases:**

- Library APIs with extensible behavior
- Plugin systems
- Type-based routing
- Polymorphic collections

**Avoid For:**

- Single implementation
- Performance-critical hot paths (unless consolidated)
- Simple conditional logic

## Built-in Protocols

Elixir includes several built-in protocols:

```elixir
# Enumerable - iterate over collections
Enum.map([1, 2, 3], &(&1 * 2))
Enum.map(%{a: 1, b: 2}, fn {k, v} -> {k, v * 2} end)

# String.Chars - convert to string (to_string/1)
to_string(123)
to_string(:atom)

# Inspect - inspect representation (inspect/1)
inspect(%User{name: "Alice"})

# Collectable - collect items into structure (Enum.into/2)
Enum.into([a: 1], %{})
```

## Testing Protocols

```elixir
defmodule SerializableTest do
  use ExUnit.Case

  defmodule TestStruct do
    defstruct [:value]
  end

  defimpl Serializable, for: TestStruct do
    def to_json(%TestStruct{value: value}) do
      Jason.encode!(%{value: value})
    end

    def to_xml(%TestStruct{value: value}) do
      "<test><value>#{value}</value></test>"
    end
  end

  test "serializes struct to JSON" do
    struct = %TestStruct{value: 42}
    assert Serializable.to_json(struct) == "{\"value\":42}"
  end

  test "serializes struct to XML" do
    struct = %TestStruct{value: 42}
    assert Serializable.to_xml(struct) == "<test><value>42</value></test>"
  end

  test "works with built-in types" do
    assert Serializable.to_json([1, 2]) == "[1,2]"
    assert Serializable.to_json(%{a: 1}) == "{\"a\":1}"
  end
end
```

## Protocols vs. Behaviors

**Protocols:**

- Type-based dispatch (polymorphism on data)
- Implemented outside the module
- Can extend existing types
- Dynamic dispatch (unless consolidated)

**Behaviors:**

- Module-based contracts
- Implemented inside the module (`use` or `@behaviour`)
- Compile-time checking
- Static dispatch

```elixir
# Protocol - dispatch on data type
defprotocol Serializable do
  def to_json(data)
end

# Behavior - module must implement functions
defmodule MyBehaviour do
  @callback handle(term()) :: term()
end

defmodule MyImpl do
  @behaviour MyBehaviour

  def handle(data), do: data
end
```

## Related Resources

- [Beginner Tutorial](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner)
- [Advanced Tutorial](/en/learn/software-engineering/programming-language/elixir/tutorials/advanced)
- [Best Practices](/en/learn/software-engineering/programming-language/elixir/explanation/best-practices)
- [GenServer Guide](/en/learn/software-engineering/programming-language/elixir/how-to/genserver)
