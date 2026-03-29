---
title: "Structs Protocols"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000014
description: "From maps to structs for compile-time guarantees, protocols for polymorphism in production Elixir"
tags: ["elixir", "structs", "protocols", "polymorphism", "data-modeling", "compile-time"]
prev: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/immutability-patterns"
next: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/type-specifications"
---

**How do you model domain data with compile-time safety and polymorphic behavior?** This guide teaches the progression from maps through structs to protocols, showing when each abstraction provides production value for data modeling.

## Why It Matters

Maps are Elixir's standard data structure, but production applications need stronger guarantees. Real-world requirements:

- **Type safety** - Prevent runtime errors from missing or wrong keys
- **Data validation** - Enforce business rules at compile time
- **Polymorphism** - Same operation across different data types
- **Domain modeling** - Represent business concepts with structure

Real-world scenarios requiring structs and protocols:

- **Financial systems** - Money with currency validation and arithmetic
- **E-commerce** - Products, orders with type-specific behavior
- **API clients** - Response types with polymorphic serialization
- **Domain models** - User accounts, transactions with guarantees
- **Event systems** - Event types with polymorphic handling

Production question: When should you upgrade from maps to structs? When do you need protocols for polymorphism? The answer depends on your compile-time safety and extensibility requirements.

## Maps - Standard Library Foundation

Maps are Elixir's built-in key-value data structure.

### Basic Map Usage

```elixir
# Create map with atom keys
account = %{
  id: "acc_123",          # => Key: :id, Value: "acc_123"
  balance: 1000,          # => Key: :balance, Value: 1000
  currency: "USD"         # => Key: :currency, Value: "USD"
}
# => Returns map
# => Type: %{id: binary(), balance: integer(), currency: binary()}
# => No compile-time guarantees

# Access values
account[:id]              # => Returns "acc_123"
account.balance           # => Returns 1000
                          # => Dot notation only works with atom keys
```

Maps work for simple cases but lack structure.

### Map Limitations in Production

```elixir
# Typo in key - runtime error
account = %{
  id: "acc_123",
  balence: 1000           # => Typo: "balence" instead of "balance"
}
# => No compile-time error
# => Returns map with wrong key

account.balance           # => KeyError at runtime!
                          # => No compile-time detection

# Missing required keys
transfer = %{
  from: "acc_123",
  to: "acc_456"
  # Missing: amount
}
# => No compile-time error
# => Runtime error when accessing amount

# Wrong types
payment = %{
  amount: "100",          # => String instead of integer
  currency: :usd          # => Atom instead of string
}
# => No compile-time validation
# => Type errors appear in business logic
```

Production problems with raw maps:

- **No compile-time key validation** - Typos become runtime errors
- **No required keys enforcement** - Missing data causes crashes
- **No type checking** - Wrong types propagate through system
- **No default values** - Must handle missing keys everywhere
- **No polymorphism** - Cannot define behavior per type

## Structs - Compile-Time Guarantees

Structs add compile-time structure and validation to maps.

### Defining Structs

```elixir
defmodule Money do
  @enforce_keys [:amount, :currency]   # => Required keys at compile time
  defstruct [:amount, :currency]       # => Struct definition
                                       # => Creates %Money{} type
end
# => Returns module
# => Defines Money struct with required fields

# Create valid struct
price = %Money{amount: 1000, currency: "USD"}
# => Returns %Money{amount: 1000, currency: "USD"}
# => Type: %Money{}
# => Compile-time validation passed

# Missing required key - compile error
invalid = %Money{amount: 1000}
# => Compile error: required key :currency not found
# => Catches error before runtime
```

Structs enforce required keys at compile time.

### Struct with Default Values

```elixir
defmodule Account do
  @enforce_keys [:id]                  # => Only :id required
  defstruct [
    :id,                               # => Required field
    balance: 0,                        # => Default: 0
    currency: "USD",                   # => Default: "USD"
    active: true                       # => Default: true
  ]
end
# => Returns module
# => Creates Account struct with defaults

account = %Account{id: "acc_123"}
# => Returns %Account{
#      id: "acc_123",
#      balance: 0,
#      currency: "USD",
#      active: true
#    }
# => Defaults applied automatically
```

Default values reduce boilerplate and provide safe fallbacks.

### Pattern Matching with Structs

```elixir
defmodule Transfer do
  defstruct [:from_account, :to_account, :amount, :currency]
end
# => Returns module

# Pattern match on struct type
def process_transfer(%Transfer{} = transfer) do
  # => Matches only Transfer structs
  # => Compile-time type narrowing
  %Transfer{
    from_account: from,              # => Extract from_account
    to_account: to,                  # => Extract to_account
    amount: amount,                  # => Extract amount
    currency: currency               # => Extract currency
  } = transfer

  validate_transfer(from, to, amount, currency)
  # => Calls validation with extracted values
end

def process_transfer(_other) do
  # => Matches non-Transfer values
  {:error, :invalid_transfer_type}
  # => Type mismatch caught
end
```

Pattern matching provides compile-time type checking.

### Updating Structs

```elixir
account = %Account{id: "acc_123", balance: 1000}
# => Returns %Account{id: "acc_123", balance: 1000, currency: "USD", active: true}

# Update fields (immutable)
updated = %{account | balance: 1500}
# => Returns new %Account{} with balance: 1500
# => Original account unchanged (immutability)
# => Type: %Account{}

# Cannot add new fields
invalid = %{account | new_field: "value"}
# => Compile error: unknown key :new_field for struct Account
# => Prevents typos and invalid fields

# Cannot update to wrong struct type
other = %Money{amount: 100, currency: "USD"}
mixed = %{other | balance: 200}
# => Compile error: key :balance not found in struct Money
# => Type safety enforced
```

Struct updates maintain type safety and prevent invalid operations.

## Financial Domain Example with Structs

```elixir
defmodule Money do
  @enforce_keys [:amount, :currency]
  defstruct [:amount, :currency]

  def new(amount, currency) when is_integer(amount) and amount >= 0 do
    # => Validates amount is non-negative integer
    %Money{amount: amount, currency: currency}
    # => Returns validated Money struct
  end

  def new(_amount, _currency) do
    # => Invalid amount
    {:error, :invalid_amount}
    # => Returns error tuple
  end

  def add(%Money{currency: curr} = m1, %Money{currency: curr} = m2) do
    # => Pattern match: same currency required
    %Money{amount: m1.amount + m2.amount, currency: curr}
    # => Returns new Money with sum
    # => Preserves currency
  end

  def add(%Money{}, %Money{}) do
    # => Different currencies
    {:error, :currency_mismatch}
    # => Cannot add different currencies
  end
end
# => Returns module

# Valid operations
price1 = Money.new(1000, "USD")        # => %Money{amount: 1000, currency: "USD"}
price2 = Money.new(500, "USD")         # => %Money{amount: 500, currency: "USD"}
total = Money.add(price1, price2)      # => %Money{amount: 1500, currency: "USD"}

# Invalid operations caught
invalid_money = Money.new(-100, "USD") # => {:error, :invalid_amount}
mixed_money = Money.new(100, "EUR")    # => %Money{amount: 100, currency: "EUR"}
error = Money.add(price1, mixed_money) # => {:error, :currency_mismatch}
```

Structs enable domain-specific validation and business rules.

## Protocols - Polymorphic Behavior

Protocols define behavior that multiple types can implement independently.

### Built-in Protocols

Elixir provides standard protocols for common operations.

#### String.Chars Protocol

```elixir
# String.Chars defines to_string/1 behavior
price = Money.new(1000, "USD")

# Without protocol implementation
IO.puts(price)
# => Error: protocol String.Chars not implemented for %Money{}
# => Cannot convert to string

# Implement String.Chars for Money
defimpl String.Chars, for: Money do
  def to_string(%Money{amount: amount, currency: currency}) do
    # => Extract amount and currency
    formatted_amount = :erlang.float_to_binary(amount / 100, decimals: 2)
    # => Converts cents to dollars
    # => Returns "10.00" for amount 1000
    "#{currency} #{formatted_amount}"
    # => Returns "USD 10.00"
  end
end
# => Returns implementation module

# Now works with to_string/1
IO.puts(price)                         # => Output: USD 10.00
"Price: #{price}"                      # => Returns "Price: USD 10.00"
                                       # => String interpolation uses to_string/1
```

Protocols enable polymorphic behavior across types.

#### Enumerable Protocol

```elixir
defmodule OrderItems do
  defstruct items: []

  def add_item(%OrderItems{items: items}, item) do
    # => Adds item to order
    %OrderItems{items: [item | items]}
    # => Returns new OrderItems with prepended item
  end
end
# => Returns module

# Implement Enumerable for OrderItems
defimpl Enumerable, for: OrderItems do
  def count(%OrderItems{items: items}) do
    # => Returns item count
    {:ok, length(items)}
    # => Tuple format required by protocol
  end

  def member?(%OrderItems{items: items}, item) do
    # => Checks membership
    {:ok, Enum.member?(items, item)}
    # => Returns tuple with boolean
  end

  def reduce(%OrderItems{items: items}, acc, fun) do
    # => Delegates to List reduce
    Enumerable.List.reduce(items, acc, fun)
    # => Enables all Enum functions
  end

  def slice(%OrderItems{items: items}) do
    # => Enables slicing operations
    {:ok, length(items), &Enumerable.List.slice(items, &1, &2, 1)}
    # => Returns size and slice function
  end
end
# => Returns implementation module

# Now works with Enum module
order = %OrderItems{items: [
  Money.new(1000, "USD"),
  Money.new(500, "USD")
]}
# => Returns OrderItems struct

Enum.count(order)                      # => Returns 2
Enum.map(order, &(&1.amount))         # => Returns [1000, 500]
total = Enum.reduce(order, 0, fn item, acc ->
  acc + item.amount                    # => Sums amounts
end)
# => Returns 1500
```

Enumerable protocol enables standard collection operations.

### Custom Protocols

Define your own protocols for domain-specific polymorphism.

#### Arithmetic Protocol for Money

```elixir
defprotocol Arithmetic do
  @doc "Add two values"
  def add(a, b)

  @doc "Subtract second value from first"
  def subtract(a, b)
end
# => Returns protocol definition
# => Any type can implement this

# Implement for Money
defimpl Arithmetic, for: Money do
  def add(%Money{currency: curr} = m1, %Money{currency: curr} = m2) do
    # => Same currency required
    {:ok, %Money{amount: m1.amount + m2.amount, currency: curr}}
    # => Returns result tuple
  end

  def add(%Money{}, %Money{}) do
    # => Different currencies
    {:error, :currency_mismatch}
    # => Cannot add different currencies
  end

  def subtract(%Money{currency: curr} = m1, %Money{currency: curr} = m2) do
    # => Same currency required
    result = m1.amount - m2.amount
    # => Calculate difference

    if result >= 0 do
      {:ok, %Money{amount: result, currency: curr}}
      # => Non-negative result
    else
      {:error, :negative_balance}
      # => Prevent negative money
    end
  end

  def subtract(%Money{}, %Money{}) do
    # => Different currencies
    {:error, :currency_mismatch}
  end
end
# => Returns implementation module

# Polymorphic arithmetic
price1 = Money.new(1000, "USD")        # => %Money{amount: 1000, currency: "USD"}
price2 = Money.new(300, "USD")         # => %Money{amount: 300, currency: "USD"}

{:ok, total} = Arithmetic.add(price1, price2)
# => Returns {:ok, %Money{amount: 1300, currency: "USD"}}

{:ok, difference} = Arithmetic.subtract(price1, price2)
# => Returns {:ok, %Money{amount: 700, currency: "USD"}}

Arithmetic.subtract(price2, price1)
# => Returns {:error, :negative_balance}
# => Prevents invalid state
```

Custom protocols enable domain-specific polymorphism with business rules.

#### Serialization Protocol

```elixir
defprotocol Serializable do
  @doc "Convert value to JSON-compatible map"
  def to_json(value)
end
# => Returns protocol definition

# Implement for Money
defimpl Serializable, for: Money do
  def to_json(%Money{amount: amount, currency: currency}) do
    # => Extract fields
    %{
      amount: amount,                  # => Integer amount in cents
      currency: currency,              # => Currency code
      formatted: to_string(%Money{amount: amount, currency: currency})
      # => Human-readable format
    }
    # => Returns JSON-compatible map
  end
end
# => Returns implementation module

# Implement for Account
defimpl Serializable, for: Account do
  def to_json(%Account{id: id, balance: balance, currency: currency, active: active}) do
    # => Extract all fields
    %{
      id: id,
      balance: balance,
      currency: currency,
      active: active,
      type: "account"                  # => Type discriminator
    }
    # => Returns JSON-compatible map
  end
end
# => Returns implementation module

# Polymorphic serialization
serialize_for_api = fn value ->
  value
  |> Serializable.to_json()           # => Protocol dispatch
  |> Jason.encode!()                  # => JSON encoding
end
# => Returns anonymous function

price = Money.new(1500, "USD")
serialize_for_api.(price)
# => Returns JSON string with Money representation

account = %Account{id: "acc_123", balance: 1000}
serialize_for_api.(account)
# => Returns JSON string with Account representation
# => Same function handles different types
```

Serializable protocol enables polymorphic conversion without type checking.

## Production Pattern - Domain Events

```elixir
# Define event types with structs
defmodule Events do
  defmodule PaymentReceived do
    @enforce_keys [:account_id, :amount, :timestamp]
    defstruct [:account_id, :amount, :timestamp, metadata: %{}]
  end

  defmodule PaymentSent do
    @enforce_keys [:account_id, :amount, :timestamp]
    defstruct [:account_id, :amount, :timestamp, metadata: %{}]
  end

  defmodule AccountClosed do
    @enforce_keys [:account_id, :timestamp]
    defstruct [:account_id, :timestamp, reason: nil]
  end
end
# => Returns module with event definitions

# Define protocol for event handling
defprotocol EventHandler do
  @doc "Process domain event"
  def handle(event)
end
# => Returns protocol definition

# Implement for each event type
defimpl EventHandler, for: Events.PaymentReceived do
  def handle(%Events.PaymentReceived{account_id: id, amount: amount}) do
    # => Extract event data
    # Increase account balance
    AccountService.increase_balance(id, amount)
    # => Returns {:ok, updated_account}
  end
end

defimpl EventHandler, for: Events.PaymentSent do
  def handle(%Events.PaymentSent{account_id: id, amount: amount}) do
    # => Extract event data
    # Decrease account balance
    AccountService.decrease_balance(id, amount)
    # => Returns {:ok, updated_account} or {:error, reason}
  end
end

defimpl EventHandler, for: Events.AccountClosed do
  def handle(%Events.AccountClosed{account_id: id, reason: reason}) do
    # => Extract event data
    # Mark account as closed
    AccountService.close_account(id, reason)
    # => Returns {:ok, closed_account}
  end
end
# => Returns implementation modules

# Generic event processor
def process_event(event) do
  # => Accepts any event type
  EventHandler.handle(event)
  # => Protocol dispatches to correct implementation
  # => No type checking or case statements needed
end
# => Returns function

# Process different event types with same function
payment_received = %Events.PaymentReceived{
  account_id: "acc_123",
  amount: Money.new(1000, "USD"),
  timestamp: DateTime.utc_now()
}
process_event(payment_received)       # => Calls PaymentReceived handler

payment_sent = %Events.PaymentSent{
  account_id: "acc_123",
  amount: Money.new(300, "USD"),
  timestamp: DateTime.utc_now()
}
process_event(payment_sent)           # => Calls PaymentSent handler

account_closed = %Events.AccountClosed{
  account_id: "acc_123",
  timestamp: DateTime.utc_now(),
  reason: "User request"
}
process_event(account_closed)         # => Calls AccountClosed handler
```

Protocols enable extensible event handling without modifying core processor.

## When to Use Each Approach

### Use Maps When

- **Prototyping** - Quick experimentation without structure
- **External data** - JSON responses, dynamic payloads
- **Configuration** - App settings, feature flags
- **Temporary data** - Intermediate transformations

### Use Structs When

- **Domain models** - Business entities (User, Order, Payment)
- **Compile-time safety** - Required fields, type checking
- **Pattern matching** - Type-based function dispatch
- **API contracts** - Request/response schemas
- **Data validation** - Enforcing business rules

### Use Protocols When

- **Polymorphism** - Same operation across different types
- **Extensibility** - Adding behavior to existing types
- **Decoupling** - Separating interface from implementation
- **Type-specific behavior** - Different implementations per type
- **Library design** - Public interfaces for user types

## Key Takeaways

**Maps provide flexibility**:

- Built-in key-value data structure
- No compile-time guarantees
- Use for dynamic or external data

**Structs provide safety**:

- Compile-time key validation
- Required keys enforcement
- Default values support
- Pattern matching with type checking

**Protocols provide polymorphism**:

- Define behavior across types
- Implement per type independently
- Built-in protocols (String.Chars, Enumerable)
- Custom protocols for domain behavior

**Production progression**: Start with maps for prototyping → Add structs for domain models → Use protocols for polymorphic behavior. Each layer adds structure and guarantees appropriate for production systems.

**Financial modeling pattern**: Structs (Money, Account) + Custom protocols (Arithmetic, Serializable) = Type-safe domain with polymorphic operations.
