---
title: "Type Specifications"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000015
description: "Gradual typing with @spec and @type annotations, Dialyzer integration, and type-driven development in Elixir"
tags: ["elixir", "types", "dialyzer", "typespec", "static-analysis", "gradual-typing"]
prev: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/structs-protocols"
next: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/phoenix-framework"
---

**Need static type checking in dynamic Elixir?** This guide teaches type specifications with @spec and @type annotations, Dialyzer integration for compile-time analysis, and gradual typing strategies that start with public APIs and grow incrementally.

## Why Type Specifications Matter

Elixir's dynamic nature provides flexibility, but production systems benefit from type guarantees. Type specifications enable:

- **Compile-time error detection** - Dialyzer catches type mismatches before runtime
- **Documentation as code** - Type specs serve as executable documentation
- **Gradual typing adoption** - Add types incrementally without breaking changes
- **Refactoring confidence** - Type specs catch breaking changes during refactors
- **Editor intelligence** - IDEs use specs for autocompletion and hints
- **Contract verification** - Ensure module boundaries match expectations

**Type specifications prevent production bugs** by catching errors during development rather than in production.

## Financial Domain Examples

Examples use Shariah-compliant financial operations:

- **Zakat calculation** - Processing donation percentages with type safety
- **Donation tracking** - Managing typed contribution records
- **Transaction validation** - Type-checked financial state changes

These domains demonstrate type specifications with real business logic.

## Basic Type Specifications

### Pattern 1: Function Specs with @spec

@spec annotations document function parameter and return types.

**Type Primitive**: `@spec` annotation for function signatures.

```elixir
defmodule Finance.Zakat do
  @spec calculate(pos_integer()) :: float()
  # => @spec declares function type signature
  # => calculate takes pos_integer (positive integer)
  # => Returns float (floating-point number)
  # => Dialyzer verifies callers pass correct types

  def calculate(amount) when amount > 0 do
    # => amount: pos_integer() (>0 guaranteed by spec and guard)

    amount * 0.025
    # => Multiplies by 2.5% zakat rate
    # => Returns float (e.g., 1000 => 25.0)
  end
end
```

**Type Safety**: @spec enables Dialyzer to verify callers pass positive integers and handle float returns.

```elixir
# Using the typed function
result = Finance.Zakat.calculate(1000)
# => Dialyzer verifies: 1000 is pos_integer()
# => result: float() (25.0)

Finance.Zakat.calculate(-500)
# => Dialyzer ERROR: -500 not pos_integer()
# => Caught at compile-time, not runtime!
```

**Type Benefit**: Dialyzer catches negative amounts before deployment.

### Pattern 2: Custom Types with @type

@type creates reusable type aliases for domain concepts.

**Type Primitive**: `@type` for custom type definitions.

```elixir
defmodule Finance.Types do
  @type amount :: pos_integer()
  # => amount is alias for pos_integer()
  # => Used throughout Finance modules

  @type zakat_rate :: float()
  # => zakat_rate is float between 0.0 and 1.0
  # => Semantic naming improves readability

  @type currency :: :usd | :eur | :idr
  # => currency is one of three atoms
  # => Union type with literal values

  @type donation :: %{
    amount: amount(),
    currency: currency(),
    date: Date.t()
  }
  # => donation is map with specific keys
  # => amount: positive integer
  # => currency: one of :usd, :eur, :idr
  # => date: Date struct
end
```

**Type Reuse**: Custom types create shared vocabulary across modules.

```elixir
defmodule Finance.Donation do
  alias Finance.Types
  # => Imports type aliases

  @spec create(Types.amount(), Types.currency()) :: Types.donation()
  # => Uses custom types from Types module
  # => Clearer than raw pos_integer() and atom()

  def create(amount, currency) do
    # => amount: Types.amount() (pos_integer)
    # => currency: Types.currency() (:usd | :eur | :idr)

    %{
      amount: amount,
      currency: currency,
      date: Date.utc_today()
    }
    # => Returns Types.donation() map
    # => Dialyzer verifies structure matches @type
  end
end
```

**Domain Modeling**: Custom types encode business rules in the type system.

### Pattern 3: Struct Types with @typedoc

@typedoc adds documentation to custom types.

**Type Primitive**: `@typedoc` for type documentation.

```elixir
defmodule Finance.Transaction do
  @typedoc """
  Financial transaction with amount, type, and timestamp.

  ## Fields
  - amount: Positive integer (smallest currency unit, e.g., cents)
  - type: :zakat, :sadaqah, or :waqf donation types
  - timestamp: UTC DateTime of transaction
  """
  # => @typedoc provides human-readable documentation
  # => Shows in ExDoc and editor tooltips
  # => Explains business logic behind type

  @type t :: %__MODULE__{
    amount: pos_integer(),
    type: :zakat | :sadaqah | :waqf,
    timestamp: DateTime.t()
  }
  # => t is conventional name for module's main type
  # => %__MODULE__{} creates struct type
  # => Fields match struct definition below

  defstruct [:amount, :type, :timestamp]
  # => Defines struct with three fields
  # => Must match @type t definition
end
```

**Documentation Integration**: @typedoc appears in generated documentation.

```elixir
# Using documented type
alias Finance.Transaction

@spec process(Transaction.t()) :: :ok | {:error, String.t()}
# => Transaction.t() is the struct type
# => Dialyzer knows it's a struct with amount, type, timestamp
# => Editor shows @typedoc on hover

def process(%Transaction{} = txn) do
  # => txn: Transaction.t() struct
  # => Pattern matches struct shape

  if txn.amount > 0 do
    # => Accesses amount field
    # => Dialyzer verifies field exists
    :ok
  else
    {:error, "Invalid amount"}
  end
end
```

**Type Documentation**: @typedoc makes complex types understandable to developers.

## Dialyzer Integration

### Pattern 4: Dialyzer Configuration

Configure Dialyzer for your project with mix configuration.

**Tool Integration**: Dialyxir library for Mix integration.

```elixir
# File: mix.exs
defp deps do
  [
    {:dialyxir, "~> 1.4", only: [:dev, :test], runtime: false}
    # => Adds Dialyzer integration to Mix
    # => only: [:dev, :test] - not in production
    # => runtime: false - compile-time only
  ]
end

def project do
  [
    app: :finance,
    # => Application name

    dialyzer: [
      plt_add_apps: [:mix, :ex_unit],
      # => Includes Mix and ExUnit in PLT (Persistent Lookup Table)
      # => PLT caches type information for faster checks

      flags: [:error_handling, :underspecs],
      # => :error_handling - warns about unhandled error cases
      # => :underspecs - warns about underspecified functions

      list_unused_filters: true,
      # => Shows filters that don't match any warnings
      # => Helps keep ignore list clean

      ignore_warnings: ".dialyzer_ignore.exs"
      # => File listing acceptable warnings
      # => Prevents CI failures on known issues
    ]
  ]
end
```

**Dialyzer Setup**: Configuration enables gradual type checking integration.

```bash
# Install dependencies
mix deps.get
# => Downloads dialyxir package

# Build PLT (first time only, takes 5-10 minutes)
mix dialyzer --plt
# => Analyzes Erlang/Elixir standard library
# => Creates persistent type cache
# => Reused for future runs

# Run type checking
mix dialyzer
# => Analyzes project code against PLT
# => Reports type inconsistencies
# => Returns exit code 1 on errors (fails CI)
```

**CI Integration**: Run Dialyzer in continuous integration pipelines.

### Pattern 5: Gradual Typing Strategy

Start with public APIs and expand type coverage incrementally.

**Adoption Strategy**: Type specifications from public APIs inward.

```elixir
defmodule Finance.Calculator do
  # PHASE 1: Type public API only
  @spec calculate_zakat(pos_integer()) :: float()
  # => Public function gets @spec annotation
  # => External callers benefit from type checking

  def calculate_zakat(amount) do
    # => Public API with type spec

    do_calculate(amount, get_rate())
    # => Calls private functions (no specs yet)
  end

  # PHASE 2: Add types to private functions
  @spec do_calculate(pos_integer(), float()) :: float()
  # => Private helper gets spec after public API stable

  defp do_calculate(amount, rate) do
    # => amount: pos_integer()
    # => rate: float()

    amount * rate
    # => Returns float
  end

  @spec get_rate() :: float()
  # => Another private function typed

  defp get_rate do
    0.025
    # => Returns zakat rate as float
  end
end
```

**Phase 1**: Type external API for immediate caller benefits.
**Phase 2**: Add internal types after API stabilizes.

```elixir
# Gradual typing progression
# Week 1: Public functions only (20% coverage)
@spec public_function(integer()) :: String.t()

# Week 2: Critical paths (40% coverage)
@spec handle_payment(map()) :: {:ok, String.t()} | {:error, atom()}

# Week 3: Data structures (60% coverage)
@type transaction :: %{amount: integer(), type: atom()}

# Week 4: Private functions (80% coverage)
@spec validate_amount(integer()) :: boolean()

# Ongoing: Maintain 80%+ coverage as codebase grows
```

**Incremental Adoption**: Gradual approach prevents overwhelming refactors.

## Common Type Patterns

### Pattern 6: Union Types for Results

Use union types to model success/failure cases.

**Type Pattern**: Tagged tuples with union types.

```elixir
defmodule Finance.Validator do
  @type validation_error :: :invalid_amount | :invalid_currency | :future_date
  # => Specific error atoms instead of generic :error
  # => Self-documenting error cases

  @type result :: {:ok, map()} | {:error, validation_error()}
  # => Union of success and error cases
  # => Dialyzer ensures callers handle both

  @spec validate_donation(map()) :: result()
  # => Returns result() union type
  # => Forces explicit error handling

  def validate_donation(donation) do
    # => donation: map() (untyped input)

    with :ok <- validate_amount(donation.amount),
         # => Checks amount validity
         # => Returns :ok or {:error, :invalid_amount}

         :ok <- validate_currency(donation.currency),
         # => Checks currency validity

         :ok <- validate_date(donation.date) do
         # => Checks date not in future

      {:ok, donation}
      # => All validations passed
      # => Returns {:ok, map()} variant
    else
      {:error, reason} -> {:error, reason}
      # => reason: validation_error()
      # => Returns {:error, validation_error()} variant
    end
  end

  @spec validate_amount(any()) :: :ok | {:error, :invalid_amount}
  # => Binary result (no error details needed)

  defp validate_amount(amount) when is_integer(amount) and amount > 0 do
    :ok
    # => Amount valid
  end

  defp validate_amount(_), do: {:error, :invalid_amount}
  # => Any other value fails
end
```

**Result Types**: Union types encode all possible outcomes.

```elixir
# Using result types
case Finance.Validator.validate_donation(input) do
  {:ok, donation} ->
    # => donation: map() (validated)
    # => Dialyzer knows this branch has map()
    process_donation(donation)

  {:error, :invalid_amount} ->
    # => Specific error case
    Logger.error("Invalid donation amount")

  {:error, :invalid_currency} ->
    # => Another specific case
    Logger.error("Unsupported currency")

  {:error, :future_date} ->
    # => Third case
    Logger.error("Future dates not allowed")
end
# => Dialyzer warns if any variant unhandled
```

**Exhaustive Handling**: Dialyzer ensures all cases covered.

### Pattern 7: Opaque Types for Encapsulation

Use @opaque to hide internal type structure.

**Type Encapsulation**: @opaque prevents external module inspection.

```elixir
defmodule Finance.Account do
  @opaque t :: %__MODULE__{
    id: String.t(),
    balance: integer(),
    private_key: binary()
  }
  # => @opaque instead of @type
  # => External modules cannot pattern match internals
  # => Hides private_key implementation

  defstruct [:id, :balance, :private_key]
  # => Internal struct definition

  @spec new(String.t()) :: t()
  # => Returns opaque Account.t()

  def new(id) do
    # => id: String.t()

    %__MODULE__{
      id: id,
      balance: 0,
      private_key: :crypto.strong_rand_bytes(32)
    }
    # => Creates account with generated key
    # => Returns Account.t() (opaque to callers)
  end

  @spec get_balance(t()) :: integer()
  # => Takes opaque t(), returns balance
  # => External code cannot access balance directly

  def get_balance(%__MODULE__{balance: balance}) do
    # => Pattern matches internally (allowed in same module)
    balance
  end
end
```

**Opaque Benefit**: External modules cannot bypass encapsulation.

```elixir
# External module usage
account = Finance.Account.new("acc-123")
# => account: Finance.Account.t() (opaque type)

balance = Finance.Account.get_balance(account)
# => Uses public API: get_balance/1
# => balance: integer()

# FORBIDDEN (Dialyzer error):
%{balance: b} = account
# => ERROR: Cannot pattern match opaque type
# => Must use Account.get_balance/1

account.private_key
# => ERROR: Cannot access fields of opaque type
# => Encapsulation enforced by type system
```

**Encapsulation Enforcement**: @opaque prevents direct field access.

## Full Financial Example

### Zakat Calculator with Full Type Specifications

Complete example showing all type specification patterns.

```elixir
defmodule Finance.ZakatCalculator do
  @moduledoc """
  Calculates Shariah-compliant zakat (2.5% charitable donation).
  """

  # Custom types for domain modeling
  @typedoc "Positive amount in smallest currency unit (cents, fils)"
  @type amount :: pos_integer()
  # => Reusable amount type

  @typedoc "Zakat rate between 0.0 and 1.0"
  @type rate :: float()
  # => Rate type with semantic meaning

  @typedoc "Calculation result with amount and metadata"
  @type calculation :: %{
    original: amount(),
    zakat: amount(),
    remainder: amount(),
    rate_applied: rate()
  }
  # => Complex result type

  @type error_reason :: :negative_amount | :zero_amount | :invalid_rate
  # => Specific error cases

  @type result :: {:ok, calculation()} | {:error, error_reason()}
  # => Tagged union of success/error

  # Public API with full type specs
  @spec calculate(amount()) :: result()
  # => Main public function
  # => Takes amount, returns result union

  def calculate(amount) when amount > 0 do
    # => amount: pos_integer() (>0)

    rate = default_rate()
    # => Gets 0.025 rate
    # => rate: rate() = float()

    zakat = compute_zakat(amount, rate)
    # => zakat: amount() = pos_integer()

    {:ok, %{
      original: amount,
      zakat: zakat,
      remainder: amount - zakat,
      rate_applied: rate
    }}
    # => Returns {:ok, calculation()} variant
    # => All fields typed in calculation()
  end

  def calculate(0), do: {:error, :zero_amount}
  # => Returns {:error, :zero_amount} variant

  def calculate(_), do: {:error, :negative_amount}
  # => Returns {:error, :negative_amount} variant

  @spec calculate_with_rate(amount(), rate()) :: result()
  # => Alternative function with custom rate

  def calculate_with_rate(amount, rate)
      when amount > 0 and rate > 0.0 and rate <= 1.0 do
    # => amount: pos_integer()
    # => rate: float() between 0.0 and 1.0
    # => Guards enforce constraints

    zakat = compute_zakat(amount, rate)
    # => Computed zakat amount

    {:ok, %{
      original: amount,
      zakat: zakat,
      remainder: amount - zakat,
      rate_applied: rate
    }}
    # => Returns calculation() map
  end

  def calculate_with_rate(amount, _rate) when amount <= 0 do
    {:error, :negative_amount}
    # => Amount validation
  end

  def calculate_with_rate(_amount, _rate), do: {:error, :invalid_rate}
  # => Rate out of valid range

  # Private functions with specs
  @spec default_rate() :: rate()
  # => Returns standard zakat rate

  defp default_rate, do: 0.025
  # => 2.5% zakat rate

  @spec compute_zakat(amount(), rate()) :: amount()
  # => Computes zakat from amount and rate

  defp compute_zakat(amount, rate) do
    # => amount: pos_integer()
    # => rate: float()

    (amount * rate)
    |> Float.round()
    |> trunc()
    # => Converts to integer
    # => Returns pos_integer()
  end
end
```

**Complete Typing**: All functions, types, and results fully specified.

```elixir
# Using fully-typed calculator
alias Finance.ZakatCalculator

# Success case
{:ok, result} = ZakatCalculator.calculate(10000)
# => result.original: 10000 (pos_integer)
# => result.zakat: 250 (pos_integer, 2.5%)
# => result.remainder: 9750 (pos_integer)
# => result.rate_applied: 0.025 (float)

# Custom rate
{:ok, result} = ZakatCalculator.calculate_with_rate(10000, 0.05)
# => result.zakat: 500 (5% custom rate)

# Error cases
{:error, :zero_amount} = ZakatCalculator.calculate(0)
{:error, :negative_amount} = ZakatCalculator.calculate(-100)
{:error, :invalid_rate} = ZakatCalculator.calculate_with_rate(1000, 2.0)
# => Dialyzer verifies all error cases handled
```

**Dialyzer Verification**: Full type checking catches errors at compile time.

## Type Specification Checklist

When adding type specifications:

- [ ] **Start with public API** - Type external functions first
- [ ] **Use @type for domain concepts** - Create semantic type aliases
- [ ] **Document with @typedoc** - Explain complex types
- [ ] **Model errors with unions** - Specific error atoms instead of generic :error
- [ ] **Use @opaque for encapsulation** - Hide internal structure
- [ ] **Configure Dialyzer in mix.exs** - Enable CI integration
- [ ] **Run mix dialyzer regularly** - Catch type errors early
- [ ] **Maintain 80%+ coverage** - Focus on critical paths
- [ ] **Add specs during code review** - Gradual adoption
- [ ] **Update specs when refactoring** - Keep types synchronized

## Key Takeaways

Type specifications in Elixir provide:

1. **Gradual typing** - Add types incrementally without breaking changes
2. **Compile-time verification** - Dialyzer catches errors before deployment
3. **Documentation as code** - Types serve as executable contracts
4. **Refactoring confidence** - Type specs catch breaking changes
5. **Domain modeling** - Custom types encode business rules

**Start with public APIs, use Dialyzer, and expand coverage gradually** for production-ready type safety.
