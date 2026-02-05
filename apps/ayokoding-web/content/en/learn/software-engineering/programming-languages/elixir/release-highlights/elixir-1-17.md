---
title: "Elixir 1.17"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000001
description: "Elixir 1.17 release highlights - Set-theoretic types, calendar durations, OTP 27 support"
tags: ["elixir", "release-1.17", "set-theoretic-types", "duration", "otp-27"]
prev: "/en/learn/software-engineering/programming-languages/elixir/release-highlights/overview"
next: "/en/learn/software-engineering/programming-languages/elixir/release-highlights/elixir-1-16"
---

## Release Information

- **Release Date**: April 24, 2024
- **Status**: Latest stable release (recommended for new projects)
- **OTP Compatibility**: OTP 24-27
- **Key Features**: Set-theoretic types, Duration module, OTP 27 enhancements

## Set-Theoretic Types

Elixir 1.17 introduces **set-theoretic types** for enhanced compile-time warnings and gradual typing improvements.

### Type System Enhancements

**Enhanced type inference for better warnings:**

```elixir
defmodule ZakatCalculator do
  # Type inference detects potential runtime errors
  def calculate_nisab(gold_grams, _silver_grams) do
    if gold_grams > 85 do                    # => gold_grams compared to 85
                                             # => Condition evaluates to boolean
      {:ok, gold_grams * 0.025}              # => Returns tuple {:ok, float}
                                             # => 0.025 is 2.5% zakat rate
    else
      {:error, :below_nisab}                 # => Returns tuple {:error, atom}
    end
  end
end

# Type-based pattern matching improvements
result = ZakatCalculator.calculate_nisab(100, 0)  # => result is {:ok, 2.5}
                                                   # => Type: {:ok, float} | {:error, atom}
case result do
  {:ok, amount} -> amount                          # => amount is float (inferred)
                                                   # => Returns 2.5
  {:error, _} -> 0                                 # => Returns 0 (integer)
                                                   # => Type mismatch warning: float vs integer
end
```

### Gradual Typing with Type Specs

**Improved type checking for function specs:**

```elixir
defmodule TransactionValidator do
  @type transaction :: %{
    amount: Decimal.t(),                     # => amount field type: Decimal
    currency: String.t(),                    # => currency field type: String
    timestamp: DateTime.t()                  # => timestamp field type: DateTime
  }

  @spec validate(transaction) :: {:ok, transaction} | {:error, String.t()}
  # => Function signature declares input/output types
  # => Input: transaction map
  # => Output: tagged tuple with transaction or error
  def validate(%{amount: amount} = txn) when amount > 0 do
    # => Pattern match extracts amount field
    # => Guard clause validates amount > 0
    # => txn bound to full transaction map
    {:ok, txn}                               # => Returns tuple {:ok, transaction}
  end

  def validate(_txn) do
    {:error, "Invalid amount"}               # => Returns error tuple with message
                                             # => Type: {:error, String.t()}
  end
end
```

### Union Type Improvements

**Better handling of union types:**

```elixir
defmodule PaymentProcessor do
  @type payment_method :: :cash | :transfer | :card
  # => payment_method is union of three atoms
  # => Compiler tracks all possible values

  @spec process(payment_method, Decimal.t()) :: {:ok, String.t()} | {:error, atom}
  # => Input: payment_method union type and Decimal amount
  # => Output: success string or error atom
  def process(:cash, amount) do
    # => Pattern matches :cash variant
    # => amount type inferred as Decimal.t()
    {:ok, "Cash payment: #{amount}"}         # => String interpolation with amount
                                             # => Returns {:ok, String.t()}
  end

  def process(:transfer, amount) do
    # => Pattern matches :transfer variant
    {:ok, "Bank transfer: #{amount}"}        # => Returns success tuple
  end

  def process(:card, amount) do
    # => Pattern matches :card variant
    {:ok, "Card payment: #{amount}"}         # => Returns success tuple
  end

  def process(_invalid, _amount) do
    # => Catch-all for invalid payment methods
    {:error, :unsupported_method}            # => Returns error tuple
                                             # => Type: {:error, atom}
  end
end
```

## Duration Data Type

New `Duration` module for calendar-aware time calculations.

### Basic Duration Operations

**Creating and manipulating durations:**

```elixir
# Create duration for zakat payment deadline (lunar year)
duration = Duration.new!(day: 354)           # => duration is Duration struct
                                             # => 354 days (Islamic lunar year)
                                             # => Type: Duration.t()

# Add duration to date
payment_date = ~D[2024-04-15]                # => payment_date is Date struct
                                             # => Represents April 15, 2024
deadline = Date.add(payment_date, duration)  # => deadline is Date struct
                                             # => Adds 354 days to payment_date
                                             # => Result: ~D[2025-04-04]

# Duration components
grace_period = Duration.new!(                # => grace_period is Duration struct
  month: 1,                                  # => 1 month component
  day: 15                                    # => 15 days component
)                                            # => Combined: 1 month 15 days

# Negate duration (for backdating)
backdated = Duration.negate(grace_period)    # => backdated is Duration struct
                                             # => Components: -1 month, -15 days
                                             # => Used for calculating past dates
```

### Financial Period Calculations

**Calculating zakat periods with Duration:**

```elixir
defmodule ZakatPeriod do
  @lunar_year Duration.new!(day: 354)        # => Module attribute: lunar year duration
                                             # => 354 days (Islamic calendar)

  def calculate_next_payment(last_payment) do
    # => last_payment is Date struct
    # => Calculates next zakat due date
    next_date = Date.add(last_payment, @lunar_year)
    # => next_date is Date struct
    # => Adds 354 days to last_payment
    days_remaining = Date.diff(next_date, Date.utc_today())
    # => days_remaining is integer
    # => Difference between next_date and today

    %{
      next_payment: next_date,               # => next_payment field: Date.t()
      days_remaining: days_remaining,        # => days_remaining field: integer
      grace_period_end: Date.add(next_date, Duration.new!(day: 30))
      # => grace_period_end: 30 days after next_payment
      # => Type: Date.t()
    }
  end

  def is_payment_overdue?(payment_date) do
    # => payment_date is Date struct
    # => Checks if payment date has passed
    today = Date.utc_today()                 # => today is Date struct
                                             # => Current date in UTC
    Date.compare(payment_date, today) == :lt # => Compare returns :lt, :eq, or :gt
                                             # => :lt means payment_date before today
                                             # => Returns boolean (true if overdue)
  end
end
```

### Duration Arithmetic

**Complex duration calculations:**

```elixir
defmodule InvestmentPeriod do
  def calculate_maturity(start_date, term_months) do
    # => start_date: Date struct, term_months: integer
    # => Calculates investment maturity date
    term = Duration.new!(month: term_months) # => term is Duration struct
                                             # => Represents term_months months
    maturity_date = Date.add(start_date, term)
    # => maturity_date is Date struct
    # => Adds term duration to start_date

    # Calculate early withdrawal penalty period (25% of term)
    penalty_months = div(term_months, 4)     # => penalty_months is integer
                                             # => Integer division by 4 (25%)
    penalty_end = Date.add(
      start_date,
      Duration.new!(month: penalty_months)   # => Duration for penalty period
    )                                        # => penalty_end is Date struct

    %{
      maturity: maturity_date,               # => maturity field: Date.t()
      penalty_end: penalty_end,              # => penalty_end field: Date.t()
      term: term                             # => term field: Duration.t()
    }
  end
end
```

## OTP 27 Support

Elixir 1.17 adds support for Erlang/OTP 27 features.

### JSON Module

**Built-in JSON encoding/decoding:**

```elixir
defmodule TransactionLogger do
  def serialize_transaction(txn) do
    # => txn is transaction map
    # => Converts map to JSON string
    json = :json.encode(%{
      id: txn.id,                            # => Extract id field from txn
      amount: Decimal.to_float(txn.amount),  # => Convert Decimal to float
                                             # => JSON requires numeric type
      currency: txn.currency,                # => Extract currency field
      timestamp: DateTime.to_iso8601(txn.timestamp)
      # => Convert DateTime to ISO 8601 string
      # => JSON-compatible timestamp format
    })                                       # => json is binary (JSON string)
                                             # => Type: {:ok, binary} | {:error, term}

    case json do
      {:ok, encoded} -> encoded              # => encoded is binary (JSON string)
                                             # => Returns JSON string
      {:error, reason} ->                    # => reason is error term
        Logger.error("JSON encoding failed: #{inspect(reason)}")
        # => Log error with reason details
        nil                                  # => Returns nil on error
    end
  end

  def deserialize_transaction(json_string) do
    # => json_string is binary (JSON string)
    # => Converts JSON to Elixir map
    case :json.decode(json_string) do
      {:ok, decoded} ->                      # => decoded is map
        %{
          id: decoded["id"],                 # => Extract id from decoded map
          amount: Decimal.new(decoded["amount"]),
          # => Convert float to Decimal
          currency: decoded["currency"],     # => Extract currency
          timestamp: DateTime.from_iso8601!(decoded["timestamp"])
          # => Parse ISO 8601 string to DateTime
        }                                    # => Returns transaction map
      {:error, _} -> nil                     # => Returns nil on parse error
    end
  end
end
```

### Process Labels

**Enhanced process identification for audit trails:**

```elixir
defmodule AuditWorker do
  use GenServer

  def start_link(user_id) do
    # => user_id identifies the audited user
    # => Starts GenServer with process label
    GenServer.start_link(__MODULE__, user_id, name: {:via, :process_label, {:audit, user_id}})
    # => Registers process with label {:audit, user_id}
    # => Type: {:ok, pid} | {:error, term}
  end

  def init(user_id) do
    # => user_id from start_link
    # => Initialize GenServer state
    Process.set_label({:audit_worker, user_id})
    # => Sets process label for debugging
    # => Visible in :observer and crash reports
    {:ok, %{user_id: user_id, logs: []}}     # => Initial state map
                                             # => user_id and empty logs list
  end

  def log_transaction(worker_pid, transaction) do
    # => worker_pid: pid of audit worker
    # => transaction: transaction map to log
    GenServer.cast(worker_pid, {:log, transaction})
    # => Async message to worker
    # => Type: :ok
  end

  def handle_cast({:log, txn}, state) do
    # => Pattern match :log message with txn
    # => state is current GenServer state
    new_logs = [txn | state.logs]           # => Prepend txn to logs list
                                            # => new_logs is list of transactions
    {:noreply, %{state | logs: new_logs}}   # => Update state with new logs
                                            # => Type: {:noreply, state}
  end
end

# Usage with labeled processes
{:ok, worker} = AuditWorker.start_link("user_123")
# => worker is pid of started GenServer
# => Process labeled with {:audit, "user_123"}

AuditWorker.log_transaction(worker, %{
  type: :zakat_payment,                      # => Transaction type
  amount: Decimal.new("1000.00"),            # => Payment amount
  timestamp: DateTime.utc_now()              # => Current timestamp
})                                           # => Returns :ok
```

### Enhanced Telemetry

**Telemetry events for process monitoring:**

```elixir
defmodule PaymentTelemetry do
  def attach_handlers() do
    # => Attaches telemetry event handlers
    # => Monitors payment processing metrics
    :telemetry.attach_many(
      "payment-handlers",                    # => Handler group ID
      [
        [:payment, :process, :start],        # => Payment start event
        [:payment, :process, :stop],         # => Payment completion event
        [:payment, :process, :exception]     # => Payment error event
      ],
      &handle_event/4,                       # => Callback function
      nil                                    # => No metadata
    )
  end

  def handle_event([:payment, :process, :start], measurements, metadata, _config) do
    # => measurements: map with metrics (e.g., system_time)
    # => metadata: map with payment details
    Logger.info("Payment started: #{inspect(metadata)}")
    # => Log payment initiation
    # => metadata includes payment ID, amount, etc.
  end

  def handle_event([:payment, :process, :stop], measurements, metadata, _config) do
    # => measurements: includes duration
    # => metadata: includes payment result
    duration_ms = measurements.duration / 1_000_000
    # => Convert nanoseconds to milliseconds
    # => duration_ms is float
    Logger.info("Payment completed in #{duration_ms}ms: #{inspect(metadata)}")
    # => Log completion with duration
  end

  def handle_event([:payment, :process, :exception], _measurements, metadata, _config) do
    # => metadata: includes error details and stacktrace
    Logger.error("Payment failed: #{inspect(metadata)}")
    # => Log payment processing error
  end
end

# Emit telemetry events
defmodule PaymentProcessor do
  def process_payment(payment) do
    # => payment is payment map
    # => Processes payment with telemetry
    :telemetry.span(
      [:payment, :process],                  # => Event name prefix
      %{payment_id: payment.id},             # => Metadata map
      fn ->
        result = do_process_payment(payment) # => Call actual processing
                                             # => result is {:ok, _} or {:error, _}
        {result, %{result: result}}          # => Return {result, metadata} tuple
                                             # => Telemetry captures both
      end
    )
  end

  defp do_process_payment(payment) do
    # => payment is payment map
    # => Actual payment processing logic
    {:ok, %{transaction_id: UUID.uuid4()}}  # => Returns success with transaction ID
  end
end
```

## Other Improvements

### Mix Enhancements

**Improved dependency compilation:**

```bash
# Parallel dependency compilation (faster builds)
mix deps.compile --parallel

# Dependency tree visualization
mix deps.tree
```

**Better error messages for circular dependencies:**

```elixir
# Circular dependency detection now shows full cycle path
# Before: Generic "circular dependency" error
# After: "Circular dependency: A -> B -> C -> A"
```

### Compiler Improvements

**Enhanced pattern matching warnings:**

```elixir
defmodule PaymentValidator do
  # Compiler warns about unreachable clauses
  def validate_amount(amount) when amount > 0, do: :ok
  def validate_amount(amount) when amount >= 0, do: :ok
  # => Warning: This clause cannot match because a previous clause always matches
  # => Second guard (>= 0) is redundant after first guard (> 0)
end
```

**Better struct field checking:**

```elixir
defmodule Transaction do
  defstruct [:id, :amount, :currency]       # => Define struct with three fields

  def validate(%__MODULE__{ammount: _}) do  # => Typo: "ammount" instead of "amount"
    # => Compiler error: Unknown field :ammount for struct Transaction
    # => Suggests: Did you mean :amount?
    :ok
  end
end
```

## Upgrade Guidance

### Migration from Elixir 1.16

**Compatibility**: Elixir 1.17 is backward compatible with 1.16 code.

**Key Changes**:

1. **Duration API**: New Duration module available
   - Replace manual date arithmetic with Duration
   - Benefits: Calendar-aware calculations, clearer intent

2. **OTP 27**: Optional (OTP 24-26 still supported)
   - Benefits: JSON module, process labels, enhanced telemetry
   - Recommendation: Upgrade to OTP 27 for production features

3. **Type System**: Gradual improvement (no breaking changes)
   - Benefit: Better compile-time warnings
   - Action: Review warnings, add type specs where beneficial

**Update Command**:

```bash
# Update Elixir version
asdf install elixir 1.17.3
asdf global elixir 1.17.3

# Update dependencies
mix deps.update --all

# Run tests
mix test
```

### Breaking Changes

**None**: Elixir 1.17 maintains full backward compatibility.

**Deprecations**: None affecting common codebases.

## Related Topics

- [Elixir 1.16](/en/learn/software-engineering/programming-languages/elixir/release-highlights/elixir-1-16) - Previous release
- [OTP Fundamentals](/en/learn/software-engineering/programming-languages/elixir/tutorials/by-concept/otp-fundamentals) - Understanding OTP architecture

## References

**Official Resources**:

- [Elixir 1.17 Release Announcement](https://elixir-lang.org/blog/2024/04/24/elixir-v1-17-0-released/)
- [Elixir 1.17.3 Changelog](https://github.com/elixir-lang/elixir/blob/v1.17/CHANGELOG.md)
- [Duration Module Documentation](https://hexdocs.pm/elixir/1.17.3/Duration.html)
- [Set-Theoretic Types Blog Post](https://elixir-lang.org/blog/2023/06/19/elixir-v1-15-0-released/)

**Erlang/OTP 27**:

- [OTP 27 Release Notes](https://www.erlang.org/patches/OTP-27.0)
- [OTP 27 JSON Module](https://www.erlang.org/doc/apps/stdlib/json.html)

---

**Last Updated**: 2026-02-05
**Elixir Version**: 1.17.3 (latest stable)
