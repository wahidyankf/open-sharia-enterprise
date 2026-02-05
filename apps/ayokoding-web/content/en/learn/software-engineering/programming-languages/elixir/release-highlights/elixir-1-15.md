---
title: "Elixir 1.15"
date: 2025-02-05T00:00:00+07:00
draft: false
description: "Enhanced compiler diagnostics, Duration type, ExUnit improvements"
weight: 1000003
tags: ["elixir", "release-notes", "elixir-1.15", "compiler", "duration", "testing"]
prev: "/en/learn/software-engineering/programming-languages/elixir/release-highlights/elixir-1-16"
next: "/en/learn/software-engineering/programming-languages/elixir/release-highlights/elixir-1-14"
---

## Release Overview

Elixir 1.15 arrived in June 2023 as a feature release focused on developer experience improvements. The compiler gained sophisticated diagnostic capabilities that catch more errors before runtime. A new Duration type brought precision to time calculations. ExUnit's test output became more informative through enhanced diffing algorithms.

This release represents incremental refinement rather than paradigm shifts. The compiler diagnostics work supports the type system groundwork laid in 1.14. Each enhancement targets real-world pain points discovered in production Elixir applications.

The release requires Erlang/OTP 24 or later, with OTP 26 recommended for optimal performance. Most code written for Elixir 1.14 runs unchanged on 1.15. The few breaking changes affect edge cases documented in the changelog.

## Compiler Diagnostics

The compiler's new analysis phase catches entire categories of bugs during compilation rather than at runtime. These diagnostics integrate seamlessly into development workflows, appearing alongside existing warnings and errors.

### Unreachable Code Detection

The compiler now identifies code paths that never execute due to control flow structure. This catches logic errors that previously manifested as runtime bugs.

```elixir
def process_payment(amount, method) do
  case method do
    :cash ->
      charge_cash(amount)          # => Executes when method is :cash
      :ok                          # => Returns :ok atom
    :credit ->
      charge_credit(amount)        # => Executes when method is :credit
      :ok                          # => Returns :ok atom
    _ ->
      {:error, :invalid_method}    # => Returns error tuple for unknown methods
      IO.puts("This is unreachable") # => Compiler warning: code after return
  end                              # => Previous line returned, this never executes
end
```

The unreachable `IO.puts/1` call triggers a warning at compile time. Before 1.15, this silent bug would go unnoticed until code review or runtime testing. The diagnostic saves debugging time by surfacing the issue immediately.

### Pattern Matching Coverage

The compiler analyzes pattern matching exhaustiveness, warning when match expressions don't cover all possible inputs. This prevents runtime `MatchError` exceptions in production.

```elixir
def calculate_fee(transaction) do
  case transaction.type do       # => Pattern match on transaction type field
    :deposit -> 0                # => Covers :deposit case (returns 0)
    :withdrawal -> 2.50          # => Covers :withdrawal case (returns 2.50 decimal)
                                 # => Compiler warning: missing :transfer case
  end                            # => MatchError at runtime if type is :transfer
end
```

The compiler detects that `:transfer` transactions aren't handled. Adding the missing clause prevents runtime errors:

```elixir
def calculate_fee(transaction) do
  case transaction.type do       # => Pattern match on transaction type field
    :deposit -> 0                # => Free deposits (returns 0)
    :withdrawal -> 2.50          # => Withdrawal fee (returns 2.50)
    :transfer -> 1.00            # => Transfer fee (returns 1.00)
                                 # => All known transaction types now covered
  end                            # => No runtime MatchError possible
end
```

Financial applications benefit significantly from exhaustive pattern matching. Missing transaction types would previously cause payment processing failures.

### Variable Rebinding Warnings

The compiler warns when variable rebinding creates potential confusion or bugs. This diagnostic highlights shadowing that might obscure intended logic.

```elixir
def apply_interest(principal, rate, years) do
  amount = principal                    # => amount is 10000.0 (initial principal)
  amount = amount * (1 + rate) ** years # => amount is 11576.25 (rebind with calculation)
                                        # => Compiler warning: variable rebinding
  IO.inspect(amount, label: "Final")    # => Output: "Final: 11576.25"
  amount                                # => Returns 11576.25
end
```

While this code works correctly, the rebinding pattern suggests refactoring opportunities. Clearer variable names communicate intent:

```elixir
def apply_interest(principal, rate, years) do
  compound_amount = principal * (1 + rate) ** years  # => compound_amount is 11576.25
  compound_amount                                    # => Returns 11576.25 (clear naming)
end
```

The diagnostic doesn't prevent rebinding but encourages developers to consider whether rebinding improves or obscures code clarity.

## Duration Type

Elixir 1.15 introduces `Duration` as a first-class type for representing time spans with mixed units. This eliminates common bugs in time calculations where unit mismatches cause errors.

### Creating Durations

The `Duration` struct represents time spans with explicit unit tracking. This prevents mixing seconds and milliseconds incorrectly.

```elixir
# Create duration from components
loan_term = Duration.new!(year: 5)               # => %Duration{year: 5} (5 year loan term)

# Durations with multiple units
service_period = Duration.new!(                  # => Duration with mixed units
  year: 2,                                       # => 2 years
  month: 6,                                      # => 6 months
  day: 15                                        # => 15 days
)                                                # => %Duration{year: 2, month: 6, day: 15}

# Invalid duration raises error
invalid = Duration.new!(minute: -30)             # => ArgumentError: duration values must be positive
```

Duration values must be non-negative. Negative durations use separate functions like `Duration.subtract/2` for clarity.

### Duration Arithmetic

Duration arithmetic maintains unit precision rather than converting everything to seconds. This prevents rounding errors in financial calculations.

```elixir
loan_term = Duration.new!(year: 3)               # => %Duration{year: 3}
extension = Duration.new!(month: 6)              # => %Duration{month: 6}

total_term = Duration.add(loan_term, extension)  # => %Duration{year: 3, month: 6}
                                                 # => Preserves original units (not 42 months)

# Convert to specific units when needed
total_months = Duration.to_months(total_term)    # => 42 (3 years * 12 + 6 months)
```

The separate conversion step makes unit changes explicit in code. This prevents silent bugs where time calculations use wrong units.

### Comparing Durations

Duration comparison considers calendar complexities. A month isn't always 30 days due to varying month lengths.

```elixir
duration_a = Duration.new!(day: 30)              # => %Duration{day: 30}
duration_b = Duration.new!(month: 1)             # => %Duration{month: 1}

Duration.compare(duration_a, duration_b)         # => :eq for comparison context
                                                 # => Same duration for most purposes
```

For exact comparisons considering calendar edge cases, convert both durations to the same unit first.

### Financial Example: Payment Plans

Duration type excellence shines in payment schedule calculations where precision matters for compliance.

```elixir
defmodule LoanCalculator do
  def create_payment_plan(principal, annual_rate, term) do
    # term is Duration type (e.g., %Duration{year: 5})
    total_months = Duration.to_months(term)     # => 60 months for 5 year loan
    monthly_rate = annual_rate / 12             # => 0.005 for 6% annual rate

    payment = calculate_monthly_payment(        # => Calculate fixed payment amount
      principal,                                # => e.g., 100000.0
      monthly_rate,                             # => e.g., 0.005
      total_months                              # => e.g., 60
    )                                           # => Returns 1933.28

    %{
      duration: term,                           # => %Duration{year: 5} (original duration)
      total_payments: total_months,             # => 60 (number of payments)
      payment_amount: payment                   # => 1933.28 (monthly payment)
    }                                           # => Returns payment plan map
  end

  defp calculate_monthly_payment(p, r, n) do
    # Standard amortization formula
    p * (r * (1 + r) ** n) / ((1 + r) ** n - 1)
  end
end

# Create 5-year loan payment plan
loan_term = Duration.new!(year: 5)              # => %Duration{year: 5}
plan = LoanCalculator.create_payment_plan(      # => Create payment plan
  100_000.0,                                    # => Principal: 100,000
  0.06,                                         # => Annual rate: 6%
  loan_term                                     # => Term: 5 years
)                                               # => %{duration: %Duration{year: 5}, ...}

IO.inspect(plan)                                # => Output: payment plan details
# => %{
#      duration: %Duration{year: 5},
#      total_payments: 60,
#      payment_amount: 1933.28
#    }
```

The Duration type documents time spans in domain language (5 years) while enabling precise conversion to calculation units (60 months).

## ExUnit Improvements

ExUnit gained several enhancements that improve test debugging and output readability. These changes reduce time spent investigating test failures.

### Enhanced Diff Output

Test failure diffs now highlight specific differences in complex data structures. This pinpoints exactly what changed rather than showing entire structures.

```elixir
defmodule TransactionTest do
  use ExUnit.Case

  test "processes withdrawal transaction" do
    account = %{
      id: "ACC-001",                            # => Account ID
      balance: 10_000.0,                        # => Starting balance
      currency: "USD"                           # => Currency code
    }

    expected = %{
      id: "ACC-001",                            # => Expected ID (matches)
      balance: 9_950.0,                         # => Expected balance after withdrawal
      currency: "USD"                           # => Expected currency (matches)
    }

    result = process_withdrawal(account, 50.0) # => Process 50.0 withdrawal
                                               # => Returns updated account map

    assert result == expected                  # => Compare result to expected
                                               # => Enhanced diff shows only balance mismatch
  end
end
```

When the assertion fails, ExUnit 1.15 shows:

```
  1) test processes withdrawal transaction (TransactionTest)
     Assertion with == failed
     code:  assert result == expected
     left:  %{balance: 9_940.0, currency: "USD", id: "ACC-001"}
     right: %{balance: 9_950.0, currency: "USD", id: "ACC-001"}

     Diff:
     %{
       balance: 9_940.0,  # <- differs from 9_950.0
       currency: "USD",
       id: "ACC-001"
     }
```

The enhanced diff highlights the 10.0 balance difference rather than dumping the entire map structure. This saves investigation time in tests with complex data.

### Pattern Matching in Test Descriptions

Test descriptions can now use pattern matching to generate descriptive names from test data. This improves test suite readability.

```elixir
defmodule FeeCalculatorTest do
  use ExUnit.Case

  # Pattern matching in test name generates descriptive output
  test "calculates #{type} transaction fee", %{type: type} do
    fee = calculate_fee(type)                  # => Calculate fee for transaction type
    assert fee >= 0                            # => Fee must be non-negative
  end

  # Multiple test cases with pattern-matched names
  test "processes #{amount} payment", %{amount: amount} do
    result = process_payment(amount)           # => Process payment for amount
    assert result.status == :success           # => Payment succeeds
  end
end
```

The pattern-matched test names appear in test output, making it clear which specific test case failed without examining the test code.

### Async Test Improvements

ExUnit's async test execution became more efficient through better scheduling. Tests run faster while maintaining isolation guarantees.

```elixir
defmodule AccountServiceTest do
  use ExUnit.Case, async: true                 # => Run tests concurrently

  test "creates new account" do
    account = AccountService.create(           # => Create test account
      user_id: "USR-001",                      # => User ID
      initial_balance: 1000.0                  # => Starting balance
    )                                          # => Returns created account

    assert account.status == :active           # => New accounts are active
    assert account.balance == 1000.0           # => Balance matches initial amount
  end
end
```

Async test improvements reduce total test suite runtime without code changes. Tests that previously took 30 seconds might now complete in 20 seconds.

## Other Improvements

Beyond the headline features, Elixir 1.15 includes numerous quality-of-life improvements across the standard library.

### IEx Enhancements

The interactive shell gained better autocomplete and help documentation. Tab completion now works for module attributes and struct fields.

```elixir
iex> account = %Account{                       # => Create Account struct
...>   id: "ACC-001",                          # => Set account ID
...>   balance: 5000.0                         # => Set initial balance
...> }                                         # => Returns Account struct

iex> account.<TAB>                             # => Tab completion shows struct fields
     balance   id                              # => Available fields displayed

iex> h Account                                 # => Show Account module documentation
     Account is a struct for bank accounts     # => Module documentation
```

Improved autocomplete reduces typos and speeds up exploration of unfamiliar codebases.

### Logger Updates

Logger gained structured metadata filtering. This enables more precise control over which log messages appear based on context.

```elixir
require Logger

# Log with structured metadata
Logger.info("Payment processed",
  transaction_id: "TXN-123",                   # => Transaction identifier
  amount: 150.0,                               # => Transaction amount
  user_id: "USR-001"                           # => User identifier
)                                              # => Logs message with metadata

# Filter logs by metadata in configuration
config :logger,
  metadata: [:transaction_id, :user_id],       # => Include transaction and user IDs
  metadata_filter: [user_id: "USR-001"]        # => Only log for specific user
```

Structured metadata filtering reduces log noise in production while maintaining detailed logging for specific users or transactions during debugging.

### Kernel Improvements

The `Kernel` module gained several convenience functions that reduce boilerplate in common operations.

```elixir
# New is_struct/2 guard for specific struct types
defmodule PaymentValidator do
  def validate(payment) when is_struct(payment, Payment) do
    # payment is guaranteed to be a Payment struct
    validate_amount(payment.amount)            # => Validate payment amount field
    validate_method(payment.method)            # => Validate payment method field
    :ok                                        # => Return success atom
  end

  def validate(_), do: {:error, :invalid_type} # => Handle non-Payment inputs
end

# Cleaner than manual struct validation
payment = %Payment{amount: 100.0, method: :cash}
PaymentValidator.validate(payment)             # => Returns :ok
PaymentValidator.validate(%{})                 # => Returns {:error, :invalid_type}
```

The `is_struct/2` guard eliminates verbose pattern matching when validating specific struct types. This reduces boilerplate in function heads that dispatch based on struct type.

## Upgrade Guidance

Migrating from Elixir 1.14 to 1.15 typically requires minimal code changes. Most applications upgrade without modifications beyond updating the Elixir version requirement.

### Update Version Requirements

Start by updating your `mix.exs` file to require Elixir 1.15 or later:

```elixir
def project do
  [
    app: :payment_system,                      # => Application name
    version: "2.3.0",                          # => Application version
    elixir: "~> 1.15",                         # => Require Elixir 1.15 or compatible
    start_permanent: Mix.env() == :prod,       # => Permanent start in production
    deps: deps()                               # => Load dependencies
  ]                                            # => Returns project configuration
end
```

Update local dependencies with `mix deps.update --all` to ensure compatibility with Elixir 1.15.

### Address Compiler Warnings

Run `mix compile --force` to trigger full recompilation and surface any new warnings from enhanced compiler diagnostics.

```bash
mix compile --force
```

New warnings don't break compilation but highlight potential issues. Address unreachable code warnings by removing dead code paths or fixing control flow logic.

Pattern matching coverage warnings indicate missing cases. Add catch-all clauses or explicit handling for all possible inputs:

```elixir
# Before: Missing pattern match case
def process(transaction) do
  case transaction.type do
    :deposit -> handle_deposit(transaction)
    :withdrawal -> handle_withdrawal(transaction)
  end
end

# After: Added catch-all clause
def process(transaction) do
  case transaction.type do
    :deposit -> handle_deposit(transaction)
    :withdrawal -> handle_withdrawal(transaction)
    other -> {:error, {:unsupported_type, other}}
  end
end
```

### Adopt Duration Type Gradually

Existing time-based calculations continue working without Duration adoption. Introduce Duration type incrementally as you modify time-handling code.

Start with new features that benefit from explicit time units:

```elixir
# New code using Duration
def create_subscription(tier) do
  duration = case tier do
    :monthly -> Duration.new!(month: 1)        # => 1 month duration
    :quarterly -> Duration.new!(month: 3)      # => 3 month duration
    :annual -> Duration.new!(year: 1)          # => 1 year duration
  end

  %Subscription{
    duration: duration,                        # => Store Duration type
    start_date: Date.utc_today()               # => Record start date
  }
end
```

Legacy code using integer days or seconds remains functional. Refactor to Duration when touching existing time calculations during feature work.

### Test Suite Updates

Run full test suite after upgrading to verify behavior:

```bash
mix test
```

Enhanced ExUnit diff output might reveal previously hidden test issues. Improved diffs make actual vs. expected differences more visible, potentially exposing flaky tests that passed by chance.

If tests timeout more frequently, check async test concurrency limits. Enhanced async scheduling might expose race conditions in test setup/teardown:

```elixir
# Reduce async concurrency if needed
use ExUnit.Case, async: true, max_cases: 4     # => Limit concurrent test cases
```

### Breaking Changes

Elixir 1.15 has minimal breaking changes. The primary compatibility concern involves Erlang/OTP version requirements:

- **Minimum**: OTP 24
- **Recommended**: OTP 26 for full feature support

Applications running OTP 23 must upgrade Erlang before adopting Elixir 1.15.

Deprecated functions removed in 1.15 trigger compilation errors if still used. Check the changelog for the complete list of removals. Most deprecated functions have clear replacement paths documented in previous release notes.

### Post-Upgrade Verification

After successful upgrade, verify critical paths in production-like environments:

1. Run full test suite: `mix test`
2. Check compile warnings: `mix compile --warnings-as-errors`
3. Verify production builds: `MIX_ENV=prod mix release`
4. Review application logs for new warnings
5. Monitor performance metrics for unexpected changes

Plan rollback procedures before deploying upgraded applications to production. While Elixir upgrades rarely cause issues, having rollback capability reduces risk.

## Summary

Elixir 1.15 strengthens developer tooling through enhanced compiler diagnostics, introduces Duration type for precise time calculations, and improves ExUnit's test debugging capabilities. These incremental improvements reduce common bugs and development friction.

The compiler's new analysis catches unreachable code, incomplete pattern matches, and suspicious variable rebinding during compilation. This shifts error detection leftward in the development cycle, preventing runtime failures.

Duration type brings first-class support for time spans with mixed units. Financial applications benefit from explicit unit tracking that prevents conversion errors in payment and loan calculations.

ExUnit enhancements reduce debugging time through better diff output and pattern-matched test descriptions. Async test improvements speed up test suites without code changes.

Upgrading from Elixir 1.14 to 1.15 requires minimal code changes. Most applications update version requirements and address compiler warnings without logic modifications. The release continues Elixir's tradition of smooth upgrade paths and strong backward compatibility.
