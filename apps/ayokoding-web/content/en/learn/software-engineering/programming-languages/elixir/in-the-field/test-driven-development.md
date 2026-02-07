---
title: "Test Driven Development"
date: 2026-02-05T17:50:00+07:00
draft: false
weight: 1000023
description: "Master test-driven development in Elixir with ExUnit, doctests, coverage analysis, and behavior-based mocking for reliable systems"
tags: ["elixir", "testing", "tdd", "exunit", "doctests", "mocking", "coverage"]
prev: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/testing-strategies"
next: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/code-quality-tools"
---

Test-driven development (TDD) in Elixir leverages the robust ExUnit framework and the language's functional nature to create reliable, well-tested systems. Elixir's emphasis on documentation through doctests and its built-in testing support make TDD a natural fit for building production-ready applications.

## TDD Cycle: Red-Green-Refactor

The TDD cycle follows three distinct phases that ensure code correctness and maintainability.

### Red Phase: Write Failing Test

Start by writing a test that defines desired behavior. The test should fail initially because the implementation does not exist.

```elixir
defmodule DonationTest do
  use ExUnit.Case                          # => Brings in ExUnit test framework
  alias Donation.Processor                  # => Module under test

  describe "process_donation/1" do          # => Groups related tests
    test "processes valid donation amount" do
      donation = %{                         # => Test input data
        amount: 100_00,                     # => 100.00 in cents
        currency: "USD",                    # => Currency code
        donor_id: "donor_123"               # => Donor identifier
      }

      result = Processor.process_donation(donation)
                                            # => Call function being tested
                                            # => Currently fails (not implemented)

      assert {:ok, processed} = result      # => Expect success tuple
      assert processed.status == :completed # => Check processing status
      assert processed.amount == 100_00     # => Verify amount preserved
    end
  end
end
```

Running this test produces a failure because `Donation.Processor` does not exist yet.

### Green Phase: Write Minimal Implementation

Implement just enough code to make the test pass without over-engineering.

```elixir
defmodule Donation.Processor do
  @moduledoc """
  Processes donation transactions.
  """

  def process_donation(%{amount: amount} = donation) when amount > 0 do
                                            # => Pattern match donation map
                                            # => Guard ensures positive amount
    processed = Map.merge(donation, %{     # => Merge original donation
      status: :completed,                   # => Add completion status
      processed_at: DateTime.utc_now()      # => Add timestamp
    })

    {:ok, processed}                        # => Return success tuple
  end                                       # => Test now passes
end
```

This minimal implementation satisfies the test. The test transitions from red to green.

### Refactor Phase: Improve Design

Enhance the implementation while keeping tests green. Add validation, error handling, and documentation.

```elixir
defmodule Donation.Processor do
  @moduledoc """
  Processes donation transactions with validation and error handling.
  """

  @type donation :: %{
          amount: pos_integer(),            # => Amount in cents
          currency: String.t(),             # => ISO currency code
          donor_id: String.t()              # => Donor identifier
        }

  @type processed_donation :: %{
          amount: pos_integer(),
          currency: String.t(),
          donor_id: String.t(),
          status: atom(),                   # => Processing status
          processed_at: DateTime.t()        # => Processing timestamp
        }

  @spec process_donation(donation()) ::
          {:ok, processed_donation()} | {:error, String.t()}
                                            # => Function type specification

  def process_donation(donation) do
    with :ok <- validate_donation(donation),
                                            # => Validate before processing
         {:ok, processed} <- do_process(donation) do
                                            # => Perform actual processing
      {:ok, processed}                      # => Return successful result
    end
  end

  defp validate_donation(%{amount: amount}) when amount > 0, do: :ok
                                            # => Valid positive amount
  defp validate_donation(%{amount: amount}) when amount <= 0 do
    {:error, "Amount must be positive"}     # => Invalid amount error
  end
  defp validate_donation(_), do: {:error, "Missing required fields"}
                                            # => Missing fields error

  defp do_process(donation) do
    processed = Map.merge(donation, %{
      status: :completed,
      processed_at: DateTime.utc_now()
    })

    {:ok, processed}
  end
end
```

Tests remain green after refactoring. Add more tests for edge cases.

```elixir
describe "process_donation/1" do
  test "rejects negative amounts" do
    donation = %{amount: -100_00, currency: "USD", donor_id: "donor_123"}
                                            # => Negative amount input

    assert {:error, "Amount must be positive"} = Processor.process_donation(donation)
                                            # => Expect validation error
  end

  test "rejects zero amounts" do
    donation = %{amount: 0, currency: "USD", donor_id: "donor_123"}
                                            # => Zero amount input

    assert {:error, "Amount must be positive"} = Processor.process_donation(donation)
                                            # => Expect validation error
  end

  test "rejects missing fields" do
    donation = %{amount: 100_00}            # => Incomplete donation data

    assert {:error, "Missing required fields"} = Processor.process_donation(donation)
                                            # => Expect missing fields error
  end
end
```

## Doctests: Documentation as Tests

Doctests combine documentation with executable tests. They validate code examples in module documentation, ensuring examples stay current.

### Basic Doctests

Include testable examples in function documentation using `iex>` prompts.

```elixir
defmodule Donation.Calculator do
  @moduledoc """
  Calculates donation-related values.
  """

  @doc """
  Calculates processing fee for a donation amount.

  Fee structure:
  - 2.9% + $0.30 for amounts under $100
  - 2.5% + $0.30 for amounts $100 and above

  ## Examples

      iex> Donation.Calculator.calculate_fee(50_00)
                                            # => Amount: $50.00
      175                                   # => Expected result: $1.75
                                            # => Calculation: (50.00 * 0.029) + 0.30 = 1.75

      iex> Donation.Calculator.calculate_fee(100_00)
                                            # => Amount: $100.00
      280                                   # => Expected result: $2.80
                                            # => Calculation: (100.00 * 0.025) + 0.30 = 2.80

      iex> Donation.Calculator.calculate_fee(200_00)
                                            # => Amount: $200.00
      530                                   # => Expected result: $5.30
                                            # => Calculation: (200.00 * 0.025) + 0.30 = 5.30
  """
  def calculate_fee(amount) when amount < 100_00 do
    round(amount * 0.029 + 30)              # => 2.9% + $0.30
  end                                       # => Round to nearest cent

  def calculate_fee(amount) do
    round(amount * 0.025 + 30)              # => 2.5% + $0.30
  end                                       # => Round to nearest cent
end
```

Enable doctests in test file:

```elixir
defmodule Donation.CalculatorTest do
  use ExUnit.Case                          # => ExUnit test framework
  doctest Donation.Calculator              # => Run all doctests from module
                                           # => Examples become test cases

  # Additional test cases can follow
end
```

Doctests execute during test runs and fail if examples produce unexpected results.

### Doctest Directives

Control doctest behavior with special directives for edge cases.

```elixir
defmodule Donation.Receipt do
  @doc """
  Generates receipt for donation.

  ## Examples

      iex> donation = %{amount: 100_00, donor_id: "donor_123"}
                                            # => Create donation data
      iex> {:ok, receipt} = Donation.Receipt.generate(donation)
                                            # => Generate receipt
      iex> receipt.receipt_id
                                            # => Receipt ID is random
      "receipt_..." # => Pattern match prefix only

      iex> Donation.Receipt.generate(%{amount: -100})
                                            # => Invalid amount
      {:error, _reason}                     # => Error tuple with any reason
  """
  def generate(%{amount: amount, donor_id: donor_id}) when amount > 0 do
    receipt = %{                            # => Build receipt structure
      receipt_id: generate_receipt_id(),    # => Generate unique ID
      amount: amount,
      donor_id: donor_id,
      generated_at: DateTime.utc_now()
    }

    {:ok, receipt}                          # => Return receipt
  end

  def generate(_), do: {:error, "Invalid donation"}
                                            # => Handle invalid input

  defp generate_receipt_id do
    "receipt_" <> Base.encode16(:crypto.strong_rand_bytes(8))
                                            # => Generate random ID
  end
end
```

## Test Organization with Describe Blocks

Organize tests into logical groups using `describe` blocks for better readability and isolation.

### Grouping Related Tests

```elixir
defmodule Donation.ValidatorTest do
  use ExUnit.Case                          # => ExUnit framework
  alias Donation.Validator                  # => Module under test

  describe "validate_amount/1" do           # => Group amount validation tests
    test "accepts positive amounts" do
      assert :ok = Validator.validate_amount(100_00)
                                            # => Valid amount passes
    end

    test "rejects negative amounts" do
      assert {:error, _} = Validator.validate_amount(-100)
                                            # => Negative amount fails
    end

    test "rejects zero amounts" do
      assert {:error, _} = Validator.validate_amount(0)
                                            # => Zero amount fails
    end
  end

  describe "validate_currency/1" do         # => Group currency validation tests
    test "accepts valid ISO currency codes" do
      assert :ok = Validator.validate_currency("USD")
                                            # => USD is valid
      assert :ok = Validator.validate_currency("EUR")
                                            # => EUR is valid
      assert :ok = Validator.validate_currency("GBP")
                                            # => GBP is valid
    end

    test "rejects invalid currency codes" do
      assert {:error, _} = Validator.validate_currency("XYZ")
                                            # => XYZ is invalid
      assert {:error, _} = Validator.validate_currency("US")
                                            # => Too short
      assert {:error, _} = Validator.validate_currency("")
                                            # => Empty string
    end

    test "rejects non-uppercase codes" do
      assert {:error, _} = Validator.validate_currency("usd")
                                            # => Lowercase rejected
    end
  end

  describe "validate_donor_id/1" do         # => Group donor ID validation tests
    test "accepts valid donor IDs" do
      assert :ok = Validator.validate_donor_id("donor_123")
                                            # => Valid format passes
    end

    test "rejects empty donor IDs" do
      assert {:error, _} = Validator.validate_donor_id("")
                                            # => Empty string fails
    end
  end
end
```

### Setup and Cleanup with Context

Use `setup` callbacks to prepare test data and clean up after tests.

```elixir
defmodule Donation.ProcessorIntegrationTest do
  use ExUnit.Case                          # => ExUnit framework
  alias Donation.{Processor, Store}         # => Modules under test

  setup do                                  # => Runs before each test
    {:ok, store} = Store.start_link()       # => Start in-memory store
                                            # => Store process for this test

    donation = %{                           # => Prepare test donation
      amount: 100_00,
      currency: "USD",
      donor_id: "donor_#{:rand.uniform(1000)}"
    }

    on_exit(fn ->                           # => Cleanup callback
      if Process.alive?(store) do           # => Check if process running
        Store.stop(store)                   # => Stop store process
      end
    end)

    %{store: store, donation: donation}     # => Return context map
                                            # => Available in all tests
  end

  test "stores processed donation", %{store: store, donation: donation} do
                                            # => Receive context map
    {:ok, processed} = Processor.process_donation(donation)
                                            # => Process donation
    :ok = Store.save(store, processed)      # => Store result

    saved = Store.get(store, processed.donor_id)
                                            # => Retrieve from store
    assert saved.amount == donation.amount  # => Verify amount preserved
  end

  test "handles concurrent donations", %{store: store} do
                                            # => Test concurrent processing
    tasks = for i <- 1..10 do               # => Create 10 concurrent tasks
      Task.async(fn ->                      # => Each task processes donation
        donation = %{
          amount: 100_00 * i,               # => Different amounts
          currency: "USD",
          donor_id: "donor_#{i}"
        }

        Processor.process_donation(donation)
      end)
    end

    results = Task.await_many(tasks)        # => Wait for all tasks
                                            # => Collect results
    assert length(results) == 10            # => All tasks completed
    assert Enum.all?(results, fn {status, _} -> status == :ok end)
                                            # => All succeeded
  end
end
```

## Test Coverage with ExCoveralls

Track test coverage to identify untested code paths and improve test completeness.

### Configuration

Add ExCoveralls to `mix.exs`:

```elixir
def project do
  [
    app: :donation_system,
    version: "1.0.0",
    elixir: "~> 1.14",
    test_coverage: [tool: ExCoveralls],     # => Enable coverage tracking
    preferred_cli_env: [                    # => Set environment for coverage
      coveralls: :test,                     # => Basic coverage report
      "coveralls.detail": :test,            # => Detailed line-by-line
      "coveralls.html": :test,              # => HTML report
      "coveralls.json": :test               # => JSON for CI systems
    ]
  ]
end

def deps do
  [
    {:excoveralls, "~> 0.18", only: :test}  # => Coverage dependency
  ]                                         # => Test environment only
end
```

### Running Coverage Analysis

Generate coverage reports to identify gaps:

```bash
# Basic coverage report
mix coveralls
# => Shows overall percentage
# => Lists uncovered modules

# Detailed line coverage
mix coveralls.detail
# => Shows coverage per line
# => Highlights uncovered lines

# HTML report with visualization
mix coveralls.html
# => Generates cover/excoveralls.html
# => Interactive browser view
# => Color-coded coverage

# CI-friendly JSON format
mix coveralls.json
# => Machine-readable output
# => Integration with CI/CD
```

### Interpreting Coverage Results

```elixir
# Example coverage output:
#
# COV    FILE                                        LINES RELEVANT   MISSED
# 100.0% lib/donation/calculator.ex                    45       12        0
#  85.7% lib/donation/processor.ex                     68       28        4
#  75.0% lib/donation/validator.ex                     52       20        5
# -----------------------------------------------------------------------
#  87.5% Total                                        165       60        9
```

Focus coverage improvements on critical paths:

```elixir
defmodule Donation.ProcessorTest do
  use ExUnit.Case
  alias Donation.Processor

  # Existing tests...

  describe "error recovery" do              # => Add tests for uncovered paths
    test "handles network timeout" do       # => Previously untested path
      # Simulate timeout scenario
      assert {:error, :timeout} = Processor.process_with_timeout(donation, 0)
    end

    test "recovers from external service failure" do
      # Previously uncovered error path
      assert {:error, :service_unavailable} = Processor.process_external(donation)
    end
  end
end
```

## Behavior-Based Mocking with Mox

Mox provides compile-time verified mocks based on behaviors, ensuring type safety and preventing runtime errors.

### Defining Behaviors

Create behaviors for external dependencies:

```elixir
defmodule Donation.PaymentGateway do
  @moduledoc """
  Behavior for payment gateway implementations.
  """

  @callback charge(amount :: pos_integer(), currency :: String.t(), metadata :: map()) ::
              {:ok, map()} | {:error, String.t()}
                                            # => Charge payment callback

  @callback refund(transaction_id :: String.t(), amount :: pos_integer()) ::
              {:ok, map()} | {:error, String.t()}
                                            # => Refund callback
end
```

### Implementing Real Gateway

Create production implementation:

```elixir
defmodule Donation.StripeGateway do
  @behaviour Donation.PaymentGateway       # => Implements behavior
                                           # => Compiler enforces callbacks

  @impl true                               # => Marks callback implementation
  def charge(amount, currency, metadata) do
    # Real Stripe API call
    case Stripe.Charge.create(%{           # => External API call
           amount: amount,
           currency: currency,
           metadata: metadata
         }) do
      {:ok, charge} ->                     # => Successful charge
        {:ok, %{transaction_id: charge.id, status: :completed}}

      {:error, error} ->                   # => Failed charge
        {:error, error.message}
    end
  end

  @impl true
  def refund(transaction_id, amount) do
    # Real Stripe refund call
    case Stripe.Refund.create(%{           # => External refund API
           charge: transaction_id,
           amount: amount
         }) do
      {:ok, refund} ->                     # => Successful refund
        {:ok, %{refund_id: refund.id, status: :refunded}}

      {:error, error} ->                   # => Failed refund
        {:error, error.message}
    end
  end
end
```

### Configuring Runtime Implementation

Use application config to swap implementations:

```elixir
# config/config.exs
config :donation_system, :payment_gateway, Donation.StripeGateway
                                            # => Production gateway

# config/test.exs
config :donation_system, :payment_gateway, Donation.MockPaymentGateway
                                            # => Test mock gateway
```

### Creating Mox Mocks

Define mocks in `test/test_helper.exs`:

```elixir
# test/test_helper.exs
ExUnit.start()                              # => Start ExUnit

Mox.defmock(Donation.MockPaymentGateway,    # => Define mock module
  for: Donation.PaymentGateway              # => Implements behavior
)                                           # => Compile-time verified
```

### Using Mocks in Tests

```elixir
defmodule Donation.ProcessorWithPaymentTest do
  use ExUnit.Case, async: true              # => Async safe with Mox
  import Mox                                # => Import Mox helpers

  alias Donation.{Processor, MockPaymentGateway}

  setup :verify_on_exit!                    # => Verify expectations after test
                                            # => Ensures mocks called as expected

  describe "process_with_payment/1" do
    test "successfully charges payment" do
      donation = %{                         # => Test donation
        amount: 100_00,
        currency: "USD",
        donor_id: "donor_123"
      }

      expect(MockPaymentGateway, :charge, fn amount, currency, _metadata ->
                                            # => Set expectation
                                            # => Called exactly once
        assert amount == 100_00             # => Verify amount
        assert currency == "USD"            # => Verify currency

        {:ok, %{transaction_id: "txn_123", status: :completed}}
                                            # => Return mock response
      end)

      {:ok, result} = Processor.process_with_payment(donation)
                                            # => Process with mocked gateway
      assert result.transaction_id == "txn_123"
                                            # => Verify transaction ID
      assert result.status == :completed    # => Verify status
    end

    test "handles payment gateway failure" do
      donation = %{amount: 100_00, currency: "USD", donor_id: "donor_123"}

      expect(MockPaymentGateway, :charge, fn _amount, _currency, _metadata ->
        {:error, "Card declined"}           # => Simulate payment failure
      end)

      assert {:error, "Card declined"} = Processor.process_with_payment(donation)
                                            # => Verify error propagation
    end

    test "processes refund correctly" do
      expect(MockPaymentGateway, :refund, fn transaction_id, amount ->
        assert transaction_id == "txn_123"  # => Verify transaction ID
        assert amount == 100_00             # => Verify refund amount

        {:ok, %{refund_id: "rfnd_123", status: :refunded}}
      end)

      {:ok, result} = Processor.refund_payment("txn_123", 100_00)
      assert result.refund_id == "rfnd_123"
      assert result.status == :refunded
    end
  end
end
```

### Stub Mode for Less Critical Dependencies

Use stubs when exact expectations are not critical:

```elixir
describe "with notification service" do
  test "sends notification after successful donation" do
    donation = %{amount: 100_00, currency: "USD", donor_id: "donor_123"}

    stub(MockNotificationService, :send_email, fn _recipient, _template, _data ->
      {:ok, %{message_id: "msg_123"}}       # => Stub always returns success
    end)                                    # => No expectation verification

    {:ok, result} = Processor.process_with_notification(donation)
                                            # => Process donation
    assert result.status == :completed      # => Focus on main behavior
    # Notification is side effect, not critical to test behavior
  end
end
```

## Best Practices

### Write Tests First

Begin with tests to clarify requirements and drive implementation design. Tests document intended behavior before code exists.

### Keep Tests Fast

Fast tests encourage frequent running. Use mocks for external services, in-memory storage for persistence, and parallel test execution.

```elixir
# Enable async test execution
use ExUnit.Case, async: true                # => Run tests concurrently
                                            # => Faster test suite
                                            # => Requires test isolation
```

### Test Behavior, Not Implementation

Focus on public API and observable behavior. Avoid testing internal implementation details that may change during refactoring.

```elixir
# Good: Test behavior
test "calculates total with processing fee" do
  assert Processor.calculate_total(100_00) == 102_90
end

# Avoid: Test implementation details
# Don't test private functions or internal state structure
```

### Maintain High Coverage

Aim for 80-90% coverage on critical paths. Focus on business logic, error handling, and edge cases. Some code (like configuration) may not need tests.

### Use Descriptive Test Names

Test names should describe what behavior is being tested and under what conditions.

```elixir
test "process_donation/1 returns error when amount is negative" do
  # Clear what is tested and expected outcome
end
```

## Summary

Test-driven development in Elixir combines ExUnit's powerful testing framework with language features like doctests and pattern matching. The Red-Green-Refactor cycle ensures code correctness from the start. Doctests keep documentation current and executable. ExCoveralls identifies untested code paths. Mox provides type-safe, behavior-based mocking for external dependencies.

Following TDD practices creates robust, maintainable Elixir systems with confidence in correctness. Tests serve as living documentation and enable fearless refactoring.
