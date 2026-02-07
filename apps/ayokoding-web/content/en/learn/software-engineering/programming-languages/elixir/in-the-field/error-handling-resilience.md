---
title: "Error Handling Resilience"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000029
description: "Error handling patterns, resilience strategies, and fault-tolerant system design in Elixir"
tags: ["elixir", "error-handling", "resilience", "fault-tolerance", "circuit-breaker", "retry"]
prev: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/logging-observability"
next: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/performance-optimization"
---

**Building resilient Elixir systems?** This guide teaches error handling patterns and resilience strategies for production systems, covering when to use try/catch/rescue, error tuples, with pipelines, circuit breakers, and retry patterns with exponential backoff.

## Why Error Handling Matters

Production systems face inevitable failures:

- **Network failures** - External API timeouts, connection drops, DNS failures
- **Resource exhaustion** - Database connection limits, memory pressure, disk full
- **Invalid input** - Malformed data, constraint violations, business rule failures
- **Third-party errors** - Payment gateway failures, service degradation, rate limits
- **Transient failures** - Temporary network glitches, brief service unavailability

**Elixir's approach**: Design for failure. Use supervisors for process crashes, error tuples for expected failures, and resilience patterns for external dependencies.

## Financial Domain Examples

Examples use Shariah-compliant financial operations:

- **Payment processing** - Handling transaction failures with retries and idempotency
- **External API integration** - Circuit breakers for third-party services
- **Audit logging** - Ensuring error transparency for compliance
- **Donation validation** - Error pipelines for input validation

These domains demonstrate production error handling with real business requirements.

## Error Tuple Conventions

### Pattern 1: Tagged Tuples

Elixir uses `{:ok, value}` and `{:error, reason}` for expected failures.

**When to use**: Expected failures that are part of normal flow (validation, business rules, not found).

```elixir
# Payment validation using error tuples
defmodule Finance.PaymentValidator do
  # => Validates payment amount and type

  def validate_payment(%{amount: amount, type: type} = payment) do
                                                 # => payment: Map with amount and type
                                                 # => Returns: {:ok, payment} or {:error, reason}
    with :ok <- validate_amount(amount),         # => Check amount validity
                                                 # => :ok means valid
         :ok <- validate_type(type) do           # => Check type validity
                                                 # => :ok means valid
      {:ok, payment}                             # => All validations passed
                                                 # => Returns: {:ok, original payment}
    else
      {:error, reason} -> {:error, reason}       # => Validation failed
                                                 # => Propagates error reason
    end
  end

  defp validate_amount(amount) when amount > 0 and amount < 1_000_000 do
    :ok                                          # => Amount valid
                                                 # => Range: 0-1M
  end
  defp validate_amount(_amount) do
    {:error, :invalid_amount}                    # => Amount outside valid range
                                                 # => Returns: Error tuple
  end

  defp validate_type(type) when type in [:donation, :zakat, :investment] do
    :ok                                          # => Type valid
                                                 # => Allowed: donation, zakat, investment
  end
  defp validate_type(_type) do
    {:error, :invalid_payment_type}              # => Unknown payment type
                                                 # => Returns: Error tuple
  end
end
```

**Usage**:

```elixir
payment = %{amount: 1000, type: :donation}       # => Valid payment
Finance.PaymentValidator.validate_payment(payment)
                                                 # => Returns: {:ok, %{amount: 1000, type: :donation}}

invalid = %{amount: -50, type: :donation}        # => Invalid amount
Finance.PaymentValidator.validate_payment(invalid)
                                                 # => Returns: {:error, :invalid_amount}
```

**Best practice**: Use error tuples for domain errors that callers should handle explicitly.

### Pattern 2: Multiple Error Cases

Return different error reasons for specific failure modes.

```elixir
# Bank account validation with specific errors
defmodule Finance.BankAccount do
  # => Validates bank account for payment processing

  def validate_account(account_number) when byte_size(account_number) == 10 do
    case check_account_status(account_number) do
                                                 # => Check if account active
      {:ok, :active} ->
        {:ok, account_number}                    # => Account valid and active

      {:ok, :frozen} ->
        {:error, :account_frozen}                # => Account exists but frozen
                                                 # => Caller should handle differently

      {:ok, :closed} ->
        {:error, :account_closed}                # => Account permanently closed

      {:error, :not_found} ->
        {:error, :account_not_found}             # => Account doesn't exist
    end
  end
  def validate_account(_account_number) do
    {:error, :invalid_format}                    # => Wrong length
                                                 # => Must be 10 digits
  end

  defp check_account_status(account_number) do
    # => Simulated database lookup
    case account_number do
      "1234567890" -> {:ok, :active}             # => Active account
      "0987654321" -> {:ok, :frozen}             # => Frozen account
      "1111111111" -> {:ok, :closed}             # => Closed account
      _ -> {:error, :not_found}                  # => Not in database
    end
  end
end
```

**Best practice**: Provide specific error reasons so callers can handle each case appropriately.

## with for Error Pipelines

### Pattern 3: Chaining Error-Tuple Operations

`with` chains operations that return `{:ok, value}` or `{:error, reason}`.

**When to use**: Multiple validation steps where early failure should short-circuit.

```elixir
# Payment processing with validation pipeline
defmodule Finance.PaymentProcessor do
  alias Finance.{PaymentValidator, BankAccount, FraudDetector}

  def process_payment(payment_data) do
                                                 # => payment_data: Map with all payment info
    with {:ok, payment} <- PaymentValidator.validate_payment(payment_data),
                                                 # => Step 1: Validate payment structure
                                                 # => If {:error, _}, skip to else
         {:ok, account} <- BankAccount.validate_account(payment.account_number),
                                                 # => Step 2: Validate bank account
                                                 # => Uses result from step 1
         {:ok, _check} <- FraudDetector.check_transaction(payment),
                                                 # => Step 3: Fraud detection
                                                 # => All checks passed
         {:ok, receipt} <- charge_account(account, payment.amount) do
                                                 # => Step 4: Execute charge
                                                 # => Returns: Receipt on success
      audit_success(payment, receipt)            # => Log successful transaction
      {:ok, receipt}                             # => Return receipt to caller
    else
      {:error, :invalid_amount} = error ->
        audit_failure(payment_data, error)       # => Log validation failure
        {:error, :payment_validation_failed}     # => Return generic error

      {:error, :account_frozen} = error ->
        audit_failure(payment_data, error)       # => Log frozen account
        notify_customer(:account_frozen)         # => Send customer notification
        {:error, :account_unavailable}           # => Return customer-facing error

      {:error, :fraud_detected} = error ->
        audit_failure(payment_data, error)       # => Log fraud attempt
        notify_admin(:fraud_detected, payment_data)
                                                 # => Alert admin immediately
        {:error, :transaction_blocked}           # => Block transaction

      {:error, reason} = error ->
        audit_failure(payment_data, error)       # => Log unknown error
        {:error, reason}                         # => Propagate original error
    end
  end

  defp charge_account(account, amount) do
    # => Simulated payment charge
    if :rand.uniform() > 0.1 do                  # => 90% success rate
      {:ok, %{transaction_id: generate_id(), account: account, amount: amount}}
                                                 # => Returns: Receipt
    else
      {:error, :insufficient_funds}              # => 10% failure rate
    end
  end

  defp audit_success(payment, receipt) do
    # => Log successful transaction for compliance
    IO.puts("SUCCESS: Payment processed - #{receipt.transaction_id}")
  end

  defp audit_failure(payment_data, error) do
    # => Log failed transaction for compliance
    IO.puts("FAILURE: Payment failed - #{inspect(error)}")
  end

  defp notify_customer(reason) do
    # => Send customer notification (simulated)
    IO.puts("Customer notified: #{reason}")
  end

  defp notify_admin(reason, payment_data) do
    # => Alert admin of critical issue
    IO.puts("Admin alert: #{reason} - #{inspect(payment_data)}")
  end

  defp generate_id, do: :crypto.strong_rand_bytes(16) |> Base.encode64()
end
```

**Best practice**: Use `with` for validation pipelines. Handle each error case explicitly in else clause for proper logging and user feedback.

## try/catch/rescue Patterns

### Pattern 4: When to Use try/catch/rescue

**Appropriate use cases** (use sparingly):

1. Interfacing with third-party libraries that raise exceptions
2. Protecting against truly unexpected failures
3. Converting exceptions to error tuples at boundaries

**Inappropriate use cases** (avoid):

1. Control flow for expected errors (use error tuples)
2. Wrapping all code "just in case" (anti-pattern)
3. Catching and ignoring errors (hides problems)

```elixir
# Converting external library exceptions to error tuples
defmodule Finance.ExternalAPI do
  # => Wrapper for third-party payment gateway SDK

  def charge_card(card_token, amount) do
                                                 # => card_token: Tokenized card
                                                 # => amount: Charge amount
    try do
      # => External library that raises on error
      result = PaymentGatewaySDK.charge(card_token, amount)
                                                 # => May raise TimeoutError
                                                 # => May raise InvalidCardError
                                                 # => May raise NetworkError
      {:ok, result}                              # => Success: Return result
    rescue
      PaymentGatewaySDK.TimeoutError ->
        {:error, :gateway_timeout}               # => Network timeout
                                                 # => Retry eligible

      PaymentGatewaySDK.InvalidCardError ->
        {:error, :invalid_card}                  # => Invalid card details
                                                 # => NOT retry eligible

      PaymentGatewaySDK.NetworkError ->
        {:error, :network_error}                 # => Network issue
                                                 # => Retry eligible

      error ->
        # => Unexpected error - log and propagate
        require Logger
        Logger.error("Unexpected payment gateway error: #{inspect(error)}")
        {:error, :gateway_error}                 # => Generic error
    end
  end
end
```

**Best practice**: Use try/rescue at system boundaries to convert exceptions to error tuples. Never use for control flow within your domain logic.

### Pattern 5: Catch for Non-Error Throws

`catch` handles non-error exits and throws (rare in Elixir).

```elixir
# Handling early termination in external library
defmodule Finance.ReportGenerator do
  # => Generates financial reports using external library

  def generate_report(data) do
                                                 # => data: Report parameters
    try do
      # => External library uses throw for early exit
      report = LegacyReportLib.generate(data)    # => May throw {:early_return, partial_report}
                                                 # => May raise on error
      {:ok, report}                              # => Full report generated
    catch
      # => Handle throw (non-error early exit)
      {:early_return, partial} ->
        {:ok, {:partial, partial}}               # => Partial report available
                                                 # => Caller decides if acceptable
      :timeout ->
        {:error, :report_timeout}                # => Generation took too long
    rescue
      # => Handle actual errors
      error ->
        {:error, {:report_generation_failed, error}}
    end
  end
end
```

**Best practice**: Only use `catch` when interfacing with libraries that use throw for control flow. Modern Elixir code should use error tuples instead.

## Circuit Breaker Patterns

### Pattern 6: Protecting External Dependencies

Circuit breakers prevent cascading failures when external services fail.

**States**:

1. **Closed** - Normal operation, requests pass through
2. **Open** - Service failing, fast-fail without calling service
3. **Half-open** - Testing recovery, limited requests allowed

```elixir
# Circuit breaker for external payment gateway
defmodule Finance.PaymentGatewayCircuitBreaker do
  use GenServer
  # => Implements circuit breaker pattern

  @failure_threshold 5                           # => Open after 5 failures
  @recovery_timeout 60_000                       # => Try recovery after 60s
  @half_open_requests 3                          # => Test with 3 requests

  # Client API

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
                                                 # => Start GenServer
                                                 # => Registered name: module name
  end

  def call(func) do
                                                 # => func: Function to call gateway
    GenServer.call(__MODULE__, {:call, func})    # => Request through circuit breaker
                                                 # => Returns: {:ok, result} or {:error, reason}
  end

  def get_state do
    GenServer.call(__MODULE__, :get_state)       # => Get current circuit state
                                                 # => Returns: :closed | :open | :half_open
  end

  # Server Implementation

  def init(_opts) do
    {:ok, %{
      state: :closed,                            # => Initial state: closed
      failure_count: 0,                          # => No failures yet
      last_failure_time: nil,                    # => No failures
      half_open_success: 0                       # => Half-open success counter
    }}
  end

  def handle_call({:call, func}, _from, state) do
    case state.state do
      :closed ->
        # => Circuit closed: normal operation
        execute_with_error_tracking(func, state)

      :open ->
        # => Circuit open: check if recovery time elapsed
        if ready_for_half_open?(state) do
          new_state = %{state | state: :half_open, half_open_success: 0}
          execute_with_error_tracking(func, new_state)
        else
          {:reply, {:error, :circuit_open}, state}
                                                 # => Fast fail: don't call service
        end

      :half_open ->
        # => Circuit half-open: testing recovery
        execute_with_recovery_tracking(func, state)
    end
  end

  def handle_call(:get_state, _from, state) do
    {:reply, state.state, state}                 # => Return current state
  end

  # Private Functions

  defp execute_with_error_tracking(func, state) do
                                                 # => Execute and track failures
    case func.() do                              # => Call external service
      {:ok, result} ->
        # => Success: reset failure counter
        new_state = %{state | failure_count: 0}
        {:reply, {:ok, result}, new_state}

      {:error, reason} = error ->
        # => Failure: increment counter
        new_failure_count = state.failure_count + 1

        if new_failure_count >= @failure_threshold do
          # => Threshold reached: open circuit
          new_state = %{
            state |
            state: :open,
            failure_count: new_failure_count,
            last_failure_time: System.monotonic_time(:millisecond)
          }
          {:reply, error, new_state}
        else
          # => Below threshold: stay closed
          new_state = %{state | failure_count: new_failure_count}
          {:reply, error, new_state}
        end
    end
  end

  defp execute_with_recovery_tracking(func, state) do
                                                 # => Execute and track recovery
    case func.() do                              # => Call external service
      {:ok, result} ->
        # => Success in half-open state
        new_success_count = state.half_open_success + 1

        if new_success_count >= @half_open_requests do
          # => Enough successes: close circuit
          new_state = %{
            state |
            state: :closed,
            failure_count: 0,
            half_open_success: 0,
            last_failure_time: nil
          }
          {:reply, {:ok, result}, new_state}
        else
          # => Continue testing
          new_state = %{state | half_open_success: new_success_count}
          {:reply, {:ok, result}, new_state}
        end

      {:error, _reason} = error ->
        # => Failure in half-open: reopen circuit
        new_state = %{
          state |
          state: :open,
          half_open_success: 0,
          last_failure_time: System.monotonic_time(:millisecond)
        }
        {:reply, error, new_state}
    end
  end

  defp ready_for_half_open?(state) do
                                                 # => Check if recovery timeout elapsed
    if state.last_failure_time do
      elapsed = System.monotonic_time(:millisecond) - state.last_failure_time
      elapsed >= @recovery_timeout               # => True if 60s passed
    else
      false                                      # => No failure time: not ready
    end
  end
end
```

**Usage with payment gateway**:

```elixir
defmodule Finance.PaymentService do
  alias Finance.{ExternalAPI, PaymentGatewayCircuitBreaker}

  def charge_card_with_circuit_breaker(card_token, amount) do
                                                 # => Charge with protection
    PaymentGatewayCircuitBreaker.call(fn ->
      ExternalAPI.charge_card(card_token, amount)
                                                 # => Call protected by circuit breaker
    end)                                         # => Returns: {:ok, result} or {:error, reason}
  end
end

# Start circuit breaker in application supervision tree
defmodule Finance.Application do
  use Application

  def start(_type, _args) do
    children = [
      Finance.PaymentGatewayCircuitBreaker       # => Circuit breaker GenServer
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
```

**Best practice**: Use circuit breakers for all external dependencies. Monitor circuit state transitions to detect service degradation early.

## Retry Strategies with Exponential Backoff

### Pattern 7: Retry with Exponential Backoff

Transient failures often resolve with retries. Exponential backoff prevents overwhelming failing services.

```elixir
# Retry with exponential backoff
defmodule Finance.RetryStrategy do
  # => Implements retry with exponential backoff

  @max_retries 5                                 # => Maximum retry attempts
  @initial_delay 100                             # => Initial delay: 100ms
  @max_delay 30_000                              # => Maximum delay: 30s
  @jitter_factor 0.1                             # => Add 10% random jitter

  def retry(func, opts \\ []) do
                                                 # => func: Function to retry
                                                 # => opts: Configuration options
    max_retries = Keyword.get(opts, :max_retries, @max_retries)
    initial_delay = Keyword.get(opts, :initial_delay, @initial_delay)

    do_retry(func, 0, max_retries, initial_delay)
  end

  defp do_retry(func, attempt, max_retries, delay) when attempt <= max_retries do
                                                 # => attempt: Current attempt number
                                                 # => max_retries: Maximum attempts
                                                 # => delay: Current backoff delay
    case func.() do                              # => Execute function
      {:ok, result} ->
        {:ok, result}                            # => Success: return result

      {:error, reason} = error ->
        if retryable?(reason) and attempt < max_retries do
          # => Transient error: retry after delay
          actual_delay = calculate_backoff(attempt, delay)
          Process.sleep(actual_delay)            # => Wait before retry
                                                 # => Exponential backoff + jitter
          do_retry(func, attempt + 1, max_retries, delay)
        else
          # => Non-retryable or max attempts: fail
          {:error, {:max_retries_exceeded, reason}}
        end
    end
  end

  defp retryable?(reason) do
                                                 # => Determine if error is retryable
    reason in [
      :timeout,                                  # => Network timeout
      :gateway_timeout,                          # => Gateway timeout
      :network_error,                            # => Network issue
      :service_unavailable,                      # => Temporary unavailability
      :rate_limit                                # => Rate limit (wait and retry)
    ]
  end

  defp calculate_backoff(attempt, initial_delay) do
                                                 # => Calculate exponential delay
    exponential = initial_delay * :math.pow(2, attempt)
                                                 # => Doubles each attempt
                                                 # => Attempt 0: 100ms
                                                 # => Attempt 1: 200ms
                                                 # => Attempt 2: 400ms
    capped = min(exponential, @max_delay)        # => Cap at 30s
    jitter = capped * @jitter_factor * :rand.uniform()
                                                 # => Add random jitter (0-10%)
                                                 # => Prevents thundering herd
    round(capped + jitter)                       # => Final delay with jitter
  end
end
```

**Usage with payment processing**:

```elixir
defmodule Finance.PaymentService do
  alias Finance.{ExternalAPI, RetryStrategy}

  def charge_card_with_retry(card_token, amount) do
                                                 # => Charge with automatic retries
    RetryStrategy.retry(fn ->
      ExternalAPI.charge_card(card_token, amount)
    end, max_retries: 3, initial_delay: 200)    # => 3 retries, 200ms initial delay
                                                 # => Delays: 200ms, 400ms, 800ms
  end
end
```

**Best practice**: Use exponential backoff with jitter for all retries. Define clear retryable vs non-retryable errors.

### Pattern 8: Combining Circuit Breaker and Retry

Circuit breaker protects system, retry handles transient failures.

```elixir
defmodule Finance.ResilientPaymentService do
  alias Finance.{ExternalAPI, PaymentGatewayCircuitBreaker, RetryStrategy}

  def charge_card(card_token, amount) do
                                                 # => Maximum resilience strategy
    # => Layer 1: Retry for transient failures
    RetryStrategy.retry(fn ->
      # => Layer 2: Circuit breaker for cascading failure prevention
      PaymentGatewayCircuitBreaker.call(fn ->
        # => Layer 3: External API with exception handling
        ExternalAPI.charge_card(card_token, amount)
      end)
    end, max_retries: 3, initial_delay: 200)
                                                 # => Returns: {:ok, receipt} or {:error, reason}
  end
end
```

**Failure handling**:

```elixir
case Finance.ResilientPaymentService.charge_card(token, 1000) do
  {:ok, receipt} ->
    # => Success: process receipt
    IO.puts("Payment successful: #{receipt.transaction_id}")

  {:error, :circuit_open} ->
    # => Circuit open: service degraded
    # => Don't retry, notify user to try later
    {:error, :service_temporarily_unavailable}

  {:error, {:max_retries_exceeded, :gateway_timeout}} ->
    # => All retries exhausted: timeout
    # => Log for investigation, notify user
    {:error, :payment_timeout}

  {:error, :invalid_card} ->
    # => Non-retryable: invalid input
    # => Don't retry, notify user immediately
    {:error, :invalid_card_details}
end
```

**Best practice**: Combine circuit breaker (prevents cascading failures) with retry (handles transient issues). Log all failure modes for monitoring.

## Idempotency for Retry Safety

### Pattern 9: Idempotent Operations

Retries must be safe to execute multiple times without side effects.

```elixir
# Idempotent payment processing
defmodule Finance.IdempotentPaymentProcessor do
  # => Ensures payment processed exactly once even with retries

  def process_payment(idempotency_key, payment_data) do
                                                 # => idempotency_key: Unique request identifier
                                                 # => payment_data: Payment details
    # => Check if already processed
    case get_previous_result(idempotency_key) do
      {:ok, previous_result} ->
        # => Already processed: return cached result
        {:ok, previous_result}                   # => Safe retry: no double charge

      {:error, :not_found} ->
        # => First attempt: process payment
        with {:ok, receipt} <- charge_payment(payment_data),
             :ok <- store_result(idempotency_key, receipt) do
                                                 # => Store result for future retries
          {:ok, receipt}
        else
          error -> error                         # => Propagate error
        end
    end
  end

  defp get_previous_result(idempotency_key) do
    # => Check cache/database for previous result
    # => Simulated with process dictionary
    case Process.get({:payment_result, idempotency_key}) do
      nil -> {:error, :not_found}                # => First request
      result -> {:ok, result}                    # => Duplicate request
    end
  end

  defp store_result(idempotency_key, receipt) do
    # => Store result in cache/database
    # => Simulated with process dictionary
    Process.put({:payment_result, idempotency_key}, receipt)
    :ok
  end

  defp charge_payment(payment_data) do
    # => Actual payment charge (simulated)
    if :rand.uniform() > 0.3 do                  # => 70% success rate
      {:ok, %{transaction_id: generate_id(), amount: payment_data.amount}}
    else
      {:error, :gateway_timeout}                 # => 30% transient failure
    end
  end

  defp generate_id, do: :crypto.strong_rand_bytes(16) |> Base.encode64()
end
```

**Usage with retry**:

```elixir
# Client generates idempotency key once
idempotency_key = "payment-#{user_id}-#{:os.system_time(:millisecond)}"
                                                 # => Unique per payment request
                                                 # => Same key used for all retries

Finance.RetryStrategy.retry(fn ->
  Finance.IdempotentPaymentProcessor.process_payment(
    idempotency_key,                             # => Same key for retries
    payment_data
  )
end)
```

**Best practice**: All retriable operations must be idempotent. Use client-generated idempotency keys, not server-generated request IDs.

## Real-World Integration Example

### Complete Resilient Payment System

```elixir
defmodule Finance.ProductionPaymentSystem do
  @moduledoc """
  Production-grade payment system combining:
  - Error tuple conventions for domain errors
  - with pipelines for validation
  - try/rescue for external library exceptions
  - Circuit breaker for cascading failure prevention
  - Retry with exponential backoff for transient failures
  - Idempotency for retry safety
  """

  alias Finance.{
    PaymentValidator,
    BankAccount,
    FraudDetector,
    IdempotentPaymentProcessor,
    PaymentGatewayCircuitBreaker,
    RetryStrategy
  }

  def process_payment(payment_request) do
                                                 # => payment_request: Full payment details
    with {:ok, validated} <- validate_request(payment_request),
         {:ok, receipt} <- execute_payment(validated) do
      audit_success(validated, receipt)
      notify_customer(:success, receipt)
      {:ok, receipt}
    else
      {:error, :circuit_open} = error ->
        audit_failure(payment_request, error)
        notify_customer(:service_unavailable, nil)
        error

      {:error, {:max_retries_exceeded, reason}} = error ->
        audit_failure(payment_request, error)
        notify_customer(:payment_timeout, nil)
        {:error, :payment_failed}

      {:error, reason} = error ->
        audit_failure(payment_request, error)
        notify_customer(:payment_failed, nil)
        error
    end
  end

  defp validate_request(payment_request) do
                                                 # => Validation pipeline
    with {:ok, payment} <- PaymentValidator.validate_payment(payment_request),
         {:ok, account} <- BankAccount.validate_account(payment.account_number),
         {:ok, _check} <- FraudDetector.check_transaction(payment) do
      {:ok, Map.put(payment, :validated_account, account)}
    end
  end

  defp execute_payment(validated_payment) do
                                                 # => Execute with full resilience
    idempotency_key = validated_payment.idempotency_key

    RetryStrategy.retry(fn ->
      PaymentGatewayCircuitBreaker.call(fn ->
        IdempotentPaymentProcessor.process_payment(
          idempotency_key,
          validated_payment
        )
      end)
    end, max_retries: 3, initial_delay: 200)
  end

  defp audit_success(payment, receipt) do
    # => Compliance logging
    require Logger
    Logger.info("Payment success",
      transaction_id: receipt.transaction_id,
      amount: payment.amount,
      account: payment.account_number
    )
  end

  defp audit_failure(payment, error) do
    # => Compliance logging
    require Logger
    Logger.error("Payment failure",
      error: inspect(error),
      amount: payment.amount,
      account: payment[:account_number]
    )
  end

  defp notify_customer(status, receipt) do
    # => Customer notification (email/SMS)
    IO.puts("Customer notification: #{status}")
  end
end
```

## Error Handling Checklist

Before deploying error handling code:

- [ ] Expected failures use error tuples `{:ok, value}` or `{:error, reason}`
- [ ] Specific error reasons for different failure modes
- [ ] `with` pipelines for validation with proper else clauses
- [ ] try/rescue only at system boundaries to convert exceptions
- [ ] Circuit breakers for all external dependencies
- [ ] Retry with exponential backoff and jitter
- [ ] Retryable errors clearly defined and distinguished
- [ ] Idempotent operations for all retriable functions
- [ ] Comprehensive audit logging for compliance
- [ ] Customer notifications for all error paths
- [ ] Admin alerts for critical failures (fraud, circuit open)
- [ ] Monitoring and metrics for error rates and circuit states

## Summary

Elixir error handling combines multiple patterns:

**Error tuples** - Expected failures in domain logic
**with pipelines** - Validation chains with explicit error handling
**try/rescue** - Converting external exceptions to error tuples (use sparingly)
**Circuit breakers** - Preventing cascading failures from external dependencies
**Retry with backoff** - Handling transient failures automatically
**Idempotency** - Making retries safe through deduplication

**Key principle**: Design for failure. External dependencies will fail, network requests will timeout, and services will degrade. Build resilience patterns from the start, not after production incidents.

## Next Steps

- [Testing Strategies](/en/learn/software-engineering/programming-languages/elixir/in-the-field/testing-strategies) - Test error handling paths
- [Supervisor Trees](/en/learn/software-engineering/programming-languages/elixir/in-the-field/supervisor-trees) - Process-level fault tolerance
- [Phoenix Framework](/en/learn/software-engineering/programming-languages/elixir/in-the-field/phoenix-framework) - HTTP error handling in Phoenix
- [Best Practices](/en/learn/software-engineering/programming-languages/elixir/in-the-field/best-practices) - Production error handling patterns
