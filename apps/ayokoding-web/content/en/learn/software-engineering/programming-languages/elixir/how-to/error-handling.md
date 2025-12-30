---
title: "Error Handling"
date: 2025-12-21T10:00:00+07:00
draft: false
description: "Master Elixir error handling using result tuples, with expressions, try-catch-rescue, and supervision strategies"
weight: 1000003
tags: ["elixir", "error-handling", "result-tuples", "with", "try-catch"]
---

**Tired of null pointer exceptions and scattered error handling?** This guide teaches Elixir's idiomatic error handling using result tuples, with expressions, pattern matching, and supervision for fault tolerance.

## Problem

Traditional error handling with exceptions leads to:

- Hidden failure modes (uncaught exceptions)
- Scattered try-catch blocks
- Unclear error propagation
- Difficulty composing operations
- Poor testability

Elixir uses explicit result tuples `{:ok, value}` and `{:error, reason}` for predictable, composable error handling.

## Prerequisites

- [Quick Start Tutorial](/en/learn/software-engineering/programming-languages/elixir/tutorials/quick-start) - Basic syntax
- [Beginner Tutorial](/en/learn/software-engineering/programming-languages/elixir/tutorials/beginner) - Pattern matching
- [Pattern Matching Guide](/en/learn/software-engineering/programming-languages/elixir/how-to/pattern-matching)

## Solution Overview

Elixir error handling strategies:

1. **Result Tuples**: `{:ok, value}` / `{:error, reason}` pattern
2. **With Expressions**: Chain operations with error handling
3. **Try-Rescue**: Handle exceptions (use sparingly)
4. **Supervision**: Let processes crash and restart

## Result Tuple Pattern

### Basic Pattern

```elixir
defmodule UserService do
  # Success case
  def create_user(%{name: name, email: email})
      when is_binary(name) and is_binary(email) do
    {:ok, %{id: generate_id(), name: name, email: email}}
  end

  # Error case - validation failed
  def create_user(_invalid_params) do
    {:error, :invalid_params}
  end

  # Pattern match results
  def handle_user_creation(params) do
    case create_user(params) do
      {:ok, user} ->
        IO.puts("User created: #{user.name}")
        {:ok, user}

      {:error, :invalid_params} ->
        IO.puts("Invalid parameters")
        {:error, :validation_failed}
    end
  end

  defp generate_id, do: :rand.uniform(10000)
end

UserService.create_user(%{name: "Alice", email: "alice@example.com"})

UserService.create_user(%{invalid: "data"})
```

**How It Works**: Functions return tuples indicating success or failure. Callers pattern match to handle each case explicitly.

### Multiple Error Types

```elixir
defmodule AccountService do
  def withdraw(account, amount) do
    cond do
      amount <= 0 ->
        {:error, :invalid_amount}

      account.balance < amount ->
        {:error, :insufficient_funds}

      account.status == :frozen ->
        {:error, :account_frozen}

      true ->
        new_balance = account.balance - amount
        {:ok, %{account | balance: new_balance}}
    end
  end

  # Pattern match specific errors
  def handle_withdrawal(account, amount) do
    case withdraw(account, amount) do
      {:ok, updated_account} ->
        notify_success(updated_account, amount)
        {:ok, updated_account}

      {:error, :invalid_amount} ->
        {:error, "Amount must be positive"}

      {:error, :insufficient_funds} ->
        {:error, "Insufficient funds in account"}

      {:error, :account_frozen} ->
        {:error, "Account is frozen, contact support"}

      {:error, reason} ->
        {:error, "Unexpected error: #{inspect(reason)}"}
    end
  end

  defp notify_success(_account, _amount), do: :ok
end

account = %{id: 1, balance: 100.0, status: :active}

AccountService.withdraw(account, 50.0)

AccountService.withdraw(account, 200.0)
```

### Nested Result Handling

```elixir
defmodule OrderService do
  # Naive approach - nested case statements
  def process_order_nested(order) do
    case validate_order(order) do
      {:ok, validated} ->
        case reserve_inventory(validated) do
          {:ok, reserved} ->
            case charge_payment(reserved) do
              {:ok, charged} ->
                {:ok, charged}

              {:error, reason} ->
                {:error, {:payment_failed, reason}}
            end

          {:error, reason} ->
            {:error, {:inventory_failed, reason}}
        end

      {:error, reason} ->
        {:error, {:validation_failed, reason}}
    end
  end

  # Better approach - with expression
  def process_order(order) do
    with {:ok, validated} <- validate_order(order),
         {:ok, reserved} <- reserve_inventory(validated),
         {:ok, charged} <- charge_payment(reserved) do
      {:ok, charged}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp validate_order(order) do
    if Map.has_key?(order, :items) and length(order.items) > 0 do
      {:ok, order}
    else
      {:error, :empty_order}
    end
  end

  defp reserve_inventory(order), do: {:ok, order}
  defp charge_payment(order), do: {:ok, Map.put(order, :status, :paid)}
end
```

## With Expressions

### Basic With

```elixir
defmodule RegistrationService do
  def register_user(params) do
    with {:ok, validated} <- validate_params(params),
         {:ok, hashed_password} <- hash_password(validated.password),
         {:ok, user} <- create_user(validated, hashed_password),
         {:ok, _email} <- send_confirmation(user) do
      {:ok, user}
    else
      {:error, :weak_password} ->
        {:error, "Password must be at least 8 characters"}

      {:error, :email_taken} ->
        {:error, "Email already registered"}

      {:error, :email_failed} ->
        # User created but email failed - still return success
        {:ok, user}

      error ->
        {:error, "Registration failed: #{inspect(error)}"}
    end
  end

  defp validate_params(%{email: email, password: password})
      when is_binary(email) and is_binary(password) do
    if String.contains?(email, "@") do
      {:ok, %{email: email, password: password}}
    else
      {:error, :invalid_email}
    end
  end
  defp validate_params(_), do: {:error, :invalid_params}

  defp hash_password(password) when byte_size(password) >= 8 do
    {:ok, :crypto.hash(:sha256, password) |> Base.encode64()}
  end
  defp hash_password(_), do: {:error, :weak_password}

  defp create_user(%{email: email}, hashed_password) do
    # Simulate DB check
    if email == "taken@example.com" do
      {:error, :email_taken}
    else
      {:ok, %{id: :rand.uniform(1000), email: email, password: hashed_password}}
    end
  end

  defp send_confirmation(_user), do: {:ok, "sent"}
end

RegistrationService.register_user(%{
  email: "alice@example.com",
  password: "secure_password"
})

RegistrationService.register_user(%{email: "bob@example.com", password: "weak"})
```

### With Guards

```elixir
defmodule DataProcessor do
  def process_data(input) do
    with {:ok, parsed} <- parse_input(input),
         {:ok, value} when value > 0 <- compute_value(parsed),
         {:ok, result} when is_binary(result) <- format_result(value) do
      {:ok, result}
    else
      {:ok, 0} ->
        {:error, :zero_result}

      {:ok, value} when not is_binary(value) ->
        {:error, :invalid_format}

      {:error, reason} ->
        {:error, reason}

      _ ->
        {:error, :unknown_error}
    end
  end

  defp parse_input(input) when is_binary(input) do
    {:ok, %{data: input}}
  end
  defp parse_input(_), do: {:error, :invalid_input}

  defp compute_value(%{data: data}) do
    {:ok, String.length(data)}
  end

  defp format_result(value) do
    {:ok, "Result: #{value}"}
  end
end
```

### With Bare Values

```elixir
defmodule ConfigLoader do
  def load_config(path) do
    with {:ok, content} <- File.read(path),
         {:ok, decoded} <- Jason.decode(content),
         true <- valid_config?(decoded) do
      {:ok, decoded}
    else
      {:error, :enoent} ->
        {:error, "File not found: #{path}"}

      {:error, %Jason.DecodeError{}} ->
        {:error, "Invalid JSON format"}

      false ->
        {:error, "Invalid configuration structure"}

      error ->
        {:error, "Failed to load config: #{inspect(error)}"}
    end
  end

  defp valid_config?(%{"version" => _v, "settings" => _s}), do: true
  defp valid_config?(_), do: false
end
```

## Exception Handling

### Try-Rescue

```elixir
defmodule SafeOperations do
  # Use for truly exceptional cases
  def safe_divide(a, b) do
    try do
      result = a / b
      {:ok, result}
    rescue
      ArithmeticError ->
        {:error, :division_by_zero}
    end
  end

  # Rescue specific exceptions
  def safe_parse_json(json_string) do
    try do
      {:ok, Jason.decode!(json_string)}
    rescue
      Jason.DecodeError -> {:error, :invalid_json}
      ArgumentError -> {:error, :invalid_input}
    end
  end

  # Catch exits and throws
  def safe_external_call(fun) do
    try do
      {:ok, fun.()}
    rescue
      e in RuntimeError ->
        {:error, {:runtime_error, e.message}}
    catch
      :exit, reason ->
        {:error, {:exit, reason}}

      :throw, value ->
        {:error, {:thrown, value}}
    end
  end

  # After clause for cleanup
  def with_file_cleanup(path) do
    file = File.open!(path)

    try do
      content = IO.read(file, :all)
      {:ok, content}
    rescue
      e -> {:error, e}
    after
      File.close(file)
    end
  end
end

SafeOperations.safe_divide(10, 2)

SafeOperations.safe_divide(10, 0)
```

### When to Use Try-Rescue

**Do Use**:

- Third-party libraries that raise exceptions
- Interop with Erlang libraries
- Truly exceptional conditions (file I/O failures)

**Don't Use**:

- Normal control flow
- Expected errors (use result tuples)
- Business logic errors

```elixir
def find_user(id) do
  try do
    user = Repo.get!(User, id)
    {:ok, user}
  rescue
    Ecto.NoResultsError -> {:error, :not_found}
  end
end

def find_user(id) do
  case Repo.get(User, id) do
    nil -> {:error, :not_found}
    user -> {:ok, user}
  end
end
```

## Error Wrapping

### Add Context to Errors

```elixir
defmodule PaymentProcessor do
  def process_payment(order) do
    with {:ok, validated} <- validate_payment_details(order),
         {:ok, charged} <- charge_card(validated),
         {:ok, receipt} <- generate_receipt(charged) do
      {:ok, receipt}
    else
      {:error, :invalid_card} = error ->
        wrap_error(error, :validation, "Invalid credit card information")

      {:error, :insufficient_funds} = error ->
        wrap_error(error, :payment, "Insufficient funds")

      {:error, :gateway_timeout} = error ->
        wrap_error(error, :gateway, "Payment gateway timeout")

      error ->
        wrap_error(error, :unknown, "Payment processing failed")
    end
  end

  defp wrap_error(original_error, category, message) do
    {:error, %{
      category: category,
      message: message,
      original: original_error,
      timestamp: DateTime.utc_now()
    }}
  end

  defp validate_payment_details(_order), do: {:ok, %{}}
  defp charge_card(_validated), do: {:ok, %{}}
  defp generate_receipt(_charged), do: {:ok, %{}}
end
```

### Error Transformation

```elixir
defmodule APIClient do
  def fetch_user(id) do
    case HTTPoison.get("https://api.example.com/users/#{id}") do
      {:ok, %{status_code: 200, body: body}} ->
        Jason.decode(body)

      {:ok, %{status_code: 404}} ->
        {:error, :user_not_found}

      {:ok, %{status_code: status}} when status >= 500 ->
        {:error, :server_error}

      {:error, %HTTPoison.Error{reason: :timeout}} ->
        {:error, :request_timeout}

      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, {:http_error, reason}}
    end
  end

  # Normalize different error formats
  def normalize_error(error) do
    case error do
      {:error, %Ecto.Changeset{} = changeset} ->
        errors = Ecto.Changeset.traverse_errors(changeset, fn {msg, _opts} ->
          msg
        end)
        {:error, {:validation_failed, errors}}

      {:error, :not_found} ->
        {:error, {:not_found, "Resource not found"}}

      {:error, reason} when is_atom(reason) ->
        {:error, {reason, humanize(reason)}}

      error ->
        error
    end
  end

  defp humanize(atom) do
    atom
    |> Atom.to_string()
    |> String.replace("_", " ")
    |> String.capitalize()
  end
end
```

## Assertion and Bang Functions

### ! Functions (Raise on Error)

```elixir
defmodule BangExamples do
  # Non-bang version (returns result tuple)
  def fetch_user(id) do
    case DB.get(id) do
      nil -> {:error, :not_found}
      user -> {:ok, user}
    end
  end

  # Bang version (raises on error)
  def fetch_user!(id) do
    case fetch_user(id) do
      {:ok, user} -> user
      {:error, reason} -> raise "User not found: #{reason}"
    end
  end

  # Using built-in bang functions
  def process_file(path) do
    # File.read returns {:ok, content} or {:error, reason}
    case File.read(path) do
      {:ok, content} -> process_content(content)
      {:error, reason} -> {:error, reason}
    end
  end

  def process_file!(path) do
    # File.read! raises on error
    content = File.read!(path)
    process_content(content)
  end

  defp process_content(content), do: {:ok, content}
end

case BangExamples.fetch_user(123) do
  {:ok, user} -> IO.puts("Found: #{user.name}")
  {:error, :not_found} -> IO.puts("User not found")
end

user = BangExamples.fetch_user!(123)
IO.puts("Found: #{user.name}")
```

## Supervision and Let It Crash

### Crash vs Error Handling

```elixir
defmodule Worker do
  use GenServer

  # Expected errors - handle gracefully
  def handle_call({:process, data}, _from, state) do
    case validate_data(data) do
      {:ok, valid_data} ->
        result = process_data(valid_data)
        {:reply, {:ok, result}, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  # Unexpected errors - let it crash
  def handle_call({:risky_operation, data}, _from, state) do
    # Don't wrap in try-rescue
    # Let supervisor handle crashes
    result = risky_operation(data)
    {:reply, {:ok, result}, state}
  end

  defp validate_data(data) when is_map(data), do: {:ok, data}
  defp validate_data(_), do: {:error, :invalid_data}

  defp process_data(data), do: data

  defp risky_operation(data) do
    # This might crash - that's okay!
    # Supervisor will restart the process
    data.field_that_might_not_exist
  end
end

defmodule WorkerSupervisor do
  use Supervisor

  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @impl true
  def init(_init_arg) do
    children = [
      {Worker, []}
    ]

    # Restart worker if it crashes
    Supervisor.init(children, strategy: :one_for_one)
  end
end
```

### When to Let It Crash

**Let It Crash When**:

- Unexpected errors (bugs)
- Corrupt state
- Programming errors
- External dependency failures

**Handle Errors When**:

- Expected failures (validation, not found)
- Business logic errors
- User input errors
- Recoverable errors

## Error Logging

### Structured Logging

```elixir
defmodule LoggingService do
  require Logger

  def process_with_logging(data) do
    Logger.info("Starting processing", data: inspect(data))

    case process_data(data) do
      {:ok, result} ->
        Logger.info("Processing succeeded",
          data: inspect(data),
          result: inspect(result))
        {:ok, result}

      {:error, reason} = error ->
        Logger.error("Processing failed",
          data: inspect(data),
          reason: inspect(reason))
        error
    end
  end

  # Log different error levels
  def categorized_logging(operation) do
    case operation do
      {:error, :not_found} ->
        Logger.warning("Resource not found")
        {:error, :not_found}

      {:error, :validation_failed} ->
        Logger.info("Validation failed (expected)")
        {:error, :validation_failed}

      {:error, reason} ->
        Logger.error("Unexpected error: #{inspect(reason)}")
        {:error, reason}

      result ->
        result
    end
  end

  defp process_data(data), do: {:ok, data}
end
```

## Best Practices

### Do: Return Consistent Result Types

```elixir
def fetch_user(id) do
  case Repo.get(User, id) do
    nil -> {:error, :not_found}
    user -> {:ok, user}
  end
end

def fetch_user(id) do
  case Repo.get(User, id) do
    nil -> nil  # Inconsistent!
    user -> {:ok, user}
  end
end
```

### Do: Use With for Error Chains

```elixir
def create_order(params) do
  with {:ok, validated} <- validate(params),
       {:ok, order} <- insert_order(validated),
       {:ok, _email} <- send_email(order) do
    {:ok, order}
  end
end

def create_order(params) do
  case validate(params) do
    {:ok, validated} ->
      case insert_order(validated) do
        {:ok, order} ->
          case send_email(order) do
            {:ok, _email} -> {:ok, order}
            error -> error
          end
        error -> error
      end
    error -> error
  end
end
```

### Do: Add Context to Errors

```elixir
{:error, {:validation_failed, %{email: ["has already been taken"]}}}

{:error, :failed}
```

### Don't: Overuse Try-Rescue

```elixir
def parse_json(string) do
  case Jason.decode(string) do
    {:ok, decoded} -> {:ok, decoded}
    {:error, _} -> {:error, :invalid_json}
  end
end

def parse_json(string) do
  try do
    {:ok, Jason.decode!(string)}
  rescue
    _ -> {:error, :invalid_json}
  end
end
```

## See Also

- [Cookbook - Error Handling Recipes](/en/learn/software-engineering/programming-languages/elixir/how-to/cookbook#error-handling)
- [Beginner Tutorial - Error Handling](/en/learn/software-engineering/programming-languages/elixir/tutorials/beginner#error-handling)
- [Supervision Guide](/en/learn/software-engineering/programming-languages/elixir/how-to/supervision)
- [Best Practices - Error Handling](/en/learn/software-engineering/programming-languages/elixir/explanation/best-practices)
