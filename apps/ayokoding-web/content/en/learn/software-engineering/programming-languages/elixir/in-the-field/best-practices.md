---
title: "Best Practices"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000001
description: "Production patterns and OTP-first best practices for building reliable Elixir systems"
tags: ["elixir", "best-practices", "otp", "production", "genserver", "supervisor"]
prev: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/overview"
next: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/anti-patterns"
---

**Building production Elixir systems?** This guide teaches industry best practices following the OTP-first principle, ensuring you leverage BEAM's full fault-tolerance and concurrency capabilities.

## Why Production Best Practices Matter

Production Elixir differs fundamentally from development environments. The BEAM VM provides powerful OTP primitives for fault tolerance, but wrong patterns create:

- **Process leaks** - Memory growth from unsupervised processes
- **Message queue overflow** - Unbounded mailbox growth causing memory exhaustion
- **Supervision violations** - Child processes outliving supervisors
- **State corruption** - Shared mutable state in supposedly isolated processes
- **Race conditions** - Timing-dependent bugs in concurrent code
- **Resource exhaustion** - Unmanaged database connections, file handles
- **Deployment disasters** - Hot code upgrade failures, configuration errors

**These best practices prevent production disasters** by establishing OTP patterns that work reliably at scale.

## Financial Domain Examples

Examples use Shariah-compliant financial operations:

- **Zakat calculation** - Processing donation percentages for charity
- **Donation tracking** - Managing charitable contribution records
- **Transaction auditing** - Recording all financial state changes

These domains demonstrate production patterns with real business logic.

## Supervisor Tree Patterns

### Pattern 1: Supervision Strategy Selection

Supervisor strategies determine how process failures affect siblings.

**OTP Primitive**: Supervisor with `:one_for_one` strategy.

```elixir
# Financial system supervision tree
defmodule Finance.Supervisor do
  use Supervisor                                 # => Imports Supervisor behavior
                                                 # => Provides init/1 callback

  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
                                                 # => Starts supervisor process
                                                 # => Registers with module name
                                                 # => Returns {:ok, pid}
  end

  def init(_init_arg) do
    children = [
      {Finance.ZakatCalculator, []},             # => Zakat calculation service
      {Finance.DonationTracker, []},             # => Donation tracking service
      {Finance.AuditLog, []}                     # => Audit logging service
    ]                                            # => List of child specifications
                                                 # => Each tuple: {module, init_args}

    Supervisor.init(children, strategy: :one_for_one)
                                                 # => :one_for_one strategy
                                                 # => If child dies, restart only that child
                                                 # => Other children unaffected
                                                 # => Returns {:ok, {supervisor_spec, children}}
  end
end
```

**When to use each strategy**:

```elixir
# :one_for_one - Independent services (DEFAULT)
Supervisor.init(children, strategy: :one_for_one)
                                                 # => Child failures isolated
                                                 # => Use when: Services independent
                                                 # => Example: API endpoints, workers

# :one_for_all - Tightly coupled services
Supervisor.init(children, strategy: :one_for_all)
                                                 # => Any child failure restarts ALL
                                                 # => Use when: Services depend on each other
                                                 # => Example: Database + cache + queue

# :rest_for_one - Sequential dependencies
Supervisor.init(children, strategy: :rest_for_one)
                                                 # => Child N failure restarts N and all after
                                                 # => Use when: Sequential pipeline
                                                 # => Example: Reader -> Parser -> Writer
```

**Best practice**: Start with `:one_for_one` for independence. Only use `:one_for_all` when services truly must restart together.

### Pattern 2: Nested Supervision Trees

Complex applications require hierarchical supervision.

```elixir
# Top-level application supervisor
defmodule Finance.Application do
  use Application                                # => Application behavior
                                                 # => Provides start/2 callback

  def start(_type, _args) do
    children = [
      Finance.CoreSupervisor,                    # => Core financial services
      Finance.WebSupervisor,                     # => Web API services
      Finance.ReportingSupervisor                # => Reporting and analytics
    ]                                            # => Three major subsystems
                                                 # => Each manages own tree

    opts = [strategy: :one_for_one, name: Finance.Supervisor]
                                                 # => Top-level strategy
                                                 # => Subsystem failures isolated
    Supervisor.start_link(children, opts)        # => Returns {:ok, pid}
  end
end

# Core services subtree
defmodule Finance.CoreSupervisor do
  use Supervisor

  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  def init(_init_arg) do
    children = [
      {Finance.ZakatCalculator, []},             # => Zakat calculation
      {Finance.DonationTracker, []},             # => Donation tracking
      {Finance.TransactionSupervisor, []}        # => Nested: transaction workers
    ]                                            # => Core financial services
                                                 # => TransactionSupervisor manages pool

    Supervisor.init(children, strategy: :one_for_one)
  end
end

# Dynamic worker pool subtree
defmodule Finance.TransactionSupervisor do
  use DynamicSupervisor                          # => Dynamic child management
                                                 # => Start/stop children at runtime

  def start_link(init_arg) do
    DynamicSupervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  def init(_init_arg) do
    DynamicSupervisor.init(strategy: :one_for_one)
                                                 # => Children started dynamically
                                                 # => Not in init/1
  end

  def start_transaction(transaction_data) do
    spec = {Finance.TransactionWorker, transaction_data}
                                                 # => Child specification
                                                 # => transaction_data: Worker init args
    DynamicSupervisor.start_child(__MODULE__, spec)
                                                 # => Starts supervised worker
                                                 # => Returns {:ok, pid}
  end
end
```

**Supervision hierarchy best practices**:

1. **Top level**: Application supervisor with major subsystems
2. **Middle level**: Subsystem supervisors grouping related services
3. **Bottom level**: Worker processes or dynamic supervisors for pools

**Rule**: Keep each supervisor focused. Maximum 5-10 children per supervisor for clarity.

### Pattern 3: Restart Strategies

Configure restart behavior for fault tolerance.

```elixir
defmodule Finance.ZakatCalculator do
  use GenServer

  # Client API
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def child_spec(opts) do
    %{
      id: __MODULE__,                            # => Child identifier (must be unique)
      start: {__MODULE__, :start_link, [opts]}, # => Start function: MFA tuple
      restart: :permanent,                       # => Restart strategy
      shutdown: 5000,                            # => Shutdown timeout (milliseconds)
      type: :worker                              # => Process type
    }                                            # => Child specification map
                                                 # => Used by Supervisor
  end

  # Server callbacks
  def init(_opts) do
    {:ok, %{}}                                   # => Initial state: empty map
  end
end
```

**Restart strategy options**:

```elixir
# :permanent - Always restart (DEFAULT for critical services)
restart: :permanent                              # => Supervisor always restarts child
                                                 # => Use for: Core services
                                                 # => Example: Database, API server

# :temporary - Never restart (fire-and-forget tasks)
restart: :temporary                              # => Supervisor never restarts child
                                                 # => Use for: One-off tasks
                                                 # => Example: Email send, log write

# :transient - Restart only on abnormal exit (recommended for workers)
restart: :transient                              # => Restart if exit not :normal
                                                 # => Use for: Batch jobs
                                                 # => Example: Report generation
```

**Best practice**: Use `:permanent` for long-lived services, `:transient` for workers, `:temporary` for fire-and-forget tasks.

## Process Registry Patterns

### Pattern 4: Named vs Registry-Based Processes

Choose process naming strategy based on cardinality.

**Single instance - Named registration**:

```elixir
defmodule Finance.AuditLog do
  use GenServer

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
                                                 # => Registers globally with module name
                                                 # => Only ONE instance allowed
                                                 # => Returns {:ok, pid}
  end

  def log_transaction(transaction) do
    GenServer.cast(__MODULE__, {:log, transaction})
                                                 # => Async message to named process
                                                 # => No reply expected
  end

  def init(:ok) do
    {:ok, []}                                    # => Initial state: empty list
  end

  def handle_cast({:log, transaction}, state) do
    new_state = [transaction | state]            # => Prepend transaction
    {:noreply, new_state}                        # => Update state, no reply
  end
end
```

**Multiple instances - Registry-based lookup**:

```elixir
defmodule Finance.DonationTracker do
  use GenServer

  # Client API
  def start_link(user_id) do
    GenServer.start_link(__MODULE__, user_id, name: via_tuple(user_id))
                                                 # => Registers with Registry
                                                 # => Multiple instances (one per user)
  end

  defp via_tuple(user_id) do
    {:via, Registry, {Finance.Registry, {__MODULE__, user_id}}}
                                                 # => Registry-based name
                                                 # => {module, user_id} as key
                                                 # => Unique per user
  end

  def track_donation(user_id, amount) do
    case Registry.lookup(Finance.Registry, {__MODULE__, user_id}) do
      [{pid, _}] ->                              # => Process found
        GenServer.call(pid, {:donate, amount})   # => Send message to process
      [] ->                                      # => Process not found
        {:error, :not_found}                     # => Return error
    end
  end

  # Server callbacks
  def init(user_id) do
    state = %{user_id: user_id, total: 0}        # => Initial state: zero donations
    {:ok, state}
  end

  def handle_call({:donate, amount}, _from, state) do
    new_total = state.total + amount             # => Add donation
    new_state = %{state | total: new_total}      # => Update total
    {:reply, new_total, new_state}               # => Reply with new total
  end
end

# Registry setup in application supervisor
def start(_type, _args) do
  children = [
    {Registry, keys: :unique, name: Finance.Registry},
                                                 # => Registry for process lookup
                                                 # => :unique - One value per key
    # ... other children
  ]
  Supervisor.start_link(children, strategy: :one_for_one)
end
```

**When to use each approach**:

| Pattern           | Use When                        | Example               |
| ----------------- | ------------------------------- | --------------------- |
| Named             | Single instance, global service | AuditLog, ConfigStore |
| Registry (via)    | Multiple instances by key       | UserSession, OrderBot |
| DynamicSupervisor | Pools of workers                | JobWorker, TaskRunner |

### Pattern 5: Process Lifecycle Management

Properly initialize and cleanup process resources.

```elixir
defmodule Finance.DatabaseConnection do
  use GenServer

  # Client API
  def start_link(config) do
    GenServer.start_link(__MODULE__, config, name: __MODULE__)
  end

  # Server callbacks
  def init(config) do
    # SYNCHRONOUS initialization in init/1
    case establish_connection(config) do
      {:ok, conn} ->                             # => Connection successful
        state = %{conn: conn, config: config}    # => Store connection
        {:ok, state}                             # => Return initial state
      {:error, reason} ->                        # => Connection failed
        {:stop, reason}                          # => Stop process immediately
    end                                          # => Supervisor will retry
  end

  def handle_info(:timeout, state) do
    # Cleanup on timeout
    cleanup_connection(state.conn)               # => Close database connection
    {:stop, :normal, state}                      # => Stop process normally
  end

  def terminate(reason, state) do
    # ALWAYS cleanup in terminate/2
    cleanup_connection(state.conn)               # => Release connection
    :ok                                          # => Return value ignored
  end                                            # => Called before process exits

  # Helper functions
  defp establish_connection(config) do
    # Simulate connection establishment
    {:ok, :connection_handle}                    # => Placeholder connection
  end

  defp cleanup_connection(conn) do
    # Release connection resources
    :ok
  end
end
```

**Critical lifecycle rules**:

1. **init/1 must be fast** - Heavy initialization blocks supervisor
2. **Use terminate/2 for cleanup** - Always release resources
3. **Handle :timeout** - Detect and cleanup hung connections
4. **Return {:stop, reason}** - Signal initialization failure to supervisor

### Pattern 6: Async Initialization

Defer expensive initialization to prevent supervisor blocking.

```elixir
defmodule Finance.ReportGenerator do
  use GenServer

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def init(opts) do
    # Fast initialization - return immediately
    state = %{opts: opts, data: nil, status: :initializing}
    {:ok, state, {:continue, :load_data}}        # => Return with :continue tuple
                                                 # => Triggers handle_continue/2
                                                 # => Non-blocking for supervisor
  end

  def handle_continue(:load_data, state) do
    # Expensive initialization happens HERE (async)
    data = load_large_dataset()                  # => Expensive operation
    new_state = %{state | data: data, status: :ready}
    {:noreply, new_state}                        # => Update state when ready
  end

  def handle_call(:generate_report, _from, %{status: :initializing} = state) do
    {:reply, {:error, :not_ready}, state}        # => Reject if not initialized
  end

  def handle_call(:generate_report, _from, %{status: :ready} = state) do
    report = generate_from_data(state.data)      # => Process data
    {:reply, {:ok, report}, state}               # => Return report
  end

  defp load_large_dataset do
    # Simulate expensive operation
    :timer.sleep(5000)                           # => 5 second delay
    [:data1, :data2, :data3]                     # => Return dataset
  end

  defp generate_from_data(data) do
    "Report based on #{inspect(data)}"           # => Generate report string
  end
end
```

**Async initialization benefits**:

1. **Supervisor doesn't block** - Children start immediately
2. **Application boots faster** - Services become ready incrementally
3. **Graceful degradation** - Service handles calls during initialization

**Pattern**: Return `{:ok, state, {:continue, :init_task}}` from init/1, perform expensive work in handle_continue/2.

## GenServer State Management

### Pattern 7: Immutable State Updates

GenServer state must be immutable - return new state, never mutate.

```elixir
defmodule Finance.ZakatTracker do
  use GenServer

  # State structure: %{user_id => total_zakat}

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def add_zakat(user_id, amount) do
    GenServer.call(__MODULE__, {:add, user_id, amount})
  end

  def init(:ok) do
    {:ok, %{}}                                   # => Initial state: empty map
  end

  def handle_call({:add, user_id, amount}, _from, state) do
    # ❌ WRONG: Mutating state
    # state[user_id] = (state[user_id] || 0) + amount
    # This doesn't work - maps immutable!

    # ✅ CORRECT: Return new state
    current = Map.get(state, user_id, 0)         # => Get current zakat
    new_total = current + amount                 # => Calculate new total
    new_state = Map.put(state, user_id, new_total)
                                                 # => Create new map
                                                 # => Old state unchanged
    {:reply, new_total, new_state}               # => Return new state
  end
end
```

**State update patterns**:

```elixir
# Map update with Map.put
new_state = Map.put(state, key, value)           # => Returns new map
                                                 # => Original state unchanged

# Map update with map syntax (kernel special form)
new_state = %{state | key: new_value}            # => Updates existing key
                                                 # => Raises if key missing

# Map update with default
new_state = Map.update(state, key, default, fn old -> old + 1 end)
                                                 # => Updates if exists
                                                 # => Uses default if missing

# Nested map update
new_state = put_in(state, [:user, :profile, :name], "Ahmad")
                                                 # => Updates nested path
                                                 # => Returns new map
```

### Pattern 8: State Validation

Validate state consistency on updates.

```elixir
defmodule Finance.DonationPool do
  use GenServer

  # State: %{total: integer, donations: [%{user_id, amount, timestamp}]}

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def add_donation(user_id, amount) when amount > 0 do
    GenServer.call(__MODULE__, {:add, user_id, amount})
  end

  def init(:ok) do
    state = %{total: 0, donations: []}           # => Initial state
    {:ok, state}
  end

  def handle_call({:add, user_id, amount}, _from, state) do
    donation = %{
      user_id: user_id,
      amount: amount,
      timestamp: DateTime.utc_now()              # => Record donation time
    }

    new_donations = [donation | state.donations] # => Prepend donation
    new_total = state.total + amount             # => Update total
    new_state = %{state | total: new_total, donations: new_donations}

    # Validate state consistency
    case validate_state(new_state) do
      :ok ->                                     # => State valid
        {:reply, {:ok, new_total}, new_state}    # => Accept update
      {:error, reason} ->                        # => State invalid
        {:reply, {:error, reason}, state}        # => Reject update, keep old state
    end
  end

  defp validate_state(state) do
    calculated_total = Enum.sum(Enum.map(state.donations, & &1.amount))
                                                 # => Sum all donation amounts
    if calculated_total == state.total do
      :ok                                        # => Totals match
    else
      {:error, :total_mismatch}                  # => Inconsistent state
    end
  end
end
```

**Validation best practices**:

1. **Validate invariants** - Check state consistency on updates
2. **Reject invalid updates** - Return old state on validation failure
3. **Use guard clauses** - Validate inputs in function heads
4. **Log validation failures** - Track consistency violations

## Error Handling Strategies

### Pattern 9: Let It Crash (Supervised Processes)

Embrace failures - let supervisors handle recovery.

```elixir
defmodule Finance.TransactionProcessor do
  use GenServer

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def process_transaction(transaction) do
    GenServer.call(__MODULE__, {:process, transaction})
  end

  def init(:ok) do
    {:ok, %{}}
  end

  def handle_call({:process, transaction}, _from, state) do
    # ❌ DEFENSIVE: Try/catch everything
    # try do
    #   result = validate_and_process(transaction)
    #   {:reply, {:ok, result}, state}
    # rescue
    #   e -> {:reply, {:error, e}, state}
    # end

    # ✅ LET IT CRASH: Trust supervisor to restart
    result = validate_and_process!(transaction)  # => Raises on invalid transaction
                                                 # => Process crashes
                                                 # => Supervisor restarts process
                                                 # => State reset to init/1
    {:reply, {:ok, result}, state}               # => Only reached if success
  end

  defp validate_and_process!(transaction) do
    # Business logic that may crash
    unless transaction.amount > 0 do
      raise ArgumentError, "amount must be positive"
                                                 # => Raises exception
    end
    # Process transaction...
    {:processed, transaction}
  end
end
```

**When to let it crash**:

✅ **Crash for**:

- Invalid input that violates contracts
- Unexpected internal errors
- Corrupted state requiring reset
- Errors where recovery is complex

❌ **Don't crash for**:

- Expected business errors (user not found, insufficient balance)
- External service failures (network timeout, API error)
- User validation failures

### Pattern 10: Explicit Error Handling (Expected Failures)

Use tagged tuples for expected errors.

```elixir
defmodule Finance.WithdrawalService do
  use GenServer

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def withdraw(user_id, amount) do
    GenServer.call(__MODULE__, {:withdraw, user_id, amount})
  end

  def init(:ok) do
    state = %{balances: %{}}                     # => Initial state: empty balances
    {:ok, state}
  end

  def handle_call({:withdraw, user_id, amount}, _from, state) do
    current_balance = Map.get(state.balances, user_id, 0)

    # EXPLICIT error handling for business logic
    cond do
      amount <= 0 ->                             # => Invalid amount
        {:reply, {:error, :invalid_amount}, state}

      current_balance < amount ->                # => Insufficient funds
        {:reply, {:error, :insufficient_funds}, state}

      true ->                                    # => Success case
        new_balance = current_balance - amount   # => Deduct amount
        new_balances = Map.put(state.balances, user_id, new_balance)
        new_state = %{state | balances: new_balances}
        {:reply, {:ok, new_balance}, new_state}  # => Return new balance
    end
  end
end
```

**Error handling pattern**:

```elixir
# Pattern match on result
case Finance.WithdrawalService.withdraw(user_id, 100) do
  {:ok, new_balance} ->                          # => Success
    IO.puts("Withdrawal successful. New balance: #{new_balance}")
  {:error, :insufficient_funds} ->               # => Expected error
    IO.puts("Insufficient funds")
  {:error, :invalid_amount} ->                   # => Expected error
    IO.puts("Amount must be positive")
end
```

## Testing OTP Applications

### Pattern 11: Testing GenServers

Test GenServers through public API, not internals.

```elixir
defmodule Finance.ZakatCalculatorTest do
  use ExUnit.Case, async: false                  # => async: false for stateful tests
                                                 # => Prevents parallel execution

  alias Finance.ZakatCalculator

  setup do
    # Start supervised process for each test
    start_supervised!(ZakatCalculator)           # => Starts process
                                                 # => Automatically stopped after test
    :ok                                          # => Return :ok (no context needed)
  end

  test "calculates zakat correctly" do
    # Test through public API
    wealth = 10_000                              # => Total wealth: 10,000
    result = ZakatCalculator.calculate_zakat(wealth)
                                                 # => Call public function
    expected = 250                               # => 2.5% of 10,000
    assert result == {:ok, expected}             # => Verify result
  end

  test "rejects negative wealth" do
    result = ZakatCalculator.calculate_zakat(-1000)
    assert result == {:error, :invalid_wealth}   # => Verify error handling
  end

  test "maintains state across calls" do
    # Test state persistence
    assert {:ok, _} = ZakatCalculator.set_nisab(5_000)
                                                 # => Set minimum threshold
    assert {:ok, 5_000} = ZakatCalculator.get_nisab()
                                                 # => Verify persistence
  end
end
```

**Testing best practices**:

1. **Test public API only** - Don't access internal state
2. **Use start_supervised!** - Automatic cleanup
3. **Set async: false for stateful tests** - Prevent race conditions
4. **Test error cases** - Verify error handling

### Pattern 12: Testing Supervision Trees

Test supervision behavior with process monitoring.

```elixir
defmodule Finance.SupervisorTest do
  use ExUnit.Case, async: false

  test "supervisor restarts crashed children" do
    # Start supervisor
    {:ok, supervisor_pid} = Finance.Supervisor.start_link([])
                                                 # => Start supervision tree

    # Find child process
    children = Supervisor.which_children(supervisor_pid)
                                                 # => List all children
                                                 # => Returns [{id, pid, type, modules}]
    {_id, child_pid, _type, _modules} = List.first(children)
                                                 # => Get first child

    # Monitor child to detect restart
    ref = Process.monitor(child_pid)             # => Monitor child process

    # Kill child
    Process.exit(child_pid, :kill)               # => Force crash child

    # Verify DOWN message received
    assert_receive {:DOWN, ^ref, :process, ^child_pid, :killed}
                                                 # => Child died

    # Verify supervisor restarted child
    :timer.sleep(100)                            # => Wait for restart
    new_children = Supervisor.which_children(supervisor_pid)
    {_id, new_pid, _type, _modules} = List.first(new_children)
    assert new_pid != child_pid                  # => New PID - process restarted
  end
end
```

**Supervision testing patterns**:

1. **Monitor processes** - Use `Process.monitor/1` to detect crashes
2. **Verify restart** - Check new PID after crash
3. **Test strategies** - Verify :one_for_one, :one_for_all behavior
4. **Test restart intensity** - Verify max_restarts enforcement

## Production Checklist

Before deploying Elixir applications:

- [ ] **Supervision tree complete** - All processes supervised
- [ ] **Restart strategies configured** - :permanent/:transient/:transient set appropriately
- [ ] **Process registration strategy** - Named vs Registry based on cardinality
- [ ] **Resource cleanup in terminate/2** - Database connections, file handles released
- [ ] **Async initialization for expensive operations** - Use {:continue, :init} pattern
- [ ] **State immutability enforced** - No mutations, only new state returned
- [ ] **Error handling strategy clear** - Let it crash vs explicit handling
- [ ] **Tests cover supervision behavior** - Restart verification
- [ ] **Tests cover error cases** - Invalid input, business errors
- [ ] **Logging and observability** - Track process lifecycle events

## Trade-Offs: Raw GenServer vs Abstractions

| Aspect         | Raw GenServer                     | Higher Abstractions (Agent, Task) |
| -------------- | --------------------------------- | --------------------------------- |
| Control        | Full control over callbacks       | Limited, simplified API           |
| Learning curve | Steeper (init, handle_call, etc.) | Gentler (get, update)             |
| Use case       | Complex state machines, lifecycle | Simple state, fire-and-forget     |
| Debugging      | More callbacks to trace           | Simpler, fewer moving parts       |
| Performance    | Optimal (no indirection)          | Slight overhead                   |

**Recommendation**: Use GenServer for stateful services requiring lifecycle control. Use Agent for simple state wrappers. Use Task for async operations without state.

## Next Steps

- **[Anti Patterns](/en/learn/software-engineering/programming-languages/elixir/in-the-field/anti-patterns)** - Learn common mistakes to avoid
- **[Processes and Message Passing](/en/learn/software-engineering/programming-languages/elixir/in-the-field/processes-and-message-passing)** - Deep dive into BEAM process model
- **[Supervisor Trees](/en/learn/software-engineering/programming-languages/elixir/in-the-field/supervisor-trees)** - Advanced supervision patterns
- **[Testing Strategies](/en/learn/software-engineering/programming-languages/elixir/in-the-field/testing-strategies)** - Comprehensive testing approaches

## References

- [Elixir Official Guide - Supervisor and Application](https://elixir-lang.org/getting-started/mix-otp/supervisor-and-application.html)
- [Elixir School - OTP Supervisors](https://elixirschool.com/en/lessons/advanced/otp_supervisors)
- [Designing for Scalability with Erlang/OTP](https://www.oreilly.com/library/view/designing-for-scalability/9781449361556/)
