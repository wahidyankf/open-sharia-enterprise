---
title: "GenServer Patterns"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000004
description: "GenServer design patterns for production state management in Elixir"
tags: ["elixir", "genserver", "otp", "state-management", "concurrency"]
prev: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/processes-and-message-passing"
next: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/supervisor-trees"
---

**Building stateful systems in Elixir?** This guide teaches GenServer patterns through the OTP-First progression, starting with manual process primitives to understand state management challenges before introducing GenServer abstractions.

## Why GenServer Matters

Most production systems need stateful components:

- **Web servers** - Session storage, connection pools, cache management
- **Background workers** - Job queues, rate limiters, metrics collectors
- **Real-time systems** - Live data feeds, notification dispatchers, game servers
- **Domain logic** - Contract state machines, workflow engines, business processes

Elixir provides two approaches:

1. **Manual processes** - `spawn_link`, `send`, `receive` primitives (maximum control)
2. **GenServer behavior** - OTP-compliant generic server (production standard)

**Our approach**: Build with manual processes first to understand state management challenges, then see how GenServer solves them systematically.

## OTP Primitives - Manual State Management

### Basic Manual Process

Let's build a counter using raw process primitives:

```elixir
# Manual counter using spawn_link and receive
defmodule ManualCounter do
  # => Client API
  def start_link(initial \\ 0) do
    pid = spawn_link(__MODULE__, :init, [initial])
                                             # => Spawns linked process
                                             # => initial: Starting count
                                             # => Returns: pid
    {:ok, pid}                               # => Wraps in OTP-style tuple
  end

  def increment(pid) do
    send(pid, {:increment, self()})          # => Sends message to process
                                             # => self(): Reply-to address
    receive do
      {:reply, value} -> value               # => Waits for response
                                             # => Returns: new value
    after
      5000 -> {:error, :timeout}             # => 5 second timeout
    end
  end

  def get(pid) do
    send(pid, {:get, self()})                # => Request current value
    receive do
      {:reply, value} -> value               # => Returns: current value
    after
      5000 -> {:error, :timeout}
    end
  end

  # => Server implementation
  def init(initial) do
    loop(initial)                            # => Enters message loop
                                             # => initial: Starting state
  end

  defp loop(state) do
    receive do
      {:increment, caller} ->
        new_state = state + 1                # => Increment counter
        send(caller, {:reply, new_state})    # => Send response
        loop(new_state)                      # => Recurse with new state
                                             # => Tail call optimized

      {:get, caller} ->
        send(caller, {:reply, state})        # => Send current value
        loop(state)                          # => Recurse with same state
    end
  end
end
```

**Usage**:

```elixir
{:ok, pid} = ManualCounter.start_link(0)     # => Start counter at 0
                                             # => pid: Process identifier

ManualCounter.increment(pid)                 # => Returns: 1
ManualCounter.increment(pid)                 # => Returns: 2
ManualCounter.get(pid)                       # => Returns: 2
```

### Limitations of Manual Processes

This manual approach has serious production issues:

**1. No OTP Compliance**

```elixir
# Manual process doesn't follow OTP conventions
{:ok, pid} = ManualCounter.start_link(0)
# => Returns {:ok, pid}
# => But supervisor expects specific startup protocol
# => Missing: System messages handling
# => Missing: Debug info support
# => Missing: Hot code upgrade support
```

**2. Message Handling Boilerplate**

```elixir
# Every operation needs:
# 1. Send message
# 2. Receive response
# 3. Handle timeout
# 4. Pattern match reply

# Verbose and error-prone
def get_multiple(pid, count) do
  Enum.map(1..count, fn _ ->
    send(pid, {:get, self()})
    receive do
      {:reply, value} -> value
    after
      5000 -> {:error, :timeout}
    end
  end)
end
# => Repetitive timeout logic
# => No shared infrastructure
```

**3. No State Lifecycle Management**

```elixir
# Missing lifecycle hooks:
# - Initialization validation
# - Cleanup on termination
# - State persistence
# - Graceful shutdown

defp loop(state) do
  receive do
    {:stop, caller} ->
      send(caller, {:reply, :ok})
      # => Process exits
      # => No cleanup callback
      # => No resource release
      # => State lost immediately
  end
end
```

**4. Complex Synchronous Operations**

```elixir
# Implementing call/cast distinction manually:
defp loop(state) do
  receive do
    {:call, from, msg} ->
      # => Synchronous: Must reply
      {reply, new_state} = handle_call(msg, state)
      send(from, {:reply, reply})
      loop(new_state)

    {:cast, msg} ->
      # => Asynchronous: No reply
      new_state = handle_cast(msg, state)
      loop(new_state)
      # => Duplicates GenServer logic
  end
end
```

**5. No Built-in Timeout Handling**

```elixir
# Server-side timeouts require manual tracking:
defp loop(state) do
  receive do
    {:hibernate, caller} ->
      send(caller, {:reply, :ok})
      # => Want to hibernate after inactivity
      # => No built-in mechanism
      # => Must implement timer logic
  after
    60_000 ->
      # => After 60s of inactivity
      # => Manual hibernate logic
      :erlang.hibernate(__MODULE__, :loop, [state])
  end
end
```

### Production Disaster Scenarios

**Scenario 1: Process Leak**

```elixir
# Starting 1000 counters without supervision
pids = Enum.map(1..1000, fn i ->
  {:ok, pid} = ManualCounter.start_link(i)
  pid
end)
# => 1000 processes created
# => No supervision tree
# => If one crashes, no restart
# => If parent crashes, all orphaned
# => Memory leak potential
```

**Scenario 2: Message Queue Overflow**

```elixir
# Rapid message sending without backpressure
{:ok, pid} = ManualCounter.start_link(0)

Enum.each(1..1_000_000, fn _ ->
  send(pid, {:increment, self()})            # => Fire and forget
                                             # => Message queue grows
                                             # => No flow control
end)
# => Process mailbox fills up
# => Memory exhaustion
# => System crash
```

**Scenario 3: Graceless Shutdown**

```elixir
# Process exits without cleanup
defmodule DatabaseConnection do
  def loop(conn) do
    receive do
      {:query, caller, sql} ->
        result = :db.query(conn, sql)        # => External resource
        send(caller, {:reply, result})
        loop(conn)
    end
  end
end
# => Process killed by supervisor
# => Connection never closed
# => Database connection leak
```

## GenServer - Production State Management

### Basic GenServer Counter

GenServer provides a battle-tested abstraction for stateful processes:

```elixir
# Production-ready counter with GenServer
defmodule Counter do
  use GenServer                              # => Imports GenServer behavior
                                             # => Provides: init, handle_call, etc.

  # => Client API (runs in caller's process)
  def start_link(initial \\ 0) do
    GenServer.start_link(__MODULE__, initial, name: __MODULE__)
                                             # => Starts GenServer process
                                             # => __MODULE__: Callback module
                                             # => initial: Init argument
                                             # => name: Registered process name
                                             # => Returns: {:ok, pid} or {:error, reason}
  end

  def increment do
    GenServer.call(__MODULE__, :increment)   # => Synchronous call
                                             # => Waits for reply
                                             # => Default timeout: 5000ms
                                             # => Returns: new value
  end

  def get do
    GenServer.call(__MODULE__, :get)         # => Synchronous call
                                             # => Returns: current value
  end

  # => Server callbacks (runs in GenServer process)
  @impl true
  def init(initial) do
    {:ok, initial}                           # => Initial state: initial value
                                             # => Returns: {:ok, state}
  end

  @impl true
  def handle_call(:increment, _from, state) do
    new_state = state + 1                    # => Increment counter
    {:reply, new_state, new_state}           # => Reply with new value
                                             # => Update state to new_state
                                             # => Type: {:reply, reply, new_state}
  end

  @impl true
  def handle_call(:get, _from, state) do
    {:reply, state, state}                   # => Reply with current value
                                             # => State unchanged
  end
end
```

**Usage**:

```elixir
{:ok, _pid} = Counter.start_link(0)          # => Start counter, registered name
                                             # => Can call by name, not pid

Counter.increment()                          # => Returns: 1
Counter.increment()                          # => Returns: 2
Counter.get()                                # => Returns: 2
```

### GenServer Benefits Over Manual Processes

**1. OTP Compliance**

GenServer automatically handles:

- Supervisor startup protocol
- System message handling (suspend, resume, code change)
- Debug information (sys module integration)
- Hot code upgrade support

**2. Simplified API**

```elixir
# Manual process (verbose)
send(pid, {:get, self()})                    # => Send message to process
                                             # => pid: Target process identifier
                                             # => {:get, self()}: Message with return address
receive do                                   # => Block and wait for response
                                             # => Pattern match incoming messages
  {:reply, value} -> value                   # => Match reply tuple
                                             # => Extract and return value
after                                        # => Timeout clause
  5000 -> {:error, :timeout}                 # => 5 second maximum wait
                                             # => Returns error if no reply
end                                          # => Type: value | {:error, :timeout}
                                             # => 9 lines of boilerplate per operation

# GenServer (concise)
GenServer.call(pid, :get)                    # => Single line replaces 9 lines above
                                             # => All boilerplate hidden
                                             # => Timeout handled automatically
                                             # => Type-safe reply guaranteed
                                             # => Default 5s timeout
```

**3. Built-in Lifecycle Hooks**

````elixir
@impl true                                   # => Marks GenServer callback implementation
                                             # => Compiler verifies function signature
def init(initial) do                         # => Called when GenServer starts
                                             # => initial: Argument from start_link
                                             # => Runs in GenServer process
  # => Initialization logic                 # => Validate initial state
  # => Validate state                       # => Setup external resources (connections, files)
  # => Setup resources                      # => Register names or subscriptions
  {:ok, initial}                             # => Success tuple
                                             # => initial: Becomes process state
                                             # => Type: {:ok, state} | {:stop, reason}
end

@impl true                                   # => Terminate callback implementation
                                             # => Called on graceful shutdown
def terminate(reason, state) do              # => reason: Why process is stopping
                                             # => state: Current process state
                                             # => Runs before process exits
  # => Cleanup on shutdown                  # => Close database connections
  # => Release resources                    # => Cancel timers or subscriptions
  # => Persist state                        # => Save state to disk/database
  :ok                                        # => Return value ignored
                                             # => Process exits after this function
end

**4. Clear Synchronous/Asynchronous Distinction**

```elixir
# Synchronous: handle_call (waits for reply)
@impl true                                   # => GenServer callback implementation
                                             # => Required for handle_call pattern
def handle_call(:get, _from, state) do       # => :get: Message pattern to match
                                             # => _from: Caller PID (unused here)
                                             # => state: Current process state
                                             # => Runs in GenServer process
  {:reply, state, state}                     # => Tuple: {:reply, reply_value, new_state}
                                             # => First state: Value sent to caller
                                             # => Second state: Updated process state
                                             # => Caller blocks until reply
                                             # => Type: {:reply, term(), term()}
end

# Asynchronous: handle_cast (fire and forget)
@impl true                                   # => Callback for async messages
                                             # => No reply expected
def handle_cast(:reset, _state) do           # => :reset: Message pattern
                                             # => _state: Current state (ignored)
                                             # => Runs in GenServer process
  {:noreply, 0}                              # => Tuple: {:noreply, new_state}
                                             # => No reply sent
                                             # => 0: Reset state to zero
                                             # => Caller continues immediately
                                             # => Type: {:noreply, term()}
end
````

**5. Built-in Timeout Support**

```elixir
# Server-side timeouts
@impl true                                   # => GenServer callback
                                             # => Handle synchronous call
def handle_call(:long_operation, _from, state) do
                                             # => :long_operation: Message pattern
                                             # => _from: Caller PID (unused)
                                             # => state: Current GenServer state
  result = expensive_computation()           # => Expensive blocking operation
                                             # => result: Computation result
                                             # => Type depends on computation
  {:reply, result, state, 10_000}            # => Four-element reply tuple
                                             # => result: Value sent to caller
                                             # => state: Process state unchanged
                                             # => 10_000: Hibernate after 10s idle
                                             # => Reduces memory if no messages
                                             # => Type: {:reply, term(), term(), timeout()}
end

# Client-side timeouts
GenServer.call(pid, :get, 1000)              # => Synchronous call with custom timeout
                                             # => pid: Target GenServer process
                                             # => :get: Message to send
                                             # => 1000: Wait maximum 1 second
                                             # => Raises if timeout exceeded
                                             # => Default timeout is 5000ms
                                             # => Type: term() (or raises)
```

## Production Patterns

### Pattern 1: Financial Contract State Machine

Managing Murabaha contract state (Sharia-compliant financing):

```elixir
# Murabaha contract state management
defmodule MurabahaContract do
  use GenServer                              # => GenServer behavior

  # => Contract states:
  # => :pending -> :approved -> :disbursed -> :repaying -> :completed
  # => :pending -> :rejected

  defstruct [
    :contract_id,                            # => UUID
    :customer_id,                            # => Customer reference
    :asset_cost,                             # => Original asset cost
    :profit_amount,                          # => Profit (markup)
    :total_amount,                           # => Total owed
    :state,                                  # => Current state
    :approved_at,                            # => Approval timestamp
    :disbursed_at,                           # => Disbursement timestamp
    :payments                                # => List of payments
  ]

  # => Client API
  def start_link(contract_id, customer_id, asset_cost, profit_amount) do
    initial = %__MODULE__{
      contract_id: contract_id,
      customer_id: customer_id,
      asset_cost: asset_cost,
      profit_amount: profit_amount,
      total_amount: asset_cost + profit_amount,
      state: :pending,
      payments: []
    }
    GenServer.start_link(__MODULE__, initial, name: via_tuple(contract_id))
                                             # => Registers via Registry
                                             # => Each contract = separate process
  end

  def approve(contract_id) do
    GenServer.call(via_tuple(contract_id), :approve)
                                             # => Synchronous state transition
                                             # => Returns: {:ok, contract} or {:error, reason}
  end

  def disburse(contract_id) do
    GenServer.call(via_tuple(contract_id), :disburse)
                                             # => Disburse funds to customer
  end

  def record_payment(contract_id, amount) do
    GenServer.call(via_tuple(contract_id), {:record_payment, amount})
                                             # => Record payment, update state
  end

  def get_state(contract_id) do
    GenServer.call(via_tuple(contract_id), :get_state)
                                             # => Returns: current contract state
  end

  # => Server callbacks
  @impl true
  def init(contract) do
    # => Optional: Persist initial state to database
    {:ok, contract}                          # => Initial state: pending contract
  end

  @impl true
  def handle_call(:approve, _from, %{state: :pending} = contract) do
    new_contract = %{contract |
      state: :approved,
      approved_at: DateTime.utc_now()
    }
    # => TODO: Persist to database
    {:reply, {:ok, new_contract}, new_contract}
                                             # => State transition: pending -> approved
  end

  @impl true
  def handle_call(:approve, _from, contract) do
    {:reply, {:error, :invalid_state}, contract}
                                             # => Can only approve pending contracts
                                             # => State unchanged
  end

  @impl true
  def handle_call(:disburse, _from, %{state: :approved} = contract) do
    # => Disburse funds (call external payment system)
    case disburse_funds(contract) do
      :ok ->
        new_contract = %{contract |
          state: :disbursed,
          disbursed_at: DateTime.utc_now()
        }
        {:reply, {:ok, new_contract}, new_contract}
                                             # => State transition: approved -> disbursed

      {:error, reason} ->
        {:reply, {:error, reason}, contract}
                                             # => Disbursement failed, state unchanged
    end
  end

  @impl true
  def handle_call(:disburse, _from, contract) do
    {:reply, {:error, :invalid_state}, contract}
                                             # => Can only disburse approved contracts
  end

  @impl true
  def handle_call({:record_payment, amount}, _from, %{state: state} = contract)
      when state in [:disbursed, :repaying] do
    new_payments = [%{amount: amount, timestamp: DateTime.utc_now()} | contract.payments]
                                             # => Add payment to list
    total_paid = Enum.sum(Enum.map(new_payments, & &1.amount))
                                             # => Calculate total paid

    new_state = if total_paid >= contract.total_amount do
      :completed                             # => Fully paid
    else
      :repaying                              # => Partial payment
    end

    new_contract = %{contract |
      state: new_state,
      payments: new_payments
    }

    {:reply, {:ok, new_contract}, new_contract}
                                             # => Reply with updated contract
                                             # => Update state
  end

  @impl true
  def handle_call({:record_payment, _amount}, _from, contract) do
    {:reply, {:error, :invalid_state}, contract}
                                             # => Can only record payment for disbursed/repaying
  end

  @impl true
  def handle_call(:get_state, _from, contract) do
    {:reply, contract, contract}             # => Return current state
                                             # => State unchanged
  end

  @impl true
  def terminate(reason, contract) do
    # => Cleanup on shutdown
    IO.puts("Contract #{contract.contract_id} terminating: #{inspect(reason)}")
    # => TODO: Persist final state to database
    :ok
  end

  # => Helper functions
  defp via_tuple(contract_id) do
    {:via, Registry, {MurabahaRegistry, contract_id}}
                                             # => Named process via Registry
                                             # => Enables lookup by contract_id
  end

  defp disburse_funds(_contract) do
    # => TODO: Call external payment system
    :ok
  end
end
```

**Usage**:

```elixir
# Setup Registry
{:ok, _} = Registry.start_link(keys: :unique, name: MurabahaRegistry)

# Create contract
{:ok, _pid} = MurabahaContract.start_link(
  "contract-123",                            # => contract_id
  "customer-456",                            # => customer_id
  100_000,                                   # => asset_cost (100k)
  10_000                                     # => profit_amount (10k markup)
)
# => Contract created in :pending state

# Approve contract
{:ok, contract} = MurabahaContract.approve("contract-123")
# => State: :pending -> :approved
# => contract.state: :approved
# => contract.approved_at: timestamp

# Disburse funds
{:ok, contract} = MurabahaContract.disburse("contract-123")
# => State: :approved -> :disbursed
# => Funds transferred to customer

# Record payments
{:ok, contract} = MurabahaContract.record_payment("contract-123", 50_000)
# => State: :disbursed -> :repaying
# => Payment recorded: 50k

{:ok, contract} = MurabahaContract.record_payment("contract-123", 60_000)
# => State: :repaying -> :completed
# => Total paid: 110k (>= 110k required)
# => Contract completed
```

### Pattern 2: Asynchronous Operations with handle_cast

When reply not needed, use `handle_cast` for fire-and-forget operations:

```elixir
defmodule NotificationQueue do
  use GenServer

  # => Client API
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def enqueue(notification) do
    GenServer.cast(__MODULE__, {:enqueue, notification})
                                             # => Asynchronous, no reply
                                             # => Returns: :ok immediately
  end

  def get_queue do
    GenServer.call(__MODULE__, :get_queue)   # => Synchronous, waits for reply
                                             # => Returns: current queue
  end

  # => Server callbacks
  @impl true
  def init(_opts) do
    {:ok, []}                                # => Initial state: empty list
  end

  @impl true
  def handle_cast({:enqueue, notification}, queue) do
    new_queue = [notification | queue]       # => Add to queue
    {:noreply, new_queue}                    # => No reply, update state
                                             # => Type: {:noreply, new_state}
  end

  @impl true
  def handle_call(:get_queue, _from, queue) do
    {:reply, queue, queue}                   # => Return queue, state unchanged
  end
end
```

**Usage**:

```elixir
{:ok, _pid} = NotificationQueue.start_link([])

NotificationQueue.enqueue("Email notification")
# => Returns: :ok (immediately)
# => Notification queued asynchronously

NotificationQueue.enqueue("SMS notification")
# => Returns: :ok

NotificationQueue.get_queue()
# => Returns: ["SMS notification", "Email notification"]
```

### Pattern 3: Handling Unexpected Messages with handle_info

`handle_info` catches messages not sent via `call` or `cast`:

```elixir
defmodule PeriodicReporter do
  use GenServer

  # => Client API
  def start_link(interval_ms) do
    GenServer.start_link(__MODULE__, interval_ms, name: __MODULE__)
  end

  # => Server callbacks
  @impl true
  def init(interval_ms) do
    schedule_report(interval_ms)             # => Schedule first report
    {:ok, %{interval: interval_ms, count: 0}}
                                             # => Initial state
  end

  @impl true
  def handle_info(:report, state) do
    # => Periodic message from Process.send_after
    IO.puts("Report ##{state.count}: #{DateTime.utc_now()}")
    schedule_report(state.interval)          # => Schedule next report
    {:noreply, %{state | count: state.count + 1}}
                                             # => Update count, continue
  end

  defp schedule_report(interval_ms) do
    Process.send_after(self(), :report, interval_ms)
                                             # => Send :report after interval
                                             # => Returns: timer reference
  end
end
```

**Usage**:

```elixir
{:ok, _pid} = PeriodicReporter.start_link(5000)
# => Reports every 5 seconds
# => Output: Report #0: 2026-02-05 10:00:00Z
# => Output: Report #1: 2026-02-05 10:00:05Z
# => Output: Report #2: 2026-02-05 10:00:10Z
```

### Pattern 4: Graceful Shutdown with terminate

`terminate/2` provides cleanup on process exit:

```elixir
defmodule DatabasePool do
  use GenServer

  # => Client API
  def start_link(config) do
    GenServer.start_link(__MODULE__, config, name: __MODULE__)
  end

  # => Server callbacks
  @impl true
  def init(config) do
    # => Open database connections
    connections = Enum.map(1..config.pool_size, fn _ ->
      {:ok, conn} = :db.connect(config.url)
      conn
    end)
    # => connections: List of connection handles

    {:ok, %{config: config, connections: connections}}
                                             # => Initial state: pool
  end

  @impl true
  def terminate(reason, state) do
    # => Called on shutdown
    IO.puts("Shutting down pool: #{inspect(reason)}")

    Enum.each(state.connections, fn conn ->
      :db.close(conn)                        # => Close each connection
    end)
    # => All resources released

    :ok
  end

  # => ... handle_call/handle_cast implementations ...
end
```

**Shutdown scenarios**:

```elixir
{:ok, pid} = DatabasePool.start_link(%{pool_size: 10, url: "db://..."})
# => 10 connections opened

# Graceful shutdown
GenServer.stop(pid)
# => Calls terminate(:normal, state)
# => All connections closed
# => Returns: :ok

# Supervisor kills process
Process.exit(pid, :shutdown)
# => Calls terminate(:shutdown, state)
# => All connections closed
```

### Pattern 5: Timeout and Hibernation

Control process lifecycle with timeouts:

```elixir
defmodule CacheServer do
  use GenServer

  # => Client API
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  def put(key, value) do
    GenServer.cast(__MODULE__, {:put, key, value})
  end

  def get(key) do
    GenServer.call(__MODULE__, {:get, key})
  end

  # => Server callbacks
  @impl true                                 # => GenServer init callback
                                             # => Called on start_link
  def init(_opts) do                         # => _opts: Initialization arguments (unused)
                                             # => Runs once at startup
    {:ok, %{}, 60_000}                       # => Three-element tuple
                                             # => :ok: Successful initialization
                                             # => %{}: Empty map as initial state
                                             # => 60_000: Hibernate after 60s idle
                                             # => If no messages for 60s, timeout
                                             # => Type: {:ok, state(), timeout()}
  end

  @impl true                                 # => Async message handler
                                             # => No reply expected
  def handle_cast({:put, key, value}, cache) do
                                             # => {:put, key, value}: Message pattern
                                             # => cache: Current cache state (map)
                                             # => Runs in GenServer process
    new_cache = Map.put(cache, key, value)   # => Add key-value pair to map
                                             # => new_cache: Updated cache map
                                             # => Immutable update (new map created)
                                             # => Type: map()
    {:noreply, new_cache, 60_000}            # => Three-element tuple
                                             # => :noreply: No reply sent
                                             # => new_cache: Updated state
                                             # => 60_000: Reset 60s timeout
                                             # => Activity detected
                                             # => Type: {:noreply, state(), timeout()}
  end

  @impl true                                 # => Sync message handler
                                             # => Caller waits for reply
  def handle_call({:get, key}, _from, cache) do
                                             # => {:get, key}: Message pattern with key
                                             # => _from: Caller PID (unused)
                                             # => cache: Current cache state
    {:reply, Map.get(cache, key), cache, 60_000}
                                             # => Four-element tuple
                                             # => Map.get(cache, key): Value or nil
                                             # => cache: State unchanged
                                             # => 60_000: Reset timeout on read
                                             # => Type: {:reply, term(), state(), timeout()}
  end

  @impl true                                 # => Handle info messages
                                             # => For non-call/cast messages
  def handle_info(:timeout, cache) do        # => :timeout: Sent after idle period
                                             # => cache: Current state
                                             # => Triggered by 60s idle
    # => 60s of inactivity                   # => No messages received
    IO.puts("Cache idle, hibernating...")     # => Log hibernation event
                                             # => Output to console
    {:noreply, cache, :hibernate}            # => Three-element tuple
                                             # => cache: State preserved
                                             # => :hibernate: Garbage collect, minimize memory
                                             # => Wakes on next message
                                             # => Type: {:noreply, state(), :hibernate}
  end
end
```

## Trade-offs: Manual vs GenServer

| Aspect                   | Manual Processes               | GenServer                     |
| ------------------------ | ------------------------------ | ----------------------------- |
| **Complexity**           | Simple concepts, verbose code  | More concepts, concise code   |
| **OTP Compliance**       | Manual implementation required | Built-in                      |
| **Supervision Support**  | Limited                        | Full OTP integration          |
| **Boilerplate**          | High (send/receive everywhere) | Low (behavior abstracts)      |
| **Message Handling**     | Manual pattern matching        | Structured callbacks          |
| **Lifecycle Management** | Manual hooks                   | Built-in init/terminate       |
| **Timeout Support**      | Manual timer logic             | Built-in timeout parameter    |
| **Debug Support**        | Custom implementation          | sys module integration        |
| **Hot Code Upgrade**     | Not supported                  | Supported via code_change     |
| **Learning Curve**       | Lower (basic primitives)       | Higher (behavior conventions) |
| **Production Readiness** | Requires extensive validation  | Production-tested abstraction |
| **Recommended Use**      | Learning, prototyping          | Production systems            |

**Recommendation**: Use GenServer for all production stateful processes. Manual processes are valuable for learning BEAM fundamentals but require too much careful work to make production-ready.

## Best Practices

### 1. Separate Client and Server Code

```elixir
# Good: Clear separation
defmodule Counter do
  use GenServer

  # => Client API (runs in caller's process)
  def start_link(initial), do: GenServer.start_link(__MODULE__, initial, name: __MODULE__)
  def increment, do: GenServer.call(__MODULE__, :increment)
  def get, do: GenServer.call(__MODULE__, :get)

  # => Server callbacks (runs in GenServer process)
  @impl true
  def init(initial), do: {:ok, initial}

  @impl true
  def handle_call(:increment, _from, state), do: {:reply, state + 1, state + 1}

  @impl true
  def handle_call(:get, _from, state), do: {:reply, state, state}
end
```

### 2. Use @impl Attribute

Mark callback implementations explicitly:

```elixir
defmodule MyGenServer do
  use GenServer

  @impl true                                 # => Marks as behavior callback
  def init(_opts) do                         # => Compiler verifies signature
    {:ok, %{}}                               # => Warns if typo or wrong arity
  end

  @impl true
  def handle_call(:get, _from, state) do
    {:reply, state, state}
  end
end
```

### 3. Name Processes for Easy Access

```elixir
# Bad: Passing pids everywhere
{:ok, pid} = Counter.start_link(0)
Counter.increment(pid)

# Good: Named process
def start_link(initial) do
  GenServer.start_link(__MODULE__, initial, name: __MODULE__)
                                             # => Registered name
end

Counter.increment()                          # => No pid needed
```

### 4. Use Registry for Dynamic Processes

```elixir
# Multiple processes of same type
defmodule SessionManager do
  def start_link(session_id) do
    GenServer.start_link(__MODULE__, session_id,
      name: via_tuple(session_id))           # => Dynamic naming
  end

  defp via_tuple(session_id) do
    {:via, Registry, {SessionRegistry, session_id}}
                                             # => Registry-based lookup
  end
end

# Usage
{:ok, _} = Registry.start_link(keys: :unique, name: SessionRegistry)
SessionManager.start_link("session-123")
SessionManager.start_link("session-456")
```

### 5. Handle All States Explicitly

```elixir
# Bad: Missing state handling
def handle_call(:approve, _from, contract) do
  {:reply, :ok, %{contract | state: :approved}}
end
# => Allows approve from any state

# Good: Explicit state guards
def handle_call(:approve, _from, %{state: :pending} = contract) do
  {:reply, :ok, %{contract | state: :approved}}
end

def handle_call(:approve, _from, contract) do
  {:reply, {:error, :invalid_state}, contract}
end
# => Only approve from :pending state
```

### 6. Use Timeouts for Long Operations

```elixir
# Client-side timeout
GenServer.call(pid, :expensive_operation, 30_000)
                                             # => 30s timeout
                                             # => Prevents indefinite blocking

# Server-side hibernate timeout
def handle_call(:get, _from, state) do
  {:reply, state, state, 60_000}             # => Hibernate after 60s idle
end
```

### 7. Implement terminate for Cleanup

```elixir
@impl true
def terminate(reason, state) do
  # => Release external resources
  close_connections(state.connections)
  cleanup_files(state.temp_files)
  persist_state(state)                       # => Save state before exit
  :ok
end
```

## When to Use GenServer

**Use GenServer when**:

- Managing mutable state (counters, caches, connections)
- Building stateful services (session managers, workers)
- Implementing state machines (workflows, contracts)
- Need OTP supervision integration
- Require structured lifecycle management

**Consider alternatives when**:

- **Task** - For one-off computations without state
- **Agent** - For simple get/update state (GenServer wrapper)
- **GenStage** - For backpressure-aware data pipelines
- **Registry** - For process lookup without state

## Next Steps

**Completed**: GenServer patterns for state management

**Continue learning**:

- [Supervisor Trees](/en/learn/software-engineering/programming-languages/elixir/in-the-field/supervisor-trees) - Fault tolerance and process supervision
- [Application Structure](/en/learn/software-engineering/programming-languages/elixir/in-the-field/application-structure) - Application behavior and lifecycle
- [OTP Behaviors](/en/learn/software-engineering/programming-languages/elixir/in-the-field/otp-behaviors) - GenServer, GenStage, Task patterns

**Foundation knowledge**:

- [Processes and Message Passing](/en/learn/software-engineering/programming-languages/elixir/in-the-field/processes-and-message-passing) - Process primitives and structured patterns

**Quick reference**:

- [Overview](/en/learn/software-engineering/programming-languages/elixir/in-the-field/overview) - All 36 In-the-Field guides

---

**Summary**: GenServer provides production-ready state management through standardized callbacks, OTP compliance, and built-in lifecycle support. Start with manual processes to understand state management challenges, then adopt GenServer for production systems requiring supervision, timeouts, and graceful shutdown.
