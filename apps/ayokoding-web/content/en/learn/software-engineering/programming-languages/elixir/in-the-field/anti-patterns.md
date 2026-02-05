---
title: "Anti Patterns"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000002
description: "Common Elixir anti-patterns in production systems with OTP-first corrections"
tags: ["elixir", "anti-patterns", "production", "otp", "genserver", "supervision"]
prev: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/best-practices"
next: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/processes-and-message-passing"
---

**Building production Elixir systems?** Avoid these critical anti-patterns that lead to process leaks, supervision failures, and system instability.

This guide identifies anti-patterns through the **OTP-First** lens, showing violations of BEAM principles and their correct OTP implementations.

## Why Anti-Patterns Matter

Production Elixir failures often stem from:

- **Process lifecycle mismanagement** - Leaks, orphaned processes
- **Supervision violations** - Incorrect strategies, missing supervision
- **Stateful design errors** - God objects, improper state management
- **Message queue overflow** - Unbounded queues, no backpressure
- **Resource exhaustion** - Connection pooling failures, file handle leaks

**Impact**: System instability, unpredictable failures, difficult debugging

**Solution**: Recognize anti-patterns early, apply OTP principles correctly

## GenServer Anti-Patterns

### 1. God Object GenServer

**FAIL - Monolithic state manager handling unrelated concerns**:

```elixir
# ANTI-PATTERN: God Object GenServer
defmodule SystemManager do
  use GenServer                              # => Handles too many concerns

  def init(_opts) do
    state = %{
      users: %{},                            # => User management
      connections: %{},                      # => Connection pooling
      cache: %{},                            # => Caching layer
      metrics: %{},                          # => Metrics collection
      config: %{}                            # => Configuration management
    }                                        # => Single point of failure
                                             # => Impossible to supervise granularly
    {:ok, state}                             # => Type: {:ok, map()}
  end

  def handle_call({:add_user, user}, _from, state) do
    # User management logic                 # => Mixing concerns
  end                                        # => Violates single responsibility

  def handle_call({:get_cached, key}, _from, state) do
    # Cache retrieval logic                 # => Unrelated to users
  end                                        # => Same process handles everything

  def handle_call({:record_metric, metric}, _from, state) do
    # Metrics recording                     # => Third unrelated concern
  end                                        # => Process becomes bottleneck
end
```

**Why It Fails**:

1. **Single point of failure** - One crash loses all state
2. **Performance bottleneck** - All calls serialized through one process
3. **Supervision complexity** - Cannot restart individual subsystems
4. **Debugging difficulty** - Hard to isolate issues
5. **Testing complexity** - Cannot test concerns in isolation

**CORRECT - Separate GenServers with focused responsibilities**:

```elixir
# Focused GenServer for user management
defmodule UserManager do
  use GenServer                              # => Single responsibility: users

  def init(_opts) do
    {:ok, %{users: %{}}}                     # => Only user-related state
                                             # => Type: {:ok, %{users: map()}}
  end

  def handle_call({:add_user, user}, _from, state) do
    users = Map.put(state.users, user.id, user)
                                             # => Add user to state
    {:reply, :ok, %{state | users: users}}   # => Reply success, update state
                                             # => Type: {:reply, :ok, map()}
  end

  def handle_call({:get_user, id}, _from, state) do
    user = Map.get(state.users, id)          # => Retrieve user
    {:reply, user, state}                    # => Reply with user, state unchanged
                                             # => Type: {:reply, user | nil, map()}
  end
end

# Focused GenServer for connection pooling
defmodule ConnectionPool do
  use GenServer                              # => Single responsibility: connections

  def init(opts) do
    max_connections = Keyword.get(opts, :max, 10)
                                             # => Get max connections config
    {:ok, %{connections: [], max: max_connections}}
                                             # => Initialize connection pool state
                                             # => Type: {:ok, %{connections: list(), max: integer()}}
  end

  def handle_call(:checkout, _from, state) do
    case state.connections do
      [conn | rest] ->                       # => Connection available
        {:reply, {:ok, conn}, %{state | connections: rest}}
                                             # => Return connection, update pool

      [] ->                                  # => No connections available
        {:reply, {:error, :no_connections}, state}
                                             # => Return error, state unchanged
    end                                      # => Type: {:reply, result, map()}
  end
end

# Focused GenServer for caching
defmodule CacheServer do
  use GenServer                              # => Single responsibility: caching

  def init(opts) do
    ttl = Keyword.get(opts, :ttl, 60_000)    # => Get TTL config (default 60s)
    {:ok, %{cache: %{}, ttl: ttl}}           # => Initialize cache state
                                             # => Type: {:ok, %{cache: map(), ttl: integer()}}
  end

  def handle_call({:get, key}, _from, state) do
    case Map.get(state.cache, key) do
      {value, timestamp} ->                  # => Cache entry found
        if System.monotonic_time(:millisecond) - timestamp < state.ttl do
          {:reply, {:ok, value}, state}      # => Entry valid, return value
        else
          {:reply, :not_found, state}        # => Entry expired
        end

      nil ->                                 # => Key not in cache
        {:reply, :not_found, state}          # => Return not found
    end                                      # => Type: {:reply, result, map()}
  end

  def handle_call({:put, key, value}, _from, state) do
    timestamp = System.monotonic_time(:millisecond)
                                             # => Get current timestamp
    cache = Map.put(state.cache, key, {value, timestamp})
                                             # => Store value with timestamp
    {:reply, :ok, %{state | cache: cache}}   # => Reply success, update cache
                                             # => Type: {:reply, :ok, map()}
  end
end

# Supervision tree for independent subsystems
defmodule SystemSupervisor do
  use Supervisor                             # => Supervises separate concerns

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, opts, name: __MODULE__)
                                             # => Start supervisor
                                             # => Type: {:ok, pid()}
  end

  def init(_opts) do
    children = [
      {UserManager, []},                     # => User management process
      {ConnectionPool, [max: 10]},           # => Connection pool process
      {CacheServer, [ttl: 60_000]}           # => Cache process
    ]                                        # => Each can fail/restart independently

    Supervisor.init(children, strategy: :one_for_one)
                                             # => If one crashes, restart only that one
                                             # => Type: {:ok, supervisor_spec()}
  end
end
```

**Benefits**:

- Each GenServer focused on single responsibility
- Independent supervision and restart
- Parallel processing (no single bottleneck)
- Easier testing and debugging
- Granular resource management

### 2. Stateless GenServer

**FAIL - Using GenServer for stateless operations**:

```elixir
# ANTI-PATTERN: GenServer with no state management
defmodule MathServer do
  use GenServer                              # => Unnecessary GenServer

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
                                             # => Creates process for no reason
                                             # => Type: {:ok, pid()}
  end

  def init(_opts) do
    {:ok, %{}}                               # => Empty state (never used)
                                             # => Type: {:ok, map()}
  end

  def add(a, b) do
    GenServer.call(__MODULE__, {:add, a, b})
                                             # => Synchronous call overhead
                                             # => Serializes all additions
                                             # => Type: integer()
  end

  def multiply(a, b) do
    GenServer.call(__MODULE__, {:multiply, a, b})
                                             # => Another synchronous call
                                             # => Unnecessary process communication
  end

  def handle_call({:add, a, b}, _from, state) do
    result = a + b                           # => Pure calculation (no state needed)
    {:reply, result, state}                  # => State unchanged
                                             # => Type: {:reply, integer(), map()}
  end

  def handle_call({:multiply, a, b}, _from, state) do
    result = a * b                           # => Pure calculation
    {:reply, result, state}                  # => State never modified
  end
end
```

**Why It Fails**:

1. **Unnecessary process overhead** - GenServer adds latency for pure functions
2. **Performance bottleneck** - Serializes concurrent operations
3. **Resource waste** - Process created for stateless operations
4. **Complexity overhead** - GenServer boilerplate for simple functions

**CORRECT - Use plain modules for stateless operations**:

```elixir
# Plain module for stateless operations
defmodule Math do
  @doc """
  Adds two numbers.
  """
  def add(a, b) do
    a + b                                    # => Direct calculation
                                             # => No process overhead
                                             # => Type: integer()
  end

  @doc """
  Multiplies two numbers.
  """
  def multiply(a, b) do
    a * b                                    # => Direct calculation
                                             # => Fully concurrent (no serialization)
  end

  @doc """
  Calculates compound interest.
  """
  def compound_interest(principal, rate, years) do
    principal * :math.pow(1 + rate, years)   # => Pure calculation
                                             # => No state management needed
                                             # => Type: float()
  end
end

# Usage - no GenServer overhead
result = Math.add(10, 20)                    # => Direct function call
                                             # => result: 30 (type: integer())
product = Math.multiply(5, 6)                # => No process communication
                                             # => product: 30 (type: integer())
interest = Math.compound_interest(1000, 0.05, 10)
                                             # => interest: 1628.89 (type: float())
```

**When GenServer IS appropriate**:

```elixir
# CORRECT - GenServer for stateful operations
defmodule Counter do
  use GenServer                              # => State management needed

  def init(initial) do
    {:ok, %{count: initial, history: []}}    # => Maintains state
                                             # => Type: {:ok, %{count: integer(), history: list()}}
  end

  def handle_call(:increment, _from, state) do
    new_count = state.count + 1              # => Modify state
    history = [new_count | state.history]    # => Track history
    new_state = %{count: new_count, history: history}
                                             # => State updated
    {:reply, new_count, new_state}           # => Reply with new count
                                             # => Type: {:reply, integer(), map()}
  end
end
```

**Decision Rule**: Use GenServer only when managing mutable state, process lifecycle, or coordinating resources.

### 3. Blocking Operations in GenServer

**FAIL - Synchronous blocking in handle_call**:

```elixir
# ANTI-PATTERN: Blocking handle_call
defmodule ApiClient do
  use GenServer                              # => GenServer for API requests

  def handle_call({:fetch_user, user_id}, _from, state) do
    # Blocking HTTP request                 # => Blocks GenServer process
    response = HTTPoison.get("https://api.example.com/users/#{user_id}")
                                             # => All other calls blocked during request
                                             # => GenServer unresponsive for seconds
    case response do
      {:ok, %{status_code: 200, body: body}} ->
        user = Jason.decode!(body)           # => Parse response
        {:reply, {:ok, user}, state}         # => Finally reply

      {:error, reason} ->
        {:reply, {:error, reason}, state}    # => Reply with error
    end                                      # => GenServer blocked entire time
  end
end
```

**Why It Fails**:

1. **GenServer blocks all calls** - Other operations wait during HTTP request
2. **Timeout risk** - Default GenServer timeout is 5000ms
3. **Cascading failures** - Slow external service blocks entire process
4. **No concurrency** - Sequential processing of all requests

**CORRECT - Async operations with Task**:

```elixir
# Delegate blocking work to Task
defmodule ApiClient do
  use GenServer                              # => GenServer for coordination only

  def fetch_user(user_id) do
    GenServer.call(__MODULE__, {:fetch_user, user_id})
                                             # => Immediate response (no blocking)
                                             # => Type: {:ok, Task.t()}
  end

  def handle_call({:fetch_user, user_id}, _from, state) do
    # Start async task                      # => Doesn't block GenServer
    task = Task.async(fn ->
      HTTPoison.get("https://api.example.com/users/#{user_id}")
                                             # => HTTP request in separate process
    end)                                     # => Type: Task.t()

    {:reply, {:ok, task}, state}             # => Reply immediately with task
                                             # => GenServer continues processing
                                             # => Type: {:reply, {:ok, Task.t()}, map()}
  end
end

# Usage - caller handles async task
{:ok, task} = ApiClient.fetch_user(123)      # => Returns immediately
                                             # => task: Task.t()
result = Task.await(task, 10_000)            # => Caller waits for result
                                             # => result: {:ok, response} | {:error, reason}
```

**Better - Async cast with callback**:

```elixir
defmodule ApiClient do
  use GenServer                              # => Non-blocking GenServer

  def fetch_user_async(user_id, callback_pid) do
    GenServer.cast(__MODULE__, {:fetch_user, user_id, callback_pid})
                                             # => Async cast (no reply expected)
                                             # => Type: :ok
  end

  def handle_cast({:fetch_user, user_id, callback_pid}, state) do
    # Spawn task for HTTP request           # => Doesn't block GenServer
    Task.start(fn ->
      result = HTTPoison.get("https://api.example.com/users/#{user_id}")
                                             # => HTTP request in separate process
      send(callback_pid, {:user_fetched, result})
                                             # => Send result to callback process
    end)                                     # => Task runs independently

    {:noreply, state}                        # => GenServer continues immediately
                                             # => Type: {:noreply, map()}
  end
end

# Usage - receive result asynchronously
defmodule UserController do
  def get_user(user_id) do
    ApiClient.fetch_user_async(user_id, self())
                                             # => Async request with callback to self
    receive do
      {:user_fetched, {:ok, response}} ->    # => Receive successful result
        parse_user(response)                 # => Process response

      {:user_fetched, {:error, reason}} ->   # => Receive error result
        handle_error(reason)                 # => Handle error
    after
      10_000 ->                              # => Timeout after 10 seconds
        {:error, :timeout}                   # => Return timeout error
    end
  end
end
```

**Rule**: Never block GenServer with slow operations - delegate to Task, handle asynchronously.

## Supervision Anti-Patterns

### 4. No Supervision

**FAIL - Processes started without supervision**:

```elixir
# ANTI-PATTERN: Manual process start without supervision
defmodule Application do
  use Application                            # => Application behavior

  def start(_type, _args) do
    # Start processes manually              # => No supervision tree
    {:ok, user_pid} = UserManager.start_link([])
                                             # => Process not supervised
                                             # => If crashes, stays dead
    {:ok, cache_pid} = CacheServer.start_link([])
                                             # => Another unsupervised process

    # Store PIDs globally (WRONG)
    :ets.new(:pids, [:named_table, :public])
                                             # => Manual PID tracking
    :ets.insert(:pids, {:user_manager, user_pid})
                                             # => Store PIDs for lookup
    :ets.insert(:pids, {:cache_server, cache_pid})

    {:ok, self()}                            # => Return application PID
                                             # => Processes not part of supervision
  end
end
```

**Why It Fails**:

1. **No automatic restart** - Crashed processes stay dead
2. **Manual recovery required** - No fault tolerance
3. **PID tracking overhead** - Manual bookkeeping required
4. **Debugging difficulty** - No supervision reports
5. **Violates "Let It Crash"** - Must handle failures manually

**CORRECT - Supervision tree for all processes**:

```elixir
defmodule MyApp.Application do
  use Application                            # => Application behavior

  def start(_type, _args) do
    children = [
      {UserManager, []},                     # => Supervised UserManager
      {CacheServer, [ttl: 60_000]},          # => Supervised CacheServer
      {ConnectionPool, [max: 10]}            # => Supervised ConnectionPool
    ]                                        # => All processes supervised

    opts = [strategy: :one_for_one, name: MyApp.Supervisor]
                                             # => one_for_one: restart only failed process
    Supervisor.start_link(children, opts)    # => Start supervision tree
                                             # => Type: {:ok, pid()}
  end
end

# Supervised GenServer with proper name registration
defmodule UserManager do
  use GenServer                              # => GenServer behavior

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
                                             # => Register with module name
                                             # => Supervisor can restart by name
                                             # => Type: {:ok, pid()}
  end

  def init(opts) do
    {:ok, %{users: %{}}}                     # => Initialize state
                                             # => Type: {:ok, map()}
  end
end
```

**Benefits**:

- Automatic restart on crash
- Supervision reports for debugging
- No manual PID tracking (use named processes)
- Follows "Let It Crash" philosophy
- System self-healing

### 5. Wrong Supervision Strategy

**FAIL - Mismatched supervision strategy**:

```elixir
# ANTI-PATTERN: :one_for_all when processes are independent
defmodule DatabaseSupervisor do
  use Supervisor                             # => Supervisor behavior

  def init(_opts) do
    children = [
      {UserRepo, []},                        # => User database connection
      {ProductRepo, []},                     # => Product database connection
      {OrderRepo, []}                        # => Order database connection
    ]                                        # => Independent database pools

    # WRONG: one_for_all strategy
    Supervisor.init(children, strategy: :one_for_all)
                                             # => If UserRepo crashes, restart ALL repos
                                             # => Unnecessary downtime for ProductRepo and OrderRepo
                                             # => Type: {:ok, supervisor_spec()}
  end
end
```

**Why It Fails**:

1. **Cascading restarts** - One failure restarts all independent processes
2. **Unnecessary downtime** - Healthy processes restarted needlessly
3. **Resource waste** - Re-establishing connections for healthy processes
4. **Longer recovery time** - All processes must restart sequentially

**CORRECT - Match strategy to dependency**:

```elixir
# :one_for_one for independent processes
defmodule DatabaseSupervisor do
  use Supervisor                             # => Supervisor behavior

  def init(_opts) do
    children = [
      {UserRepo, []},                        # => Independent user repo
      {ProductRepo, []},                     # => Independent product repo
      {OrderRepo, []}                        # => Independent order repo
    ]                                        # => No dependencies between repos

    # CORRECT: one_for_one strategy
    Supervisor.init(children, strategy: :one_for_one)
                                             # => Restart only failed process
                                             # => Other processes continue running
                                             # => Type: {:ok, supervisor_spec()}
  end
end

# :rest_for_one for dependent processes
defmodule PaymentSupervisor do
  use Supervisor                             # => Supervisor behavior

  def init(_opts) do
    children = [
      {PaymentGateway, []},                  # => Must start first (foundation)
      {TransactionLogger, []},               # => Depends on gateway
      {FraudDetector, []}                    # => Depends on logger
    ]                                        # => Sequential dependency chain

    # CORRECT: rest_for_one strategy
    Supervisor.init(children, strategy: :rest_for_one)
                                             # => If gateway crashes, restart gateway + logger + detector
                                             # => If logger crashes, restart logger + detector only
                                             # => If detector crashes, restart detector only
                                             # => Type: {:ok, supervisor_spec()}
  end
end

# :one_for_all for tightly coupled processes
defmodule ClusterSupervisor do
  use Supervisor                             # => Supervisor behavior

  def init(_opts) do
    children = [
      {ClusterNode1, []},                    # => Cluster node 1
      {ClusterNode2, []},                    # => Cluster node 2
      {ClusterCoordinator, []}               # => Cluster coordinator
    ]                                        # => Tightly coupled (cluster state)

    # CORRECT: one_for_all strategy
    Supervisor.init(children, strategy: :one_for_all)
                                             # => Any crash requires full cluster restart
                                             # => Ensures consistent cluster state
                                             # => Type: {:ok, supervisor_spec()}
  end
end
```

**Strategy Selection**:

| Strategy        | Use Case                             | Restart Behavior          |
| --------------- | ------------------------------------ | ------------------------- |
| `:one_for_one`  | Independent processes                | Restart only failed child |
| `:rest_for_one` | Sequential dependencies (A → B → C)  | Restart failed + rest     |
| `:one_for_all`  | Tightly coupled (shared state/order) | Restart all children      |

### 6. Missing Child Specs

**FAIL - Incorrect child spec configuration**:

```elixir
# ANTI-PATTERN: Improper child spec
defmodule WorkerPool do
  use GenServer                              # => GenServer behavior

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)   # => No name registration
                                             # => Supervisor cannot restart by name
                                             # => Type: {:ok, pid()}
  end

  def child_spec(opts) do
    %{
      id: WorkerPool,                        # => ID for supervisor
      start: {WorkerPool, :start_link, [opts]},
                                             # => Start specification
      restart: :temporary,                   # => WRONG: temporary means no restart
      type: :worker                          # => Type: worker
    }                                        # => Process won't restart on crash
  end
end
```

**Why It Fails**:

1. **No restart** - `:temporary` means process not restarted on crash
2. **No name registration** - Cannot reference process after restart
3. **Lost process** - Supervisor loses track after crash

**CORRECT - Proper child spec with restart**:

```elixir
defmodule WorkerPool do
  use GenServer                              # => GenServer behavior

  def start_link(opts) do
    name = Keyword.get(opts, :name, __MODULE__)
                                             # => Get name from opts or use module name
    GenServer.start_link(__MODULE__, opts, name: name)
                                             # => Register with name
                                             # => Type: {:ok, pid()}
  end

  def child_spec(opts) do
    %{
      id: Keyword.get(opts, :id, __MODULE__),
                                             # => Unique ID (allows multiple instances)
      start: {__MODULE__, :start_link, [opts]},
                                             # => Start specification
      restart: :permanent,                   # => CORRECT: always restart
      shutdown: 5000,                        # => Graceful shutdown timeout (5s)
      type: :worker                          # => Type: worker
    }                                        # => Type: child_spec()
  end

  def init(opts) do
    {:ok, %{workers: [], max: Keyword.get(opts, :max, 10)}}
                                             # => Initialize worker pool state
                                             # => Type: {:ok, map()}
  end
end

# Usage with multiple instances
defmodule MyApp.Supervisor do
  use Supervisor                             # => Supervisor behavior

  def init(_opts) do
    children = [
      {WorkerPool, [name: :pool_1, id: :pool_1, max: 5]},
                                             # => First pool with 5 workers
      {WorkerPool, [name: :pool_2, id: :pool_2, max: 10]}
                                             # => Second pool with 10 workers
    ]                                        # => Multiple instances with unique IDs

    Supervisor.init(children, strategy: :one_for_one)
                                             # => Type: {:ok, supervisor_spec()}
  end
end
```

**Restart Options**:

- `:permanent` - Always restart (default for critical processes)
- `:temporary` - Never restart (one-time tasks)
- `:transient` - Restart only on abnormal termination (optional services)

## Process Communication Anti-Patterns

### 7. Message Queue Overflow

**FAIL - Unbounded message accumulation**:

```elixir
# ANTI-PATTERN: No backpressure, unbounded queue
defmodule Logger do
  use GenServer                              # => GenServer for logging

  def init(_opts) do
    {:ok, %{queue: []}}                      # => Unbounded queue
                                             # => Type: {:ok, map()}
  end

  def log(message) do
    GenServer.cast(__MODULE__, {:log, message})
                                             # => Async cast (no backpressure)
                                             # => Fast producer can overwhelm
                                             # => Type: :ok
  end

  def handle_cast({:log, message}, state) do
    # Slow operation (write to file)
    File.write!("app.log", message <> "\n", [:append])
                                             # => Slow I/O operation
                                             # => Messages accumulate faster than processing

    {:noreply, state}                        # => No queue tracking
                                             # => Memory can grow unbounded
                                             # => Type: {:noreply, map()}
  end
end
```

**Why It Fails**:

1. **Memory exhaustion** - Queue grows unbounded
2. **System instability** - OOM kills entire application
3. **No feedback** - Producer doesn't know queue is full
4. **Delayed processing** - Messages queued for minutes/hours

**CORRECT - Backpressure with queue limits**:

```elixir
defmodule Logger do
  use GenServer                              # => GenServer with backpressure

  @max_queue_size 1000                       # => Maximum queue size

  def init(_opts) do
    {:ok, %{queue: :queue.new(), size: 0}}   # => Erlang queue + size tracking
                                             # => Type: {:ok, map()}
  end

  def log(message) do
    GenServer.call(__MODULE__, {:log, message}, 5000)
                                             # => Synchronous call with timeout
                                             # => Provides backpressure to producer
                                             # => Type: :ok | {:error, :queue_full}
  end

  def handle_call({:log, message}, _from, state) do
    if state.size >= @max_queue_size do
      # Queue full - reject message         # => Apply backpressure
      {:reply, {:error, :queue_full}, state}
                                             # => Producer must handle rejection
    else
      # Add to queue
      queue = :queue.in(message, state.queue)
                                             # => Enqueue message
      new_state = %{state | queue: queue, size: state.size + 1}
                                             # => Update state and size

      # Process async if not already processing
      if state.size == 0 do
        send(self(), :process_queue)         # => Trigger processing
      end

      {:reply, :ok, new_state}               # => Reply success
                                             # => Type: {:reply, :ok, map()}
    end
  end

  def handle_info(:process_queue, state) do
    case :queue.out(state.queue) do
      {{:value, message}, new_queue} ->      # => Message available
        # Process message
        File.write!("app.log", message <> "\n", [:append])
                                             # => Write to file
        new_state = %{state | queue: new_queue, size: state.size - 1}
                                             # => Update queue and size

        # Continue processing if queue not empty
        if state.size > 1 do
          send(self(), :process_queue)       # => Schedule next processing
        end

        {:noreply, new_state}                # => Type: {:noreply, map()}

      {:empty, _queue} ->                    # => Queue empty
        {:noreply, state}                    # => No action needed
    end
  end
end
```

**Better - Use GenStage for producer/consumer backpressure**:

```elixir
# Producer with explicit demand
defmodule LogProducer do
  use GenStage                               # => GenStage producer

  def start_link(opts) do
    GenStage.start_link(__MODULE__, opts, name: __MODULE__)
                                             # => Type: {:ok, pid()}
  end

  def init(_opts) do
    {:producer, %{queue: :queue.new()}}      # => Producer state
                                             # => Type: {:producer, map()}
  end

  def handle_demand(demand, state) when demand > 0 do
    # Only send messages when consumer requests them
    events = dequeue_messages(state.queue, demand)
                                             # => Dequeue up to demand count
    new_queue = update_queue(state.queue, events)
                                             # => Remove sent events
    {:noreply, events, %{state | queue: new_queue}}
                                             # => Emit events to consumer
                                             # => Type: {:noreply, [event], map()}
  end
end

# Consumer with backpressure
defmodule LogConsumer do
  use GenStage                               # => GenStage consumer

  def start_link(opts) do
    GenStage.start_link(__MODULE__, opts, name: __MODULE__)
                                             # => Type: {:ok, pid()}
  end

  def init(_opts) do
    {:consumer, %{}, subscribe_to: [{LogProducer, max_demand: 10, min_demand: 5}]}
                                             # => Consumer subscribes to producer
                                             # => Requests 5-10 events at a time
                                             # => Type: {:consumer, map(), keyword()}
  end

  def handle_events(events, _from, state) do
    # Process events (producer waits until we finish)
    Enum.each(events, fn event ->
      File.write!("app.log", event <> "\n", [:append])
                                             # => Write each event
    end)

    {:noreply, [], state}                    # => Request more events
                                             # => Type: {:noreply, [], map()}
  end
end
```

**Rule**: Always implement backpressure for message-heavy systems - use call instead of cast, or use GenStage.

### 8. Synchronous Chain Calls

**FAIL - Nested synchronous GenServer calls**:

```elixir
# ANTI-PATTERN: Synchronous call chain leading to deadlock
defmodule ServiceA do
  use GenServer                              # => GenServer A

  def handle_call(:process, _from, state) do
    # Call ServiceB synchronously           # => Blocks ServiceA
    result = GenServer.call(ServiceB, :compute)
                                             # => Waits for ServiceB response
    {:reply, result, state}                  # => ServiceA blocked until reply
                                             # => Type: {:reply, term(), map()}
  end
end

defmodule ServiceB do
  use GenServer                              # => GenServer B

  def handle_call(:compute, _from, state) do
    # Call ServiceC synchronously           # => Blocks ServiceB
    result = GenServer.call(ServiceC, :calculate)
                                             # => Waits for ServiceC response
    {:reply, result, state}                  # => ServiceB blocked until reply
  end
end

defmodule ServiceC do
  use GenServer                              # => GenServer C

  def handle_call(:calculate, _from, state) do
    # DEADLOCK: Tries to call ServiceA      # => ServiceA waiting for ServiceC
    result = GenServer.call(ServiceA, :validate)
                                             # => ServiceC waiting for ServiceA
                                             # => Circular wait = DEADLOCK
    {:reply, result, state}                  # => Never reached
  end
end
```

**Why It Fails**:

1. **Deadlock risk** - Circular dependencies cause deadlock
2. **Timeout cascades** - One slow service times out entire chain
3. **Poor concurrency** - Sequential blocking reduces throughput
4. **Debugging difficulty** - Hard to trace call chains

**CORRECT - Async messaging with cast or Task**:

```elixir
# Use cast for async communication
defmodule ServiceA do
  use GenServer                              # => GenServer A

  def handle_call(:process, from, state) do
    # Send async request to ServiceB        # => Non-blocking
    GenServer.cast(ServiceB, {:compute, from, self()})
                                             # => Include reply destination
    {:noreply, state}                        # => Don't block waiting for reply
                                             # => Type: {:noreply, map()}
  end

  def handle_cast({:result, result}, state) do
    # Receive result from ServiceB          # => Async result
    # Process result...
    {:noreply, state}                        # => Type: {:noreply, map()}
  end
end

defmodule ServiceB do
  use GenServer                              # => GenServer B

  def handle_cast({:compute, reply_to, caller}, state) do
    # Perform computation                   # => Non-blocking
    result = do_compute()                    # => Calculate result

    # Send result back to ServiceA          # => Async reply
    GenServer.cast(caller, {:result, result})
                                             # => No blocking

    # Also reply to original caller
    GenServer.reply(reply_to, result)        # => Reply to original caller

    {:noreply, state}                        # => Type: {:noreply, map()}
  end
end
```

**Better - Use Task for async operations**:

```elixir
defmodule ServiceA do
  use GenServer                              # => GenServer A

  def handle_call(:process, _from, state) do
    # Start async task                      # => Non-blocking
    task = Task.async(fn ->
      ServiceB.compute()                     # => Run in separate process
    end)                                     # => Type: Task.t()

    # Return task reference immediately     # => No blocking
    {:reply, {:ok, task}, state}             # => Caller can await task
                                             # => Type: {:reply, {:ok, Task.t()}, map()}
  end
end

# Usage
{:ok, task} = ServiceA.process()             # => Get task reference
result = Task.await(task, 10_000)            # => Wait with timeout
                                             # => result: term()
```

**Rule**: Avoid synchronous call chains - use async cast or Task to prevent deadlocks and improve concurrency.

## Performance Anti-Patterns

### 9. Process Per Request

**FAIL - Spawning process for every request**:

```elixir
# ANTI-PATTERN: Creating short-lived processes repeatedly
defmodule RequestHandler do
  def handle_request(request) do
    # Spawn new process for each request    # => Process creation overhead
    spawn(fn ->
      process_request(request)               # => Short-lived process
    end)                                     # => Process dies after processing
                                             # => Type: pid()
  end

  defp process_request(request) do
    # Process request...
    result = expensive_computation(request)  # => Computation
    send_response(result)                    # => Send response
  end                                        # => Process terminates
end
```

**Why It Fails**:

1. **Process creation overhead** - Spawning processes is not free
2. **No pooling** - No process reuse
3. **Supervision complexity** - Short-lived processes hard to supervise
4. **Resource waste** - Constant allocation/deallocation

**CORRECT - Use Task with supervision**:

```elixir
# Supervised task pool
defmodule RequestHandler do
  def handle_request(request) do
    # Use Task.Supervisor for supervised task
    Task.Supervisor.start_child(MyApp.TaskSupervisor, fn ->
      process_request(request)               # => Supervised task
    end)                                     # => Automatic restart on crash
                                             # => Type: {:ok, pid()}
  end

  defp process_request(request) do
    result = expensive_computation(request)  # => Computation in supervised task
    send_response(result)                    # => Send response
  end                                        # => Task supervised until completion
end

# Supervision tree
defmodule MyApp.Application do
  use Application                            # => Application behavior

  def start(_type, _args) do
    children = [
      {Task.Supervisor, name: MyApp.TaskSupervisor}
                                             # => Task supervisor for async tasks
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
                                             # => Type: {:ok, pid()}
  end
end
```

**Better - Use GenServer pool for long-lived processes**:

```elixir
# Worker pool with Poolboy
defmodule WorkerPool do
  def handle_request(request) do
    :poolboy.transaction(
      :worker_pool,                          # => Pool name
      fn worker ->
        GenServer.call(worker, {:process, request})
                                             # => Reuse pooled worker
      end,
      5000                                   # => Timeout
    )                                        # => Type: term()
  end
end

# Poolboy configuration
defmodule MyApp.Application do
  use Application                            # => Application behavior

  def start(_type, _args) do
    poolboy_config = [
      {:name, {:local, :worker_pool}},       # => Pool name
      {:worker_module, Worker},              # => Worker module
      {:size, 10},                           # => Pool size (10 workers)
      {:max_overflow, 5}                     # => Max overflow (15 total)
    ]

    children = [
      :poolboy.child_spec(:worker_pool, poolboy_config, [])
                                             # => Poolboy worker pool
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
                                             # => Type: {:ok, pid()}
  end
end
```

**Rule**: Use Task.Supervisor for short tasks, GenServer pools for long-lived workers - avoid spawning bare processes.

### 10. ETS Misuse

**FAIL - Using ETS without read/write patterns**:

```elixir
# ANTI-PATTERN: Write-heavy ETS with :set type
defmodule Cache do
  def init do
    :ets.new(:cache, [:set, :public, :named_table])
                                             # => :set type for frequent writes
                                             # => Contention on write operations
                                             # => Type: :ets.tid()
  end

  def put(key, value) do
    :ets.insert(:cache, {key, value})        # => Frequent writes cause contention
                                             # => Type: true
  end

  def get(key) do
    case :ets.lookup(:cache, key) do
      [{^key, value}] -> {:ok, value}        # => Lookup value
      [] -> :not_found                       # => Key not found
    end                                      # => Type: {:ok, term()} | :not_found
  end
end
```

**Why It Fails**:

1. **Write contention** - :set type has contention on writes
2. **No expiration** - Cache grows unbounded
3. **No access control** - :public allows any process to modify
4. **Single table** - All data in one table (no sharding)

**CORRECT - Match ETS type to access pattern**:

```elixir
# Read-heavy cache with :ordered_set
defmodule ReadHeavyCache do
  def init do
    :ets.new(:read_cache, [
      :ordered_set,                          # => Ordered for range queries
      :public,                               # => Public for read access
      :named_table,                          # => Named table
      read_concurrency: true                 # => Optimize for concurrent reads
    ])                                       # => Type: :ets.tid()
  end

  def get(key) do
    case :ets.lookup(:read_cache, key) do
      [{^key, value, timestamp}] ->
        if System.monotonic_time(:second) - timestamp < 3600 do
          {:ok, value}                       # => Cache hit (valid)
        else
          :ets.delete(:read_cache, key)      # => Expired entry
          :not_found                         # => Cache miss
        end

      [] -> :not_found                       # => Cache miss
    end                                      # => Type: {:ok, term()} | :not_found
  end

  def put(key, value) do
    timestamp = System.monotonic_time(:second)
                                             # => Current timestamp
    :ets.insert(:read_cache, {key, value, timestamp})
                                             # => Insert with timestamp
                                             # => Type: true
  end
end

# Write-heavy log with :bag type
defmodule WriteHeavyLog do
  def init do
    :ets.new(:log, [
      :bag,                                  # => Allows duplicate keys
      :public,                               # => Public access
      :named_table,                          # => Named table
      write_concurrency: true                # => Optimize for concurrent writes
    ])                                       # => Type: :ets.tid()
  end

  def append(key, entry) do
    :ets.insert(:log, {key, entry})          # => Append entry (duplicate key allowed)
                                             # => Low contention with write_concurrency
                                             # => Type: true
  end

  def get_all(key) do
    :ets.lookup(:log, key)                   # => Get all entries for key
                                             # => Type: [tuple()]
  end
end

# Sharded cache for high concurrency
defmodule ShardedCache do
  @shard_count 16                            # => Number of shards

  def init do
    for i <- 1..@shard_count do
      :ets.new(
        :"cache_shard_#{i}",
        [:set, :public, :named_table, read_concurrency: true]
      )                                      # => Create 16 sharded tables
    end                                      # => Reduces contention
  end

  def put(key, value) do
    shard = get_shard(key)                   # => Determine shard by key hash
    :ets.insert(shard, {key, value})         # => Insert into specific shard
                                             # => Type: true
  end

  def get(key) do
    shard = get_shard(key)                   # => Determine shard by key hash
    case :ets.lookup(shard, key) do
      [{^key, value}] -> {:ok, value}        # => Lookup in specific shard
      [] -> :not_found
    end                                      # => Type: {:ok, term()} | :not_found
  end

  defp get_shard(key) do
    hash = :erlang.phash2(key)               # => Hash key
    shard_index = rem(hash, @shard_count) + 1
                                             # => Calculate shard index (1-16)
    :"cache_shard_#{shard_index}"            # => Return shard table name
                                             # => Type: atom()
  end
end
```

**ETS Type Selection**:

| Type             | Use Case               | Concurrency Options |
| ---------------- | ---------------------- | ------------------- |
| `:set`           | Unique keys, general   | `read_concurrency`  |
| `:ordered_set`   | Range queries, sorted  | `read_concurrency`  |
| `:bag`           | Duplicate keys allowed | `write_concurrency` |
| `:duplicate_bag` | Duplicate entries      | `write_concurrency` |

**Rule**: Match ETS type and concurrency options to access pattern - use sharding for high contention.

## Summary of Anti-Patterns

| Anti-Pattern               | Problem                 | Solution                             |
| -------------------------- | ----------------------- | ------------------------------------ |
| God Object GenServer       | Single point of failure | Separate GenServers per concern      |
| Stateless GenServer        | Unnecessary overhead    | Use plain modules                    |
| Blocking GenServer         | Process blocked         | Delegate to Task                     |
| No Supervision             | No automatic restart    | Supervision tree for all processes   |
| Wrong Supervision Strategy | Cascading restarts      | Match strategy to dependencies       |
| Missing Child Specs        | Cannot restart by name  | Proper child specs with `:permanent` |
| Message Queue Overflow     | Memory exhaustion       | Backpressure with queue limits       |
| Synchronous Call Chains    | Deadlock risk           | Async cast or Task                   |
| Process Per Request        | Creation overhead       | Task.Supervisor or GenServer pool    |
| ETS Misuse                 | Write contention        | Match type to access pattern         |

## Key Principles

**OTP-First Approach**:

- Use supervision trees for all processes
- Apply correct supervision strategies
- Implement proper child specs

**Process Design**:

- GenServers for stateful operations only
- Plain modules for stateless operations
- Delegate blocking work to Tasks

**Communication**:

- Implement backpressure for message-heavy systems
- Avoid synchronous call chains
- Use async cast or Task for non-blocking operations

**Performance**:

- Pool long-lived processes
- Match ETS type to access pattern
- Shard for high concurrency

**Production Reliability**:

- Supervise all processes
- Handle errors with "Let It Crash"
- Monitor message queues for overflow

## Next Steps

Apply these patterns to avoid production failures:

- [Best Practices](/en/learn/software-engineering/programming-languages/elixir/in-the-field/best-practices) - Production patterns and idioms
- [Genserver Patterns](/en/learn/software-engineering/programming-languages/elixir/in-the-field/genserver-patterns) - GenServer design patterns
- [Supervisor Trees](/en/learn/software-engineering/programming-languages/elixir/in-the-field/supervisor-trees) - Supervision strategies
- [Performance Optimization](/en/learn/software-engineering/programming-languages/elixir/in-the-field/performance-optimization) - Production optimization
