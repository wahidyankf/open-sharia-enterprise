---
title: "Process Registry Patterns"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000008
description: "From manual PID tracking to Registry module for production process discovery in Elixir"
tags: ["elixir", "registry", "process-discovery", "otp", "genserver", "process-groups"]
prev: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/otp-behaviors"
next: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/concurrency-patterns"
---

**How do you track and discover processes in production systems?** This guide teaches process registry patterns using the OTP-first progression, starting with manual PID tracking to understand discovery challenges before introducing Registry abstractions.

## Why Process Discovery Matters

Production systems need reliable process discovery for:

- **User sessions** - Track active user connections across donation flows
- **Entity management** - Find processes managing specific business entities (contracts, orders, transactions)
- **Resource pools** - Locate available workers (database connections, API clients, compute resources)
- **Dynamic routing** - Route messages to correct handler processes based on entity identifiers
- **Monitoring** - Discover and inspect running processes for health checks and diagnostics

Consider a Shariah-compliant donation platform where users initiate donation flows. Each active session needs process tracking to handle concurrent donations, prevent duplicate submissions, and maintain transaction consistency.

## Manual PID Tracking - The Foundation

### Basic Session Tracking

Let's build a user session tracker using GenServer with manual PID storage:

```elixir
# Manual session tracking with PIDs stored in state
defmodule SessionTracker do
  use GenServer
                                             # => OTP GenServer behavior
                                             # => Provides supervised process

  # => Client API
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
                                             # => Starts named GenServer
                                             # => Initial state: empty map
                                             # => name: Global registration
  end

  def register_session(user_id, session_pid) do
    GenServer.call(__MODULE__, {:register, user_id, session_pid})
                                             # => Synchronous call
                                             # => user_id: Lookup key
                                             # => session_pid: Process to track
  end

  def get_session(user_id) do
    GenServer.call(__MODULE__, {:get, user_id})
                                             # => Returns: {:ok, pid} | :error
  end

  # => Server callbacks
  def init(initial_state) do
    {:ok, initial_state}                     # => initial_state: %{}
                                             # => State tracks user_id => pid
  end

  def handle_call({:register, user_id, session_pid}, _from, state) do
    Process.monitor(session_pid)             # => Monitor for crashes
                                             # => Sends :DOWN on termination
    new_state = Map.put(state, user_id, session_pid)
                                             # => Adds user_id => pid mapping
    {:reply, :ok, new_state}                 # => Returns: :ok to caller
                                             # => Updates state
  end

  def handle_call({:get, user_id}, _from, state) do
    result = Map.fetch(state, user_id)       # => Returns: {:ok, pid} | :error
    {:reply, result, state}                  # => State unchanged
  end

  def handle_info({:DOWN, _ref, :process, pid, _reason}, state) do
    new_state = state
    |> Enum.reject(fn {_user_id, session_pid} -> session_pid == pid end)
                                             # => Removes crashed process
    |> Map.new()                             # => Converts back to map
    {:noreply, new_state}                    # => Updates state
                                             # => No reply (info message)
  end
end
```

**Usage**:

```elixir
{:ok, _pid} = SessionTracker.start_link([])  # => Start tracker
                                             # => _pid: Tracker process

{:ok, session_pid} = UserSession.start_link(user_id: "donor-123")
                                             # => Start user session
                                             # => session_pid: Session process

SessionTracker.register_session("donor-123", session_pid)
                                             # => Returns: :ok
                                             # => Registers PID for lookup

{:ok, found_pid} = SessionTracker.get_session("donor-123")
                                             # => Returns: {:ok, session_pid}
                                             # => found_pid == session_pid
```

### Limitations of Manual Tracking

Manual PID storage faces critical production challenges:

**1. No Process Naming** - Cannot reference processes by name, only PID:

```elixir
# Need PID for every interaction
{:ok, pid} = SessionTracker.get_session("donor-123")
UserSession.submit_donation(pid, donation_data)
                                             # => Requires extra lookup step
                                             # => Two-phase operation
```

**2. No Built-in Lookup** - Must implement custom search logic:

```elixir
# Finding all sessions for multiple users requires iteration
user_ids = ["donor-123", "donor-456", "donor-789"]
sessions = Enum.flat_map(user_ids, fn user_id ->
  case SessionTracker.get_session(user_id) do
    {:ok, pid} -> [pid]
    :error -> []
  end
end)                                         # => Manual batch lookup
                                             # => O(n) complexity per user
```

**3. Race Conditions** - Process might die between lookup and use:

```elixir
{:ok, pid} = SessionTracker.get_session("donor-123")
                                             # => Process alive here
# ... (time passes, process crashes) ...
UserSession.submit_donation(pid, donation_data)
                                             # => Process dead here
                                             # => Raises: no process error
```

**4. Stale PID Cleanup** - Monitoring cleanup happens asynchronously:

```elixir
Process.exit(session_pid, :kill)             # => Kill session
{:ok, stale_pid} = SessionTracker.get_session("donor-123")
                                             # => May return dead PID
                                             # => :DOWN message not processed yet
```

These limitations become critical in production donation systems where concurrent users submit donations, sessions timeout, and processes crash under load.

## Registry Module - Production Discovery

### Registry with Unique Keys

The Registry module provides production-grade process discovery with name-based lookup:

```elixir
# Registry-based session tracking
defmodule SessionRegistry do
  # => Client API
  def start_link do
    Registry.start_link(keys: :unique, name: __MODULE__)
                                             # => keys: :unique - one process per key
                                             # => name: Registry identifier
                                             # => Returns: {:ok, pid}
  end

  def register_session(user_id) do
    Registry.register(__MODULE__, user_id, %{})
                                             # => Registers current process
                                             # => user_id: Lookup key
                                             # => %{}: Optional metadata
                                             # => Returns: {:ok, pid} | {:error, reason}
  end

  def get_session(user_id) do
    case Registry.lookup(__MODULE__, user_id) do
      [{pid, _metadata}] -> {:ok, pid}       # => Found: single entry
                                             # => _metadata: Registered metadata
      [] -> :error                           # => Not found
    end
  end

  def via_tuple(user_id) do
    {:via, Registry, {__MODULE__, user_id}}  # => via tuple for GenServer naming
                                             # => Allows name-based GenServer.call
  end
end
```

**Usage with automatic registration**:

```elixir
# Start Registry
{:ok, _pid} = SessionRegistry.start_link()   # => Initialize registry
                                             # => _pid: Registry process

# Start session with via tuple (automatic registration)
{:ok, session_pid} = UserSession.start_link(
  name: SessionRegistry.via_tuple("donor-123")
)                                            # => Registers in start_link
                                             # => name: via tuple for Registry
                                             # => session_pid: Session process

# Direct name-based calls (no lookup needed)
UserSession.submit_donation(
  SessionRegistry.via_tuple("donor-123"),
  %{amount: 100_000, currency: "IDR"}
)                                            # => Calls by name, not PID
                                             # => Registry resolves to PID
                                             # => Returns: donation result
```

**Automatic cleanup on process termination**:

```elixir
Process.exit(session_pid, :kill)             # => Kill session process
{:error, reason} = SessionRegistry.get_session("donor-123")
                                             # => Returns: :error immediately
                                             # => Registry auto-removed entry
                                             # => No stale PIDs
```

### Registry with Duplicate Keys

For tracking multiple processes per key (e.g., user with multiple donation flows):

```elixir
# Multiple sessions per user
defmodule MultiSessionRegistry do
  def start_link do
    Registry.start_link(keys: :duplicate, name: __MODULE__)
                                             # => keys: :duplicate - many processes per key
                                             # => Allows multiple registrations
  end

  def register_flow(user_id, flow_metadata) do
    Registry.register(__MODULE__, user_id, flow_metadata)
                                             # => Multiple processes can register
                                             # => flow_metadata: Flow-specific data
  end

  def get_all_flows(user_id) do
    Registry.lookup(__MODULE__, user_id)     # => Returns: list of {pid, metadata}
                                             # => All flows for user
  end

  def broadcast_to_user(user_id, message) do
    Registry.dispatch(__MODULE__, user_id, fn entries ->
      for {pid, _metadata} <- entries do
        send(pid, message)                   # => Sends to all processes
      end
    end)                                     # => Atomic dispatch operation
  end
end
```

**Usage**:

```elixir
# User starts multiple donation flows
{:ok, flow1} = DonationFlow.start_link(user_id: "donor-123")
Registry.register(MultiSessionRegistry, "donor-123", %{flow_id: "flow-1"})
                                             # => First flow registered

{:ok, flow2} = DonationFlow.start_link(user_id: "donor-123")
Registry.register(MultiSessionRegistry, "donor-123", %{flow_id: "flow-2"})
                                             # => Second flow registered
                                             # => Same user_id, different process

# Lookup returns all flows
flows = MultiSessionRegistry.get_all_flows("donor-123")
                                             # => Returns: [
                                             # =>   {flow1, %{flow_id: "flow-1"}},
                                             # =>   {flow2, %{flow_id: "flow-2"}}
                                             # => ]

# Broadcast to all flows
MultiSessionRegistry.broadcast_to_user("donor-123", {:update, new_data})
                                             # => Sends to flow1 and flow2
                                             # => Atomic operation
```

## Production Patterns

### Pattern 1: Registry vs Named Processes

**Use Named Processes** (`:name` option) when:

- **Single global instance** - Application-level singletons (rate limiter, cache manager)
- **Known at compile time** - Fixed process names in supervision tree
- **Simple lookup** - No dynamic keys required

```elixir
# Named process for global rate limiter
GenServer.start_link(RateLimiter, [], name: RateLimiter)
GenServer.call(RateLimiter, :check_limit)    # => Direct name lookup
                                             # => No registry needed
```

**Use Registry** when:

- **Dynamic keys** - User IDs, entity IDs, session tokens
- **Many processes** - Thousands to millions of tracked processes
- **Flexible lookup** - Query by dynamic runtime values
- **Metadata tracking** - Store process-specific information

```elixir
# Registry for dynamic user sessions
Registry.register(SessionRegistry, user_id, %{connected_at: DateTime.utc_now()})
                                             # => Dynamic key
                                             # => Metadata stored
```

### Pattern 2: Via Tuples for Supervised Processes

Via tuples enable Registry-based naming in supervision trees:

```elixir
defmodule UserSession do
  use GenServer

  def start_link(opts) do
    user_id = Keyword.fetch!(opts, :user_id)
                                             # => Extract user_id from opts
                                             # => Raises if missing
    GenServer.start_link(
      __MODULE__,
      opts,
      name: via_tuple(user_id)               # => Register with via tuple
    )                                        # => Supervised by DynamicSupervisor
  end

  defp via_tuple(user_id) do
    {:via, Registry, {SessionRegistry, user_id}}
                                             # => Registry registration
  end

  # Client API uses via tuples
  def submit_donation(user_id, donation_data) do
    GenServer.call(via_tuple(user_id), {:submit, donation_data})
                                             # => Name-based call
                                             # => No PID lookup needed
  end
end
```

### Pattern 3: Registry vs :pg (Process Groups)

**Use Registry** when:

- **Unique identification** - Each key maps to specific process(es)
- **Metadata required** - Store process-specific data
- **Local node** - Processes on single node (most applications)
- **Fast lookup** - O(1) key-based retrieval

**Use :pg** when:

- **Distributed processes** - Processes across multiple nodes
- **Group membership** - Processes belong to named groups without unique keys
- **Broadcast patterns** - Send messages to all group members
- **Node failure handling** - Automatic group membership updates on node disconnects

```elixir
# Registry: unique session per user
Registry.register(SessionRegistry, user_id, %{})
                                             # => One session per user_id

# :pg: multiple workers in group
:pg.join(:donation_workers, self())          # => Join worker group
                                             # => Multiple processes in group
:pg.get_members(:donation_workers)           # => Returns: all worker PIDs
                                             # => Across all nodes
```

### Pattern 4: Registry with Partitioning

For high-concurrency scenarios, partition Registry to reduce contention:

```elixir
# Partitioned Registry
Registry.start_link(
  keys: :unique,
  name: SessionRegistry,
  partitions: System.schedulers_online()     # => One partition per core
)                                            # => Reduces lock contention
                                             # => Improved throughput
```

**Trade-offs**:

- **Pros**: Higher concurrent registration/lookup throughput
- **Cons**: Cannot use `Registry.dispatch/3` efficiently, slightly higher memory

## Common Mistakes

**Mistake 1: Not handling registration failures**:

```elixir
# Wrong: Ignores registration errors
Registry.register(SessionRegistry, user_id, %{})
UserSession.do_work()                        # => Might fail if registration failed

# Right: Handle registration result
case Registry.register(SessionRegistry, user_id, %{}) do
  {:ok, _pid} ->
    UserSession.do_work()                    # => Registration succeeded
  {:error, {:already_registered, _pid}} ->
    {:error, :session_exists}                # => Unique key conflict
end
```

**Mistake 2: Using Registry for global singletons**:

```elixir
# Wrong: Overcomplicating singleton with Registry
Registry.register(AppRegistry, :rate_limiter, %{})

# Right: Use named process
GenServer.start_link(RateLimiter, [], name: RateLimiter)
```

**Mistake 3: Forgetting via tuple in supervised children**:

```elixir
# Wrong: start_link without registration
def start_link(opts) do
  GenServer.start_link(__MODULE__, opts)     # => Not registered
end                                          # => Cannot lookup later

# Right: Register with via tuple
def start_link(opts) do
  user_id = Keyword.fetch!(opts, :user_id)
  GenServer.start_link(
    __MODULE__,
    opts,
    name: {:via, Registry, {SessionRegistry, user_id}}
  )                                          # => Registered automatically
end
```

**Mistake 4: Mixing Registry keys and metadata**:

```elixir
# Wrong: Using metadata for lookup
Registry.register(SessionRegistry, :all_users, %{user_id: "donor-123"})
                                             # => Cannot query by metadata

# Right: Use user_id as key
Registry.register(SessionRegistry, "donor-123", %{connected_at: DateTime.utc_now()})
                                             # => Key for lookup, metadata for context
```

## Summary

Process registry patterns in Elixir:

**Manual Tracking** - Store PIDs in GenServer state for basic discovery, but faces naming limitations, race conditions, and cleanup complexity.

**Registry Module** - Production-grade process discovery with name-based lookup, automatic cleanup, metadata storage, and via tuple integration for supervised processes.

**Production Decisions**:

- **Named processes** for global singletons and compile-time names
- **Registry** for dynamic keys, metadata, and high-volume process tracking
- **:pg** for distributed process groups across nodes
- **Partitioned Registry** for high-concurrency scenarios

The Registry module eliminates manual PID tracking complexity while providing robust process discovery for production Elixir systems.
