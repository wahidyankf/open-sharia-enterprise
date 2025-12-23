---
title: "Supervision"
date: 2025-12-21T17:05:00+07:00
draft: false
description: "Design fault-tolerant Elixir applications using Supervisor with restart strategies, child specifications, and hierarchical supervision for automatic crash recovery."
weight: 1000005
tags:
  ["elixir", "supervisor", "otp", "fault-tolerance", "supervision", "how-to"]
---

**Need fault-tolerant applications that recover from crashes automatically?** This guide teaches you OTP supervision patterns with restart strategies, child specifications, and hierarchical trees for building resilient systems.

## Prerequisites

- Understanding of GenServer
- Basic OTP concepts
- Completed [Intermediate Tutorial](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate) or equivalent

## Problem

Processes crash due to bugs, invalid input, or external failures. Manual crash handling with `try`/`catch` is error-prone and doesn't scale. You need automatic recovery and isolation.

**Challenges:**

- Recovering from process crashes automatically
- Isolating failures (preventing cascade)
- Choosing appropriate restart strategies
- Managing process dependencies
- Monitoring system health

## Solution

Use **Supervisor** - OTP's process that monitors child processes and restarts them according to defined strategies. Supervisors form hierarchical trees for systematic fault tolerance.

### Key Concepts

1. **Supervisor** - Monitors and restarts children
2. **Child Specification** - Defines how to start/restart child
3. **Restart Strategy** - Determines which children restart on failure
4. **Restart Intensity** - Limits restart frequency to prevent crash loops

## How It Works

### 1. Basic Supervisor

```elixir
defmodule MyApp.Supervisor do
  use Supervisor

  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @impl true
  def init(_init_arg) do
    children = [
      {Counter, 0},
      {KeyValueStore, []},
      {SessionStore, []}
    ]

    # Restart strategy: if one child dies, restart only that child
    Supervisor.init(children, strategy: :one_for_one)
  end
end

# Usage
{:ok, _pid} = MyApp.Supervisor.start_link([])
# All three children (Counter, KeyValueStore, SessionStore) started
# If any child crashes, Supervisor restarts it automatically
```

**Anatomy:**

- `start_link/1` - Start supervisor
- `init/1` - Define children and strategy
- `children` - List of child specs (module + args)
- `strategy` - How to handle child failures

### 2. Restart Strategies

#### `:one_for_one` - Restart Only Failed Child

```elixir
defmodule OneForOneSupervisor do
  use Supervisor

  def start_link(_opts) do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    children = [
      {WorkerA, []},
      {WorkerB, []},
      {WorkerC, []}
    ]

    # If WorkerB crashes, only WorkerB restarts
    # WorkerA and WorkerC keep running
    Supervisor.init(children, strategy: :one_for_one)
  end
end
```

**Use when**: Children are independent (no shared state).

#### `:one_for_all` - Restart All Children

```elixir
defmodule OneForAllSupervisor do
  use Supervisor

  def start_link(_opts) do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    children = [
      {DatabaseConnection, []},
      {CacheServer, []},
      {APIHandler, []}
    ]

    # If ANY child crashes, ALL children restart
    # Ensures consistent state across dependent processes
    Supervisor.init(children, strategy: :one_for_all)
  end
end
```

**Use when**: Children are interdependent (must restart together for consistency).

#### `:rest_for_one` - Restart Failed Child and Following Children

```elixir
defmodule RestForOneSupervisor do
  use Supervisor

  def start_link(_opts) do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    children = [
      {DatabaseConnection, []},  # 1
      {CacheServer, []},         # 2 (depends on 1)
      {APIHandler, []}           # 3 (depends on 2)
    ]

    # If CacheServer crashes:
    #   - DatabaseConnection keeps running
    #   - CacheServer restarts
    #   - APIHandler restarts (depends on CacheServer)
    Supervisor.init(children, strategy: :rest_for_one)
  end
end
```

**Use when**: Children have sequential dependencies (pipeline).

### 3. Child Specifications

#### Simple Format

```elixir
# {Module, args} - uses Module.start_link(args)
{Counter, 0}
{KeyValueStore, [name: :kv_store]}
```

#### Full Format with Options

```elixir
%{
  id: Counter,                    # Unique identifier
  start: {Counter, :start_link, [0]},  # {module, function, args}
  restart: :permanent,            # :permanent | :temporary | :transient
  shutdown: 5000,                 # Timeout in ms or :brutal_kill
  type: :worker                   # :worker | :supervisor
}
```

#### Using `child_spec/1`

```elixir
defmodule ConfigurableWorker do
  use GenServer

  def start_link(opts) do
    name = Keyword.get(opts, :name, __MODULE__)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  def child_spec(opts) do
    %{
      id: Keyword.get(opts, :name, __MODULE__),
      start: {__MODULE__, :start_link, [opts]},
      restart: Keyword.get(opts, :restart, :permanent),
      shutdown: 5000,
      type: :worker
    }
  end

  # GenServer callbacks...
  def init(opts), do: {:ok, opts}
end

# Usage in Supervisor
children = [
  {ConfigurableWorker, name: :worker1, restart: :transient},
  {ConfigurableWorker, name: :worker2, restart: :permanent}
]
```

### 4. Restart Types

#### `:permanent` - Always Restart

```elixir
%{
  id: CriticalService,
  start: {CriticalService, :start_link, []},
  restart: :permanent  # ALWAYS restart on exit (normal or crash)
}
```

**Use for**: Essential services that must always run.

#### `:temporary` - Never Restart

```elixir
%{
  id: OneTimeTask,
  start: {OneTimeTask, :start_link, []},
  restart: :temporary  # NEVER restart (even on crash)
}
```

**Use for**: Tasks meant to run once.

#### `:transient` - Restart Only on Abnormal Exit

```elixir
%{
  id: Worker,
  start: {Worker, :start_link, []},
  restart: :transient  # Restart on crash, NOT on normal exit
}
```

**Use for**: Workers that may exit normally but should recover from crashes.

### 5. Dynamic Supervisors

For dynamically starting/stopping children at runtime:

```elixir
defmodule TaskSupervisor do
  use DynamicSupervisor

  def start_link(init_arg) do
    DynamicSupervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @impl true
  def init(_init_arg) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end

  # Client API
  def start_task(task_fun) do
    child_spec = %{
      id: Task,
      start: {Task, :start_link, [task_fun]},
      restart: :temporary
    }

    DynamicSupervisor.start_child(__MODULE__, child_spec)
  end

  def stop_task(pid) do
    DynamicSupervisor.terminate_child(__MODULE__, pid)
  end

  def list_tasks do
    DynamicSupervisor.which_children(__MODULE__)
  end
end

# Usage
{:ok, _sup_pid} = TaskSupervisor.start_link([])

{:ok, task_pid} = TaskSupervisor.start_task(fn ->
  Process.sleep(1000)
  IO.puts("Task complete!")
end)

TaskSupervisor.list_tasks()  # [{:undefined, task_pid, :worker, [Task]}]
```

### 6. Hierarchical Supervision Trees

```elixir
defmodule MyApp.Application do
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Top-level supervisor
      {MyApp.DatabaseSupervisor, []},
      {MyApp.CacheSupervisor, []},
      {MyApp.WebSupervisor, []},
      {MyApp.WorkerSupervisor, []}
    ]

    opts = [strategy: :one_for_one, name: MyApp.Supervisor]
    Supervisor.start_link(children, opts)
  end
end

defmodule MyApp.DatabaseSupervisor do
  use Supervisor

  def start_link(_opts) do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    children = [
      {MyApp.Repo, []},
      {MyApp.DatabaseMonitor, []}
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end
end

defmodule MyApp.WebSupervisor do
  use Supervisor

  def start_link(_opts) do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    children = [
      {Plug.Cowboy, scheme: :http, plug: MyApp.Router, options: [port: 4000]},
      {MyApp.Endpoint, []},
      {Phoenix.PubSub, name: MyApp.PubSub}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
```

**Tree Structure:**

```
MyApp.Supervisor (one_for_one)
├─ MyApp.DatabaseSupervisor (one_for_all)
│  ├─ MyApp.Repo
│  └─ MyApp.DatabaseMonitor
├─ MyApp.CacheSupervisor
├─ MyApp.WebSupervisor (one_for_one)
│  ├─ Plug.Cowboy
│  ├─ MyApp.Endpoint
│  └─ Phoenix.PubSub
└─ MyApp.WorkerSupervisor
```

### 7. Max Restarts and Intensity

Prevent infinite crash loops with restart intensity limits:

```elixir
defmodule LimitedSupervisor do
  use Supervisor

  def start_link(_opts) do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    children = [
      {FlakyWorker, []}
    ]

    # Max 3 restarts in 5 seconds
    # If exceeded, Supervisor itself crashes (escalates to parent)
    Supervisor.init(
      children,
      strategy: :one_for_one,
      max_restarts: 3,
      max_seconds: 5
    )
  end
end
```

**Default**: `max_restarts: 3`, `max_seconds: 5`

**When limit exceeded**: Supervisor terminates and escalates to parent supervisor (fault isolation).

### 8. Shutdown Strategies

Control how children are terminated:

```elixir
children = [
  # Worker with 5 second graceful shutdown
  %{
    id: Worker,
    start: {Worker, :start_link, []},
    shutdown: 5000  # Wait 5s for graceful termination
  },

  # Worker with brutal kill (immediate SIGKILL)
  %{
    id: FastWorker,
    start: {FastWorker, :start_link, []},
    shutdown: :brutal_kill
  },

  # Supervisor with infinity timeout (wait for all children)
  %{
    id: SubSupervisor,
    start: {SubSupervisor, :start_link, []},
    type: :supervisor,
    shutdown: :infinity  # Supervisors should use :infinity
  }
]
```

## Variations

### Registry-Based Dynamic Supervisors

```elixir
defmodule SessionSupervisor do
  use DynamicSupervisor

  def start_link(_opts) do
    DynamicSupervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end

  def start_session(user_id) do
    child_spec = {Session, user_id}
    DynamicSupervisor.start_child(__MODULE__, child_spec)
  end

  def stop_session(pid) do
    DynamicSupervisor.terminate_child(__MODULE__, pid)
  end
end

# With Registry for name lookup
defmodule SessionRegistry do
  def start_session(user_id) do
    case SessionSupervisor.start_session(user_id) do
      {:ok, pid} ->
        Registry.register(SessionRegistry, user_id, pid)
        {:ok, pid}
      error -> error
    end
  end

  def lookup_session(user_id) do
    case Registry.lookup(SessionRegistry, user_id) do
      [{pid, _}] -> {:ok, pid}
      [] -> {:error, :not_found}
    end
  end
end
```

### Task.Supervisor for Concurrent Tasks

```elixir
# In application.ex
children = [
  {Task.Supervisor, name: MyApp.TaskSupervisor}
]

# Usage
Task.Supervisor.start_child(MyApp.TaskSupervisor, fn ->
  # Supervised task (crashes isolated)
  perform_work()
end)

Task.Supervisor.async(MyApp.TaskSupervisor, fn ->
  # Supervised async task with result
  fetch_data()
end) |> Task.await()
```

## Pitfalls

### Wrong Restart Strategy

```elixir
# BAD: Using :one_for_one for dependent processes
children = [
  {Database, []},       # 1
  {Cache, []},          # 2 (needs Database)
  {APIHandler, []}      # 3 (needs Cache)
]

Supervisor.init(children, strategy: :one_for_one)
# If Database crashes and restarts, Cache still references old connection!

# GOOD: Use :rest_for_one for dependencies
Supervisor.init(children, strategy: :rest_for_one)
# Database crash restarts Cache and APIHandler too
```

### Restart Loops (No Intensity Limit)

```elixir
# BAD: Default 3 restarts/5s may be too lenient
Supervisor.init(children, strategy: :one_for_one)

# GOOD: Tune for flaky services
Supervisor.init(
  children,
  strategy: :one_for_one,
  max_restarts: 10,
  max_seconds: 60
)
```

### Blocking `init/1`

```elixir
# BAD: Slow initialization blocks supervisor
def init(:ok) do
  # This blocks supervision tree startup!
  result = HTTPoison.get!("http://slow-service.com/config")
  config = parse_config(result)

  children = [{Worker, config}]
  Supervisor.init(children, strategy: :one_for_one)
end

# GOOD: Move slow init to child process
def init(:ok) do
  children = [{Worker, :fetch_config_async}]
  Supervisor.init(children, strategy: :one_for_one)
end

# In Worker
def init(:fetch_config_async) do
  # Fetch config after process started
  config = HTTPoison.get!("http://slow-service.com/config") |> parse_config()
  {:ok, config}
end
```

### Not Using Hierarchical Trees

```elixir
# BAD: Flat supervision (all children at top level)
children = [
  {DatabaseConnection, []},
  {DatabaseMonitor, []},
  {CacheServer, []},
  {Worker1, []},
  {Worker2, []},
  {Worker3, []}
]

# GOOD: Hierarchical (group related processes)
children = [
  {DatabaseSupervisor, []},  # Manages DB connection + monitor
  {CacheSupervisor, []},     # Manages cache-related processes
  {WorkerSupervisor, []}     # Manages worker pool
]
```

## Use Cases

**Application Startup:**

- Supervise all critical services
- Ensure consistent startup order
- Handle initialization failures

**Worker Pools:**

- Dynamically start/stop workers
- Isolate worker failures
- Manage resource limits

**Connection Management:**

- Database connection pools
- External API client supervision
- WebSocket connection supervision

**Fault Isolation:**

- Prevent cascading failures
- Automatic recovery from crashes
- System resilience

## Related Resources

- [GenServer Guide](/en/learn/software-engineering/programming-language/elixir/how-to/genserver) - Building supervised workers
- [Intermediate Tutorial](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate) - Supervision fundamentals
- [Error Handling](/en/learn/software-engineering/programming-language/elixir/how-to/error-handling) - Complementary error strategies
- [Cookbook](/en/learn/software-engineering/programming-language/elixir/how-to/cookbook) - Supervision recipes

## Next Steps

1. Build multi-level supervision tree for your application
2. Experiment with different restart strategies
3. Implement dynamic worker pool with DynamicSupervisor
4. Learn OTP Application behavior for full app supervision
5. Study Phoenix supervision tree structure
