---
title: "Debugging"
date: 2025-12-21T18:30:00+07:00
draft: false
description: "Debug Elixir applications using IEx, dbg, Observer, logging, and tracing tools for effective problem diagnosis and resolution."
weight: 1000022
tags: ["elixir", "debugging", "troubleshooting", "iex", "observer", "how-to"]
---

**Need to debug Elixir code?** Use IEx.pry, dbg, Observer, and logging for effective debugging.

## Prerequisites

- Basic Elixir syntax
- Understanding of processes and the BEAM
- Completed [Beginner Tutorial](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner)

## Problem

Debugging functional, concurrent applications requires different tools than traditional imperative debugging. You need to inspect pipelines, trace messages between processes, monitor system resources, and understand failure cascades in supervision trees.

**Challenges:**

- Inspecting intermediate values in pipelines
- Understanding process crashes and restarts
- Tracking messages between processes
- Identifying performance bottlenecks
- Debugging production issues without stopping the system

## Solution

Leverage **IEx.pry** for breakpoints, **dbg** for pipeline inspection, **Observer** for system visualization, and **Logger** for production debugging.

## How It Works

### 1. IEx.pry - Interactive Breakpoints

```elixir
defmodule MyApp.Calculator do
  require IEx

  def complex_calculation(data) do
    step1 = transform(data)
    IEx.pry()  # Execution stops here, IEx session opens
    step2 = validate(step1)
    IEx.pry()  # Another breakpoint
    finalize(step2)
  end

  defp transform(data), do: data * 2
  defp validate(data), do: max(data, 0)
  defp finalize(data), do: data + 1
end
```

**In IEx:**

```elixir
iex> step1
20

iex> step1 * 3
60

iex> respawn()
```

**Key commands:**

- `whereami()` - Show current location in code
- `respawn()` - Continue execution
- `break!` - Set breakpoints dynamically

### 2. dbg - Pipeline Debugging

```elixir
result = [1, 2, 3]
|> Enum.map(&(&1 * 2))
|> Enum.filter(&(&1 > 2))
|> Enum.sum()

result = [1, 2, 3]
|> Enum.map(&(&1 * 2))
|> dbg()  # Shows: [2, 4, 6]
|> Enum.filter(&(&1 > 2))
|> dbg()  # Shows: [4, 6]
|> Enum.sum()
|> dbg()  # Shows: 10
```

**Advanced dbg usage:**

```elixir
def process(user) do
  dbg(user.name)  # Just the name
  dbg(user.age > 18)  # Boolean result
  user
  |> prepare()
  |> dbg()  # Entire pipeline step
  |> save()
end
```

### 3. IO.inspect - Quick Inspection

```elixir
[1, 2, 3]
|> Enum.map(&(&1 * 2))
|> IO.inspect(label: "After map")
|> Enum.sum()
|> IO.inspect(label: "Final result")

%User{name: "Alice", age: 30}
|> IO.inspect(limit: :infinity, pretty: true)
```

### 4. Logger - Production Debugging

```elixir
require Logger

defmodule MyApp.UserService do
  def create_user(params) do
    Logger.debug("Creating user with params: #{inspect(params)}")

    case validate(params) do
      {:ok, validated} ->
        Logger.info("User validation successful", user_id: validated.id)
        save_user(validated)

      {:error, changeset} ->
        Logger.warning("User validation failed",
          errors: inspect(changeset.errors),
          params: inspect(params)
        )
        {:error, changeset}
    end
  rescue
    exception ->
      Logger.error("User creation crashed",
        exception: Exception.format(:error, exception, __STACKTRACE__)
      )
      reraise exception, __STACKTRACE__
  end
end
```

**Structured logging:**

```elixir
config :logger,
  backends: [:console],
  compile_time_purge_matching: [
    [level_lower_than: :info]  # Remove debug in prod
  ]

config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id, :user_id, :module, :function]

Logger.info("User logged in",
  user_id: 123,
  ip_address: "192.168.1.1",
  user_agent: "Mozilla/5.0"
)
```

### 5. Observer - System Visualization

```elixir
:observer.start()
```

**Observer features:**

- **System tab:** CPU, memory, process count
- **Load Charts:** Real-time resource graphs
- **Applications:** Supervision tree structure
- **Processes:** All processes, sort by memory/reductions
- **Table Viewer:** ETS/Mnesia tables
- **Trace Overview:** Trace function calls

**Remote observation:**

```elixir
iex --name prod@host --cookie secret

iex --name debug@local --cookie secret
Node.connect(:"prod@host")
:observer.start()
```

### 6. Tracing with :sys

```elixir
{:ok, pid} = MyServer.start_link()
:sys.trace(pid, true)


:sys.trace(pid, false)

:sys.get_state(pid)

:sys.suspend(pid)
:sys.resume(pid)
```

### 7. Erlang's :dbg Module

```elixir
:dbg.tracer()

:dbg.tp(MyModule, :my_function, :cx)

:dbg.p(pid, [:call, :return_to])

:dbg.p(:all, :call)

:dbg.stop_clear()
```

**Example - trace user creation:**

```elixir
:dbg.tracer()
:dbg.tp(MyApp.Accounts, :create_user, [])
:dbg.p(:all, :call)

MyApp.Accounts.create_user(%{name: "Bob"})

```

### 8. Process Information

```elixir
Process.list()

Process.info(pid)
Process.info(pid, :messages)  # Message queue
Process.info(pid, :memory)     # Memory usage
Process.info(pid, :current_stacktrace)

Process.whereis(MyApp.Server)

Process.registered()
```

### 9. Recon - Production Debugging

```elixir
{:recon, "~> 2.5"}

:recon.proc_count(:memory, 10)

:recon.proc_count(:reductions, 10)

:recon.info(pid)

:recon.port_info()
```

### 10. ExUnit Debugging

```elixir
defmodule MyTest do
  use ExUnit.Case

  test "debugging with IEx" do
    result = some_function()
    require IEx; IEx.pry()  # Inspect during test
    assert result == :expected
  end

  # Run single test with debugging
  # mix test path/to/test.exs:10
end
```

## Variations

### Remote Console for Production

```bash
bin/my_app remote

iex --remsh my_app@hostname
```

### Custom Inspect Protocol

```elixir
defmodule User do
  defstruct [:id, :name, :password_hash]
end

defimpl Inspect, for: User do
  def inspect(user, _opts) do
    "#User<id: #{user.id}, name: #{user.name}, password: [REDACTED]>"
  end
end

IO.inspect(%User{id: 1, name: "Alice", password_hash: "secret"})
```

### Crash Dump Analysis

```bash
:erlang.halt(1)

erl_crash.dump

crashdump_viewer.start()
```

## Advanced Patterns

### 1. Distributed Debugging

```elixir
Node.connect(:"node_b@host")

pid = :rpc.call(:"node_b@host", Process, :whereis, [MyServer])
:sys.get_state(pid)
```

### 2. Debugging LiveView

```elixir
defmodule MyAppWeb.PageLive do
  use Phoenix.LiveView
  require Logger

  def mount(_params, _session, socket) do
    Logger.debug("LiveView mounted", socket_id: socket.id)
    if connected?(socket), do: Logger.info("WebSocket connected")
    {:ok, assign(socket, count: 0)}
  end

  def handle_event("inc", _params, socket) do
    Logger.debug("Increment event", current: socket.assigns.count)
    {:noreply, update(socket, :count, &(&1 + 1))}
  end
end
```

### 3. Memory Leak Detection

```elixir
before = :erlang.memory()

run_operation()

after_mem = :erlang.memory()

IO.inspect(after_mem[:total] - before[:total], label: "Memory delta")

:recon_alloc.memory(:allocated)
```

### 4. Deadlock Detection

```elixir
waiting = Process.list()
|> Enum.map(&{&1, Process.info(&1, :current_function)})
|> Enum.filter(fn {_pid, {:current_function, {mod, fun, _}}} ->
  mod == :gen_server and fun == :loop
end)

IO.inspect(waiting, label: "Processes waiting")
```

## Use Cases

**Development:**

- Understanding pipeline transformations
- Testing error handling paths
- Learning library behavior
- Debugging test failures

**Production:**

- Investigating slow requests
- Finding memory leaks
- Analyzing crashes
- Monitoring system health

**Performance:**

- Identifying bottlenecks
- Profiling function calls
- Measuring resource usage
- Optimizing hot paths

## Troubleshooting

### IEx.pry Not Working

```elixir
require IEx

iex -S mix phx.server --trace

IEx.break!(MyModule, :function_name, 2)  # arity 2
```

### Observer Crashes

```bash
brew install wxwidgets

apt-get install libwxgtk3.0-dev
```

### Can't See Logs

```elixir
Logger.configure(level: :debug)

Application.get_env(:logger, :backends)
```

## Best Practices

1. **Remove debug code before commit:**

   ```elixir
   # Use mix format to spot forgotten IEx.pry
   mix format --check-formatted
   ```

2. **Use appropriate log levels:**
   - `debug` - Development only
   - `info` - Important events
   - `warning` - Degraded state
   - `error` - Failures

3. **Structured logging in production:**

   ```elixir
   Logger.info("User action", user_id: id, action: "purchase")
   # Better than: Logger.info("User #{id} made purchase")
   ```

4. **Use Observer on QA, not production:**
   Observer's GUI can impact performance

5. **Set up remote access securely:**

   ```bash
   # Use SSH tunnel
   ssh -L 9001:localhost:9001 production-server
   ```

6. **Don't log sensitive data:**
   ```elixir
   Logger.info("Login attempt", user: sanitize(user))
   ```

## Common Pitfalls

1. **Forgetting to remove IEx.pry:** Breaks production
2. **Over-logging:** Fills disk, impacts performance
3. **Not using structured logging:** Hard to parse logs
4. **Ignoring process limits:** Observer shows current limits
5. **Debugging prod without supervision:** Always use supervised sessions

## Performance Impact

```elixir
Logger.info("Event occurred")  # Async

IO.inspect(large_data)  # Blocks

:dbg.p(:all, :call)  # Traces everything

:observer.start()  # GUI + polling
```

## Related Resources

- [Testing Guide](/en/learn/software-engineering/programming-language/elixir/how-to/testing)
- [Monitoring Guide](/en/learn/software-engineering/programming-language/elixir/how-to/monitoring)
- [Performance Guide](/en/learn/software-engineering/programming-language/elixir/how-to/performance)
- [GenServer Guide](/en/learn/software-engineering/programming-language/elixir/how-to/genserver)
- [Best Practices](/en/learn/software-engineering/programming-language/elixir/explanation/best-practices)
