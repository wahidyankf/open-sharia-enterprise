---
title: "Performance Profiling and Optimization"
date: 2025-12-21T17:45:00+07:00
draft: false
description: "Profile and optimize Elixir applications using fprof, benchee, Observer, and performance best practices for production systems."
weight: 1000013
tags:
  [
    "elixir",
    "performance",
    "profiling",
    "optimization",
    "benchmarking",
    "how-to",
  ]
---

**Need to optimize slow Elixir code?** This guide teaches systematic profiling and optimization techniques using benchmarking, profiling tools, and BEAM VM insights to identify and eliminate bottlenecks in production applications.

## Prerequisites

- Intermediate Elixir knowledge
- Understanding of BEAM VM basics
- Completed [Intermediate Tutorial](/en/learn/swe/prog-lang/elixir/tutorials/intermediate)
- Basic understanding of processes and concurrency

## Problem

Performance issues in Elixir applications can stem from multiple sources: inefficient algorithms, process bottlenecks, memory pressure, or improper use of concurrency primitives. Without systematic profiling, optimization efforts waste time on non-critical code paths.

**Challenges:**

- Identifying actual bottlenecks vs. perceived slow code
- Measuring performance impact of optimizations
- Balancing readability with performance
- Understanding BEAM VM performance characteristics
- Profiling in production without impacting users

## Solution Overview

Use a systematic approach: **measure first, optimize second, verify third**. Elixir provides excellent profiling tools built on Erlang's battle-tested infrastructure.

**Key Tools:**

- **Benchee**: Micro-benchmarking for comparing implementations
- **:fprof**: Function-level profiling with call counts and timing
- **:eprof**: Time-based profiling focusing on function execution time
- **Observer**: Visual system inspection (processes, memory, schedulers)
- **:recon**: Production-safe debugging and profiling
- **Telemetry**: Runtime metrics collection

## Detailed Implementation

### 1. Benchmarking with Benchee

Benchee provides statistical benchmarking with warmup, multiple runs, and comparison reports.

#### Basic Benchmark

```elixir
# Add to mix.exs
defp deps do
  [{:benchee, "~> 1.0", only: :dev}]
end

# Run benchmark
Benchee.run(%{
  "Enum.map" => fn -> Enum.map(1..1000, &(&1 * 2)) end,
  "for comprehension" => fn -> for x <- 1..1000, do: x * 2 end,
  "list comprehension" => fn -> Enum.to_list(for x <- 1..1000, do: x * 2) end
})
```

**Output:**

```
Name                         ips        average  deviation         median         99th %
Enum.map                  11.53 K       86.71 μs    ±18.45%       82.80 μs      141.20 μs
for comprehension         11.26 K       88.81 μs    ±17.23%       85.40 μs      143.60 μs
list comprehension        11.21 K       89.21 μs    ±16.89%       86.10 μs      144.20 μs

Comparison:
Enum.map                  11.53 K
for comprehension         11.26 K - 1.02x slower
list comprehension        11.21 K - 1.03x slower
```

#### Advanced Benchmark with Inputs

```elixir
Benchee.run(
  %{
    "String.split" => fn input -> String.split(input, ",") end,
    "Regex.split" => fn input -> Regex.split(~r/,/, input) end,
    "Binary pattern match" => fn input -> split_csv(input) end
  },
  inputs: %{
    "Small (10 items)" => String.duplicate("a,", 10),
    "Medium (100 items)" => String.duplicate("a,", 100),
    "Large (1000 items)" => String.duplicate("a,", 1000)
  },
  formatters: [
    Benchee.Formatters.Console,
    {Benchee.Formatters.HTML, file: "benchmark_results.html"}
  ],
  warmup: 2,
  time: 5
)

defp split_csv(input) do
  split_csv(input, [], [])
end

defp split_csv("," <> rest, current, acc) do
  split_csv(rest, [], [Enum.reverse(current) |> IO.iodata_to_binary() | acc])
end

defp split_csv(<<char, rest::binary>>, current, acc) do
  split_csv(rest, [char | current], acc)
end

defp split_csv("", current, acc) do
  Enum.reverse([Enum.reverse(current) |> IO.iodata_to_binary() | acc])
end
```

### 2. Profiling with :fprof

:fprof provides detailed function-level profiling including call counts and time spent.

#### Basic Profiling

```elixir
# Profile a function
:fprof.apply(&MyModule.slow_function/0, [])
:fprof.profile()
:fprof.analyse()
```

**Output Analysis:**

```
%% Analysis results:
{  analysis_options,
 [{callers, true},
  {sort, acc},
  {totals, false},
  {details, true}]}.

%                                               CNT       ACC       OWN
[{ totals,                                      849,  undefined,  100.123}].

%                                               CNT       ACC       OWN
[{ "<0.80.0>",                                  849,  undefined,  100.123}].

{[{undefined,                                     0,    0.000,    0.000}],
 { {fprof,call,1},                                1,    0.012,    0.001},
 [{{MyModule,slow_function,0},                    1,  100.111,    0.001},
  {suspend,                                       1,    0.000,    0.000}]}.

{[{{fprof,call,1},                                1,  100.111,    0.001}],
 { {MyModule,slow_function,0},                    1,  100.111,    0.001},
 [{{MyModule,process_data,1},                   100,   95.234,   45.123},
  {{MyModule,transform,1},                      100,    4.876,    3.456}]}.
```

**Interpretation:**

- **CNT**: Call count
- **ACC**: Accumulated time (including called functions)
- **OWN**: Time spent in function itself

#### Profiling Specific Code Block

```elixir
:fprof.start()

# Code to profile
result = Enum.map(1..10000, fn x ->
  expensive_computation(x)
end)

:fprof.stop()
:fprof.analyse(dest: "fprof_results.txt")
```

### 3. Time Profiling with :eprof

:eprof focuses on execution time per function, simpler than :fprof.

```elixir
# Profile with eprof
:eprof.start()
:eprof.start_profiling([self()])

# Run code
MyModule.slow_operation()

:eprof.stop_profiling()
:eprof.analyze()
:eprof.stop()
```

**Output:**

```
FUNCTION                      CALLS      %     TIME  [uS / CALLS]
MyModule.transform/1           1000   45.23   123456  [  123]
MyModule.validate/1            1000   30.12    82345  [   82]
Enum.map/2                        1   15.67    42890  [42890]
```

### 4. Visual Profiling with Observer

Observer provides real-time system visualization.

```elixir
# Start Observer GUI
:observer.start()
```

**Key Tabs:**

1. **System**: Overall BEAM VM stats (processes, memory, schedulers)
2. **Load Charts**: CPU, memory, process count over time
3. **Applications**: Supervision tree visualization
4. **Processes**: Process table with memory, reductions, message queue
5. **Table Viewer**: ETS/DETS table inspection
6. **Trace Overview**: Message tracing between processes

#### Identifying Process Bottlenecks

In Processes tab, sort by:

- **Message Queue Len**: Processes with backed-up messages
- **Reductions**: CPU-intensive processes
- **Memory**: Memory-hungry processes
- **Current Function**: What each process is doing

### 5. Production Profiling with :recon

:recon provides production-safe profiling without impacting performance.

```elixir
# Find top memory consumers
:recon.proc_count(:memory, 10)
# [{<0.123.0>, 15234567, [...]}]

# Find top CPU consumers
:recon.proc_count(:reductions, 10)

# Find processes with large message queues
:recon.proc_count(:message_queue_len, 10)

# Get process info
:recon.info(<0.123.0>)
```

#### Memory Analysis

```elixir
# System memory breakdown
:recon_alloc.memory(:allocated)
:recon_alloc.memory(:used)

# ETS table memory usage
:recon.ets_memory()

# Binary memory (often culprit for memory leaks)
:recon.bin_leak(10)
```

### 6. Telemetry for Runtime Metrics

Collect metrics during normal operation.

```elixir
defmodule MyApp.Telemetry do
  def setup do
    :telemetry.attach_many(
      "my-app-metrics",
      [
        [:my_app, :request, :start],
        [:my_app, :request, :stop],
        [:my_app, :db, :query]
      ],
      &handle_event/4,
      nil
    )
  end

  defp handle_event([:my_app, :request, :stop], measurements, metadata, _config) do
    duration = measurements.duration
    path = metadata.path

    # Log slow requests
    if duration > 1_000_000_000 do  # > 1 second
      require Logger
      Logger.warning("Slow request", duration_ms: div(duration, 1_000_000), path: path)
    end

    # Export to metrics system
    :telemetry_metrics.emit([:http, :request, :duration], duration, %{path: path})
  end
end
```

## How It Works

### BEAM VM Performance Characteristics

Understanding BEAM performance model is crucial:

1. **Process Scheduling**: Reductions budget (2000 per scheduler slice)
2. **Message Passing**: Copy semantics for small messages, reference passing for large binaries
3. **Garbage Collection**: Per-process GC, not stop-the-world
4. **ETS**: Shared memory tables, lock-free reads
5. **Binary Memory**: Reference counted, shared across processes

### Performance Measurement Accuracy

**Benchee Methodology:**

1. **Warmup**: Prepares JIT, caches before measuring
2. **Multiple Runs**: Statistical validity (mean, median, deviation)
3. **Garbage Collection**: Runs GC between scenarios
4. **Scheduler Fairness**: Runs each scenario multiple times

**Profiling Overhead:**

- :fprof: High overhead (10-100x slowdown), detailed results
- :eprof: Medium overhead (2-10x slowdown), simpler output
- :recon: Low overhead, production-safe

## Variations

### 1. Continuous Benchmarking

```elixir
# benchmark/my_benchmark.exs
defmodule MyBenchmark do
  def run do
    Benchee.run(
      %{
        "current" => fn -> current_implementation() end,
        "optimized" => fn -> optimized_implementation() end
      },
      save: [path: "benchmark_results.benchee", tag: "v1.2.0"],
      load: "benchmark_results.benchee"
    )
  end
end

# Compare with previous results
MyBenchmark.run()
```

### 2. Profiling Specific Processes

```elixir
# Find process by registered name
pid = Process.whereis(MyApp.Worker)

# Profile only that process
:eprof.start_profiling([pid])
# ... let it run ...
:eprof.stop_profiling()
:eprof.analyze()
```

### 3. Custom Telemetry Metrics

```elixir
defmodule MyApp.Metrics do
  use Telemetry.Metrics

  def metrics do
    [
      summary("my_app.request.duration",
        unit: {:native, :millisecond},
        tags: [:path, :method]
      ),
      counter("my_app.request.count",
        tags: [:status]
      ),
      distribution("my_app.db.query.duration",
        unit: {:native, :millisecond},
        reporter_options: [buckets: [10, 50, 100, 500, 1000]]
      )
    ]
  end
end
```

## Pitfalls and Best Practices

### Common Mistakes

**1. Premature Optimization**

**Bad:**

```elixir
# Optimizing before profiling
def process(items) do
  # Complex, unreadable optimization
  :ets.foldl(fn {k, v}, acc ->
    process_item(k, v, acc)
  end, [], :my_table)
end
```

**Good:**

```elixir
# Clear code first, profile, then optimize if needed
def process(items) do
  Enum.map(items, &process_item/1)
end

# After profiling shows this is bottleneck:
# - Benchmark alternative implementations
# - Optimize only if needed
# - Keep old version in comments for comparison
```

**2. Benchmarking in Development Environment**

**Bad:**

```bash
# MIX_ENV=dev is default (includes debugger overhead)
mix run benchmark.exs
```

**Good:**

```bash
# Use prod environment for realistic results
MIX_ENV=prod mix run benchmark.exs
```

**3. Ignoring Warmup**

**Bad:**

```elixir
Benchee.run(%{
  "function" => fn -> expensive_operation() end
}, warmup: 0)  # First runs will be slower
```

**Good:**

```elixir
Benchee.run(%{
  "function" => fn -> expensive_operation() end
}, warmup: 2)  # Give JIT time to optimize
```

**4. Profiling with Too Much Data**

:fprof generates massive output for long-running operations.

**Good:**

```elixir
# Profile small representative sample
:fprof.apply(&MyModule.process_batch/1, [Enum.take(large_dataset, 100)])
```

### Optimization Strategies

**1. Use Streams for Large Datasets**

```elixir
# Bad: Loads entire file into memory
File.read!("large.csv")
|> String.split("\n")
|> Enum.map(&parse_line/1)
|> Enum.filter(&valid?/1)

# Good: Processes line by line
File.stream!("large.csv")
|> Stream.map(&parse_line/1)
|> Stream.filter(&valid?/1)
|> Enum.to_list()
```

**2. Leverage ETS for Shared State**

```elixir
# Bad: GenServer becomes bottleneck for reads
def get_config(key) do
  GenServer.call(ConfigServer, {:get, key})
end

# Good: Lock-free ETS reads
def get_config(key) do
  case :ets.lookup(:config, key) do
    [{^key, value}] -> value
    [] -> nil
  end
end
```

**3. Batch Database Operations**

```elixir
# Bad: N+1 queries
Enum.each(user_ids, fn id ->
  Repo.get(User, id) |> update_user()
end)

# Good: Single query with preload
Repo.all(from u in User, where: u.id in ^user_ids, preload: :posts)
|> Enum.each(&update_user/1)
```

## Related Resources

- [Advanced Tutorial](/en/learn/swe/prog-lang/elixir/tutorials/advanced) - BEAM VM internals
- [Best Practices](/en/learn/swe/prog-lang/elixir/explanation/best-practices) - Performance patterns
- [Cookbook](/en/learn/swe/prog-lang/elixir/how-to/cookbook) - Optimization recipes
- [Caching Guide](/en/learn/swe/prog-lang/elixir/how-to/caching) - ETS and caching strategies
