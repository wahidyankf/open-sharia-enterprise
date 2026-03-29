---
title: "Persistent Term"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000012
description: "High-performance read-once configuration storage using :persistent_term for Elixir production systems"
tags: ["elixir", "persistent-term", "configuration", "performance", "ets", "optimization"]
prev: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/ets-dets"
next: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/immutability-patterns"
---

**When should you use `:persistent_term` for storing configuration data?** This guide teaches persistent term storage patterns using the OTP-first progression, starting with manual configuration loading to understand access patterns before introducing `:persistent_term` for read-optimized storage.

## Why Configuration Storage Matters

Production systems need efficient configuration storage for:

- **Read-once data** - Configuration values loaded at startup and read frequently (API keys, thresholds, feature flags)
- **Performance optimization** - Eliminate lookup overhead for hot-path configuration (rate limits, validation rules)
- **Shariah compliance rules** - Store nisab thresholds, zakat rates, profit-sharing ratios accessed by every transaction
- **System constants** - Store business rules that rarely change but are read extensively (currency conversion tables, tax rates)
- **Memory efficiency** - Share read-only data across all processes without copying

Consider a Shariah-compliant fintech platform where zakat calculations need nisab threshold values for gold, silver, and cash. These values update quarterly but are read millions of times per day across all zakat calculations.

## Manual Configuration Loading - The Foundation

### Basic Module Attribute Pattern

Let's build configuration storage using compile-time module attributes:

```elixir
# Compile-time configuration via module attributes
defmodule ZakatConfig do
                                             # => Module for zakat calculations
                                             # => Compile-time constants

  # => Configuration values
  @gold_nisab_grams 85                       # => Gold nisab: 85 grams
                                             # => Fixed at compile time
  @silver_nisab_grams 595                    # => Silver nisab: 595 grams
  @zakat_rate 0.025                          # => Zakat rate: 2.5%

  def gold_nisab, do: @gold_nisab_grams      # => Returns: 85
                                             # => Inlined by compiler

  def silver_nisab, do: @silver_nisab_grams  # => Returns: 595

  def zakat_rate, do: @zakat_rate            # => Returns: 0.025

  def calculate_zakat(amount_in_grams, metal) do
    nisab = case metal do
      :gold -> @gold_nisab_grams             # => Compare to gold nisab
      :silver -> @silver_nisab_grams         # => Compare to silver nisab
    end

    if amount_in_grams >= nisab do
      amount_in_grams * @zakat_rate          # => Zakat = amount * 2.5%
                                             # => Returns: float
    else
      0                                      # => Below nisab, no zakat
    end
  end
end
```

**Usage**:

```elixir
ZakatConfig.gold_nisab()                     # => Returns: 85
                                             # => Compile-time constant

ZakatConfig.calculate_zakat(100, :gold)      # => Returns: 2.5
                                             # => 100 grams >= 85 nisab
                                             # => Zakat: 100 * 0.025 = 2.5

ZakatConfig.calculate_zakat(50, :gold)       # => Returns: 0
                                             # => 50 < 85 (below nisab)
```

**Limitations** - Compile-time configuration problems:

- **Deployment required** - Changing nisab values requires recompilation and deployment
- **No runtime updates** - Cannot adjust for exchange rate fluctuations or regulatory changes
- **Test inflexibility** - Cannot override values for testing different scenarios
- **Multi-environment complexity** - Different values per environment need compile-time switches

## Runtime Configuration with Application Environment

### Application.get_env Pattern

Let's add runtime configuration using application environment:

```elixir
# Runtime configuration via application environment
defmodule ZakatConfig do
                                             # => Uses application environment
                                             # => Runtime-configurable

  def gold_nisab do
    Application.get_env(:zakat, :gold_nisab, 85)
                                             # => app: :zakat
                                             # => key: :gold_nisab
                                             # => default: 85
                                             # => Returns: integer
  end

  def silver_nisab do
    Application.get_env(:zakat, :silver_nisab, 595)
                                             # => Reads from config
                                             # => Fallback: 595 grams
  end

  def zakat_rate do
    Application.get_env(:zakat, :zakat_rate, 0.025)
                                             # => Reads from config
                                             # => Fallback: 2.5%
  end

  def calculate_zakat(amount, metal) do
    nisab = case metal do
      :gold -> gold_nisab()                  # => Runtime lookup
                                             # => Reads from app env
      :silver -> silver_nisab()
    end

    if amount >= nisab do
      amount * zakat_rate()                  # => Another runtime lookup
                                             # => Reads rate from config
    else
      0
    end
  end
end
```

**Configuration** (config/runtime.exs):

```elixir
import Config

config :zakat,
  gold_nisab: System.get_env("GOLD_NISAB", "85") |> String.to_integer(),
                                             # => ENV var: GOLD_NISAB
                                             # => default: "85"
                                             # => Converted to integer
  silver_nisab: System.get_env("SILVER_NISAB", "595") |> String.to_integer(),
  zakat_rate: System.get_env("ZAKAT_RATE", "0.025") |> String.to_float()
                                             # => Converted to float
```

**Performance Problem** - Application.get_env costs:

```elixir
# Every call performs ETS lookup
# Hot path in transaction processing:
Enum.map(1..1_000_000, fn amount ->
  ZakatConfig.calculate_zakat(amount, :gold)
                                             # => 1M function calls
                                             # => Each calls gold_nisab()
                                             # => Each performs ETS lookup
                                             # => 2M+ ETS lookups total
end)
```

## ETS-Based Configuration Cache

### GenServer with ETS Storage

Let's add ETS caching to reduce Application.get_env overhead:

```elixir
# ETS-cached configuration via GenServer
defmodule ZakatConfig do
  use GenServer
                                             # => OTP GenServer behavior
                                             # => Manages ETS cache

  @table_name :zakat_config_cache            # => ETS table name

  # => Client API
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
                                             # => Named GenServer
                                             # => Manages ETS lifecycle
  end

  def gold_nisab do
    case :ets.lookup(@table_name, :gold_nisab) do
      [{:gold_nisab, value}] -> value        # => Cache hit
                                             # => Returns: cached integer
      [] -> load_and_cache(:gold_nisab)      # => Cache miss
                                             # => Loads from app env
    end
  end

  def silver_nisab do
    case :ets.lookup(@table_name, :silver_nisab) do
      [{:silver_nisab, value}] -> value
      [] -> load_and_cache(:silver_nisab)
    end
  end

  def zakat_rate do
    case :ets.lookup(@table_name, :zakat_rate) do
      [{:zakat_rate, value}] -> value
      [] -> load_and_cache(:zakat_rate)
    end
  end

  def reload_config do
    GenServer.call(__MODULE__, :reload)      # => Clears cache
                                             # => Forces reload
  end

  # => Server callbacks
  def init([]) do
    table = :ets.new(@table_name, [:set, :named_table, :public, read_concurrency: true])
                                             # => type: :set
                                             # => name: :zakat_config_cache
                                             # => access: :public
                                             # => optimization: read_concurrency
    load_initial_config(table)               # => Preloads all config
    {:ok, %{table: table}}                   # => Stores table reference
  end

  def handle_call(:reload, _from, state) do
    :ets.delete_all_objects(@table_name)     # => Clears cache
    load_initial_config(state.table)         # => Reloads from app env
    {:reply, :ok, state}                     # => Returns: :ok
  end

  defp load_initial_config(table) do
    :ets.insert(table, {:gold_nisab, Application.get_env(:zakat, :gold_nisab, 85)})
                                             # => Caches gold nisab
    :ets.insert(table, {:silver_nisab, Application.get_env(:zakat, :silver_nisab, 595)})
    :ets.insert(table, {:zakat_rate, Application.get_env(:zakat, :zakat_rate, 0.025)})
                                             # => Caches all config
  end

  defp load_and_cache(key) do
    value = Application.get_env(:zakat, key)
                                             # => Loads from app env
    :ets.insert(@table_name, {key, value})   # => Caches value
    value
  end
end
```

**ETS Trade-offs** - Cache complexity:

- **Read overhead** - Still requires ETS lookup per access (faster than Application.get_env but not free)
- **Cache invalidation** - Reload logic needed for config updates
- **Memory copies** - ETS returns copied data (no true zero-copy sharing)
- **GenServer dependency** - Cache lifetime tied to GenServer supervision

## :persistent_term - Zero-Cost Reads

### When to Use :persistent_term

**Ideal use cases** - Read-once, write-rarely configuration:

- **System constants** - Loaded once at startup, read millions of times (nisab values, tax rates, API endpoints)
- **Feature flags** - Set during deployment, checked on every request
- **Lookup tables** - Static or slowly changing data read in hot paths (currency codes, country lists)
- **Compiled configuration** - Values that only change across deployments

**When NOT to use** - Dynamic or frequently updated data:

- **User sessions** - Constant creation/deletion (use Registry or ETS)
- **Rate limiting counters** - High write frequency (use ETS or Atomics)
- **Real-time metrics** - Continuous updates (use dedicated metric systems)

### :persistent_term Configuration Pattern

Let's implement zero-cost configuration reads:

```elixir
# Zero-cost reads via :persistent_term
defmodule ZakatConfig do
  use GenServer
                                             # => Manages :persistent_term lifecycle
                                             # => One-time initialization

  @config_key {__MODULE__, :config}          # => Unique key for storage
                                             # => Namespaced by module

  # => Client API
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def gold_nisab do
    get_config(:gold_nisab)                  # => Direct :persistent_term read
                                             # => Returns: integer
                                             # => Zero copy, constant time
  end

  def silver_nisab do
    get_config(:silver_nisab)
  end

  def zakat_rate do
    get_config(:zakat_rate)
  end

  def reload_config do
    GenServer.call(__MODULE__, :reload)      # => Triggers expensive write
                                             # => Use sparingly
  end

  # => Server callbacks
  def init([]) do
    load_config()                            # => One-time load at startup
    {:ok, %{}}
  end

  def handle_call(:reload, _from, state) do
    load_config()                            # => Reload from app env
                                             # => Expensive: global GC pause
    {:reply, :ok, state}
  end

  defp load_config do
    config = %{
      gold_nisab: Application.get_env(:zakat, :gold_nisab, 85),
      silver_nisab: Application.get_env(:zakat, :silver_nisab, 595),
      zakat_rate: Application.get_env(:zakat, :zakat_rate, 0.025)
    }
    :persistent_term.put(@config_key, config)
                                             # => Stores entire config map
                                             # => Expensive write operation
                                             # => Global GC triggered
  end

  defp get_config(key) do
    config = :persistent_term.get(@config_key)
                                             # => Returns: config map
                                             # => Zero-cost read
                                             # => No copying
    Map.get(config, key)                     # => Extract specific value
  end
end
```

**Performance characteristics**:

```elixir
# Reads are essentially free (constant time, no copies)
Enum.map(1..10_000_000, fn _ ->
  ZakatConfig.gold_nisab()                   # => 10M reads
                                             # => Negligible overhead
                                             # => No ETS lookups
                                             # => Direct memory access
end)

# Writes are expensive (full GC pause)
ZakatConfig.reload_config()                  # => Global operation
                                             # => All processes pause briefly
                                             # => Only use for rare updates
```

## :persistent_term vs ETS Trade-offs

### Performance Comparison

```elixir
# Benchmark configuration reads
defmodule ConfigBenchmark do
  def benchmark_reads(iterations) do
    # ETS read (with read_concurrency)
    ets_time = :timer.tc(fn ->
      Enum.each(1..iterations, fn _ ->
        :ets.lookup(:config_ets, :gold_nisab)
                                             # => ETS lookup per iteration
                                             # => ~100-200ns per read
      end)
    end) |> elem(0)

    # :persistent_term read
    pt_time = :timer.tc(fn ->
      Enum.each(1..iterations, fn _ ->
        :persistent_term.get({ZakatConfig, :gold_nisab})
                                             # => Direct read
                                             # => ~10-20ns per read
                                             # => 10x faster than ETS
      end)
    end) |> elem(0)

    IO.puts("ETS: #{ets_time}μs, :persistent_term: #{pt_time}μs")
                                             # => Typical: ETS 10x slower
  end
end
```

### Memory Characteristics

**ETS behavior**:

- **Copy on read** - Each lookup copies data to calling process
- **Process-local** - Read process owns the copy
- **GC per-process** - Each copy subject to process GC

**:persistent_term behavior**:

- **Zero-copy reads** - All processes share single instance
- **Global GC** - Writes trigger full system GC
- **Permanent until replaced** - Data lives until explicitly overwritten

### Write Cost Analysis

```elixir
# ETS write (cheap)
:ets.insert(:config_ets, {:gold_nisab, 90})  # => Fast, local operation
                                             # => No global impact
                                             # => ~1-2μs

# :persistent_term write (expensive)
:persistent_term.put({ZakatConfig, :gold_nisab}, 90)
                                             # => Global operation
                                             # => Full GC pause
                                             # => 10-100ms+ (system-dependent)
                                             # => All processes briefly pause
```

## Production Configuration Patterns

### Startup-Only Loading

**Best practice** - Load configuration once at application start:

```elixir
# config/runtime.exs
import Config

# Load from environment at startup
config :zakat,
  gold_nisab: System.get_env("GOLD_NISAB", "85") |> String.to_integer(),
  silver_nisab: System.get_env("SILVER_NISAB", "595") |> String.to_integer(),
  zakat_rate: System.get_env("ZAKAT_RATE", "0.025") |> String.to_float(),
  nisab_currency: System.get_env("NISAB_CURRENCY", "USD")
```

```elixir
# lib/zakat/application.ex
defmodule Zakat.Application do
  use Application

  def start(_type, _args) do
    children = [
      ZakatConfig,                           # => Loads :persistent_term at startup
      # ... other children
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
```

### Graceful Configuration Reload

**Pattern** - Scheduled reload with monitoring:

```elixir
# Scheduled configuration reload (quarterly for nisab updates)
defmodule ZakatConfig.Reloader do
  use GenServer
  require Logger

  @reload_interval :timer.hours(24 * 90)     # => 90 days
                                             # => Quarterly updates

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init([]) do
    schedule_reload()                        # => Schedule first reload
    {:ok, %{}}
  end

  def handle_info(:reload, state) do
    Logger.info("Reloading zakat configuration")

    # Reload during low-traffic window
    start_time = System.monotonic_time()
    ZakatConfig.reload_config()              # => Expensive operation
    duration = System.monotonic_time() - start_time

    Logger.info("Config reloaded in #{duration}μs")
    schedule_reload()                        # => Schedule next reload
    {:noreply, state}
  end

  defp schedule_reload do
    Process.send_after(self(), :reload, @reload_interval)
                                             # => 90 day interval
  end
end
```

### Multi-Namespace Configuration

**Pattern** - Organize related configuration by namespace:

```elixir
defmodule SystemConfig do
  @zakat_key {__MODULE__, :zakat}
  @api_key {__MODULE__, :api}
  @features_key {__MODULE__, :features}

  def init do
    # Store zakat configuration
    :persistent_term.put(@zakat_key, %{
      gold_nisab: Application.get_env(:zakat, :gold_nisab, 85),
      silver_nisab: Application.get_env(:zakat, :silver_nisab, 595),
      zakat_rate: Application.get_env(:zakat, :zakat_rate, 0.025)
    })

    # Store API configuration
    :persistent_term.put(@api_key, %{
      base_url: Application.get_env(:api, :base_url),
      timeout: Application.get_env(:api, :timeout, 5000),
      retry_attempts: Application.get_env(:api, :retry_attempts, 3)
    })

    # Store feature flags
    :persistent_term.put(@features_key, %{
      enhanced_zakat: Application.get_env(:features, :enhanced_zakat, false),
      multi_currency: Application.get_env(:features, :multi_currency, true)
    })
  end

  def get_zakat_config, do: :persistent_term.get(@zakat_key)
  def get_api_config, do: :persistent_term.get(@api_key)
  def get_features, do: :persistent_term.get(@features_key)
end
```

## Testing with :persistent_term

### Test Isolation Pattern

```elixir
# Test helper for configuration override
defmodule ZakatConfig.Test do
  def with_config(config_overrides, test_fn) do
    original = :persistent_term.get({ZakatConfig, :config})
                                             # => Save original config

    try do
      merged = Map.merge(original, config_overrides)
      :persistent_term.put({ZakatConfig, :config}, merged)
                                             # => Apply test overrides
      test_fn.()                             # => Run test
    after
      :persistent_term.put({ZakatConfig, :config}, original)
                                             # => Restore original
    end
  end
end
```

**Usage in tests**:

```elixir
defmodule ZakatCalculatorTest do
  use ExUnit.Case

  test "calculates zakat with custom nisab" do
    ZakatConfig.Test.with_config(%{gold_nisab: 100}, fn ->
      assert ZakatCalculator.calculate(150, :gold) == 3.75
                                             # => Uses test nisab: 100
                                             # => 150 >= 100
                                             # => Zakat: 150 * 0.025
    end)
                                             # => Original config restored
  end
end
```

## When to Choose Each Approach

### Decision Matrix

**Use :persistent_term when**:

- Configuration loads once at startup
- Read frequency is very high (hot path, millions of reads)
- Updates are extremely rare (quarterly, annually, or never)
- All processes need identical view of data
- Memory efficiency matters (large shared data)

**Use ETS when**:

- Write frequency is moderate to high
- Configuration updates regularly (hourly, daily)
- Per-process isolation needed
- Delete operations required

**Use Application.get_env when**:

- Read frequency is low
- Configuration rarely accessed
- Development simplicity preferred over performance

**Use compile-time (module attributes) when**:

- Values truly never change
- Maximum performance critical
- Deployment-based updates acceptable

## Real-World Financial Configuration

### Complete Zakat System Configuration

```elixir
defmodule Zakat.Config do
  use GenServer
  require Logger

  @config_key {__MODULE__, :config}

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  # => Gold configuration
  def gold_nisab, do: get_in_config([:gold, :nisab])
  def gold_price_per_gram, do: get_in_config([:gold, :price_per_gram])

  # => Silver configuration
  def silver_nisab, do: get_in_config([:silver, :nisab])
  def silver_price_per_gram, do: get_in_config([:silver, :price_per_gram])

  # => Cash configuration
  def cash_nisab_currency(currency), do: get_in_config([:cash, :nisab, currency])

  # => Zakat rate (universal 2.5%)
  def zakat_rate, do: get_in_config([:rates, :zakat])

  def init([]) do
    load_config()
    Logger.info("Zakat configuration loaded to :persistent_term")
    {:ok, %{}}
  end

  defp load_config do
    config = %{
      gold: %{
        nisab: Application.get_env(:zakat, :gold_nisab, 85),
        price_per_gram: fetch_gold_price()
      },
      silver: %{
        nisab: Application.get_env(:zakat, :silver_nisab, 595),
        price_per_gram: fetch_silver_price()
      },
      cash: %{
        nisab: %{
          "USD" => 5000,                     # => Approximate USD equivalent
          "EUR" => 4500,
          "GBP" => 4000,
          "IDR" => 75_000_000
        }
      },
      rates: %{
        zakat: 0.025                         # => Universal 2.5%
      }
    }

    :persistent_term.put(@config_key, config)
  end

  defp get_in_config(path) do
    config = :persistent_term.get(@config_key)
    get_in(config, path)
  end

  defp fetch_gold_price do
    # In production: fetch from external API
    # For now: default value
    60.0                                     # => USD per gram
  end

  defp fetch_silver_price do
    0.80                                     # => USD per gram
  end
end
```

## Summary

**Key takeaways**:

- **:persistent_term** provides zero-cost reads for read-once, write-rarely configuration
- **Writes are expensive** - Trigger global GC pause, use only at startup or rare updates
- **Perfect for constants** - Nisab thresholds, tax rates, feature flags, system configuration
- **Trade-off with ETS** - :persistent_term faster reads but expensive writes; ETS balanced
- **Not for dynamic data** - Use Registry/ETS for high-write-frequency data
- **Namespace configuration** - Use distinct keys for different config domains
- **Test isolation** - Save/restore pattern for test configuration overrides

**Next steps**: Explore [ETS patterns](/en/learn/software-engineering/programming-languages/elixir/in-the-field/ets-dets) for dynamic data storage, or [performance optimization](/en/learn/software-engineering/programming-languages/elixir/in-the-field/performance-optimization) for comprehensive system tuning.
