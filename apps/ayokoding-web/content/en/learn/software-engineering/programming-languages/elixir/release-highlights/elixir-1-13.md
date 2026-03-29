---
title: "Elixir 1 13"
date: 2025-02-05T00:00:00+07:00
draft: false
description: "Semantic recompilation, Mix.install improvements, Registry partitions"
weight: 1000005
tags: ["elixir", "release-notes", "elixir-1.13", "compilation", "mix", "registry"]
prev: "/en/learn/software-engineering/programming-languages/elixir/release-highlights/elixir-1-14"
next: "/en/learn/software-engineering/programming-languages/elixir/release-highlights/elixir-1-12"
---

## Release Overview

Elixir 1.13 arrived in December 2021, delivering significant compilation speed improvements through semantic recompilation. This release focused on developer experience enhancements rather than new language features, making builds faster and Mix.install more practical for production scenarios.

The semantic recompilation feature fundamentally changed how Elixir determines which files need rebuilding. Instead of recompiling whenever a file's syntax changed, the compiler now analyzes semantic changes, meaning modifications to private functions or documentation no longer trigger cascading recompilations across dependent modules.

Mix.install matured from an experimental scripting feature to a reliable tool for notebooks and single-file applications. The improvements made Livebook notebooks more practical and enabled production-ready Elixir scripts without traditional project structures.

Registry received performance optimizations through partitioning support, making it more suitable for high-throughput scenarios. These improvements benefited Phoenix PubSub and other libraries relying on Registry for process coordination.

## Semantic Recompilation

Traditional recompilation strategies recompile a module whenever any dependency changes. If ModuleA depends on ModuleB, any change to ModuleB triggers recompilation of ModuleA, even if the change only affected private functions ModuleA never called.

Semantic recompilation analyzes the actual interface changes that matter. When you modify a private function in ModuleB, the compiler recognizes that ModuleA only depends on ModuleB's public interface. Since the public interface remained unchanged, ModuleA doesn't need recompilation.

```elixir
# File: accounting/transaction.ex
defmodule Accounting.Transaction do
  # => Public interface: record_payment/2
  def record_payment(invoice_id, amount) when is_integer(amount) do
    # => Called by other modules
    validate_amount(amount)
    # => Returns: {:ok, transaction_id} (type: {:ok, integer})
    {:ok, persist_transaction(invoice_id, amount)}
  end

  # => Private function: validate_amount/1
  defp validate_amount(amount) when amount > 0, do: :ok
  # => Raising error for invalid amounts
  defp validate_amount(_), do: raise "Amount must be positive"

  # => Private function: persist_transaction/2
  defp persist_transaction(invoice_id, amount) do
    # => Generates transaction ID
    transaction_id = :erlang.unique_integer([:positive])
    # => Persisting to database (simulated)
    # => Returns: transaction_id (type: integer)
    transaction_id
  end
end
```

```elixir
# File: billing/invoice_processor.ex
defmodule Billing.InvoiceProcessor do
  # => Only depends on public interface of Accounting.Transaction
  def process_payment(invoice_id, amount) do
    # => Calls public function record_payment/2
    Accounting.Transaction.record_payment(invoice_id, amount)
  end
end
```

If you modify `persist_transaction/2` to use a different database strategy, `Billing.InvoiceProcessor` doesn't recompile because it never directly called that private function. The semantic recompilation algorithm detected that the public interface `record_payment/2` signature remained unchanged.

This optimization dramatically reduces compilation time in large codebases with deep dependency trees. A monorepo with 500 modules might previously recompile 200 modules when changing a private function in a commonly imported utility module. With semantic recompilation, only modules actually using the changed interface recompile.

Documentation changes also benefit from this optimization. Adding or updating module documentation (`@moduledoc`) or function documentation (`@doc`) no longer triggers recompilation of dependent modules. This encouraged better documentation practices without the compilation time penalty.

The compiler tracks several semantic elements to determine recompilation necessity:

- **Public function signatures** - Changes to function names, arity, or guard clauses
- **Public macros** - Changes to macro definitions or compile-time behavior
- **Module attributes** - Changes to compile-time attributes used by dependent modules
- **Behaviours** - Changes to callback definitions
- **Structs** - Changes to struct field definitions

```elixir
# File: financial/currency.ex
defmodule Financial.Currency do
  # => Struct definition with fields
  defstruct [:code, :amount]
  # => code: currency code string (e.g., "USD")
  # => amount: integer amount in smallest unit (cents)

  # => Public function for currency creation
  def new(code, amount) when is_binary(code) and is_integer(amount) do
    # => Returns: %Financial.Currency{} struct
    %__MODULE__{code: code, amount: amount}
  end

  # => Private formatting function
  defp format_amount(amount) do
    # => Converts cents to dollars with decimal
    # => Example: 12550 becomes "125.50"
    dollars = div(amount, 100)
    cents = rem(amount, 100)
    # => Returns: string with 2 decimal places
    "#{dollars}.#{String.pad_leading(to_string(cents), 2, "0")}"
  end
end
```

If you change the `format_amount/1` implementation to use different rounding rules, modules that use `Financial.Currency.new/2` won't recompile. However, if you add a new field to the struct definition, all modules using the struct will recompile because the struct's semantic interface changed.

## Mix.install Improvements

Mix.install enables running Elixir scripts with dependencies without creating a full Mix project. Introduced experimentally in Elixir 1.12, version 1.13 stabilized the API and improved reliability for production scenarios.

```elixir
# File: financial_report.exs
#!/usr/bin/env elixir

# => Installing dependencies for this script
Mix.install([
  # => Jason for JSON encoding/decoding
  {:jason, "~> 1.2"},
  # => Decimal for precise decimal arithmetic
  {:decimal, "~> 2.0"}
])

# => Financial report generator module
defmodule FinancialReportGenerator do
  # => Generates monthly revenue report
  def generate_monthly_report(transactions) when is_list(transactions) do
    # => Calculating total revenue using Decimal
    total = Enum.reduce(transactions, Decimal.new(0), fn tx, acc ->
      # => Adding transaction amount to accumulator
      # => tx.amount is string like "125.50"
      Decimal.add(acc, Decimal.new(tx.amount))
    end)

    # => Building report structure
    report = %{
      # => Report metadata
      period: Date.utc_today() |> Date.to_string(),
      # => Total revenue as string
      total_revenue: Decimal.to_string(total),
      # => Transaction count
      transaction_count: length(transactions)
    }

    # => Encoding report to JSON
    # => Returns: JSON string
    Jason.encode!(report)
  end
end

# Example transactions
# => Simulating transaction data
transactions = [
  %{id: 1, amount: "125.50"},
  # => Second transaction
  %{id: 2, amount: "89.99"},
  # => Third transaction
  %{id: 3, amount: "250.00"}
]

# => Generating and printing report
# => Output: JSON string with total revenue
transactions
|> FinancialReportGenerator.generate_monthly_report()
|> IO.puts()
```

Running this script executes several steps:

1. Mix.install downloads and compiles the `jason` and `decimal` dependencies
2. The compiler loads the dependencies into the script's environment
3. The FinancialReportGenerator module compiles and executes
4. Output appears on stdout

The 1.13 improvements focused on reliability and caching:

- **Dependency caching** - Dependencies cache across script runs, avoiding repeated downloads
- **Lock file support** - Scripts can reference a `Mix.lock` file for reproducible dependency versions
- **Configuration support** - Scripts can set Mix configuration values
- **Better error messages** - Dependency resolution errors provide clearer feedback

This made Mix.install practical for operational scripts in production environments:

```elixir
# File: database_backup.exs
#!/usr/bin/env elixir

# => Installing database client dependency
Mix.install([
  {:postgrex, "~> 0.15"}
], lockfile: "database_backup.lock")
# => Using lockfile for reproducible builds
# => Ensures same dependency versions across runs

# => Configure database connection
Application.put_env(:postgrex, :config, [
  # => Database connection parameters
  hostname: System.get_env("DB_HOST"),
  # => Credentials from environment
  username: System.get_env("DB_USER"),
  password: System.get_env("DB_PASSWORD")
])

# => Database backup module
defmodule DatabaseBackup do
  # => Exports transaction data to JSON
  def export_transactions(start_date, end_date) do
    # => Connecting to database
    {:ok, conn} = Postgrex.start_link(Application.get_env(:postgrex, :config))

    # => Querying transactions in date range
    query = """
    SELECT id, amount, created_at
    FROM transactions
    WHERE created_at BETWEEN $1 AND $2
    """

    # => Executing query with parameters
    {:ok, result} = Postgrex.query(conn, query, [start_date, end_date])
    # => result.rows contains result data

    # => Closing connection
    GenServer.stop(conn)

    # => Returns: list of transaction rows
    result.rows
  end
end

# => Parsing command-line arguments
[start_date, end_date] = System.argv()

# => Running export and printing results
DatabaseBackup.export_transactions(start_date, end_date)
|> IO.inspect()
```

This operational script installs the Postgrex dependency, connects to a production database, and exports transaction data. The lockfile ensures the script uses the same tested dependency version in production that developers used locally.

Livebook notebooks became more powerful with improved Mix.install. Data scientists could create notebooks with complex dependencies (Nx, Axon, Explorer) without managing separate Mix projects. The notebook file itself declared its dependencies, making notebooks portable and reproducible.

## Registry Partitions

Registry provides process name registration with property storage. Applications use Registry to locate processes by name or query processes by properties. High-throughput scenarios (thousands of registrations per second) could experience contention on Registry's internal ETS tables.

Elixir 1.13 introduced partition support, distributing Registry operations across multiple ETS tables to reduce contention:

```elixir
# File: application.ex
defmodule FinancialPlatform.Application do
  use Application

  # => Application start callback
  def start(_type, _args) do
    children = [
      # => Registry with 8 partitions for scalability
      {Registry, keys: :unique, name: FinancialPlatform.ProcessRegistry,
                 partitions: System.schedulers_online()},
      # => System.schedulers_online() returns CPU core count
      # => Creating one partition per core for optimal parallelism

      # Other supervisors...
    ]

    # => Supervising children with one_for_one strategy
    opts = [strategy: :one_for_one, name: FinancialPlatform.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
```

```elixir
# File: transaction_processor.ex
defmodule FinancialPlatform.TransactionProcessor do
  # => Starts a transaction processor for specific account
  def start_processor(account_id) when is_integer(account_id) do
    # => Generating process name from account ID
    name = {:via, Registry, {FinancialPlatform.ProcessRegistry, account_id}}

    # => Starting GenServer with Registry name
    GenServer.start_link(__MODULE__, account_id, name: name)
    # => Registry automatically routes to correct partition based on key hash
  end

  # => Looks up processor for account
  def lookup_processor(account_id) do
    # => Querying Registry for process
    case Registry.lookup(FinancialPlatform.ProcessRegistry, account_id) do
      # => Process found, returning PID
      [{pid, _value}] -> {:ok, pid}
      # => Process not found
      [] -> {:error, :not_found}
    end
  end

  # GenServer callbacks...
  # => init/1, handle_call/3, etc.
end
```

Without partitions, all Registry operations contended on a single ETS table. A system processing 10,000 registrations per second across 16 CPU cores would serialize these operations at the ETS table, underutilizing available parallelism.

With partitions matching CPU core count, Registry distributes registrations across 16 separate ETS tables. Each registration hashes its key to determine which partition handles it. Operations on different partitions proceed in parallel without contention.

The hashing algorithm ensures consistent routing - the same key always maps to the same partition. This allows lookup operations to query only the relevant partition rather than searching all partitions.

**Partition count selection:**

- **`System.schedulers_online()`** - Default recommendation, one partition per CPU core
- **Fixed number** - Use specific count (4, 8, 16) for predictable scaling
- **Power of 2** - Slightly more efficient hashing, but not required

```elixir
# Measuring Registry performance with partitions
# => Running benchmark for registration throughput

# Without partitions
{time_no_partition, _} = :timer.tc(fn ->
  # => Starting 10,000 processes with single partition
  Enum.each(1..10_000, fn id ->
    # => Each registration contends on single ETS table
    Registry.start_link(keys: :unique, name: :"registry_#{id}")
  end)
end)

# With 8 partitions
{time_with_partitions, _} = :timer.tc(fn ->
  # => Starting 10,000 processes with 8 partitions
  Enum.each(1..10_000, fn id ->
    # => Registrations distributed across 8 partitions
    Registry.start_link(keys: :unique, name: :"registry_part_#{id}",
                       partitions: 8)
  end)
end)

# => Calculating speedup ratio
# => time_no_partition / time_with_partitions
# => Typical results show 3-5x speedup with partitions
```

Phoenix PubSub, Phoenix Presence, and other libraries that built on Registry benefited automatically from partition support when upgrading to Elixir 1.13. Applications seeing Registry-related contention in production could tune partition counts for their workload characteristics.

## Other Improvements

**Kernel Enhancements:**

The `tap/2` and `then/2` functions gained improved documentation clarifying their different use cases. Both functions pipeline values, but with different semantics:

```elixir
# => Processing payment transaction
amount = 15000
# => amount: 15000 cents = $150.00

# Using tap/2 for side effects
# => tap returns the original value unchanged
result = amount
|> tap(fn amt ->
  # => Logging transaction (side effect)
  # => amt is 15000
  Logger.info("Processing payment of $#{amt / 100}")
  # => Return value ignored by tap
end)
|> Decimal.new()
# => result is Decimal for 15000
# => tap didn't modify the value

# Using then/2 for transformation
# => then returns the function's return value
converted = amount
|> then(fn amt ->
  # => Converting to dollars
  # => amt is 15000 cents
  amt / 100
  # => Returns: 150.0 (dollars)
end)
|> Decimal.new()
# => converted is Decimal for 150.0
# => then transformed the value
```

**Enum Improvements:**

`Enum.zip/2` gained performance optimizations for list inputs. Financial calculations processing parallel lists of debits and credits became more efficient:

```elixir
# => Lists of debits and credits for accounts
debits = [100, 250, 500, 150]
# => debit amounts in cents
credits = [50, 250, 300, 200]
# => credit amounts in cents

# => Zipping to calculate net positions
net_positions = Enum.zip(debits, credits)
|> Enum.map(fn {debit, credit} ->
  # => debit and credit are integers
  # => Calculating net: debit minus credit
  net = debit - credit
  # => net is positive for debit position, negative for credit
  net
end)
# => net_positions: [50, 0, 200, -50] (cents)
# => Optimized zip/2 processes this faster in 1.13
```

**Calendar Updates:**

`DateTime` gained better support for parsing ISO 8601 durations. Financial applications tracking transaction timestamps and calculating time-based fees benefited from improved parsing:

```elixir
# => Parsing ISO 8601 duration string
duration_str = "P30D"
# => P30D means "Period of 30 Days"

# => Converting to seconds
# => 30 days = 2,592,000 seconds
seconds = 30 * 24 * 60 * 60

# => Calculating interest accrual period
transaction_date = ~U[2021-12-15 10:30:00Z]
# => Starting date for interest calculation

# => Adding duration to get maturity date
maturity_date = DateTime.add(transaction_date, seconds, :second)
# => maturity_date: ~U[2022-01-14 10:30:00Z]
# => 30 days after transaction date

# => Better duration parsing enabled by 1.13 improvements
```

## Upgrade Guidance

Upgrading from Elixir 1.12 to 1.13 introduced no breaking changes for most applications. The semantic recompilation feature activated automatically without configuration changes.

**Update Mix Dependencies:**

```elixir
# File: mix.exs
def project do
  [
    app: :financial_platform,
    # => Updating Elixir version requirement
    elixir: "~> 1.13",
    # => Version constraint allows 1.13.x releases
    # => Previous: elixir: "~> 1.12"
    start_permanent: Mix.env() == :prod,
    deps: deps()
  ]
end
```

**Test Compilation Speed:**

After upgrading, measure compilation times to verify semantic recompilation benefits:

```bash
# Clean build to establish baseline
mix clean
# => Removes all compiled beam files
# => Forces full recompilation

# Time full compilation
time mix compile
# => First compile: full build of all modules
# => Baseline time varies by project size

# Make minor documentation change
# Edit a module's @moduledoc
# => Modify only documentation, no code changes

# Time incremental compilation
time mix compile
# => Second compile: only changed files recompile
# => Should be significantly faster with semantic recompilation
# => Modules depending on changed module may not recompile
```

Large codebases (500+ modules) typically saw 50-70% reduction in incremental compilation time after documentation changes. Projects with deep dependency trees benefited most from semantic recompilation.

**Update Registry Configuration:**

Applications experiencing Registry contention can enable partitions:

```elixir
# File: application.ex
children = [
  # => Old Registry configuration (single partition)
  # {Registry, keys: :unique, name: MyApp.Registry}

  # => New Registry configuration with partitions
  {Registry, keys: :unique, name: MyApp.Registry,
             partitions: System.schedulers_online()}
  # => System.schedulers_online() typically returns 8-16 on server hardware
]
```

This change requires no modifications to Registry usage code. The partition routing happens transparently in the Registry implementation.

**Evaluate Mix.install Opportunities:**

Teams maintaining operational scripts can migrate to Mix.install:

```elixir
# Before: Required separate Mix project
# Project structure:
# - reporting_scripts/
#   - mix.exs
#   - lib/report_generator.ex
#   - config/config.exs

# After: Single self-contained script
# File: generate_report.exs
Mix.install([{:jason, "~> 1.2"}])

defmodule ReportGenerator do
  # => Report generation logic inline
end

ReportGenerator.run()
# => Entire script in single file with dependencies
```

This simplified deployment and maintenance of operational tooling. Scripts became easier to share and version alongside application code.

**Testing Strategy:**

Run your existing test suite against Elixir 1.13 to verify compatibility:

```bash
# => Install Elixir 1.13 via version manager
asdf install elixir 1.13.4

# => Switch to 1.13 for testing
asdf local elixir 1.13.4

# => Fetch and compile dependencies
mix deps.get
mix deps.compile

# => Run full test suite
mix test
# => Verifying no behavioral changes
# => All tests should pass without modification

# => Run tests with warnings as errors
mix test --warnings-as-errors
# => Catching any deprecation warnings
# => Elixir 1.13 introduced no new deprecations
```

Most projects upgraded to Elixir 1.13 without any code changes. The release focused on performance and tooling improvements rather than language changes, making it one of the smoothest upgrades in Elixir's history.
