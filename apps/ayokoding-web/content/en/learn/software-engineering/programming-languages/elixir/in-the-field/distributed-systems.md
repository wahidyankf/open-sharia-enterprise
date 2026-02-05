---
title: "Distributed Systems"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000032
description: "From single-node BEAM to distributed Erlang clustering with libcluster and Horde for production-grade distributed applications"
tags: ["elixir", "distributed-systems", "clustering", "libcluster", "horde", "erlang"]
prev: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/hot-code-upgrades"
next: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/mix-build-tool"
---

**How do you scale Elixir applications across multiple machines?** This guide teaches distributed systems patterns from single-node BEAM through distributed Erlang clustering, showing production solutions with libcluster for automatic node discovery and Horde for distributed process management.

## Why Distributed Systems Matter

Single-node applications have fundamental limitations:

- **Scaling ceiling** - Single machine CPU and memory limits
- **Single point of failure** - Hardware failure stops entire application
- **Geographic distribution** - Cannot place nodes close to users
- **Load distribution** - Cannot spread work across machines
- **Hot upgrades** - Difficult without redundancy
- **Availability requirements** - Cannot achieve high availability on single node

**Production systems require distribution** for scalability, fault tolerance, and high availability.

## Financial Domain Examples

Examples use distributed donation processing scenarios:

- **Distributed donation processing** - Process donations across multiple nodes
- **Global process registry** - Track donation processors across cluster
- **Partition tolerance** - Handle network splits between donation centers
- **Consistent hashing** - Distribute donations predictably across nodes

These demonstrate distributed patterns with real financial operations.

## Standard Library - Single Node BEAM

### Process Communication on Single Node

BEAM provides excellent concurrency on single node.

```elixir
# Single-node process communication
defmodule DonationProcessor do
  def start_link do
    pid = spawn_link(fn -> process_loop() end)
                                             # => Process on local node
                                             # => Type: pid()
    {:ok, pid}
  end

  defp process_loop do
    receive do
      {:process, donation} ->
        zakat = donation.amount * 0.025      # => Calculate 2.5% Zakat
                                             # => Type: float()
        IO.puts("Processed: $#{donation.amount}, Zakat: $#{zakat}")
                                             # => Output to console
        process_loop()                       # => Continue processing
    end
  end
end

# Usage - single node only
{:ok, pid} = DonationProcessor.start_link()
# => pid: Process identifier (local)
# => Only accessible on this node

send(pid, {:process, %{amount: 1000}})
# => Sends message to process
# => Output: Processed: $1000, Zakat: $25.0
# => Works only on same node
```

Process communication works perfectly within single node.

### Named Processes with Registry

```elixir
# Register process with name on single node
defmodule DonationRegistry do
  def start_link do
    Registry.start_link(keys: :unique, name: __MODULE__)
                                             # => Local registry on this node
                                             # => Type: {:ok, pid()}
  end

  def register_processor(donation_id) do
    pid = spawn(fn ->
      receive do
        {:process, amount} ->
          zakat = amount * 0.025             # => Calculate Zakat
          IO.puts("Donation #{donation_id}: $#{amount}, Zakat: $#{zakat}")
                                             # => Process donation
      end
    end)

    Registry.register(__MODULE__, donation_id, pid)
                                             # => Register with ID
                                             # => Only visible on local node
    {:ok, pid}
  end

  def lookup(donation_id) do
    case Registry.lookup(__MODULE__, donation_id) do
      [{pid, _}] -> {:ok, pid}               # => Found on local node
      [] -> {:error, :not_found}             # => Not found
    end
  end
end

# Usage
{:ok, _} = DonationRegistry.start_link()
{:ok, pid} = DonationRegistry.register_processor("DON-001")
# => Registered on local node only

{:ok, found_pid} = DonationRegistry.lookup("DON-001")
# => found_pid: Process on local node
# => Works only on this node
```

Registry works perfectly for single node but doesn't distribute.

## Limitations of Single Node

### Problem 1: No Distribution

```elixir
# Cannot access processes on other nodes
# Node A:
{:ok, pid} = DonationProcessor.start_link()
Process.register(pid, :donation_processor)   # => Registered on Node A only
                                             # => Not visible to Node B

# Node B (different machine):
send(:donation_processor, {:process, %{amount: 1000}})
# => Error: :noproc (no process)
# => Cannot find process on Node A
# => No cross-node communication
```

Processes registered on one machine invisible to others.

### Problem 2: Single Point of Failure

```elixir
# Single node crash stops all processing
# Only one node running DonationProcessor
{:ok, pid} = DonationProcessor.start_link()

# If this node crashes:
# - All donation processing stops
# - No automatic failover
# - Manual intervention required
# - Downtime until restart
```

Hardware failure means complete system failure.

### Problem 3: Scaling Limits

```elixir
# CPU and memory bounded by single machine
# Processing 10,000 donations:
Enum.each(1..10_000, fn i ->
  DonationProcessor.process(%{amount: i * 100})
                                             # => All on single machine
                                             # => Limited by single CPU
                                             # => Cannot scale horizontally
end)
# => Hits single machine limits
# => Cannot add more machines to help
```

Cannot scale beyond single machine capacity.

### Problem 4: No Geographic Distribution

```elixir
# All processing on single datacenter
# Users in Asia, Europe, North America
# All requests go to single US datacenter
# - High latency for distant users
# - Cannot place nodes near users
# - No geographic redundancy
```

Single location means poor global performance.

## Distributed Erlang - Built-in Clustering

### Connecting Nodes

BEAM includes distributed Erlang for node clustering.

```elixir
# Start nodes with distributed names
# Terminal 1:
iex --sname node1 --cookie secret_token

# Terminal 2:
iex --sname node2 --cookie secret_token

# Connect nodes (in node2):
Node.connect(:"node1@hostname")              # => Connects to node1
                                             # => Returns: true
                                             # => Forms cluster

Node.list()                                  # => [:"node1@hostname"]
                                             # => Shows connected nodes
                                             # => Type: [atom()]

# Now nodes can communicate
Node.spawn(:"node1@hostname", fn ->
  IO.puts("Running on node1")                # => Executes on remote node
end)
# => Spawns process on node1
# => Cross-node process creation
```

Distributed Erlang enables cross-node communication.

### Global Process Registry

```elixir
# :global registry spans cluster
defmodule DistributedProcessor do
  def start_link(node_name) do
    pid = spawn_link(fn -> process_loop(node_name) end)
                                             # => Local process

    # Register globally across cluster
    :global.register_name(:donation_processor, pid)
                                             # => Visible to all nodes
                                             # => Type: :yes | :no
    {:ok, pid}
  end

  defp process_loop(node_name) do
    receive do
      {:process, donation} ->
        zakat = donation.amount * 0.025
        IO.puts("[#{node_name}] Processed: $#{donation.amount}, Zakat: $#{zakat}")
                                             # => Shows which node processed
        process_loop(node_name)
    end
  end

  def send_to_processor(donation) do
    case :global.whereis_name(:donation_processor) do
      :undefined ->
        {:error, :not_found}                 # => No registered processor

      pid ->
        send(pid, {:process, donation})      # => Send to registered process
                                             # => Works across nodes
        :ok
    end
  end
end

# Node1:
{:ok, _pid} = DistributedProcessor.start_link("Node1")
# => Registered globally

# Node2 (different machine):
DistributedProcessor.send_to_processor(%{amount: 1000})
# => Finds process on Node1
# => Sends message across network
# => Output on Node1: [Node1] Processed: $1000, Zakat: $25.0
```

:global registry provides cluster-wide process discovery.

### pg Module for Process Groups

```elixir
# pg (Process Groups) for distributed groups
defmodule DonationGroup do
  def start_processor(region) do
    pid = spawn(fn ->
      receive do
        {:process, donation} ->
          zakat = donation.amount * 0.025
          IO.puts("[#{region}] Processed: $#{donation.amount}")
                                             # => Regional processing
      end
    end)

    # Join process group
    :pg.join(:donation_processors, region, pid)
                                             # => Add to regional group
                                             # => Distributed across cluster
    {:ok, pid}
  end

  def broadcast_to_region(region, donation) do
    members = :pg.get_members(:donation_processors, region)
                                             # => Get all processes in region
                                             # => Across all nodes
                                             # => Type: [pid()]

    Enum.each(members, fn pid ->
      send(pid, {:process, donation})        # => Send to each processor
    end)

    {:ok, length(members)}                   # => Count of processors
  end
end

# Node1:
{:ok, _} = DonationGroup.start_processor("US-East")
# => Processor joins US-East group

# Node2:
{:ok, _} = DonationGroup.start_processor("US-East")
# => Another processor in same region

# Node3:
DonationGroup.broadcast_to_region("US-East", %{amount: 1000})
# => Broadcasts to all US-East processors
# => Both Node1 and Node2 receive message
# => Type: {:ok, 2}
```

pg module enables distributed process groups.

## libcluster - Automatic Node Discovery

### Problem with Manual Connection

```elixir
# Manual Node.connect() has issues:
# - Must know node names in advance
# - Hardcoded hostnames
# - No automatic discovery
# - Manual reconnection on failure
# - Doesn't handle dynamic cloud environments
```

Production needs automatic node discovery.

### libcluster Solution

```elixir
# mix.exs
defp deps do
  [
    {:libcluster, "~> 3.3"}                  # => Automatic clustering library
  ]
end

# application.ex
defmodule Finance.Application do
  use Application

  def start(_type, _args) do
    topologies = [
      donation_cluster: [
        strategy: Cluster.Strategy.Epmd,     # => Strategy for node discovery
                                             # => Epmd: Erlang Port Mapper Daemon
        config: [
          hosts: [
            :"donation@node1.example.com",   # => Known nodes
            :"donation@node2.example.com",
            :"donation@node3.example.com"
          ]
        ]
      ]
    ]

    children = [
      {Cluster.Supervisor, [topologies, [name: Finance.ClusterSupervisor]]},
                                             # => Starts cluster supervisor
                                             # => Automatically connects nodes
      # Other children...
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
```

libcluster automatically discovers and connects nodes.

### Kubernetes Strategy

```elixir
# libcluster with Kubernetes for cloud environments
topologies = [
  k8s_donation_cluster: [
    strategy: Cluster.Strategy.Kubernetes,   # => Kubernetes-aware strategy
    config: [
      mode: :dns,                            # => DNS-based discovery
      kubernetes_node_basename: "donation",  # => Service name prefix
      kubernetes_selector: "app=donation-processor",
                                             # => Label selector
      kubernetes_namespace: "finance",       # => Namespace
      polling_interval: 10_000               # => Check every 10 seconds
    ]
  ]
]

# libcluster queries Kubernetes API
# - Discovers pods matching selector
# - Extracts pod IPs
# - Forms erlang node names
# - Automatically connects
# - Reconnects on pod changes
```

Kubernetes strategy handles dynamic cloud environments.

### Gossip Strategy for DNS-less

```elixir
# Gossip strategy for environments without DNS
topologies = [
  gossip_cluster: [
    strategy: Cluster.Strategy.Gossip,       # => UDP multicast gossip
    config: [
      port: 45892,                           # => UDP port for gossip
      multicast_addr: "230.1.1.251",         # => Multicast address
      multicast_ttl: 1,                      # => Time-to-live
      secret: "secret_token"                 # => Shared secret for security
    ]
  ]
]

# Nodes broadcast presence via UDP multicast
# - No DNS required
# - No configuration of node names
# - Automatic peer discovery
# - Good for local development
```

Gossip strategy works without DNS infrastructure.

## Horde - Distributed Registry and Supervisor

### Problem with :global

```elixir
# :global registry has limitations:
# 1. No supervision (processes not restarted)
# 2. No consistent hashing (uneven distribution)
# 3. Manual failover on node crash
# 4. Network partition causes split-brain
```

Production needs distributed supervision.

### Horde.Registry - Distributed Registry

```elixir
# mix.exs
defp deps do
  [
    {:horde, "~> 0.8"}                       # => Distributed process registry
  ]
end

# Start Horde.Registry in application
defmodule Finance.Application do
  use Application

  def start(_type, _args) do
    children = [
      {Cluster.Supervisor, [topologies, [name: Finance.ClusterSupervisor]]},
      {Horde.Registry, [name: Finance.DonationRegistry, keys: :unique]},
                                             # => Distributed registry
                                             # => Replicated across cluster
      # Other children...
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end

# Register process in Horde
defmodule DonationWorker do
  def start_link(donation_id) do
    pid = spawn_link(fn -> worker_loop(donation_id) end)
                                             # => Worker process

    # Register in distributed registry
    Horde.Registry.register(
      Finance.DonationRegistry,
      {:donation, donation_id},              # => Unique key
      pid                                    # => Process pid
    )
    # => Registered across entire cluster
    # => Any node can lookup

    {:ok, pid}
  end

  defp worker_loop(donation_id) do
    receive do
      {:process, amount} ->
        zakat = amount * 0.025
        IO.puts("Worker #{donation_id}: Processed $#{amount}")
                                             # => Process donation
        worker_loop(donation_id)
    end
  end

  def send_to_worker(donation_id, amount) do
    case Horde.Registry.lookup(Finance.DonationRegistry, {:donation, donation_id}) do
      [{pid, _}] ->
        send(pid, {:process, amount})        # => Send to worker
                                             # => Works across nodes
        :ok

      [] ->
        {:error, :not_found}                 # => Worker not registered
    end
  end
end

# Node1:
{:ok, _} = DonationWorker.start_link("DON-001")
# => Registered in Horde

# Node2:
DonationWorker.send_to_worker("DON-001", 1000)
# => Finds worker on Node1
# => Sends message across cluster
```

Horde.Registry provides cluster-wide process registry with CRDT consistency.

### Horde.DynamicSupervisor - Distributed Supervision

```elixir
# Start Horde.DynamicSupervisor
defmodule Finance.Application do
  use Application

  def start(_type, _args) do
    children = [
      {Cluster.Supervisor, [topologies, [name: Finance.ClusterSupervisor]]},
      {Horde.Registry, [name: Finance.DonationRegistry, keys: :unique]},
      {Horde.DynamicSupervisor,
       [
         name: Finance.DonationSupervisor,   # => Distributed supervisor
         strategy: :one_for_one,             # => Restart strategy
         members: :auto                      # => Auto-discover cluster members
       ]},
      # Other children...
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end

# Start supervised worker
defmodule DonationSupervisedWorker do
  use GenServer

  def start_link(donation_id) do
    GenServer.start_link(__MODULE__, donation_id,
      name: via_tuple(donation_id))          # => Named via Horde.Registry
  end

  defp via_tuple(donation_id) do
    {:via, Horde.Registry, {Finance.DonationRegistry, {:donation, donation_id}}}
                                             # => Registry via tuple
  end

  def init(donation_id) do
    {:ok, %{donation_id: donation_id, processed: 0}}
                                             # => Initial state
  end

  def handle_call({:process, amount}, _from, state) do
    zakat = amount * 0.025
    new_state = %{state | processed: state.processed + 1}
                                             # => Update processed count
    {:reply, {:ok, zakat}, new_state}
  end
end

# Start worker under Horde supervisor
defmodule DonationService do
  def start_worker(donation_id) do
    child_spec = %{
      id: donation_id,
      start: {DonationSupervisedWorker, :start_link, [donation_id]},
      restart: :transient                    # => Restart on failure
    }

    Horde.DynamicSupervisor.start_child(
      Finance.DonationSupervisor,
      child_spec
    )
    # => Worker started on some node
    # => Supervised by distributed supervisor
    # => Automatically restarted on failure
  end

  def process_donation(donation_id, amount) do
    GenServer.call(
      {:via, Horde.Registry, {Finance.DonationRegistry, {:donation, donation_id}}},
      {:process, amount}
    )
    # => Calls worker wherever it is
    # => Transparent cross-node communication
  end
end

# Usage - any node:
{:ok, _pid} = DonationService.start_worker("DON-001")
# => Started on some node in cluster

# Different node:
{:ok, zakat} = DonationService.process_donation("DON-001", 1000)
# => zakat: 25.0
# => Called across cluster transparently
```

Horde.DynamicSupervisor distributes and supervises processes across cluster.

## Partition Tolerance - CAP Theorem

### Network Partition Scenarios

```elixir
# Cluster splits into two partitions:
# Partition 1: [Node1, Node2]
# Partition 2: [Node3, Node4]

# Without partition handling:
# - Each partition thinks it's the full cluster
# - Duplicate processes may start
# - Split-brain scenario
# - Data inconsistency
```

Production must handle network partitions.

### Horde with CRDT Consistency

```elixir
# Horde uses CRDTs (Conflict-free Replicated Data Types)
# - Eventually consistent
# - Handles network partitions
# - Automatic reconciliation on heal
# - No split-brain

# When partition heals:
# - Horde detects partition
# - Syncs registry state
# - Removes duplicate processes
# - Converges to consistent state
# - No manual intervention
```

Horde's CRDT approach provides partition tolerance.

### Choosing Consistency Strategy

```elixir
# CAP Theorem: Choose 2 of 3
# - Consistency: All nodes see same data
# - Availability: System responds to requests
# - Partition tolerance: Works despite network split

# :global registry:
# - CP (Consistency + Partition tolerance)
# - Blocks on partition
# - Guarantees single registration
# - May be unavailable during split

# Horde:
# - AP (Availability + Partition tolerance)
# - Continues operating on partition
# - Eventually consistent
# - May have temporary duplicates
# - Converges on heal
```

Choose strategy based on requirements.

## Production Deployment Pattern

### Complete Distributed System

```elixir
# Complete production setup
defmodule Finance.Application do
  use Application

  def start(_type, _args) do
    # Configure libcluster
    topologies = [
      k8s_cluster: [
        strategy: Cluster.Strategy.Kubernetes.DNS,
        config: [
          service: "donation-service",       # => Kubernetes service name
          application_name: "finance",       # => Application name
          kubernetes_namespace: "production"
        ]
      ]
    ]

    children = [
      # 1. Cluster formation
      {Cluster.Supervisor, [topologies, [name: Finance.ClusterSupervisor]]},

      # 2. Distributed registry
      {Horde.Registry, [name: Finance.Registry, keys: :unique]},

      # 3. Distributed supervisor
      {Horde.DynamicSupervisor,
       [
         name: Finance.Supervisor,
         strategy: :one_for_one,
         members: :auto,
         distribution_strategy: Horde.UniformQuorumDistribution
                                             # => Balanced distribution
       ]},

      # 4. Application workers
      Finance.DonationRouter,                # => Routes requests to workers

      # Other children...
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end

# Donation router for load balancing
defmodule Finance.DonationRouter do
  use GenServer

  def start_link(_) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init(_) do
    {:ok, %{}}
  end

  def process_donation(donation) do
    # Start worker if needed
    worker_id = "donation_#{donation.id}"
    ensure_worker_started(worker_id)

    # Process via distributed registry
    GenServer.call(
      {:via, Horde.Registry, {Finance.Registry, worker_id}},
      {:process, donation}
    )
  end

  defp ensure_worker_started(worker_id) do
    case Horde.Registry.lookup(Finance.Registry, worker_id) do
      [] ->
        # Start new worker
        child_spec = %{
          id: worker_id,
          start: {DonationSupervisedWorker, :start_link, [worker_id]}
        }
        Horde.DynamicSupervisor.start_child(Finance.Supervisor, child_spec)

      [{_pid, _}] ->
        # Worker already exists
        :ok
    end
  end
end

# Usage - transparent distribution:
donation = %{id: "DON-123", amount: 1000}
{:ok, result} = Finance.DonationRouter.process_donation(donation)
# => Worker started on some node
# => Balanced across cluster
# => Supervised for fault tolerance
# => Transparent to caller
```

Complete distributed system with clustering, registry, and supervision.

## Decision Matrix

| Approach                 | Clustering | Registry         | Supervision | Use Case                    |
| ------------------------ | ---------- | ---------------- | ----------- | --------------------------- |
| **Single Node**          | ❌ None    | Registry (local) | ✅ Local    | Development, small apps     |
| **Distributed Erlang**   | ✅ Manual  | :global          | ❌ Manual   | Learning, simple clustering |
| **libcluster + :global** | ✅ Auto    | :global          | ❌ Manual   | Basic auto-clustering       |
| **libcluster + Horde**   | ✅ Auto    | Horde.Registry   | ✅ Horde    | Production distributed apps |

### Decision Guide

**Use Single Node When**:

- Development environment
- Low traffic (<10k requests/day)
- No HA requirements
- Single datacenter deployment

**Use Distributed Erlang When**:

- Learning distribution fundamentals
- Simple clustering needs
- Manual control required

**Use libcluster + :global When**:

- Need automatic clustering
- Simple process registry sufficient
- Manual supervision acceptable

**Use libcluster + Horde When**:

- Production systems
- High availability required
- Need distributed supervision
- Partition tolerance critical
- Automatic failover needed

## Best Practices

### 1. Start Single Node, Add Distribution Later

```elixir
# Good: Start simple
# 1. Build on single node first
# 2. Test thoroughly
# 3. Add distribution when needed
# 4. Distribution adds complexity

# Avoid: Starting distributed
# - Adds unnecessary complexity early
# - Harder to debug
# - May not need distribution
```

Add distribution only when single node insufficient.

### 2. Use libcluster for Clustering

```elixir
# Good: libcluster handles connection
topologies = [k8s_cluster: [strategy: Cluster.Strategy.Kubernetes.DNS, ...]]

# Avoid: Manual Node.connect()
Node.connect(:"node1@host")                  # => Manual, error-prone
```

Always use libcluster in production.

### 3. Prefer Horde over :global

```elixir
# Good: Horde for production
Horde.Registry.register(Finance.Registry, key, pid)

# Avoid: :global for production
:global.register_name(key, pid)              # => No partition tolerance
```

Horde provides better partition tolerance.

### 4. Monitor Cluster Health

```elixir
# Monitor cluster state
defmodule ClusterMonitor do
  use GenServer

  def init(_) do
    :net_kernel.monitor_nodes(true)          # => Enable node monitoring
    {:ok, %{}}
  end

  def handle_info({:nodeup, node}, state) do
    Logger.info("Node joined: #{node}")      # => Log joins
    {:noreply, state}
  end

  def handle_info({:nodedown, node}, state) do
    Logger.warn("Node left: #{node}")        # => Log departures
    # Trigger alerts, cleanup, etc.
    {:noreply, state}
  end
end
```

Monitor cluster health for operations.

### 5. Test Partition Scenarios

```elixir
# Test network partition handling
# Use :partisan for partition simulation
# Test split-brain scenarios
# Verify state convergence on heal
```

Test partition handling before production.

## Common Pitfalls

### Pitfall 1: Starting Distributed Too Early

```elixir
# Wrong: Distribution from day one
# - Added complexity
# - Harder debugging
# - May not need it

# Right: Add when needed
# - Start single node
# - Add distribution at scale
```

### Pitfall 2: Using :global in Production

```elixir
# Wrong: :global without partition handling
:global.register_name(:worker, pid)          # => Vulnerable to split-brain

# Right: Horde with CRDT
Horde.Registry.register(Registry, :worker, pid)
                                             # => Partition tolerant
```

### Pitfall 3: Ignoring Network Partitions

```elixir
# Wrong: Assuming network always works
# - Partitions happen in production
# - Must handle split-brain
# - Test partition scenarios

# Right: Plan for partitions
# - Use partition-tolerant tools
# - Monitor cluster health
# - Test failure modes
```

### Pitfall 4: No Cluster Monitoring

```elixir
# Wrong: No visibility into cluster state
# - Cannot detect issues
# - No alerting on node failure

# Right: Monitor actively
# - Log node joins/leaves
# - Alert on unexpected changes
# - Track process distribution
```

## Further Reading

**Related distributed topics**:

- [GenServer Patterns](/en/learn/software-engineering/programming-languages/elixir/in-the-field/genserver-patterns) - State management across nodes
- [Supervisor Trees](/en/learn/software-engineering/programming-languages/elixir/in-the-field/supervisor-trees) - Supervision strategies

**Production patterns**:

- [Deployment Strategies](/en/learn/software-engineering/programming-languages/elixir/in-the-field/deployment-strategies) - Deploying distributed apps
- [Best Practices](/en/learn/software-engineering/programming-languages/elixir/in-the-field/best-practices) - Production OTP patterns

## Summary

Distributed systems in Elixir follow clear progression:

1. **Single Node** - BEAM processes with local Registry
2. **Limitations** - No distribution, single point of failure, scaling ceiling
3. **Distributed Erlang** - Node clustering with :global registry
4. **Production** - libcluster for auto-clustering + Horde for distributed registry/supervisor

**Use libcluster** for automatic node discovery and clustering in production environments.

**Use Horde** for distributed process registry and supervision with partition tolerance.

**Consider CAP theorem** when choosing consistency vs. availability trade-offs.

Key insight: **Start single-node, add distribution when scaling demands it**. Distribution adds complexity but enables horizontal scaling and high availability.
