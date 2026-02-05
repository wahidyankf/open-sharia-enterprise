---
title: "Sistem Terdistribusi"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000032
description: "Dari BEAM single-node ke clustering Distributed Erlang dengan libcluster dan Horde untuk aplikasi terdistribusi tingkat production"
tags: ["elixir", "distributed-systems", "clustering", "libcluster", "horde", "erlang"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/upgrade-kode-panas"
next: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/alat-build-mix"
---

**Bagaimana cara scaling aplikasi Elixir di beberapa mesin?** Panduan ini mengajarkan pola sistem terdistribusi dari BEAM single-node melalui clustering Distributed Erlang, menunjukkan solusi production dengan libcluster untuk automatic node discovery dan Horde untuk distributed process management.

## Mengapa Sistem Terdistribusi Penting

Aplikasi single-node memiliki keterbatasan fundamental:

- **Batas scaling** - CPU dan memory single machine terbatas
- **Single point of failure** - Hardware failure menghentikan seluruh aplikasi
- **Distribusi geografis** - Tidak bisa menempatkan node dekat dengan pengguna
- **Distribusi load** - Tidak bisa menyebarkan kerja di beberapa mesin
- **Hot upgrades** - Sulit tanpa redundancy
- **Persyaratan availability** - Tidak bisa mencapai high availability di single node

**Sistem production memerlukan distribusi** untuk skalabilitas, fault tolerance, dan high availability.

## Contoh Domain Finansial

Contoh menggunakan skenario pemrosesan donasi terdistribusi:

- **Pemrosesan donasi terdistribusi** - Proses donasi di beberapa node
- **Global process registry** - Melacak processor donasi di cluster
- **Partition tolerance** - Menangani network split antar pusat donasi
- **Consistent hashing** - Mendistribusikan donasi secara predictable di node

Ini mendemonstrasikan pola terdistribusi dengan operasi finansial nyata.

## Standard Library - Single Node BEAM

### Komunikasi Process pada Single Node

BEAM menyediakan concurrency yang sangat baik di single node.

```elixir
# Komunikasi process single-node
defmodule DonationProcessor do
  def start_link do
    pid = spawn_link(fn -> process_loop() end)
                                             # => Process di local node
                                             # => Type: pid()
    {:ok, pid}
  end

  defp process_loop do
    receive do
      {:process, donation} ->
        zakat = donation.amount * 0.025      # => Hitung 2.5% Zakat
                                             # => Type: float()
        IO.puts("Diproses: $#{donation.amount}, Zakat: $#{zakat}")
                                             # => Output ke console
        process_loop()                       # => Lanjut processing
    end
  end
end

# Penggunaan - single node only
{:ok, pid} = DonationProcessor.start_link()
# => pid: Process identifier (local)
# => Hanya accessible di node ini

send(pid, {:process, %{amount: 1000}})
# => Kirim message ke process
# => Output: Diproses: $1000, Zakat: $25.0
# => Hanya bekerja di node yang sama
```

Komunikasi process bekerja sempurna dalam single node.

### Named Processes dengan Registry

```elixir
# Register process dengan name di single node
defmodule DonationRegistry do
  def start_link do
    Registry.start_link(keys: :unique, name: __MODULE__)
                                             # => Local registry di node ini
                                             # => Type: {:ok, pid()}
  end

  def register_processor(donation_id) do
    pid = spawn(fn ->
      receive do
        {:process, amount} ->
          zakat = amount * 0.025             # => Hitung Zakat
          IO.puts("Donasi #{donation_id}: $#{amount}, Zakat: $#{zakat}")
                                             # => Proses donasi
      end
    end)

    Registry.register(__MODULE__, donation_id, pid)
                                             # => Register dengan ID
                                             # => Hanya visible di local node
    {:ok, pid}
  end

  def lookup(donation_id) do
    case Registry.lookup(__MODULE__, donation_id) do
      [{pid, _}] -> {:ok, pid}               # => Ditemukan di local node
      [] -> {:error, :not_found}             # => Tidak ditemukan
    end
  end
end

# Penggunaan
{:ok, _} = DonationRegistry.start_link()
{:ok, pid} = DonationRegistry.register_processor("DON-001")
# => Terdaftar di local node saja

{:ok, found_pid} = DonationRegistry.lookup("DON-001")
# => found_pid: Process di local node
# => Hanya bekerja di node ini
```

Registry bekerja sempurna untuk single node tapi tidak terdistribusi.

## Keterbatasan Single Node

### Masalah 1: Tidak Ada Distribusi

```elixir
# Tidak bisa mengakses process di node lain
# Node A:
{:ok, pid} = DonationProcessor.start_link()
Process.register(pid, :donation_processor)   # => Terdaftar di Node A saja
                                             # => Tidak visible ke Node B

# Node B (mesin berbeda):
send(:donation_processor, {:process, %{amount: 1000}})
# => Error: :noproc (no process)
# => Tidak bisa menemukan process di Node A
# => Tidak ada komunikasi cross-node
```

Process yang terdaftar di satu mesin tidak terlihat oleh mesin lain.

### Masalah 2: Single Point of Failure

```elixir
# Single node crash menghentikan semua processing
# Hanya satu node yang menjalankan DonationProcessor
{:ok, pid} = DonationProcessor.start_link()

# Jika node ini crash:
# - Semua pemrosesan donasi berhenti
# - Tidak ada automatic failover
# - Perlu intervensi manual
# - Downtime sampai restart
```

Hardware failure berarti complete system failure.

### Masalah 3: Batas Scaling

```elixir
# CPU dan memory dibatasi oleh single machine
# Memproses 10,000 donasi:
Enum.each(1..10_000, fn i ->
  DonationProcessor.process(%{amount: i * 100})
                                             # => Semua di single machine
                                             # => Dibatasi oleh single CPU
                                             # => Tidak bisa scale horizontal
end)
# => Mencapai batas single machine
# => Tidak bisa menambah mesin lain untuk membantu
```

Tidak bisa scale melampaui kapasitas single machine.

### Masalah 4: Tidak Ada Distribusi Geografis

```elixir
# Semua processing di single datacenter
# Pengguna di Asia, Eropa, Amerika Utara
# Semua request ke single US datacenter
# - Latency tinggi untuk pengguna jauh
# - Tidak bisa menempatkan node dekat pengguna
# - Tidak ada redundancy geografis
```

Single location berarti performa global yang buruk.

## Distributed Erlang - Built-in Clustering

### Menghubungkan Node

BEAM menyertakan Distributed Erlang untuk node clustering.

```elixir
# Start node dengan distributed names
# Terminal 1:
iex --sname node1 --cookie secret_token

# Terminal 2:
iex --sname node2 --cookie secret_token

# Hubungkan node (di node2):
Node.connect(:"node1@hostname")              # => Terhubung ke node1
                                             # => Returns: true
                                             # => Membentuk cluster

Node.list()                                  # => [:"node1@hostname"]
                                             # => Menunjukkan connected nodes
                                             # => Type: [atom()]

# Sekarang node bisa berkomunikasi
Node.spawn(:"node1@hostname", fn ->
  IO.puts("Running on node1")                # => Eksekusi di remote node
end)
# => Spawn process di node1
# => Pembuatan process cross-node
```

Distributed Erlang mengaktifkan komunikasi cross-node.

### Global Process Registry

```elixir
# Registry :global mencakup cluster
defmodule DistributedProcessor do
  def start_link(node_name) do
    pid = spawn_link(fn -> process_loop(node_name) end)
                                             # => Local process

    # Register secara global di cluster
    :global.register_name(:donation_processor, pid)
                                             # => Visible ke semua node
                                             # => Type: :yes | :no
    {:ok, pid}
  end

  defp process_loop(node_name) do
    receive do
      {:process, donation} ->
        zakat = donation.amount * 0.025
        IO.puts("[#{node_name}] Diproses: $#{donation.amount}, Zakat: $#{zakat}")
                                             # => Menunjukkan node mana yang memproses
        process_loop(node_name)
    end
  end

  def send_to_processor(donation) do
    case :global.whereis_name(:donation_processor) do
      :undefined ->
        {:error, :not_found}                 # => Tidak ada processor terdaftar

      pid ->
        send(pid, {:process, donation})      # => Kirim ke registered process
                                             # => Bekerja across nodes
        :ok
    end
  end
end

# Node1:
{:ok, _pid} = DistributedProcessor.start_link("Node1")
# => Terdaftar secara global

# Node2 (mesin berbeda):
DistributedProcessor.send_to_processor(%{amount: 1000})
# => Menemukan process di Node1
# => Kirim message across network
# => Output di Node1: [Node1] Diproses: $1000, Zakat: $25.0
```

Registry :global menyediakan cluster-wide process discovery.

### Module pg untuk Process Groups

```elixir
# pg (Process Groups) untuk distributed groups
defmodule DonationGroup do
  def start_processor(region) do
    pid = spawn(fn ->
      receive do
        {:process, donation} ->
          zakat = donation.amount * 0.025
          IO.puts("[#{region}] Diproses: $#{donation.amount}")
                                             # => Regional processing
      end
    end)

    # Gabung ke process group
    :pg.join(:donation_processors, region, pid)
                                             # => Tambah ke regional group
                                             # => Terdistribusi di cluster
    {:ok, pid}
  end

  def broadcast_to_region(region, donation) do
    members = :pg.get_members(:donation_processors, region)
                                             # => Dapatkan semua process di region
                                             # => Across all nodes
                                             # => Type: [pid()]

    Enum.each(members, fn pid ->
      send(pid, {:process, donation})        # => Kirim ke tiap processor
    end)

    {:ok, length(members)}                   # => Jumlah processors
  end
end

# Node1:
{:ok, _} = DonationGroup.start_processor("US-East")
# => Processor bergabung ke US-East group

# Node2:
{:ok, _} = DonationGroup.start_processor("US-East")
# => Processor lain di region yang sama

# Node3:
DonationGroup.broadcast_to_region("US-East", %{amount: 1000})
# => Broadcast ke semua US-East processors
# => Node1 dan Node2 menerima message
# => Type: {:ok, 2}
```

Module pg mengaktifkan distributed process groups.

## libcluster - Automatic Node Discovery

### Masalah dengan Manual Connection

```elixir
# Manual Node.connect() memiliki masalah:
# - Harus tahu node names sebelumnya
# - Hostname hardcoded
# - Tidak ada automatic discovery
# - Reconnection manual saat failure
# - Tidak menangani dynamic cloud environments
```

Production memerlukan automatic node discovery.

### Solusi libcluster

```elixir
# mix.exs
defp deps do
  [
    {:libcluster, "~> 3.3"}                  # => Library automatic clustering
  ]
end

# application.ex
defmodule Finance.Application do
  use Application

  def start(_type, _args) do
    topologies = [
      donation_cluster: [
        strategy: Cluster.Strategy.Epmd,     # => Strategy untuk node discovery
                                             # => Epmd: Erlang Port Mapper Daemon
        config: [
          hosts: [
            :"donation@node1.example.com",   # => Node yang diketahui
            :"donation@node2.example.com",
            :"donation@node3.example.com"
          ]
        ]
      ]
    ]

    children = [
      {Cluster.Supervisor, [topologies, [name: Finance.ClusterSupervisor]]},
                                             # => Start cluster supervisor
                                             # => Otomatis menghubungkan nodes
      # Children lainnya...
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
```

libcluster otomatis menemukan dan menghubungkan node.

### Strategi Kubernetes

```elixir
# libcluster dengan Kubernetes untuk cloud environments
topologies = [
  k8s_donation_cluster: [
    strategy: Cluster.Strategy.Kubernetes,   # => Strategy Kubernetes-aware
    config: [
      mode: :dns,                            # => DNS-based discovery
      kubernetes_node_basename: "donation",  # => Service name prefix
      kubernetes_selector: "app=donation-processor",
                                             # => Label selector
      kubernetes_namespace: "finance",       # => Namespace
      polling_interval: 10_000               # => Cek setiap 10 detik
    ]
  ]
]

# libcluster query Kubernetes API
# - Menemukan pod yang cocok dengan selector
# - Ekstrak pod IPs
# - Membentuk erlang node names
# - Otomatis terhubung
# - Reconnect saat pod berubah
```

Strategi Kubernetes menangani dynamic cloud environments.

### Strategi Gossip untuk DNS-less

```elixir
# Strategi Gossip untuk environment tanpa DNS
topologies = [
  gossip_cluster: [
    strategy: Cluster.Strategy.Gossip,       # => UDP multicast gossip
    config: [
      port: 45892,                           # => UDP port untuk gossip
      multicast_addr: "230.1.1.251",         # => Multicast address
      multicast_ttl: 1,                      # => Time-to-live
      secret: "secret_token"                 # => Shared secret untuk security
    ]
  ]
]

# Node broadcast presence via UDP multicast
# - Tidak perlu DNS
# - Tidak perlu konfigurasi node names
# - Automatic peer discovery
# - Bagus untuk local development
```

Strategi Gossip bekerja tanpa infrastruktur DNS.

## Horde - Distributed Registry dan Supervisor

### Masalah dengan :global

```elixir
# Registry :global memiliki keterbatasan:
# 1. Tidak ada supervision (process tidak direstart)
# 2. Tidak ada consistent hashing (distribusi tidak merata)
# 3. Failover manual saat node crash
# 4. Network partition menyebabkan split-brain
```

Production memerlukan distributed supervision.

### Horde.Registry - Distributed Registry

```elixir
# mix.exs
defp deps do
  [
    {:horde, "~> 0.8"}                       # => Distributed process registry
  ]
end

# Start Horde.Registry di application
defmodule Finance.Application do
  use Application

  def start(_type, _args) do
    children = [
      {Cluster.Supervisor, [topologies, [name: Finance.ClusterSupervisor]]},
      {Horde.Registry, [name: Finance.DonationRegistry, keys: :unique]},
                                             # => Distributed registry
                                             # => Replikasi across cluster
      # Children lainnya...
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end

# Register process di Horde
defmodule DonationWorker do
  def start_link(donation_id) do
    pid = spawn_link(fn -> worker_loop(donation_id) end)
                                             # => Worker process

    # Register di distributed registry
    Horde.Registry.register(
      Finance.DonationRegistry,
      {:donation, donation_id},              # => Unique key
      pid                                    # => Process pid
    )
    # => Terdaftar di seluruh cluster
    # => Node mana pun bisa lookup

    {:ok, pid}
  end

  defp worker_loop(donation_id) do
    receive do
      {:process, amount} ->
        zakat = amount * 0.025
        IO.puts("Worker #{donation_id}: Diproses $#{amount}")
                                             # => Proses donasi
        worker_loop(donation_id)
    end
  end

  def send_to_worker(donation_id, amount) do
    case Horde.Registry.lookup(Finance.DonationRegistry, {:donation, donation_id}) do
      [{pid, _}] ->
        send(pid, {:process, amount})        # => Kirim ke worker
                                             # => Bekerja across nodes
        :ok

      [] ->
        {:error, :not_found}                 # => Worker tidak terdaftar
    end
  end
end

# Node1:
{:ok, _} = DonationWorker.start_link("DON-001")
# => Terdaftar di Horde

# Node2:
DonationWorker.send_to_worker("DON-001", 1000)
# => Menemukan worker di Node1
# => Kirim message across cluster
```

Horde.Registry menyediakan cluster-wide process registry dengan konsistensi CRDT.

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
      # Children lainnya...
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

# Start worker di bawah Horde supervisor
defmodule DonationService do
  def start_worker(donation_id) do
    child_spec = %{
      id: donation_id,
      start: {DonationSupervisedWorker, :start_link, [donation_id]},
      restart: :transient                    # => Restart saat failure
    }

    Horde.DynamicSupervisor.start_child(
      Finance.DonationSupervisor,
      child_spec
    )
    # => Worker dimulai di node tertentu
    # => Supervised oleh distributed supervisor
    # => Otomatis direstart saat failure
  end

  def process_donation(donation_id, amount) do
    GenServer.call(
      {:via, Horde.Registry, {Finance.DonationRegistry, {:donation, donation_id}}},
      {:process, amount}
    )
    # => Memanggil worker di mana pun lokasinya
    # => Komunikasi cross-node transparan
  end
end

# Penggunaan - node mana pun:
{:ok, _pid} = DonationService.start_worker("DON-001")
# => Dimulai di node tertentu di cluster

# Node berbeda:
{:ok, zakat} = DonationService.process_donation("DON-001", 1000)
# => zakat: 25.0
# => Dipanggil across cluster secara transparan
```

Horde.DynamicSupervisor mendistribusikan dan mensupervisi process across cluster.

## Partition Tolerance - CAP Theorem

### Skenario Network Partition

```elixir
# Cluster terbagi menjadi dua partition:
# Partition 1: [Node1, Node2]
# Partition 2: [Node3, Node4]

# Tanpa partition handling:
# - Tiap partition mengira dirinya adalah full cluster
# - Process duplikat mungkin dimulai
# - Skenario split-brain
# - Inkonsistensi data
```

Production harus menangani network partitions.

### Horde dengan Konsistensi CRDT

```elixir
# Horde menggunakan CRDTs (Conflict-free Replicated Data Types)
# - Eventually consistent
# - Menangani network partitions
# - Reconciliation otomatis saat heal
# - Tidak ada split-brain

# Saat partition heal:
# - Horde mendeteksi partition
# - Sync registry state
# - Menghapus process duplikat
# - Konvergen ke consistent state
# - Tidak perlu intervensi manual
```

Pendekatan CRDT Horde menyediakan partition tolerance.

### Memilih Strategi Konsistensi

```elixir
# CAP Theorem: Pilih 2 dari 3
# - Consistency: Semua node melihat data yang sama
# - Availability: Sistem merespons request
# - Partition tolerance: Bekerja meskipun network split

# Registry :global:
# - CP (Consistency + Partition tolerance)
# - Block saat partition
# - Menjamin single registration
# - Mungkin unavailable saat split

# Horde:
# - AP (Availability + Partition tolerance)
# - Terus beroperasi saat partition
# - Eventually consistent
# - Mungkin ada duplikat sementara
# - Konvergen saat heal
```

Pilih strategi berdasarkan requirements.

## Pola Production Deployment

### Sistem Terdistribusi Lengkap

```elixir
# Setup production lengkap
defmodule Finance.Application do
  use Application

  def start(_type, _args) do
    # Konfigurasi libcluster
    topologies = [
      k8s_cluster: [
        strategy: Cluster.Strategy.Kubernetes.DNS,
        config: [
          service: "donation-service",       # => Nama Kubernetes service
          application_name: "finance",       # => Nama application
          kubernetes_namespace: "production"
        ]
      ]
    ]

    children = [
      # 1. Pembentukan cluster
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
                                             # => Distribusi seimbang
       ]},

      # 4. Application workers
      Finance.DonationRouter,                # => Route requests ke workers

      # Children lainnya...
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end

# Donation router untuk load balancing
defmodule Finance.DonationRouter do
  use GenServer

  def start_link(_) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init(_) do
    {:ok, %{}}
  end

  def process_donation(donation) do
    # Start worker jika diperlukan
    worker_id = "donation_#{donation.id}"
    ensure_worker_started(worker_id)

    # Proses via distributed registry
    GenServer.call(
      {:via, Horde.Registry, {Finance.Registry, worker_id}},
      {:process, donation}
    )
  end

  defp ensure_worker_started(worker_id) do
    case Horde.Registry.lookup(Finance.Registry, worker_id) do
      [] ->
        # Start worker baru
        child_spec = %{
          id: worker_id,
          start: {DonationSupervisedWorker, :start_link, [worker_id]}
        }
        Horde.DynamicSupervisor.start_child(Finance.Supervisor, child_spec)

      [{_pid, _}] ->
        # Worker sudah ada
        :ok
    end
  end
end

# Penggunaan - distribusi transparan:
donation = %{id: "DON-123", amount: 1000}
{:ok, result} = Finance.DonationRouter.process_donation(donation)
# => Worker dimulai di node tertentu
# => Seimbang across cluster
# => Supervised untuk fault tolerance
# => Transparan untuk caller
```

Sistem terdistribusi lengkap dengan clustering, registry, dan supervision.

## Decision Matrix

| Pendekatan               | Clustering | Registry         | Supervision | Use Case                          |
| ------------------------ | ---------- | ---------------- | ----------- | --------------------------------- |
| **Single Node**          | ❌ None    | Registry (local) | ✅ Local    | Development, aplikasi kecil       |
| **Distributed Erlang**   | ✅ Manual  | :global          | ❌ Manual   | Learning, clustering sederhana    |
| **libcluster + :global** | ✅ Auto    | :global          | ❌ Manual   | Auto-clustering dasar             |
| **libcluster + Horde**   | ✅ Auto    | Horde.Registry   | ✅ Horde    | Aplikasi terdistribusi production |

### Panduan Keputusan

**Gunakan Single Node Saat**:

- Development environment
- Traffic rendah (<10k requests/hari)
- Tidak ada persyaratan HA
- Deployment single datacenter

**Gunakan Distributed Erlang Saat**:

- Belajar fundamental distribusi
- Kebutuhan clustering sederhana
- Perlu kontrol manual

**Gunakan libcluster + :global Saat**:

- Perlu automatic clustering
- Process registry sederhana cukup
- Supervision manual acceptable

**Gunakan libcluster + Horde Saat**:

- Sistem production
- Perlu high availability
- Perlu distributed supervision
- Partition tolerance kritis
- Perlu automatic failover

## Best Practices

### 1. Mulai Single Node, Tambahkan Distribusi Nanti

```elixir
# Bagus: Mulai sederhana
# 1. Build di single node dulu
# 2. Test secara menyeluruh
# 3. Tambahkan distribusi saat diperlukan
# 4. Distribusi menambah kompleksitas

# Hindari: Mulai terdistribusi
# - Menambah kompleksitas tidak perlu di awal
# - Lebih sulit debug
# - Mungkin tidak perlu distribusi
```

Tambahkan distribusi hanya saat single node tidak cukup.

### 2. Gunakan libcluster untuk Clustering

```elixir
# Bagus: libcluster menangani connection
topologies = [k8s_cluster: [strategy: Cluster.Strategy.Kubernetes.DNS, ...]]

# Hindari: Manual Node.connect()
Node.connect(:"node1@host")                  # => Manual, rawan error
```

Selalu gunakan libcluster di production.

### 3. Lebih Suka Horde daripada :global

```elixir
# Bagus: Horde untuk production
Horde.Registry.register(Finance.Registry, key, pid)

# Hindari: :global untuk production
:global.register_name(key, pid)              # => Tidak ada partition tolerance
```

Horde menyediakan partition tolerance yang lebih baik.

### 4. Monitor Cluster Health

```elixir
# Monitor cluster state
defmodule ClusterMonitor do
  use GenServer

  def init(_) do
    :net_kernel.monitor_nodes(true)          # => Aktifkan node monitoring
    {:ok, %{}}
  end

  def handle_info({:nodeup, node}, state) do
    Logger.info("Node joined: #{node}")      # => Log joins
    {:noreply, state}
  end

  def handle_info({:nodedown, node}, state) do
    Logger.warn("Node left: #{node}")        # => Log departures
    # Trigger alerts, cleanup, dll.
    {:noreply, state}
  end
end
```

Monitor cluster health untuk operasi.

### 5. Test Skenario Partition

```elixir
# Test partition handling
# Gunakan :partisan untuk simulasi partition
# Test skenario split-brain
# Verifikasi konvergensi state saat heal
```

Test partition handling sebelum production.

## Common Pitfalls

### Pitfall 1: Mulai Terdistribusi Terlalu Awal

```elixir
# Salah: Distribusi dari hari pertama
# - Kompleksitas bertambah
# - Debugging lebih sulit
# - Mungkin tidak perlu

# Benar: Tambahkan saat diperlukan
# - Mulai single node
# - Tambah distribusi saat scale
```

### Pitfall 2: Menggunakan :global di Production

```elixir
# Salah: :global tanpa partition handling
:global.register_name(:worker, pid)          # => Rentan terhadap split-brain

# Benar: Horde dengan CRDT
Horde.Registry.register(Registry, :worker, pid)
                                             # => Partition tolerant
```

### Pitfall 3: Mengabaikan Network Partitions

```elixir
# Salah: Asumsi network selalu bekerja
# - Partitions terjadi di production
# - Harus menangani split-brain
# - Test skenario partition

# Benar: Plan untuk partitions
# - Gunakan tools partition-tolerant
# - Monitor cluster health
# - Test failure modes
```

### Pitfall 4: Tidak Ada Cluster Monitoring

```elixir
# Salah: Tidak ada visibility ke cluster state
# - Tidak bisa deteksi masalah
# - Tidak ada alerting saat node failure

# Benar: Monitor secara aktif
# - Log node joins/leaves
# - Alert saat perubahan tak terduga
# - Track distribusi process
```

## Bacaan Lebih Lanjut

**Topik terdistribusi terkait**:

- [GenServer Patterns](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/pola-genserver) - State management across nodes
- [Supervisor Trees](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/pohon-supervisor) - Strategi supervision

**Pola production**:

- [Deployment Strategies](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/strategi-deployment) - Deploy aplikasi terdistribusi
- [Best Practices](/id/belajar/software-engineering/programming-languages/elixir/in-the-field/praktik-terbaik) - Pola OTP production

## Ringkasan

Sistem terdistribusi di Elixir mengikuti progresi yang jelas:

1. **Single Node** - BEAM process dengan Registry lokal
2. **Keterbatasan** - Tidak ada distribusi, single point of failure, batas scaling
3. **Distributed Erlang** - Node clustering dengan registry :global
4. **Production** - libcluster untuk auto-clustering + Horde untuk distributed registry/supervisor

**Gunakan libcluster** untuk automatic node discovery dan clustering di production environments.

**Gunakan Horde** untuk distributed process registry dan supervision dengan partition tolerance.

**Pertimbangkan CAP theorem** saat memilih trade-off consistency vs. availability.

Key insight: **Mulai single-node, tambahkan distribusi saat scaling membutuhkannya**. Distribusi menambah kompleksitas tapi mengaktifkan horizontal scaling dan high availability.
