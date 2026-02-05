---
title: "GraphQL dengan Absinthe"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000020
description: "Dari parsing GraphQL manual ke framework Absinthe untuk GraphQL API production-ready dengan schema, resolver, dan subscription real-time"
tags: ["elixir", "graphql", "absinthe", "api", "real-time", "subscriptions"]
prev: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/desain-api-rest"
next: "/id/belajar/software-engineering/programming-languages/elixir/in-the-field/autentikasi-otorisasi"
---

**Bagaimana cara membangun GraphQL API production di Elixir?** Panduan ini mengajarkan progresi dari parsing GraphQL manual hingga framework Absinthe, menunjukkan bagaimana definisi schema, resolver, dan subscription memungkinkan API type-safe dan real-time untuk aplikasi modern.

## Mengapa Ini Penting

GraphQL menyediakan API fleksibel dan efisien di mana klien menentukan data apa yang mereka butuhkan. Kebutuhan dunia nyata:

- **Type safety** - Tipe yang didefinisikan schema mencegah runtime error
- **Query efisien** - Klien hanya request field yang dibutuhkan
- **Update real-time** - Subscription push perubahan ke klien
- **Query nested** - Resolve relasi kompleks dalam satu request
- **Evolusi API** - Tambah field tanpa merusak klien yang ada

Skenario dunia nyata yang membutuhkan GraphQL dengan Absinthe:

- **Platform donasi** - Update kampanye real-time, data nested donor/transaksi
- **E-commerce** - Katalog produk dengan varian, inventori, review
- **Platform sosial** - Feed user dengan post, komentar, reaksi
- **Dashboard keuangan** - Saldo akun, transaksi, harga real-time
- **Aplikasi mobile** - Loading data efisien, sinkronisasi offline

Pertanyaan production: Kapan harus gunakan GraphQL daripada REST? Bagaimana Absinthe menyediakan infrastruktur GraphQL production-ready? Jawabannya tergantung pada kebutuhan fleksibilitas klien dan real-time Anda.

## GraphQL Manual - Keterbatasan Standard Library

Standard library Elixir tidak memiliki dukungan GraphQL. Implementasi manual diperlukan.

### Parsing Query GraphQL Manual

```elixir
# Parsing query GraphQL manual
query_string = """
{
  campaign(id: "ramadan_2026") {
    name
    goal
    raised
  }
}
"""
# => String query GraphQL
# => Masalah: Tidak ada parser built-in

# Parsing string manual (rapuh)
defmodule ManualGraphQL do
  # => Implementasi manual naif
  def parse_query(query_string) do
    # => Ekstrak operasi dan field
    # Masalah: Parsing berbasis regex
    # => Rapuh, rawan error

    cond do
      String.contains?(query_string, "campaign(id:") ->
        # => Ekstrak ID campaign
        id = extract_id(query_string)
        # => Mengembalikan string ID campaign
        fields = extract_fields(query_string)
        # => Mengembalikan list field yang diminta

        {:campaign, id, fields}
        # => Mengembalikan tuple query yang di-parse

      true ->
        {:error, :unknown_query}
        # => Tipe query tidak didukung
    end
  end

  defp extract_id(query) do
    # => Ekstraksi ID berbasis regex
    # Masalah: Parsing string yang rapuh
    ~r/id:\s*"([^"]+)"/
    |> Regex.run(query)
    |> Enum.at(1)
    # => Mengembalikan ID yang ditangkap atau nil
  end

  defp extract_fields(query) do
    # => Ekstrak field yang diminta
    # Masalah: Tidak bisa handle field nested
    query
    |> String.split(["{", "}"])
    |> Enum.at(2)
    # => Ambil konten antara {} kedua
    |> String.split("\n")
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    # => Mengembalikan list nama field
  end
end
# => Mengembalikan module

{:campaign, id, fields} = ManualGraphQL.parse_query(query_string)
# => Mengembalikan {:campaign, "ramadan_2026", ["name", "goal", "raised"]}
# => Rapuh, mudah rusak
```

### Keterbatasan Pendekatan Manual

```elixir
# Masalah 1: Tidak ada validasi tipe
query_invalid = """
{
  campaign(id: 123) {
    # => ID harus string, bukan integer
    invalid_field
    # => Field tidak ada
  }
}
"""
# => Tidak ada validasi compile-time atau parse-time
# => Error ditemukan di runtime

# Masalah 2: Tidak bisa handle query nested
query_nested = """
{
  campaign(id: "ramadan_2026") {
    name
    donations {
      # => Field nested
      donor
      amount
    }
  }
}
"""
# => Parser manual tidak bisa handle nesting
# => Memerlukan parsing rekursif kompleks

# Masalah 3: Tidak ada definisi schema
# => Tidak ada single source of truth untuk struktur API
# => Informasi tipe tersebar di kode resolver
# => Tidak bisa generate dokumentasi otomatis

# Masalah 4: Tidak ada introspection
# => Klien tidak bisa discover query/field yang tersedia
# => Query introspection GraphQL tidak didukung
# => Merusak tooling GraphQL (GraphiQL, Playground)

# Masalah 5: Tidak ada proteksi N+1 query
# => Setiap field nested trigger query database terpisah
# => 100 donasi = 100+ query database
# => Performa menurun drastis
```

Masalah production dengan GraphQL manual:

- **Tidak ada parsing query** - Harus implementasi dari awal (kompleks)
- **Tidak ada validasi tipe** - Runtime error untuk query invalid
- **Tidak ada definisi schema** - Tidak bisa enforce struktur
- **Tidak ada query nested** - Kompleks untuk implementasi dengan benar
- **Tidak ada introspection** - Merusak tooling GraphQL
- **Tidak ada batching** - Masalah N+1 query

## Absinthe - Framework GraphQL Production

Absinthe menyediakan implementasi GraphQL lengkap untuk Elixir.

### Instalasi Absinthe

```elixir
# Tambahkan ke dependency mix.exs
defp deps do
  [
    {:absinthe, "~> 1.7"},
    # => Core Absinthe GraphQL
    {:absinthe_plug, "~> 1.5"},
    # => Integrasi Phoenix/Plug
    {:absinthe_phoenix, "~> 2.0"}
    # => Phoenix channels untuk subscription
  ]
end
# => Mengembalikan list dependency

# Install dependency
# $ mix deps.get
# => Fetch package Absinthe
```

### Definisi Schema GraphQL

```elixir
defmodule DonationPlatform.Schema do
  # => Definisi schema GraphQL
  use Absinthe.Schema
  # => Import DSL Absinthe

  # Define tipe Campaign
  object :campaign do
    # => Tipe object GraphQL
    field :id, non_null(:id)
    # => Field ID wajib
    # => non_null enforce keberadaan

    field :name, non_null(:string)
    # => Field string wajib

    field :goal, non_null(:integer)
    # => Jumlah goal dalam sen

    field :raised, non_null(:integer)
    # => Jumlah terkumpul saat ini

    field :currency, non_null(:string)
    # => Kode mata uang (USD, EUR, dll)

    field :donations, list_of(:donation) do
      # => List donasi nested
      # => Mengembalikan list object Donation
      resolve &Resolvers.Campaign.donations/3
      # => Fungsi resolver untuk field donations
    end
  end
  # => Mengembalikan definisi tipe

  # Define tipe Donation
  object :donation do
    # => Tipe object Donation
    field :id, non_null(:id)
    field :amount, non_null(:integer)
    # => Jumlah donasi dalam sen

    field :donor, non_null(:string)
    # => Nama donor

    field :timestamp, non_null(:string)
    # => Timestamp ISO 8601

    field :campaign, :campaign do
      # => Referensi campaign nested
      resolve &Resolvers.Donation.campaign/3
      # => Resolver untuk parent campaign
    end
  end
  # => Mengembalikan definisi tipe

  # Define query root
  query do
    # => Tipe query root
    field :campaign, :campaign do
      # => Field query campaign
      arg :id, non_null(:id)
      # => Argumen ID wajib

      resolve &Resolvers.Campaign.get/3
      # => Fungsi resolver
    end

    field :campaigns, list_of(:campaign) do
      # => List semua campaign
      resolve &Resolvers.Campaign.list/3
    end
  end
  # => Mengembalikan definisi query
end
# => Mengembalikan module schema
```

Schema mendefinisikan tipe, field, dan resolver dengan validasi compile-time.

### Implementasi Resolver

```elixir
defmodule DonationPlatform.Resolvers.Campaign do
  # => Fungsi resolver Campaign

  def get(_parent, %{id: id}, _resolution) do
    # => Resolve campaign tunggal
    # => Args: parent (nil untuk root), map argumen, konteks resolution

    case DonationDB.get_campaign(id) do
      nil ->
        {:error, "Campaign not found"}
        # => Tuple error untuk not found

      campaign ->
        {:ok, campaign}
        # => Tuple sukses dengan data campaign
    end
  end
  # => Mengembalikan fungsi resolver

  def list(_parent, _args, _resolution) do
    # => Resolve list campaign
    campaigns = DonationDB.list_campaigns()
    # => Fetch semua campaign
    {:ok, campaigns}
    # => Mengembalikan tuple sukses
  end

  def donations(%{id: campaign_id}, _args, _resolution) do
    # => Resolve field donations nested
    # => Parent adalah campaign dengan ID
    donations = DonationDB.get_donations_for_campaign(campaign_id)
    # => Fetch donasi untuk campaign ini
    {:ok, donations}
    # => Mengembalikan list donasi
  end
end
# => Mengembalikan module resolver

defmodule DonationPlatform.Resolvers.Donation do
  # => Fungsi resolver Donation

  def campaign(%{campaign_id: campaign_id}, _args, _resolution) do
    # => Resolve parent campaign dari donasi
    campaign = DonationDB.get_campaign(campaign_id)
    # => Fetch campaign
    {:ok, campaign}
  end
end
# => Mengembalikan module resolver
```

Resolver menghubungkan schema ke sumber data.

### Eksekusi Query GraphQL

```elixir
# Query GraphQL
query = """
{
  campaign(id: "ramadan_2026") {
    name
    goal
    raised
    currency
    donations {
      donor
      amount
      timestamp
    }
  }
}
"""
# => String query GraphQL
# => Di-parse dan divalidasi oleh Absinthe

# Eksekusi query
{:ok, result} = Absinthe.run(
  query,
  # => String query
  DonationPlatform.Schema
  # => Schema yang digunakan
)
# => Mengembalikan {:ok, %{data: ..., errors: ...}}

# Struktur result
result == %{
  data: %{
    "campaign" => %{
      "name" => "Ramadan 2026",
      # => Field string
      "goal" => 100_000_000,
      # => Integer (100 juta IDR)
      "raised" => 45_000_000,
      # => Integer (45 juta IDR)
      "currency" => "IDR",
      # => Kode mata uang
      "donations" => [
        # => List nested
        %{
          "donor" => "Ahmad",
          "amount" => 1_000_000,
          "timestamp" => "2026-02-05T10:00:00Z"
        },
        %{
          "donor" => "Fatimah",
          "amount" => 500_000,
          "timestamp" => "2026-02-05T11:30:00Z"
        }
      ]
    }
  }
}
# => Data nested di-resolve dengan benar
```

Absinthe handle parsing, validasi, dan eksekusi otomatis.

## Mengatasi N+1 Query dengan DataLoader

Tanpa batching, query nested menyebabkan N+1 query database.

### Masalah N+1

```elixir
# Query campaign dengan donasi
query = """
{
  campaigns {
    name
    donations {
      # => Field nested
      donor
      amount
    }
  }
}
"""
# => Fetch beberapa campaign dengan donasi

# Tanpa DataLoader
# => 1 query untuk ambil semua campaign: SELECT * FROM campaigns
# => Untuk 100 campaign:
#    - 100 query: SELECT * FROM donations WHERE campaign_id = ?
# => Total: 101 query database (masalah N+1)
# => Degradasi performa masif
```

### Implementasi DataLoader

```elixir
defmodule DonationPlatform.Schema do
  use Absinthe.Schema
  import_types Absinthe.Type.Custom

  # Tambah plugin DataLoader
  def plugins do
    # => Plugin schema
    [Absinthe.Middleware.Dataloader | Absinthe.Plugin.defaults()]
    # => Menambahkan middleware DataLoader
  end

  def dataloader do
    # => Konfigurasi DataLoader
    Dataloader.new()
    # => Buat instance DataLoader
    |> Dataloader.add_source(
      :db,
      # => Nama source
      Dataloader.Ecto.new(DonationPlatform.Repo)
      # => Source data Ecto (batch query)
    )
  end

  def context(ctx) do
    # => Tambah DataLoader ke konteks resolution
    Map.put(ctx, :loader, dataloader())
    # => Membuat DataLoader tersedia di resolver
  end

  # Update tipe Campaign untuk gunakan DataLoader
  object :campaign do
    field :id, non_null(:id)
    field :name, non_null(:string)
    field :goal, non_null(:integer)
    field :raised, non_null(:integer)

    field :donations, list_of(:donation) do
      # => Resolution di-batch
      resolve dataloader(:db)
      # => DataLoader batch query otomatis
      # => Donasi beberapa campaign di-fetch dalam query tunggal
    end
  end
end
# => Mengembalikan module schema

# Dengan DataLoader
# => 1 query: SELECT * FROM campaigns
# => 1 query: SELECT * FROM donations WHERE campaign_id IN (?, ?, ..., ?)
# => Total: 2 query untuk jumlah campaign berapapun
# => O(1) query daripada O(N)
```

DataLoader mem-batch query untuk mencegah masalah N+1.

## Subscription Real-Time

Subscription memungkinkan update real-time di-push ke klien.

### Definisi Subscription

```elixir
defmodule DonationPlatform.Schema do
  use Absinthe.Schema

  # ... tipe dan query yang ada ...

  # Define tipe subscription
  subscription do
    # => Tipe subscription root
    field :donation_received, :donation do
      # => Field subscription
      arg :campaign_id, non_null(:id)
      # => Subscribe ke campaign spesifik

      config fn args, _info ->
        # => Konfigurasi subscription
        {:ok, topic: args.campaign_id}
        # => Subscribe ke topik spesifik campaign
      end

      trigger :create_donation, topic: fn donation ->
        # => Triggered ketika mutation create_donation dijalankan
        donation.campaign_id
        # => Mengembalikan topik (campaign_id)
        # => Publish ke subscriber campaign ini
      end
    end
  end
  # => Mengembalikan definisi subscription

  # Define mutation yang trigger subscription
  mutation do
    field :create_donation, :donation do
      # => Field mutation
      arg :campaign_id, non_null(:id)
      arg :amount, non_null(:integer)
      arg :donor, non_null(:string)

      resolve &Resolvers.Donation.create/3
      # => Resolver buat donasi
    end
  end
end
# => Mengembalikan module schema

defmodule DonationPlatform.Resolvers.Donation do
  def create(_parent, args, _resolution) do
    # => Mutation buat donasi
    donation = %{
      id: UUID.uuid4(),
      campaign_id: args.campaign_id,
      amount: args.amount,
      donor: args.donor,
      timestamp: DateTime.utc_now() |> DateTime.to_iso8601()
    }
    # => Buat struct donasi

    DonationDB.insert_donation(donation)
    # => Persist ke database

    # Subscription otomatis di-trigger oleh :create_donation
    {:ok, donation}
    # => Mengembalikan donasi yang dibuat
    # => Subscriber terima update
  end
end
# => Mengembalikan module resolver
```

### Contoh Subscription Klien

```elixir
# Klien subscribe ke donasi campaign
subscription = """
subscription($campaignId: ID!) {
  donationReceived(campaignId: $campaignId) {
    donor
    amount
    timestamp
  }
}
"""
# => Query subscription
# => $campaignId adalah variabel

# Klien terima update real-time
# Ketika donasi dibuat:
# => Subscription push update ke klien
# => Tidak perlu polling
# => Update dikirim via WebSocket

# Contoh update yang diterima:
# %{
#   "donationReceived" => %{
#     "donor" => "Ahmad",
#     "amount" => 1_000_000,
#     "timestamp" => "2026-02-05T12:00:00Z"
#   }
# }
# => Notifikasi real-time
```

Subscription push update tanpa polling.

## Pola Production - API Platform Donasi

```elixir
defmodule DonationPlatform.Schema do
  use Absinthe.Schema

  import_types DonationPlatform.Schema.CampaignTypes
  import_types DonationPlatform.Schema.DonationTypes
  # => Module tipe terpisah untuk organisasi

  # DataLoader untuk pencegahan N+1
  def plugins do
    [Absinthe.Middleware.Dataloader | Absinthe.Plugin.defaults()]
  end

  def dataloader do
    Dataloader.new()
    |> Dataloader.add_source(:db, Dataloader.Ecto.new(Repo))
  end

  def context(ctx) do
    # Tambah authentication
    ctx
    |> Map.put(:loader, dataloader())
    |> Map.put(:current_user, get_current_user(ctx))
    # => Tambah user terauthentikasi ke konteks
  end

  # Query
  query do
    field :campaign, :campaign do
      arg :id, non_null(:id)
      resolve &Resolvers.Campaign.get/3
    end

    field :campaigns, list_of(:campaign) do
      arg :filter, :campaign_filter
      # => Argumen filter opsional
      resolve &Resolvers.Campaign.list/3
    end

    field :my_donations, list_of(:donation) do
      # => Memerlukan authentication
      resolve &Resolvers.Donation.list_for_user/3
      middleware Middleware.Authenticate
      # => Cek authentication
    end
  end

  # Mutation
  mutation do
    field :create_campaign, :campaign do
      arg :name, non_null(:string)
      arg :goal, non_null(:integer)
      arg :currency, non_null(:string)

      resolve &Resolvers.Campaign.create/3
      middleware Middleware.Authenticate
      # => Memerlukan authentication
    end

    field :create_donation, :donation do
      arg :campaign_id, non_null(:id)
      arg :amount, non_null(:integer)
      arg :donor, non_null(:string)

      resolve &Resolvers.Donation.create/3
      # => Trigger subscription
    end

    field :close_campaign, :campaign do
      arg :id, non_null(:id)

      resolve &Resolvers.Campaign.close/3
      middleware Middleware.Authenticate
      middleware Middleware.RequireOwnership
      # => Hanya pemilik campaign yang bisa close
    end
  end

  # Subscription
  subscription do
    field :donation_received, :donation do
      arg :campaign_id, non_null(:id)

      config fn args, _info ->
        {:ok, topic: "campaign:#{args.campaign_id}"}
      end

      trigger :create_donation, topic: fn donation ->
        "campaign:#{donation.campaign_id}"
      end
    end

    field :campaign_updated, :campaign do
      arg :campaign_id, non_null(:id)

      config fn args, _info ->
        {:ok, topic: "campaign:#{args.campaign_id}"}
      end

      trigger [:update_campaign, :close_campaign], topic: fn campaign ->
        "campaign:#{campaign.id}"
      end
      # => Multiple trigger
    end
  end
end
# => Mengembalikan schema production

# Middleware authentication
defmodule DonationPlatform.Middleware.Authenticate do
  @behaviour Absinthe.Middleware

  def call(resolution, _config) do
    # => Cek authentication
    case resolution.context[:current_user] do
      nil ->
        # => Tidak terauthentikasi
        resolution
        |> Absinthe.Resolution.put_result({:error, "Unauthenticated"})

      _user ->
        # => Terauthentikasi, lanjutkan
        resolution
    end
  end
end
# => Mengembalikan module middleware
```

Schema production termasuk authentication, DataLoader, dan subscription.

## Kapan Gunakan GraphQL vs REST

### Gunakan GraphQL Ketika

- **Data nested kompleks** - Produk dengan varian, review, kategori
- **Aplikasi mobile** - Minimalisir bandwidth, request hanya field yang dibutuhkan
- **Kebutuhan real-time** - Subscription untuk update live
- **Iterasi cepat** - Tambah field tanpa merusak klien
- **Banyak klien** - Setiap klien request data berbeda

### Gunakan REST Ketika

- **CRUD sederhana** - Create, read, update, delete dasar
- **Upload file** - Data multipart form lebih mudah dengan REST
- **Kebutuhan caching** - HTTP caching sudah dipahami dengan baik
- **Familiaritas tim** - Tim berpengalaman dengan pola REST
- **Kebutuhan sederhana** - Endpoint tetap sudah cukup

## Kesimpulan Utama

**GraphQL manual tidak praktis**:

- Tidak ada dukungan standard library
- Parsing kompleks diperlukan
- Tidak ada validasi tipe
- Tidak ada dukungan tooling

**Absinthe menyediakan framework production**:

- Definisi schema dengan tipe
- Parsing dan validasi query otomatis
- Infrastruktur resolver
- Dukungan introspection
- Integrasi Phoenix

**DataLoader mencegah N+1 query**:

- Batch query database
- O(1) query daripada O(N)
- Transparan ke resolver
- Kritis untuk performa production

**Subscription memungkinkan real-time**:

- Push update ke klien
- Tidak perlu polling
- Transport WebSocket
- Routing berbasis topik

**Pola production**: Schema dengan tipe → Resolver dengan DataLoader → Mutation yang trigger subscription → Middleware authentication = API GraphQL type-safe, efisien, dan real-time.

**Contoh platform donasi**: Campaign dengan donasi nested, notifikasi donasi real-time, mutation terauthentikasi, pencegahan N+1 yang dioptimasi.
