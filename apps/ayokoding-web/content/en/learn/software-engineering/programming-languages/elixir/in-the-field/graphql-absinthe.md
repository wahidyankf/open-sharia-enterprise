---
title: "GraphQL Absinthe"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000020
description: "From manual GraphQL parsing to Absinthe framework for production-ready GraphQL APIs with schemas, resolvers, and real-time subscriptions"
tags: ["elixir", "graphql", "absinthe", "api", "real-time", "subscriptions"]
prev: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/rest-api-design"
next: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/authentication-authorization"
---

**How do you build production GraphQL APIs in Elixir?** This guide teaches the progression from manual GraphQL parsing through Absinthe framework, showing how schema definition, resolvers, and subscriptions enable type-safe, real-time APIs for modern applications.

## Why It Matters

GraphQL provides flexible, efficient APIs where clients specify exactly what data they need. Real-world requirements:

- **Type safety** - Schema-defined types prevent runtime errors
- **Efficient queries** - Clients request only needed fields
- **Real-time updates** - Subscriptions push changes to clients
- **Nested queries** - Resolve complex relationships in single request
- **API evolution** - Add fields without breaking existing clients

Real-world scenarios requiring GraphQL with Absinthe:

- **Donation platforms** - Real-time campaign updates, nested donor/transaction data
- **E-commerce** - Product catalogs with variants, inventory, reviews
- **Social platforms** - User feeds with posts, comments, reactions
- **Financial dashboards** - Account balances, transactions, real-time prices
- **Mobile apps** - Efficient data loading, offline sync

Production question: When should you use GraphQL instead of REST? How does Absinthe provide production-ready GraphQL infrastructure? The answer depends on your client flexibility and real-time requirements.

## Manual GraphQL - Standard Library Limitations

Elixir standard library has no GraphQL support. Manual implementation required.

### Parsing GraphQL Query Manually

```elixir
# Manual GraphQL query parsing
query_string = """
{
  campaign(id: "ramadan_2026") {
    name
    goal
    raised
  }
}
"""
# => GraphQL query string
# => Problem: No built-in parser

# Manual string parsing (fragile)
defmodule ManualGraphQL do
  # => Naive manual implementation
  def parse_query(query_string) do
    # => Extract operation and fields
    # Problem: Regex-based parsing
    # => Brittle, error-prone

    cond do
      String.contains?(query_string, "campaign(id:") ->
        # => Extract campaign ID
        id = extract_id(query_string)
        # => Returns campaign ID string
        fields = extract_fields(query_string)
        # => Returns list of requested fields

        {:campaign, id, fields}
        # => Returns parsed query tuple

      true ->
        {:error, :unknown_query}
        # => Unsupported query type
    end
  end

  defp extract_id(query) do
    # => Regex-based ID extraction
    # Problem: Fragile string parsing
    ~r/id:\s*"([^"]+)"/
    |> Regex.run(query)
    |> Enum.at(1)
    # => Returns captured ID or nil
  end

  defp extract_fields(query) do
    # => Extract requested fields
    # Problem: Cannot handle nested fields
    query
    |> String.split(["{", "}"])
    |> Enum.at(2)
    # => Gets content between second {}
    |> String.split("\n")
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    # => Returns list of field names
  end
end
# => Returns module

{:campaign, id, fields} = ManualGraphQL.parse_query(query_string)
# => Returns {:campaign, "ramadan_2026", ["name", "goal", "raised"]}
# => Fragile, breaks easily
```

### Limitations of Manual Approach

```elixir
# Problem 1: No type validation
query_invalid = """
{
  campaign(id: 123) {
    # => ID should be string, not integer
    invalid_field
    # => Field doesn't exist
  }
}
"""
# => No compile-time or parse-time validation
# => Errors discovered at runtime

# Problem 2: Cannot handle nested queries
query_nested = """
{
  campaign(id: "ramadan_2026") {
    name
    donations {
      # => Nested field
      donor
      amount
    }
  }
}
"""
# => Manual parser cannot handle nesting
# => Would require complex recursive parsing

# Problem 3: No schema definition
# => No single source of truth for API structure
# => Type information scattered across resolver code
# => Cannot generate documentation automatically

# Problem 4: No introspection
# => Clients cannot discover available queries/fields
# => GraphQL introspection queries not supported
# => Breaks GraphQL tooling (GraphiQL, Playground)

# Problem 5: No N+1 query protection
# => Each nested field triggers separate database query
# => 100 donations = 100+ database queries
# => Performance degrades rapidly
```

Production problems with manual GraphQL:

- **No query parsing** - Must implement from scratch (complex)
- **No type validation** - Runtime errors for invalid queries
- **No schema definition** - Cannot enforce structure
- **No nested queries** - Complex to implement correctly
- **No introspection** - Breaks GraphQL tooling
- **No batching** - N+1 query problems

## Absinthe - Production GraphQL Framework

Absinthe provides complete GraphQL implementation for Elixir.

### Installing Absinthe

```elixir
# Add to mix.exs dependencies
defp deps do
  [
    {:absinthe, "~> 1.7"},
    # => Core Absinthe GraphQL
    {:absinthe_plug, "~> 1.5"},
    # => Phoenix/Plug integration
    {:absinthe_phoenix, "~> 2.0"}
    # => Phoenix channels for subscriptions
  ]
end
# => Returns dependency list

# Install dependencies
# $ mix deps.get
# => Fetches Absinthe packages
```

### Defining GraphQL Schema

```elixir
defmodule DonationPlatform.Schema do
  # => GraphQL schema definition
  use Absinthe.Schema
  # => Import Absinthe DSL

  # Define Campaign type
  object :campaign do
    # => GraphQL object type
    field :id, non_null(:id)
    # => Required ID field
    # => non_null enforces presence

    field :name, non_null(:string)
    # => Required string field

    field :goal, non_null(:integer)
    # => Goal amount in cents

    field :raised, non_null(:integer)
    # => Current raised amount

    field :currency, non_null(:string)
    # => Currency code (USD, EUR, etc.)

    field :donations, list_of(:donation) do
      # => Nested donations list
      # => Returns list of Donation objects
      resolve &Resolvers.Campaign.donations/3
      # => Resolver function for donations field
    end
  end
  # => Returns type definition

  # Define Donation type
  object :donation do
    # => Donation object type
    field :id, non_null(:id)
    field :amount, non_null(:integer)
    # => Donation amount in cents

    field :donor, non_null(:string)
    # => Donor name

    field :timestamp, non_null(:string)
    # => ISO 8601 timestamp

    field :campaign, :campaign do
      # => Nested campaign reference
      resolve &Resolvers.Donation.campaign/3
      # => Resolver for parent campaign
    end
  end
  # => Returns type definition

  # Define root query
  query do
    # => Root query type
    field :campaign, :campaign do
      # => campaign query field
      arg :id, non_null(:id)
      # => Required ID argument

      resolve &Resolvers.Campaign.get/3
      # => Resolver function
    end

    field :campaigns, list_of(:campaign) do
      # => List all campaigns
      resolve &Resolvers.Campaign.list/3
    end
  end
  # => Returns query definition
end
# => Returns schema module
```

Schema defines types, fields, and resolvers with compile-time validation.

### Implementing Resolvers

```elixir
defmodule DonationPlatform.Resolvers.Campaign do
  # => Campaign resolver functions

  def get(_parent, %{id: id}, _resolution) do
    # => Resolve single campaign
    # => Args: parent (nil for root), arguments map, resolution context

    case DonationDB.get_campaign(id) do
      nil ->
        {:error, "Campaign not found"}
        # => Error tuple for not found

      campaign ->
        {:ok, campaign}
        # => Success tuple with campaign data
    end
  end
  # => Returns resolver function

  def list(_parent, _args, _resolution) do
    # => Resolve campaigns list
    campaigns = DonationDB.list_campaigns()
    # => Fetch all campaigns
    {:ok, campaigns}
    # => Returns success tuple
  end

  def donations(%{id: campaign_id}, _args, _resolution) do
    # => Resolve nested donations field
    # => Parent is campaign with ID
    donations = DonationDB.get_donations_for_campaign(campaign_id)
    # => Fetch donations for this campaign
    {:ok, donations}
    # => Returns list of donations
  end
end
# => Returns resolver module

defmodule DonationPlatform.Resolvers.Donation do
  # => Donation resolver functions

  def campaign(%{campaign_id: campaign_id}, _args, _resolution) do
    # => Resolve parent campaign from donation
    campaign = DonationDB.get_campaign(campaign_id)
    # => Fetch campaign
    {:ok, campaign}
  end
end
# => Returns resolver module
```

Resolvers connect schema to data sources.

### Executing GraphQL Queries

```elixir
# GraphQL query
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
# => GraphQL query string
# => Parsed and validated by Absinthe

# Execute query
{:ok, result} = Absinthe.run(
  query,
  # => Query string
  DonationPlatform.Schema
  # => Schema to use
)
# => Returns {:ok, %{data: ..., errors: ...}}

# Result structure
result == %{
  data: %{
    "campaign" => %{
      "name" => "Ramadan 2026",
      # => String field
      "goal" => 100_000_000,
      # => Integer (100 million IDR)
      "raised" => 45_000_000,
      # => Integer (45 million IDR)
      "currency" => "IDR",
      # => Currency code
      "donations" => [
        # => Nested list
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
# => Nested data resolved correctly
```

Absinthe handles parsing, validation, and execution automatically.

## Solving N+1 Queries with DataLoader

Without batching, nested queries cause N+1 database queries.

### The N+1 Problem

```elixir
# Query campaigns with donations
query = """
{
  campaigns {
    name
    donations {
      # => Nested field
      donor
      amount
    }
  }
}
"""
# => Fetches multiple campaigns with donations

# Without DataLoader
# => 1 query to get all campaigns: SELECT * FROM campaigns
# => For 100 campaigns:
#    - 100 queries: SELECT * FROM donations WHERE campaign_id = ?
# => Total: 101 database queries (N+1 problem)
# => Massive performance degradation
```

### Implementing DataLoader

```elixir
defmodule DonationPlatform.Schema do
  use Absinthe.Schema
  import_types Absinthe.Type.Custom

  # Add DataLoader plugin
  def plugins do
    # => Schema plugins
    [Absinthe.Middleware.Dataloader | Absinthe.Plugin.defaults()]
    # => Adds DataLoader middleware
  end

  def dataloader do
    # => DataLoader configuration
    Dataloader.new()
    # => Creates DataLoader instance
    |> Dataloader.add_source(
      :db,
      # => Source name
      Dataloader.Ecto.new(DonationPlatform.Repo)
      # => Ecto data source (batches queries)
    )
  end

  def context(ctx) do
    # => Add DataLoader to resolution context
    Map.put(ctx, :loader, dataloader())
    # => Makes DataLoader available in resolvers
  end

  # Update Campaign type to use DataLoader
  object :campaign do
    field :id, non_null(:id)
    field :name, non_null(:string)
    field :goal, non_null(:integer)
    field :raised, non_null(:integer)

    field :donations, list_of(:donation) do
      # => Batched resolution
      resolve dataloader(:db)
      # => DataLoader batches queries automatically
      # => Multiple campaign donations fetched in single query
    end
  end
end
# => Returns schema module

# With DataLoader
# => 1 query: SELECT * FROM campaigns
# => 1 query: SELECT * FROM donations WHERE campaign_id IN (?, ?, ..., ?)
# => Total: 2 queries for any number of campaigns
# => O(1) queries instead of O(N)
```

DataLoader batches queries to prevent N+1 problems.

## Real-Time Subscriptions

Subscriptions enable real-time updates pushed to clients.

### Defining Subscription

```elixir
defmodule DonationPlatform.Schema do
  use Absinthe.Schema

  # ... existing types and queries ...

  # Define subscription type
  subscription do
    # => Root subscription type
    field :donation_received, :donation do
      # => Subscription field
      arg :campaign_id, non_null(:id)
      # => Subscribe to specific campaign

      config fn args, _info ->
        # => Subscription configuration
        {:ok, topic: args.campaign_id}
        # => Subscribe to campaign-specific topic
      end

      trigger :create_donation, topic: fn donation ->
        # => Triggered when create_donation mutation runs
        donation.campaign_id
        # => Returns topic (campaign_id)
        # => Publishes to subscribers of this campaign
      end
    end
  end
  # => Returns subscription definition

  # Define mutation that triggers subscription
  mutation do
    field :create_donation, :donation do
      # => Mutation field
      arg :campaign_id, non_null(:id)
      arg :amount, non_null(:integer)
      arg :donor, non_null(:string)

      resolve &Resolvers.Donation.create/3
      # => Resolver creates donation
    end
  end
end
# => Returns schema module

defmodule DonationPlatform.Resolvers.Donation do
  def create(_parent, args, _resolution) do
    # => Create donation mutation
    donation = %{
      id: UUID.uuid4(),
      campaign_id: args.campaign_id,
      amount: args.amount,
      donor: args.donor,
      timestamp: DateTime.utc_now() |> DateTime.to_iso8601()
    }
    # => Create donation struct

    DonationDB.insert_donation(donation)
    # => Persist to database

    # Subscription automatically triggered by :create_donation
    {:ok, donation}
    # => Returns created donation
    # => Subscribers receive update
  end
end
# => Returns resolver module
```

### Subscription Client Example

```elixir
# Client subscribes to campaign donations
subscription = """
subscription($campaignId: ID!) {
  donationReceived(campaignId: $campaignId) {
    donor
    amount
    timestamp
  }
}
"""
# => Subscription query
# => $campaignId is variable

# Client receives real-time updates
# When donation created:
# => Subscription pushes update to client
# => No polling required
# => Updates delivered via WebSocket

# Example update received:
# %{
#   "donationReceived" => %{
#     "donor" => "Ahmad",
#     "amount" => 1_000_000,
#     "timestamp" => "2026-02-05T12:00:00Z"
#   }
# }
# => Real-time notification
```

Subscriptions push updates without polling.

## Production Pattern - Donation Platform API

```elixir
defmodule DonationPlatform.Schema do
  use Absinthe.Schema

  import_types DonationPlatform.Schema.CampaignTypes
  import_types DonationPlatform.Schema.DonationTypes
  # => Separate type modules for organization

  # DataLoader for N+1 prevention
  def plugins do
    [Absinthe.Middleware.Dataloader | Absinthe.Plugin.defaults()]
  end

  def dataloader do
    Dataloader.new()
    |> Dataloader.add_source(:db, Dataloader.Ecto.new(Repo))
  end

  def context(ctx) do
    # Add authentication
    ctx
    |> Map.put(:loader, dataloader())
    |> Map.put(:current_user, get_current_user(ctx))
    # => Add authenticated user to context
  end

  # Queries
  query do
    field :campaign, :campaign do
      arg :id, non_null(:id)
      resolve &Resolvers.Campaign.get/3
    end

    field :campaigns, list_of(:campaign) do
      arg :filter, :campaign_filter
      # => Optional filter argument
      resolve &Resolvers.Campaign.list/3
    end

    field :my_donations, list_of(:donation) do
      # => Requires authentication
      resolve &Resolvers.Donation.list_for_user/3
      middleware Middleware.Authenticate
      # => Check authentication
    end
  end

  # Mutations
  mutation do
    field :create_campaign, :campaign do
      arg :name, non_null(:string)
      arg :goal, non_null(:integer)
      arg :currency, non_null(:string)

      resolve &Resolvers.Campaign.create/3
      middleware Middleware.Authenticate
      # => Requires authentication
    end

    field :create_donation, :donation do
      arg :campaign_id, non_null(:id)
      arg :amount, non_null(:integer)
      arg :donor, non_null(:string)

      resolve &Resolvers.Donation.create/3
      # => Triggers subscription
    end

    field :close_campaign, :campaign do
      arg :id, non_null(:id)

      resolve &Resolvers.Campaign.close/3
      middleware Middleware.Authenticate
      middleware Middleware.RequireOwnership
      # => Only campaign owner can close
    end
  end

  # Subscriptions
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
      # => Multiple triggers
    end
  end
end
# => Returns production schema

# Authentication middleware
defmodule DonationPlatform.Middleware.Authenticate do
  @behaviour Absinthe.Middleware

  def call(resolution, _config) do
    # => Check authentication
    case resolution.context[:current_user] do
      nil ->
        # => Not authenticated
        resolution
        |> Absinthe.Resolution.put_result({:error, "Unauthenticated"})

      _user ->
        # => Authenticated, continue
        resolution
    end
  end
end
# => Returns middleware module
```

Production schema includes authentication, DataLoader, and subscriptions.

## When to Use GraphQL vs REST

### Use GraphQL When

- **Complex nested data** - Products with variants, reviews, categories
- **Mobile apps** - Minimize bandwidth, request only needed fields
- **Real-time requirements** - Subscriptions for live updates
- **Rapid iteration** - Add fields without breaking clients
- **Multiple clients** - Each client requests different data

### Use REST When

- **Simple CRUD** - Basic create, read, update, delete
- **File uploads** - Multipart form data easier with REST
- **Caching requirements** - HTTP caching well-understood
- **Team familiarity** - Team experienced with REST patterns
- **Simple requirements** - Fixed endpoints sufficient

## Key Takeaways

**Manual GraphQL is impractical**:

- No standard library support
- Complex parsing required
- No type validation
- No tooling support

**Absinthe provides production framework**:

- Schema definition with types
- Automatic query parsing and validation
- Resolver infrastructure
- Introspection support
- Phoenix integration

**DataLoader prevents N+1 queries**:

- Batches database queries
- O(1) queries instead of O(N)
- Transparent to resolvers
- Critical for production performance

**Subscriptions enable real-time**:

- Push updates to clients
- No polling required
- WebSocket transport
- Topic-based routing

**Production pattern**: Schema with types → Resolvers with DataLoader → Mutations triggering subscriptions → Authentication middleware = Type-safe, efficient, real-time GraphQL API.

**Donation platform example**: Campaigns with nested donations, real-time donation notifications, authenticated mutations, optimized N+1 prevention.
