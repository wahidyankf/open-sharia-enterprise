---
title: Elixir DDD Templates
description: Production-ready Domain-Driven Design templates adapted for Elixir's functional programming model
category: explanation
subcategory: prog-lang
tags:
  - elixir
  - templates
  - ddd
  - functional-programming
  - domain-modeling
created: 2026-01-25
updated: 2026-01-25
---

# Domain-Driven Design Templates for Elixir

**Category**: DDD Templates
**Elixir Version**: 1.19.0+
**Related**: [Domain-Driven Design](../ex-so-prla-el__domain-driven-design.md)

## Overview

This directory contains production-ready Domain-Driven Design (DDD) templates adapted for Elixir's functional programming model. Each template provides complete, runnable code with tests and financial domain examples.

**Key Features**:

- 📝 **Production-Ready**: Complete, tested, deployable code
- 🎯 **Elixir-Specific**: Leverages BEAM VM, OTP, and functional patterns
- 💰 **Financial Domain**: All examples use Zakat, donations, and campaigns
- 🧪 **Fully Tested**: ExUnit tests included with each template
- 📚 **Well-Documented**: When to use, common mistakes, best practices

## When to Use These Templates

### Use DDD Templates When

- **Complex Domain Logic**: Business rules that change frequently
- **Large Systems**: Multiple bounded contexts and aggregates
- **Team Collaboration**: Need ubiquitous language and clear boundaries
- **Long-Term Maintenance**: System expected to evolve over years

### Don't Use DDD Templates When

- **Simple CRUD**: Basic data management with minimal logic
- **Small Projects**: Overhead outweighs benefits
- **Prototypes**: DDD adds complexity too early
- **Data-Driven**: Focus on reporting, not complex behavior

## Template Structure

### Each Template Includes

1. **Complete Code**: Fully functional Elixir implementation
2. **Tests**: ExUnit test suite with multiple scenarios
3. **Financial Example**: Real-world Zakat/donation example
4. **When to Use**: Guidance on appropriate usage
5. **Common Mistakes**: Pitfalls to avoid
6. **Best Practices**: Production-ready patterns

## Available Templates

### 1. Entity Template

**File**: [entity-template.md](entity-template.md)

**Purpose**: Domain objects with persistent identity

**Example**: Donation entity with UUID, mutable state

**Key Concepts**:

- Ecto schema for persistence
- Identity-based equality
- Lifecycle management
- Changeset validation

```elixir
defmodule DonationEntity do
  use Ecto.Schema

  schema "donations" do
    field :amount, :decimal
    field :currency, :string
    field :status, :string

    timestamps()
  end
end
```

### 2. Value Object Template

**File**: [value-object-template.md](value-object-template.md)

**Purpose**: Immutable domain values without identity

**Example**: Money value object with amount + currency

**Key Concepts**:

- Struct-based implementation
- Value-based equality
- Immutability
- Protocol implementations

```elixir
defmodule Money do
  @enforce_keys [:amount, :currency]
  defstruct [:amount, :currency]

  def new(amount, currency) when amount >= 0 do
    %Money{amount: Decimal.new(amount), currency: currency}
  end
end
```

### 3. Aggregate Template

**File**: [aggregate-template.md](aggregate-template.md)

**Purpose**: Consistency boundaries with aggregate roots

**Example**: Campaign aggregate with donations

**Key Concepts**:

- Bounded context enforcement
- Transaction boundaries
- Invariant protection
- Domain event emission

```elixir
defmodule Campaign do
  use Ecto.Schema

  schema "campaigns" do
    field :goal, :decimal
    field :current, :decimal
    has_many :donations, Donation

    timestamps()
  end

  def add_donation(%Campaign{} = campaign, donation) do
    # Enforce invariants
    # Emit domain events
  end
end
```

### 4. Domain Event Template

**File**: [domain-event-template.md](domain-event-template.md)

**Purpose**: Capture state changes as events

**Example**: DonationReceived event with payload

**Key Concepts**:

- Event struct definition
- Event handler GenServer
- Event sourcing patterns
- Event store integration

```elixir
defmodule DonationReceived do
  @enforce_keys [:donation_id, :amount, :campaign_id, :occurred_at]
  defstruct [:donation_id, :amount, :campaign_id, :occurred_at]

  def new(attrs) do
    struct!(__MODULE__, Map.put(attrs, :occurred_at, DateTime.utc_now()))
  end
end
```

### 5. Repository Template

**File**: [repository-template.md](repository-template.md)

**Purpose**: Abstract persistence from domain logic

**Example**: DonationRepository with business queries

**Key Concepts**:

- Ecto.Repo abstraction
- Business-focused queries
- Transaction management
- Query composition

```elixir
defmodule DonationRepository do
  import Ecto.Query
  alias Financial.Repo

  def find(id), do: Repo.get(Donation, id)

  def find_by_campaign(campaign_id) do
    from(d in Donation, where: d.campaign_id == ^campaign_id)
    |> Repo.all()
  end
end
```

### 6. Service Layer Template

**File**: [service-layer-template.md](service-layer-template.md)

**Purpose**: Orchestrate domain logic and manage transactions

**Example**: DonationContext coordinating donation workflow

**Key Concepts**:

- Phoenix context pattern
- Transaction boundaries
- Multi-step operations
- Error handling

```elixir
defmodule DonationContext do
  alias Financial.Repo

  def create_donation(attrs) do
    Repo.transaction(fn ->
      with {:ok, donation} <- validate_and_insert(attrs),
           {:ok, campaign} <- update_campaign(donation),
           :ok <- emit_events(donation) do
        {:ok, donation}
      end
    end)
  end
end
```

### 7. Build Configuration Template

**File**: [build-configuration-template.md](build-configuration-template.md)

**Purpose**: Complete project setup for DDD application

**Example**: Financial platform with Mix, Docker, CI/CD

**Key Concepts**:

- Mix.exs configuration
- Environment management
- Release configuration
- Docker containerization
- CI/CD pipeline

```elixir
# mix.exs
defmodule Financial.MixProject do
  use Mix.Project

  def project do
    [
      app: :financial,
      version: "1.0.0",
      elixir: "~> 1.17",
      deps: deps(),
      releases: releases()
    ]
  end
end
```

## Quick Reference

| Template          | Use For                | Persistence      | Example                 |
| ----------------- | ---------------------- | ---------------- | ----------------------- |
| **Entity**        | Objects with identity  | Ecto schema      | Donation, Campaign      |
| **Value Object**  | Immutable values       | Struct           | Money, Email            |
| **Aggregate**     | Consistency boundaries | Ecto + has_many  | Campaign with donations |
| **Domain Event**  | State changes          | Struct           | DonationReceived        |
| **Repository**    | Data access            | Ecto.Repo        | DonationRepository      |
| **Service Layer** | Orchestration          | Context module   | DonationContext         |
| **Build Config**  | Project setup          | Mix.exs + Docker | Complete application    |

## Financial Domain Examples Summary

### Zakat Calculation System

```elixir
# Entity: ZakatPayment with identity
ZakatPayment
├── id: UUID
├── taxpayer_id: UUID
├── wealth: Money (value object)
├── nisab: Money (value object)
├── amount: Money (value object)
└── status: :pending | :paid

# Aggregate: TaxpayerAccount
TaxpayerAccount (root)
├── taxpayer_id (identity)
├── ZakatPayments[] (entities)
└── total_zakat (calculated)

# Events
- ZakatCalculated
- ZakatPaymentRecorded
- ZakatPaid

# Repository
ZakatRepository
├── find_by_taxpayer(taxpayer_id)
├── find_unpaid()
└── save(zakat_payment)

# Service
ZakatContext
├── calculate_zakat(wealth, nisab)
├── record_payment(zakat_payment)
└── mark_as_paid(payment_id)
```

### Donation Management System

```elixir
# Entity: Donation
Donation
├── id: UUID
├── amount: Money (value object)
├── donor_id: UUID
├── campaign_id: UUID
└── timestamp: DateTime

# Aggregate: Campaign
Campaign (root)
├── id (identity)
├── goal: Money
├── current: Money
├── donations: [Donation]
└── status: :draft | :active | :completed

# Events
- DonationReceived
- CampaignMilestoneReached
- CampaignCompleted

# Repository
DonationRepository
├── find(donation_id)
├── find_by_campaign(campaign_id)
└── find_by_donor(donor_id)

CampaignRepository
├── find(campaign_id)
├── list_active()
└── save(campaign)

# Service
DonationContext
├── create_donation(attrs)
├── process_donation(donation)
└── complete_campaign(campaign_id)
```

## Getting Started

### 1. Choose the Right Template

```elixir
# Need persistent identity? → Entity Template
defmodule Donation do
  use Ecto.Schema
  schema "donations" do
    field :amount, :decimal
  end
end

# Need immutable value? → Value Object Template
defmodule Money do
  defstruct [:amount, :currency]
end

# Need consistency boundary? → Aggregate Template
defmodule Campaign do
  schema "campaigns" do
    has_many :donations, Donation
  end
end
```

### 2. Copy Template Code

Each template provides complete, runnable code. Copy and adapt to your domain.

### 3. Run Tests

Every template includes ExUnit tests. Verify everything works:

```bash
mix test
```

### 4. Customize for Your Domain

Replace financial domain examples with your own:

- Donation → Your entity
- Money → Your value object
- Campaign → Your aggregate

## Common Patterns

### Pattern 1: Entity with Value Objects

```elixir
defmodule Donation do
  use Ecto.Schema

  schema "donations" do
    field :amount, :decimal      # Store as decimal
    field :currency, :string     # Store as string
  end

  # Convert to value object on read
  def to_domain(donation) do
    %{donation | amount: Money.new(donation.amount, donation.currency)}
  end
end
```

### Pattern 2: Aggregate with Events

```elixir
defmodule Campaign do
  def add_donation(campaign, donation) do
    # Validate invariants
    if Money.compare(campaign.current + donation.amount, campaign.goal) == :gt do
      raise "Exceeds goal"
    end

    # Update state
    updated = %{campaign | current: campaign.current + donation.amount}

    # Emit event
    event = DonationReceived.new(campaign.id, donation.id)
    {updated, [event]}
  end
end
```

### Pattern 3: Repository with Transaction

```elixir
defmodule DonationContext do
  def create_donation(attrs) do
    Repo.transaction(fn ->
      with {:ok, donation} <- DonationRepository.save(attrs),
           {:ok, campaign} <- CampaignRepository.update_total(donation.campaign_id) do
        {:ok, donation}
      end
    end)
  end
end
```

## Best Practices

### 1. Keep Aggregates Small

```elixir
# Good: Campaign + Donations (natural boundary)
Campaign
└── donations: [Donation]

# Bad: Campaign + Donations + Donors + Payments (too large)
Campaign
├── donations: [Donation]
├── donors: [Donor]
└── payments: [Payment]  # Separate aggregate
```

### 2. Use Value Objects Liberally

```elixir
# Good: Explicit types
defmodule Money do
  defstruct [:amount, :currency]
end

defmodule Email do
  defstruct [:address]
end

# Bad: Primitive obsession
%{amount: 100, currency: "IDR", email: "user@example.com"}
```

### 3. Emit Domain Events

```elixir
# Good: Events for state changes
{:ok, donation, [DonationReceived.new(donation)]}

# Less ideal: Side effects in transaction
Repo.transaction(fn ->
  donation = save_donation()
  send_email()  # Side effect
end)
```

### 4. Separate Read and Write Models

```elixir
# Write model: Full aggregate
defmodule Campaign do
  schema "campaigns" do
    has_many :donations, Donation
  end
end

# Read model: Optimized for queries
defmodule CampaignSummary do
  schema "campaign_summaries" do
    field :total_donations, :integer
    field :total_amount, :decimal
  end
end
```

## Testing Strategy

### Test Each Layer

```elixir
# 1. Value Object Tests
test "Money addition" do
  m1 = Money.new(100, :IDR)
  m2 = Money.new(200, :IDR)
  assert Money.add(m1, m2) == Money.new(300, :IDR)
end

# 2. Entity Tests
test "Donation changeset validation" do
  changeset = Donation.changeset(%Donation{}, %{amount: -100})
  refute changeset.valid?
end

# 3. Aggregate Tests
test "Campaign enforces goal limit" do
  campaign = %Campaign{goal: Money.new(1000, :IDR), current: Money.new(900, :IDR)}
  donation = %Donation{amount: Money.new(200, :IDR)}

  assert_raise RuntimeError, fn ->
    Campaign.add_donation(campaign, donation)
  end
end

# 4. Repository Tests
test "DonationRepository finds by campaign" do
  donations = DonationRepository.find_by_campaign("campaign_123")
  assert length(donations) == 5
end

# 5. Service Tests
test "DonationContext creates donation" do
  attrs = %{amount: 100, currency: :IDR, campaign_id: "camp_123"}
  assert {:ok, donation} = DonationContext.create_donation(attrs)
end
```

## Migration Path

### From Simple to DDD

```elixir
# Phase 1: Start with Phoenix contexts
defmodule Financial.Donations do
  def create_donation(attrs) do
    %Donation{}
    |> Donation.changeset(attrs)
    |> Repo.insert()
  end
end

# Phase 2: Introduce value objects
defmodule Financial.Donations do
  def create_donation(attrs) do
    money = Money.new(attrs.amount, attrs.currency)
    # ...
  end
end

# Phase 3: Add domain events
defmodule Financial.Donations do
  def create_donation(attrs) do
    Repo.transaction(fn ->
      donation = # create donation
      event = DonationReceived.new(donation)
      EventStore.append(event)
    end)
  end
end

# Phase 4: Extract aggregates
defmodule Campaign do
  def add_donation(campaign, donation) do
    # Enforce invariants
    # Emit events
    {updated_campaign, events}
  end
end
```

## Resources

### Template Files

- [Entity Template](entity-template.md) - Persistent identity
- [Value Object Template](value-object-template.md) - Immutable values
- [Aggregate Template](aggregate-template.md) - Consistency boundaries
- [Domain Event Template](domain-event-template.md) - State changes
- [Repository Template](repository-template.md) - Data access
- [Service Layer Template](service-layer-template.md) - Orchestration
- [Build Configuration Template](build-configuration-template.md) - Project setup

### Related Documentation

- [Back to Elixir README](../README.md)
- [Domain-Driven Design](../ex-so-prla-el__domain-driven-design.md)
- [Ecto Schema](https://hexdocs.pm/ecto/Ecto.Schema.html)
- [Phoenix Contexts](https://hexdocs.pm/phoenix/contexts.html)

### Books and Articles

- **Domain-Driven Design** by Eric Evans
- **Implementing Domain-Driven Design** by Vaughn Vernon
- **Functional and Reactive Domain Modeling** by Debasish Ghosh
- [Phoenix Context Guidelines](https://hexdocs.pm/phoenix/contexts.html)

---

**Last Updated**: 2025-01-23
**Elixir Version**: 1.19.0+
**Template Count**: 7 templates + this README
