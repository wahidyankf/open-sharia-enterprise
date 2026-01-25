# Domain Event Template

**Category**: DDD Template
**Complexity**: ⭐⭐ (Medium)
**Prerequisites**: Structs, GenServer, PubSub basics

## Overview

Domain Events represent something that happened in the domain. They are immutable records of state changes, used for event-driven architecture, event sourcing, and decoupling system components.

## When to Use

✅ **Use Domain Events when**:

- Need to notify other parts of system about changes
- Building event-driven architecture
- Implementing event sourcing
- Decoupling bounded contexts
- Auditing and compliance required

❌ **Don't use Domain Events for**:

- Simple CRUD operations without side effects
- Tightly coupled synchronous operations
- Small systems with no decoupling needs

## Template Code

### Basic Domain Event

```elixir
defmodule DonationReceived do
  @moduledoc """
  Event emitted when a donation is received.
  Immutable record of what happened.
  """

  @enforce_keys [:donation_id, :amount, :campaign_id, :donor_id, :occurred_at]
  defstruct [:donation_id, :amount, :campaign_id, :donor_id, :occurred_at, :metadata]

  @type t :: %__MODULE__{
    donation_id: String.t(),
    amount: Money.t(),
    campaign_id: String.t(),
    donor_id: String.t(),
    occurred_at: DateTime.t(),
    metadata: map() | nil
  }

  @doc """
  Creates a new DonationReceived event.
  """
  def new(donation_id, amount, campaign_id, donor_id, opts \\ []) do
    %__MODULE__{
      donation_id: donation_id,
      amount: amount,
      campaign_id: campaign_id,
      donor_id: donor_id,
      occurred_at: DateTime.utc_now(),
      metadata: Keyword.get(opts, :metadata, %{})
    }
  end
end

defmodule CampaignCompleted do
  @enforce_keys [:campaign_id, :final_amount, :goal_amount, :donor_count, :occurred_at]
  defstruct [:campaign_id, :final_amount, :goal_amount, :donor_count, :occurred_at, :metadata]

  @type t :: %__MODULE__{
    campaign_id: String.t(),
    final_amount: Money.t(),
    goal_amount: Money.t(),
    donor_count: integer(),
    occurred_at: DateTime.t(),
    metadata: map() | nil
  }

  def new(campaign_id, final_amount, goal_amount, donor_count, opts \\ []) do
    %__MODULE__{
      campaign_id: campaign_id,
      final_amount: final_amount,
      goal_amount: goal_amount,
      donor_count: donor_count,
      occurred_at: DateTime.utc_now(),
      metadata: Keyword.get(opts, :metadata, %{})
    }
  end
end
```

### Event Handler (GenServer)

```elixir
defmodule DonationEventHandler do
  @moduledoc """
  Handles domain events related to donations.
  """

  use GenServer
  require Logger

  def start_link(_) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init(_) do
    # Subscribe to event bus
    Phoenix.PubSub.subscribe(Financial.PubSub, "domain_events")

    {:ok, %{handled_events: []}}
  end

  # Handle DonationReceived event
  def handle_info(%DonationReceived{} = event, state) do
    Logger.info("Handling DonationReceived: #{event.donation_id}")

    # Side effects
    send_donor_confirmation(event)
    update_campaign_stats(event)
    notify_campaign_owner(event)

    new_state = %{state | handled_events: [event | state.handled_events]}
    {:noreply, new_state}
  end

  # Handle CampaignCompleted event
  def handle_info(%CampaignCompleted{} = event, state) do
    Logger.info("Handling CampaignCompleted: #{event.campaign_id}")

    send_completion_emails(event)
    generate_final_report(event)
    archive_campaign(event)

    new_state = %{state | handled_events: [event | state.handled_events]}
    {:noreply, new_state}
  end

  # Catch-all for unhandled events
  def handle_info(_event, state) do
    {:noreply, state}
  end

  # Private functions

  defp send_donor_confirmation(event) do
    # Send thank you email
    Task.start(fn ->
      Email.send_donation_confirmation(event.donor_id, event.amount)
    end)
  end

  defp update_campaign_stats(event) do
    # Update campaign statistics
    Task.start(fn ->
      CampaignStats.increment(event.campaign_id, event.amount)
    end)
  end

  defp notify_campaign_owner(event) do
    # Notify campaign owner
    Task.start(fn ->
      Notification.send(event.campaign_id, :donation_received, event)
    end)
  end

  defp send_completion_emails(event) do
    Task.start(fn ->
      Email.send_campaign_completion(event.campaign_id)
    end)
  end

  defp generate_final_report(event) do
    Task.start(fn ->
      ReportGenerator.generate_campaign_report(event.campaign_id)
    end)
  end

  defp archive_campaign(event) do
    Task.start(fn ->
      CampaignArchiver.archive(event.campaign_id)
    end)
  end
end
```

### Event Publisher

```elixir
defmodule EventPublisher do
  @moduledoc """
  Publishes domain events to subscribers.
  """

  @doc """
  Publishes an event to all subscribers.
  """
  def publish(event) do
    Phoenix.PubSub.broadcast(
      Financial.PubSub,
      "domain_events",
      event
    )
  end

  @doc """
  Publishes multiple events.
  """
  def publish_all(events) when is_list(events) do
    Enum.each(events, &publish/1)
  end
end
```

### Event Store (Simple Implementation)

```elixir
defmodule EventStore do
  @moduledoc """
  Simple event store for persisting domain events.
  """

  use GenServer

  defmodule StoredEvent do
    use Ecto.Schema

    @primary_key {:id, :binary_id, autogenerate: true}

    schema "domain_events" do
      field :event_type, :string
      field :event_data, :map
      field :occurred_at, :utc_datetime
      field :aggregate_id, :string
      field :aggregate_type, :string

      timestamps(updated_at: false)
    end
  end

  def start_link(_) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init(_) do
    {:ok, %{}}
  end

  @doc """
  Appends an event to the store.
  """
  def append(event) do
    GenServer.call(__MODULE__, {:append, event})
  end

  @doc """
  Retrieves events for an aggregate.
  """
  def get_events(aggregate_type, aggregate_id) do
    GenServer.call(__MODULE__, {:get_events, aggregate_type, aggregate_id})
  end

  def handle_call({:append, event}, _from, state) do
    stored_event = %StoredEvent{
      event_type: event.__struct__ |> to_string(),
      event_data: Map.from_struct(event),
      occurred_at: event.occurred_at,
      aggregate_id: extract_aggregate_id(event),
      aggregate_type: extract_aggregate_type(event)
    }

    case Financial.Repo.insert(stored_event) do
      {:ok, _} -> {:reply, :ok, state}
      {:error, reason} -> {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:get_events, aggregate_type, aggregate_id}, _from, state) do
    import Ecto.Query

    events =
      from(e in StoredEvent,
        where: e.aggregate_type == ^aggregate_type and e.aggregate_id == ^aggregate_id,
        order_by: [asc: e.occurred_at]
      )
      |> Financial.Repo.all()
      |> Enum.map(&deserialize_event/1)

    {:reply, events, state}
  end

  defp extract_aggregate_id(%{donation_id: id}), do: id
  defp extract_aggregate_id(%{campaign_id: id}), do: id
  defp extract_aggregate_id(_), do: nil

  defp extract_aggregate_type(%DonationReceived{}), do: "Donation"
  defp extract_aggregate_type(%CampaignCompleted{}), do: "Campaign"
  defp extract_aggregate_type(_), do: "Unknown"

  defp deserialize_event(%StoredEvent{event_type: "Elixir.DonationReceived", event_data: data}) do
    struct(DonationReceived, data)
  end

  defp deserialize_event(%StoredEvent{event_type: "Elixir.CampaignCompleted", event_data: data}) do
    struct(CampaignCompleted, data)
  end

  defp deserialize_event(_), do: nil
end
```

### Tests

```elixir
defmodule DonationReceivedTest do
  use ExUnit.Case, async: true

  test "creates event with required fields" do
    event = DonationReceived.new(
      "don_123",
      Money.new(100, :IDR),
      "camp_456",
      "donor_789"
    )

    assert event.donation_id == "don_123"
    assert Money.equal?(event.amount, Money.new(100, :IDR))
    assert event.campaign_id == "camp_456"
    assert event.donor_id == "donor_789"
    assert %DateTime{} = event.occurred_at
  end

  test "includes metadata when provided" do
    metadata = %{source: "web", ip: "192.168.1.1"}

    event = DonationReceived.new(
      "don_123",
      Money.new(100, :IDR),
      "camp_456",
      "donor_789",
      metadata: metadata
    )

    assert event.metadata == metadata
  end
end

defmodule EventPublisherTest do
  use ExUnit.Case, async: false

  test "publishes event to subscribers" do
    # Subscribe to events
    Phoenix.PubSub.subscribe(Financial.PubSub, "domain_events")

    event = DonationReceived.new(
      "don_123",
      Money.new(100, :IDR),
      "camp_456",
      "donor_789"
    )

    # Publish
    EventPublisher.publish(event)

    # Assert received
    assert_receive %DonationReceived{donation_id: "don_123"}
  end

  test "publishes multiple events" do
    Phoenix.PubSub.subscribe(Financial.PubSub, "domain_events")

    events = [
      DonationReceived.new("don_1", Money.new(100, :IDR), "camp_1", "donor_1"),
      DonationReceived.new("don_2", Money.new(200, :IDR), "camp_2", "donor_2")
    ]

    EventPublisher.publish_all(events)

    assert_receive %DonationReceived{donation_id: "don_1"}
    assert_receive %DonationReceived{donation_id: "don_2"}
  end
end
```

## Financial Domain Example: Zakat Events

```elixir
defmodule ZakatCalculated do
  @enforce_keys [:zakat_payment_id, :wealth, :nisab, :zakat_amount, :occurred_at]
  defstruct [:zakat_payment_id, :wealth, :nisab, :zakat_amount, :taxpayer_id, :occurred_at]

  def new(zakat_payment_id, wealth, nisab, zakat_amount, taxpayer_id) do
    %__MODULE__{
      zakat_payment_id: zakat_payment_id,
      wealth: wealth,
      nisab: nisab,
      zakat_amount: zakat_amount,
      taxpayer_id: taxpayer_id,
      occurred_at: DateTime.utc_now()
    }
  end
end

defmodule ZakatPaid do
  @enforce_keys [:zakat_payment_id, :amount, :payment_reference, :occurred_at]
  defstruct [:zakat_payment_id, :amount, :payment_reference, :taxpayer_id, :occurred_at]

  def new(zakat_payment_id, amount, payment_reference, taxpayer_id) do
    %__MODULE__{
      zakat_payment_id: zakat_payment_id,
      amount: amount,
      payment_reference: payment_reference,
      taxpayer_id: taxpayer_id,
      occurred_at: DateTime.utc_now()
    }
  end
end

defmodule ZakatEventHandler do
  use GenServer
  require Logger

  def start_link(_) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init(_) do
    Phoenix.PubSub.subscribe(Financial.PubSub, "domain_events")
    {:ok, %{}}
  end

  def handle_info(%ZakatCalculated{} = event, state) do
    Logger.info("Zakat calculated: #{event.zakat_payment_id}")

    # Send calculation notification
    send_calculation_notification(event)

    # Update taxpayer record
    update_taxpayer_status(event)

    {:noreply, state}
  end

  def handle_info(%ZakatPaid{} = event, state) do
    Logger.info("Zakat paid: #{event.zakat_payment_id}")

    # Send payment confirmation
    send_payment_confirmation(event)

    # Issue receipt
    issue_payment_receipt(event)

    # Update tax records
    update_tax_records(event)

    {:noreply, state}
  end

  defp send_calculation_notification(event) do
    Task.start(fn ->
      Email.send_zakat_calculation(event.taxpayer_id, event.zakat_amount)
    end)
  end

  defp update_taxpayer_status(event) do
    Task.start(fn ->
      TaxpayerStatus.mark_calculated(event.taxpayer_id, event.zakat_payment_id)
    end)
  end

  defp send_payment_confirmation(event) do
    Task.start(fn ->
      Email.send_payment_confirmation(event.taxpayer_id, event.payment_reference)
    end)
  end

  defp issue_payment_receipt(event) do
    Task.start(fn ->
      ReceiptGenerator.generate(event.zakat_payment_id, event.payment_reference)
    end)
  end

  defp update_tax_records(event) do
    Task.start(fn ->
      TaxRecords.record_payment(event.taxpayer_id, event.zakat_payment_id)
    end)
  end
end
```

## Common Mistakes

### 1. Mutable Events

```elixir
# Bad: Mutable event
defmodule DonationReceived do
  defstruct [:donation_id, :amount]
  # NO! Can be mutated!
end

# Good: Immutable with @enforce_keys
defmodule DonationReceived do
  @enforce_keys [:donation_id, :amount, :occurred_at]
  defstruct [:donation_id, :amount, :occurred_at]
end
```

### 2. Missing Timestamp

```elixir
# Bad: No timestamp
defmodule DonationReceived do
  @enforce_keys [:donation_id]
  defstruct [:donation_id]
end

# Good: Always include occurred_at
defmodule DonationReceived do
  @enforce_keys [:donation_id, :occurred_at]
  defstruct [:donation_id, :occurred_at]

  def new(donation_id) do
    %__MODULE__{
      donation_id: donation_id,
      occurred_at: DateTime.utc_now()
    }
  end
end
```

### 3. Synchronous Side Effects

```elixir
# Bad: Blocking side effects
def handle_info(event, state) do
  send_email(event)  # Blocks!
  update_database(event)  # Blocks!
  {:noreply, state}
end

# Good: Async side effects
def handle_info(event, state) do
  Task.start(fn -> send_email(event) end)
  Task.start(fn -> update_database(event) end)
  {:noreply, state}
end
```

## Best Practices

1. **Immutable**: Events never change
2. **Past Tense**: Name events in past tense (DonationReceived, not ReceiveDonation)
3. **Complete Info**: Include all context needed by handlers
4. **Timestamp**: Always include `occurred_at`
5. **Async Handlers**: Use Task.start for side effects
6. **Event Store**: Persist events for audit and replay
7. **Versioning**: Plan for event schema evolution

## Resources

- [Back to Templates README](README.md)
- [Phoenix PubSub](https://hexdocs.pm/phoenix_pubsub)
- [GenServer Patterns](../ex-so-stla-el__otp-genserver.md)
- [Event Sourcing](https://martinfowler.com/eaaDev/EventSourcing.html)

---

**Last Updated**: 2025-01-23
**Complexity**: Medium
**Production Ready**: ✅ Yes
