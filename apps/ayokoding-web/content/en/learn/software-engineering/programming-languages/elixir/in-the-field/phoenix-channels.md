---
title: "Phoenix Channels"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000017
description: "Real-time WebSocket communication using Phoenix Channels for broadcasting, presence tracking, and client synchronization"
tags: ["elixir", "phoenix", "channels", "websocket", "real-time", "pubsub", "presence"]
prev: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/phoenix-framework"
next: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/ecto-patterns"
---

## When GenServer PubSub Falls Short

Standard GenServer patterns provide process-level messaging, but real-time web applications need WebSocket-based communication between servers and clients.

```elixir
# GenServer PubSub for process communication
defmodule DonationBroadcaster do
  # => Broadcasting within BEAM VM only
  # => No web client communication
  use GenServer
  # => OTP GenServer behavior

  def start_link(_opts) do
    # => Initialize broadcaster process
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
    # => Returns: {:ok, pid}
    # => Named process for easy access
  end

  def broadcast_donation(campaign_id, amount, donor) do
    # => Public API for broadcasting
    # => campaign_id: string identifier
    # => amount: integer donation value
    GenServer.cast(__MODULE__, {:broadcast, campaign_id, amount, donor})
    # => Async message to GenServer
    # => Returns: :ok immediately
  end

  def init(_) do
    # => Setup subscriptions on init
    Phoenix.PubSub.subscribe(MyApp.PubSub, "donations")
    # => Subscribe to "donations" topic
    # => Only receives messages from same VM
    # => Cannot reach external clients
    {:ok, %{}}
    # => Empty state map
  end

  def handle_cast({:broadcast, campaign_id, amount, donor}, state) do
    # => Handle broadcast request
    # => Pattern match tuple: {:broadcast, ...}
    Phoenix.PubSub.broadcast(MyApp.PubSub, "donations", {:new_donation, campaign_id, amount, donor})
    # => Broadcasts to all subscribed processes
    # => Message: {:new_donation, ...} tuple
    # => Limited to server processes only
    # => Web clients cannot receive
    {:noreply, state}
    # => Continue without reply
  end
end
```

**Limitations of GenServer PubSub**:

- **No WebSocket support** - Cannot communicate with browser clients directly
- **Manual connection handling** - Need custom WebSocket implementation for external clients
- **No presence tracking** - Cannot track which users are connected
- **No room abstraction** - Manual topic management for different channels

## Phoenix Channels - Real-time Communication Layer

Phoenix Channels provide WebSocket abstraction with built-in PubSub, presence tracking, and room management.

### Channel Basics

**Architecture components**:

- **Channel** - Server-side process handling WebSocket communication
- **Socket** - Connection between client and server
- **Topic** - String identifier for message routing (e.g., "campaign:ramadan_2026")
- **PubSub** - Underlying message distribution system

```elixir
# Define channel for donation campaigns
defmodule MyAppWeb.CampaignChannel do
  # => Channel module for real-time updates
  # => Handles WebSocket connections
  use MyAppWeb, :channel
  # => Phoenix Channel behavior
  # => Provides: join/3, handle_in/3, broadcast/3

  @impl true
  def join("campaign:" <> campaign_id, _payload, socket) do
    # => Join callback when client connects
    # => Pattern match: "campaign:" prefix + ID
    # => campaign_id: extracted from topic string
    # => socket: Connection state container
    send(self(), {:after_join, campaign_id})
    # => Send async message to self
    # => Allows join to return quickly
    # => Post-join setup happens in handle_info

    {:ok, socket}
    # => Allow connection
    # => Returns socket for future messages
    # => Client now connected
  end
  # => Other topics automatically rejected

  @impl true
  def handle_info({:after_join, campaign_id}, socket) do
    # => Handle post-join setup
    # => Called after join completes
    # => campaign_id: from send(self(), ...)
    campaign = get_campaign_data(campaign_id)
    # => Fetch current campaign state from database
    # => Returns: map with campaign details

    push(socket, "campaign_data", campaign)
    # => Send message to connected client
    # => Event name: "campaign_data"
    # => Payload: campaign map
    # => Only sent to this client

    {:noreply, socket}
    # => Continue with same socket
    # => No state change needed
  end

  @impl true
  def handle_in("new_donation", %{"amount" => amount, "donor" => donor}, socket) do
    # => Handle incoming message from client
    # => Event: "new_donation"
    # => Payload destructured: amount, donor
    # => socket: current connection state
    campaign_id = socket.assigns.campaign_id
    # => Extract campaign ID from socket assigns
    # => Stored during join

    process_donation(campaign_id, amount, donor)
    # => Business logic: record donation
    # => Database write happens here

    broadcast(socket, "donation_received", %{
      # => Broadcast to ALL clients on this topic
      # => Including sender
      amount: amount,
      # => amount: integer IDR value
      donor: donor,
      # => donor: string name
      timestamp: DateTime.utc_now()
      # => Include server timestamp
      # => Ensures consistent time across clients
    })
    # => All connected clients receive update
    # => Real-time notification

    {:reply, :ok, socket}
    # => Acknowledge to sender
    # => Reply: :ok atom
    # => Continue with same socket
  end

  defp get_campaign_data(campaign_id) do
    # => Fetch campaign details from database
    # => campaign_id: string identifier
    %{
      id: campaign_id,
      # => Same ID as requested
      goal: 100_000_000,
      # => Target: 100 million IDR
      raised: 45_000_000,
      # => Current progress: 45 million IDR
      donors: 1250
      # => Total unique donors
    }
    # => Returns: map with campaign state
  end

  defp process_donation(campaign_id, amount, donor) do
    # => Record donation in database
    # => Update campaign totals
    # => Validate amount > 0
    :ok
    # => Returns: :ok on success
  end
end
```

### Socket Configuration

**Endpoint socket setup**:

```elixir
# lib/my_app_web/endpoint.ex
defmodule MyAppWeb.Endpoint do
  # => Phoenix endpoint configuration
  use Phoenix.Endpoint, otp_app: :my_app
  # => Defines endpoint behavior

  socket "/socket", MyAppWeb.UserSocket,
    # => WebSocket endpoint path: /socket
    # => Routes to: MyAppWeb.UserSocket
    websocket: true,
    # => Enable WebSocket transport
    # => Preferred for real-time
    longpoll: false
    # => Disable legacy long-polling
    # => Reduces server overhead

  # ... other endpoint config
end
```

**Socket implementation**:

```elixir
# lib/my_app_web/channels/user_socket.ex
defmodule MyAppWeb.UserSocket do
  # => Socket connection handler
  use Phoenix.Socket
  # => Phoenix Socket behavior
  # => Handles WebSocket lifecycle

  channel "campaign:*", MyAppWeb.CampaignChannel
  # => Route topics to channels
  # => Pattern: "campaign:*" matches all campaign topics
  # => Examples: campaign:ramadan_2026, campaign:education_2026
  # => Handler: MyAppWeb.CampaignChannel

  @impl true
  def connect(_params, socket, _connect_info) do
    # => Connection callback
    # => Called when client first connects
    # => Before any channel joins
    # => _params: query params from client
    {:ok, socket}
    # => Accept connection
    # => socket: connection state
    # => Can add authentication here
  end

  @impl true
  def id(_socket), do: nil
  # => Socket ID for connection identification
  # => nil: don't track socket by ID
  # => Can return user ID for presence tracking
  # => Format: "user:#{user_id}"
end
```

### Client-Side JavaScript Integration

```javascript
// assets/js/socket.js
import { Socket } from "phoenix";
// => Import Phoenix Socket client
// => Handles WebSocket connection

// Connect to socket
let socket = new Socket("/socket", {
  // => Create socket instance
  // => Path: /socket matches endpoint config
  params: { token: window.userToken },
  // => Authentication token
  // => Sent to connect/3 callback
});
socket.connect();
// => Establish WebSocket connection
// => Async operation

// Join campaign channel
let channel = socket.channel("campaign:ramadan_2026", {});
// => Create channel instance
// => Topic: "campaign:ramadan_2026"
// => Payload: empty object (no join params)

channel
  .join()
  // => Attempt to join channel
  // => Calls join/3 on server
  .receive("ok", (resp) => {
    // => Handle successful join
    // => resp: response from server
    console.log("Joined campaign", resp);
    // => Log success message
  })
  .receive("error", (resp) => {
    // => Handle join failure
    // => resp: error details
    console.log("Unable to join", resp);
    // => Log error message
  });

// Listen for donation events
channel.on("donation_received", (payload) => {
  // => Subscribe to event
  // => Event name: "donation_received"
  // => payload: {amount, donor, timestamp}
  console.log(`New donation: ${payload.amount} from ${payload.donor}`);
  // => Log donation details
  // => amount: integer IDR
  // => donor: string name

  updateCampaignUI(payload);
  // => Update page with new donation
  // => Reflects real-time changes
});

// Send donation from client
function submitDonation(amount, donor) {
  // => Client-side donation submission
  // => amount: integer donation value
  // => donor: string donor name
  channel
    .push("new_donation", { amount, donor })
    // => Send message to server
    // => Event: "new_donation"
    // => Payload: {amount, donor}
    .receive("ok", () => console.log("Donation sent"))
    // => Handle success acknowledgment
    .receive("error", (e) => console.log("Error", e));
  // => Handle error response
}
```

## Broadcasting Patterns

### Broadcasting to All Connected Clients

```elixir
defmodule MyAppWeb.CampaignChannel do
  # => Campaign channel with broadcasting
  use MyAppWeb, :channel

  def handle_in("update_campaign", %{"raised" => raised}, socket) do
    # => Handle campaign update request from client
    # => Event: "update_campaign"
    # => Payload: %{"raised" => new_amount}
    campaign_id = socket.assigns.campaign_id
    # => Extract campaign ID from socket state
    # => Stored during join

    broadcast(socket, "campaign_updated", %{
      # => Broadcast to ALL clients on this topic
      # => Including sender (no self-exclusion)
      campaign_id: campaign_id,
      # => Include campaign identifier
      raised: raised,
      # => New raised amount
      timestamp: DateTime.utc_now()
      # => Server timestamp for consistency
    })
    # => Every connected client receives update
    # => Real-time synchronization

    {:reply, :ok, socket}
    # => Acknowledge to sender
    # => Reply: :ok success atom
  end
end
```

### Broadcasting from External Processes

**PubSub pattern for server-initiated broadcasts**:

```elixir
# Broadcast from anywhere in your application
defmodule DonationProcessor do
  # => Background donation processor
  # => Handles offline donations

  def process_bank_transfer(campaign_id, amount, donor) do
    # => Process offline donation
    # => campaign_id: string identifier
    # => amount: integer IDR value
    # => donor: string name
    record_donation(campaign_id, amount, donor)
    # => Save to database first
    # => Ensures persistence before broadcast

    # Broadcast to all connected clients
    MyAppWeb.Endpoint.broadcast("campaign:#{campaign_id}", "donation_received", %{
      # => Broadcast through endpoint (not channel)
      # => Topic: "campaign:ramadan_2026" (interpolated)
      # => Event: "donation_received"
      amount: amount,
      # => Donation amount
      donor: donor,
      # => Donor name
      source: "bank_transfer",
      # => Distinguishes offline donations
      timestamp: DateTime.utc_now()
      # => Server timestamp
    })
    # => All clients on this campaign receive update
    # => No channel process needed
  end

  defp record_donation(campaign_id, amount, donor) do
    # => Database insertion
    # => Insert into donations table
    :ok
    # => Returns: :ok on success
  end
end

# Usage: Broadcast from GenServer
defmodule CampaignWorker do
  # => Background worker for periodic updates
  use GenServer
  # => GenServer behavior

  def handle_info(:refresh_campaign_data, state) do
    # => Handle periodic refresh trigger
    # => Scheduled with Process.send_after
    campaign_id = state.campaign_id
    # => Extract from GenServer state
    updated_data = fetch_campaign_summary(campaign_id)
    # => Get latest campaign stats from database
    # => Returns: map with totals

    MyAppWeb.Endpoint.broadcast!("campaign:#{campaign_id}", "campaign_refreshed", updated_data)
    # => broadcast! raises on error
    # => Topic: campaign with ID
    # => Event: "campaign_refreshed"
    # => All clients receive fresh data

    schedule_refresh()
    # => Schedule next refresh in 1 minute
    {:noreply, state}
    # => Continue with same state
  end

  defp fetch_campaign_summary(campaign_id) do
    # => Aggregate campaign data from database
    # => campaign_id: string identifier
    %{raised: 50_000_000, donors: 1350, goal: 100_000_000}
    # => Returns: current campaign stats
  end

  defp schedule_refresh do
    # => Schedule next refresh
    Process.send_after(self(), :refresh_campaign_data, :timer.minutes(1))
    # => Send message to self after 1 minute
    # => Triggers handle_info(:refresh_campaign_data, ...)
  end
end
```

## Presence Tracking

Phoenix Presence tracks which users are connected to channels in real-time.

### Presence Setup

```elixir
# lib/my_app_web/channels/presence.ex
defmodule MyAppWeb.Presence do
  # => Presence tracking module
  use Phoenix.Presence,
    # => Phoenix Presence behavior
    # => Handles distributed presence
    otp_app: :my_app,
    # => Application name
    pubsub_server: MyApp.PubSub
    # => PubSub backend for presence sync
    # => Syncs across nodes
end
```

**Add to supervision tree**:

```elixir
# lib/my_app/application.ex
defmodule MyApp.Application do
  # => Main application module
  use Application

  def start(_type, _args) do
    # => Application start callback
    children = [
      MyApp.Repo,
      # => Database connection pool
      MyAppWeb.Endpoint,
      # => Phoenix endpoint
      MyAppWeb.Presence,
      # => Start Presence tracker
      # => Tracks user connections
      # ... other children
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: MyApp.Supervisor)
    # => Start supervisor
    # => strategy: :one_for_one (restart failed child only)
  end
end
```

### Tracking User Presence

```elixir
defmodule MyAppWeb.CampaignChannel do
  # => Campaign channel with presence tracking
  use MyAppWeb, :channel
  alias MyAppWeb.Presence
  # => Alias Presence module

  @impl true
  def join("campaign:" <> campaign_id, %{"user_id" => user_id}, socket) do
    # => Join with user authentication
    # => Pattern match topic: "campaign:" + ID
    # => Payload must include user_id
    send(self(), :after_join)
    # => Async post-join setup
    # => Allows join to return quickly

    {:ok, assign(socket, :user_id, user_id)}
    # => Store user_id in socket assigns
    # => Available as socket.assigns.user_id
    # => Returns: {:ok, updated_socket}
  end

  @impl true
  def handle_info(:after_join, socket) do
    # => Track user presence after join
    # => Called after join completes
    push(socket, "presence_state", Presence.list(socket))
    # => Send current presence list to joining user
    # => Event: "presence_state"
    # => Payload: map of connected users
    # => Shows who's already here

    {:ok, _} = Presence.track(socket, socket.assigns.user_id, %{
      # => Track this user's presence
      # => Key: user_id from socket.assigns
      # => Metadata: custom data
      online_at: inspect(System.system_time(:second))
      # => Metadata: connection timestamp
      # => Format: string of seconds since epoch
    })
    # => User now visible to all clients
    # => Broadcasts presence_diff to all

    {:noreply, socket}
    # => Continue with socket
  end

  @impl true
  def terminate(_reason, socket) do
    # => Cleanup on disconnect
    # => Called when client disconnects
    # => Presence automatically untracked
    # => Broadcasts presence_diff to remaining clients
    :ok
  end
end
```

### Client-Side Presence Handling

```javascript
// assets/js/socket.js
import { Presence } from "phoenix";
// => Import Presence client
// => Handles presence state and diffs

let channel = socket.channel("campaign:ramadan_2026", { user_id: currentUserId });
// => Create channel with user_id
// => currentUserId: from application state
let presence = new Presence(channel);
// => Create Presence instance
// => Binds to channel events

// Track presence changes
presence.onSync(() => {
  // => Called when presence state changes
  // => Triggered by: joins, leaves, metadata updates
  displayUsers(presence.list());
  // => Update UI with current users
  // => presence.list() returns current state
});

function displayUsers(presences) {
  // => Render user list
  // => presences: map of user presences
  let userList = document.getElementById("user-list");
  // => Get DOM element
  userList.innerHTML = "";
  // => Clear existing list

  presence.list((id, { metas }) => {
    // => Iterate over present users
    // => id: user_id (tracking key)
    // => metas: array of presence metadata
    let li = document.createElement("li");
    // => Create list item element
    li.textContent = `User ${id} (online at: ${metas[0].online_at})`;
    // => Set text: user ID and timestamp
    // => metas[0]: first (usually only) metadata entry
    userList.appendChild(li);
    // => Add to list
  });
}

channel
  .join()
  // => Attempt to join channel
  .receive("ok", (resp) => console.log("Joined campaign"))
  // => Handle success
  .receive("error", (resp) => console.log("Join failed", resp));
// => Handle failure
```

## Production Pattern - Live Donation Dashboard

```elixir
# Complete real-time donation tracking system
defmodule MyAppWeb.DonationDashboardChannel do
  # => Admin dashboard channel
  # => Real-time donation monitoring
  use MyAppWeb, :channel
  alias MyAppWeb.Presence
  # => Presence tracking for admins

  @impl true
  def join("dashboard:live", %{"admin_token" => token}, socket) do
    # => Admin-only dashboard join
    # => Topic: "dashboard:live"
    # => Payload requires: admin_token
    case verify_admin(token) do
      {:ok, admin_id} ->
        # => Valid admin token
        # => admin_id: authenticated admin identifier
        send(self(), {:after_join, admin_id})
        # => Async post-join setup
        {:ok, assign(socket, :admin_id, admin_id)}
        # => Store admin ID in socket
        # => Allow connection

      {:error, _reason} ->
        # => Invalid token
        # => Authentication failed
        {:error, %{reason: "unauthorized"}}
        # => Reject connection
        # => Client receives error
    end
  end

  @impl true
  def handle_info({:after_join, admin_id}, socket) do
    # => Post-join setup for admin
    # => admin_id: from send(self(), ...)
    Presence.track(socket, admin_id, %{
      # => Track admin presence
      role: "admin",
      # => Metadata: role identifier
      joined_at: DateTime.utc_now()
      # => Metadata: join timestamp
    })
    # => Admin visible to other admins

    push(socket, "dashboard_state", get_dashboard_data())
    # => Send initial dashboard data
    # => Event: "dashboard_state"
    # => Payload: current campaign stats, recent donations
    # => Initializes admin view

    {:noreply, socket}
    # => Continue with socket
  end

  @impl true
  def handle_in("request_campaign_update", %{"campaign_id" => campaign_id}, socket) do
    # => Admin requests specific campaign refresh
    # => Event: "request_campaign_update"
    # => Payload: campaign_id to fetch
    data = get_campaign_details(campaign_id)
    # => Fetch latest campaign data
    # => Returns: map with campaign details

    {:reply, {:ok, data}, socket}
    # => Respond to admin only
    # => Reply: {:ok, data} tuple
    # => Not broadcast to all admins
  end

  defp get_dashboard_data do
    # => Aggregate all campaigns
    # => Fetch summary statistics
    %{
      total_raised: 250_000_000,
      # => Total across all campaigns
      # => Sum of all raised amounts
      active_campaigns: 8,
      # => Number of ongoing campaigns
      recent_donations: [
        # => Latest donations across all campaigns
        %{campaign: "ramadan_2026", amount: 1_000_000, donor: "Ahmad"},
        # => First recent donation
        %{campaign: "education_2026", amount: 500_000, donor: "Fatimah"}
        # => Second recent donation
      ]
    }
    # => Returns: dashboard summary map
  end

  defp get_campaign_details(campaign_id) do
    # => Fetch specific campaign details
    # => campaign_id: string identifier
    %{
      id: campaign_id,
      # => Campaign identifier
      raised: 45_000_000,
      # => Current raised amount
      goal: 100_000_000,
      # => Target goal
      donors: 1250
      # => Unique donor count
    }
    # => Returns: campaign detail map
  end

  defp verify_admin(token) do
    # => Validate admin token
    # => token: from join payload
    # => In production: check JWT, database session, etc.
    if token == "admin_secret" do
      # => Development-only check
      {:ok, "admin_1"}
      # => Returns: admin ID
    else
      # => Invalid token
      {:error, :invalid_token}
      # => Returns: error tuple
    end
  end
end
```

**Broadcasting to dashboard from donation processor**:

```elixir
defmodule DonationProcessor do
  # => Process donations and broadcast updates
  # => Coordinates database and channel updates

  def process_donation(campaign_id, amount, donor) do
    # => Main donation processing
    # => campaign_id: string identifier
    # => amount: integer IDR value
    # => donor: string name
    record_donation(campaign_id, amount, donor)
    # => Record donation in database
    # => Ensures persistence first

    # Broadcast to campaign channel
    MyAppWeb.Endpoint.broadcast("campaign:#{campaign_id}", "donation_received", %{
      # => Notify campaign-specific clients
      # => Topic: specific campaign
      # => All campaign viewers receive update
      amount: amount,
      # => Donation amount
      donor: donor,
      # => Donor name
      timestamp: DateTime.utc_now()
      # => Server timestamp
    })
    # => Campaign page updates in real-time

    # Broadcast to admin dashboard
    MyAppWeb.Endpoint.broadcast("dashboard:live", "new_donation", %{
      # => Notify all admin users
      # => Topic: admin dashboard
      # => All admins receive notification
      campaign_id: campaign_id,
      # => Which campaign received donation
      amount: amount,
      # => Donation amount
      donor: donor,
      # => Donor name
      timestamp: DateTime.utc_now()
      # => Server timestamp
    })
    # => Dashboard updates in real-time
  end

  defp record_donation(campaign_id, amount, donor) do
    # => Database insertion
    # => Insert into donations table
    # => Update campaign totals
    :ok
    # => Returns: :ok on success
  end
end
```

## Channel Testing

```elixir
# test/my_app_web/channels/campaign_channel_test.exs
defmodule MyAppWeb.CampaignChannelTest do
  # => Channel testing suite
  use MyAppWeb.ChannelCase
  # => Channel testing helpers
  # => Provides: socket/2, subscribe_and_join/3, etc.

  setup do
    # => Setup for each test
    # => Runs before every test
    {:ok, _, socket} =
      MyAppWeb.UserSocket
      |> socket("user_id", %{some: :assign})
      # => Create test socket
      # => Assigns: %{some: :assign}
      |> subscribe_and_join(MyAppWeb.CampaignChannel, "campaign:test")
      # => Join test topic
      # => Returns: {:ok, reply, socket}

    %{socket: socket}
    # => Return socket for tests
    # => Available as %{socket: socket} in test context
  end

  test "broadcasts donation to all clients", %{socket: socket} do
    # => Test broadcast behavior
    # => socket: from setup
    push(socket, "new_donation", %{"amount" => 100_000, "donor" => "Test"})
    # => Send message to channel
    # => Event: "new_donation"
    # => Payload: %{"amount" => 100_000, "donor" => "Test"}

    assert_broadcast "donation_received", %{amount: 100_000, donor: "Test"}
    # => Verify broadcast received
    # => Event: "donation_received"
    # => Payload matches: amount and donor
    # => All clients would receive this
  end

  test "replies with ok to donation", %{socket: socket} do
    # => Test reply behavior
    # => socket: from setup
    ref = push(socket, "new_donation", %{"amount" => 100_000, "donor" => "Test"})
    # => Send and capture reference
    # => ref: unique message reference

    assert_reply ref, :ok
    # => Verify sender received acknowledgment
    # => Reply: :ok atom
    # => Confirms message processed
  end
end
```

## Summary

**GenServer PubSub limitations**: No WebSocket support, manual connection handling, no presence tracking

**Phoenix Channels provide**: WebSocket abstraction, automatic connection management, room-based routing

**Broadcasting patterns**: Client-triggered broadcasts, server-initiated broadcasts from any process

**Presence tracking**: Real-time user connection tracking with metadata

**Production use cases**: Live donation dashboards, campaign updates, admin monitoring

**Real-time benefits**: Immediate updates to all connected clients, scalable WebSocket infrastructure

**Next steps**: Explore [distributed Phoenix](/en/learn/software-engineering/programming-languages/elixir/in-the-field/distributed-phoenix) for multi-node real-time systems, or [performance optimization](/en/learn/software-engineering/programming-languages/elixir/in-the-field/performance-optimization) for channel scaling.
