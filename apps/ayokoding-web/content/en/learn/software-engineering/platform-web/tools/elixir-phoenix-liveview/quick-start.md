---
title: "Quick Start"
date: 2026-02-01T00:00:00+07:00
draft: false
weight: 10000000
description: "Rapid tour of Phoenix LiveView essentials - real-time forms, validation, PubSub, and interactive components in one comprehensive tutorial"
tags: ["phoenix", "liveview", "quick-start", "elixir", "real-time", "forms"]
---

**Ready to build with Phoenix LiveView?** This quick start tutorial provides a fast-paced tour through LiveView's core capabilities. By the end, you'll build a real-time chat room with message validation, user presence tracking, and live updates.

This tutorial provides 5-30% coverage—practical hands-on experience with essential LiveView features. For comprehensive learning, continue to [By Example](/en/learn/software-engineering/platform-web/tools/elixir-phoenix-liveview/by-example) (95% coverage).

## Prerequisites

Before starting this tutorial, you need:

- Phoenix LiveView installed and configured (see [Initial Setup](/en/learn/software-engineering/platform-web/tools/elixir-phoenix-liveview/initial-setup))
- Elixir fundamentals (pattern matching, processes) - see [Elixir by Example](/en/learn/software-engineering/programming-languages/elixir/by-example)
- Phoenix basics (routing, controllers) - see [Phoenix by Example](/en/learn/software-engineering/platform-web/tools/elixir-phoenix/by-example)
- Basic understanding of WebSockets

## Learning Objectives

By the end of this tutorial, you will understand:

1. **LiveView Lifecycle** - mount, render, handle_event callbacks
2. **State Management** - Assigns, socket, updates
3. **Forms and Validation** - Changesets, live validation, error display
4. **Events** - phx-click, phx-submit, phx-change
5. **PubSub** - Real-time multi-user synchronization
6. **Presence** - Tracking who's online
7. **Assigns Updates** - update/3, assign/3, push_event
8. **Best Practices** - Stateless LiveViews, temporary assigns, performance

## The Scenario: Real-Time Chat Room

We'll build a chat application with:

- Message submission with validation (no empty messages, 200 char max)
- Real-time message broadcast (all users see new messages instantly)
- User presence tracking (see who's online)
- Automatic scrolling to latest message
- Message timestamps

This demonstrates LiveView's real-time capabilities and form handling.

## Project Setup

Assuming you have a Phoenix app with LiveView configured:

```bash
# Create LiveView file
touch lib/my_app_web/live/chat_live.ex

# Add route (edit router.ex)
# live "/chat", ChatLive
```

## Basic LiveView Structure

Start with minimal LiveView - just mount and render:

```elixir
defmodule MyAppWeb.ChatLive do
  use MyAppWeb, :live_view

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, messages: [], username: generate_username())}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="chat-container">
      <h1>Chat Room</h1>
      <p>Welcome, <%= @username %>!</p>

      <div class="messages">
        <%= for message <- @messages do %>
          <div class="message">
            <strong><%= message.username %>:</strong>
            <%= message.text %>
          </div>
        <% end %>
      </div>
    </div>
    """
  end

  defp generate_username do
    "User#{:rand.uniform(9999)}"
  end
end
```

**Key concepts**:

- `mount/3`: Initialize state (runs on initial load and WebSocket connect)
- `assign/3`: Add data to socket assigns (accessible in template as `@key`)
- `~H"""` sigil: HEEx template syntax (HTML with Elixir)
- `<%= %>`: Output Elixir expression in template

Start server and visit `http://localhost:4000/chat` - you'll see empty chat room with random username.

## Adding Form for Messages

Add a form to submit messages:

```elixir
def render(assigns) do
  ~H"""
  <div class="chat-container">
    <h1>Chat Room</h1>
    <p>Welcome, <%= @username %>!</p>

    <div class="messages">
      <%= for message <- @messages do %>
        <div class="message">
          <strong><%= message.username %>:</strong>
          <%= message.text %>
        </div>
      <% end %>
    </div>

    <form phx-submit="send_message">
      <input
        type="text"
        name="message"
        placeholder="Type a message..."
        autocomplete="off"
      />
      <button type="submit">Send</button>
    </form>
  </div>
  """
end

@impl true
def handle_event("send_message", %{"message" => text}, socket) do
  message = %{
    username: socket.assigns.username,
    text: text,
    timestamp: DateTime.utc_now()
  }

  {:noreply, update(socket, :messages, fn messages -> messages ++ [message] end)}
end
```

**What this does**:

- `phx-submit="send_message"`: LiveView event binding (submit triggers server event)
- `handle_event/3`: Handle "send_message" event, extract text from params
- `update/3`: Update assigns by applying function to current value
- `{:noreply, socket}`: Return updated socket without reply message

Type a message and click Send - it appears in the message list (but only for you, not other users yet).

## Form Validation with Changesets

Add validation to prevent empty messages and enforce length limits:

```elixir
defmodule MyAppWeb.ChatLive do
  use MyAppWeb, :live_view
  alias Ecto.Changeset

  @impl true
  def mount(_params, _session, socket) do
    socket =
      socket
      |> assign(messages: [], username: generate_username())
      |> assign_form(to_form(%{}, as: :message))

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="chat-container">
      <h1>Chat Room</h1>
      <p>Welcome, <%= @username %>!</p>

      <div class="messages">
        <%= for message <- @messages do %>
          <div class="message">
            <strong><%= message.username %>:</strong>
            <%= message.text %>
            <small><%= format_time(message.timestamp) %></small>
          </div>
        <% end %>
      </div>

      <.form for={@form} phx-submit="send_message" phx-change="validate">
        <.input field={@form[:text]} placeholder="Type a message..." />
        <.button>Send</.button>

        <.error :if={@form.errors[:text]}>
          <%= translate_error(@form.errors[:text]) %>
        </.error>
      </.form>
    </div>
    """
  end

  @impl true
  def handle_event("validate", %{"message" => params}, socket) do
    changeset =
      %{text: ""}
      |> cast(params, [:text])
      |> validate_required([:text], message: "Message cannot be empty")
      |> validate_length(:text, max: 200, message: "Message too long (max 200 chars)")

    {:noreply, assign_form(socket, changeset)}
  end

  @impl true
  def handle_event("send_message", %{"message" => params}, socket) do
    changeset =
      %{text: ""}
      |> cast(params, [:text])
      |> validate_required([:text])
      |> validate_length(:text, max: 200)

    case Changeset.apply_action(changeset, :insert) do
      {:ok, data} ->
        message = %{
          username: socket.assigns.username,
          text: data.text,
          timestamp: DateTime.utc_now()
        }

        socket = update(socket, :messages, fn messages -> messages ++ [message] end)
        socket = assign_form(socket, to_form(%{}, as: :message))  # Reset form

        {:noreply, socket}

      {:error, changeset} ->
        {:noreply, assign_form(socket, changeset)}
    end
  end

  defp assign_form(socket, changeset_or_form) do
    assign(socket, :form, to_form(changeset_or_form))
  end

  defp cast(data, params, allowed) do
    {data, %{text: :string}}
    |> Changeset.cast(params, allowed)
  end

  defp validate_required(changeset, fields, opts \\ []) do
    Changeset.validate_required(changeset, fields, opts)
  end

  defp validate_length(changeset, field, opts) do
    Changeset.validate_length(changeset, field, opts)
  end

  defp translate_error({msg, _opts}), do: msg
  defp translate_error(msg) when is_binary(msg), do: msg

  defp format_time(datetime) do
    Calendar.strftime(datetime, "%H:%M:%S")
  end

  defp generate_username do
    "User#{:rand.uniform(9999)}"
  end
end
```

**Key concepts**:

- `phx-change="validate"`: Triggers validation on every keystroke
- `Ecto.Changeset`: Validation abstraction (works without database)
- `validate_required/2`, `validate_length/3`: Built-in validators
- `apply_action/2`: Convert changeset to data or return errors
- Form reset after successful submission (UX improvement)

Now try submitting empty message or 201+ char message - validation errors appear in real-time!

## Real-Time Updates with PubSub

Make messages broadcast to all connected users using Phoenix.PubSub:

```elixir
defmodule MyAppWeb.ChatLive do
  use MyAppWeb, :live_view
  alias Phoenix.PubSub

  @topic "chat:lobby"

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      PubSub.subscribe(MyApp.PubSub, @topic)
    end

    socket =
      socket
      |> assign(messages: [], username: generate_username())
      |> assign_form(to_form(%{}, as: :message))

    {:ok, socket}
  end

  @impl true
  def handle_event("send_message", %{"message" => params}, socket) do
    changeset =
      %{text: ""}
      |> cast(params, [:text])
      |> validate_required([:text])
      |> validate_length(:text, max: 200)

    case Changeset.apply_action(changeset, :insert) do
      {:ok, data} ->
        message = %{
          username: socket.assigns.username,
          text: data.text,
          timestamp: DateTime.utc_now()
        }

        # Broadcast to all subscribers
        PubSub.broadcast(MyApp.PubSub, @topic, {:new_message, message})

        socket = assign_form(socket, to_form(%{}, as: :message))
        {:noreply, socket}

      {:error, changeset} ->
        {:noreply, assign_form(socket, changeset)}
    end
  end

  @impl true
  def handle_info({:new_message, message}, socket) do
    {:noreply, update(socket, :messages, fn messages -> messages ++ [message] end)}
  end

  # ... rest of code
end
```

**What changed**:

- `PubSub.subscribe/2`: Subscribe to "chat:lobby" topic (only on WebSocket connect, not initial HTTP)
- `connected?/1`: Check if LiveView is connected via WebSocket (false on initial render)
- `PubSub.broadcast/3`: Send message to all subscribers
- `handle_info/2`: Receive broadcasted messages (Elixir process message passing)

Open two browser windows - messages sent from one appear in both instantly!

## User Presence Tracking

Track who's online using Phoenix.Presence:

First, create Presence module (`lib/my_app_web/presence.ex`):

```elixir
defmodule MyAppWeb.Presence do
  use Phoenix.Presence,
    otp_app: :my_app,
    pubsub_server: MyApp.PubSub
end
```

Add to application supervision tree (`lib/my_app/application.ex`):

```elixir
children = [
  # ... existing children
  MyAppWeb.Presence
]
```

Update ChatLive to track presence:

```elixir
defmodule MyAppWeb.ChatLive do
  use MyAppWeb, :live_view
  alias Phoenix.PubSub
  alias MyAppWeb.Presence

  @topic "chat:lobby"
  @presence_topic "chat:lobby:presence"

  @impl true
  def mount(_params, _session, socket) do
    username = generate_username()

    if connected?(socket) do
      PubSub.subscribe(MyApp.PubSub, @topic)
      PubSub.subscribe(MyApp.PubSub, @presence_topic)

      {:ok, _} = Presence.track(self(), @presence_topic, username, %{
        joined_at: System.system_time(:second)
      })
    end

    socket =
      socket
      |> assign(messages: [], username: username, online_users: [])
      |> assign_form(to_form(%{}, as: :message))
      |> handle_presence_diff(Presence.list(@presence_topic))

    {:ok, socket}
  end

  @impl true
  def handle_info(%{event: "presence_diff"}, socket) do
    {:noreply, handle_presence_diff(socket, Presence.list(@presence_topic))}
  end

  defp handle_presence_diff(socket, presences) do
    online_users =
      presences
      |> Map.keys()
      |> Enum.sort()

    assign(socket, online_users: online_users)
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="chat-container">
      <h1>Chat Room</h1>
      <p>Welcome, <%= @username %>!</p>

      <div class="sidebar">
        <h3>Online (<%= length(@online_users) %>)</h3>
        <%= for user <- @online_users do %>
          <div class={["user", user == @username && "current-user"]}>
            <%= user %>
          </div>
        <% end %>
      </div>

      <div class="messages">
        <%= for message <- @messages do %>
          <div class="message">
            <strong><%= message.username %>:</strong>
            <%= message.text %>
            <small><%= format_time(message.timestamp) %></small>
          </div>
        <% end %>
      </div>

      <.form for={@form} phx-submit="send_message" phx-change="validate">
        <.input field={@form[:text]} placeholder="Type a message..." />
        <.button>Send</.button>

        <.error :if={@form.errors[:text]}>
          <%= translate_error(@form.errors[:text]) %>
        </.error>
      </.form>
    </div>
    """
  end

  # ... rest of code
end
```

**Key concepts**:

- `Presence.track/4`: Track user presence (automatically untracked on disconnect)
- `Presence.list/1`: Get all tracked users
- `handle_info(%{event: "presence_diff"})`: Receive presence updates
- Presence uses CRDTs (Conflict-free Replicated Data Types) for distributed tracking

Open multiple windows - user list updates in real-time as users join/leave!

## Optimizing with Temporary Assigns

Prevent messages from accumulating in LiveView state (memory leak):

```elixir
@impl true
def mount(_params, _session, socket) do
  username = generate_username()

  if connected?(socket) do
    PubSub.subscribe(MyApp.PubSub, @topic)
    PubSub.subscribe(MyApp.PubSub, @presence_topic)

    {:ok, _} = Presence.track(self(), @presence_topic, username, %{
      joined_at: System.system_time(:second)
    })
  end

  socket =
    socket
    |> assign(messages: [], username: username, online_users: [])
    |> assign_form(to_form(%{}, as: :message))
    |> handle_presence_diff(Presence.list(@presence_topic))
    |> stream(:messages, [])  # Use stream instead of assign for messages

  {:ok, socket, temporary_assigns: [messages: []]}
end

@impl true
def handle_info({:new_message, message}, socket) do
  {:noreply, stream_insert(socket, :messages, message)}
end

@impl true
def render(assigns) do
  ~H"""
  <div class="chat-container">
    <!-- ... header and sidebar ... -->

    <div class="messages" id="messages" phx-update="stream">
      <div
        :for={{id, message} <- @streams.messages}
        id={id}
        class="message"
      >
        <strong><%= message.username %>:</strong>
        <%= message.text %>
        <small><%= format_time(message.timestamp) %></small>
      </div>
    </div>

    <!-- ... form ... -->
  </div>
  """
end
```

**Why streams?**:

- `stream/3`: Collections that don't accumulate in memory
- `stream_insert/3`: Add item to stream (sent to client, then discarded server-side)
- `phx-update="stream"`: Tell LiveView DOM to handle streaming updates
- `temporary_assigns`: Reset assigns after rendering (memory efficient)

Now messages don't accumulate in server memory - only stored client-side in DOM!

## Complete Example: Production-Ready Chat

Putting it all together with error handling and polish:

```elixir
defmodule MyAppWeb.ChatLive do
  use MyAppWeb, :live_view
  alias Phoenix.PubSub
  alias MyAppWeb.Presence
  alias Ecto.Changeset

  @topic "chat:lobby"
  @presence_topic "chat:lobby:presence"

  @impl true
  def mount(_params, _session, socket) do
    username = generate_username()

    if connected?(socket) do
      PubSub.subscribe(MyApp.PubSub, @topic)
      PubSub.subscribe(MyApp.PubSub, @presence_topic)

      {:ok, _} = Presence.track(self(), @presence_topic, username, %{
        joined_at: System.system_time(:second)
      })
    end

    socket =
      socket
      |> assign(username: username, online_users: [])
      |> assign_form(to_form(%{}, as: :message))
      |> handle_presence_diff(Presence.list(@presence_topic))
      |> stream(:messages, [])

    {:ok, socket, temporary_assigns: [messages: []]}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="chat-container">
      <header>
        <h1>Chat Room</h1>
        <p>Welcome, <strong><%= @username %></strong>!</p>
      </header>

      <div class="chat-layout">
        <aside class="sidebar">
          <h3>Online Users (<%= length(@online_users) %>)</h3>
          <ul>
            <li
              :for={user <- @online_users}
              class={[user == @username && "current-user"]}
            >
              <%= user %>
            </li>
          </ul>
        </aside>

        <main class="chat-main">
          <div class="messages" id="messages" phx-update="stream">
            <div
              :for={{id, message} <- @streams.messages}
              id={id}
              class="message"
            >
              <div class="message-header">
                <strong><%= message.username %></strong>
                <small><%= format_time(message.timestamp) %></small>
              </div>
              <div class="message-text"><%= message.text %></div>
            </div>
          </div>

          <.form
            for={@form}
            phx-submit="send_message"
            phx-change="validate"
            class="message-form"
          >
            <.input
              field={@form[:text]}
              placeholder="Type a message..."
              autocomplete="off"
              phx-debounce="300"
            />
            <.button>Send</.button>
          </.form>
        </main>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("validate", %{"message" => params}, socket) do
    changeset =
      %{text: ""}
      |> cast(params, [:text])
      |> validate_required([:text], message: "Message cannot be empty")
      |> validate_length(:text, max: 200, message: "Too long (max 200)")

    {:noreply, assign_form(socket, changeset)}
  end

  @impl true
  def handle_event("send_message", %{"message" => params}, socket) do
    changeset =
      %{text: ""}
      |> cast(params, [:text])
      |> validate_required([:text])
      |> validate_length(:text, max: 200)

    case Changeset.apply_action(changeset, :insert) do
      {:ok, data} ->
        message = %{
          id: Ecto.UUID.generate(),
          username: socket.assigns.username,
          text: String.trim(data.text),
          timestamp: DateTime.utc_now()
        }

        PubSub.broadcast(MyApp.PubSub, @topic, {:new_message, message})

        socket = assign_form(socket, to_form(%{}, as: :message))
        {:noreply, socket}

      {:error, changeset} ->
        {:noreply, assign_form(socket, changeset)}
    end
  end

  @impl true
  def handle_info({:new_message, message}, socket) do
    {:noreply, stream_insert(socket, :messages, message)}
  end

  @impl true
  def handle_info(%{event: "presence_diff"}, socket) do
    {:noreply, handle_presence_diff(socket, Presence.list(@presence_topic))}
  end

  defp handle_presence_diff(socket, presences) do
    online_users =
      presences
      |> Map.keys()
      |> Enum.sort()

    assign(socket, online_users: online_users)
  end

  defp assign_form(socket, changeset_or_form) do
    assign(socket, :form, to_form(changeset_or_form))
  end

  defp cast(data, params, allowed) do
    {data, %{text: :string}}
    |> Changeset.cast(params, allowed)
  end

  defp validate_required(changeset, fields, opts \\ []) do
    Changeset.validate_required(changeset, fields, opts)
  end

  defp validate_length(changeset, field, opts) do
    Changeset.validate_length(changeset, field, opts)
  end

  defp format_time(datetime) do
    Calendar.strftime(datetime, "%H:%M:%S")
  end

  defp generate_username do
    "User#{:rand.uniform(9999)}"
  end
end
```

## What to Try Next

Extend your chat application:

1. **Persist messages** - Save to database with Ecto
2. **Add reactions** - Emoji reactions to messages
3. **Typing indicators** - Show "User is typing..."
4. **Private rooms** - Multiple chat rooms
5. **Message editing** - Edit/delete your own messages
6. **File uploads** - Share images/files
7. **User authentication** - Replace random usernames with real accounts

## Common Gotchas

### 1. Mount runs twice

**Problem**: mount/3 runs on initial HTTP render AND WebSocket connect

**Solution**: Use `connected?/1` to detect WebSocket connection:

```elixir
if connected?(socket) do
  # Only run on WebSocket connect
  PubSub.subscribe(...)
end
```

### 2. Memory leak from assigns

**Problem**: Large lists in assigns accumulate in memory

**Solution**: Use streams for collections:

```elixir
{:ok, socket, temporary_assigns: [messages: []]}
stream(:messages, [])
stream_insert(socket, :messages, item)
```

### 3. Form not resetting after submit

**Problem**: Form keeps old value after submission

**Solution**: Reset form in handle_event:

```elixir
socket = assign_form(socket, to_form(%{}, as: :message))
```

### 4. PubSub messages not received

**Problem**: Forgot to subscribe to topic

**Solution**: Subscribe in mount/3 when connected:

```elixir
if connected?(socket) do
  PubSub.subscribe(MyApp.PubSub, @topic)
end
```

## Best Practices Summary

1. **Use connected?/1** - Differentiate initial render from WebSocket connect
2. **Temporary assigns** - Prevent memory leaks with large collections
3. **Streams for lists** - Efficient DOM updates for dynamic content
4. **Debounce validation** - phx-debounce="300" reduces server load
5. **Reset forms** - Clear form after successful submission
6. **Unique IDs for streams** - Use UUIDs or database IDs for stream items
7. **Subscribe in mount** - Set up PubSub subscriptions when connected

## Next Steps

Now that you understand LiveView basics:

1. **[By Example](/en/learn/software-engineering/platform-web/tools/elixir-phoenix-liveview/by-example)** - 85 annotated examples covering 95% of LiveView
2. **Practice with your project** - Apply LiveView to your actual application
3. **Official Documentation** - Advanced features, components, hooks, testing

**Recommended learning path**: Quick Start → practice on real projects → By Example for comprehensive reference.

## Summary

You've learned:

- ✅ LiveView lifecycle (mount, render, handle_event, handle_info)
- ✅ State management (assigns, socket updates)
- ✅ Forms and validation (changesets, live validation)
- ✅ Real-time updates (PubSub, broadcast)
- ✅ User presence (Phoenix.Presence, tracking)
- ✅ Performance optimization (streams, temporary assigns)
- ✅ Best practices (connected check, debouncing, memory management)

**Coverage**: 5-30% of LiveView features - practical foundation for real-world applications.

**Next**: Explore [By Example](/en/learn/software-engineering/platform-web/tools/elixir-phoenix-liveview/by-example) for comprehensive 95% coverage through 85 annotated examples.
