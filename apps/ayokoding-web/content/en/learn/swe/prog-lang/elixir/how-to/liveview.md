---
title: "LiveView Real-Time Interfaces"
date: 2025-12-21T17:20:00+07:00
draft: false
description: "Build interactive real-time web interfaces with Phoenix LiveView using server-side rendering, WebSocket updates, and zero JavaScript."
weight: 1000008
tags:
  ["elixir", "phoenix", "liveview", "real-time", "web-development", "how-to"]
---

# LiveView Real-Time Interfaces

**Want interactive UIs without writing JavaScript?** Phoenix LiveView enables real-time features with server-side rendering over WebSockets.

## Prerequisites

- Phoenix framework installed
- Basic HTML/CSS knowledge
- Understanding of processes
- Completed [Intermediate Tutorial](/en/learn/swe/prog-lang/elixir/tutorials/intermediate)

## Problem

Building interactive web UIs traditionally requires writing JavaScript for client-side interactivity, managing state synchronization between client and server, and handling WebSocket connections manually. You need real-time updates, form validation, and dynamic interfaces without the complexity of a separate frontend framework.

**Challenges:**

- Managing client-server state synchronization
- Handling WebSocket connections and reconnections
- Writing minimal JavaScript for complex interactions
- Implementing real-time features efficiently
- Testing interactive components

## Solution

Use **Phoenix LiveView** for server-rendered interactive components that update in real-time over WebSockets, with automatic state management and minimal JavaScript.

## How It Works

### 1. Basic LiveView Counter

```elixir
defmodule MyAppWeb.CounterLive do
  use MyAppWeb, :live_view

  # Initial state when page loads
  def mount(_params, _session, socket) do
    {:ok, assign(socket, count: 0)}
  end

  # Handle button clicks
  def handle_event("increment", _params, socket) do
    {:noreply, update(socket, :count, &(&1 + 1))}
  end

  def handle_event("decrement", _params, socket) do
    {:noreply, update(socket, :count, &(&1 - 1))}
  end

  def handle_event("reset", _params, socket) do
    {:noreply, assign(socket, count: 0)}
  end

  # HTML template
  def render(assigns) do
    ~H"""
    <div class="counter">
      <h1>Count: <%= @count %></h1>
      <button phx-click="increment">+</button>
      <button phx-click="decrement">-</button>
      <button phx-click="reset">Reset</button>
    </div>
    """
  end
end
```

Route configuration:

```elixir
# lib/my_app_web/router.ex
scope "/", MyAppWeb do
  pipe_through :browser

  live "/counter", CounterLive
end
```

### 2. Form Handling with Changesets

```elixir
defmodule MyAppWeb.UserLive do
  use MyAppWeb, :live_view

  alias MyApp.Accounts
  alias MyApp.Accounts.User

  def mount(_params, _session, socket) do
    changeset = Accounts.change_user(%User{})
    {:ok, assign(socket, changeset: changeset)}
  end

  # Validate on each keystroke
  def handle_event("validate", %{"user" => user_params}, socket) do
    changeset =
      %User{}
      |> Accounts.change_user(user_params)
      |> Map.put(:action, :validate)

    {:noreply, assign(socket, changeset: changeset)}
  end

  # Submit form
  def handle_event("save", %{"user" => user_params}, socket) do
    case Accounts.create_user(user_params) do
      {:ok, user} ->
        {:noreply,
         socket
         |> put_flash(:info, "User created successfully")
         |> push_navigate(to: ~p"/users/#{user}")}

      {:error, changeset} ->
        {:noreply, assign(socket, changeset: changeset)}
    end
  end

  def render(assigns) do
    ~H"""
    <div>
      <.form for={@changeset} phx-change="validate" phx-submit="save">
        <.input field={@changeset[:name]} label="Name" />
        <.input field={@changeset[:email]} label="Email" type="email" />
        <.input field={@changeset[:age]} label="Age" type="number" />
        <.button>Save User</.button>
      </.form>
    </div>
    """
  end
end
```

### 3. Live Components for Reusability

```elixir
defmodule MyAppWeb.UserFormComponent do
  use MyAppWeb, :live_component

  alias MyApp.Accounts

  def update(%{user: user} = assigns, socket) do
    changeset = Accounts.change_user(user)

    {:ok,
     socket
     |> assign(assigns)
     |> assign(:changeset, changeset)}
  end

  def handle_event("validate", %{"user" => params}, socket) do
    changeset =
      socket.assigns.user
      |> Accounts.change_user(params)
      |> Map.put(:action, :validate)

    {:noreply, assign(socket, changeset: changeset)}
  end

  def handle_event("save", %{"user" => params}, socket) do
    case Accounts.update_user(socket.assigns.user, params) do
      {:ok, user} ->
        send(self(), {:user_updated, user})
        {:noreply, socket}

      {:error, changeset} ->
        {:noreply, assign(socket, changeset: changeset)}
    end
  end

  def render(assigns) do
    ~H"""
    <div>
      <.form for={@changeset} phx-target={@myself} phx-change="validate" phx-submit="save">
        <.input field={@changeset[:name]} label="Name" />
        <.input field={@changeset[:email]} label="Email" />
        <.button>Update</.button>
      </.form>
    </div>
    """
  end
end
```

Usage in parent LiveView:

```elixir
def render(assigns) do
  ~H"""
  <div>
    <.live_component module={MyAppWeb.UserFormComponent} id="user-form" user={@user} />
  </div>
  """
end

def handle_info({:user_updated, user}, socket) do
  {:noreply, assign(socket, user: user)}
end
```

### 4. Real-Time Updates with PubSub

```elixir
defmodule MyAppWeb.ChatLive do
  use MyAppWeb, :live_view

  alias MyApp.Chat

  def mount(_params, _session, socket) do
    if connected?(socket) do
      Phoenix.PubSub.subscribe(MyApp.PubSub, "chat:lobby")
    end

    messages = Chat.list_messages()
    {:ok, assign(socket, messages: messages, message: "")}
  end

  def handle_event("send_message", %{"message" => text}, socket) do
    {:ok, message} = Chat.create_message(%{text: text, user: "User"})

    # Broadcast to all connected clients
    Phoenix.PubSub.broadcast(MyApp.PubSub, "chat:lobby", {:new_message, message})

    {:noreply, assign(socket, message: "")}
  end

  # Receive broadcasts
  def handle_info({:new_message, message}, socket) do
    {:noreply, update(socket, :messages, fn messages -> [message | messages] end)}
  end

  def render(assigns) do
    ~H"""
    <div>
      <div id="messages" phx-update="stream">
        <%= for message <- @messages do %>
          <div><%= message.text %></div>
        <% end %>
      </div>

      <.form for={%{}} phx-submit="send_message">
        <input type="text" name="message" value={@message} />
        <button>Send</button>
      </.form>
    </div>
    """
  end
end
```

### 5. Infinite Scroll with Streams

```elixir
defmodule MyAppWeb.PostsLive do
  use MyAppWeb, :live_view

  def mount(_params, _session, socket) do
    {:ok,
     socket
     |> assign(page: 1, per_page: 20)
     |> stream(:posts, Posts.list_posts(page: 1, per_page: 20))}
  end

  # Load more posts
  def handle_event("load_more", _params, socket) do
    page = socket.assigns.page + 1
    posts = Posts.list_posts(page: page, per_page: socket.assigns.per_page)

    {:noreply,
     socket
     |> assign(page: page)
     |> stream(:posts, posts, at: -1)}
  end

  def render(assigns) do
    ~H"""
    <div id="posts" phx-update="stream">
      <%= for {id, post} <- @streams.posts do %>
        <div id={id}>
          <h2><%= post.title %></h2>
          <p><%= post.body %></p>
        </div>
      <% end %>
    </div>
    <div phx-viewport-bottom="load_more" id="infinite-scroll-marker"></div>
    """
  end
end
```

### 6. File Uploads

```elixir
defmodule MyAppWeb.UploadLive do
  use MyAppWeb, :live_view

  def mount(_params, _session, socket) do
    {:ok,
     socket
     |> assign(:uploaded_files, [])
     |> allow_upload(:avatar, accept: ~w(.jpg .jpeg .png), max_entries: 1)}
  end

  def handle_event("validate", _params, socket) do
    {:noreply, socket}
  end

  def handle_event("save", _params, socket) do
    uploaded_files =
      consume_uploaded_entries(socket, :avatar, fn %{path: path}, entry ->
        dest = Path.join("priv/static/uploads", "#{entry.uuid}.#{ext(entry)}")
        File.cp!(path, dest)
        {:ok, ~p"/uploads/#{Path.basename(dest)}"}
      end)

    {:noreply, update(socket, :uploaded_files, &(&1 ++ uploaded_files))}
  end

  defp ext(entry) do
    [ext | _] = MIME.extensions(entry.client_type)
    ext
  end

  def render(assigns) do
    ~H"""
    <div>
      <.form for={%{}} phx-submit="save" phx-change="validate">
        <.live_file_input upload={@uploads.avatar} />
        <button>Upload</button>
      </.form>

      <%= for entry <- @uploads.avatar.entries do %>
        <div>
          <.live_img_preview entry={entry} />
          <progress value={entry.progress} max="100"><%= entry.progress %>%</progress>
        </div>
      <% end %>

      <%= for path <- @uploaded_files do %>
        <img src={path} />
      <% end %>
    </div>
    """
  end
end
```

### 7. Presence for User Tracking

```elixir
defmodule MyAppWeb.PresenceLive do
  use MyAppWeb, :live_view

  alias MyAppWeb.Presence

  def mount(_params, %{"user_id" => user_id}, socket) do
    topic = "room:lobby"

    if connected?(socket) do
      Phoenix.PubSub.subscribe(MyApp.PubSub, topic)

      {:ok, _} = Presence.track(self(), topic, user_id, %{
        online_at: System.system_time(:second)
      })
    end

    {:ok, assign(socket, users: Presence.list(topic), user_id: user_id)}
  end

  def handle_info(%{event: "presence_diff"}, socket) do
    users = Presence.list("room:lobby")
    {:noreply, assign(socket, users: users)}
  end

  def render(assigns) do
    ~H"""
    <div>
      <h2>Online Users (<%= map_size(@users) %>)</h2>
      <%= for {user_id, _meta} <- @users do %>
        <div><%= user_id %></div>
      <% end %>
    </div>
    """
  end
end
```

### 8. JavaScript Interop with Hooks

```javascript
// assets/js/app.js
let Hooks = {};

Hooks.Scroll = {
  mounted() {
    this.el.scrollIntoView();
  },
  updated() {
    this.el.scrollIntoView();
  },
};

let liveSocket = new LiveSocket("/live", Socket, {
  params: { _csrf_token: csrfToken },
  hooks: Hooks,
});
```

```elixir
def render(assigns) do
  ~H"""
  <div id="messages">
    <%= for message <- @messages do %>
      <div><%= message.text %></div>
    <% end %>
    <div id="scroll-marker" phx-hook="Scroll"></div>
  </div>
  """
end
```

### 9. Modal with LiveView

```elixir
defmodule MyAppWeb.ModalComponent do
  use MyAppWeb, :live_component

  def render(assigns) do
    ~H"""
    <div
      id="modal"
      class="modal"
      phx-remove={hide_modal()}
      phx-mounted={show_modal()}
    >
      <div class="modal-content">
        <%= render_slot(@inner_block) %>
        <button phx-click={hide_modal()}>Close</button>
      </div>
    </div>
    """
  end

  defp show_modal(js \\ %JS{}) do
    js
    |> JS.show(to: "#modal", transition: "fade-in")
  end

  defp hide_modal(js \\ %JS{}) do
    js
    |> JS.hide(to: "#modal", transition: "fade-out")
    |> JS.push("close_modal")
  end
end
```

### 10. Optimistic UI Updates

```elixir
def handle_event("like", %{"id" => id}, socket) do
  # Update UI immediately
  socket = update(socket, :posts, fn posts ->
    Enum.map(posts, fn post ->
      if post.id == id do
        %{post | likes: post.likes + 1, liked: true}
      else
        post
      end
    end)
  end)

  # Save to database asynchronously
  Task.start(fn ->
    Posts.like_post(id)
  end)

  {:noreply, socket}
end
```

## Variations

### Pagination

```elixir
def handle_event("go_to_page", %{"page" => page}, socket) do
  page = String.to_integer(page)
  posts = Posts.list_posts(page: page, per_page: 20)

  {:noreply, assign(socket, posts: posts, page: page)}
end
```

### Search with Debouncing

```elixir
def handle_event("search", %{"query" => query}, socket) do
  # Cancel previous timer
  if socket.assigns[:search_timer] do
    Process.cancel_timer(socket.assigns.search_timer)
  end

  # Set new timer (300ms delay)
  timer = Process.send_after(self(), {:search, query}, 300)

  {:noreply, assign(socket, search_timer: timer)}
end

def handle_info({:search, query}, socket) do
  results = Search.search(query)
  {:noreply, assign(socket, results: results)}
end
```

## Advanced Patterns

### 1. LiveView Lifecycle

```elixir
def mount(params, session, socket) do
  # Runs on both static and live render
end

def handle_params(params, uri, socket) do
  # Runs on navigation (pushes/patches)
end

def handle_event(event, params, socket) do
  # Handles client events
end

def handle_info(msg, socket) do
  # Handles Erlang messages
end

def terminate(reason, socket) do
  # Cleanup when LiveView terminates
end
```

### 2. Authentication

```elixir
def on_mount(:ensure_authenticated, _params, session, socket) do
  case session["user_id"] do
    nil ->
      socket = put_flash(socket, :error, "Please log in")
      {:halt, redirect(socket, to: ~p"/login")}

    user_id ->
      {:cont, assign(socket, current_user_id: user_id)}
  end
end

# In router
live_session :authenticated, on_mount: MyAppWeb.Hooks.EnsureAuthenticated do
  live "/dashboard", DashboardLive
end
```

### 3. Rate Limiting

```elixir
def handle_event("action", _params, socket) do
  case check_rate_limit(socket) do
    :ok ->
      perform_action()
      {:noreply, socket}

    {:error, :rate_limited} ->
      {:noreply, put_flash(socket, :error, "Too many requests")}
  end
end
```

## Use Cases

**Interactive UIs:**

- Real-time dashboards
- Chat applications
- Live notifications
- Collaborative editing

**Forms:**

- Multi-step wizards
- Real-time validation
- Auto-save drafts
- Dynamic form fields

**Data Visualization:**

- Live charts and graphs
- Real-time metrics
- Streaming data displays

## Troubleshooting

### WebSocket Disconnects

```elixir
# Increase timeout
config :phoenix, :live_view,
  signing_salt: "...",
  timeout: 60_000  # 60 seconds
```

### Memory Leaks

```elixir
# Clean up on terminate
def terminate(_reason, socket) do
  Phoenix.PubSub.unsubscribe(MyApp.PubSub, "topic")
  :ok
end
```

### Slow Updates

```elixir
# Use streams for large lists
stream(:items, items)

# Debounce rapid events
handle_event with timers
```

## Best Practices

1. **Use streams for large datasets:** Better performance
2. **Debounce search inputs:** Avoid excessive queries
3. **Clean up subscriptions:** In `terminate/2`
4. **Use components for reusability:** DRY principle
5. **Test with LiveViewTest:** Comprehensive testing
6. **Minimize assigns:** Only what template needs
7. **Use temporary assigns:** For large one-time data
8. **Handle disconnects gracefully:** Show offline state

## Common Pitfalls

1. **Too much state in assigns:** Memory issues
2. **Not using connected?/1:** Static render errors
3. **Forgetting phx-target:** Events go to wrong component
4. **Not cleaning up:** Memory leaks from subscriptions
5. **Over-using JavaScript:** Defeats purpose of LiveView

## Related Resources

- [Phoenix REST API](/en/learn/swe/prog-lang/elixir/how-to/phoenix-rest-api)
- [Testing Guide](/en/learn/swe/prog-lang/elixir/how-to/testing)
- [Ecto Guide](/en/learn/swe/prog-lang/elixir/how-to/ecto)
- [LiveView Documentation](https://hexdocs.pm/phoenix_live_view/)
- [Phoenix PubSub](https://hexdocs.pm/phoenix_pubsub/)
