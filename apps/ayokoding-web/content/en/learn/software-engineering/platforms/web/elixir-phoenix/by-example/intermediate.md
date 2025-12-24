---
title: "Intermediate"
weight: 11000002
date: 2025-12-24T00:00:00+07:00
draft: false
description: Master production patterns in Elixir Phoenix through 20 annotated examples covering advanced LiveView, real-time features, authentication, and testing
tags:
  - phoenix
  - elixir
  - web-framework
  - tutorial
  - by-example
  - intermediate
  - liveview
  - channels
  - authentication
  - testing
---

## Group 5: Advanced LiveView

### Example 21: LiveView Streams

Streams efficiently update lists without re-rendering the entire collection. Each stream item gets a unique ID for targeted updates.

```elixir
defmodule MyAppWeb.PostsLive.Index do
  use Phoenix.LiveView

  def mount(_params, _session, socket) do
    {:ok,
     socket
     |> assign(:posts, [])
     |> stream(:posts, MyApp.Blog.list_posts())}  # => Initialize stream
  end

  def render(assigns) do
    ~H"""
    <div id="posts" phx-update="stream">
      <!-- Streams each item with unique ID -->
      <%= for {dom_id, post} <- @streams.posts do %>
        <article id={dom_id}>
          <h2><%= post.title %></h2>
        </article>
      <% end %>
    </div>
    """
  end

  def handle_info({:post_created, post}, socket) do
    {:noreply, stream_insert(socket, :posts, post)}  # => Add post
  end

  def handle_info({:post_deleted, post_id}, socket) do
    {:noreply, stream_delete_by_dom_id(socket, :posts, "posts-#{post_id}")}  # => Remove post
  end
end

# Subscribe to real-time updates
def mount(_params, _session, socket) do
  MyAppWeb.Endpoint.subscribe("posts")
  {:ok, stream(socket, :posts, MyApp.Blog.list_posts())}
end
```

**Key Takeaway**: Streams use phx-update="stream" for efficient list rendering. stream_insert/3 adds items. stream_delete_by_dom_id/3 removes items. Only modified items are updated on the page.

### Example 22: Async Operations with Loading States

Load data asynchronously without blocking the page. Show loading states while waiting.

```elixir
defmodule MyAppWeb.DataLive do
  use Phoenix.LiveView

  def mount(_params, _session, socket) do
    {:ok,
     socket
     |> assign(:result, nil, :loading)
     |> assign_async(:data, fn -> {:ok, %{data: fetch_expensive_data()}} end)}
  end

  def render(assigns) do
    ~H"""
    <div>
      <%= case @data do %>
        <% {:loading} -> %>
          <p>Loading...</p>

        <% {:ok, %{data: data}} -> %>
          <p><%= data %></p>

        <% {:error, reason} -> %>
          <p>Error: <%= reason %></p>
      <% end %>
    </div>
    """
  end

  defp fetch_expensive_data do
    Process.sleep(2000)  # => Simulate slow operation
    "Data loaded!"
  end
end

# Or using start_async for event-triggered async work
def handle_event("search", %{"query" => query}, socket) do
  {:noreply,
   start_async(socket, :search, fn ->
     results = MyApp.Search.query(query)
     {:ok, results}
   end)}
end

def handle_async(:search, {:ok, results}, socket) do
  {:noreply, assign(socket, :results, results)}
end
```

**Key Takeaway**: assign_async/3 loads data when component mounts. start_async/3 performs async work on demand. Render different content based on async state (:loading, :ok, :error).

### Example 23: LiveView File Uploads with External Storage

Upload files to external services like Amazon S3 instead of storing locally.

```elixir
defmodule MyAppWeb.ProfileLive.Edit do
  use Phoenix.LiveView

  def mount(_params, _session, socket) do
    {:ok,
     socket
     |> assign(:uploaded_files, [])
     |> allow_upload(:photo,
       accept: ~w(.jpg .jpeg .png),
       max_entries: 5,
       max_file_size: 10_000_000)}
  end

  def render(assigns) do
    ~H"""
    <form phx-submit="save">
      <.live_file_input upload={@uploads.photo} />

      <%= for entry <- @uploads.photo.entries do %>
        <div>
          <.live_img_preview entry={entry} />
          <button phx-click="cancel_upload" phx-value-ref={entry.ref}>Cancel</button>
          <progress value={entry.progress} max="100" />
        </div>
      <% end %>

      <button type="submit">Save</button>
    </form>
    """
  end

  def handle_event("cancel_upload", %{"ref" => ref}, socket) do
    {:noreply, cancel_upload(socket, :photo, ref)}
  end

  def handle_event("save", _params, socket) do
    uploaded_files =
      consume_uploaded_entries(socket, :photo, fn %{path: path}, entry ->
        # Generate S3 pre-signed URL for client upload
        key = "uploads/#{entry.client_name}"
        url = MyApp.S3.get_presigned_url(key)

        # Upload file directly from client to S3
        {:ok, _} = MyApp.S3.put_object(key, File.read!(path))

        {:ok, %{name: entry.client_name, url: url}}
      end)

    {:noreply, assign(socket, :uploaded_files, uploaded_files)}
  end
end
```

**Key Takeaway**: allow_upload/2 restricts file types and sizes client-side. consume_uploaded_entries/3 processes files after submission. Upload to S3 or other external storage instead of local filesystem.

### Example 24: Stateful Live Components

Live components manage isolated state. Events target only that component, not the parent.

```elixir
defmodule MyAppWeb.ShoppingCart do
  use Phoenix.LiveComponent

  @impl true
  def mount(socket) do
    {:ok, assign(socket, items: [], total: 0)}
  end

  @impl true
  def handle_event("add_item", %{"product_id" => id}, socket) do
    product = MyApp.Catalog.get_product!(id)
    items = [product | socket.assigns.items]
    total = socket.assigns.total + product.price

    {:noreply, assign(socket, items: items, total: total)}
  end

  @impl true
  def handle_event("remove_item", %{"product_id" => id}, socket) do
    product = MyApp.Catalog.get_product!(id)
    items = Enum.reject(socket.assigns.items, &(&1.id == id))
    total = socket.assigns.total - product.price

    {:noreply, assign(socket, items: items, total: total)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id="cart">
      <h2>Cart (<%= length(@items) %> items)</h2>
      <ul>
        <%= for item <- @items do %>
          <li>
            <%= item.name %> - $<%= item.price %>
            <button phx-click="remove_item" phx-value-product_id={item.id} phx-target={@myself}>
              Remove
            </button>
          </li>
        <% end %>
      </ul>
      <p>Total: $<%= @total %></p>
    </div>
    """
  end
end

# Parent uses live_component to render
def parent_render(assigns) do
  ~H"""
  <%= live_component(MyAppWeb.ShoppingCart, id: "cart") %>
  """
end
```

**Key Takeaway**: Live components have their own state via assign/2. Events target component using phx-target={@myself}. Each component instance maintains separate state.

### Example 25: LiveView JS Interop with Phoenix.LiveView.JS

Trigger JavaScript from LiveView without custom JS. Use Phoenix.LiveView.JS for common patterns.

```elixir
defmodule MyAppWeb.ModalLive do
  use Phoenix.LiveView

  def render(assigns) do
    ~H"""
    <div>
      <button phx-click={JS.show(to: "#modal")}>Open Modal</button>

      <div id="modal" class="modal" style="display: none;">
        <h2>Modal Title</h2>

        <button phx-click={
          JS.hide(to: "#modal")
          |> JS.push("close_modal")
        }>
          Close
        </button>
      </div>
    </div>
    """
  end

  def handle_event("close_modal", _params, socket) do
    {:noreply, socket}
  end
end

# Chaining multiple JS commands
action = JS.push("validate")
  |> JS.show(to: "#spinner")
  |> JS.add_class("error", to: "#field")
  |> JS.transition({"fade-in", "duration-500"}, time: 500)

<button phx-click={action}>Submit</button>
```

**Key Takeaway**: JS.show/2, JS.hide/2, JS.add_class/2, JS.remove_class/2 manipulate DOM. Chain multiple commands together. Use JS.push/1 to send server event alongside DOM changes.

### Example 26: Optimistic UI Updates with Rollback

Update the UI immediately (optimistic), then rollback if the server operation fails.

```elixir
defmodule MyAppWeb.PostLive do
  use Phoenix.LiveView

  def render(assigns) do
    ~H"""
    <article>
      <h2><%= @post.title %></h2>
      <p>Likes: <%= @post.likes %></p>

      <!-- Optimistically increment, rollback on error -->
      <button phx-click="like" phx-value-post_id={@post.id}>
        Like
      </button>
    </article>
    """
  end

  def handle_event("like", %{"post_id" => id}, socket) do
    post = socket.assigns.post

    # Update UI immediately
    updated_post = %{post | likes: post.likes + 1}
    socket = assign(socket, :post, updated_post)

    case MyApp.Blog.increment_likes(post) do
      {:ok, _} ->
        # Server confirmed - keep the change
        {:noreply, socket}

      {:error, _reason} ->
        # Server failed - rollback to original
        {:noreply, assign(socket, :post, post)}
    end
  end
end
```

**Key Takeaway**: Update assigns immediately for fast UI response. If server operation fails, restore original values. Users see instant feedback without waiting for server confirmation.

## Group 6: Real-Time Features

### Example 27: Phoenix Channels - Basic Communication

Channels provide real-time bidirectional WebSocket communication for features like chat and notifications.

```elixir
# lib/my_app_web/channels/room_channel.ex
defmodule MyAppWeb.RoomChannel do
  use Phoenix.Channel

  @impl true
  def join("room:" <> room_id, _message, socket) do
    {:ok, assign(socket, :room_id, room_id)}  # => User joined successfully
  end

  @impl true
  def handle_in("new_message", %{"body" => body}, socket) do
    # Broadcast to all users in the room
    broadcast(socket, "message", %{body: body, user_id: socket.assigns.user_id})
    {:noreply, socket}
  end

  @impl true
  def handle_in("typing", _params, socket) do
    broadcast_from(socket, "user_typing", %{user_id: socket.assigns.user_id})
    {:noreply, socket}
  end
end

# Client-side subscription (in JavaScript or LiveView)
<script>
let channel = socket.channel("room:123", {})

channel.join()
  .receive("ok", resp => console.log("Joined", resp))

channel.on("message", payload => {
  console.log("Message:", payload.body)
})

document.getElementById("send").addEventListener("click", () => {
  channel.push("new_message", {body: input.value})
})
</script>
```

**Key Takeaway**: Channels are named "topic:subtopic". join/3 handles subscription. broadcast/3 sends to all users. handle_in/3 processes incoming messages. Perfect for real-time collaboration.

### Example 28: PubSub for LiveView Updates

PubSub delivers messages across the system. LiveView components subscribe to topics and update when messages arrive.

```elixir
defmodule MyAppWeb.PostsLive.List do
  use Phoenix.LiveView

  def mount(_params, _session, socket) do
    # Subscribe to "posts" topic
    MyAppWeb.Endpoint.subscribe("posts")

    posts = MyApp.Blog.list_posts()
    {:ok, stream(socket, :posts, posts)}
  end

  def render(assigns) do
    ~H"""
    <div id="posts" phx-update="stream">
      <%= for {dom_id, post} <- @streams.posts do %>
        <article id={dom_id}>
          <h2><%= post.title %></h2>
        </article>
      <% end %>
    </div>
    """
  end

  @impl true
  def handle_info({:post_created, post}, socket) do
    {:noreply, stream_insert(socket, :posts, post, at: 0)}
  end

  @impl true
  def handle_info({:post_updated, post}, socket) do
    {:noreply, stream_insert(socket, :posts, post)}
  end

  @impl true
  def handle_info({:post_deleted, post_id}, socket) do
    {:noreply, stream_delete_by_dom_id(socket, :posts, "posts-#{post_id}")}
  end
end

# When a post is created (in your context)
defmodule MyApp.Blog do
  def create_post(attrs) do
    case Repo.insert(changeset) do
      {:ok, post} ->
        MyAppWeb.Endpoint.broadcast("posts", "post_created", post)
        {:ok, post}

      {:error, changeset} ->
        {:error, changeset}
    end
  end
end
```

**Key Takeaway**: Endpoint.subscribe/1 listens to topic. Endpoint.broadcast/3 publishes messages. handle_info/2 receives broadcasts. Multiple LiveView instances stay synchronized.

### Example 29: Presence Tracking

Track which users are online and what they're doing. Presence automatically cleans up when users disconnect.

```elixir
defmodule MyAppWeb.Presence do
  use Phoenix.Presence,
    otp_app: :my_app,
    pubsub_server: MyApp.PubSub
end

# Track user presence
defmodule MyAppWeb.RoomChannel do
  use Phoenix.Channel
  alias MyAppWeb.Presence

  def join("room:" <> room_id, _params, socket) do
    send(self(), :after_join)
    {:ok, assign(socket, :room_id, room_id)}
  end

  def handle_info(:after_join, socket) do
    {:ok, _} =
      Presence.track(socket, "user:#{socket.assigns.user_id}", %{
        user_id: socket.assigns.user_id,
        username: socket.assigns.username,
        status: "online"
      })

    push(socket, "presence_state", Presence.list(socket))
    {:noreply, socket}
  end
end

# Get list of online users
onlineUsers = Presence.list("room:123")
# => %{
#   "user:1" => %{metas: [%{user_id: 1, username: "Alice", status: "online"}]},
#   "user:2" => %{metas: [%{user_id: 2, username: "Bob", status: "online"}]}
# }
```

**Key Takeaway**: Presence.track/3 records user state. Presence.list/1 gets all users in topic. Automatically removes user when connection closes. Great for "who's online" features.

### Example 30: Channel Authentication

Secure channels with authentication tokens. Only authenticated users can join sensitive channels.

```elixir
defmodule MyAppWeb.UserSocket do
  use Phoenix.Socket

  channel "room:*", MyAppWeb.RoomChannel
  channel "private:*", MyAppWeb.PrivateChannel

  @impl true
  def connect(%{"token" => token}, socket) do
    case verify_token(token) do
      {:ok, user_id} ->
        {:ok, assign(socket, user_id: user_id)}

      {:error, _reason} ->
        :error  # => Reject connection
    end
  end

  @impl true
  def id(socket), do: "user_socket:#{socket.assigns.user_id}"

  defp verify_token(token) do
    Phoenix.Token.verify(socket, "user socket", token, max_age: 86400)
  end
end

# Generate token in your controller
def login(conn, %{"email" => email, "password" => password}) do
  case MyApp.Accounts.authenticate(email, password) do
    {:ok, user} ->
      token = Phoenix.Token.sign(conn, "user socket", user.id)

      conn
      |> put_session(:user_token, token)
      |> render("login_success.html", user_token: token)

    {:error, _} ->
      render(conn, "login_error.html")
  end
end
```

**Key Takeaway**: connect/2 authenticates socket connection using tokens. assign/2 stores user info. id/1 generates socket ID for tracking. Return :error to reject connection.

### Example 31: Channel Testing

Test channel behavior with ChannelCase. Assert messages, errors, and state changes.

```elixir
defmodule MyAppWeb.RoomChannelTest do
  use MyAppWeb.ChannelCase

  setup do
    {:ok, _, socket} = subscribe_and_join(MyAppWeb.UserSocket, "room:123", %{})
    {:ok, socket: socket}
  end

  test "broadcast new_message", %{socket: socket} do
    push(socket, "new_message", %{"body" => "Hello"})
    assert_broadcast("message", %{body: "Hello"})
  end

  test "handles join message", %{socket: socket} do
    assert_push("presence_state", _)
  end

  test "rejects unauthorized join" do
    assert {:error, _} = subscribe_and_join(MyAppWeb.UserSocket, "private:secret", %{})
  end

  test "handle_in updates state", %{socket: socket} do
    push(socket, "typing", %{})
    assert_broadcast("user_typing", _)
  end
end
```

**Key Takeaway**: ChannelCase provides testing utilities. subscribe_and_join/3 joins a channel. push/2 sends messages. assert_broadcast/2 verifies messages sent. assert_push/2 verifies server pushes.

## Group 7: Authentication & Authorization

### Example 32: Session-Based Authentication

Store user info in encrypted session after login. Session persists across requests.

```elixir
defmodule MyAppWeb.SessionController do
  use MyAppWeb, :controller
  alias MyApp.Accounts

  def new(conn, _params) do
    render(conn, "new.html")
  end

  def create(conn, %{"session" => %{"email" => email, "password" => password}}) do
    case Accounts.authenticate(email, password) do
      {:ok, user} ->
        conn
        |> put_session(:user_id, user.id)  # => Store in session
        |> put_flash(:info, "Welcome back!")
        |> redirect(to: ~p"/dashboard")

      {:error, _} ->
        conn
        |> put_flash(:error, "Invalid credentials")
        |> render("new.html")
    end
  end

  def delete(conn, _params) do
    conn
    |> delete_session(:user_id)  # => Clear session
    |> put_flash(:info, "Logged out")
    |> redirect(to: ~p"/")
  end
end

# Get current user from session
defmodule MyAppWeb.Plugs.SetCurrentUser do
  def init(opts), do: opts

  def call(conn, _opts) do
    user_id = get_session(conn, :user_id)
    user = if user_id, do: MyApp.Accounts.get_user!(user_id)
    assign(conn, :current_user, user)
  end
end
```

**Key Takeaway**: put_session/3 stores data encrypted. get_session/2 retrieves data. delete_session/2 clears it. Sessions survive across requests but are specific to each browser.

### Example 33: Password Hashing and Reset

Securely hash passwords before storing. Implement password reset with time-limited tokens.

```elixir
defmodule MyApp.Accounts.User do
  schema "users" do
    field :email, :string
    field :password, :string, virtual: true  # => Not stored
    field :password_hash, :string            # => Stored in DB
    field :password_reset_token, :string
    field :password_reset_at, :utc_datetime
    timestamps()
  end

  def registration_changeset(user, attrs) do
    user
    |> cast(attrs, [:email, :password])
    |> validate_required([:email, :password])
    |> unique_constraint(:email)
    |> put_password_hash()  # => Hash password
  end

  defp put_password_hash(changeset) do
    case changeset do
      %Ecto.Changeset{valid?: true, changes: %{password: password}} ->
        put_change(changeset, :password_hash, Bcrypt.hash_pwd_salt(password))

      changeset ->
        changeset
    end
  end

  def password_reset_changeset(user) do
    token = :crypto.strong_rand_bytes(32) |> Base.encode64()

    user
    |> change(%{password_reset_token: token, password_reset_at: DateTime.utc_now()})
  end
end

# Password reset flow
def reset_password_request(conn, %{"email" => email}) do
  case MyApp.Accounts.get_user_by_email(email) do
    user ->
      {:ok, user} = MyApp.Accounts.generate_password_reset_token(user)
      # Send email with reset token
      send_reset_email(user)
      {:ok, conn}

    nil ->
      {:ok, conn}  # => Don't reveal if email exists
  end
end

def reset_password(conn, %{"token" => token, "password" => password}) do
  case MyApp.Accounts.get_user_by_reset_token(token) do
    {:ok, user} ->
      MyApp.Accounts.update_password(user, %{password: password})
      {:ok, conn}

    {:error, :expired} ->
      {:error, "Reset token expired"}
  end
end
```

**Key Takeaway**: Hash passwords with Bcrypt before storing. Use random tokens for password reset. Store token expiration time. Don't reveal if email exists in system.

### Example 34: Role-Based Access Control

Restrict actions based on user roles. Use plugs for authorization checks.

```elixir
defmodule MyAppWeb.Plugs.RequireRole do
  def init(opts) do
    Keyword.fetch!(opts, :role)
  end

  def call(conn, role) do
    if has_role?(conn.assigns.current_user, role) do
      conn
    else
      conn
      |> put_flash(:error, "Not authorized")
      |> redirect(to: ~p"/")
      |> halt()
    end
  end

  defp has_role?(%{role: user_role}, required_role) do
    user_role == required_role or user_role == :admin
  end
end

# In router
scope "/admin", MyAppWeb.Admin do
  pipe_through :browser
  pipe_through :require_login

  pipe MyAppWeb.Plugs.RequireRole, role: :admin  # => Only admins

  resources "/users", UserController
  resources "/settings", SettingController
end

# Or check in controller
def delete(conn, %{"id" => id}) do
  if can_delete?(conn.assigns.current_user, id) do
    # Delete logic
  else
    conn
    |> put_flash(:error, "Not authorized")
    |> redirect(to: ~p"/")
  end
end
```

**Key Takeaway**: Store user role in database (:admin, :moderator, :user). Use plugs to enforce role requirements at route level. Check permissions in controller actions.

### Example 35: JWT Token Authentication for APIs

Use JWT tokens for stateless API authentication. Tokens are signed and verified without server storage.

```elixir
defmodule MyAppWeb.AuthToken do
  @salt "user auth"

  def sign(%{id: user_id, email: email}) do
    Phoenix.Token.sign(MyAppWeb.Endpoint, @salt, %{user_id: user_id, email: email})
  end

  def verify(token) do
    Phoenix.Token.verify(MyAppWeb.Endpoint, @salt, token, max_age: 86400)
  end
end

# Generate token on login
def create(conn, %{"email" => email, "password" => password}) do
  case MyApp.Accounts.authenticate(email, password) do
    {:ok, user} ->
      token = MyAppWeb.AuthToken.sign(user)
      json(conn, %{access_token: token, user: user})

    {:error, _} ->
      conn
      |> put_status(:unauthorized)
      |> json(%{error: "Invalid credentials"})
  end
end

# Plug to verify token in API requests
defmodule MyAppWeb.Plugs.VerifyToken do
  def init(opts), do: opts

  def call(conn, _opts) do
    case get_auth_header(conn) do
      "Bearer " <> token ->
        case MyAppWeb.AuthToken.verify(token) do
          {:ok, claims} ->
            assign(conn, :current_user_id, claims.user_id)

          {:error, _} ->
            conn
            |> put_status(:unauthorized)
            |> json(%{error: "Invalid token"})
            |> halt()
        end

      nil ->
        conn
        |> put_status(:unauthorized)
        |> json(%{error: "Missing token"})
        |> halt()
    end
  end

  defp get_auth_header(conn) do
    case get_req_header(conn, "authorization") do
      [header] -> header
      _ -> nil
    end
  end
end
```

**Key Takeaway**: Phoenix.Token.sign/3 creates signed tokens. verify/2 validates tokens. Tokens are stateless (no server storage needed). Include token in "Authorization: Bearer TOKEN" header.

### Example 36: OAuth2 Social Login

Allow users to sign in with Google, GitHub, etc. using Ueberauth library.

```elixir
# config/config.exs
config :ueberauth, Ueberauth,
  providers: [
    google: {Ueberauth.Strategy.Google, [default_scope: "email profile"]},
    github: {Ueberauth.Strategy.Github, [default_scope: "user email"]}
  ]

config :ueberauth, Ueberauth.Strategy.Google.OAuth,
  client_id: System.get_env("GOOGLE_CLIENT_ID"),
  client_secret: System.get_env("GOOGLE_CLIENT_SECRET")

# Router
scope "/auth", MyAppWeb do
  pipe_through :browser

  get "/:provider", AuthController, :request
  get "/:provider/callback", AuthController, :callback
end

# Controller
defmodule MyAppWeb.AuthController do
  use MyAppWeb, :controller
  alias MyApp.Accounts

  def request(conn, _params) do
    render(conn, "request.html", callback_url: Routes.auth_url(conn, :callback, :google))
  end

  def callback(%{assigns: %{ueberauth_auth: auth}} = conn, _params) do
    case Accounts.find_or_create_user(auth) do
      {:ok, user} ->
        conn
        |> put_session(:user_id, user.id)
        |> redirect(to: ~p"/dashboard")

      {:error, _} ->
        conn
        |> put_flash(:error, "OAuth login failed")
        |> redirect(to: ~p"/")
    end
  end

  def callback(conn, _params) do
    conn
    |> put_flash(:error, "OAuth failed")
    |> redirect(to: ~p"/")
  end
end

# Find or create user from OAuth info
def find_or_create_user(auth) do
  case get_user_by_provider(auth.provider, auth.uid) do
    user = %User{} ->
      {:ok, user}

    nil ->
      %User{}
      |> User.oauth_changeset(auth)
      |> Repo.insert()
  end
end
```

**Key Takeaway**: Ueberauth handles OAuth flow. Redirect to "/auth/google" to start login. Callback returns user info. Store provider and UID to link OAuth account.

## Group 8: Testing & Quality

### Example 37: Controller Testing with ConnCase

Test controller actions, responses, status codes, and flash messages.

```elixir
defmodule MyAppWeb.PostControllerTest do
  use MyAppWeb.ConnCase

  describe "GET /posts" do
    test "lists all posts", %{conn: conn} do
      post = insert(:post)

      response = get(conn, ~p"/posts") |> html_response(200)
      assert response =~ post.title
    end
  end

  describe "POST /posts" do
    test "creates post and redirects", %{conn: conn} do
      post_params = %{title: "Hello", body: "World"}

      conn = post(conn, ~p"/posts", post: post_params)

      assert redirected_to(conn) == ~p"/posts/1"
      assert has_flash?(conn, :info, "Post created!")
    end

    test "renders errors on invalid params", %{conn: conn} do
      conn = post(conn, ~p"/posts", post: %{title: ""})

      assert html_response(conn, 200) =~ "can't be blank"
    end
  end

  describe "authenticated routes" do
    setup %{conn: conn} do
      user = insert(:user)
      conn = conn |> assign(:current_user, user)
      {:ok, conn: conn}
    end

    test "requires authentication", %{conn: conn} do
      conn = delete(conn, ~p"/logout")
      assert redirected_to(conn) == ~p"/"
    end
  end
end
```

**Key Takeaway**: Use get/3, post/3, put/3, delete/3 to make requests. html_response/2 checks status and returns HTML. assert redirected_to/1 verifies redirects. Use fixtures or factories for test data.

### Example 38: LiveView Component Testing

Test LiveView mount, render, and event handling.

```elixir
defmodule MyAppWeb.CounterLiveTest do
  use MyAppWeb.ConnCase
  import Phoenix.LiveViewTest

  test "mount and render counter", %{conn: conn} do
    {:ok, _live, html} = live(conn, "/counter")
    assert html =~ "Count: 0"
  end

  test "increment event", %{conn: conn} do
    {:ok, live, _html} = live(conn, "/counter")

    assert live
           |> element("button", "+")
           |> render_click() =~ "Count: 1"
  end

  test "decrement multiple times", %{conn: conn} do
    {:ok, live, _html} = live(conn, "/counter")

    assert live
           |> element("button", "-")
           |> render_click()
           |> render() =~ "Count: -1"
  end

  test "form submission", %{conn: conn} do
    {:ok, live, _html} = live(conn, "/posts/new")

    assert live
           |> form("form", post: %{title: "Test", body: "Content"})
           |> render_submit()

    assert has_element?(live, "h1", "Test")
  end
end
```

**Key Takeaway**: live/2 mounts LiveView component. render_click/1 triggers events. render/1 returns rendered HTML. form/3 submits form. has_element?/3 asserts DOM content exists.

### Example 39: Test Fixtures with ExMachina

Use factories to generate consistent test data without repetition.

```elixir
# test/support/factory.ex
defmodule MyApp.Factory do
  use ExMachina.Ecto, repo: MyApp.Repo

  def user_factory do
    %MyApp.Accounts.User{
      email: sequence(:email, &"user#{&1}@example.com"),
      password: "password123",
      password_hash: Bcrypt.hash_pwd_salt("password123")
    }
  end

  def post_factory do
    %MyApp.Blog.Post{
      title: "Test Post",
      body: "Test body",
      user: build(:user)
    }
  end

  def comment_factory do
    %MyApp.Blog.Comment{
      body: "Great post!",
      post: build(:post),
      user: build(:user)
    }
  end
end

# In test
defmodule MyAppWeb.PostControllerTest do
  use MyAppWeb.ConnCase

  setup do
    user = insert(:user)
    post = insert(:post, user: user)
    {:ok, post: post, user: user}
  end

  test "shows post", %{conn: conn, post: post} do
    conn = get(conn, ~p"/posts/#{post.id}")
    assert html_response(conn, 200) =~ post.title
  end

  test "creates multiple posts", %{conn: conn} do
    insert_list(5, :post)
    posts = MyApp.Repo.all(Post)
    assert length(posts) == 5
  end
end
```

**Key Takeaway**: Define factories using ExMachina. insert/1 creates in database. insert/2 with attributes overrides defaults. insert_list/2 creates multiple records. Factories reduce boilerplate.

### Example 40: Mocking External Services with Mox

Mock external API calls in tests using Mox library.

```elixir
# lib/my_app/payment_api.ex
defmodule MyApp.PaymentAPI do
  @callback charge(amount: integer, customer_id: string) :: {:ok, map} | {:error, term}
end

defmodule MyApp.PaymentAPI.Stripe do
  @behaviour MyApp.PaymentAPI

  def charge(amount: amount, customer_id: customer_id) do
    # Real Stripe API call
    {:ok, %{id: "ch_123", amount: amount}}
  end
end

# config/test.exs
config :my_app, payment_api: MyApp.PaymentAPI.Mock

# test/support/mocks.ex
Mox.defmock(MyApp.PaymentAPI.Mock, for: MyApp.PaymentAPI)

# In test
defmodule MyAppWeb.OrderControllerTest do
  use MyAppWeb.ConnCase
  import Mox

  setup :verify_on_exit!

  test "processes payment on order", %{conn: conn} do
    # Expect charge to be called with these args
    expect(MyApp.PaymentAPI.Mock, :charge, fn %{amount: 1000} ->
      {:ok, %{id: "ch_123"}}
    end)

    conn = post(conn, ~p"/orders", order: %{amount: 1000})
    assert html_response(conn, 302)  # Redirect on success
  end

  test "handles payment failure", %{conn: conn} do
    # Mock payment failure
    stub(MyApp.PaymentAPI.Mock, :charge, fn _ ->
      {:error, "Card declined"}
    end)

    conn = post(conn, ~p"/orders", order: %{amount: 1000})
    assert html_response(conn, 200) =~ "Payment failed"
  end
end
```

**Key Takeaway**: Mox.defmock/2 creates a mock. expect/3 verifies function was called. stub/2 returns values without verification. Use verify_on_exit!/1 to assert expected calls happened.
