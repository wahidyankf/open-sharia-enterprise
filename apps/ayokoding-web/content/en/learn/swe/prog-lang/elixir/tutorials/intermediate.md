---
title: Intermediate Elixir
date: 2025-12-21T00:00:00+07:00
draft: false
weight: 1000004
description: Production-grade Elixir with OTP platform, GenServer, Supervisor, Phoenix framework, LiveView, Ecto, and testing strategies
tags:
  [
    "elixir",
    "tutorial",
    "intermediate",
    "otp",
    "genserver",
    "supervisor",
    "phoenix",
    "liveview",
    "ecto",
  ]
---

**Want to build production-ready Elixir applications?** This tutorial covers OTP platform essentials, Phoenix web development, and production patterns needed for real-world Elixir systems.

## Coverage

This tutorial covers **60-85%** of Elixir knowledge - production-grade OTP and web development.

## Prerequisites

- [Beginner Tutorial](/en/learn/swe/prog-lang/elixir/tutorials/beginner) complete
- Strong understanding of processes and message passing
- Familiarity with pattern matching and functional programming
- Comfortable with recursion and immutability

## Learning Outcomes

By the end of this tutorial, you will:

- Build stateful services with GenServer
- Design supervision trees for fault tolerance
- Understand OTP application structure and configuration
- Build web applications with Phoenix framework
- Create real-time interfaces with LiveView
- Work with databases using Ecto
- Implement concurrent patterns with Task and Agent
- Write comprehensive tests for OTP applications
- Deploy Elixir applications to production

---

## Learning Path

```mermaid
%%{ Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161 }%%
graph TD
    A[GenServer ⭐] --> B[Supervisor ⭐]
    B --> C[Application]
    C --> D[Task & Agent]
    D --> E[Phoenix Framework ⭐]
    E --> F[LiveView]
    F --> G[Ecto]
    G --> H[Testing Strategies]
    H --> I[Configuration]
    I --> J[Production Patterns]

    style A fill:#DE8F05,stroke:#000000,stroke-width:3px,color:#000000
    style B fill:#DE8F05,stroke:#000000,stroke-width:3px,color:#000000
    style E fill:#DE8F05,stroke:#000000,stroke-width:3px,color:#000000
```

**Color Palette**: Orange (#DE8F05 - critical sections for production Elixir)

**⭐ Most important sections**: GenServer, Supervisor, and Phoenix - master these for production readiness!

---

## Section 1: GenServer - Building Stateful Services

GenServer (Generic Server) is the foundation of stateful services in Elixir.

### Understanding GenServer

GenServer provides:

- **State management**: Maintain process state across calls
- **Synchronous calls**: Request-response pattern with `call/3`
- **Asynchronous casts**: Fire-and-forget with `cast/2`
- **Info messages**: Handle arbitrary messages with `handle_info/2`
- **Lifecycle hooks**: Initialize, terminate, code change

### Basic GenServer Implementation

Create a simple counter service:

```elixir
defmodule Counter do
  use GenServer

  # Client API

  def start_link(initial_value) do
    GenServer.start_link(__MODULE__, initial_value, name: __MODULE__)
  end

  def increment do
    GenServer.call(__MODULE__, :increment)
  end

  def get_value do
    GenServer.call(__MODULE__, :get_value)
  end

  def reset do
    GenServer.cast(__MODULE__, :reset)
  end

  # Server Callbacks

  @impl true
  def init(initial_value) do
    {:ok, initial_value}
  end

  @impl true
  def handle_call(:increment, _from, state) do
    new_state = state + 1
    {:reply, new_state, new_state}
  end

  @impl true
  def handle_call(:get_value, _from, state) do
    {:reply, state, state}
  end

  @impl true
  def handle_cast(:reset, _state) do
    {:noreply, 0}
  end
end
```

Usage:

```elixir
# Start the server
{:ok, _pid} = Counter.start_link(0)

# Synchronous calls - wait for response
Counter.increment()  # Returns 1
Counter.increment()  # Returns 2
Counter.get_value()  # Returns 2

# Asynchronous cast - don't wait
Counter.reset()
Counter.get_value()  # Returns 0
```

**How It Works**:

- `start_link/1`: Spawns GenServer process, calls `init/1`
- `call/2`: Sends message, blocks until server replies via `handle_call/3`
- `cast/2`: Sends message, returns immediately, server handles via `handle_cast/2`
- State flows through callbacks: `{:reply, response, new_state}` or `{:noreply, new_state}`

### Real-World GenServer: Key-Value Store

```elixir
defmodule KeyValueStore do
  use GenServer

  # Client API

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, %{}, opts)
  end

  def put(server, key, value) do
    GenServer.call(server, {:put, key, value})
  end

  def get(server, key) do
    GenServer.call(server, {:get, key})
  end

  def delete(server, key) do
    GenServer.cast(server, {:delete, key})
  end

  def list_keys(server) do
    GenServer.call(server, :list_keys)
  end

  # Server Callbacks

  @impl true
  def init(_opts) do
    {:ok, %{}}
  end

  @impl true
  def handle_call({:put, key, value}, _from, state) do
    new_state = Map.put(state, key, value)
    {:reply, :ok, new_state}
  end

  @impl true
  def handle_call({:get, key}, _from, state) do
    {:reply, Map.get(state, key), state}
  end

  @impl true
  def handle_call(:list_keys, _from, state) do
    {:reply, Map.keys(state), state}
  end

  @impl true
  def handle_cast({:delete, key}, state) do
    new_state = Map.delete(state, key)
    {:noreply, new_state}
  end
end
```

Usage:

```elixir
{:ok, store} = KeyValueStore.start_link()

KeyValueStore.put(store, :name, "Alice")
KeyValueStore.put(store, :age, 30)

KeyValueStore.get(store, :name)      # "Alice"
KeyValueStore.list_keys(store)       # [:name, :age]

KeyValueStore.delete(store, :age)
KeyValueStore.list_keys(store)       # [:name]
```

### GenServer with Timeouts

Handle long-running operations with timeouts:

```elixir
defmodule Worker do
  use GenServer

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def process_with_timeout(data, timeout \\ 5000) do
    GenServer.call(__MODULE__, {:process, data}, timeout)
  end

  @impl true
  def init(_opts) do
    {:ok, %{}}
  end

  @impl true
  def handle_call({:process, data}, _from, state) do
    # Simulate long processing
    result = expensive_operation(data)
    {:reply, result, state}
  end

  defp expensive_operation(data) do
    # Simulate work
    :timer.sleep(1000)
    String.upcase(data)
  end
end
```

```elixir
Worker.start_link([])

# Default 5 second timeout
Worker.process_with_timeout("hello")  # "HELLO" after 1 second

# Custom timeout
Worker.process_with_timeout("world", 500)  # Might raise timeout error
```

### GenServer Lifecycle Hooks

```elixir
defmodule LifecycleDemo do
  use GenServer
  require Logger

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(opts) do
    Logger.info("GenServer starting with opts: #{inspect(opts)}")
    # Schedule periodic work
    schedule_work()
    {:ok, %{started_at: DateTime.utc_now()}}
  end

  @impl true
  def handle_info(:work, state) do
    Logger.info("Performing periodic work...")
    schedule_work()
    {:noreply, state}
  end

  @impl true
  def terminate(reason, state) do
    Logger.info("GenServer terminating: #{inspect(reason)}")
    Logger.info("Final state: #{inspect(state)}")
    :ok
  end

  defp schedule_work do
    Process.send_after(self(), :work, 10_000)  # Every 10 seconds
  end
end
```

**Best Practices**:

- **Separate client API from callbacks**: Clean interface, hide implementation
- **Use `call` for queries**: When you need a response
- **Use `cast` for commands**: When you don't need confirmation
- **Keep callbacks fast**: Long operations block the GenServer
- **Handle `terminate/2`**: Clean up resources (close files, connections)
- **Use named processes sparingly**: Registry is better for dynamic processes

---

## Section 2: Supervisor - Fault Tolerance

Supervisors monitor processes and restart them when they crash.

### Supervision Strategies

Three restart strategies:

1. **:one_for_one**: Restart only crashed child
2. **:one_for_all**: Restart all children when one crashes
3. **:rest_for_one**: Restart crashed child and all children started after it

### Basic Supervisor

```elixir
defmodule MyApp.Supervisor do
  use Supervisor

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    children = [
      {Counter, 0},
      {KeyValueStore, []}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
```

Start the supervisor:

```elixir
{:ok, _pid} = MyApp.Supervisor.start_link([])
```

Now if `Counter` or `KeyValueStore` crashes, the supervisor restarts it automatically.

### Supervision Tree Example

Build a multi-level supervision tree:

```elixir
defmodule MyApp.Application do
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Database connection pool
      {MyApp.Repo, []},

      # Web endpoint (Phoenix)
      MyAppWeb.Endpoint,

      # Business logic supervisor
      MyApp.BusinessSupervisor,

      # Background job supervisor
      MyApp.JobSupervisor
    ]

    opts = [strategy: :one_for_one, name: MyApp.Supervisor]
    Supervisor.start_link(children, opts)
  end
end

defmodule MyApp.BusinessSupervisor do
  use Supervisor

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    children = [
      {UserCache, []},
      {SessionManager, []},
      {NotificationService, []}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
```

**Supervision Tree Visualization**:

```
Application.Supervisor (:one_for_one)
├── Repo
├── Endpoint
├── BusinessSupervisor (:one_for_one)
│   ├── UserCache
│   ├── SessionManager
│   └── NotificationService
└── JobSupervisor (:one_for_one)
    ├── EmailWorker
    └── ReportWorker
```

### Dynamic Supervisor

Supervise dynamically created processes:

```elixir
defmodule MyApp.WorkerSupervisor do
  use DynamicSupervisor

  def start_link(opts) do
    DynamicSupervisor.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end

  def start_worker(arg) do
    spec = {Worker, arg}
    DynamicSupervisor.start_child(__MODULE__, spec)
  end

  def stop_worker(pid) do
    DynamicSupervisor.terminate_child(__MODULE__, pid)
  end
end
```

Usage:

```elixir
# Start the dynamic supervisor
{:ok, _pid} = MyApp.WorkerSupervisor.start_link([])

# Start workers dynamically
{:ok, worker1} = MyApp.WorkerSupervisor.start_worker("job1")
{:ok, worker2} = MyApp.WorkerSupervisor.start_worker("job2")

# Stop a worker
MyApp.WorkerSupervisor.stop_worker(worker1)
```

### Restart Strategies Comparison

```elixir
defmodule StrategyDemo do
  use Supervisor

  # :one_for_one - independent workers
  def init_one_for_one(_opts) do
    children = [
      {Logger1, []},
      {Logger2, []},
      {Logger3, []}
    ]
    # If Logger2 crashes, only Logger2 restarts
    Supervisor.init(children, strategy: :one_for_one)
  end

  # :one_for_all - interdependent workers
  def init_one_for_all(_opts) do
    children = [
      {Database, []},
      {Cache, []},     # Depends on Database
      {WebServer, []}  # Depends on Cache
    ]
    # If any crashes, all restart (maintain consistency)
    Supervisor.init(children, strategy: :one_for_all)
  end

  # :rest_for_one - dependent chain
  def init_rest_for_one(_opts) do
    children = [
      {ConfigLoader, []},
      {Database, []},      # Depends on ConfigLoader
      {APIClient, []}      # Depends on Database
    ]
    # If Database crashes, Database and APIClient restart
    # If APIClient crashes, only APIClient restarts
    Supervisor.init(children, strategy: :rest_for_one)
  end
end
```

**When to Use Each Strategy**:

- **:one_for_one**: Default choice, independent services
- **:one_for_all**: Tightly coupled services that must stay in sync
- **:rest_for_one**: Services with start-order dependencies

### Restart Intensity and Period

Prevent infinite restart loops:

```elixir
defmodule MyApp.Supervisor do
  use Supervisor

  @impl true
  def init(_opts) do
    children = [
      {FlakeyWorker, []}
    ]

    # Allow 3 restarts within 5 seconds
    # After that, supervisor itself crashes (escalate to parent)
    Supervisor.init(children,
      strategy: :one_for_one,
      max_restarts: 3,
      max_seconds: 5
    )
  end
end
```

**Best Practices**:

- **Design for crashes**: Let it crash, supervisor handles recovery
- **Use :one_for_one by default**: Simplest and most common
- **Limit restart intensity**: Prevent infinite restart loops
- **Organize by failure domain**: Group related processes under supervisor
- **Escalate failures**: If child keeps crashing, let supervisor crash too

---

## Section 3: Application - Project Structure

OTP Application is the standard unit of deployment.

### Application Behavior

Every Mix project can be an application:

```elixir
# mix.exs
defmodule MyApp.MixProject do
  use Mix.Project

  def project do
    [
      app: :my_app,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {MyApp.Application, []}  # Application callback
    ]
  end

  defp deps do
    []
  end
end
```

### Application Callback

```elixir
# lib/my_app/application.ex
defmodule MyApp.Application do
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Processes to supervise
      {MyApp.Repo, []},
      {MyApp.Cache, []},
      MyAppWeb.Endpoint
    ]

    opts = [strategy: :one_for_one, name: MyApp.Supervisor]
    Supervisor.start_link(children, opts)
  end

  @impl true
  def stop(_state) do
    # Cleanup on application stop
    :ok
  end
end
```

### Application Configuration

Configure your application:

```elixir
# config/config.exs
import Config

config :my_app,
  api_key: "dev-key",
  timeout: 5000

# config/prod.exs
import Config

config :my_app,
  api_key: System.get_env("API_KEY"),
  timeout: 30000
```

Access configuration:

```elixir
api_key = Application.get_env(:my_app, :api_key)
timeout = Application.get_env(:my_app, :timeout, 5000)  # Default 5000
```

Runtime configuration (Elixir 1.11+):

```elixir
# config/runtime.exs
import Config

if config_env() == :prod do
  config :my_app,
    api_key: System.fetch_env!("API_KEY"),
    database_url: System.fetch_env!("DATABASE_URL")
end
```

### Application Dependencies

Declare application dependencies:

```elixir
def application do
  [
    extra_applications: [:logger, :crypto, :ssl],
    mod: {MyApp.Application, []}
  ]
end
```

Applications start in dependency order:

1. Logger
2. Crypto
3. SSL
4. MyApp

---

## Section 4: Task and Agent - Concurrent Utilities

Task and Agent provide simpler abstractions for common patterns.

### Task - Concurrent Computation

Execute work concurrently:

```elixir
# Async/await pattern
task = Task.async(fn ->
  :timer.sleep(1000)
  "Result"
end)

# Do other work...
IO.puts("Doing other work...")

# Wait for result (blocks)
result = Task.await(task)
IO.puts(result)  # "Result" after 1 second
```

Multiple concurrent tasks:

```elixir
tasks = Enum.map(1..5, fn i ->
  Task.async(fn ->
    :timer.sleep(1000)
    i * 2
  end)
end)

results = Task.await_many(tasks)
IO.inspect(results)  # [2, 4, 6, 8, 10] after 1 second (not 5)
```

### Supervised Tasks

Run tasks under supervision:

```elixir
defmodule MyApp.Application do
  use Application

  def start(_type, _args) do
    children = [
      {Task.Supervisor, name: MyApp.TaskSupervisor}
    ]

    opts = [strategy: :one_for_one, name: MyApp.Supervisor]
    Supervisor.start_link(children, opts)
  end
end

# Start supervised task
Task.Supervisor.start_child(MyApp.TaskSupervisor, fn ->
  # This task is supervised - if it crashes, supervisor handles it
  perform_work()
end)

# Async supervised task with await
task = Task.Supervisor.async(MyApp.TaskSupervisor, fn ->
  fetch_data()
end)
result = Task.await(task)
```

### Agent - Simple State Container

Agent wraps state in a process:

```elixir
# Start an agent with initial state
{:ok, agent} = Agent.start_link(fn -> %{} end)

# Update state
Agent.update(agent, fn state ->
  Map.put(state, :count, 1)
end)

# Get state
count = Agent.get(agent, fn state ->
  Map.get(state, :count)
end)

IO.puts(count)  # 1

# Get and update atomically
{old_count, new_count} = Agent.get_and_update(agent, fn state ->
  old = Map.get(state, :count, 0)
  new = old + 1
  {old, Map.put(state, :count, new)}
end)
```

Named agent:

```elixir
defmodule Counter do
  def start_link(initial_value) do
    Agent.start_link(fn -> initial_value end, name: __MODULE__)
  end

  def increment do
    Agent.update(__MODULE__, &(&1 + 1))
  end

  def get do
    Agent.get(__MODULE__, & &1)
  end
end

Counter.start_link(0)
Counter.increment()
Counter.increment()
Counter.get()  # 2
```

**Task vs Agent vs GenServer**:

- **Task**: One-off concurrent computation, short-lived
- **Agent**: Simple state storage, minimal logic
- **GenServer**: Complex state + behavior, long-lived

---

## Section 5: Phoenix Framework - Web Development

Phoenix is the leading Elixir web framework.

### Creating a Phoenix Project

```bash
# Install Phoenix
mix archive.install hex phx_new

# Create new project
mix phx.new my_app
cd my_app

# Start server
mix phx.server
```

Visit `http://localhost:4000`

### Phoenix Project Structure

```
my_app/
├── lib/
│   ├── my_app/             # Business logic
│   │   ├── accounts/       # Domain context
│   │   └── repo.ex         # Database repo
│   └── my_app_web/         # Web interface
│       ├── controllers/
│       ├── views/
│       ├── templates/
│       ├── router.ex
│       └── endpoint.ex
├── test/
├── config/
└── mix.exs
```

### Routing

Define routes in `router.ex`:

```elixir
defmodule MyAppWeb.Router do
  use MyAppWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, {MyAppWeb.LayoutView, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", MyAppWeb do
    pipe_through :browser

    get "/", PageController, :index
    get "/about", PageController, :about

    resources "/users", UserController
    resources "/posts", PostController, only: [:index, :show]
  end

  scope "/api", MyAppWeb do
    pipe_through :api

    get "/health", HealthController, :check
    resources "/posts", API.PostController, except: [:new, :edit]
  end
end
```

### Controllers

Handle requests:

```elixir
defmodule MyAppWeb.PageController do
  use MyAppWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end

  def about(conn, _params) do
    render(conn, "about.html", title: "About Us")
  end
end

defmodule MyAppWeb.UserController do
  use MyAppWeb, :controller

  def index(conn, _params) do
    users = Accounts.list_users()
    render(conn, "index.html", users: users)
  end

  def show(conn, %{"id" => id}) do
    user = Accounts.get_user!(id)
    render(conn, "show.html", user: user)
  end

  def create(conn, %{"user" => user_params}) do
    case Accounts.create_user(user_params) do
      {:ok, user} ->
        conn
        |> put_flash(:info, "User created successfully.")
        |> redirect(to: Routes.user_path(conn, :show, user))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "new.html", changeset: changeset)
    end
  end
end
```

### JSON API

```elixir
defmodule MyAppWeb.API.PostController do
  use MyAppWeb, :controller

  def index(conn, _params) do
    posts = Blog.list_posts()
    render(conn, "index.json", posts: posts)
  end

  def show(conn, %{"id" => id}) do
    post = Blog.get_post!(id)
    render(conn, "show.json", post: post)
  end

  def create(conn, %{"post" => post_params}) do
    case Blog.create_post(post_params) do
      {:ok, post} ->
        conn
        |> put_status(:created)
        |> render("show.json", post: post)

      {:error, changeset} ->
        conn
        |> put_status(:unprocessable_entity)
        |> render(MyAppWeb.ChangesetView, "error.json", changeset: changeset)
    end
  end
end
```

View:

```elixir
defmodule MyAppWeb.API.PostView do
  use MyAppWeb, :view

  def render("index.json", %{posts: posts}) do
    %{data: Enum.map(posts, &post_json/1)}
  end

  def render("show.json", %{post: post}) do
    %{data: post_json(post)}
  end

  defp post_json(post) do
    %{
      id: post.id,
      title: post.title,
      body: post.body,
      author: post.author,
      inserted_at: post.inserted_at
    }
  end
end
```

### Plugs - HTTP Pipeline

Plugs are composable modules for transforming requests:

```elixir
defmodule MyAppWeb.Plugs.RequireAuth do
  import Plug.Conn
  import Phoenix.Controller

  def init(opts), do: opts

  def call(conn, _opts) do
    if get_session(conn, :user_id) do
      conn
    else
      conn
      |> put_flash(:error, "You must be logged in")
      |> redirect(to: "/login")
      |> halt()
    end
  end
end
```

Use in router or controller:

```elixir
# In router
scope "/admin", MyAppWeb.Admin do
  pipe_through [:browser, MyAppWeb.Plugs.RequireAuth]

  resources "/posts", PostController
end

# In controller
defmodule MyAppWeb.AdminController do
  use MyAppWeb, :controller

  plug MyAppWeb.Plugs.RequireAuth when action in [:edit, :update, :delete]

  # ...
end
```

---

## Section 6: LiveView - Real-Time Interfaces

LiveView enables real-time, interactive UIs without JavaScript.

### Basic LiveView

```elixir
defmodule MyAppWeb.CounterLive do
  use MyAppWeb, :live_view

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, count: 0)}
  end

  @impl true
  def handle_event("increment", _params, socket) do
    {:noreply, update(socket, :count, &(&1 + 1))}
  end

  @impl true
  def handle_event("decrement", _params, socket) do
    {:noreply, update(socket, :count, &(&1 - 1))}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <h1>Counter: <%= @count %></h1>
      <button phx-click="increment">+</button>
      <button phx-click="decrement">-</button>
    </div>
    """
  end
end
```

Add to router:

```elixir
scope "/", MyAppWeb do
  pipe_through :browser

  live "/counter", CounterLive
end
```

### LiveView Form Handling

```elixir
defmodule MyAppWeb.SearchLive do
  use MyAppWeb, :live_view

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, query: "", results: [])}
  end

  @impl true
  def handle_event("search", %{"query" => query}, socket) do
    results = perform_search(query)
    {:noreply, assign(socket, query: query, results: results)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <form phx-submit="search">
        <input type="text" name="query" value={@query}
               placeholder="Search..." phx-debounce="300" />
        <button type="submit">Search</button>
      </form>

      <ul>
        <%= for result <- @results do %>
          <li><%= result.title %></li>
        <% end %>
      </ul>
    </div>
    """
  end

  defp perform_search(query) do
    # Simulate search
    [
      %{title: "Result 1 for #{query}"},
      %{title: "Result 2 for #{query}"}
    ]
  end
end
```

### LiveView with Streams (Phoenix 1.7+)

Efficiently handle large lists:

```elixir
defmodule MyAppWeb.PostsLive do
  use MyAppWeb, :live_view

  @impl true
  def mount(_params, _session, socket) do
    posts = Blog.list_posts()
    {:ok, stream(socket, :posts, posts)}
  end

  @impl true
  def handle_event("delete", %{"id" => id}, socket) do
    post = Blog.get_post!(id)
    {:ok, _} = Blog.delete_post(post)
    {:noreply, stream_delete(socket, :posts, post)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <h1>Posts</h1>
      <div id="posts" phx-update="stream">
        <%= for {dom_id, post} <- @streams.posts do %>
          <div id={dom_id}>
            <h2><%= post.title %></h2>
            <button phx-click="delete" phx-value-id={post.id}>Delete</button>
          </div>
        <% end %>
      </div>
    </div>
    """
  end
end
```

### LiveView PubSub

Real-time updates across clients:

```elixir
defmodule MyAppWeb.ChatLive do
  use MyAppWeb, :live_view

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      Phoenix.PubSub.subscribe(MyApp.PubSub, "chat:lobby")
    end

    messages = Chat.list_messages()
    {:ok, assign(socket, messages: messages, message: "")}
  end

  @impl true
  def handle_event("send", %{"message" => message}, socket) do
    Chat.create_message(%{body: message, user: "User"})
    Phoenix.PubSub.broadcast(MyApp.PubSub, "chat:lobby", {:new_message, message})
    {:noreply, assign(socket, message: "")}
  end

  @impl true
  def handle_info({:new_message, message}, socket) do
    {:noreply, update(socket, :messages, fn messages ->
      [message | messages]
    end)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <div id="messages">
        <%= for message <- @messages do %>
          <div><%= message %></div>
        <% end %>
      </div>

      <form phx-submit="send">
        <input type="text" name="message" value={@message} />
        <button type="submit">Send</button>
      </form>
    </div>
    """
  end
end
```

---

## Section 7: Ecto - Database Layer

Ecto is Elixir's database wrapper and query generator.

### Defining Schemas

```elixir
defmodule MyApp.Accounts.User do
  use Ecto.Schema
  import Ecto.Changeset

  schema "users" do
    field :name, :string
    field :email, :string
    field :age, :integer
    field :confirmed, :boolean, default: false

    timestamps()
  end

  def changeset(user, attrs) do
    user
    |> cast(attrs, [:name, :email, :age, :confirmed])
    |> validate_required([:name, :email])
    |> validate_format(:email, ~r/@/)
    |> validate_number(:age, greater_than: 0, less_than: 150)
    |> unique_constraint(:email)
  end
end
```

### Migrations

```elixir
# Generated with: mix ecto.gen.migration create_users
defmodule MyApp.Repo.Migrations.CreateUsers do
  use Ecto.Migration

  def change do
    create table(:users) do
      add :name, :string, null: false
      add :email, :string, null: false
      add :age, :integer
      add :confirmed, :boolean, default: false

      timestamps()
    end

    create unique_index(:users, [:email])
  end
end
```

Run migrations:

```bash
mix ecto.migrate
mix ecto.rollback
```

### Querying

```elixir
import Ecto.Query
alias MyApp.Accounts.User
alias MyApp.Repo

# Get all users
users = Repo.all(User)

# Get by ID
user = Repo.get(User, 1)
user = Repo.get!(User, 1)  # Raises if not found

# Get by field
user = Repo.get_by(User, email: "alice@example.com")

# Query with conditions
query = from u in User,
  where: u.age > 18,
  select: u

adults = Repo.all(query)

# Pipe syntax
adults = User
  |> where([u], u.age > 18)
  |> order_by([u], desc: u.inserted_at)
  |> limit(10)
  |> Repo.all()

# Aggregate
count = User
  |> where([u], u.confirmed == true)
  |> Repo.aggregate(:count)

# Update all
{count, _} = User
  |> where([u], u.age < 18)
  |> Repo.update_all(set: [confirmed: false])
```

### Changesets and Validation

```elixir
# Create
changeset = User.changeset(%User{}, %{
  name: "Alice",
  email: "alice@example.com",
  age: 30
})

case Repo.insert(changeset) do
  {:ok, user} ->
    IO.puts("User created: #{user.id}")
  {:error, changeset} ->
    IO.inspect(changeset.errors)
end

# Update
user = Repo.get!(User, 1)
changeset = User.changeset(user, %{age: 31})
Repo.update(changeset)

# Delete
user = Repo.get!(User, 1)
Repo.delete(user)
```

### Associations

```elixir
defmodule MyApp.Blog.Post do
  use Ecto.Schema

  schema "posts" do
    field :title, :string
    field :body, :text
    belongs_to :author, MyApp.Accounts.User

    timestamps()
  end
end

defmodule MyApp.Accounts.User do
  use Ecto.Schema

  schema "users" do
    field :name, :string
    has_many :posts, MyApp.Blog.Post, foreign_key: :author_id

    timestamps()
  end
end
```

Preloading associations:

```elixir
# Lazy load (N+1 query problem)
users = Repo.all(User)
Enum.each(users, fn user ->
  posts = Repo.preload(user, :posts).posts
  IO.inspect(posts)
end)

# Eager load (single query with join)
users = User
  |> preload(:posts)
  |> Repo.all()

# Selective preload
users = User
  |> join(:inner, [u], p in assoc(u, :posts))
  |> where([u, p], p.published == true)
  |> preload([u, p], posts: p)
  |> Repo.all()
```

---

## Section 8: Testing Strategies

Comprehensive testing for OTP applications.

### ExUnit Basics

```elixir
defmodule MyApp.AccountsTest do
  use ExUnit.Case, async: true
  alias MyApp.Accounts

  describe "create_user/1" do
    test "creates user with valid attributes" do
      attrs = %{name: "Alice", email: "alice@example.com", age: 30}
      assert {:ok, user} = Accounts.create_user(attrs)
      assert user.name == "Alice"
      assert user.email == "alice@example.com"
    end

    test "returns error with invalid email" do
      attrs = %{name: "Alice", email: "invalid", age: 30}
      assert {:error, changeset} = Accounts.create_user(attrs)
      assert %{email: ["has invalid format"]} = errors_on(changeset)
    end
  end
end
```

### Testing GenServers

```elixir
defmodule CounterTest do
  use ExUnit.Case

  setup do
    {:ok, pid} = Counter.start_link(0)
    %{counter: pid}
  end

  test "increments counter", %{counter: counter} do
    assert Counter.increment(counter) == 1
    assert Counter.increment(counter) == 2
  end

  test "resets counter", %{counter: counter} do
    Counter.increment(counter)
    Counter.increment(counter)
    Counter.reset(counter)
    assert Counter.get_value(counter) == 0
  end
end
```

### Testing with Mocks

```elixir
# Use Mox for mocking
# mix.exs
defp deps do
  [
    {:mox, "~> 1.0", only: :test}
  ]
end

# Define behaviour
defmodule MyApp.HTTPClient do
  @callback get(url :: String.t()) :: {:ok, map()} | {:error, term()}
end

# Production implementation
defmodule MyApp.HTTPClient.HTTPoison do
  @behaviour MyApp.HTTPClient

  def get(url) do
    case HTTPoison.get(url) do
      {:ok, %{body: body}} -> {:ok, Jason.decode!(body)}
      error -> error
    end
  end
end

# Define mock
# test/support/mocks.ex
Mox.defmock(MyApp.HTTPClient.Mock, for: MyApp.HTTPClient)

# Test
defmodule MyApp.APITest do
  use ExUnit.Case, async: true
  import Mox

  setup :verify_on_exit!

  test "fetches data from API" do
    expect(MyApp.HTTPClient.Mock, :get, fn _url ->
      {:ok, %{"data" => "test"}}
    end)

    assert {:ok, %{"data" => "test"}} = MyApp.API.fetch_data()
  end
end
```

### Testing Phoenix Controllers

```elixir
defmodule MyAppWeb.PageControllerTest do
  use MyAppWeb.ConnCase

  test "GET /", %{conn: conn} do
    conn = get(conn, "/")
    assert html_response(conn, 200) =~ "Welcome"
  end
end

defmodule MyAppWeb.UserControllerTest do
  use MyAppWeb.ConnCase

  describe "create user" do
    test "redirects when data is valid", %{conn: conn} do
      attrs = %{name: "Alice", email: "alice@example.com"}
      conn = post(conn, Routes.user_path(conn, :create), user: attrs)

      assert %{id: id} = redirected_params(conn)
      assert redirected_to(conn) == Routes.user_path(conn, :show, id)
    end

    test "renders errors when data is invalid", %{conn: conn} do
      attrs = %{name: "", email: "invalid"}
      conn = post(conn, Routes.user_path(conn, :create), user: attrs)

      assert html_response(conn, 200) =~ "Oops, something went wrong!"
    end
  end
end
```

### Testing LiveView

```elixir
defmodule MyAppWeb.CounterLiveTest do
  use MyAppWeb.ConnCase
  import Phoenix.LiveViewTest

  test "increments counter", %{conn: conn} do
    {:ok, view, html} = live(conn, "/counter")

    assert html =~ "Counter: 0"

    assert view |> element("button", "+") |> render_click() =~ "Counter: 1"
    assert view |> element("button", "+") |> render_click() =~ "Counter: 2"
  end

  test "decrements counter", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/counter")

    view |> element("button", "+") |> render_click()
    view |> element("button", "+") |> render_click()

    assert view |> element("button", "-") |> render_click() =~ "Counter: 1"
  end
end
```

---

## Section 9: Configuration Management

Manage configuration across environments.

### Compile-Time Configuration

```elixir
# config/config.exs
import Config

config :my_app,
  api_endpoint: "https://api.example.com",
  timeout: 5000,
  retry_attempts: 3

# Environment-specific
import_config "#{config_env()}.exs"
```

```elixir
# config/dev.exs
import Config

config :my_app,
  api_endpoint: "http://localhost:4000",
  timeout: 30000
```

```elixir
# config/prod.exs
import Config

config :my_app,
  timeout: 10000
```

### Runtime Configuration

```elixir
# config/runtime.exs (Elixir 1.11+)
import Config

if config_env() == :prod do
  config :my_app,
    api_key: System.fetch_env!("API_KEY"),
    database_url: System.fetch_env!("DATABASE_URL"),
    secret_key_base: System.fetch_env!("SECRET_KEY_BASE")
end
```

### Accessing Configuration

```elixir
defmodule MyApp.API do
  @api_endpoint Application.compile_env(:my_app, :api_endpoint)
  @timeout Application.compile_env(:my_app, :timeout, 5000)

  def fetch_data do
    # Use compile-time config
    HTTPoison.get(@api_endpoint, [], recv_timeout: @timeout)
  end

  def fetch_with_runtime_key do
    # Use runtime config
    api_key = Application.get_env(:my_app, :api_key)
    HTTPoison.get("#{@api_endpoint}?key=#{api_key}")
  end
end
```

---

## Section 10: Production Patterns

Patterns for production-ready applications.

### Graceful Degradation

```elixir
defmodule MyApp.ResilientAPI do
  require Logger

  def fetch_data(url, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 5000)
    retries = Keyword.get(opts, :retries, 3)

    fetch_with_retry(url, timeout, retries)
  end

  defp fetch_with_retry(url, timeout, retries) when retries > 0 do
    case HTTPoison.get(url, [], recv_timeout: timeout) do
      {:ok, %{status_code: 200, body: body}} ->
        {:ok, body}

      {:ok, %{status_code: status}} ->
        Logger.warn("API returned #{status} for #{url}")
        {:error, :bad_status}

      {:error, reason} ->
        Logger.warn("API request failed: #{inspect(reason)}, retries left: #{retries - 1}")
        :timer.sleep(1000)
        fetch_with_retry(url, timeout, retries - 1)
    end
  end

  defp fetch_with_retry(_url, _timeout, 0) do
    {:error, :max_retries}
  end
end
```

### Circuit Breaker Pattern

```elixir
defmodule MyApp.CircuitBreaker do
  use GenServer

  defstruct [:failures, :state, :threshold, :timeout]

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def call(fun) do
    case GenServer.call(__MODULE__, :get_state) do
      :open ->
        {:error, :circuit_open}

      :closed ->
        execute_and_track(fun)
    end
  end

  @impl true
  def init(opts) do
    state = %__MODULE__{
      failures: 0,
      state: :closed,
      threshold: Keyword.get(opts, :threshold, 5),
      timeout: Keyword.get(opts, :timeout, 60_000)
    }
    {:ok, state}
  end

  @impl true
  def handle_call(:get_state, _from, state) do
    {:reply, state.state, state}
  end

  defp execute_and_track(fun) do
    case fun.() do
      {:ok, result} ->
        GenServer.cast(__MODULE__, :success)
        {:ok, result}

      error ->
        GenServer.cast(__MODULE__, :failure)
        error
    end
  end

  @impl true
  def handle_cast(:success, state) do
    {:noreply, %{state | failures: 0, state: :closed}}
  end

  @impl true
  def handle_cast(:failure, state) do
    new_failures = state.failures + 1

    if new_failures >= state.threshold do
      Process.send_after(self(), :half_open, state.timeout)
      {:noreply, %{state | failures: 0, state: :open}}
    else
      {:noreply, %{state | failures: new_failures}}
    end
  end

  @impl true
  def handle_info(:half_open, state) do
    {:noreply, %{state | state: :closed}}
  end
end
```

### Health Checks

```elixir
defmodule MyAppWeb.HealthController do
  use MyAppWeb, :controller

  def check(conn, _params) do
    health = %{
      status: "ok",
      database: check_database(),
      cache: check_cache(),
      external_api: check_external_api()
    }

    overall_status = if all_healthy?(health), do: 200, else: 503

    conn
    |> put_status(overall_status)
    |> json(health)
  end

  defp check_database do
    try do
      MyApp.Repo.query!("SELECT 1")
      "healthy"
    rescue
      _ -> "unhealthy"
    end
  end

  defp check_cache do
    case MyApp.Cache.get(:health_check) do
      {:ok, _} -> "healthy"
      _ -> "unhealthy"
    end
  end

  defp check_external_api do
    case MyApp.ExternalAPI.ping() do
      {:ok, _} -> "healthy"
      _ -> "degraded"
    end
  end

  defp all_healthy?(health) do
    Enum.all?(Map.values(health), &(&1 == "healthy" or &1 == "ok"))
  end
end
```

### Telemetry and Metrics

```elixir
# lib/my_app/application.ex
defmodule MyApp.Application do
  use Application

  def start(_type, _args) do
    children = [
      # Telemetry supervisor
      MyAppWeb.Telemetry,

      # Other children...
      MyApp.Repo,
      MyAppWeb.Endpoint
    ]

    opts = [strategy: :one_for_one, name: MyApp.Supervisor]
    Supervisor.start_link(children, opts)
  end
end

# lib/my_app_web/telemetry.ex
defmodule MyAppWeb.Telemetry do
  use Supervisor
  import Telemetry.Metrics

  def start_link(arg) do
    Supervisor.start_link(__MODULE__, arg, name: __MODULE__)
  end

  def init(_arg) do
    children = [
      {:telemetry_poller, measurements: periodic_measurements(), period: 10_000}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  def metrics do
    [
      # Phoenix metrics
      summary("phoenix.endpoint.stop.duration",
        unit: {:native, :millisecond}
      ),
      summary("phoenix.router_dispatch.stop.duration",
        tags: [:route],
        unit: {:native, :millisecond}
      ),

      # Database metrics
      summary("my_app.repo.query.total_time", unit: {:native, :millisecond}),
      summary("my_app.repo.query.decode_time", unit: {:native, :millisecond}),
      summary("my_app.repo.query.query_time", unit: {:native, :millisecond}),

      # VM metrics
      summary("vm.memory.total", unit: {:byte, :kilobyte}),
      summary("vm.total_run_queue_lengths.total"),
      summary("vm.total_run_queue_lengths.cpu"),
      summary("vm.total_run_queue_lengths.io")
    ]
  end

  defp periodic_measurements do
    []
  end
end
```

---

## Next Steps

**Master These Concepts**:

1. **GenServer**: Practice building stateful services
2. **Supervisor**: Design fault-tolerant systems
3. **Phoenix**: Build complete web applications
4. **Ecto**: Master database interactions

**Continue Learning**:

- [Advanced Tutorial](/en/learn/swe/prog-lang/elixir/tutorials/advanced) - BEAM VM, distributed systems, metaprogramming
- [How-To Guides](/en/learn/swe/prog-lang/elixir/how-to) - Practical patterns and solutions
- [Cookbook](/en/learn/swe/prog-lang/elixir/how-to/cookbook) - Ready-to-use recipes

**Practice Projects**:

1. **Chat Application**: Phoenix + LiveView + PubSub
2. **Task Queue**: GenServer + Supervisor + Ecto
3. **REST API**: Phoenix + Ecto + Authentication
4. **Monitoring Dashboard**: LiveView + Telemetry + Charts

**Resources**:

- [Elixir Guides](https://elixir-lang.org/getting-started/introduction.html)
- [Phoenix Guides](https://hexdocs.pm/phoenix/overview.html)
- [Ecto Guides](https://hexdocs.pm/ecto/getting-started.html)
- [Best Practices](/en/learn/swe/prog-lang/elixir/explanation/best-practices)

You now have the foundation for production Elixir development!
