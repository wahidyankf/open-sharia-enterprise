---
title: "Best Practices"
date: 2025-12-21T00:00:00+07:00
draft: false
weight: 1000001
description: Idiomatic Elixir patterns, best practices for functional programming, OTP design, Phoenix development, testing, and code organization
tags: ["elixir", "best-practices", "idiomatic", "patterns", "design-principles"]
---

**Want to write idiomatic Elixir?** This guide presents best practices for production Elixir code across functional programming, OTP, Phoenix, testing, and performance.

## What Makes Elixir Special

### Philosophy: Immutability and Predictability

**Core Principle**: All data is immutable. Transformations create new data, leaving original unchanged.

**Why it matters**:

- Predictable code (no hidden state changes)
- Thread-safe by default (no race conditions on data)
- Easier debugging (data flow is explicit)
- Simpler testing (functions pure and deterministic)

**Example**:

```elixir
defmodule Counter do
  def increment(state) do
    state.count = state.count + 1  # ❌ Error: cannot mutate
  end
end

defmodule Counter do
  def increment(state) do
    %{state | count: state.count + 1}  # ✅ Returns new state
  end
end
```

### Philosophy: Processes as Isolation Boundaries

**Core Principle**: Use processes for isolation, not just concurrency.

**Why it matters**:

- Crash isolation (one process crash doesn't affect others)
- State isolation (no shared memory, only messages)
- Resource isolation (per-process GC, heap)
- Fault tolerance (supervisors restart failed processes)

**Example**:

```elixir
{:ok, session1} = UserSession.start_link(user_id: 1)
{:ok, session2} = UserSession.start_link(user_id: 2)

```

### Philosophy: "Let It Crash"

**Core Principle**: Don't defensive code. Let processes crash and supervisors restart them to known-good state.

**Why it matters**:

- Simpler code (no complex error recovery)
- Self-healing systems (supervisors restore health)
- Clear separation (business logic vs error handling)
- Reduced bugs (fewer edge cases to handle)

**Example**:

```elixir
defmodule Parser do
  def parse(data) do
    try do
      case validate(data) do
        :ok ->
          try do
            transform(data)
          rescue
            _ -> {:error, "transform failed"}
          end
        :error -> {:error, "validation failed"}
      end
    rescue
      _ -> {:error, "unknown error"}
    end
  end
end

defmodule Parser do
  def parse(data) do
    data
    |> validate!()
    |> transform!()
  end
end

```

### Philosophy: Message Passing Over Shared State

**Core Principle**: Processes communicate via messages, never shared memory.

**Why it matters**:

- No locks or mutexes (eliminates deadlocks)
- Location transparency (works across nodes)
- Decoupling (sender doesn't know receiver internals)
- Testability (message-based protocols easy to test)

**Example**:

```elixir
GenServer.call(pid, {:get_value, key})
GenServer.cast(pid, {:update_value, key, new_value})

```

## Pattern Matching Best Practices

### Match Early, Match Often

**Good**:

```elixir
def process_user(%User{role: :admin, id: id}) do
  # Admin-specific logic
end

def process_user(%User{role: :user, id: id}) do
  # User-specific logic
end

```

**Why**: Function signatures document expected input structure.

---

### Use Guards for Type and Value Constraints

**Good**:

```elixir
def divide(a, b) when is_number(a) and is_number(b) and b != 0 do
  a / b
end

def categorize_age(age) when age < 13, do: :child
def categorize_age(age) when age < 20, do: :teen
def categorize_age(age) when age < 65, do: :adult
def categorize_age(_age), do: :senior
```

**Why**: Guards provide compile-time documentation and runtime safety.

---

### Pin Operator for Matching Existing Values

**Good**:

```elixir
expected_status = :ok
case fetch_data() do
  {^expected_status, data} -> process(data)
  {:error, reason} -> handle_error(reason)
end
```

**Why**: Makes intent clear (match specific value, not bind new variable).

---

### Destructure in Function Arguments

**Good**:

```elixir
def format_user(%{name: name, email: email}) do
  "#{name} <#{email}>"
end

def sum([]), do: 0
def sum([head | tail]), do: head + sum(tail)
```

**Avoid**:

```elixir
def format_user(user) do
  name = user.name
  email = user.email
  "#{name} <#{email}>"
end
```

**Why**: Destructuring in function head is more declarative and concise.

## Immutability Patterns

### Prefer Transformations Over Mutations

**Good**:

```elixir
defmodule Cart do
  def add_item(cart, item) do
    %{cart | items: [item | cart.items]}
  end

  def update_quantity(cart, item_id, quantity) do
    items = Enum.map(cart.items, fn item ->
      if item.id == item_id do
        %{item | quantity: quantity}
      else
        item
      end
    end)
    %{cart | items: items}
  end
end
```

**Why**: Clear data flow, easier to test, thread-safe.

---

### Use Update Syntax for Maps and Structs

**Good**:

```elixir
%{user | name: "Alice"}              # Update one field
%{user | name: "Alice", age: 30}     # Update multiple fields
```

**Note**: Only works for existing keys (compile-time check for structs).

---

### Leverage Pipe Operator for Transformations

**Good**:

```elixir
data
|> String.trim()
|> String.downcase()
|> String.split(",")
|> Enum.map(&String.trim/1)
|> Enum.reject(&(&1 == ""))
```

**Avoid**:

```elixir
reject_empty(map_trim(split_comma(downcase(trim(data)))))
```

**Why**: Pipelines read top-to-bottom, left-to-right (natural order).

---

### Chain with Result Tuples

**Good**:

```elixir
with {:ok, user} <- fetch_user(id),
     {:ok, profile} <- fetch_profile(user),
     {:ok, posts} <- fetch_posts(user) do
  {:ok, {user, profile, posts}}
else
  {:error, reason} -> {:error, reason}
end
```

**Why**: Clear error handling, short-circuits on first error.

## Process Design

### One Process Per Resource

**Good**:

```elixir
defmodule ChatSocket do
  use Phoenix.Channel

  def join("room:lobby", _payload, socket) do
    {:ok, socket}  # This socket = one process
  end
end

defmodule UserSession do
  use GenServer

  def start_link(user_id) do
    GenServer.start_link(__MODULE__, user_id)
  end
end
```

**Why**: Isolation, independent failure, resource cleanup on crash.

---

### Use GenServer for Stateful Resources

**Good**:

```elixir
defmodule Cache do
  use GenServer

  # Client API
  def start_link(opts), do: GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  def get(key), do: GenServer.call(__MODULE__, {:get, key})
  def put(key, value), do: GenServer.cast(__MODULE__, {:put, key, value})

  # Server callbacks
  def init(_opts), do: {:ok, %{}}
  def handle_call({:get, key}, _from, state), do: {:reply, Map.get(state, key), state}
  def handle_cast({:put, key, value}, state), do: {:noreply, Map.put(state, key, value)}
end
```

**When to use**:

- Manage state over time
- Coordinate access to resource
- Background work with state

---

### Use Task for Fire-and-Forget

**Good**:

```elixir
Task.start(fn -> send_email(user) end)

Task.Supervisor.start_child(MyApp.TaskSupervisor, fn ->
  send_email(user)
end)
```

**When to use**:

- One-off computations
- Background work without state
- Don't need result

---

### Use Agent for Simple State

**Good**:

```elixir
{:ok, counter} = Agent.start_link(fn -> 0 end)
Agent.get(counter, & &1)          # 0
Agent.update(counter, &(&1 + 1))
Agent.get(counter, & &1)          # 1
```

**When to use**:

- Simple shared state
- No complex logic
- Alternative to ETS for small data

**Avoid for**: Complex business logic (use GenServer).

---

### Minimize Process State

**Good**:

```elixir
defmodule UserSession do
  use GenServer

  def init(user_id) do
    # Only store essential state
    {:ok, %{user_id: user_id, last_activity: now()}}
  end

  def handle_call(:get_user, _from, state) do
    # Fetch fresh data on demand
    user = Users.get_user!(state.user_id)
    {:reply, user, state}
  end
end
```

**Why**: Less memory, avoid stale data, simpler state management.

## OTP Design Patterns

### Design Supervision Trees Bottom-Up

**Good**:

```elixir
defmodule MyApp.Supervisor do
  use Supervisor

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def init(_opts) do
    children = [
      # Workers first
      {MyApp.Cache, []},
      {MyApp.Registry, []},

      # Then dynamic supervisor for workers
      {DynamicSupervisor, name: MyApp.SessionSupervisor, strategy: :one_for_one},

      # Then higher-level components
      MyApp.WebSocket
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
```

**Strategy**:

- Workers at bottom
- Dynamic supervisors for runtime workers
- Static supervisors for infrastructure
- Application callbacks at top

---

### Choose Right Supervision Strategy

**:one_for_one** (most common):

```elixir
Supervisor.init(children, strategy: :one_for_one)
```

**Use when**: Children are independent (cache, workers, sessions).

**:rest_for_one**:

```elixir
Supervisor.init(children, strategy: :rest_for_one)
```

**Use when**: Children depend on each other in order (database → repo → cache).

**:one_for_all**:

```elixir
Supervisor.init(children, strategy: :one_for_all)
```

**Use when**: Children share state that must be consistent (rare).

---

### Name Processes Strategically

**Good**:

```elixir
GenServer.start_link(Cache, [], name: Cache)

{:ok, pid} = GenServer.start_link(UserSession, user_id, name: {:via, Registry, {MyApp.Registry, user_id}})

GenServer.start_link(Worker, [])
```

**When to name**:

- Singletons (one per application)
- Registry-based lookup needed

**When not to name**:

- Many instances (use Registry or store PIDs)
- Temporary workers

---

### Use Application Callbacks for Startup

**Good**:

```elixir
defmodule MyApp.Application do
  use Application

  def start(_type, _args) do
    children = [
      MyApp.Repo,
      MyApp.Endpoint,
      {Phoenix.PubSub, name: MyApp.PubSub}
    ]

    opts = [strategy: :one_for_one, name: MyApp.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
```

**Why**: Clean startup, supervisor tree defined, OTP-compliant.

## Phoenix Best Practices

### Thin Controllers, Rich Contexts

**Good**:

```elixir
defmodule MyAppWeb.UserController do
  use MyAppWeb, :controller

  def create(conn, %{"user" => user_params}) do
    case Accounts.create_user(user_params) do
      {:ok, user} ->
        conn
        |> put_flash(:info, "User created successfully")
        |> redirect(to: ~p"/users/#{user}")

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, :new, changeset: changeset)
    end
  end
end

defmodule MyApp.Accounts do
  def create_user(attrs) do
    %User{}
    |> User.changeset(attrs)
    |> Repo.insert()
    |> maybe_send_welcome_email()
  end

  defp maybe_send_welcome_email({:ok, user}) do
    Email.send_welcome(user)
    {:ok, user}
  end
  defp maybe_send_welcome_email(error), do: error
end
```

**Why**: Controllers handle HTTP, contexts handle business logic.

---

### Use Changesets for All Data Validation

**Good**:

```elixir
defmodule User do
  use Ecto.Schema
  import Ecto.Changeset

  schema "users" do
    field :email, :string
    field :age, :integer
  end

  def changeset(user, attrs) do
    user
    |> cast(attrs, [:email, :age])
    |> validate_required([:email])
    |> validate_format(:email, ~r/@/)
    |> validate_number(:age, greater_than: 0)
    |> unique_constraint(:email)
  end
end

%User{}
|> User.changeset(params)
|> Repo.insert()
```

**Why**: Centralized validation, reusable, composable.

---

### Leverage LiveView for Interactive UIs

**Good**:

```elixir
defmodule MyAppWeb.CounterLive do
  use Phoenix.LiveView

  def mount(_params, _session, socket) do
    {:ok, assign(socket, count: 0)}
  end

  def handle_event("increment", _value, socket) do
    {:noreply, update(socket, :count, &(&1 + 1))}
  end

  def render(assigns) do
    ~H"""
    <div>
      <p>Count: <%= @count %></p>
      <button phx-click="increment">+</button>
    </div>
    """
  end
end
```

**When to use**: Real-time updates, dynamic UIs, form validation.

**Why**: No JavaScript framework needed, simpler stack.

---

### Use Plugs for Cross-Cutting Concerns

**Good**:

```elixir
defmodule MyAppWeb.AuthPlug do
  import Plug.Conn

  def init(opts), do: opts

  def call(conn, _opts) do
    case get_session(conn, :user_id) do
      nil -> conn |> put_flash(:error, "Not authenticated") |> redirect(to: "/login") |> halt()
      user_id -> assign(conn, :current_user_id, user_id)
    end
  end
end

pipeline :authenticated do
  plug MyAppWeb.AuthPlug
end
```

**Use for**: Authentication, authorization, logging, rate limiting.

---

### Organize Routes with Pipelines

**Good**:

```elixir
defmodule MyAppWeb.Router do
  use MyAppWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :protect_from_forgery
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  pipeline :authenticated do
    plug MyAppWeb.AuthPlug
  end

  scope "/", MyAppWeb do
    pipe_through [:browser]
    get "/", PageController, :index
  end

  scope "/admin", MyAppWeb.Admin do
    pipe_through [:browser, :authenticated]
    resources "/users", UserController
  end

  scope "/api", MyAppWeb.API do
    pipe_through [:api]
    resources "/posts", PostController
  end
end
```

**Why**: Clear separation, composable pipelines, DRY.

## Testing Best Practices

### Test Behaviors, Not Implementation

**Good**:

```elixir
test "user registration sends welcome email" do
  {:ok, user} = Accounts.create_user(@valid_attrs)

  assert_email_sent to: user.email, subject: "Welcome!"
end
```

**Avoid**:

```elixir
test "create_user calls Email.send_welcome" do
  # Testing implementation detail
end
```

**Why**: Tests survive refactoring, focus on what matters.

---

### Use ExUnit Features Effectively

**Good**:

```elixir
defmodule UserTest do
  use ExUnit.Case, async: true  # Parallel tests

  setup do
    # Runs before each test
    {:ok, user: create_user()}
  end

  describe "create_user/1" do
    test "with valid data creates user", %{user: user} do
      assert user.email =~ "@"
    end

    test "with invalid data returns error" do
      assert {:error, changeset} = Accounts.create_user(%{email: "invalid"})
      assert "is invalid" in errors_on(changeset).email
    end
  end
end
```

**Features**: `async: true`, `setup`, `describe` for organization.

---

### Test Edge Cases and Error Paths

**Good**:

```elixir
describe "divide/2" do
  test "divides positive numbers" do
    assert Math.divide(10, 2) == 5.0
  end

  test "divides negative numbers" do
    assert Math.divide(-10, 2) == -5.0
  end

  test "returns error for division by zero" do
    assert {:error, :division_by_zero} = Math.divide(10, 0)
  end

  test "handles very large numbers" do
    assert Math.divide(1.0e308, 2) == 5.0e307
  end
end
```

**Why**: Edge cases reveal bugs, error paths ensure robustness.

---

### Use Mox for Test Doubles

**Good**:

```elixir
defmodule EmailSender do
  @callback send(String.t(), String.t()) :: :ok | {:error, term()}
end

defmodule RealEmailSender do
  @behaviour EmailSender
  def send(to, body), do: # ... actual sending
end

Mox.defmock(MockEmailSender, for: EmailSender)

config :my_app, email_sender: MockEmailSender

test "sends email" do
  expect(MockEmailSender, :send, fn to, body ->
    assert to == "user@example.com"
    :ok
  end)

  Accounts.create_user(@valid_attrs)
end
```

**Why**: Type-safe mocks, explicit contracts, compile-time errors.

## Performance Patterns

### Use Streams for Large Collections

**Good**:

```elixir
File.stream!("large_file.csv")
|> Stream.map(&parse_line/1)
|> Stream.filter(&valid?/1)
|> Stream.take(100)
|> Enum.to_list()
```

**Avoid**:

```elixir
File.read!("large_file.csv")
|> String.split("\n")
|> Enum.map(&parse_line/1)
|> Enum.filter(&valid?/1)
|> Enum.take(100)
```

**When to use Streams**: Large/infinite sequences, I/O operations, pipelines with early termination.

---

### Leverage Tail Recursion

**Good** (tail-recursive):

```elixir
def sum(list, acc \\ 0)
def sum([], acc), do: acc
def sum([head | tail], acc), do: sum(tail, head + acc)
```

**Avoid** (non-tail-recursive):

```elixir
def sum([]), do: 0
def sum([head | tail]), do: head + sum(tail)
```

**Why**: Tail recursion optimized to constant stack space.

---

### Use ETS for Fast In-Memory Storage

**Good**:

```elixir
defmodule Cache do
  def init do
    :ets.new(:cache, [:set, :public, :named_table])
  end

  def get(key) do
    case :ets.lookup(:cache, key) do
      [{^key, value}] -> {:ok, value}
      [] -> :error
    end
  end

  def put(key, value) do
    :ets.insert(:cache, {key, value})
  end
end
```

**When to use**: High-performance lookups, shared cache, counters.

**Trade-off**: No automatic cleanup (manage lifecycle carefully).

---

### Batch Database Queries

**Good**:

```elixir
user_ids = [1, 2, 3, 4, 5]
users = Repo.all(from u in User, where: u.id in ^user_ids)
```

**Avoid**:

```elixir
users = Enum.map(user_ids, &Repo.get(User, &1))
```

**Why**: Reduces database round-trips, improves performance.

## Code Organization

### Group Related Functions in Modules

**Good**:

```elixir
defmodule Users do
  def list_users, do: # ...
  def get_user!(id), do: # ...
  def create_user(attrs), do: # ...
  def update_user(user, attrs), do: # ...
  def delete_user(user), do: # ...
end
```

**Why**: Clear API boundary, discoverability.

---

### Use Module Attributes for Constants

**Good**:

```elixir
defmodule Config do
  @max_retries 3
  @timeout 5000

  def retry(fun, attempt \\ 0) do
    if attempt < @max_retries do
      # ...
    end
  end
end
```

**Why**: Centralized configuration, compile-time constants.

---

### Leverage `alias`, `import`, `require` Wisely

**Good**:

```elixir
defmodule MyApp.Users do
  alias MyApp.{Repo, User}           # Alias multiple
  import Ecto.Query, only: [from: 2] # Import specific
  require Logger                      # For macros

  def list_users do
    from(u in User, select: u)
    |> Repo.all()
  end
end
```

**Prefer `alias`**: Most common, just shortens names.

**Use `import` sparingly**: Only for frequently used functions.

**Use `require` for macros**: Logger, assert, pattern matching.

## Elixir Idioms

### Use Bang Functions Appropriately

**Conventions**:

- `function()` returns `{:ok, result}` or `{:error, reason}`
- `function!()` returns `result` or raises exception

**Good**:

```elixir
case Repo.insert(changeset) do
  {:ok, user} -> # ...
  {:error, changeset} -> # ...
end

user = Repo.insert!(changeset)  # Raise if error
```

**When to use `!`**: When error is truly exceptional, caller can't recover.

---

### Tagged Tuples for Results

**Good**:

```elixir
def fetch_user(id) do
  case Repo.get(User, id) do
    nil -> {:error, :not_found}
    user -> {:ok, user}
  end
end
```

**Convention**: `:ok` for success, `:error` for failure.

---

### Use `with` for Sequential Operations

**Good**:

```elixir
with {:ok, user} <- fetch_user(id),
     {:ok, posts} <- fetch_posts(user),
     {:ok, comments} <- fetch_comments(posts) do
  {:ok, {user, posts, comments}}
else
  {:error, reason} -> {:error, reason}
end
```

**Why**: Reads naturally, short-circuits on first error.

---

### Comprehensions for Transformations

**Good**:

```elixir
for x <- 1..10, rem(x, 2) == 0, do: x * 2
```

**When to use**: Simple transformations with filters, generating collections.

**When to avoid**: Complex logic (use `Enum` functions).

Ready to learn what to avoid? See [Anti-Patterns](/en/learn/software-engineering/programming-languages/elixir/explanation/anti-patterns) for common mistakes and how to fix them.
