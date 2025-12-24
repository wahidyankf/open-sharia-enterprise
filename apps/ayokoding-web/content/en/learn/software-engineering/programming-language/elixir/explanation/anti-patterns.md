---
title: "Anti Patterns"
date: 2025-12-21T00:00:00+07:00
draft: false
weight: 1000002
description: Common Elixir mistakes and anti-patterns - OOP habits, process misuse, performance pitfalls, testing issues, and how to fix them
tags: ["elixir", "anti-patterns", "mistakes", "pitfalls", "code-smells"]
---

**Want to avoid common Elixir pitfalls?** This guide identifies anti-patterns from OOP habits, process misuse, performance issues, and testing mistakes with solutions.

## Coming from OOP: Unlearning Bad Habits

### Thinking in Objects Instead of Processes

**The Mistake**:

```elixir
defmodule User do
  defstruct name: "", balance: 0

  def new(name), do: %User{name: name}

  def deposit(user, amount) do
    %{user | balance: user.balance + amount}
  end

  # Treating struct like object with methods
end

user = User.new("Alice")
user = User.deposit(user, 100)  # Confusing "mutation"
```

**Why It's Bad**:

- Confuses immutability (data doesn't change, you get new data)
- Misses process abstraction (where state should live)
- No isolation (struct shared across code)

**Better Approach**:

```elixir
defmodule Account do
  use GenServer

  def start_link(name) do
    GenServer.start_link(__MODULE__, %{name: name, balance: 0})
  end

  def deposit(pid, amount) do
    GenServer.call(pid, {:deposit, amount})
  end

  def balance(pid) do
    GenServer.call(pid, :balance)
  end

  # Server callbacks
  def init(state), do: {:ok, state}

  def handle_call({:deposit, amount}, _from, state) do
    new_balance = state.balance + amount
    new_state = %{state | balance: new_balance}
    {:reply, new_balance, new_state}
  end

  def handle_call(:balance, _from, state) do
    {:reply, state.balance, state}
  end
end

{:ok, account} = Account.start_link("Alice")
Account.deposit(account, 100)
Account.balance(account)  # 100
```

**Migration Path**:

1. Identify stateful "objects" (things that change over time)
2. Convert to GenServers (one process per instance)
3. Use structs only for immutable data (user profiles, configs)

---

### Overusing GenServer for Everything

**The Mistake**:

```elixir
defmodule Calculator do
  use GenServer

  def start_link, do: GenServer.start_link(__MODULE__, nil, name: __MODULE__)

  def add(a, b), do: GenServer.call(__MODULE__, {:add, a, b})

  def init(_), do: {:ok, nil}

  def handle_call({:add, a, b}, _from, state) do
    {:reply, a + b, state}  # No state needed!
  end
end
```

**Why It's Bad**:

- Unnecessary process (adds overhead)
- Serializes computation (single process bottleneck)
- No state to manage (pure function)

**Better Approach**:

```elixir
defmodule Calculator do
  def add(a, b), do: a + b
end
```

**When to use GenServer**:

- Managing state over time
- Coordinating access to resource
- Background work with state

**When NOT to use GenServer**:

- Pure functions (no state)
- One-off computations (use Task)
- Simple shared state (use Agent)

---

### Fighting Immutability

**The Mistake**:

```elixir
defmodule ShoppingCart do
  def add_item(cart, item) do
    cart.items = [item | cart.items]  # ❌ Error: can't mutate
  end

  # Or workaround with Process dictionary (don't do this!)
  def add_item_bad(item) do
    items = Process.get(:items, [])
    Process.put(:items, [item | items])  # ❌ Anti-pattern
  end
end
```

**Why It's Bad**:

- Immutability is fundamental to Elixir
- Process dictionary is hidden state (hard to debug)
- Defeats benefits (predictability, thread safety)

**Better Approach**:

```elixir
defmodule ShoppingCart do
  def add_item(cart, item) do
    %{cart | items: [item | cart.items]}
  end

  # Or use GenServer for persistent state
  def add_item_stateful(pid, item) do
    GenServer.call(pid, {:add_item, item})
  end
end

cart = %{items: []}
cart = ShoppingCart.add_item(cart, "Book")
cart = ShoppingCart.add_item(cart, "Pen")
```

**Migration Path**:

1. Accept that data doesn't change
2. Return new data from transformations
3. Use processes for long-lived mutable state

---

### Ignoring Pattern Matching Power

**The Mistake**:

```elixir
def process_response(response) do
  if response.status == :ok do
    data = response.data
    process(data)
  else
    error = response.error
    handle_error(error)
  end
end
```

**Why It's Bad**:

- Verbose, imperative
- Misses pattern matching benefits
- Unclear intent

**Better Approach**:

```elixir
def process_response(%{status: :ok, data: data}), do: process(data)
def process_response(%{status: :error, error: error}), do: handle_error(error)

def process_response(response) do
  case response do
    {:ok, data} -> process(data)
    {:error, reason} -> handle_error(reason)
  end
end
```

**Migration Path**:

1. Identify conditional logic
2. Replace with pattern matching in function heads or case
3. Use guards for value constraints

## Process Anti-Patterns

### Process Per Request

**The Mistake**:

```elixir
def handle_request(conn) do
  spawn(fn ->
    # Process request
    result = expensive_computation()
    send_response(conn, result)
  end)
end
```

**Why It's Bad**:

- Unnecessary overhead (processes for stateless work)
- No backpressure (unlimited spawning)
- Hard to supervise (many short-lived processes)

**Better Approach**:

```elixir
def handle_request(conn) do
  task = Task.async(fn -> expensive_computation() end)
  result = Task.await(task)
  send_response(conn, result)
end

Task.Supervisor.async_nolink(MySupervisor, fn ->
  expensive_computation()
end)
```

**When processes make sense per request**: Long-lived connections (WebSockets, channels).

---

### Shared State in Process Dictionary

**The Mistake**:

```elixir
defmodule Cache do
  def put(key, value) do
    Process.put(key, value)
  end

  def get(key) do
    Process.get(key)
  end
end
```

**Why It's Bad**:

- Hidden state (not in function signatures)
- Process-local only (not shared)
- Hard to debug and test
- Unpredictable (depends on calling process)

**Better Approach**:

```elixir
defmodule Cache do
  def init do
    :ets.new(:cache, [:set, :public, :named_table])
  end

  def put(key, value) do
    :ets.insert(:cache, {key, value})
  end

  def get(key) do
    case :ets.lookup(:cache, key) do
      [{^key, value}] -> {:ok, value}
      [] -> :error
    end
  end
end

defmodule Cache do
  use GenServer
  # ... proper state management
end
```

**When process dictionary is okay**: Very rare, usually library internals.

---

### Not Linking Processes

**The Mistake**:

```elixir
defmodule Worker do
  def start do
    spawn(fn ->
      loop()
    end)
  end

  defp loop do
    receive do
      :work -> do_work()
    end
    loop()
  end
end
```

**Why It's Bad**:

- No supervision (crashes undetected)
- Resource leaks (orphaned processes)
- Can't restart failed workers

**Better Approach**:

```elixir
defmodule Worker do
  use GenServer

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  def init(_opts), do: {:ok, %{}}

  def handle_info(:work, state) do
    do_work()
    {:noreply, state}
  end
end

defmodule MyApp.Supervisor do
  use Supervisor

  def init(_opts) do
    children = [
      Worker
    ]
    Supervisor.init(children, strategy: :one_for_one)
  end
end
```

**When spawn without link is okay**: Fire-and-forget tasks where failure doesn't matter.

---

### Blocking GenServer with Synchronous Calls

**The Mistake**:

```elixir
defmodule Fetcher do
  use GenServer

  def handle_call(:fetch_data, _from, state) do
    # Blocking HTTP call (can take seconds)
    data = HTTPoison.get!("https://api.example.com/data")
    {:reply, data, state}
  end
end

```

**Why It's Bad**:

- GenServer handles one message at a time
- Long operations block all callers
- Poor concurrency

**Better Approach**:

```elixir
defmodule Fetcher do
  use GenServer

  def handle_call(:fetch_data, from, state) do
    # Spawn task for blocking work
    Task.start(fn ->
      data = HTTPoison.get!("https://api.example.com/data")
      GenServer.reply(from, data)
    end)

    {:noreply, state}  # Don't block
  end
end

defmodule Fetcher do
  use GenServer

  def fetch_data(pid) do
    GenServer.cast(pid, :fetch_data)
  end

  def handle_cast(:fetch_data, state) do
    Task.start(fn ->
      data = HTTPoison.get!("https://api.example.com/data")
      send(self(), {:data_fetched, data})
    end)

    {:noreply, state}
  end

  def handle_info({:data_fetched, data}, state) do
    # Process data
    {:noreply, Map.put(state, :data, data)}
  end
end
```

**Migration Path**:

1. Identify blocking operations (I/O, HTTP, DB)
2. Move to Task or separate process
3. Use cast + handle_info instead of call

## Performance Anti-Patterns

### Premature Optimization

**The Mistake**:

```elixir
defmodule Processor do
  # Using ETS for everything "because it's fast"
  def process(data) do
    :ets.insert(:cache, {:data, data})
    # ... complex ETS logic when simple map would work
  end

  # Manually implementing tail recursion when Enum is clearer
  def sum(list, acc \\ 0)
  def sum([], acc), do: acc
  def sum([h | t], acc), do: sum(t, h + acc)

  # Instead of: Enum.sum(list)
end
```

**Why It's Bad**:

- Complexity without benefit (harder to read)
- Wrong optimizations (optimize wrong part)
- Maintenance burden

**Better Approach**:

```elixir
defmodule Processor do
  def process(data) do
    data
    |> validate()
    |> transform()
    |> save()
  end

  def sum(list), do: Enum.sum(list)
end

Benchee.run(%{
  "enum" => fn -> Enum.sum(1..10_000) end,
  "manual" => fn -> manual_sum(1..10_000) end
})

```

**Rule**: Make it work, make it right, make it fast (in that order).

---

### N+1 Query Problem

**The Mistake**:

```elixir
posts = Repo.all(Post)

posts_with_authors = Enum.map(posts, fn post ->
  author = Repo.get(User, post.author_id)  # N queries
  %{post | author: author}
end)
```

**Why It's Bad**:

- N+1 database queries (1 for posts, N for authors)
- Slow performance
- Database load

**Better Approach**:

```elixir
posts = Repo.all(Post) |> Repo.preload(:author)

posts = from(p in Post, preload: [:author]) |> Repo.all()

posts = from(p in Post,
  join: u in User, on: u.id == p.author_id,
  select: %{post: p, author: u}
) |> Repo.all()
```

**Migration Path**:

1. Identify loops fetching related data
2. Use `Repo.preload/2` or joins
3. Enable Ecto query logging to detect N+1

---

### Overusing Enum (Not Using Streams)

**The Mistake**:

```elixir
File.read!("huge_file.csv")
|> String.split("\n")
|> Enum.map(&parse_line/1)
|> Enum.filter(&valid?/1)
|> Enum.take(100)  # Only need 100, but loaded entire file
```

**Why It's Bad**:

- Memory waste (loads entire file)
- Slow (multiple full passes)
- Crashes on very large files (out of memory)

**Better Approach**:

```elixir
File.stream!("huge_file.csv")
|> Stream.map(&parse_line/1)
|> Stream.filter(&valid?/1)
|> Stream.take(100)
|> Enum.to_list()  # Only processes 100 lines
```

**When to use Stream**:

- Large/infinite collections
- Early termination (take, take_while)
- I/O operations (File.stream!)
- Chained transformations

**When Enum is fine**:

- Small collections
- Need entire result
- Single transformation

---

### Not Using Tail Recursion

**The Mistake**:

```elixir
def factorial(0), do: 1
def factorial(n), do: n * factorial(n - 1)

factorial(100_000)  # ❌ Crash
```

**Why It's Bad**:

- Stack overflow on large inputs
- Memory waste (stack frames)

**Better Approach**:

```elixir
def factorial(n, acc \\ 1)
def factorial(0, acc), do: acc
def factorial(n, acc), do: factorial(n - 1, n * acc)

factorial(100_000)  # ✅ OK
```

**Pattern**: Last operation must be recursive call (nothing after it).

---

### Creating Too Many Processes

**The Mistake**:

```elixir
Enum.each(items, fn item ->
  spawn(fn -> process_item(item) end)
end)
```

**Why It's Bad**:

- Scheduler overhead (too many processes)
- No backpressure (unbounded growth)
- Memory waste

**Better Approach**:

```elixir
items
|> Task.async_stream(&process_item/1, max_concurrency: 10)
|> Stream.run()

Flow.from_enumerable(items)
|> Flow.map(&process_item/1)
|> Enum.to_list()
```

**Rule**: Use processes for isolation and state, not just concurrency.

## Phoenix Anti-Patterns

### Fat Controllers

**The Mistake**:

```elixir
defmodule MyAppWeb.UserController do
  use MyAppWeb, :controller

  def create(conn, %{"user" => user_params}) do
    # All business logic in controller
    changeset = User.changeset(%User{}, user_params)

    case Repo.insert(changeset) do
      {:ok, user} ->
        # Send email
        Mailer.send_email(user.email, "Welcome!")

        # Log event
        Logger.info("User created: #{user.id}")

        # Update cache
        Cache.put(:users, user.id, user)

        # Notify other systems
        Notifier.notify(:user_created, user)

        conn
        |> put_flash(:info, "User created")
        |> redirect(to: ~p"/users/#{user}")

      {:error, changeset} ->
        render(conn, :new, changeset: changeset)
    end
  end
end
```

**Why It's Bad**:

- Mixed concerns (HTTP + business logic)
- Hard to test (need conn)
- Can't reuse logic (tied to HTTP)
- Violates Phoenix contexts pattern

**Better Approach**:

```elixir
defmodule MyAppWeb.UserController do
  use MyAppWeb, :controller

  def create(conn, %{"user" => user_params}) do
    case Accounts.create_user(user_params) do
      {:ok, user} ->
        conn
        |> put_flash(:info, "User created")
        |> redirect(to: ~p"/users/#{user}")

      {:error, changeset} ->
        render(conn, :new, changeset: changeset)
    end
  end
end

defmodule MyApp.Accounts do
  def create_user(attrs) do
    %User{}
    |> User.changeset(attrs)
    |> Repo.insert()
    |> tap(&maybe_send_welcome_email/1)
    |> tap(&maybe_log_event/1)
    |> tap(&maybe_update_cache/1)
    |> tap(&maybe_notify/1)
  end

  defp maybe_send_welcome_email({:ok, user}) do
    Task.start(fn -> Mailer.send_email(user.email, "Welcome!") end)
  end
  defp maybe_send_welcome_email(_), do: :ok

  # ... other helpers
end
```

**Migration Path**:

1. Extract business logic to context modules
2. Controllers handle HTTP only (params, responses)
3. Test contexts independently

---

### Not Using Changesets

**The Mistake**:

```elixir
defmodule MyAppWeb.UserController do
  def create(conn, %{"user" => params}) do
    cond do
      params["email"] == nil ->
        render(conn, :new, error: "Email required")

      not String.contains?(params["email"], "@") ->
        render(conn, :new, error: "Invalid email")

      String.length(params["password"]) < 8 ->
        render(conn, :new, error: "Password too short")

      true ->
        user = %User{
          email: params["email"],
          password: hash_password(params["password"])
        }
        Repo.insert(user)
    end
  end
end
```

**Why It's Bad**:

- Scattered validation logic
- No reusability (copy-paste everywhere)
- Hard to compose validations
- Doesn't integrate with forms

**Better Approach**:

```elixir
defmodule User do
  use Ecto.Schema
  import Ecto.Changeset

  schema "users" do
    field :email, :string
    field :password, :string, virtual: true
    field :password_hash, :string
  end

  def changeset(user, attrs) do
    user
    |> cast(attrs, [:email, :password])
    |> validate_required([:email, :password])
    |> validate_format(:email, ~r/@/)
    |> validate_length(:password, min: 8)
    |> unique_constraint(:email)
    |> put_password_hash()
  end

  defp put_password_hash(changeset) do
    case changeset do
      %{valid?: true, changes: %{password: password}} ->
        put_change(changeset, :password_hash, hash_password(password))
      _ ->
        changeset
    end
  end
end

%User{}
|> User.changeset(params)
|> Repo.insert()
```

**Migration Path**:

1. Create changesets for all schemas
2. Move validation to changesets
3. Use changesets in controllers and contexts

---

### Ignoring LiveView for Interactive UIs

**The Mistake**:

```elixir

```

**Why It's Bad**:

- Two codebases (Elixir + JavaScript)
- Complex state management
- Deployment complexity
- More moving parts

**Better Approach**:

```elixir
defmodule MyAppWeb.DashboardLive do
  use Phoenix.LiveView

  def mount(_params, _session, socket) do
    if connected?(socket) do
      # Subscribe to real-time updates
      Phoenix.PubSub.subscribe(MyApp.PubSub, "dashboard")
    end

    {:ok, fetch_data(socket)}
  end

  def handle_info({:update, data}, socket) do
    {:noreply, assign(socket, :data, data)}
  end

  def render(assigns) do
    ~H"""
    <div>
      <h1>Dashboard</h1>
      <%= for item <- @data do %>
        <div><%= item.name %></div>
      <% end %>
    </div>
    """
  end
end
```

**When LiveView makes sense**: Real-time updates, forms, dashboards, interactive UIs.

**When to use React/Vue**: Extreme client-side interactivity, offline-first, mobile apps.

## Testing Anti-Patterns

### Over-Mocking

**The Mistake**:

```elixir
test "create_user sends welcome email" do
  mock(Repo, :insert, fn _ -> {:ok, %User{id: 1}} end)
  mock(Mailer, :send_email, fn _, _ -> :ok end)
  mock(Logger, :info, fn _ -> :ok end)
  mock(Cache, :put, fn _, _, _ -> :ok end)

  Accounts.create_user(%{email: "user@example.com"})

  # What are we even testing?
end
```

**Why It's Bad**:

- Tests implementation, not behavior
- Brittle (breaks on refactoring)
- False confidence (mocks may not match reality)

**Better Approach**:

```elixir
test "create_user sends welcome email" do
  {:ok, user} = Accounts.create_user(%{email: "user@example.com", password: "password123"})

  assert_email_sent to: user.email, subject: "Welcome!"
  assert Repo.get(User, user.id)
end

defmock(EmailSenderMock, for: EmailSender)

test "create_user handles email failure" do
  expect(EmailSenderMock, :send, fn _, _ -> {:error, :timeout} end)

  {:ok, user} = Accounts.create_user(%{email: "user@example.com"})

  # User still created despite email failure
  assert Repo.get(User, user.id)
end
```

**When to mock**: External services (APIs, email), expensive operations, unpredictable results.

**When NOT to mock**: Database (use test DB), modules you own, deterministic functions.

---

### Testing Implementation Details

**The Mistake**:

```elixir
test "create_user calls validate_email" do
  assert_called(Accounts, :validate_email, ["user@example.com"])
end
```

**Why It's Bad**:

- Breaks when refactoring
- Doesn't test actual behavior
- False confidence

**Better Approach**:

```elixir
test "create_user rejects invalid email" do
  assert {:error, changeset} = Accounts.create_user(%{email: "invalid"})
  assert "is invalid" in errors_on(changeset).email
end
```

**Rule**: Test inputs and outputs, not internal implementation.

---

### Not Using `async: true`

**The Mistake**:

```elixir
defmodule UserTest do
  use ExUnit.Case  # No async: true

  test "test 1", do: # ...
  test "test 2", do: # ...
  # ... 100 tests, all sequential
end
```

**Why It's Bad**:

- Slow test suite (sequential execution)
- Underutilizes CPU cores

**Better Approach**:

```elixir
defmodule UserTest do
  use ExUnit.Case, async: true

  test "test 1", do: # ...
  test "test 2", do: # ...
end
```

**When NOT to use `async: true`**:

- Tests share global state
- Tests modify environment variables
- Tests use singleton processes

**Migration Path**:

1. Add `async: true` to all test modules
2. Fix tests that fail (likely shared state issues)
3. Enjoy faster test suite

Ready to write idiomatic Elixir? Check [Best Practices](/en/learn/software-engineering/programming-language/elixir/explanation/best-practices) for patterns to follow!
