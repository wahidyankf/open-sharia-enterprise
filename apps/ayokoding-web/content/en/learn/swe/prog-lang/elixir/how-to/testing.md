---
title: "Testing with ExUnit"
date: 2025-12-21T17:30:00+07:00
draft: false
description: "Write comprehensive tests in Elixir using ExUnit with assertions, setup callbacks, async tests, mocks, and property-based testing."
weight: 1000010
tags: ["elixir", "testing", "exunit", "tdd", "quality", "how-to"]
---

**Need robust testing for Elixir applications?** ExUnit provides powerful testing with async execution, setup callbacks, and rich assertions.

## Prerequisites

- Basic Elixir syntax
- Understanding of modules and functions
- Completed [Beginner Tutorial](/en/learn/swe/prog-lang/elixir/tutorials/beginner)

## Problem

Building reliable applications requires comprehensive test coverage across unit tests, integration tests, and property-based tests. You need fast test execution, clear failure messages, and patterns for testing asynchronous code, databases, and external services.

**Challenges:**

- Testing concurrent and asynchronous code
- Managing test data and database state
- Mocking external dependencies
- Writing maintainable test suites
- Ensuring adequate test coverage

## Solution

Use **ExUnit** for comprehensive test coverage with assertions, setup callbacks, async execution, and integration with tools like Mox for mocking and StreamData for property-based testing.

## How It Works

### 1. Basic Tests

```elixir
defmodule MathTest do
  use ExUnit.Case

  describe "addition" do
    test "adds two positive numbers" do
      assert 1 + 1 == 2
    end

    test "adds negative numbers" do
      assert -1 + -1 == -2
    end

    test "adds zero" do
      assert 5 + 0 == 5
    end
  end

  describe "division" do
    test "divides evenly" do
      assert div(10, 2) == 5
    end

    test "raises on division by zero" do
      assert_raise ArithmeticError, fn ->
        div(1, 0)
      end
    end

    test "raises with specific message" do
      assert_raise ArithmeticError, "bad argument in arithmetic expression", fn ->
        div(1, 0)
      end
    end
  end
end
```

Run tests:

```bash
mix test                    # All tests
mix test test/math_test.exs  # Specific file
mix test test/math_test.exs:10  # Specific line
mix test --trace            # Detailed output
mix test --seed 0           # Deterministic order
mix test --max-failures 1   # Stop after first failure
```

### 2. Setup and Teardown

```elixir
defmodule UserTest do
  use ExUnit.Case

  # Runs once before all tests
  setup_all do
    start_supervised!(MyApp.Repo)
    :ok
  end

  # Runs before each test
  setup do
    user = %User{name: "Alice", age: 30}
    {:ok, user: user}
  end

  # Context from setup available in test
  test "user has name", %{user: user} do
    assert user.name == "Alice"
  end

  test "user has age", %{user: user} do
    assert user.age == 30
  end
end
```

### 3. Async Tests

```elixir
defmodule FastTest do
  # Run tests concurrently (no shared state)
  use ExUnit.Case, async: true

  test "runs in parallel 1" do
    assert true
  end

  test "runs in parallel 2" do
    assert 1 + 1 == 2
  end
end
```

### 4. Assertions

```elixir
defmodule AssertionsTest do
  use ExUnit.Case

  test "equality assertions" do
    assert 1 == 1
    refute 1 == 2
  end

  test "truthiness" do
    assert true
    assert 1  # Non-nil, non-false
    refute false
    refute nil
  end

  test "pattern matching" do
    assert {:ok, value} = {:ok, 42}
    assert value == 42
  end

  test "approximate equality" do
    assert_in_delta 0.1 + 0.2, 0.3, 0.0001
  end

  test "exceptions" do
    assert_raise RuntimeError, fn ->
      raise "error"
    end

    assert_raise RuntimeError, "specific message", fn ->
      raise "specific message"
    end
  end

  test "received messages" do
    send(self(), :hello)
    assert_received :hello
  end

  test "will receive messages" do
    Task.start(fn ->
      Process.sleep(10)
      send(self(), :delayed)
    end)

    assert_receive :delayed, 100  # Wait up to 100ms
  end
end
```

### 5. Testing GenServers

```elixir
defmodule CounterTest do
  use ExUnit.Case

  setup do
    {:ok, pid} = Counter.start_link(0)
    {:ok, counter: pid}
  end

  test "increments counter", %{counter: pid} do
    assert Counter.get(pid) == 0
    Counter.increment(pid)
    assert Counter.get(pid) == 1
  end

  test "handles multiple increments", %{counter: pid} do
    Counter.increment(pid)
    Counter.increment(pid)
    Counter.increment(pid)
    assert Counter.get(pid) == 3
  end

  test "resets counter", %{counter: pid} do
    Counter.increment(pid)
    Counter.reset(pid)
    assert Counter.get(pid) == 0
  end
end
```

### 6. Database Testing with Ecto

```elixir
defmodule MyApp.AccountsTest do
  use MyApp.DataCase  # Custom test case with Ecto sandbox

  alias MyApp.Accounts
  alias MyApp.Accounts.User

  describe "create_user/1" do
    test "creates user with valid data" do
      attrs = %{name: "Alice", email: "alice@example.com", age: 30}

      assert {:ok, %User{} = user} = Accounts.create_user(attrs)
      assert user.name == "Alice"
      assert user.email == "alice@example.com"
      assert user.age == 30
    end

    test "returns error with invalid data" do
      attrs = %{name: nil, email: "invalid"}

      assert {:error, changeset} = Accounts.create_user(attrs)
      assert "can't be blank" in errors_on(changeset).name
      assert "has invalid format" in errors_on(changeset).email
    end

    test "enforces unique email" do
      attrs = %{name: "Alice", email: "alice@example.com", age: 30}

      assert {:ok, _user} = Accounts.create_user(attrs)
      assert {:error, changeset} = Accounts.create_user(attrs)
      assert "has already been taken" in errors_on(changeset).email
    end
  end
end
```

Custom DataCase:

```elixir
defmodule MyApp.DataCase do
  use ExUnit.CaseTemplate

  using do
    quote do
      alias MyApp.Repo
      import Ecto
      import Ecto.Changeset
      import Ecto.Query
      import MyApp.DataCase
    end
  end

  setup tags do
    pid = Ecto.Adapters.SQL.Sandbox.start_owner!(MyApp.Repo, shared: not tags[:async])
    on_exit(fn -> Ecto.Adapters.SQL.Sandbox.stop_owner(pid) end)
    :ok
  end

  def errors_on(changeset) do
    Ecto.Changeset.traverse_errors(changeset, fn {message, opts} ->
      Regex.replace(~r"%{(\w+)}", message, fn _, key ->
        opts |> Keyword.get(String.to_existing_atom(key), key) |> to_string()
      end)
    end)
  end
end
```

### 7. Mocking with Mox

Add dependency:

```elixir
{:mox, "~> 1.0", only: :test}
```

Define behavior:

```elixir
defmodule MyApp.WeatherAPI do
  @callback get_temperature(String.t()) :: {:ok, float()} | {:error, term()}
end
```

Implementation:

```elixir
defmodule MyApp.WeatherAPI.HTTP do
  @behaviour MyApp.WeatherAPI

  def get_temperature(city) do
    # Real HTTP call
    HTTPoison.get("https://api.weather.com/#{city}")
  end
end
```

Mock in test:

```elixir
# test/test_helper.exs
Mox.defmock(MyApp.WeatherAPI.Mock, for: MyApp.WeatherAPI)

# test/my_app/weather_test.exs
defmodule MyApp.WeatherTest do
  use ExUnit.Case, async: true

  import Mox

  # Verify mocks are called
  setup :verify_on_exit!

  test "gets temperature for city" do
    expect(MyApp.WeatherAPI.Mock, :get_temperature, fn "London" ->
      {:ok, 18.5}
    end)

    assert {:ok, 18.5} = MyApp.Weather.get_temperature("London")
  end

  test "handles API errors" do
    expect(MyApp.WeatherAPI.Mock, :get_temperature, fn "Mars" ->
      {:error, :not_found}
    end)

    assert {:error, :not_found} = MyApp.Weather.get_temperature("Mars")
  end
end
```

Configure mock in config:

```elixir
# config/test.exs
config :my_app, :weather_api, MyApp.WeatherAPI.Mock

# config/prod.exs
config :my_app, :weather_api, MyApp.WeatherAPI.HTTP

# In application code
@weather_api Application.compile_env(:my_app, :weather_api)

def get_temperature(city) do
  @weather_api.get_temperature(city)
end
```

### 8. Property-Based Testing with StreamData

```elixir
{:stream_data, "~> 1.0", only: :test}
```

```elixir
defmodule StringTest do
  use ExUnit.Case
  use ExUnitProperties

  property "reversing a string twice returns original" do
    check all str <- string(:alphanumeric) do
      assert str == str |> String.reverse() |> String.reverse()
    end
  end

  property "list length is preserved after sorting" do
    check all list <- list_of(integer()) do
      assert length(list) == length(Enum.sort(list))
    end
  end

  property "addition is commutative" do
    check all a <- integer(),
              b <- integer() do
      assert a + b == b + a
    end
  end
end
```

Custom generators:

```elixir
defmodule MyGenerators do
  import StreamData

  def user do
    gen all name <- string(:alphanumeric, min_length: 1),
            email <- email(),
            age <- integer(18..120) do
      %{name: name, email: email, age: age}
    end
  end

  def email do
    gen all username <- string(:alphanumeric, min_length: 1),
            domain <- string(:alphanumeric, min_length: 1) do
      "#{username}@#{domain}.com"
    end
  end
end

# Usage
property "user creation" do
  check all user <- MyGenerators.user() do
    assert {:ok, _} = Accounts.create_user(user)
  end
end
```

### 9. Testing Phoenix Controllers

```elixir
defmodule MyAppWeb.UserControllerTest do
  use MyAppWeb.ConnCase

  describe "index" do
    test "lists all users", %{conn: conn} do
      conn = get(conn, ~p"/users")
      assert html_response(conn, 200) =~ "Listing Users"
    end
  end

  describe "create user" do
    test "redirects to show when data is valid", %{conn: conn} do
      attrs = %{name: "Alice", email: "alice@example.com"}

      conn = post(conn, ~p"/users", user: attrs)

      assert %{id: id} = redirected_params(conn)
      assert redirected_to(conn) == ~p"/users/#{id}"

      conn = get(conn, ~p"/users/#{id}")
      assert html_response(conn, 200) =~ "Alice"
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, ~p"/users", user: %{name: nil})
      assert html_response(conn, 200) =~ "can&#39;t be blank"
    end
  end
end
```

JSON API testing:

```elixir
describe "GET /api/users" do
  test "returns list of users", %{conn: conn} do
    user = insert(:user, name: "Alice")

    conn = get(conn, ~p"/api/users")

    assert json_response(conn, 200) == %{
      "data" => [
        %{
          "id" => user.id,
          "name" => "Alice",
          "email" => user.email
        }
      ]
    }
  end
end
```

### 10. Testing LiveView

```elixir
defmodule MyAppWeb.CounterLiveTest do
  use MyAppWeb.ConnCase

  import Phoenix.LiveViewTest

  test "renders initial count", %{conn: conn} do
    {:ok, view, html} = live(conn, ~p"/counter")

    assert html =~ "Count: 0"
  end

  test "increments count on button click", %{conn: conn} do
    {:ok, view, _html} = live(conn, ~p"/counter")

    # Click increment button
    assert view
           |> element("button", "Increment")
           |> render_click() =~ "Count: 1"

    # Click again
    assert view
           |> element("button", "Increment")
           |> render_click() =~ "Count: 2"
  end

  test "handles form submission", %{conn: conn} do
    {:ok, view, _html} = live(conn, ~p"/users/new")

    # Submit form
    assert view
           |> form("#user-form", user: %{name: "Alice", email: "alice@example.com"})
           |> render_submit()

    assert_redirect(view, ~p"/users")
  end
end
```

## Variations

### Test Factories with ExMachina

```elixir
{:ex_machina, "~> 2.7", only: :test}

defmodule MyApp.Factory do
  use ExMachina.Ecto, repo: MyApp.Repo

  def user_factory do
    %MyApp.Accounts.User{
      name: sequence(:name, &"User #{&1}"),
      email: sequence(:email, &"user#{&1}@example.com"),
      age: 30
    }
  end

  def admin_factory do
    struct!(
      user_factory(),
      %{role: :admin}
    )
  end
end

# Usage in tests
user = insert(:user)
admin = insert(:admin)
users = insert_list(3, :user)
params = params_for(:user)
```

### Coverage Reports

```bash
# Run with coverage
mix test --cover

# Generate HTML report
mix test --cover --export-coverage default
mix test.coverage

# View report
open cover/excoveralls.html
```

Add excoveralls:

```elixir
{:excoveralls, "~> 0.18", only: :test}

# mix.exs
def project do
  [
    test_coverage: [tool: ExCoveralls],
    preferred_cli_env: [
      coveralls: :test,
      "coveralls.detail": :test,
      "coveralls.post": :test,
      "coveralls.html": :test
    ]
  ]
end
```

## Advanced Patterns

### 1. Testing Concurrent Code

```elixir
test "handles concurrent requests" do
  {:ok, pid} = Server.start_link()

  # Spawn multiple processes
  tasks = for i <- 1..100 do
    Task.async(fn ->
      Server.increment(pid)
    end)
  end

  # Wait for all to complete
  Task.await_many(tasks)

  assert Server.get(pid) == 100
end
```

### 2. Testing with Real Time

```elixir
test "expires cache after TTL" do
  Cache.put(:key, :value, ttl: 100)  # 100ms

  assert Cache.get(:key) == {:ok, :value}

  Process.sleep(150)

  assert Cache.get(:key) == {:error, :not_found}
end
```

### 3. Tagging Tests

```elixir
@tag :slow
test "expensive operation" do
  # Long-running test
end

@tag :integration
test "external API call" do
  # Integration test
end

# Run only tagged tests
# mix test --only slow
# mix test --exclude integration
```

## Use Cases

**Unit Tests:**

- Pure functions
- Business logic
- Data transformations

**Integration Tests:**

- Database operations
- External APIs
- Full request/response cycle

**Property-Based Tests:**

- Edge cases
- Input validation
- Algorithm correctness

## Troubleshooting

### Tests Hang

```elixir
# Add timeout to test
@tag timeout: 1000  # milliseconds
test "completes quickly" do
  # ...
end
```

### Database Issues

```bash
# Reset test database
mix ecto.reset

# Check sandbox mode
# config/test.exs
config :my_app, MyApp.Repo,
  pool: Ecto.Adapters.SQL.Sandbox
```

### Flaky Tests

```elixir
# Use assert_receive instead of assert_received
assert_receive :message, 100  # Wait up to 100ms

# Add delays for async operations
Process.sleep(10)
```

## Best Practices

1. **Test behavior, not implementation:**
   Focus on what, not how

2. **Use descriptive test names:**

   ```elixir
   test "creates user when all required fields are provided"
   ```

3. **Arrange-Act-Assert pattern:**

   ```elixir
   test "increments counter" do
     # Arrange
     {:ok, pid} = Counter.start_link(0)

     # Act
     Counter.increment(pid)

     # Assert
     assert Counter.get(pid) == 1
   end
   ```

4. **One assertion per test (generally):**
   Makes failures clearer

5. **Use setup for common test data:**
   DRY principle

6. **Test edge cases:**
   Nil, empty, negative, maximum values

7. **Mock external dependencies:**
   Tests should be fast and deterministic

8. **Run tests in CI:**
   Catch issues before production

## Common Pitfalls

1. **Shared state in async tests:** Use async: false if needed
2. **Not cleaning up processes:** Use start_supervised!
3. **Testing implementation details:** Brittle tests
4. **Slow tests:** Mock expensive operations
5. **Insufficient coverage:** Aim for >80%

## Related Resources

- [Debugging Guide](/en/learn/swe/prog-lang/elixir/how-to/debugging)
- [GenServer Guide](/en/learn/swe/prog-lang/elixir/how-to/genserver)
- [Ecto Guide](/en/learn/swe/prog-lang/elixir/how-to/ecto)
- [ExUnit Documentation](https://hexdocs.pm/ex_unit/)
- [Mox Documentation](https://hexdocs.pm/mox/)
- [StreamData Documentation](https://hexdocs.pm/stream_data/)
