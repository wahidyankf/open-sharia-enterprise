# Phoenix Code Templates

Production-ready code templates for Phoenix applications in the open-sharia-enterprise platform. These templates provide complete, functional boilerplate code following OSE Platform conventions and Elixir/Phoenix best practices.

## 📋 Available Templates

### 1. Context Template

**File**: [ex-so-plwe-elph-te\_\_context-template.md](./ex-so-plwe-elph-te__context-template.md)

Complete Phoenix Context template with:

- Business logic organization following Phoenix Contexts pattern
- Public API functions with documentation
- Private helper functions
- Ecto repository interactions
- Error handling with tagged tuples (`{:ok, result}` / `{:error, reason}`)
- OSE Platform examples (Zakat, Murabaha, Waqf)

**Use when**: Creating new business logic modules (Contexts) for domain functionality

**Example scenarios**:

- Zakat Context for zakat calculations and tracking
- Murabaha Context for Islamic financing applications
- Waqf Context for charitable endowment management

### 2. Controller Template

**File**: [ex-so-plwe-elph-te\_\_controller-template.md](./ex-so-plwe-elph-te__controller-template.md)

REST controller template with:

- CRUD actions (index, show, create, update, delete)
- JSON rendering with views
- Error handling and status codes
- Parameter validation
- Authentication with plugs
- Pagination support

**Use when**: Creating REST API endpoints for resources

**Example scenarios**:

- Zakat calculation endpoints
- Murabaha contract management endpoints
- Waqf project donation endpoints

### 3. LiveView Template

**File**: [ex-so-plwe-elph-te\_\_liveview-template.md](./ex-so-plwe-elph-te__liveview-template.md)

Phoenix LiveView template with:

- mount/3 lifecycle for initial state
- handle_event/3 for user interactions
- handle_info/2 for async updates
- Real-time updates with PubSub
- Form handling with changesets
- Optimistic updates

**Use when**: Creating real-time interactive interfaces

**Example scenarios**:

- Real-time Zakat calculator with live updates
- Murabaha application form with validation
- Live Waqf donation tracker

### 4. Channel Template

**File**: [ex-so-plwe-elph-te\_\_channel-template.md](./ex-so-plwe-elph-te__channel-template.md)

Phoenix Channel template with:

- join/3 authorization
- handle_in/3 for incoming messages
- handle_out/3 for broadcast filtering
- PubSub integration
- Presence tracking

**Use when**: Creating WebSocket channels for bi-directional communication

**Example scenarios**:

- Real-time notification channel
- Chat channel for customer support
- Live data updates channel

### 5. Schema Template

**File**: [ex-so-plwe-elph-te\_\_schema-template.md](./ex-so-plwe-elph-te__schema-template.md)

Ecto Schema template with:

- Schema definition with fields and types
- Associations (belongs_to, has_many)
- Embedded schemas
- Changeset with validation
- Virtual fields

**Use when**: Creating database-backed data structures

**Example scenarios**:

- ZakatCalculation schema
- MurabahaContract schema
- WaqfProject schema

### 6. Test Template

**File**: [ex-so-plwe-elph-te\_\_test-template.md](./ex-so-plwe-elph-te__test-template.md)

Comprehensive test template with:

- ExUnit test structure
- Context/function tests
- Controller tests with Conn
- LiveView tests
- Channel tests
- Test helpers and fixtures

**Use when**: Writing tests for any Phoenix component

**Example scenarios**:

- Testing Zakat calculation logic
- Testing Murabaha API endpoints
- Testing LiveView interactions

## 🚀 Quick Start

### Using a Template

1. **Choose appropriate template** based on what you're creating (Context, Controller, LiveView, Channel, Schema, or Test)

2. **Copy template code** from the template file

3. **Replace placeholders**:
   - `[EntityName]` → Your entity name (e.g., `ZakatCalculation`, `MurabahaContract`)
   - `[entity_name]` → Lowercase snake_case (e.g., `zakat_calculation`, `murabaha_contract`)
   - `[entities]` → Plural form (e.g., `zakat_calculations`, `murabaha_contracts`)
   - Module names, function names, and logic

4. **Test thoroughly**:
   - Unit tests for business logic
   - Integration tests for full stack
   - Property-based tests for edge cases
   - Load tests for performance

### Example: Creating a Zakat Calculation Context

**Step 1**: Copy context template from `ex-so-plwe-elph-te__context-template.md`

**Step 2**: Replace placeholders:

```elixir
# From template:
defmodule OsePlatform.[EntityName] do
  # ...
end

# Your code:
defmodule OsePlatform.Zakat do
  @moduledoc """
  The Zakat context - manages zakat calculations and tracking.
  """
  # ...
end
```

**Step 3**: Customize business logic:

```elixir
defmodule OsePlatform.Zakat do
  @moduledoc """
  The Zakat context - manages zakat calculations and tracking.
  """

  import Ecto.Query, warn: false
  alias OsePlatform.Repo
  alias OsePlatform.Zakat.Calculation

  @doc """
  Calculates zakat based on wealth and nisab threshold.

  ## Examples

      iex> calculate_zakat(%{wealth: 10000, currency: "USD", user_id: "user-123"})
      {:ok, %Calculation{zakat_amount: Decimal.new("250.00"), zakat_due: true}}

      iex> calculate_zakat(%{wealth: 3000, currency: "USD", user_id: "user-123"})
      {:ok, %Calculation{zakat_amount: Decimal.new("0.00"), zakat_due: false}}
  """
  def calculate_zakat(attrs) do
    with {:ok, nisab} <- get_nisab_threshold(attrs.currency),
         {:ok, calculation} <- perform_calculation(attrs, nisab) do
      {:ok, Repo.insert!(calculation)}
    end
  end

  defp perform_calculation(%{wealth: wealth} = attrs, nisab) do
    zakat_due = Decimal.compare(wealth, nisab) in [:gt, :eq]
    zakat_amount = if zakat_due, do: Decimal.mult(wealth, Decimal.new("0.025")), else: Decimal.new("0")

    calculation = %Calculation{
      wealth: wealth,
      currency: attrs.currency,
      nisab_threshold: nisab,
      zakat_amount: zakat_amount,
      zakat_due: zakat_due,
      user_id: attrs.user_id,
      calculation_date: Date.utc_today()
    }

    {:ok, calculation}
  end

  defp get_nisab_threshold(currency) do
    # Fetch from external API or cache
    {:ok, Decimal.new("5000")}
  end
end
```

**Step 4**: Write tests:

```elixir
defmodule OsePlatform.ZakatTest do
  use OsePlatform.DataCase

  alias OsePlatform.Zakat

  describe "calculate_zakat/1" do
    test "calculates zakat when wealth above nisab" do
      attrs = %{wealth: Decimal.new("10000"), currency: "USD", user_id: "user-123"}

      assert {:ok, calculation} = Zakat.calculate_zakat(attrs)
      assert calculation.zakat_due == true
      assert Decimal.equal?(calculation.zakat_amount, Decimal.new("250.00"))
    end

    test "returns zero zakat when wealth below nisab" do
      attrs = %{wealth: Decimal.new("3000"), currency: "USD", user_id: "user-123"}

      assert {:ok, calculation} = Zakat.calculate_zakat(attrs)
      assert calculation.zakat_due == false
      assert Decimal.equal?(calculation.zakat_amount, Decimal.new("0.00"))
    end
  end
end
```

**Step 5**: Test and deploy

## 📝 Template Conventions

All templates follow these conventions:

### Elixir & Phoenix

- **Pattern matching**: Use pattern matching for control flow and error handling
- **Pipe operator**: Chain operations with `|>`
- **Documentation**: Add `@moduledoc` and `@doc` to all public functions
- **Type specs**: Use `@spec` for function type specifications (optional but recommended)

### Error Handling

- **Tagged tuples**: Return `{:ok, result}` or `{:error, reason}`
- **with expressions**: Use `with` for sequential operations that may fail
- **Rescue clauses**: Catch exceptions only when necessary
- **Error atoms**: Use descriptive atoms (`:not_found`, `:invalid_params`, `:unauthorized`)

### Contexts & Boundaries

- **Context modules**: Group related functionality in contexts
- **Public API**: Expose only necessary functions
- **Private helpers**: Use `defp` for internal functions
- **Boundary enforcement**: Contexts don't call other contexts directly

### Data Access

- **Ecto queries**: Use Ecto.Query for database operations
- **Changesets**: Validate data with Ecto.Changeset
- **Transactions**: Use Repo.transaction for multi-step operations
- **Preloading**: Fetch associations explicitly to prevent N+1 queries

### Real-time Features

- **LiveView**: Use for rich interactive UIs
- **Channels**: Use for custom WebSocket protocols
- **PubSub**: Broadcast updates to multiple clients
- **Presence**: Track online users

## 🎨 OSE Platform Context

Templates include examples from the OSE Platform Islamic finance domain:

### Zakat (Obligatory Charity)

- **Calculation Context**: Calculate zakat based on wealth and nisab threshold
- **Nisab tracking**: Fetch current gold/silver prices for nisab calculation
- **Historical tracking**: Store annual zakat calculations
- **Notifications**: Send reminders when zakat is due

**Domain concepts**:

- Nisab: Minimum wealth threshold (85g gold or 595g silver)
- Hawal: Lunar year (354 days) for wealth tracking
- Zakat rate: 2.5% of qualifying wealth

### Murabaha (Cost-Plus Financing)

- **Application Context**: Process financing applications
- **Credit verification**: Check credit scores and risk levels
- **Installment scheduling**: Generate payment schedules
- **Contract management**: Track active contracts and payments

**Domain concepts**:

- Purchase price: Original asset cost
- Markup: Profit margin percentage
- Installments: Monthly payments over contract period
- Grace period: Days before late payment penalties

### Waqf (Endowment)

- **Donation Context**: Process one-time and recurring donations
- **Project management**: Create and manage charitable projects
- **Impact tracking**: Monitor project outcomes and donor contributions
- **Donor management**: Maintain donor profiles and donation history

**Domain concepts**:

- Waqf project: Charitable project with funding goal
- Donation: One-time or recurring contribution
- Impact report: Quarterly reports on project outcomes
- Donor receipt: Tax-deductible donation receipts

## 🔒 Security Best Practices

Templates incorporate security measures:

- **Input validation**: Ecto.Changeset validation on all external data
- **SQL injection prevention**: Ecto parameterized queries (automatic)
- **Authentication**: Plug-based authentication in controllers and LiveViews
- **Authorization**: Function-level authorization checks
- **CSRF protection**: Built-in Phoenix CSRF token verification
- **XSS prevention**: Automatic HTML escaping in templates
- **Rate limiting**: Plug-based rate limiting for API endpoints

### Example: Secured Controller

```elixir
defmodule OsePlatformWeb.ZakatCalculationController do
  use OsePlatformWeb, :controller

  alias OsePlatform.Zakat
  alias OsePlatform.Auth

  # Require authentication for all actions
  plug :require_authenticated_user

  # Require specific permission for create action
  plug :require_permission, :zakat_write when action in [:create, :update, :delete]

  def index(conn, _params) do
    user_id = conn.assigns.current_user.id
    calculations = Zakat.list_calculations_by_user(user_id)

    render(conn, "index.json", calculations: calculations)
  end

  def create(conn, %{"calculation" => calculation_params}) do
    user_id = conn.assigns.current_user.id

    attrs = Map.put(calculation_params, "user_id", user_id)

    case Zakat.calculate_zakat(attrs) do
      {:ok, calculation} ->
        conn
        |> put_status(:created)
        |> render("show.json", calculation: calculation)

      {:error, changeset} ->
        conn
        |> put_status(:unprocessable_entity)
        |> render("error.json", changeset: changeset)
    end
  end

  defp require_authenticated_user(conn, _opts) do
    case Auth.get_current_user(conn) do
      nil ->
        conn
        |> put_status(:unauthorized)
        |> json(%{error: "Authentication required"})
        |> halt()

      user ->
        assign(conn, :current_user, user)
    end
  end

  defp require_permission(conn, permission) do
    user = conn.assigns.current_user

    if Auth.has_permission?(user, permission) do
      conn
    else
      conn
      |> put_status(:forbidden)
      |> json(%{error: "Insufficient permissions"})
      |> halt()
    end
  end
end
```

## 🧪 Testing Guidelines

Test templates thoroughly:

### Context Tests (Business Logic)

```elixir
defmodule OsePlatform.ZakatTest do
  use OsePlatform.DataCase

  alias OsePlatform.Zakat

  describe "calculate_zakat/1" do
    test "calculates correct zakat amount" do
      attrs = %{wealth: Decimal.new("10000"), currency: "USD", user_id: "user-123"}

      assert {:ok, calculation} = Zakat.calculate_zakat(attrs)
      assert Decimal.equal?(calculation.zakat_amount, Decimal.new("250.00"))
    end

    test "handles invalid input" do
      attrs = %{wealth: Decimal.new("-100"), currency: "USD", user_id: "user-123"}

      assert {:error, changeset} = Zakat.calculate_zakat(attrs)
      assert "must be greater than 0" in errors_on(changeset).wealth
    end
  end
end
```

### Controller Tests (HTTP Endpoints)

```elixir
defmodule OsePlatformWeb.ZakatCalculationControllerTest do
  use OsePlatformWeb.ConnCase

  alias OsePlatform.Zakat

  setup %{conn: conn} do
    user = insert(:user)
    conn = conn |> authenticate_user(user)
    {:ok, conn: conn, user: user}
  end

  describe "POST /api/zakat/calculations" do
    test "creates calculation with valid params", %{conn: conn} do
      params = %{
        "wealth" => "10000",
        "currency" => "USD"
      }

      conn = post(conn, ~p"/api/zakat/calculations", calculation: params)

      assert %{"id" => id, "zakat_amount" => "250.00"} = json_response(conn, 201)
    end

    test "returns error with invalid params", %{conn: conn} do
      params = %{"wealth" => "-100", "currency" => "USD"}

      conn = post(conn, ~p"/api/zakat/calculations", calculation: params)

      assert json_response(conn, 422)["errors"] != %{}
    end
  end
end
```

### LiveView Tests (Interactive UI)

```elixir
defmodule OsePlatformWeb.ZakatCalculatorLiveTest do
  use OsePlatformWeb.ConnCase

  import Phoenix.LiveViewTest

  test "calculates zakat on form submit", %{conn: conn} do
    {:ok, view, _html} = live(conn, ~p"/zakat/calculator")

    form_data = %{
      "calculation" => %{
        "wealth" => "10000",
        "currency" => "USD"
      }
    }

    view
    |> form("#calculation-form", form_data)
    |> render_submit()

    assert has_element?(view, "#zakat-result", "250.00")
  end
end
```

---

**Last Updated**: 2026-01-26
**Phoenix Version**: 1.7+
**Elixir Version**: 1.14+
