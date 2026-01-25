# HTTP Server Example

**Category**: Service Template
**Complexity**: ⭐⭐⭐ (Advanced)
**Prerequisites**: Phoenix, Plug, Ecto, ExUnit

## Overview

This example demonstrates building a production-ready REST API server using Phoenix Framework with Plug middleware, JSON responses, error handling, and comprehensive testing. The example implements a donation campaign management system with Sharia-compliant financial operations.

## When to Use

✅ **Use Phoenix/Plug HTTP servers when**:

- Building REST APIs for web/mobile clients
- Need routing, request parsing, response formatting
- Require middleware (auth, logging, CORS)
- Building microservices with JSON APIs
- Need WebSocket support (Phoenix Channels)

❌ **Don't use for**:

- Simple scripts (use Mix tasks)
- Background jobs (use Oban)
- CLI applications (use escript)
- GraphQL APIs (use Absinthe instead)

## Complete Server Implementation

### Project Structure

```
donation_api/
├── lib/
│   ├── donation_api/
│   │   ├── campaigns/
│   │   │   ├── campaign.ex           # Ecto schema
│   │   │   ├── campaign_context.ex   # Business logic
│   │   │   └── donation.ex           # Ecto schema
│   │   ├── repo.ex                   # Ecto repository
│   │   └── application.ex            # OTP application
│   └── donation_api_web/
│       ├── controllers/
│       │   ├── campaign_controller.ex
│       │   ├── donation_controller.ex
│       │   └── fallback_controller.ex
│       ├── views/
│       │   ├── campaign_view.ex
│       │   ├── donation_view.ex
│       │   └── error_view.ex
│       ├── router.ex                 # HTTP routing
│       └── endpoint.ex               # Phoenix endpoint
├── config/
│   ├── config.exs
│   ├── dev.exs
│   ├── test.exs
│   └── prod.exs
├── test/
│   └── donation_api_web/
│       └── controllers/
│           ├── campaign_controller_test.exs
│           └── donation_controller_test.exs
└── mix.exs
```

### Phoenix Router

```elixir
# lib/donation_api_web/router.ex
defmodule DonationApiWeb.Router do
  use DonationApiWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
    plug :put_resp_content_type, "application/json"
  end

  pipeline :authenticated do
    plug DonationApiWeb.Plugs.Authentication
  end

  scope "/api/v1", DonationApiWeb do
    pipe_through :api

    # Public endpoints
    get "/health", HealthController, :check
    get "/campaigns", CampaignController, :index
    get "/campaigns/:id", CampaignController, :show

    # Authenticated endpoints
    scope "/" do
      pipe_through :authenticated

      post "/campaigns", CampaignController, :create
      put "/campaigns/:id", CampaignController, :update
      delete "/campaigns/:id", CampaignController, :delete

      post "/campaigns/:campaign_id/donations", DonationController, :create
      get "/campaigns/:campaign_id/donations", DonationController, :index
    end
  end
end
```

### Campaign Controller

```elixir
# lib/donation_api_web/controllers/campaign_controller.ex
defmodule DonationApiWeb.CampaignController do
  use DonationApiWeb, :controller

  alias DonationApi.Campaigns.CampaignContext
  alias DonationApiWeb.FallbackController

  action_fallback FallbackController

  @doc """
  GET /api/v1/campaigns
  Lists all active campaigns with pagination.
  """
  def index(conn, params) do
    page = Map.get(params, "page", "1") |> String.to_integer()
    per_page = Map.get(params, "per_page", "20") |> String.to_integer()

    case CampaignContext.list_campaigns(page, per_page) do
      {:ok, campaigns, total_count} ->
        conn
        |> put_status(:ok)
        |> put_resp_header("x-total-count", to_string(total_count))
        |> render("index.json", campaigns: campaigns)

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  GET /api/v1/campaigns/:id
  Shows a single campaign with statistics.
  """
  def show(conn, %{"id" => id}) do
    case CampaignContext.get_campaign(id) do
      {:ok, campaign} ->
        render(conn, "show.json", campaign: campaign)

      {:error, :not_found} ->
        {:error, :not_found}
    end
  end

  @doc """
  POST /api/v1/campaigns
  Creates a new campaign (requires authentication).

  Request body:
  {
    "title": "Build Masjid Al-Ikhlas",
    "description": "New mosque construction",
    "goal_amount": "500000000",
    "goal_currency": "IDR",
    "start_date": "2025-01-01",
    "end_date": "2025-12-31"
  }
  """
  def create(conn, %{"campaign" => campaign_params}) do
    case CampaignContext.create_campaign(campaign_params) do
      {:ok, campaign} ->
        conn
        |> put_status(:created)
        |> put_resp_header("location", campaign_path(conn, :show, campaign.id))
        |> render("show.json", campaign: campaign)

      {:error, changeset} ->
        {:error, changeset}
    end
  end

  @doc """
  PUT /api/v1/campaigns/:id
  Updates an existing campaign (requires authentication).
  """
  def update(conn, %{"id" => id, "campaign" => campaign_params}) do
    case CampaignContext.update_campaign(id, campaign_params) do
      {:ok, campaign} ->
        render(conn, "show.json", campaign: campaign)

      {:error, :not_found} ->
        {:error, :not_found}

      {:error, changeset} ->
        {:error, changeset}
    end
  end

  @doc """
  DELETE /api/v1/campaigns/:id
  Soft-deletes a campaign (requires authentication).
  """
  def delete(conn, %{"id" => id}) do
    case CampaignContext.delete_campaign(id) do
      {:ok, _campaign} ->
        send_resp(conn, :no_content, "")

      {:error, :not_found} ->
        {:error, :not_found}

      {:error, :has_donations} ->
        {:error, :cannot_delete_campaign_with_donations}
    end
  end

  # Helper for location header
  defp campaign_path(conn, :show, id) do
    DonationApiWeb.Router.Helpers.campaign_path(conn, :show, id)
  end
end
```

### Donation Controller

```elixir
# lib/donation_api_web/controllers/donation_controller.ex
defmodule DonationApiWeb.DonationController do
  use DonationApiWeb, :controller

  alias DonationApi.Campaigns.CampaignContext
  alias DonationApiWeb.FallbackController

  action_fallback FallbackController

  @doc """
  POST /api/v1/campaigns/:campaign_id/donations
  Creates a new donation for a campaign.

  Request body:
  {
    "donor_name": "Ahmad Abdullah",
    "donor_email": "ahmad@example.com",
    "amount": "1000000",
    "currency": "IDR",
    "payment_method": "bank_transfer",
    "is_anonymous": false
  }
  """
  def create(conn, %{"campaign_id" => campaign_id, "donation" => donation_params}) do
    params = Map.put(donation_params, "campaign_id", campaign_id)

    case CampaignContext.create_donation(params) do
      {:ok, donation} ->
        conn
        |> put_status(:created)
        |> render("show.json", donation: donation)

      {:error, :campaign_not_found} ->
        {:error, :campaign_not_found}

      {:error, :campaign_ended} ->
        {:error, :campaign_ended}

      {:error, :goal_exceeded} ->
        {:error, :goal_exceeded}

      {:error, changeset} ->
        {:error, changeset}
    end
  end

  @doc """
  GET /api/v1/campaigns/:campaign_id/donations
  Lists all donations for a campaign.
  """
  def index(conn, %{"campaign_id" => campaign_id} = params) do
    page = Map.get(params, "page", "1") |> String.to_integer()
    per_page = Map.get(params, "per_page", "50") |> String.to_integer()

    case CampaignContext.list_donations(campaign_id, page, per_page) do
      {:ok, donations, total_count} ->
        conn
        |> put_status(:ok)
        |> put_resp_header("x-total-count", to_string(total_count))
        |> render("index.json", donations: donations)

      {:error, :campaign_not_found} ->
        {:error, :campaign_not_found}
    end
  end
end
```

### Fallback Controller (Error Handling)

```elixir
# lib/donation_api_web/controllers/fallback_controller.ex
defmodule DonationApiWeb.FallbackController do
  use DonationApiWeb, :controller

  def call(conn, {:error, :not_found}) do
    conn
    |> put_status(:not_found)
    |> put_view(DonationApiWeb.ErrorView)
    |> render("404.json", message: "Resource not found")
  end

  def call(conn, {:error, :campaign_not_found}) do
    conn
    |> put_status(:not_found)
    |> put_view(DonationApiWeb.ErrorView)
    |> render("404.json", message: "Campaign not found")
  end

  def call(conn, {:error, :campaign_ended}) do
    conn
    |> put_status(:unprocessable_entity)
    |> put_view(DonationApiWeb.ErrorView)
    |> render("422.json", message: "Campaign has ended, no more donations accepted")
  end

  def call(conn, {:error, :goal_exceeded}) do
    conn
    |> put_status(:unprocessable_entity)
    |> put_view(DonationApiWeb.ErrorView)
    |> render("422.json", message: "Donation would exceed campaign goal")
  end

  def call(conn, {:error, :cannot_delete_campaign_with_donations}) do
    conn
    |> put_status(:unprocessable_entity)
    |> put_view(DonationApiWeb.ErrorView)
    |> render("422.json", message: "Cannot delete campaign with existing donations")
  end

  def call(conn, {:error, %Ecto.Changeset{} = changeset}) do
    conn
    |> put_status(:unprocessable_entity)
    |> put_view(DonationApiWeb.ErrorView)
    |> render("422.json", changeset: changeset)
  end

  def call(conn, {:error, reason}) when is_atom(reason) do
    conn
    |> put_status(:internal_server_error)
    |> put_view(DonationApiWeb.ErrorView)
    |> render("500.json", message: "Internal server error: #{reason}")
  end
end
```

### JSON Views

```elixir
# lib/donation_api_web/views/campaign_view.ex
defmodule DonationApiWeb.CampaignView do
  use DonationApiWeb, :view

  alias DonationApi.Campaigns.Campaign

  def render("index.json", %{campaigns: campaigns}) do
    %{
      data: Enum.map(campaigns, &campaign_json/1)
    }
  end

  def render("show.json", %{campaign: campaign}) do
    %{
      data: campaign_json(campaign)
    }
  end

  defp campaign_json(%Campaign{} = campaign) do
    %{
      id: campaign.id,
      title: campaign.title,
      description: campaign.description,
      goal: %{
        amount: Decimal.to_string(campaign.goal_amount),
        currency: campaign.goal_currency
      },
      current: %{
        amount: Decimal.to_string(campaign.current_amount),
        currency: campaign.goal_currency
      },
      progress_percentage: calculate_progress(campaign),
      start_date: Date.to_iso8601(campaign.start_date),
      end_date: Date.to_iso8601(campaign.end_date),
      status: campaign.status,
      donation_count: campaign.donation_count || 0,
      inserted_at: DateTime.to_iso8601(campaign.inserted_at),
      updated_at: DateTime.to_iso8601(campaign.updated_at)
    }
  end

  defp calculate_progress(%Campaign{goal_amount: goal, current_amount: current}) do
    if Decimal.equal?(goal, Decimal.new(0)) do
      0
    else
      current
      |> Decimal.div(goal)
      |> Decimal.mult(Decimal.new(100))
      |> Decimal.round(2)
      |> Decimal.to_float()
    end
  end
end
```

```elixir
# lib/donation_api_web/views/donation_view.ex
defmodule DonationApiWeb.DonationView do
  use DonationApiWeb, :view

  alias DonationApi.Campaigns.Donation

  def render("index.json", %{donations: donations}) do
    %{
      data: Enum.map(donations, &donation_json/1)
    }
  end

  def render("show.json", %{donation: donation}) do
    %{
      data: donation_json(donation)
    }
  end

  defp donation_json(%Donation{} = donation) do
    base = %{
      id: donation.id,
      campaign_id: donation.campaign_id,
      amount: %{
        amount: Decimal.to_string(donation.amount),
        currency: donation.currency
      },
      payment_method: donation.payment_method,
      is_anonymous: donation.is_anonymous,
      inserted_at: DateTime.to_iso8601(donation.inserted_at)
    }

    if donation.is_anonymous do
      base
    else
      Map.merge(base, %{
        donor_name: donation.donor_name,
        donor_email: donation.donor_email
      })
    end
  end
end
```

```elixir
# lib/donation_api_web/views/error_view.ex
defmodule DonationApiWeb.ErrorView do
  use DonationApiWeb, :view

  def render("404.json", %{message: message}) do
    %{
      error: %{
        code: "not_found",
        message: message
      }
    }
  end

  def render("422.json", %{changeset: changeset}) do
    %{
      error: %{
        code: "validation_failed",
        message: "Validation failed",
        details: translate_errors(changeset)
      }
    }
  end

  def render("422.json", %{message: message}) do
    %{
      error: %{
        code: "unprocessable_entity",
        message: message
      }
    }
  end

  def render("500.json", %{message: message}) do
    %{
      error: %{
        code: "internal_server_error",
        message: message
      }
    }
  end

  defp translate_errors(changeset) do
    Ecto.Changeset.traverse_errors(changeset, fn {msg, opts} ->
      Enum.reduce(opts, msg, fn {key, value}, acc ->
        String.replace(acc, "%{#{key}}", to_string(value))
      end)
    end)
  end
end
```

### Integration Tests

```elixir
# test/donation_api_web/controllers/campaign_controller_test.exs
defmodule DonationApiWeb.CampaignControllerTest do
  use DonationApiWeb.ConnCase, async: true

  alias DonationApi.Campaigns.{Campaign, CampaignContext}

  @valid_attrs %{
    title: "Build Masjid Al-Ikhlas",
    description: "New mosque construction project",
    goal_amount: "500000000",
    goal_currency: "IDR",
    start_date: "2025-01-01",
    end_date: "2025-12-31"
  }

  @invalid_attrs %{
    title: "",
    goal_amount: "-1000",
    goal_currency: "INVALID"
  }

  describe "GET /api/v1/campaigns" do
    test "lists all campaigns", %{conn: conn} do
      # Create test campaigns
      {:ok, campaign1} = CampaignContext.create_campaign(@valid_attrs)
      {:ok, campaign2} = CampaignContext.create_campaign(
        Map.put(@valid_attrs, :title, "Another Campaign")
      )

      conn = get(conn, "/api/v1/campaigns")

      assert %{"data" => campaigns} = json_response(conn, 200)
      assert length(campaigns) == 2
      assert Enum.any?(campaigns, fn c -> c["id"] == campaign1.id end)
      assert Enum.any?(campaigns, fn c -> c["id"] == campaign2.id end)
    end

    test "supports pagination", %{conn: conn} do
      # Create 25 campaigns
      for i <- 1..25 do
        {:ok, _} = CampaignContext.create_campaign(
          Map.put(@valid_attrs, :title, "Campaign #{i}")
        )
      end

      conn = get(conn, "/api/v1/campaigns?page=1&per_page=10")

      assert %{"data" => campaigns} = json_response(conn, 200)
      assert length(campaigns) == 10
      assert get_resp_header(conn, "x-total-count") == ["25"]
    end
  end

  describe "GET /api/v1/campaigns/:id" do
    test "shows campaign when exists", %{conn: conn} do
      {:ok, campaign} = CampaignContext.create_campaign(@valid_attrs)

      conn = get(conn, "/api/v1/campaigns/#{campaign.id}")

      assert %{"data" => data} = json_response(conn, 200)
      assert data["id"] == campaign.id
      assert data["title"] == campaign.title
      assert data["goal"]["amount"] == "500000000"
      assert data["goal"]["currency"] == "IDR"
    end

    test "returns 404 when campaign not found", %{conn: conn} do
      conn = get(conn, "/api/v1/campaigns/00000000-0000-0000-0000-000000000000")

      assert %{"error" => %{"code" => "not_found"}} = json_response(conn, 404)
    end
  end

  describe "POST /api/v1/campaigns" do
    @tag :authenticated
    test "creates campaign with valid data", %{conn: conn} do
      conn = post(conn, "/api/v1/campaigns", campaign: @valid_attrs)

      assert %{"data" => data} = json_response(conn, 201)
      assert data["title"] == @valid_attrs.title
      assert data["status"] == "active"
      assert get_resp_header(conn, "location") != []
    end

    @tag :authenticated
    test "returns errors with invalid data", %{conn: conn} do
      conn = post(conn, "/api/v1/campaigns", campaign: @invalid_attrs)

      assert %{"error" => error} = json_response(conn, 422)
      assert error["code"] == "validation_failed"
      assert error["details"]["title"] != nil
    end
  end

  describe "PUT /api/v1/campaigns/:id" do
    @tag :authenticated
    test "updates campaign with valid data", %{conn: conn} do
      {:ok, campaign} = CampaignContext.create_campaign(@valid_attrs)

      update_attrs = %{title: "Updated Title"}
      conn = put(conn, "/api/v1/campaigns/#{campaign.id}", campaign: update_attrs)

      assert %{"data" => data} = json_response(conn, 200)
      assert data["title"] == "Updated Title"
    end

    @tag :authenticated
    test "returns 404 when campaign not found", %{conn: conn} do
      conn = put(conn, "/api/v1/campaigns/00000000-0000-0000-0000-000000000000", campaign: %{})

      assert %{"error" => %{"code" => "not_found"}} = json_response(conn, 404)
    end
  end

  describe "DELETE /api/v1/campaigns/:id" do
    @tag :authenticated
    test "deletes campaign when no donations", %{conn: conn} do
      {:ok, campaign} = CampaignContext.create_campaign(@valid_attrs)

      conn = delete(conn, "/api/v1/campaigns/#{campaign.id}")

      assert response(conn, 204)
    end

    @tag :authenticated
    test "returns error when campaign has donations", %{conn: conn} do
      {:ok, campaign} = CampaignContext.create_campaign(@valid_attrs)
      {:ok, _donation} = CampaignContext.create_donation(%{
        campaign_id: campaign.id,
        donor_name: "Test Donor",
        donor_email: "test@example.com",
        amount: "1000000",
        currency: "IDR",
        payment_method: "bank_transfer",
        is_anonymous: false
      })

      conn = delete(conn, "/api/v1/campaigns/#{campaign.id}")

      assert %{"error" => error} = json_response(conn, 422)
      assert error["message"] =~ "Cannot delete campaign with existing donations"
    end
  end
end
```

## Key Patterns

### 1. Action Fallback Pattern

```elixir
action_fallback FallbackController

def show(conn, %{"id" => id}) do
  case Context.get_resource(id) do
    {:ok, resource} -> render(conn, "show.json", resource: resource)
    {:error, :not_found} -> {:error, :not_found}  # Handled by fallback
  end
end
```

### 2. JSON View Pattern

```elixir
def render("show.json", %{resource: resource}) do
  %{data: resource_json(resource)}
end
```

### 3. Pagination Headers

```elixir
conn
|> put_resp_header("x-total-count", to_string(total))
|> render("index.json", items: items)
```

### 4. Location Header for Created Resources

```elixir
conn
|> put_status(:created)
|> put_resp_header("location", resource_path(conn, :show, resource.id))
|> render("show.json", resource: resource)
```

## Best Practices

1. **Use `action_fallback`**: Centralize error handling
2. **Return tuples**: `{:ok, result}` or `{:error, reason}` from contexts
3. **Separate views**: Keep JSON serialization in view modules
4. **Test with ConnCase**: Use Phoenix testing helpers
5. **Validate in context**: Business validation in context, not controller
6. **Use proper status codes**: 200 OK, 201 Created, 204 No Content, 404 Not Found, 422 Unprocessable Entity
7. **Add pagination**: Always paginate list endpoints

## Common Mistakes

### ❌ Mistake 1: Business logic in controllers

```elixir
# Bad: Validation in controller
def create(conn, params) do
  if params["amount"] > 0 do
    # ...
  end
end

# Good: Validation in context
def create(conn, params) do
  case Context.create_resource(params) do
    {:ok, resource} -> # ...
  end
end
```

### ❌ Mistake 2: Not using fallback controller

```elixir
# Bad: Repetitive error handling
def show(conn, %{"id" => id}) do
  case get(id) do
    {:ok, item} -> render(conn, "show.json", item: item)
    {:error, :not_found} ->
      conn
      |> put_status(:not_found)
      |> render("error.json", message: "Not found")
  end
end

# Good: Fallback handles errors
action_fallback FallbackController

def show(conn, %{"id" => id}) do
  case get(id) do
    {:ok, item} -> render(conn, "show.json", item: item)
    {:error, :not_found} -> {:error, :not_found}
  end
end
```

## Resources

- [Back to Templates README](README.md)
- [Phoenix Framework](https://hexdocs.pm/phoenix/Phoenix.html)
- [Plug Documentation](https://hexdocs.pm/plug/Plug.html)
- [Phoenix Testing](https://hexdocs.pm/phoenix/testing.html)

---

**Last Updated**: 2025-01-23
**Complexity**: Advanced
**Production Ready**: ✅ Yes
