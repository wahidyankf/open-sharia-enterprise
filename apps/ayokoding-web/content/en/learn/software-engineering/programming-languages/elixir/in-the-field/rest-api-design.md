---
title: "REST API Design"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000019
description: "From basic routing to production API design with versioning, authentication, error handling, and pagination"
tags: ["elixir", "phoenix", "rest", "api", "authentication", "pagination"]
prev: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/ecto-patterns"
next: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/graphql-absinthe"
---

**How do you design production-grade REST APIs in Elixir?** This guide teaches RESTful routing conventions, API versioning strategies, authentication patterns with JWT, error response design, and pagination/filtering approaches for building robust HTTP APIs.

## Why It Matters

REST API design determines how clients interact with your system. Production APIs need:

- **RESTful conventions** - Standard resource routing (GET /donations, POST /donations/:id)
- **API versioning** - Backward compatibility for evolving interfaces (/api/v1/, /api/v2/)
- **Authentication** - Secure access control (JWT tokens, session management)
- **Error consistency** - Standard error responses with proper HTTP status codes
- **Pagination/filtering** - Handle large datasets efficiently (limit/offset, cursor-based)

Real-world scenarios requiring robust API design:

- **Financial services** - Donation APIs, transaction history, account management
- **E-commerce platforms** - Product catalogs, order processing, inventory queries
- **Mobile backends** - User authentication, data sync, push notifications
- **Third-party integrations** - Webhook endpoints, public APIs, partner integrations
- **Internal microservices** - Service-to-service communication, health checks

Production question: Should you use /api/v1 prefix, JWT authentication, or cursor-based pagination? The answer depends on your versioning strategy, security requirements, and data volume.

## Phoenix Router - RESTful Routing Conventions

Phoenix provides router DSL for standard REST resource routing.

### resources/4 - Standard Resource Routes

```elixir
# Router with RESTful resource routes
defmodule DonationAPI.Router do
  use Phoenix.Router                             # => Import router macros
                                                 # => Provides get, post, resources

  pipeline :api do
    plug :accepts, ["json"]                      # => Only accept JSON content type
                                                 # => Type: Plug.t()
  end

  scope "/api/v1", DonationAPI do
    pipe_through :api                            # => Apply :api pipeline to all routes
                                                 # => Runs :accepts plug

    resources "/donations", DonationController   # => Generates 7 standard routes:
                                                 # => GET    /donations           (index)
                                                 # => GET    /donations/:id       (show)
                                                 # => POST   /donations           (create)
                                                 # => PATCH  /donations/:id       (update)
                                                 # => PUT    /donations/:id       (update)
                                                 # => DELETE /donations/:id       (delete)
                                                 # => Type: macro expansion
  end
end
```

Standard RESTful routing with single `resources` declaration.

### Nested Resources - Related Entities

```elixir
# Nested donation comments route
scope "/api/v1", DonationAPI do
  pipe_through :api

  resources "/donations", DonationController do
    resources "/comments", CommentController     # => Nested resource routes:
                                                 # => GET /donations/:donation_id/comments
                                                 # => POST /donations/:donation_id/comments
                                                 # => Parameters include :donation_id
                                                 # => Type: nested route macro
  end
end
```

Nested routes automatically include parent resource ID in parameters.

### Custom Routes - Non-Standard Actions

```elixir
# Custom action routes
scope "/api/v1", DonationAPI do
  pipe_through :api

  resources "/donations", DonationController do
    post "/approve", DonationController, :approve
                                                 # => POST /donations/:id/approve
                                                 # => Custom action beyond REST
                                                 # => Calls approve/2 controller action

    get "/pending", DonationController, :pending, as: :pending
                                                 # => GET /donations/:id/pending
                                                 # => Named route: donation_pending_path
                                                 # => Type: custom route definition
  end
end
```

Custom routes extend standard REST actions for domain-specific operations.

### Complete Example - Financial Donation API

```elixir
# Production donation API router
defmodule DonationAPI.Router do
  use Phoenix.Router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/api/v1", DonationAPI do
    pipe_through :api

    resources "/donations", DonationController, except: [:new, :edit] do
                                                 # => Exclude HTML form routes
                                                 # => Only API routes: index, show, create, update, delete
                                                 # => Type: options keyword list

      post "/approve", DonationController, :approve
      post "/reject", DonationController, :reject
      get "/receipt", DonationController, :receipt
    end

    resources "/campaigns", CampaignController, only: [:index, :show]
                                                 # => Read-only campaign access
                                                 # => No create/update/delete
  end
end
```

Production router with selective route generation and custom actions.

## API Versioning - Backward Compatibility

### URL Prefix Versioning - /api/v1/

```elixir
# Version-based routing with URL prefixes
defmodule DonationAPI.Router do
  use Phoenix.Router

  pipeline :api do
    plug :accepts, ["json"]
  end

  # API Version 1
  scope "/api/v1", DonationAPI.V1 do             # => Version 1 routes
    pipe_through :api                            # => Namespace: DonationAPI.V1

    resources "/donations", DonationController   # => V1.DonationController
                                                 # => Path: /api/v1/donations
  end

  # API Version 2 - New fields
  scope "/api/v2", DonationAPI.V2 do             # => Version 2 routes
    pipe_through :api                            # => Namespace: DonationAPI.V2

    resources "/donations", DonationController   # => V2.DonationController
                                                 # => Path: /api/v2/donations
                                                 # => Different implementation than V1
  end
end
```

URL prefix versioning allows parallel version support with separate controllers.

### Version-Specific Controllers

```elixir
# V1 controller - Original response format
defmodule DonationAPI.V1.DonationController do
  use Phoenix.Controller

  def show(conn, %{"id" => id}) do
    donation = Donations.get_donation!(id)       # => Fetch donation by ID
                                                 # => Type: %Donation{}

    json(conn, %{
      id: donation.id,
      amount: donation.amount,                   # => Integer amount in cents
      donor: donation.donor_name                 # => String donor name
    })                                           # => V1 response format
  end
end

# V2 controller - Enhanced response format
defmodule DonationAPI.V2.DonationController do
  use Phoenix.Controller

  def show(conn, %{"id" => id}) do
    donation = Donations.get_donation!(id)

    json(conn, %{
      id: donation.id,
      amount: %{
        cents: donation.amount,                  # => V2: Structured amount
        currency: "USD"                          # => V2: Added currency field
      },
      donor: %{
        name: donation.donor_name,               # => V2: Structured donor
        email: donation.donor_email              # => V2: Added email field
      },
      metadata: donation.metadata                # => V2: New metadata field
    })                                           # => V2 enhanced format
  end
end
```

Separate controllers per version support different response structures without breaking V1 clients.

## Authentication - JWT with Guardian

Guardian library provides JWT token authentication for Elixir APIs.

### Guardian Configuration

```elixir
# Guardian JWT configuration
defmodule DonationAPI.Guardian do
  use Guardian, otp_app: :donation_api           # => Guardian behavior
                                                 # => Config from :donation_api

  def subject_for_token(user, _claims) do
    {:ok, to_string(user.id)}                    # => User ID as token subject
                                                 # => Type: {:ok, String.t()}
  end

  def resource_from_claims(%{"sub" => id}) do
    user = Accounts.get_user!(id)                # => Fetch user from token subject
    {:ok, user}                                  # => Return user resource
                                                 # => Type: {:ok, %User{}}
  end
end
```

Guardian configuration defines token generation and user lookup from claims.

### Authentication Pipeline

```elixir
# Protected API routes with JWT authentication
defmodule DonationAPI.Router do
  use Phoenix.Router

  pipeline :api do
    plug :accepts, ["json"]
  end

  pipeline :authenticated do
    plug Guardian.Plug.Pipeline,
      module: DonationAPI.Guardian,              # => Guardian module to use
      error_handler: DonationAPI.AuthErrorHandler
                                                 # => Custom error handler for auth failures

    plug Guardian.Plug.VerifyHeader              # => Extract JWT from Authorization header
                                                 # => Format: "Authorization: Bearer <token>"
    plug Guardian.Plug.EnsureAuthenticated       # => Halt if no valid token
                                                 # => Returns 401 if authentication fails
    plug Guardian.Plug.LoadResource              # => Load user from token into conn
                                                 # => Available as Guardian.Plug.current_resource(conn)
  end

  scope "/api/v1", DonationAPI do
    pipe_through [:api, :authenticated]          # => Apply both pipelines

    resources "/donations", DonationController   # => Protected donation routes
    get "/profile", UserController, :profile     # => Protected profile endpoint
  end
end
```

Authentication pipeline validates JWT tokens and loads authenticated user.

### Token Generation - Login

```elixir
# Login controller generating JWT tokens
defmodule DonationAPI.SessionController do
  use Phoenix.Controller
  alias DonationAPI.Guardian

  def create(conn, %{"email" => email, "password" => password}) do
    case Accounts.authenticate(email, password) do
                                                 # => Verify email/password credentials
                                                 # => Type: {:ok, user} | {:error, reason}

      {:ok, user} ->
        {:ok, token, _claims} = Guardian.encode_and_sign(user)
                                                 # => Generate JWT token for user
                                                 # => Type: {:ok, String.t(), map()}

        json(conn, %{
          token: token,                          # => JWT access token
          user: %{
            id: user.id,
            email: user.email,
            name: user.name
          }
        })

      {:error, _reason} ->
        conn
        |> put_status(401)                       # => 401 Unauthorized
        |> json(%{error: "Invalid credentials"})
    end
  end
end
```

Login endpoint validates credentials and returns JWT token for authenticated requests.

### Protected Controller Actions

```elixir
# Controller accessing authenticated user
defmodule DonationAPI.DonationController do
  use Phoenix.Controller
  alias DonationAPI.Guardian.Plug

  def create(conn, params) do
    user = Plug.current_resource(conn)           # => Get authenticated user from conn
                                                 # => Type: %User{}
                                                 # => Loaded by Guardian pipeline

    case Donations.create_donation(user, params) do
      {:ok, donation} ->
        conn
        |> put_status(201)                       # => 201 Created
        |> json(%{data: donation})

      {:error, changeset} ->
        conn
        |> put_status(422)                       # => 422 Unprocessable Entity
        |> json(%{errors: format_errors(changeset)})
    end
  end
end
```

Protected actions access authenticated user from connection.

## Error Responses - Consistent Error Handling

### Standard Error Format

```elixir
# Fallback controller for consistent errors
defmodule DonationAPI.FallbackController do
  use Phoenix.Controller

  def call(conn, {:error, :not_found}) do
    conn
    |> put_status(404)                           # => 404 Not Found
    |> json(%{
      error: %{
        code: "not_found",                       # => Machine-readable error code
        message: "Resource not found",           # => Human-readable message
        details: nil                             # => Optional error details
      }
    })
  end

  def call(conn, {:error, %Ecto.Changeset{} = changeset}) do
    conn
    |> put_status(422)                           # => 422 Unprocessable Entity
    |> json(%{
      error: %{
        code: "validation_error",
        message: "Validation failed",
        details: format_changeset_errors(changeset)
                                                 # => Field-level validation errors
                                                 # => Type: %{field: [error_message]}
      }
    })
  end

  def call(conn, {:error, :unauthorized}) do
    conn
    |> put_status(401)                           # => 401 Unauthorized
    |> json(%{
      error: %{
        code: "unauthorized",
        message: "Authentication required"
      }
    })
  end

  defp format_changeset_errors(changeset) do
    Ecto.Changeset.traverse_errors(changeset, fn {msg, opts} ->
      Enum.reduce(opts, msg, fn {key, value}, acc ->
        String.replace(acc, "%{#{key}}", to_string(value))
      end)
    end)                                         # => Convert changeset errors to map
                                                 # => Type: %{atom() => [String.t()]}
  end
end
```

Fallback controller provides consistent error response format across all endpoints.

### Using Fallback Controller

```elixir
# Controller with action_fallback
defmodule DonationAPI.DonationController do
  use Phoenix.Controller

  action_fallback DonationAPI.FallbackController
                                                 # => Catch non-conn returns
                                                 # => Delegate to fallback controller

  def show(conn, %{"id" => id}) do
    case Donations.get_donation(id) do
      nil -> {:error, :not_found}                # => Return error tuple
                                                 # => Fallback handles response

      donation -> json(conn, %{data: donation})  # => Return conn (no fallback)
    end
  end

  def create(conn, params) do
    case Donations.create_donation(params) do
      {:ok, donation} ->
        conn
        |> put_status(201)
        |> json(%{data: donation})

      {:error, changeset} ->
        {:error, changeset}                      # => Fallback handles validation errors
    end
  end
end
```

Action fallback automatically handles error tuples with consistent responses.

## Pagination and Filtering

### Basic Pagination - Offset-Based

```elixir
# Donations context with pagination
defmodule DonationAPI.Donations do
  import Ecto.Query

  def list_donations(params \\ %{}) do
    limit = Map.get(params, "limit", 20)         # => Default 20 items per page
                                                 # => Type: integer()
    offset = Map.get(params, "offset", 0)        # => Default start from 0
                                                 # => Type: integer()

    donations =
      Donation
      |> limit(^limit)                           # => Limit results
      |> offset(^offset)                         # => Skip offset items
      |> order_by([d], desc: d.inserted_at)      # => Newest first
      |> Repo.all()                              # => Execute query
                                                 # => Type: [%Donation{}]

    total = Repo.aggregate(Donation, :count, :id)
                                                 # => Total count for pagination metadata
                                                 # => Type: integer()

    %{
      data: donations,
      pagination: %{
        limit: limit,
        offset: offset,
        total: total,
        has_more: offset + limit < total         # => Boolean: more pages available
      }
    }
  end
end
```

Offset-based pagination with metadata for page navigation.

### Filtering - Query Parameters

```elixir
# Donations with filtering support
def list_donations(params \\ %{}) do
  limit = Map.get(params, "limit", 20)
  offset = Map.get(params, "offset", 0)

  query =
    Donation
    |> apply_status_filter(params)               # => Filter by status if provided
    |> apply_amount_filter(params)               # => Filter by amount range
    |> apply_date_filter(params)                 # => Filter by date range

  donations =
    query
    |> limit(^limit)
    |> offset(^offset)
    |> order_by([d], desc: d.inserted_at)
    |> Repo.all()

  total = Repo.aggregate(query, :count, :id)     # => Count filtered results

  %{data: donations, pagination: %{limit: limit, offset: offset, total: total}}
end

defp apply_status_filter(query, %{"status" => status}) do
  where(query, [d], d.status == ^status)         # => Filter: WHERE status = ?
                                                 # => Type: Ecto.Query.t()
end
defp apply_status_filter(query, _params), do: query

defp apply_amount_filter(query, %{"min_amount" => min, "max_amount" => max}) do
  query
  |> where([d], d.amount >= ^min)                # => Filter: amount >= min
  |> where([d], d.amount <= ^max)                # => Filter: amount <= max
end
defp apply_amount_filter(query, %{"min_amount" => min}) do
  where(query, [d], d.amount >= ^min)
end
defp apply_amount_filter(query, _params), do: query

defp apply_date_filter(query, %{"from_date" => from_date, "to_date" => to_date}) do
  query
  |> where([d], d.inserted_at >= ^from_date)     # => Date range filter
  |> where([d], d.inserted_at <= ^to_date)
end
defp apply_date_filter(query, _params), do: query
```

Composable filters applied conditionally based on query parameters.

### Cursor-Based Pagination - Efficient Large Datasets

```elixir
# Cursor-based pagination using ID
def list_donations_cursor(params \\ %{}) do
  limit = Map.get(params, "limit", 20)
  after_id = Map.get(params, "after_id")         # => Cursor: last seen ID
                                                 # => Type: integer() | nil

  query =
    case after_id do
      nil ->
        Donation                                 # => First page: no cursor
        |> order_by([d], desc: d.id)

      cursor_id ->
        Donation
        |> where([d], d.id < ^cursor_id)         # => Filter: ID less than cursor
                                                 # => Descending order: fetch older
        |> order_by([d], desc: d.id)
    end

  donations =
    query
    |> limit(^limit + 1)                         # => Fetch limit + 1 to check has_more
    |> Repo.all()

  {results, has_more} =
    case length(donations) > limit do
      true ->
        {Enum.take(donations, limit), true}      # => More results available
                                                 # => Type: {[%Donation{}], true}

      false ->
        {donations, false}                       # => Last page
    end

  next_cursor =
    case {has_more, List.last(results)} do
      {true, %{id: id}} -> id                    # => Next cursor: last item ID
      _ -> nil                                   # => No next cursor (last page)
    end

  %{
    data: results,
    pagination: %{
      limit: limit,
      next_cursor: next_cursor,
      has_more: has_more
    }
  }
end
```

Cursor-based pagination scales better for large datasets (no offset scan).

### Controller with Pagination

```elixir
# Controller exposing paginated donations
defmodule DonationAPI.DonationController do
  use Phoenix.Controller

  def index(conn, params) do
    %{data: donations, pagination: meta} = Donations.list_donations(params)
                                                 # => Context handles pagination logic
                                                 # => Type: %{data: list(), pagination: map()}

    json(conn, %{
      data: donations,
      meta: meta                                 # => Include pagination metadata
                                                 # => Client uses for next page
    })
  end
end

# Example request: GET /api/v1/donations?limit=10&offset=20&status=approved&min_amount=1000
# => Returns 10 donations, skipping first 20, filtered by status and amount
# => Response includes pagination metadata for navigation
```

Controller delegates pagination to context, returns data with metadata.

## Production Patterns

### Complete Donation API Example

```elixir
# Production donation API with all patterns
defmodule DonationAPI.Router do
  use Phoenix.Router

  pipeline :api do
    plug :accepts, ["json"]
  end

  pipeline :authenticated do
    plug Guardian.Plug.Pipeline, module: DonationAPI.Guardian, error_handler: DonationAPI.AuthErrorHandler
    plug Guardian.Plug.VerifyHeader
    plug Guardian.Plug.EnsureAuthenticated
    plug Guardian.Plug.LoadResource
  end

  # Public routes (no auth)
  scope "/api/v1", DonationAPI.V1 do
    pipe_through :api

    post "/sessions", SessionController, :create
                                                 # => POST /api/v1/sessions (login)
                                                 # => Returns JWT token

    resources "/campaigns", CampaignController, only: [:index, :show]
                                                 # => Public campaign listing
  end

  # Protected routes (auth required)
  scope "/api/v1", DonationAPI.V1 do
    pipe_through [:api, :authenticated]

    resources "/donations", DonationController do
      post "/approve", DonationController, :approve
      get "/receipt", DonationController, :receipt
    end

    get "/profile", UserController, :profile
  end
end

# Donation controller with all patterns
defmodule DonationAPI.V1.DonationController do
  use Phoenix.Controller
  alias DonationAPI.Guardian.Plug

  action_fallback DonationAPI.FallbackController

  def index(conn, params) do
    %{data: donations, pagination: meta} = Donations.list_donations(params)
                                                 # => Pagination + filtering

    json(conn, %{data: donations, meta: meta})
  end

  def show(conn, %{"id" => id}) do
    case Donations.get_donation(id) do
      nil -> {:error, :not_found}                # => Fallback handles 404
      donation -> json(conn, %{data: donation})
    end
  end

  def create(conn, params) do
    user = Plug.current_resource(conn)           # => Get authenticated user

    case Donations.create_donation(user, params) do
      {:ok, donation} ->
        conn
        |> put_status(201)
        |> json(%{data: donation})

      {:error, changeset} ->
        {:error, changeset}                      # => Fallback handles validation errors
    end
  end

  def approve(conn, %{"donation_id" => id}) do
    user = Plug.current_resource(conn)

    with {:ok, donation} <- Donations.get_donation(id),
         :ok <- authorize_approval(user, donation),
         {:ok, approved} <- Donations.approve_donation(donation, user) do
                                                 # => with pipeline for multiple validations
                                                 # => Type: {:ok, term()} | {:error, term()}

      json(conn, %{data: approved})
    else
      {:error, :not_found} -> {:error, :not_found}
      {:error, :unauthorized} -> {:error, :unauthorized}
      {:error, changeset} -> {:error, changeset}
    end
  end

  defp authorize_approval(%{role: "admin"}, _donation), do: :ok
  defp authorize_approval(_, _), do: {:error, :unauthorized}
end
```

Production API combining versioning, authentication, pagination, filtering, and consistent error handling.

## When to Use Each Pattern

**RESTful Routing**:

- Standard CRUD operations (donations, users, campaigns)
- Clear resource hierarchy (donations have comments)
- Simple API clients (mobile apps, web frontends)

**API Versioning**:

- Public APIs with external clients (breaking changes need parallel versions)
- Long-term API contracts (v1 support during v2 migration)
- Different feature sets per version (free vs premium API tiers)

**JWT Authentication**:

- Stateless API servers (no session storage required)
- Mobile/SPA clients (token stored client-side)
- Microservices architecture (token includes claims for authorization)

**Offset Pagination**:

- Small to medium datasets (< 10k records)
- Random page access needed (jump to page 5)
- Simple UI requirements (traditional page numbers)

**Cursor Pagination**:

- Large datasets (millions of records)
- Infinite scroll UIs (load more pattern)
- Real-time feeds (new items don't break pagination)

Production systems often combine patterns: JWT auth with cursor pagination for feeds, offset pagination for admin dashboards, versioned endpoints for public API.

## Key Takeaways

1. **RESTful conventions** - Use `resources` for standard CRUD, nested routes for relationships
2. **URL prefix versioning** - `/api/v1/` with separate controllers per version
3. **Guardian for JWT** - Pipeline-based authentication with token generation
4. **Consistent errors** - Fallback controller with standard error format
5. **Pagination choice** - Offset for small datasets, cursor for large/real-time feeds
6. **Filtering composition** - Composable query filters based on parameters
7. **Controller delegation** - Controllers handle HTTP, contexts handle business logic

REST API design balances developer ergonomics (standard conventions) with production requirements (versioning, auth, performance). Phoenix and Guardian provide the primitives; your API design applies them to your domain (donations, campaigns, transactions).
