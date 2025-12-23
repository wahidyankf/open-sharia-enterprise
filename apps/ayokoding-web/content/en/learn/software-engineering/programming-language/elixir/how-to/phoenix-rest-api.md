---
title: "Phoenix REST API Development"
date: 2025-12-21T17:15:00+07:00
draft: false
description: "Build production-ready REST APIs with Phoenix framework covering routing, controllers, JSON rendering, authentication, and error handling."
weight: 1000007
tags: ["elixir", "phoenix", "rest-api", "web-development", "how-to"]
---

**Need to build RESTful APIs with Phoenix?** This guide covers routing, controllers, JSON responses, authentication, versioning, and production patterns.

## Prerequisites

- Phoenix framework basics
- Ecto for data layer
- HTTP/REST concepts
- Completed [Intermediate Tutorial](/en/learn/software-engineering/programming-language/elixir/tutorials/intermediate)

## Problem

Building production-ready REST APIs requires proper HTTP method handling, JSON serialization, authentication, authorization, error handling, versioning, and rate limiting. You need patterns for CRUD operations, nested resources, pagination, filtering, and API documentation.

**Challenges:**

- Structuring RESTful routes and controllers
- Handling JSON requests and responses consistently
- Implementing authentication and authorization
- Validating input and handling errors gracefully
- Managing API versioning and backwards compatibility
- Implementing pagination, filtering, and sorting

## Solution

Use **Phoenix controllers** with JSON rendering, **Plug** for middleware, **Guardian** for authentication, and structured patterns for production-ready REST APIs.

## How It Works

### 1. Basic API Controller

Configure routes:

```elixir
# lib/my_app_web/router.ex
defmodule MyAppWeb.Router do
  use MyAppWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/api", MyAppWeb do
    pipe_through :api

    resources "/users", UserController, except: [:new, :edit]
    resources "/posts", PostController, except: [:new, :edit]
  end
end
```

Controller implementation:

```elixir
# lib/my_app_web/controllers/user_controller.ex
defmodule MyAppWeb.UserController do
  use MyAppWeb, :controller

  alias MyApp.Accounts
  alias MyApp.Accounts.User

  action_fallback MyAppWeb.FallbackController

  def index(conn, _params) do
    users = Accounts.list_users()
    render(conn, :index, users: users)
  end

  def show(conn, %{"id" => id}) do
    user = Accounts.get_user!(id)
    render(conn, :show, user: user)
  end

  def create(conn, %{"user" => user_params}) do
    with {:ok, %User{} = user} <- Accounts.create_user(user_params) do
      conn
      |> put_status(:created)
      |> put_resp_header("location", ~p"/api/users/#{user}")
      |> render(:show, user: user)
    end
  end

  def update(conn, %{"id" => id, "user" => user_params}) do
    user = Accounts.get_user!(id)

    with {:ok, %User{} = user} <- Accounts.update_user(user, user_params) do
      render(conn, :show, user: user)
    end
  end

  def delete(conn, %{"id" => id}) do
    user = Accounts.get_user!(id)

    with {:ok, %User{}} <- Accounts.delete_user(user) do
      send_resp(conn, :no_content, "")
    end
  end
end
```

JSON view:

```elixir
# lib/my_app_web/controllers/user_json.ex
defmodule MyAppWeb.UserJSON do
  alias MyApp.Accounts.User

  @doc """
  Renders a list of users.
  """
  def index(%{users: users}) do
    %{data: for(user <- users, do: data(user))}
  end

  @doc """
  Renders a single user.
  """
  def show(%{user: user}) do
    %{data: data(user)}
  end

  defp data(%User{} = user) do
    %{
      id: user.id,
      name: user.name,
      email: user.email,
      inserted_at: user.inserted_at,
      updated_at: user.updated_at
    }
  end
end
```

### 2. Error Handling with Fallback Controller

```elixir
# lib/my_app_web/controllers/fallback_controller.ex
defmodule MyAppWeb.FallbackController do
  use MyAppWeb, :controller

  def call(conn, {:error, %Ecto.Changeset{} = changeset}) do
    conn
    |> put_status(:unprocessable_entity)
    |> put_view(json: MyAppWeb.ChangesetJSON)
    |> render(:error, changeset: changeset)
  end

  def call(conn, {:error, :not_found}) do
    conn
    |> put_status(:not_found)
    |> put_view(json: MyAppWeb.ErrorJSON)
    |> render(:"404")
  end

  def call(conn, {:error, :unauthorized}) do
    conn
    |> put_status(:unauthorized)
    |> put_view(json: MyAppWeb.ErrorJSON)
    |> render(:"401")
  end
end
```

Changeset error renderer:

```elixir
# lib/my_app_web/controllers/changeset_json.ex
defmodule MyAppWeb.ChangesetJSON do
  @doc """
  Renders changeset errors.
  """
  def error(%{changeset: changeset}) do
    %{errors: translate_errors(changeset)}
  end

  defp translate_errors(changeset) do
    Ecto.Changeset.traverse_errors(changeset, fn {msg, opts} ->
      Regex.replace(~r"%{(\w+)}", msg, fn _, key ->
        opts |> Keyword.get(String.to_existing_atom(key), key) |> to_string()
      end)
    end)
  end
end
```

### 3. Authentication with Guardian

Add dependency:

```elixir
# mix.exs
{:guardian, "~> 2.3"}
```

Guardian implementation:

```elixir
# lib/my_app/guardian.ex
defmodule MyApp.Guardian do
  use Guardian, otp_app: :my_app

  alias MyApp.Accounts

  def subject_for_token(%{id: id}, _claims) do
    {:ok, to_string(id)}
  end

  def resource_from_claims(%{"sub" => id}) do
    case Accounts.get_user(id) do
      nil -> {:error, :resource_not_found}
      user -> {:ok, user}
    end
  end
end
```

Authentication controller:

```elixir
# lib/my_app_web/controllers/auth_controller.ex
defmodule MyAppWeb.AuthController do
  use MyAppWeb, :controller

  alias MyApp.Accounts
  alias MyApp.Guardian

  action_fallback MyAppWeb.FallbackController

  def sign_in(conn, %{"email" => email, "password" => password}) do
    case Accounts.authenticate_user(email, password) do
      {:ok, user} ->
        {:ok, token, _claims} = Guardian.encode_and_sign(user)

        conn
        |> put_status(:ok)
        |> render(:token, token: token, user: user)

      {:error, :unauthorized} ->
        {:error, :unauthorized}
    end
  end

  def refresh(conn, %{"token" => token}) do
    case Guardian.refresh(token) do
      {:ok, _old_token, {new_token, _new_claims}} ->
        conn
        |> put_status(:ok)
        |> render(:token, token: new_token)

      {:error, _reason} ->
        {:error, :unauthorized}
    end
  end

  def sign_out(conn, _params) do
    token = Guardian.Plug.current_token(conn)
    Guardian.revoke(token)

    conn
    |> put_status(:no_content)
    |> send_resp(:no_content, "")
  end
end
```

Authentication pipeline:

```elixir
# lib/my_app_web/router.ex
pipeline :api_auth do
  plug MyAppWeb.AuthPipeline
end

defmodule MyAppWeb.AuthPipeline do
  use Guardian.Plug.Pipeline,
    otp_app: :my_app,
    module: MyApp.Guardian,
    error_handler: MyAppWeb.AuthErrorHandler

  plug Guardian.Plug.VerifyHeader
  plug Guardian.Plug.EnsureAuthenticated
  plug Guardian.Plug.LoadResource
end
```

### 4. Pagination

```elixir
# lib/my_app/repo.ex
defmodule MyApp.Repo do
  use Ecto.Repo,
    otp_app: :my_app,
    adapter: Ecto.Adapters.Postgres

  def paginate(query, page, page_size) do
    offset = (page - 1) * page_size

    results = query
    |> limit(^page_size)
    |> offset(^offset)
    |> all()

    total = aggregate(query, :count)

    %{
      data: results,
      page: page,
      page_size: page_size,
      total: total,
      total_pages: ceil(total / page_size)
    }
  end
end
```

Controller with pagination:

```elixir
def index(conn, params) do
  page = String.to_integer(params["page"] || "1")
  page_size = String.to_integer(params["page_size"] || "20")

  result = User
  |> MyApp.Repo.paginate(page, page_size)

  conn
  |> put_resp_header("x-total", to_string(result.total))
  |> put_resp_header("x-page", to_string(result.page))
  |> put_resp_header("x-page-size", to_string(result.page_size))
  |> put_resp_header("x-total-pages", to_string(result.total_pages))
  |> render(:index, users: result.data)
end
```

### 5. Filtering and Sorting

```elixir
defmodule MyApp.Accounts do
  import Ecto.Query

  def list_users(filters \\ %{}) do
    User
    |> apply_filters(filters)
    |> apply_sorting(filters)
    |> Repo.all()
  end

  defp apply_filters(query, filters) do
    query
    |> filter_by_name(filters["name"])
    |> filter_by_email(filters["email"])
    |> filter_by_status(filters["status"])
  end

  defp filter_by_name(query, nil), do: query
  defp filter_by_name(query, name) do
    from u in query, where: ilike(u.name, ^"%#{name}%")
  end

  defp filter_by_email(query, nil), do: query
  defp filter_by_email(query, email) do
    from u in query, where: u.email == ^email
  end

  defp filter_by_status(query, nil), do: query
  defp filter_by_status(query, status) do
    from u in query, where: u.status == ^status
  end

  defp apply_sorting(query, %{"sort" => sort_field}) do
    case sort_field do
      "name" -> from u in query, order_by: [asc: u.name]
      "email" -> from u in query, order_by: [asc: u.email]
      "created" -> from u in query, order_by: [desc: u.inserted_at]
      "-name" -> from u in query, order_by: [desc: u.name]
      "-email" -> from u in query, order_by: [desc: u.email]
      "-created" -> from u in query, order_by: [asc: u.inserted_at]
      _ -> query
    end
  end
  defp apply_sorting(query, _), do: query
end
```

### 6. Nested Resources

```elixir
# Router
resources "/users", UserController do
  resources "/posts", PostController
end

# Controller
defmodule MyAppWeb.PostController do
  use MyAppWeb, :controller

  def index(conn, %{"user_id" => user_id}) do
    user = Accounts.get_user!(user_id)
    posts = Blog.list_posts_for_user(user)

    render(conn, :index, posts: posts)
  end

  def create(conn, %{"user_id" => user_id, "post" => post_params}) do
    user = Accounts.get_user!(user_id)

    with {:ok, post} <- Blog.create_post(user, post_params) do
      conn
      |> put_status(:created)
      |> render(:show, post: post)
    end
  end
end
```

### 7. API Versioning

URL versioning:

```elixir
# Router
scope "/api/v1", MyAppWeb.V1, as: :v1 do
  pipe_through :api

  resources "/users", UserController
end

scope "/api/v2", MyAppWeb.V2, as: :v2 do
  pipe_through :api

  resources "/users", UserController
end
```

Header versioning:

```elixir
# Plug
defmodule MyAppWeb.APIVersion do
  import Plug.Conn

  def init(opts), do: opts

  def call(conn, _opts) do
    version = get_req_header(conn, "accept")
    |> List.first("")
    |> parse_version()

    assign(conn, :api_version, version)
  end

  defp parse_version(header) do
    case Regex.run(~r/application\/vnd\.myapp\.v(\d+)\+json/, header) do
      [_, version] -> String.to_integer(version)
      _ -> 1  # Default to v1
    end
  end
end
```

## Variations

### Rate Limiting with Hammer

```elixir
# mix.exs
{:hammer, "~> 6.1"}

# Plug
defmodule MyAppWeb.RateLimiter do
  import Plug.Conn

  def init(opts), do: opts

  def call(conn, opts) do
    rate_limit = Keyword.get(opts, :rate_limit, 100)
    time_window = Keyword.get(opts, :time_window, 60_000)

    case check_rate(conn, rate_limit, time_window) do
      {:allow, count} ->
        conn
        |> put_resp_header("x-ratelimit-limit", to_string(rate_limit))
        |> put_resp_header("x-ratelimit-remaining", to_string(rate_limit - count))

      {:deny, _count} ->
        conn
        |> put_status(:too_many_requests)
        |> Phoenix.Controller.put_view(json: MyAppWeb.ErrorJSON)
        |> Phoenix.Controller.render(:"429")
        |> halt()
    end
  end

  defp check_rate(conn, limit, window) do
    key = get_rate_limit_key(conn)
    Hammer.check_rate(key, window, limit)
  end

  defp get_rate_limit_key(conn) do
    # Use authenticated user ID or IP address
    case Guardian.Plug.current_resource(conn) do
      %{id: user_id} -> "user:#{user_id}"
      nil -> "ip:#{conn.remote_ip |> :inet.ntoa() |> to_string()}"
    end
  end
end

# Apply to routes
pipeline :api_limited do
  plug :api
  plug MyAppWeb.RateLimiter, rate_limit: 100, time_window: 60_000
end
```

### CORS Support

```elixir
# mix.exs
{:cors_plug, "~> 3.0"}

# Router
pipeline :api do
  plug CORSPlug,
    origin: ["http://localhost:3000", "https://example.com"],
    methods: ["GET", "POST", "PUT", "PATCH", "DELETE", "OPTIONS"],
    headers: ["Authorization", "Content-Type", "Accept"],
    expose: ["x-total", "x-page", "x-page-size"],
    max_age: 86400
end
```

### API Documentation with OpenAPI

```elixir
# mix.exs
{:open_api_spex, "~> 3.16"}

# Schema
defmodule MyAppWeb.Schemas.User do
  require OpenApiSpex
  alias OpenApiSpex.Schema

  OpenApiSpex.schema(%{
    title: "User",
    description: "A user of the application",
    type: :object,
    properties: %{
      id: %Schema{type: :integer, description: "User ID"},
      name: %Schema{type: :string, description: "User's full name"},
      email: %Schema{type: :string, format: :email, description: "User's email"}
    },
    required: [:name, :email],
    example: %{
      "id" => 123,
      "name" => "Alice Smith",
      "email" => "alice@example.com"
    }
  })
end

# Controller with specs
defmodule MyAppWeb.UserController do
  use MyAppWeb, :controller
  use OpenApiSpex.ControllerSpecs

  alias MyAppWeb.Schemas

  tags ["users"]

  operation :index,
    summary: "List users",
    parameters: [
      page: [in: :query, type: :integer, description: "Page number"],
      page_size: [in: :query, type: :integer, description: "Items per page"]
    ],
    responses: [
      ok: {"Users", "application/json", Schemas.UsersResponse}
    ]

  def index(conn, params) do
    # Implementation
  end
end
```

## Advanced Patterns

### 1. Request/Response Logging

```elixir
defmodule MyAppWeb.RequestLogger do
  require Logger

  def init(opts), do: opts

  def call(conn, _opts) do
    start_time = System.monotonic_time()

    Plug.Conn.register_before_send(conn, fn conn ->
      duration = System.monotonic_time() - start_time
      duration_ms = System.convert_time_unit(duration, :native, :millisecond)

      Logger.info([
        "method=", conn.method,
        " path=", conn.request_path,
        " status=", to_string(conn.status),
        " duration=", to_string(duration_ms), "ms"
      ])

      conn
    end)
  end
end
```

### 2. Field Selection (Sparse Fieldsets)

```elixir
defmodule MyAppWeb.UserJSON do
  def show(%{user: user, fields: fields}) do
    %{data: select_fields(data(user), fields)}
  end

  defp select_fields(data, nil), do: data
  defp select_fields(data, fields) when is_binary(fields) do
    field_list = String.split(fields, ",") |> Enum.map(&String.to_atom/1)
    Map.take(data, field_list)
  end
end

# Controller
def show(conn, %{"id" => id} = params) do
  user = Accounts.get_user!(id)
  fields = params["fields"]

  render(conn, :show, user: user, fields: fields)
end

# Usage: GET /api/users/1?fields=id,name,email
```

### 3. Batch Operations

```elixir
def batch_create(conn, %{"users" => users_params}) do
  results = Enum.map(users_params, fn user_params ->
    case Accounts.create_user(user_params) do
      {:ok, user} -> {:ok, user}
      {:error, changeset} -> {:error, changeset}
    end
  end)

  {successes, failures} = Enum.split_with(results, fn
    {:ok, _} -> true
    {:error, _} -> false
  end)

  conn
  |> put_status(:multi_status)
  |> render(:batch, successes: successes, failures: failures)
end
```

## Use Cases

**Public APIs:**

- Third-party integrations
- Mobile app backends
- Partner APIs
- Webhooks

**Internal APIs:**

- Microservices communication
- Frontend/backend separation
- Service-to-service calls
- Admin dashboards

**Production Features:**

- Authentication and authorization
- Rate limiting and throttling
- Versioning and deprecation
- Monitoring and logging

## Best Practices

1. **Use proper HTTP status codes:**
   - 200 OK, 201 Created, 204 No Content
   - 400 Bad Request, 401 Unauthorized, 403 Forbidden, 404 Not Found
   - 422 Unprocessable Entity, 429 Too Many Requests
   - 500 Internal Server Error

2. **Version your API from the start:**
   Support multiple versions simultaneously

3. **Paginate large collections:**
   Default to reasonable page sizes (20-100 items)

4. **Use consistent error formats:**

   ```json
   {
     "errors": {
       "email": ["has already been taken"],
       "name": ["can't be blank"]
     }
   }
   ```

5. **Document your API:**
   Use OpenAPI/Swagger for interactive documentation

6. **Implement HATEOAS links:**
   ```json
   {
     "data": {...},
     "links": {
       "self": "/api/users/1",
       "posts": "/api/users/1/posts"
     }
   }
   ```

## Common Pitfalls

1. **Not using action_fallback:** Leads to repetitive error handling
2. **Missing pagination:** Returning unbounded collections
3. **Inconsistent JSON structure:** Mix of formats
4. **No rate limiting:** Vulnerable to abuse
5. **Exposing internal errors:** Leaking implementation details
6. **Not versioning:** Breaking changes affect all clients

## Troubleshooting

### CORS Issues

```elixir
# Ensure OPTIONS requests are handled
plug CORSPlug, origin: ["*"]
```

### Authentication Not Working

```bash
# Check Guardian configuration
config :my_app, MyApp.Guardian,
  issuer: "my_app",
  secret_key: "secret"
```

### JSON Encoding Errors

```elixir
# Implement Jason.Encoder for custom types
defimpl Jason.Encoder, for: MyApp.CustomType do
  def encode(value, opts) do
    Jason.Encode.map(%{id: value.id, name: value.name}, opts)
  end
end
```

## Related Resources

- [Ecto Guide](/en/learn/software-engineering/programming-language/elixir/how-to/ecto)
- [Testing Guide](/en/learn/software-engineering/programming-language/elixir/how-to/testing)
- [Authentication Guide](/en/learn/software-engineering/programming-language/elixir/how-to/authentication)
- [Phoenix Documentation](https://hexdocs.pm/phoenix/)
- [Guardian Documentation](https://hexdocs.pm/guardian/)
- [OpenAPI Specification](https://swagger.io/specification/)
