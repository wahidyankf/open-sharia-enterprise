---
title: "Authentication Authorization"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000021
description: "Authentication and authorization patterns for production Elixir web applications"
tags: ["elixir", "authentication", "authorization", "security", "guardian", "pow", "bodyguard"]
prev: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/graphql-absinthe"
next: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/testing-strategies"
---

**Building secure Elixir web applications?** This guide teaches authentication and authorization through the OTP-First progression, starting with manual session management to understand security challenges before introducing production libraries like Guardian, Pow, and Bodyguard.

## Why Authentication and Authorization Matter

Every production web application needs secure user access control:

- **Multi-tenant SaaS** - User accounts, organization boundaries, role-based access
- **Financial systems** - Transaction authorization, admin privileges, audit trails
- **Healthcare platforms** - Patient data access, provider permissions, HIPAA compliance
- **E-commerce** - Customer accounts, seller dashboards, admin operations

Elixir provides two approaches:

1. **Manual session/token management** - Direct control with Plug (maximum flexibility)
2. **Production libraries** - Guardian (JWT), Pow (session-based), Bodyguard (authorization)

**Our approach**: Implement manual authentication to understand security patterns, limitations, and CSRF protection, then see how production libraries provide battle-tested solutions.

## OTP Primitives - Manual Authentication

### Basic Session-Based Authentication

Let's build authentication using Plug's session store:

```elixir
# Manual session authentication with Plug
defmodule MyAppWeb.Auth do
  import Plug.Conn
  # => Imports: put_session, get_session, configure_session
  import Phoenix.Controller
  # => Imports: redirect, put_flash

  # => Plug to load current user from session
  def load_current_user(conn, _opts) do
    user_id = get_session(conn, :user_id)
    # => Retrieves user_id from encrypted session cookie
    # => Returns: user_id (integer) or nil

    case user_id do
      nil ->
        # => No user logged in
        assign(conn, :current_user, nil)
        # => Sets conn.assigns.current_user = nil

      user_id ->
        # => User ID found in session
        user = MyApp.Accounts.get_user(user_id)
        # => Load user from database
        # => user: %User{} struct or nil
        assign(conn, :current_user, user)
        # => Sets conn.assigns.current_user = user
    end
  end

  # => Plug to require authentication
  def require_authenticated(conn, _opts) do
    if conn.assigns[:current_user] do
      # => User is authenticated
      conn
      # => Continue pipeline
    else
      # => User not authenticated
      conn
      |> put_flash(:error, "You must be logged in")
      |> redirect(to: "/login")
      |> halt()
      # => Stops pipeline, returns redirect response
    end
  end

  # => Login function
  def login(conn, user) do
    conn
    |> put_session(:user_id, user.id)
    # => Stores user ID in encrypted session cookie
    # => Session cookie: HTTPOnly, Secure (HTTPS only)
    |> configure_session(renew: true)
    # => Generates new session ID (prevents session fixation)
  end

  # => Logout function
  def logout(conn) do
    conn
    |> configure_session(drop: true)
    # => Drops entire session
    # => Clears session cookie
  end

  # => Verify password (bcrypt)
  def verify_password(user, password) do
    Bcrypt.verify_pass(password, user.password_hash)
    # => Compares password against bcrypt hash
    # => Returns: boolean
  end
end
```

**Controller implementation**:

```elixir
defmodule MyAppWeb.SessionController do
  use MyAppWeb, :controller
  # => Imports controller functions
  alias MyApp.Accounts
  # => User management module
  alias MyAppWeb.Auth
  # => Authentication helpers

  # => Login form
  def new(conn, _params) do
    render(conn, :new)
    # => Renders login page
  end

  # => Login submission
  def create(conn, %{"email" => email, "password" => password}) do
    case Accounts.get_user_by_email(email) do
      nil ->
        # => User not found
        conn
        |> put_flash(:error, "Invalid email or password")
        |> render(:new)
        # => Re-render login form with error

      user ->
        # => User found
        if Auth.verify_password(user, password) do
          # => Password correct
          conn
          |> Auth.login(user)
          # => Set session
          |> put_flash(:info, "Welcome back!")
          |> redirect(to: "/dashboard")
          # => Redirect to dashboard
        else
          # => Password incorrect
          conn
          |> put_flash(:error, "Invalid email or password")
          |> render(:new)
          # => Re-render login form
        end
    end
  end

  # => Logout
  def delete(conn, _params) do
    conn
    |> Auth.logout()
    # => Clear session
    |> put_flash(:info, "Logged out successfully")
    |> redirect(to: "/")
    # => Redirect to home page
  end
end
```

**Router setup**:

```elixir
defmodule MyAppWeb.Router do
  use MyAppWeb, :router
  import MyAppWeb.Auth
  # => Import authentication plugs

  pipeline :browser do
    plug :accepts, ["html"]
    # => Accept HTML requests
    plug :fetch_session
    # => Load session from cookie
    plug :load_current_user
    # => Load user from session into conn.assigns
    plug :fetch_flash
    # => Load flash messages
    plug :protect_from_forgery
    # => CSRF protection
    plug :put_secure_browser_headers
    # => Security headers
  end

  scope "/", MyAppWeb do
    pipe_through :browser
    # => All routes use browser pipeline

    get "/", PageController, :index
    # => Home page (no auth required)

    get "/login", SessionController, :new
    # => Login form
    post "/login", SessionController, :create
    # => Login submission
    delete "/logout", SessionController, :delete
    # => Logout
  end

  scope "/", MyAppWeb do
    pipe_through [:browser, :require_authenticated]
    # => Requires authentication

    get "/dashboard", DashboardController, :index
    # => Protected: Dashboard page
    get "/profile", ProfileController, :show
    # => Protected: User profile
  end
end
```

### Manual Token-Based Authentication (JWT)

For API authentication, manual JWT implementation:

```elixir
# Manual JWT authentication
defmodule MyAppWeb.JWTAuth do
  # => JOKEN library for JWT
  use Joken.Config
  # => Provides token generation and verification

  @secret System.get_env("JWT_SECRET") || "default_secret"
  # => Secret key for signing JWT
  # => Production: Use environment variable

  # => Generate JWT token for user
  def generate_token(user) do
    claims = %{
      "sub" => to_string(user.id),
      # => Subject: User ID
      "email" => user.email,
      # => User email
      "exp" => Joken.current_time() + (60 * 60 * 24 * 7)
      # => Expiration: 7 days from now
      # => Unix timestamp
    }

    signer = Joken.Signer.create("HS256", @secret)
    # => HMAC SHA-256 signer
    # => Secret-based signing

    case Joken.generate_and_sign(%{}, claims, signer) do
      {:ok, token, _claims} ->
        # => Token generated successfully
        {:ok, token}
        # => Returns: JWT string

      {:error, reason} ->
        # => Token generation failed
        {:error, reason}
    end
  end

  # => Verify JWT token
  def verify_token(token) do
    signer = Joken.Signer.create("HS256", @secret)
    # => Same signer used for generation

    case Joken.verify_and_validate(%{}, token, signer) do
      {:ok, claims} ->
        # => Token valid, claims extracted
        {:ok, claims}
        # => Returns: %{"sub" => user_id, "email" => ...}

      {:error, reason} ->
        # => Token invalid or expired
        {:error, reason}
    end
  end

  # => Plug to authenticate API requests
  def authenticate_api(conn, _opts) do
    case get_req_header(conn, "authorization") do
      ["Bearer " <> token] ->
        # => Authorization header present
        # => Format: "Bearer <token>"
        case verify_token(token) do
          {:ok, claims} ->
            # => Token valid
            user_id = String.to_integer(claims["sub"])
            # => Extract user ID from claims
            user = MyApp.Accounts.get_user(user_id)
            # => Load user from database

            conn
            |> assign(:current_user, user)
            # => Set current user
            # => Continue pipeline

          {:error, _reason} ->
            # => Token invalid
            conn
            |> put_status(:unauthorized)
            |> json(%{error: "Invalid token"})
            |> halt()
            # => Return 401 Unauthorized
        end

      _ ->
        # => No Authorization header
        conn
        |> put_status(:unauthorized)
        |> json(%{error: "Missing authorization header"})
        |> halt()
    end
  end
end
```

**API controller usage**:

```elixir
defmodule MyAppWeb.API.SessionController do
  use MyAppWeb, :controller
  alias MyApp.Accounts
  alias MyAppWeb.JWTAuth

  # => Login endpoint
  def create(conn, %{"email" => email, "password" => password}) do
    case Accounts.get_user_by_email(email) do
      nil ->
        # => User not found
        conn
        |> put_status(:unauthorized)
        |> json(%{error: "Invalid credentials"})

      user ->
        # => User found
        if Auth.verify_password(user, password) do
          # => Password correct
          {:ok, token} = JWTAuth.generate_token(user)
          # => Generate JWT token

          conn
          |> json(%{token: token, user: user_json(user)})
          # => Return token + user data
          # => Status: 200 OK
        else
          # => Password incorrect
          conn
          |> put_status(:unauthorized)
          |> json(%{error: "Invalid credentials"})
        end
    end
  end

  defp user_json(user) do
    %{
      id: user.id,
      email: user.email,
      name: user.name
    }
  end
end
```

**API router**:

```elixir
scope "/api", MyAppWeb.API do
  pipe_through :api
  # => JSON API pipeline

  post "/login", SessionController, :create
  # => Login endpoint (no auth required)
end

scope "/api", MyAppWeb.API do
  pipe_through [:api, MyAppWeb.JWTAuth, :authenticate_api]
  # => Requires JWT authentication

  get "/profile", ProfileController, :show
  # => Protected: User profile
  resources "/posts", PostController
  # => Protected: CRUD operations
end
```

### Limitations of Manual Authentication

**1. No Security Pattern Standards**

```elixir
# Missing critical security features:
# - No password reset flow
# - No email confirmation
# - No account locking after failed attempts
# - No session timeout handling
# - No "remember me" functionality
# - No social login (OAuth) integration
```

**2. Manual CSRF Protection**

```elixir
# Plug provides basic CSRF, but manual handling needed for:
# - AJAX requests with CSRF tokens
# - API endpoints (CSRF exempt)
# - Token refresh logic
# - Multi-tab session coordination

def create(conn, params) do
  # => Must manually verify CSRF token for state-changing operations
  # => Phoenix provides plug :protect_from_forgery
  # => But custom flows need manual handling
end
```

**3. No Role-Based Access Control**

```elixir
# Manual authorization requires repetitive checks:
def update(conn, %{"id" => post_id} = params) do
  post = Blog.get_post(post_id)
  current_user = conn.assigns.current_user

  cond do
    post.user_id == current_user.id ->
      # => Owner can update
      update_post(post, params)

    current_user.role == :admin ->
      # => Admin can update
      update_post(post, params)

    true ->
      # => Unauthorized
      conn
      |> put_status(:forbidden)
      |> json(%{error: "Not authorized"})
  end
  # => Repetitive authorization logic in every controller action
end
```

**4. No Token Refresh Mechanism**

```elixir
# JWT tokens are stateless:
# - No built-in token refresh
# - Can't revoke tokens (need blacklist)
# - Must implement refresh token flow manually
# - Token expiration handling scattered across codebase
```

**5. Password Management Complexity**

```elixir
# Manual password handling risks:
# - Must choose bcrypt rounds (balance security vs performance)
# - Password reset requires secure token generation
# - Password strength validation scattered
# - No unified password policy enforcement
```

## Guardian - Production JWT Authentication

Guardian provides battle-tested JWT authentication with token refresh, revocation, and flexible claims:

**Installation**:

```elixir
# mix.exs
defp deps do
  [
    {:guardian, "~> 2.3"},
    # => JWT authentication library
    {:bcrypt_elixir, "~> 3.0"}
    # => Password hashing
  ]
end
```

**Guardian implementation module**:

```elixir
defmodule MyApp.Guardian do
  use Guardian, otp_app: :my_app
  # => Guardian behavior
  # => Reads config from :my_app application

  alias MyApp.Accounts
  # => User management

  # => Encode user into JWT subject claim
  def subject_for_token(%{id: id}, _claims) do
    # => subject: User ID
    {:ok, to_string(id)}
    # => Returns: user_id string
  end

  def subject_for_token(_, _) do
    {:error, :no_subject}
    # => Invalid resource
  end

  # => Decode JWT subject claim into user resource
  def resource_from_claims(%{"sub" => id}) do
    # => Extract user ID from subject
    case Accounts.get_user(id) do
      nil ->
        {:error, :user_not_found}
        # => User deleted after token issued

      user ->
        {:ok, user}
        # => Returns: %User{} struct
    end
  end

  def resource_from_claims(_claims) do
    {:error, :invalid_claims}
  end
end
```

**Configuration**:

```elixir
# config/config.exs
config :my_app, MyApp.Guardian,
  issuer: "my_app",
  # => JWT issuer claim
  secret_key: System.get_env("GUARDIAN_SECRET_KEY"),
  # => Secret for signing JWT
  # => Generate with: mix guardian.gen.secret
  ttl: {7, :days},
  # => Token expiration: 7 days
  verify_issuer: true
  # => Verify issuer claim on decode
```

**Controller with Guardian**:

```elixir
defmodule MyAppWeb.API.SessionController do
  use MyAppWeb, :controller
  alias MyApp.Accounts
  alias MyApp.Guardian

  # => Login endpoint
  def create(conn, %{"email" => email, "password" => password}) do
    case Accounts.authenticate_user(email, password) do
      {:ok, user} ->
        # => Authentication successful
        {:ok, token, _claims} = Guardian.encode_and_sign(user)
        # => Generate JWT token
        # => token: JWT string
        # => _claims: Map of claims

        conn
        |> json(%{
          token: token,
          # => Access token
          user: user_json(user)
          # => User data
        })

      {:error, :invalid_credentials} ->
        # => Authentication failed
        conn
        |> put_status(:unauthorized)
        |> json(%{error: "Invalid credentials"})
    end
  end

  # => Refresh token endpoint
  def refresh(conn, %{"token" => token}) do
    case Guardian.exchange(token, "access", "access") do
      {:ok, _old, {new_token, _new_claims}} ->
        # => Token refreshed successfully
        # => _old: Old token (now invalidated)
        # => new_token: Fresh token with extended expiry

        conn
        |> json(%{token: new_token})

      {:error, reason} ->
        # => Token refresh failed
        conn
        |> put_status(:unauthorized)
        |> json(%{error: "Token refresh failed: #{reason}"})
    end
  end

  # => Logout endpoint (optional: token revocation)
  def delete(conn, _params) do
    # => Get token from Guardian plug
    token = Guardian.Plug.current_token(conn)
    # => token: JWT string from Authorization header

    # => Revoke token (requires Guardian.DB)
    Guardian.revoke(token)
    # => Adds token to revocation blacklist

    conn
    |> json(%{message: "Logged out successfully"})
  end

  defp user_json(user) do
    %{id: user.id, email: user.email, name: user.name}
  end
end
```

**Guardian pipeline (authentication plug)**:

```elixir
defmodule MyAppWeb.AuthPipeline do
  use Guardian.Plug.Pipeline,
    otp_app: :my_app,
    # => Application name
    module: MyApp.Guardian,
    # => Guardian implementation module
    error_handler: MyAppWeb.AuthErrorHandler
    # => Custom error handling

  # => Verify JWT from Authorization header
  plug Guardian.Plug.VerifyHeader, scheme: "Bearer"
  # => Extracts token from "Authorization: Bearer <token>"
  # => Decodes and verifies signature

  # => Load user resource from verified claims
  plug Guardian.Plug.LoadResource, allow_blank: true
  # => Calls Guardian.resource_from_claims/1
  # => Sets Guardian.Plug.current_resource(conn)
  # => allow_blank: Allow requests without token
end

defmodule MyAppWeb.AuthErrorHandler do
  @behaviour Guardian.Plug.ErrorHandler
  # => Implements error handling behavior

  import Plug.Conn
  import Phoenix.Controller

  # => Handle authentication errors
  @impl Guardian.Plug.ErrorHandler
  def auth_error(conn, {type, _reason}, _opts) do
    # => type: :invalid_token, :unauthenticated, etc.
    conn
    |> put_status(:unauthorized)
    |> json(%{error: to_string(type)})
    # => Returns JSON error response
  end
end
```

**Require authentication plug**:

```elixir
defmodule MyAppWeb.RequireAuth do
  import Plug.Conn
  import Phoenix.Controller

  def init(opts), do: opts

  def call(conn, _opts) do
    case Guardian.Plug.current_resource(conn) do
      nil ->
        # => No user loaded (unauthenticated)
        conn
        |> put_status(:unauthorized)
        |> json(%{error: "Authentication required"})
        |> halt()

      _user ->
        # => User authenticated
        conn
        # => Continue pipeline
    end
  end
end
```

**Router with Guardian**:

```elixir
scope "/api", MyAppWeb.API do
  pipe_through :api
  # => JSON API pipeline

  post "/login", SessionController, :create
  # => Login (no auth required)
end

scope "/api", MyAppWeb.API do
  pipe_through [:api, MyAppWeb.AuthPipeline, MyAppWeb.RequireAuth]
  # => Requires JWT authentication

  post "/logout", SessionController, :delete
  # => Logout (revoke token)
  post "/refresh", SessionController, :refresh
  # => Refresh token
  get "/profile", ProfileController, :show
  # => Protected endpoints
end
```

## Pow - Session-Based Authentication

Pow provides complete session-based authentication with email confirmation, password reset, and extensible modules:

**Installation**:

```elixir
# mix.exs
defp deps do
  [
    {:pow, "~> 1.0"},
    # => Session-based authentication
    {:pow_assent, "~> 0.4"}
    # => Optional: OAuth integration
  ]
end
```

**Configuration**:

```elixir
# config/config.exs
config :my_app, :pow,
  user: MyApp.Users.User,
  # => User schema module
  repo: MyApp.Repo,
  # => Ecto repo
  web_module: MyAppWeb
  # => Phoenix web module
```

**User schema**:

```elixir
defmodule MyApp.Users.User do
  use Ecto.Schema
  use Pow.Ecto.Schema
  # => Adds Pow fields and changeset functions

  schema "users" do
    # => Pow adds:
    # => - email (unique)
    # => - password_hash
    pow_user_fields()
    # => Macro injects required fields

    # => Custom fields
    field :name, :string
    field :role, :string, default: "user"
    # => role: "user", "admin", "moderator"

    timestamps()
  end

  # => Pow changeset
  def changeset(user_or_changeset, attrs) do
    user_or_changeset
    |> pow_changeset(attrs)
    # => Pow validation (email, password)
    |> Ecto.Changeset.cast(attrs, [:name, :role])
    # => Custom fields
    |> Ecto.Changeset.validate_required([:name])
  end
end
```

**Migration**:

```elixir
defmodule MyApp.Repo.Migrations.CreateUsers do
  use Ecto.Migration

  def change do
    create table(:users) do
      # => Pow fields
      add :email, :string, null: false
      add :password_hash, :string

      # => Custom fields
      add :name, :string
      add :role, :string, default: "user"

      timestamps()
    end

    create unique_index(:users, [:email])
    # => Email uniqueness constraint
  end
end
```

**Router with Pow**:

```elixir
defmodule MyAppWeb.Router do
  use MyAppWeb, :router
  use Pow.Phoenix.Router
  # => Imports Pow routing functions

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :protected do
    plug Pow.Plug.RequireAuthenticated,
      error_handler: Pow.Phoenix.PlugErrorHandler
    # => Requires authenticated user
    # => Redirects to login if not authenticated
  end

  scope "/" do
    pipe_through :browser

    pow_routes()
    # => Generates routes:
    # => GET /registration/new - Signup form
    # => POST /registration - Create account
    # => GET /session/new - Login form
    # => POST /session - Login
    # => DELETE /session - Logout
  end

  scope "/", MyAppWeb do
    pipe_through :browser

    get "/", PageController, :index
    # => Home page (public)
  end

  scope "/", MyAppWeb do
    pipe_through [:browser, :protected]
    # => Requires authentication

    get "/dashboard", DashboardController, :index
    # => Protected: Dashboard
    resources "/posts", PostController
    # => Protected: CRUD operations
  end
end
```

**Access current user in controller**:

```elixir
defmodule MyAppWeb.DashboardController do
  use MyAppWeb, :controller

  def index(conn, _params) do
    current_user = Pow.Plug.current_user(conn)
    # => Returns: %User{} struct
    # => Loaded by Pow.Plug.Session

    render(conn, :index, user: current_user)
  end
end
```

**Custom registration with invitation code**:

```elixir
defmodule MyAppWeb.RegistrationController do
  use MyAppWeb, :controller
  alias Pow.Plug

  def new(conn, %{"invitation_code" => code}) do
    # => Custom registration flow with invitation
    case validate_invitation_code(code) do
      :ok ->
        # => Valid invitation code
        changeset = MyApp.Users.User.changeset(%MyApp.Users.User{}, %{})
        render(conn, :new, changeset: changeset, invitation_code: code)

      {:error, reason} ->
        # => Invalid code
        conn
        |> put_flash(:error, "Invalid invitation code")
        |> redirect(to: "/")
    end
  end

  def create(conn, %{"user" => user_params, "invitation_code" => code}) do
    # => Create user with validated invitation
    case validate_invitation_code(code) do
      :ok ->
        # => Valid code, proceed with registration
        conn
        |> Plug.create_user(user_params)
        # => Pow handles user creation
        |> case do
          {:ok, user, conn} ->
            # => User created successfully
            # => User automatically logged in
            conn
            |> put_flash(:info, "Welcome!")
            |> redirect(to: "/dashboard")

          {:error, changeset, conn} ->
            # => Validation failed
            render(conn, :new, changeset: changeset, invitation_code: code)
        end

      {:error, _reason} ->
        # => Invalid code
        conn
        |> put_flash(:error, "Invalid invitation code")
        |> redirect(to: "/")
    end
  end

  defp validate_invitation_code(code) do
    # => Custom validation logic
    if code == "SECRET2024", do: :ok, else: {:error, :invalid}
  end
end
```

## Bodyguard - Policy-Based Authorization

Bodyguard provides policy-based authorization with clear separation of concerns:

**Installation**:

```elixir
# mix.exs
defp deps do
  [
    {:bodyguard, "~> 2.4"}
  ]
end
```

**Policy module**:

```elixir
defmodule MyApp.Blog.Post.Policy do
  @behaviour Bodyguard.Policy
  # => Implements authorization behavior

  alias MyApp.Blog.Post
  alias MyApp.Users.User

  # => Authorization rules
  def authorize(:list_posts, %User{}, _params), do: :ok
  # => Anyone can list posts

  def authorize(:show_post, %User{}, %Post{}), do: :ok
  # => Anyone can view posts

  def authorize(:create_post, %User{}, _params), do: :ok
  # => Any authenticated user can create

  def authorize(:update_post, %User{id: user_id}, %Post{user_id: user_id}), do: :ok
  # => Owner can update their own post

  def authorize(:update_post, %User{role: "admin"}, %Post{}), do: :ok
  # => Admin can update any post

  def authorize(:delete_post, %User{id: user_id}, %Post{user_id: user_id}), do: :ok
  # => Owner can delete their own post

  def authorize(:delete_post, %User{role: "admin"}, %Post{}), do: :ok
  # => Admin can delete any post

  def authorize(_action, _user, _resource), do: :error
  # => Deny all other operations
end
```

**Controller with Bodyguard**:

```elixir
defmodule MyAppWeb.PostController do
  use MyAppWeb, :controller
  alias MyApp.Blog
  alias MyApp.Blog.Post

  # => List posts
  def index(conn, _params) do
    current_user = conn.assigns.current_user
    # => Loaded by authentication pipeline

    with :ok <- Bodyguard.permit(Post.Policy, :list_posts, current_user, %{}) do
      # => Authorization check
      posts = Blog.list_posts()
      render(conn, :index, posts: posts)
    else
      :error ->
        # => Authorization failed
        conn
        |> put_status(:forbidden)
        |> json(%{error: "Not authorized"})
    end
  end

  # => Show post
  def show(conn, %{"id" => id}) do
    current_user = conn.assigns.current_user
    post = Blog.get_post!(id)

    with :ok <- Bodyguard.permit(Post.Policy, :show_post, current_user, post) do
      render(conn, :show, post: post)
    else
      :error ->
        conn
        |> put_status(:forbidden)
        |> json(%{error: "Not authorized"})
    end
  end

  # => Create post
  def create(conn, %{"post" => post_params}) do
    current_user = conn.assigns.current_user

    with :ok <- Bodyguard.permit(Post.Policy, :create_post, current_user, %{}),
         {:ok, post} <- Blog.create_post(current_user, post_params) do
      # => Both authorization and creation succeeded
      conn
      |> put_status(:created)
      |> json(%{post: post})
    else
      :error ->
        # => Authorization failed
        conn
        |> put_status(:forbidden)
        |> json(%{error: "Not authorized"})

      {:error, changeset} ->
        # => Validation failed
        conn
        |> put_status(:unprocessable_entity)
        |> json(%{errors: translate_errors(changeset)})
    end
  end

  # => Update post
  def update(conn, %{"id" => id, "post" => post_params}) do
    current_user = conn.assigns.current_user
    post = Blog.get_post!(id)

    with :ok <- Bodyguard.permit(Post.Policy, :update_post, current_user, post),
         {:ok, post} <- Blog.update_post(post, post_params) do
      json(conn, %{post: post})
    else
      :error ->
        conn
        |> put_status(:forbidden)
        |> json(%{error: "Not authorized"})

      {:error, changeset} ->
        conn
        |> put_status(:unprocessable_entity)
        |> json(%{errors: translate_errors(changeset)})
    end
  end

  # => Delete post
  def delete(conn, %{"id" => id}) do
    current_user = conn.assigns.current_user
    post = Blog.get_post!(id)

    with :ok <- Bodyguard.permit(Post.Policy, :delete_post, current_user, post),
         {:ok, _post} <- Blog.delete_post(post) do
      send_resp(conn, :no_content, "")
      # => 204 No Content
    else
      :error ->
        conn
        |> put_status(:forbidden)
        |> json(%{error: "Not authorized"})

      {:error, reason} ->
        conn
        |> put_status(:unprocessable_entity)
        |> json(%{error: reason})
    end
  end

  defp translate_errors(changeset) do
    # => Convert Ecto changeset errors to JSON
    Ecto.Changeset.traverse_errors(changeset, fn {msg, opts} ->
      Enum.reduce(opts, msg, fn {key, value}, acc ->
        String.replace(acc, "%{#{key}}", to_string(value))
      end)
    end)
  end
end
```

**Scope queries with authorization**:

```elixir
defmodule MyApp.Blog.Post.Policy do
  # => ... previous authorize clauses ...

  # => Scope: Filter posts user can access
  def scope(Post, %User{role: "admin"}, _params) do
    Post
    # => Admin sees all posts
    # => Returns: Ecto.Queryable
  end

  def scope(Post, %User{id: user_id}, _params) do
    import Ecto.Query
    from p in Post, where: p.user_id == ^user_id or p.published == true
    # => User sees their own posts + published posts
    # => Returns: Ecto.Query
  end

  def scope(Post, _user, _params) do
    import Ecto.Query
    from p in Post, where: p.published == true
    # => Anonymous users see only published posts
  end
end

# Usage in controller
def index(conn, _params) do
  current_user = conn.assigns.current_user
  # => May be nil (anonymous)

  posts =
    Post
    |> Bodyguard.scope(current_user)
    # => Apply policy scoping
    # => Returns: Ecto.Query with where clauses
    |> Repo.all()
    # => Execute query

  render(conn, :index, posts: posts)
end
```

## Production Pattern: Donation Platform with RBAC

Complete authentication and authorization for Islamic charity donation platform:

**User roles**:

- **Donor** - Can donate, view donation history
- **Campaign Manager** - Can create/manage campaigns
- **Finance Admin** - Can approve disbursements, view financial reports
- **Super Admin** - Full system access

**Schema**:

```elixir
defmodule MyApp.Users.User do
  use Ecto.Schema
  use Pow.Ecto.Schema
  # => Pow authentication

  schema "users" do
    pow_user_fields()
    # => email, password_hash

    field :name, :string
    field :phone, :string
    field :role, :string, default: "donor"
    # => Roles: "donor", "campaign_manager", "finance_admin", "super_admin"
    field :verified_at, :utc_datetime
    # => Email verification timestamp

    has_many :donations, MyApp.Donations.Donation
    has_many :campaigns, MyApp.Campaigns.Campaign

    timestamps()
  end

  def changeset(user_or_changeset, attrs) do
    user_or_changeset
    |> pow_changeset(attrs)
    |> Ecto.Changeset.cast(attrs, [:name, :phone, :role])
    |> Ecto.Changeset.validate_required([:name])
    |> Ecto.Changeset.validate_inclusion(:role, [
      "donor",
      "campaign_manager",
      "finance_admin",
      "super_admin"
    ])
  end
end
```

**Authorization policies**:

```elixir
defmodule MyApp.Donations.Donation.Policy do
  @behaviour Bodyguard.Policy
  alias MyApp.Donations.Donation
  alias MyApp.Users.User

  # => Create donation
  def authorize(:create_donation, %User{verified_at: verified_at}, _params)
      when not is_nil(verified_at) do
    # => Only verified users can donate
    :ok
  end

  # => View own donations
  def authorize(:list_donations, %User{id: user_id}, %{user_id: user_id}), do: :ok

  # => Finance admin can view all donations
  def authorize(:list_donations, %User{role: role}, _params)
      when role in ["finance_admin", "super_admin"] do
    :ok
  end

  # => Refund donation (admin only)
  def authorize(:refund_donation, %User{role: role}, %Donation{})
      when role in ["finance_admin", "super_admin"] do
    :ok
  end

  def authorize(_action, _user, _resource), do: :error
end

defmodule MyApp.Campaigns.Campaign.Policy do
  @behaviour Bodyguard.Policy
  alias MyApp.Campaigns.Campaign
  alias MyApp.Users.User

  # => Anyone can view campaigns
  def authorize(:list_campaigns, %User{}, _params), do: :ok
  def authorize(:show_campaign, %User{}, %Campaign{}), do: :ok

  # => Campaign manager can create
  def authorize(:create_campaign, %User{role: role}, _params)
      when role in ["campaign_manager", "super_admin"] do
    :ok
  end

  # => Owner or admin can update
  def authorize(:update_campaign, %User{id: user_id}, %Campaign{user_id: user_id}), do: :ok
  def authorize(:update_campaign, %User{role: "super_admin"}, %Campaign{}), do: :ok

  # => Finance admin can approve disbursement
  def authorize(:approve_disbursement, %User{role: role}, %Campaign{})
      when role in ["finance_admin", "super_admin"] do
    :ok
  end

  def authorize(_action, _user, _resource), do: :error
end
```

**Controllers with full auth**:

```elixir
defmodule MyAppWeb.DonationController do
  use MyAppWeb, :controller
  alias MyApp.Donations
  alias MyApp.Donations.Donation

  # => Create donation (authenticated + verified)
  def create(conn, %{"donation" => donation_params}) do
    current_user = Pow.Plug.current_user(conn)
    # => Loaded by Pow authentication

    with :ok <- Bodyguard.permit(Donation.Policy, :create_donation, current_user, %{}),
         # => Check authorization
         {:ok, donation} <- Donations.create_donation(current_user, donation_params) do
      # => Process payment gateway integration
      # => Send receipt email

      conn
      |> put_status(:created)
      |> json(%{donation: donation})
    else
      :error ->
        conn
        |> put_status(:forbidden)
        |> json(%{error: "Email verification required to donate"})

      {:error, changeset} ->
        conn
        |> put_status(:unprocessable_entity)
        |> json(%{errors: translate_errors(changeset)})
    end
  end

  # => List user's donations or all (for admin)
  def index(conn, params) do
    current_user = Pow.Plug.current_user(conn)

    with :ok <- Bodyguard.permit(Donation.Policy, :list_donations, current_user, params) do
      donations =
        Donation
        |> Bodyguard.scope(current_user)
        # => Apply policy scoping
        |> Donations.list_donations()

      render(conn, :index, donations: donations)
    else
      :error ->
        conn
        |> put_status(:forbidden)
        |> json(%{error: "Not authorized"})
    end
  end

  defp translate_errors(changeset), do: # ... error translation ...
end

defmodule MyAppWeb.CampaignController do
  use MyAppWeb, :controller
  alias MyApp.Campaigns
  alias MyApp.Campaigns.Campaign

  # => Create campaign (campaign_manager only)
  def create(conn, %{"campaign" => campaign_params}) do
    current_user = Pow.Plug.current_user(conn)

    with :ok <- Bodyguard.permit(Campaign.Policy, :create_campaign, current_user, %{}),
         {:ok, campaign} <- Campaigns.create_campaign(current_user, campaign_params) do
      conn
      |> put_status(:created)
      |> json(%{campaign: campaign})
    else
      :error ->
        conn
        |> put_status(:forbidden)
        |> json(%{error: "Campaign manager role required"})

      {:error, changeset} ->
        conn
        |> put_status(:unprocessable_entity)
        |> json(%{errors: translate_errors(changeset)})
    end
  end

  # => Approve disbursement (finance_admin only)
  def approve_disbursement(conn, %{"id" => id}) do
    current_user = Pow.Plug.current_user(conn)
    campaign = Campaigns.get_campaign!(id)

    with :ok <- Bodyguard.permit(
           Campaign.Policy,
           :approve_disbursement,
           current_user,
           campaign
         ),
         {:ok, campaign} <- Campaigns.approve_disbursement(campaign, current_user) do
      # => Process disbursement to campaign bank account
      # => Log transaction for audit trail

      json(conn, %{campaign: campaign})
    else
      :error ->
        conn
        |> put_status(:forbidden)
        |> json(%{error: "Finance admin access required"})

      {:error, reason} ->
        conn
        |> put_status(:unprocessable_entity)
        |> json(%{error: reason})
    end
  end

  defp translate_errors(changeset), do: # ... error translation ...
end
```

## Trade-offs: Manual vs Production Libraries

| Aspect                        | Manual Auth                     | Guardian + Pow + Bodyguard             |
| ----------------------------- | ------------------------------- | -------------------------------------- |
| **Complexity**                | Simple concepts, verbose code   | More concepts, concise integration     |
| **Security Patterns**         | Manual implementation           | Battle-tested standards                |
| **JWT Support**               | Manual with Joken               | Guardian (token refresh, revocation)   |
| **Session Management**        | Plug.Session (basic)            | Pow (email confirm, password reset)    |
| **Authorization**             | Repetitive controller checks    | Bodyguard policies (DRY)               |
| **CSRF Protection**           | Manual for custom flows         | Framework-integrated                   |
| **Password Reset**            | Custom token generation         | Pow built-in flow                      |
| **Email Verification**        | Custom implementation           | Pow extension                          |
| **Token Refresh**             | Manual refresh token flow       | Guardian.exchange/3                    |
| **Token Revocation**          | Redis blacklist                 | Guardian.DB (optional)                 |
| **Role-Based Access Control** | Manual conditionals             | Bodyguard policies + query scoping     |
| **OAuth Integration**         | Manual OAuth flow               | PowAssent (Google, GitHub, etc.)       |
| **Audit Trail**               | Custom logging                  | Policy-based with Bodyguard hooks      |
| **Testing Complexity**        | High (mock sessions/tokens)     | Moderate (test helpers provided)       |
| **Learning Curve**            | Lower (Plug primitives)         | Higher (library APIs and conventions)  |
| **Maintenance Burden**        | High (custom security patterns) | Low (library updates)                  |
| **Production Readiness**      | Requires security audit         | Production-tested, community-validated |
| **Recommended Use**           | Learning, simple apps           | Production systems, complex RBAC       |

**Recommendation**: Use Guardian + Pow + Bodyguard for production systems requiring secure authentication, authorization, and RBAC. Manual auth is valuable for understanding security fundamentals but requires extensive validation for production use.

## Best Practices

### 1. Always Hash Passwords with Bcrypt

```elixir
# Good: Use Bcrypt (default in Pow)
defmodule MyApp.Accounts do
  def create_user(attrs) do
    %User{}
    |> User.changeset(attrs)
    # => Pow handles password hashing with Bcrypt
    |> Repo.insert()
  end
end

# Good: Manual hashing if not using Pow
def hash_password(password) do
  Bcrypt.hash_pwd_salt(password)
  # => Bcrypt with salt
  # => Secure, slow by design (prevents brute force)
end
```

### 2. Implement Token Refresh for Long Sessions

```elixir
# Good: Refresh token flow with Guardian
def refresh_token(conn, %{"token" => old_token}) do
  case Guardian.exchange(old_token, "access", "access") do
    {:ok, _old, {new_token, _claims}} ->
      # => Old token invalidated, new token issued
      json(conn, %{token: new_token})

    {:error, _reason} ->
      conn
      |> put_status(:unauthorized)
      |> json(%{error: "Token refresh failed"})
  end
end
```

### 3. Use Policy Modules for Authorization

```elixir
# Bad: Authorization logic in controller
def update(conn, params) do
  if conn.assigns.current_user.role == "admin" do
    # ... update logic ...
  else
    # ... error ...
  end
end

# Good: Policy module with Bodyguard
def update(conn, params) do
  with :ok <- Bodyguard.permit(Post.Policy, :update_post, current_user, post) do
    # ... update logic ...
  end
end
```

### 4. Implement Email Verification

```elixir
# Good: Email verification with Pow extension
# config/config.exs
config :my_app, :pow,
  user: MyApp.Users.User,
  repo: MyApp.Repo,
  extensions: [PowEmailConfirmation]
  # => Adds email confirmation flow

# User schema
defmodule MyApp.Users.User do
  use Pow.Ecto.Schema
  use PowEmailConfirmation.Ecto.Schema
  # => Adds email_confirmed_at, email_confirmation_token

  # ... rest of schema ...
end
```

### 5. Use Secure Session Configuration

```elixir
# config/config.exs
config :my_app, MyAppWeb.Endpoint,
  secret_key_base: System.get_env("SECRET_KEY_BASE"),
  # => Strong secret (generate with: mix phx.gen.secret)
  session_options: [
    store: :cookie,
    # => Store session in encrypted cookie
    key: "_my_app_session",
    signing_salt: System.get_env("SESSION_SIGNING_SALT"),
    # => Additional signature layer
    same_site: "Lax",
    # => CSRF protection
    secure: true,
    # => HTTPS only (production)
    http_only: true,
    # => Prevent JavaScript access
    max_age: 60 * 60 * 24 * 7
    # => 7 day expiration
  ]
```

### 6. Implement Rate Limiting for Auth Endpoints

```elixir
# Prevent brute force attacks
defmodule MyAppWeb.RateLimiter do
  use Plug.Builder
  import Plug.Conn

  plug :rate_limit

  defp rate_limit(conn, _opts) do
    key = "login:#{get_ip(conn)}"
    # => Rate limit by IP address

    case Hammer.check_rate(key, 60_000, 5) do
      # => Allow 5 attempts per minute
      {:allow, _count} ->
        conn
        # => Continue

      {:deny, _limit} ->
        conn
        |> put_status(:too_many_requests)
        |> json(%{error: "Too many login attempts"})
        |> halt()
    end
  end

  defp get_ip(conn) do
    conn.remote_ip |> :inet.ntoa() |> to_string()
  end
end
```

### 7. Log Authentication Events for Audit Trail

```elixir
# Good: Log all auth events
defmodule MyAppWeb.SessionController do
  def create(conn, params) do
    case authenticate_user(params) do
      {:ok, user} ->
        # => Log successful login
        Logger.info("User login: #{user.id} (#{user.email}) from #{get_ip(conn)}")
        # => Audit trail

        # ... login logic ...

      {:error, :invalid_credentials} ->
        # => Log failed attempt
        Logger.warning("Failed login attempt for #{params["email"]} from #{get_ip(conn)}")
        # => Security monitoring

        # ... error response ...
    end
  end
end
```

## When to Use Each Approach

**Use Manual Authentication when**:

- Learning Elixir authentication fundamentals
- Building simple internal tools with minimal security requirements
- Prototyping authentication flows
- Understanding Plug session and token mechanics

**Use Guardian when**:

- Building stateless APIs with JWT tokens
- Require token refresh and revocation
- Need flexible claims-based authorization
- Mobile app backends (token-based)

**Use Pow when**:

- Building traditional web applications with sessions
- Need email confirmation and password reset
- Want complete authentication out-of-the-box
- Require OAuth integration (with PowAssent)

**Use Bodyguard when**:

- Implementing role-based access control (RBAC)
- Need policy-based authorization
- Want DRY authorization logic
- Require query scoping based on user permissions

**Use Combined Stack (Guardian + Pow + Bodyguard) when**:

- Building production applications with complex requirements
- Need both API (JWT) and web (session) authentication
- Require sophisticated RBAC with fine-grained permissions
- Multi-role systems (admin, manager, user)
- Financial or healthcare applications (audit trail, security compliance)

## Next Steps

**Completed**: Authentication and authorization patterns with Guardian, Pow, and Bodyguard

**Continue learning**:

- [Phoenix Framework](/en/learn/software-engineering/programming-languages/elixir/in-the-field/phoenix-framework) - Web framework integration with auth
- [Ecto Patterns](/en/learn/software-engineering/programming-languages/elixir/in-the-field/ecto-patterns) - Database patterns for user management
- [Rest Api Design](/en/learn/software-engineering/programming-languages/elixir/in-the-field/rest-api-design) - RESTful API security patterns

**Foundation knowledge**:

- [Testing Strategies](/en/learn/software-engineering/programming-languages/elixir/in-the-field/testing-strategies) - Testing authentication and authorization

**Quick reference**:

- [Overview](/en/learn/software-engineering/programming-languages/elixir/in-the-field/overview) - All 36 In-the-Field guides

---

**Summary**: Authentication and authorization in Elixir start with manual session and token management using Plug primitives, revealing security pattern complexity, CSRF protection needs, and authorization boilerplate. Production systems adopt Guardian for JWT authentication with token refresh and revocation, Pow for complete session-based authentication with email confirmation and password reset, and Bodyguard for policy-based authorization with DRY RBAC patterns. The combined stack provides battle-tested security for production applications requiring sophisticated access control.
