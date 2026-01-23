---
title: "Elixir Security"
description: Defense-in-depth security for Elixir applications including input validation, authentication, authorization, secure communication, and data protection
category: explanation
subcategory: stack-lang
tags:
  - elixir
  - security
  - input-validation
  - authentication
  - authorization
  - xss-prevention
  - sql-injection-prevention
  - csrf-protection
  - data-encryption
related:
  - ./ex-so-stla-el__best-practices.md
  - ./ex-so-stla-el__web-services.md
  - ./ex-so-stla-el__anti-patterns.md
principles:
  - security-first
  - explicit-over-implicit
last_updated: 2026-01-23
---

# Security

Security in Elixir applications requires defense-in-depth: **input validation**, **authentication**, **authorization**, **secure communication**, and **data protection**. This is especially critical for financial applications handling sensitive donation and payment data.

**Quick Reference**:

- [Input Validation](#input-validation)
  - [Ecto Changesets](#ecto-changesets)
  - [Parameter Validation](#parameter-validation)
  - [Type Checking](#type-checking)
- [XSS Prevention](#xss-prevention)
  - [Phoenix HTML Escaping](#phoenix-html-escaping)
  - [Safe Rendering](#safe-rendering)
  - [Content Security Policy](#content-security-policy)
- [SQL Injection Prevention](#sql-injection-prevention)
  - [Parameterized Queries](#parameterized-queries)
  - [Ecto Query Safety](#ecto-query-safety)
- [Authentication](#authentication)
  - [Password Hashing](#password-hashing)
  - [Guardian JWT](#guardian-jwt)
  - [Session Management](#session-management)
- [Authorization](#authorization)
  - [Role-Based Access Control](#role-based-access-control)
  - [Policy-Based Authorization](#policy-based-authorization)
- [CSRF Protection](#csrf-protection)
- [Secure Communication](#secure-communication)
  - [HTTPS/TLS](#httpstls)
  - [Certificate Validation](#certificate-validation)
- [Secret Management](#secret-management)
  - [Runtime Configuration](#runtime-configuration)
  - [Environment Variables](#environment-variables)
- [Security Headers](#security-headers)
- [Rate Limiting](#rate-limiting)
- [Audit Logging](#audit-logging)
- [Financial Domain Examples](#financial-domain-examples)
- [Security Best Practices](#security-best-practices)
- [Security Anti-patterns](#security-anti-patterns)
- [Security Testing](#security-testing)
- [Related Topics](#related-topics)
- [Sources](#sources)

## Input Validation

### Ecto Changesets

Use Ecto changesets for comprehensive input validation:

```elixir
defmodule FinancialPlatform.Donations.Donation do
  use Ecto.Schema
  import Ecto.Changeset

  schema "donations" do
    field :amount, :decimal
    field :currency, :string
    field :donor_email, :string
    field :donor_name, :string
    field :payment_method, :string
    field :notes, :string

    timestamps()
  end

  @required_fields [:amount, :currency, :donor_email, :donor_name]
  @optional_fields [:payment_method, :notes]
  @valid_currencies ["IDR", "USD", "EUR"]
  @valid_payment_methods ["credit_card", "bank_transfer", "ewallet"]

  def changeset(donation \\ %__MODULE__{}, attrs) do
    donation
    |> cast(attrs, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
    |> validate_amount()
    |> validate_currency()
    |> validate_email()
    |> validate_payment_method()
    |> validate_length(:donor_name, min: 2, max: 100)
    |> validate_length(:notes, max: 500)
  end

  defp validate_amount(changeset) do
    changeset
    |> validate_number(:amount,
      greater_than: 0,
      less_than_or_equal_to: 1_000_000_000,
      message: "must be between 0 and 1,000,000,000"
    )
  end

  defp validate_currency(changeset) do
    validate_inclusion(changeset, :currency, @valid_currencies,
      message: "must be one of: #{Enum.join(@valid_currencies, ", ")}"
    )
  end

  defp validate_email(changeset) do
    changeset
    |> validate_format(:donor_email, ~r/^[^\s]+@[^\s]+$/,
      message: "must be a valid email address"
    )
    |> validate_length(:donor_email, max: 160)
  end

  defp validate_payment_method(changeset) do
    case get_field(changeset, :payment_method) do
      nil ->
        changeset

      method ->
        validate_inclusion(changeset, :payment_method, @valid_payment_methods,
          message: "must be one of: #{Enum.join(@valid_payment_methods, ", ")}"
        )
    end
  end
end
```

The following diagram illustrates the defense-in-depth approach to input validation in Elixir financial applications:

```mermaid
graph TD
    %% Input Source
    CLIENT[Client Request<br/>amount: -100<br/>currency: XYZ]

    %% Layer 1: Controller
    CONTROLLER[Controller Layer<br/>Parameter Whitelist]
    WHITELIST{Allowed<br/>keys?}
    SANITIZE[String Sanitization<br/>trim, length limit]

    %% Layer 2: Changeset
    CHANGESET[Ecto Changeset<br/>Business Rules]
    CAST[cast/3<br/>Type coercion]
    VALIDATE_REQ[validate_required/2<br/>Mandatory fields]
    VALIDATE_BIZ[Custom Validations<br/>validate_amount<br/>validate_currency<br/>validate_email]

    %% Layer 3: Database
    DATABASE[(Database<br/>Constraints)]
    DB_CONSTRAINTS[Check Constraints<br/>amount > 0<br/>NOT NULL<br/>UNIQUE email]

    %% Success/Error Paths
    REJECT_PARAM[❌ Reject<br/>400 Bad Request<br/>Invalid parameters]
    REJECT_BIZ[❌ Reject<br/>422 Unprocessable<br/>Validation errors]
    REJECT_DB[❌ Reject<br/>Database constraint<br/>violation]
    SUCCESS[✅ Success<br/>Donation created<br/>Audit logged]

    %% Flow
    CLIENT --> CONTROLLER
    CONTROLLER --> WHITELIST

    WHITELIST -->|No| REJECT_PARAM
    WHITELIST -->|Yes| SANITIZE

    SANITIZE --> CHANGESET
    CHANGESET --> CAST
    CAST --> VALIDATE_REQ
    VALIDATE_REQ --> VALIDATE_BIZ

    VALIDATE_BIZ -->|Invalid| REJECT_BIZ
    VALIDATE_BIZ -->|Valid| DATABASE

    DATABASE --> DB_CONSTRAINTS
    DB_CONSTRAINTS -->|Violation| REJECT_DB
    DB_CONSTRAINTS -->|Pass| SUCCESS

    %% Styling (WCAG AA compliant)
    style CLIENT fill:#0173B2,stroke:#023B5A,color:#FFF
    style CONTROLLER fill:#029E73,stroke:#01593F,color:#FFF
    style WHITELIST fill:#DE8F05,stroke:#8A5903,color:#FFF
    style SANITIZE fill:#029E73,stroke:#01593F,color:#FFF
    style CHANGESET fill:#CC78BC,stroke:#7A4871,color:#FFF
    style CAST fill:#CC78BC,stroke:#7A4871,color:#FFF
    style VALIDATE_REQ fill:#CC78BC,stroke:#7A4871,color:#FFF
    style VALIDATE_BIZ fill:#CC78BC,stroke:#7A4871,color:#FFF
    style DATABASE fill:#CA9161,stroke:#7A5739,color:#FFF
    style DB_CONSTRAINTS fill:#CA9161,stroke:#7A5739,color:#FFF
    style REJECT_PARAM fill:#DE8F05,stroke:#8A5903,color:#FFF
    style REJECT_BIZ fill:#DE8F05,stroke:#8A5903,color:#FFF
    style REJECT_DB fill:#DE8F05,stroke:#8A5903,color:#FFF
    style SUCCESS fill:#029E73,stroke:#01593F,color:#FFF
```

**Defense-in-Depth Layers**:

1. **Controller Layer** (green): Whitelist allowed parameters, sanitize strings
2. **Changeset Layer** (purple): Type validation, business rules, format checks
3. **Database Layer** (brown): Constraints as final safety net

This multi-layer approach ensures malicious or invalid input is caught at the earliest possible stage.

### Parameter Validation

Validate controller parameters:

```elixir
defmodule FinancialWeb.DonationController do
  use FinancialWeb, :controller

  # Validate and sanitize params before processing
  def create(conn, params) do
    with {:ok, validated_params} <- validate_params(params),
         {:ok, donation} <- Donations.create_donation(validated_params) do
      conn
      |> put_status(:created)
      |> json(%{donation: donation})
    else
      {:error, :invalid_params} ->
        conn
        |> put_status(:bad_request)
        |> json(%{error: "Invalid parameters"})

      {:error, changeset} ->
        conn
        |> put_status(:unprocessable_entity)
        |> json(%{errors: format_errors(changeset)})
    end
  end

  defp validate_params(%{"donation" => donation_params}) when is_map(donation_params) do
    # Whitelist allowed keys
    allowed_keys = ["amount", "currency", "donor_email", "donor_name", "payment_method", "notes"]

    params =
      donation_params
      |> Map.take(allowed_keys)
      |> sanitize_strings()

    {:ok, params}
  end

  defp validate_params(_), do: {:error, :invalid_params}

  defp sanitize_strings(params) do
    Map.new(params, fn {key, value} ->
      {key, sanitize_value(value)}
    end)
  end

  defp sanitize_value(value) when is_binary(value) do
    value
    |> String.trim()
    |> String.slice(0, 1000)  # Limit length
  end

  defp sanitize_value(value), do: value

  defp format_errors(changeset) do
    Ecto.Changeset.traverse_errors(changeset, fn {msg, opts} ->
      Enum.reduce(opts, msg, fn {key, value}, acc ->
        String.replace(acc, "%{#{key}}", to_string(value))
      end)
    end)
  end
end
```

### Type Checking

Use pattern matching and guards for type safety:

```elixir
defmodule FinancialPlatform.Zakat do
  alias FinancialPlatform.Money

  # Pattern match and guard for type safety
  @spec calculate(Money.t(), Money.t()) :: {:ok, Money.t()} | {:error, atom()}
  def calculate(%Money{amount: wealth, currency: c1}, %Money{amount: nisab, currency: c2})
      when is_struct(wealth, Decimal) and is_struct(nisab, Decimal) and c1 == c2 do
    if Decimal.compare(wealth, nisab) == :gt do
      zakat = Decimal.mult(wealth, Decimal.new("0.025"))
      {:ok, Money.new(zakat, c1)}
    else
      {:error, :below_nisab}
    end
  end

  def calculate(%Money{}, %Money{currency: c2}) when c2 != :c1 do
    {:error, :currency_mismatch}
  end

  def calculate(_, _) do
    {:error, :invalid_input}
  end
end
```

## XSS Prevention

### Phoenix HTML Escaping

Phoenix automatically escapes HTML by default:

```elixir
# In template (.heex)
# Automatic escaping (safe)
<p><%= @user_input %></p>
<!-- Even if @user_input = "<script>alert('xss')</script>" -->
<!-- Rendered as: &lt;script&gt;alert('xss')&lt;/script&gt; -->

# Raw HTML (dangerous - only for trusted content)
<%= raw(@trusted_html) %>

# Safe rendering with Phoenix.HTML
<%= Phoenix.HTML.Tag.content_tag(:div, @user_input, class: "content") %>
```

Controller handling user input:

```elixir
defmodule FinancialWeb.CampaignController do
  use FinancialWeb, :controller

  def show(conn, %{"id" => campaign_id}) do
    campaign = Campaigns.get_campaign(campaign_id)

    # Phoenix automatically escapes in templates
    render(conn, :show, campaign: campaign)
  end

  # JSON responses don't need HTML escaping
  def api_show(conn, %{"id" => campaign_id}) do
    campaign = Campaigns.get_campaign(campaign_id)

    json(conn, %{
      id: campaign.id,
      name: campaign.name,  # Safe in JSON
      description: campaign.description
    })
  end
end
```

### Safe Rendering

Sanitize user HTML input when needed:

```elixir
# Add to mix.exs
{:html_sanitize_ex, "~> 1.4"}

defmodule FinancialPlatform.Campaigns do
  def create_campaign(attrs) do
    attrs
    |> sanitize_description()
    |> Campaign.changeset()
    |> Repo.insert()
  end

  defp sanitize_description(%{"description" => desc} = attrs) when is_binary(desc) do
    # Allow only safe HTML tags
    sanitized = HtmlSanitizeEx.basic_html(desc)
    Map.put(attrs, "description", sanitized)
  end

  defp sanitize_description(attrs), do: attrs
end
```

### Content Security Policy

Configure CSP headers:

```elixir
# In endpoint.ex
defmodule FinancialWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :financial_platform

  plug :put_security_headers

  defp put_security_headers(conn, _opts) do
    conn
    |> put_resp_header("content-security-policy",
      "default-src 'self'; " <>
      "script-src 'self' 'unsafe-inline' 'unsafe-eval'; " <>
      "style-src 'self' 'unsafe-inline'; " <>
      "img-src 'self' data: https:; " <>
      "font-src 'self'; " <>
      "connect-src 'self'; " <>
      "frame-ancestors 'none'"
    )
  end
end
```

## SQL Injection Prevention

### Parameterized Queries

Always use parameterized queries with Ecto:

```elixir
defmodule FinancialPlatform.Donations do
  import Ecto.Query

  # ✅ SAFE - parameterized query
  def search_donations(email) do
    from(d in Donation,
      where: d.donor_email == ^email
    )
    |> Repo.all()
  end

  # ❌ DANGEROUS - string interpolation (DON'T DO THIS!)
  def search_donations_unsafe(email) do
    query = "SELECT * FROM donations WHERE donor_email = '#{email}'"
    Repo.query(query)  # SQL INJECTION VULNERABLE!
  end

  # ✅ SAFE - Ecto handles escaping
  def search_by_amount_range(min, max) do
    from(d in Donation,
      where: d.amount >= ^min and d.amount <= ^max
    )
    |> Repo.all()
  end

  # ✅ SAFE - even with LIKE queries
  def search_by_name_pattern(pattern) do
    # Ecto escapes the pattern
    like_pattern = "%#{pattern}%"

    from(d in Donation,
      where: like(d.donor_name, ^like_pattern)
    )
    |> Repo.all()
  end
end
```

### Ecto Query Safety

Use Ecto's query DSL exclusively:

```elixir
defmodule FinancialPlatform.Reports do
  import Ecto.Query

  def donation_report(filters) do
    base_query = from(d in Donation, as: :donation)

    base_query
    |> apply_filters(filters)
    |> select_report_fields()
    |> Repo.all()
  end

  defp apply_filters(query, filters) do
    Enum.reduce(filters, query, fn
      {:campaign_id, id}, query ->
        where(query, [donation: d], d.campaign_id == ^id)

      {:min_amount, amount}, query ->
        where(query, [donation: d], d.amount >= ^amount)

      {:date_range, {start_date, end_date}}, query ->
        where(query, [donation: d],
          d.inserted_at >= ^start_date and d.inserted_at <= ^end_date
        )

      _unknown_filter, query ->
        query
    end)
  end

  defp select_report_fields(query) do
    from([donation: d] in query,
      select: %{
        total: sum(d.amount),
        count: count(d.id),
        average: avg(d.amount)
      }
    )
  end
end
```

## Authentication

### Password Hashing

Use bcrypt for password hashing:

```elixir
# Add to mix.exs
{:bcrypt_elixir, "~> 3.1"}

defmodule FinancialPlatform.Accounts.User do
  use Ecto.Schema
  import Ecto.Changeset

  schema "users" do
    field :email, :string
    field :password, :string, virtual: true
    field :password_hash, :string

    timestamps()
  end

  def registration_changeset(user \\ %__MODULE__{}, attrs) do
    user
    |> cast(attrs, [:email, :password])
    |> validate_required([:email, :password])
    |> validate_format(:email, ~r/^[^\s]+@[^\s]+$/)
    |> validate_length(:password, min: 12)
    |> validate_password_strength()
    |> unique_constraint(:email)
    |> put_password_hash()
  end

  defp validate_password_strength(changeset) do
    password = get_change(changeset, :password)

    cond do
      is_nil(password) ->
        changeset

      not String.match?(password, ~r/[A-Z]/) ->
        add_error(changeset, :password, "must contain at least one uppercase letter")

      not String.match?(password, ~r/[a-z]/) ->
        add_error(changeset, :password, "must contain at least one lowercase letter")

      not String.match?(password, ~r/[0-9]/) ->
        add_error(changeset, :password, "must contain at least one number")

      not String.match?(password, ~r/[!@#$%^&*(),.?":{}|<>]/) ->
        add_error(changeset, :password, "must contain at least one special character")

      true ->
        changeset
    end
  end

  defp put_password_hash(changeset) do
    case get_change(changeset, :password) do
      nil -> changeset
      password -> put_change(changeset, :password_hash, Bcrypt.hash_pwd_salt(password))
    end
  end
end

defmodule FinancialPlatform.Accounts do
  alias FinancialPlatform.Accounts.User

  def authenticate(email, password) do
    with %User{} = user <- Repo.get_by(User, email: email),
         true <- Bcrypt.verify_pass(password, user.password_hash) do
      {:ok, user}
    else
      _ ->
        # Prevent timing attacks - always run hash verification
        Bcrypt.no_user_verify()
        {:error, :invalid_credentials}
    end
  end
end
```

### Guardian JWT

Implement JWT authentication with Guardian:

```elixir
# Add to mix.exs
{:guardian, "~> 2.3"}

# config/config.exs
config :financial_platform, FinancialPlatform.Guardian,
  issuer: "financial_platform",
  secret_key: System.get_env("GUARDIAN_SECRET_KEY")

# lib/financial_platform/guardian.ex
defmodule FinancialPlatform.Guardian do
  use Guardian, otp_app: :financial_platform

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

# lib/financial_web/auth_pipeline.ex
defmodule FinancialWeb.AuthPipeline do
  use Guardian.Plug.Pipeline,
    otp_app: :financial_platform,
    module: FinancialPlatform.Guardian,
    error_handler: FinancialWeb.AuthErrorHandler

  plug Guardian.Plug.VerifyHeader, scheme: "Bearer"
  plug Guardian.Plug.EnsureAuthenticated
  plug Guardian.Plug.LoadResource
end

# lib/financial_web/controllers/auth_controller.ex
defmodule FinancialWeb.AuthController do
  use FinancialWeb, :controller

  alias FinancialPlatform.{Accounts, Guardian}

  def login(conn, %{"email" => email, "password" => password}) do
    case Accounts.authenticate(email, password) do
      {:ok, user} ->
        {:ok, token, _claims} = Guardian.encode_and_sign(user, %{}, ttl: {1, :hour})

        conn
        |> put_status(:ok)
        |> json(%{token: token, user: user_json(user)})

      {:error, :invalid_credentials} ->
        conn
        |> put_status(:unauthorized)
        |> json(%{error: "Invalid credentials"})
    end
  end

  def refresh(conn, _params) do
    user = Guardian.Plug.current_resource(conn)
    {:ok, token, _claims} = Guardian.encode_and_sign(user, %{}, ttl: {1, :hour})

    json(conn, %{token: token})
  end

  defp user_json(user) do
    %{
      id: user.id,
      email: user.email
    }
  end
end
```

The following diagram illustrates the complete JWT authentication flow using Guardian in a financial application:

```mermaid
sequenceDiagram
    participant Client as Client<br/>(Browser/Mobile)
    participant Auth as AuthController<br/>login/refresh
    participant Accounts as Accounts Context<br/>authenticate
    participant Bcrypt as Bcrypt<br/>password verify
    participant Guardian as Guardian<br/>JWT encoding
    participant Protected as Protected Route<br/>DonationController
    participant Pipeline as AuthPipeline<br/>VerifyHeader
    participant DB as Database

    %% Login Flow
    Client->>Auth: POST /api/auth/login<br/>{email, password}
    Auth->>Accounts: authenticate(email, password)
    Accounts->>DB: Get user by email
    DB-->>Accounts: User record with password_hash

    Accounts->>Bcrypt: verify_pass(password, hash)
    Bcrypt-->>Accounts: true/false

    alt Invalid Credentials
        Bcrypt->>Bcrypt: no_user_verify()<br/>(prevent timing attack)
        Accounts-->>Auth: {:error, :invalid_credentials}
        Auth-->>Client: 401 Unauthorized<br/>Invalid credentials
    else Valid Credentials
        Accounts-->>Auth: {:ok, user}
        Auth->>Guardian: encode_and_sign(user, ttl: 1h)
        Guardian->>Guardian: Generate JWT<br/>sub: user.id<br/>exp: timestamp
        Guardian-->>Auth: {:ok, token, claims}
        Auth-->>Client: 200 OK<br/>{token: "eyJ...", user: {...}}
    end

    %% Protected Route Access
    Client->>Protected: GET /api/donations<br/>Header: "Bearer eyJ..."
    Protected->>Pipeline: Verify token
    Pipeline->>Guardian: VerifyHeader (scheme: Bearer)
    Guardian->>Guardian: Decode JWT<br/>Verify signature<br/>Check expiration

    alt Invalid/Expired Token
        Guardian-->>Pipeline: {:error, reason}
        Pipeline-->>Client: 401 Unauthorized<br/>Invalid token
    else Valid Token
        Guardian-->>Pipeline: {:ok, claims}
        Pipeline->>Guardian: resource_from_claims(claims)
        Guardian->>DB: Get user by sub (user_id)
        DB-->>Guardian: User record
        Guardian-->>Pipeline: {:ok, user}
        Pipeline->>Protected: conn.assigns.current_user = user
        Protected->>Protected: Process request<br/>(user authorized)
        Protected-->>Client: 200 OK<br/>{donations: [...]}
    end

    %% Token Refresh
    Client->>Auth: POST /api/auth/refresh<br/>Header: "Bearer eyJ..."
    Auth->>Guardian: current_resource(conn)
    Guardian-->>Auth: user
    Auth->>Guardian: encode_and_sign(user, ttl: 1h)
    Guardian-->>Auth: {:ok, new_token, claims}
    Auth-->>Client: 200 OK<br/>{token: "eyJ..."}

    %% Styling (WCAG AA compliant)
    participant Client as Client
    participant Auth as AuthController
    participant Accounts as Accounts Context
    participant Bcrypt as Bcrypt
    participant Guardian as Guardian
    participant Protected as Protected Route
    participant Pipeline as AuthPipeline
    participant DB as Database
```

**Key Security Features**:

- **Password Hashing**: Bcrypt with salt rounds prevents password leaks
- **Timing Attack Prevention**: `no_user_verify()` runs even when user not found
- **JWT Expiration**: Tokens expire after 1 hour (configurable)
- **Stateless Auth**: JWT contains claims, no server-side session storage
- **Token Refresh**: Clients can refresh tokens before expiration
- **Authorization Header**: Bearer token in `Authorization: Bearer <token>` header

### Session Management

Secure session configuration:

```elixir
# config/config.exs
config :financial_platform, FinancialWeb.Endpoint,
  secret_key_base: System.get_env("SECRET_KEY_BASE"),
  live_view: [signing_salt: System.get_env("LIVEVIEW_SIGNING_SALT")]

# In endpoint.ex
defmodule FinancialWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :financial_platform

  @session_options [
    store: :cookie,
    key: "_financial_platform_key",
    signing_salt: "secure_salt",
    max_age: 24 * 60 * 60,  # 24 hours
    http_only: true,         # Prevent JavaScript access
    secure: true,            # HTTPS only
    same_site: "Lax"         # CSRF protection
  ]

  plug Plug.Session, @session_options
end

# Session controller
defmodule FinancialWeb.SessionController do
  use FinancialWeb, :controller

  def create(conn, %{"email" => email, "password" => password}) do
    case Accounts.authenticate(email, password) do
      {:ok, user} ->
        conn
        |> put_session(:user_id, user.id)
        |> put_session(:login_at, System.system_time(:second))
        |> configure_session(renew: true)  # Prevent session fixation
        |> redirect(to: "/dashboard")

      {:error, :invalid_credentials} ->
        conn
        |> put_flash(:error, "Invalid credentials")
        |> redirect(to: "/login")
    end
  end

  def delete(conn, _params) do
    conn
    |> configure_session(drop: true)
    |> redirect(to: "/")
  end
end
```

## Authorization

### Role-Based Access Control

Implement RBAC:

```elixir
defmodule FinancialPlatform.Accounts.User do
  use Ecto.Schema

  schema "users" do
    field :email, :string
    field :role, Ecto.Enum, values: [:donor, :admin, :finance_manager]

    timestamps()
  end
end

defmodule FinancialWeb.Plugs.RequireRole do
  import Plug.Conn
  import Phoenix.Controller

  def init(roles) when is_list(roles), do: roles

  def call(conn, required_roles) do
    user = conn.assigns[:current_user]

    if user && user.role in required_roles do
      conn
    else
      conn
      |> put_status(:forbidden)
      |> json(%{error: "Insufficient permissions"})
      |> halt()
    end
  end
end

# In router
defmodule FinancialWeb.Router do
  use FinancialWeb, :router

  pipeline :admin do
    plug FinancialWeb.Plugs.RequireRole, [:admin]
  end

  pipeline :finance do
    plug FinancialWeb.Plugs.RequireRole, [:admin, :finance_manager]
  end

  scope "/api/admin", FinancialWeb.Admin do
    pipe_through [:api, :auth, :admin]

    resources "/users", UserController
  end

  scope "/api/finance", FinancialWeb.Finance do
    pipe_through [:api, :auth, :finance]

    get "/reports", ReportController, :index
  end
end
```

### Policy-Based Authorization

Use Bodyguard for policy-based authorization:

```elixir
# Add to mix.exs
{:bodyguard, "~> 2.4"}

# Define policies
defmodule FinancialPlatform.Donations.Policy do
  @behaviour Bodyguard.Policy

  alias FinancialPlatform.Accounts.User
  alias FinancialPlatform.Donations.Donation

  # Admins can do anything
  def authorize(:all, %User{role: :admin}, _), do: :ok

  # Donors can view their own donations
  def authorize(:view_donation, %User{id: user_id}, %Donation{donor_id: donor_id})
      when user_id == donor_id do
    :ok
  end

  # Finance managers can view all donations
  def authorize(:view_donation, %User{role: :finance_manager}, %Donation{}) do
    :ok
  end

  # Finance managers can approve donations
  def authorize(:approve_donation, %User{role: :finance_manager}, %Donation{}) do
    :ok
  end

  # Donors can create donations
  def authorize(:create_donation, %User{role: :donor}, _) do
    :ok
  end

  # Default deny
  def authorize(_, _, _), do: :error
end

# Use in controllers
defmodule FinancialWeb.DonationController do
  use FinancialWeb, :controller

  alias FinancialPlatform.Donations
  alias FinancialPlatform.Donations.Policy

  def show(conn, %{"id" => id}) do
    user = conn.assigns.current_user
    donation = Donations.get_donation(id)

    with :ok <- Bodyguard.permit(Policy, :view_donation, user, donation) do
      json(conn, %{donation: donation})
    else
      :error ->
        conn
        |> put_status(:forbidden)
        |> json(%{error: "You don't have permission to view this donation"})
    end
  end

  def approve(conn, %{"id" => id}) do
    user = conn.assigns.current_user
    donation = Donations.get_donation(id)

    with :ok <- Bodyguard.permit(Policy, :approve_donation, user, donation),
         {:ok, approved} <- Donations.approve_donation(donation) do
      json(conn, %{donation: approved})
    else
      :error ->
        conn
        |> put_status(:forbidden)
        |> json(%{error: "You don't have permission to approve donations"})

      {:error, reason} ->
        conn
        |> put_status(:unprocessable_entity)
        |> json(%{error: reason})
    end
  end
end
```

## CSRF Protection

Phoenix includes CSRF protection by default:

```elixir
# Enabled in endpoint.ex
defmodule FinancialWeb.Endpoint do
  plug Plug.Session, @session_options
  plug :protect_from_forgery
  plug :put_secure_browser_headers
end

# In forms (.heex)
<.form for={@changeset} action={~p"/donations"} method="post">
  <%= csrf_token() %>
  <!-- Form fields -->
</.form>

# In API controllers, use token authentication instead
defmodule FinancialWeb.API.DonationController do
  plug :skip_csrf_protection  # Use token auth instead
end
```

## Secure Communication

### HTTPS/TLS

Force HTTPS in production:

```elixir
# config/prod.exs
config :financial_platform, FinancialWeb.Endpoint,
  force_ssl: [
    rewrite_on: [:x_forwarded_proto],
    hsts: true,
    expires: 31_536_000  # 1 year
  ]

# In endpoint.ex
defmodule FinancialWeb.Endpoint do
  if Application.compile_env(:financial_platform, :force_ssl) do
    plug Plug.SSL,
      rewrite_on: [:x_forwarded_proto],
      hsts: true,
      host: nil
  end
end
```

### Certificate Validation

Validate SSL certificates for external API calls:

```elixir
# Add to mix.exs
{:httpoison, "~> 2.2"}

defmodule FinancialPlatform.PaymentGateway do
  @base_url "https://api.paymentgateway.com"

  def create_payment(amount, currency) do
    HTTPoison.post(
      "#{@base_url}/payments",
      Jason.encode!(%{amount: amount, currency: currency}),
      [
        {"Content-Type", "application/json"},
        {"Authorization", "Bearer #{api_key()}"}
      ],
      # SSL options
      ssl: [
        verify: :verify_peer,
        cacerts: :public_key.cacerts_get(),
        depth: 3,
        customize_hostname_check: [
          match_fun: :public_key.pkix_verify_hostname_match_fun(:https)
        ]
      ]
    )
  end

  defp api_key, do: Application.get_env(:financial_platform, :payment_gateway_api_key)
end
```

## Secret Management

### Runtime Configuration

Use runtime configuration for secrets:

```elixir
# config/runtime.exs
import Config

if config_env() == :prod do
  database_url =
    System.get_env("DATABASE_URL") ||
      raise """
      environment variable DATABASE_URL is missing.
      """

  config :financial_platform, FinancialPlatform.Repo,
    url: database_url,
    pool_size: String.to_integer(System.get_env("POOL_SIZE") || "10"),
    ssl: true,
    ssl_opts: [
      verify: :verify_peer,
      cacerts: :public_key.cacerts_get()
    ]

  secret_key_base =
    System.get_env("SECRET_KEY_BASE") ||
      raise """
      environment variable SECRET_KEY_BASE is missing.
      """

  config :financial_platform, FinancialWeb.Endpoint,
    secret_key_base: secret_key_base

  guardian_secret =
    System.get_env("GUARDIAN_SECRET_KEY") ||
      raise """
      environment variable GUARDIAN_SECRET_KEY is missing.
      """

  config :financial_platform, FinancialPlatform.Guardian,
    secret_key: guardian_secret
end
```

### Environment Variables

Never commit secrets to version control:

```elixir
# .env (add to .gitignore)
export DATABASE_URL="postgresql://user:pass@localhost/financial_prod"
export SECRET_KEY_BASE="very_long_secret_key_here"
export GUARDIAN_SECRET_KEY="another_secret_key"
export PAYMENT_GATEWAY_API_KEY="payment_api_key"

# Generate secrets
mix phx.gen.secret

# Use in application
defmodule FinancialPlatform.Config do
  def payment_gateway_api_key do
    System.get_env("PAYMENT_GATEWAY_API_KEY") ||
      raise "PAYMENT_GATEWAY_API_KEY not set"
  end

  def database_encryption_key do
    System.get_env("DB_ENCRYPTION_KEY") ||
      raise "DB_ENCRYPTION_KEY not set"
  end
end
```

## Security Headers

Configure comprehensive security headers:

```elixir
defmodule FinancialWeb.SecurityHeaders do
  import Plug.Conn

  def init(opts), do: opts

  def call(conn, _opts) do
    conn
    |> put_resp_header("x-frame-options", "DENY")
    |> put_resp_header("x-content-type-options", "nosniff")
    |> put_resp_header("x-xss-protection", "1; mode=block")
    |> put_resp_header("referrer-policy", "strict-origin-when-cross-origin")
    |> put_resp_header(
      "content-security-policy",
      "default-src 'self'; " <>
        "script-src 'self' 'unsafe-inline'; " <>
        "style-src 'self' 'unsafe-inline'; " <>
        "img-src 'self' data: https:; " <>
        "font-src 'self'; " <>
        "connect-src 'self'; " <>
        "frame-ancestors 'none'; " <>
        "base-uri 'self'; " <>
        "form-action 'self'"
    )
    |> put_resp_header(
      "permissions-policy",
      "geolocation=(), microphone=(), camera=()"
    )
    |> put_resp_header(
      "strict-transport-security",
      "max-age=31536000; includeSubDomains"
    )
  end
end

# In endpoint.ex
plug FinancialWeb.SecurityHeaders
```

## Rate Limiting

Implement rate limiting:

```elixir
# Add to mix.exs
{:hammer, "~> 6.1"}

# config/config.exs
config :hammer,
  backend: {Hammer.Backend.ETS, [expiry_ms: 60_000 * 60 * 4, cleanup_interval_ms: 60_000 * 10]}

defmodule FinancialWeb.RateLimiter do
  import Plug.Conn

  def init(opts), do: opts

  def call(conn, opts) do
    limit = Keyword.get(opts, :limit, 100)
    window_ms = Keyword.get(opts, :window_ms, 60_000)
    identifier = get_identifier(conn)

    case Hammer.check_rate("api:#{identifier}", window_ms, limit) do
      {:allow, _count} ->
        conn

      {:deny, _limit} ->
        conn
        |> put_status(:too_many_requests)
        |> Phoenix.Controller.json(%{error: "Rate limit exceeded"})
        |> halt()
    end
  end

  defp get_identifier(conn) do
    # Use user ID if authenticated, otherwise IP
    case conn.assigns[:current_user] do
      %{id: user_id} -> "user:#{user_id}"
      _ -> "ip:#{get_ip(conn)}"
    end
  end

  defp get_ip(conn) do
    conn.remote_ip
    |> :inet.ntoa()
    |> to_string()
  end
end

# In router
scope "/api", FinancialWeb.API do
  pipe_through :api
  plug FinancialWeb.RateLimiter, limit: 1000, window_ms: 60_000

  resources "/donations", DonationController
end
```

## Audit Logging

Log security-relevant events:

```elixir
defmodule FinancialPlatform.AuditLog do
  use Ecto.Schema

  schema "audit_logs" do
    field :user_id, :integer
    field :action, :string
    field :resource_type, :string
    field :resource_id, :string
    field :ip_address, :string
    field :user_agent, :string
    field :changes, :map
    field :occurred_at, :utc_datetime

    timestamps(updated_at: false)
  end

  def log(user, action, resource, conn, changes \\ %{}) do
    %__MODULE__{}
    |> changeset(%{
      user_id: user.id,
      action: to_string(action),
      resource_type: resource_type(resource),
      resource_id: resource_id(resource),
      ip_address: get_ip(conn),
      user_agent: get_user_agent(conn),
      changes: changes,
      occurred_at: DateTime.utc_now()
    })
    |> Repo.insert()
  end

  defp resource_type(%module{}), do: module |> Module.split() |> List.last()
  defp resource_id(%{id: id}), do: to_string(id)
  defp get_ip(conn), do: to_string(:inet.ntoa(conn.remote_ip))
  defp get_user_agent(conn), do: get_req_header(conn, "user-agent") |> List.first()
end

# Use in controllers
defmodule FinancialWeb.DonationController do
  def approve(conn, %{"id" => id}) do
    user = conn.assigns.current_user
    donation = Donations.get_donation(id)

    with {:ok, approved} <- Donations.approve_donation(donation) do
      # Log security-relevant action
      AuditLog.log(user, :approve_donation, approved, conn, %{
        previous_status: donation.status,
        new_status: approved.status
      })

      json(conn, %{donation: approved})
    end
  end
end
```

## Financial Domain Examples

Complete security example for financial platform:

```elixir
# See all previous examples integrated in a financial context
```

## Security Best Practices

1. **Validate all input** - Use Ecto changesets, never trust user input
2. **Escape output** - Phoenix does this automatically
3. **Use parameterized queries** - Ecto prevents SQL injection
4. **Hash passwords** - Use bcrypt with proper salt rounds
5. **Implement authorization** - Check permissions before actions
6. **Enable CSRF protection** - Phoenix includes this by default
7. **Force HTTPS** - Always use TLS in production
8. **Rate limit APIs** - Prevent abuse
9. **Audit sensitive actions** - Log security events
10. **Keep dependencies updated** - Run `mix hex.audit` regularly

## Security Anti-patterns

### 1. Trusting User Input

```elixir
# ❌ DANGEROUS
def update_donation(id, attrs) do
  Donation
  |> Repo.get(id)
  |> Repo.update(attrs)  # No validation!
end

# ✅ SAFE
def update_donation(id, attrs) do
  Donation
  |> Repo.get(id)
  |> Donation.changeset(attrs)  # Validates input
  |> Repo.update()
end
```

### 2. Committing Secrets

```elixir
# ❌ DANGEROUS - secrets in code
defmodule Config do
  def api_key, do: "sk_live_123456789"  # NEVER DO THIS!
end

# ✅ SAFE - environment variables
defmodule Config do
  def api_key, do: System.get_env("API_KEY")
end
```

### 3. Weak Password Requirements

```elixir
# ❌ WEAK
validate_length(:password, min: 6)

# ✅ STRONG
validate_length(:password, min: 12)
validate_password_complexity()  # Uppercase, lowercase, numbers, special chars
```

## Security Testing

```elixir
defmodule FinancialWeb.DonationControllerTest do
  use FinancialWeb.ConnCase

  describe "authorization" do
    test "anonymous users cannot approve donations", %{conn: conn} do
      donation = insert(:donation)

      conn = post(conn, ~p"/api/donations/#{donation.id}/approve")

      assert json_response(conn, 401)
    end

    test "donors cannot approve donations", %{conn: conn} do
      user = insert(:user, role: :donor)
      donation = insert(:donation)

      conn =
        conn
        |> authenticate_user(user)
        |> post(~p"/api/donations/#{donation.id}/approve")

      assert json_response(conn, 403)
    end

    test "finance managers can approve donations", %{conn: conn} do
      user = insert(:user, role: :finance_manager)
      donation = insert(:donation)

      conn =
        conn
        |> authenticate_user(user)
        |> post(~p"/api/donations/#{donation.id}/approve")

      assert json_response(conn, 200)
    end
  end

  describe "input validation" do
    test "rejects donations with negative amount", %{conn: conn} do
      conn = post(conn, ~p"/api/donations", %{
        donation: %{amount: -100, currency: "IDR"}
      })

      assert json_response(conn, 422)
    end

    test "rejects donations with invalid currency", %{conn: conn} do
      conn = post(conn, ~p"/api/donations", %{
        donation: %{amount: 100, currency: "INVALID"}
      })

      assert json_response(conn, 422)
    end
  end
end
```

## Related Topics

- [Web Services](ex-so-stla-el__web-services.md) - Phoenix security features
- [Error Handling](ex-so-stla-el__error-handling.md) - Secure error messages
- [Testing](ex-so-stla-el__test-driven-development.md) - Security testing
- [Dependencies](ex-so-stla-el__dependencies.md) - Dependency security

## Sources

- [OWASP Top 10](https://owasp.org/www-project-top-ten/)
- [Phoenix Security Guide](https://hexdocs.pm/phoenix/security.html)
- [Guardian Authentication](https://hexdocs.pm/guardian/)
- [Bcrypt Documentation](https://hexdocs.pm/bcrypt_elixir/)
- [Bodyguard Authorization](https://hexdocs.pm/bodyguard/)
- [Plug Security Headers](https://hexdocs.pm/plug/Plug.HTML.html)

---

**Last Updated**: 2026-01-23
**Elixir Version**: 1.12+ (baseline), 1.17+ (recommended), 1.18.0 (latest)
**Maintainers**: Platform Documentation Team
