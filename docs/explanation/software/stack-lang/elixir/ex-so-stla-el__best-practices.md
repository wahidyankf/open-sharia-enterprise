# Elixir Best Practices

**Quick Reference**: [Overview](#overview) | [Naming Conventions](#naming-conventions) | [OTP Patterns](#otp-patterns) | [Supervision](#supervision-tree-design) | [Context Modules](#context-modules) | [Ecto Changesets](#ecto-changeset-patterns) | [Testing](#testing-best-practices) | [Code Organization](#code-organization)

## Overview

Elixir best practices represent the collective wisdom of the community, distilled from years of building production systems. Following these practices leads to code that is maintainable, testable, and idiomatic.

This document covers essential best practices for Elixir development with focus on:

- **Naming Conventions**: Consistent naming across modules, functions, and variables
- **OTP Patterns**: When and how to use GenServer, Supervisors, and other OTP components
- **Supervision Design**: Building fault-tolerant systems with supervision trees
- **Context Modules**: Organizing domain logic with Phoenix contexts
- **Ecto Patterns**: Working effectively with databases
- **Testing**: Writing comprehensive, maintainable tests
- **Code Organization**: Structuring projects for growth

All examples use the financial domain (Zakat calculation, donation processing, Islamic finance operations).

## Naming Conventions

### Module Names

Use PascalCase for module names, following a hierarchical structure:

```elixir
# Good - Clear hierarchy
defmodule FinancialDomain.Zakat.Calculator do
end

defmodule FinancialDomain.Donations.Campaign do
end

defmodule FinancialDomain.Accounts.User do
end

# Bad - Flat structure
defmodule ZakatCalculator do
end

defmodule DonationCampaign do
end
```

**Pattern**: `Domain.Subdomain.Module`

- Top level represents bounded context (FinancialDomain)
- Second level represents subdomain (Zakat, Donations)
- Third level represents specific module (Calculator, Campaign)

### Function Names

Use snake_case for function names, with descriptive verbs:

```elixir
defmodule FinancialDomain.Zakat.Calculator do
  # Good - Descriptive verb + noun
  def calculate_zakat(wealth, nisab)
  def determine_eligibility(applicant)
  def process_payment(transaction)
  def validate_amount(value)

  # Bad - Unclear or inconsistent
  def calc(w, n)
  def checkEligibility(applicant)  # camelCase
  def Payment(transaction)         # PascalCase
end
```

**Conventions**:

- Boolean functions end with `?`: `eligible?/1`, `valid?/1`, `paid?/1`
- Bang functions (raising exceptions) end with `!`: `fetch!/1`, `save!/1`
- Private functions start with same name as public: `calculate/2` and `calculate_internal/2`

```elixir
defmodule FinancialDomain.Donations.Validator do
  # Boolean predicate
  def valid_amount?(amount) when amount > 0, do: true
  def valid_amount?(_), do: false

  # Regular function returns tuple
  def validate_donation(params) do
    case valid_amount?(params.amount) do
      true -> {:ok, params}
      false -> {:error, :invalid_amount}
    end
  end

  # Bang version raises on error
  def validate_donation!(params) do
    case validate_donation(params) do
      {:ok, donation} -> donation
      {:error, reason} -> raise "Validation failed: #{reason}"
    end
  end
end
```

### Variable Names

Use snake_case for variables, with descriptive names:

```elixir
defmodule FinancialDomain.Reports.Generator do
  def generate_monthly_report(year, month) do
    # Good - Descriptive variable names
    zakat_payments = fetch_zakat_payments(year, month)
    donation_totals = calculate_donation_totals(zakat_payments)
    campaign_statistics = aggregate_campaign_stats(donation_totals)

    # Bad - Unclear abbreviations
    zp = fetch_zakat_payments(year, month)
    dt = calculate_donation_totals(zp)
    cs = aggregate_campaign_stats(dt)

    format_report(campaign_statistics)
  end
end
```

### Module Attributes

Use uppercase with underscores for module constants:

```elixir
defmodule FinancialDomain.Zakat.Constants do
  @zakat_rate Decimal.new("0.025")  # 2.5%
  @nisab_gold_grams 85
  @nisab_silver_grams 595
  @lunar_year_days 354

  def zakat_rate, do: @zakat_rate
  def nisab_gold_grams, do: @nisab_gold_grams
end
```

## OTP Patterns

### When to Use GenServer

GenServer is the go-to choice for stateful processes:

```elixir
defmodule FinancialDomain.Zakat.RateCache do
  @moduledoc """
  Caches current Zakat rates and nisab thresholds.

  Use GenServer because:
  - State needs to persist between requests (rates, thresholds)
  - State updates happen periodically (daily rate refresh)
  - Multiple processes need access to same rates
  """
  use GenServer

  # Client API

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def get_rate(currency) do
    GenServer.call(__MODULE__, {:get_rate, currency})
  end

  def update_rates(new_rates) do
    GenServer.cast(__MODULE__, {:update_rates, new_rates})
  end

  # Server Callbacks

  @impl true
  def init(_opts) do
    # Schedule periodic rate refresh
    schedule_rate_refresh()

    {:ok, %{
      rates: load_initial_rates(),
      last_updated: DateTime.utc_now()
    }}
  end

  @impl true
  def handle_call({:get_rate, currency}, _from, state) do
    rate = Map.get(state.rates, currency)
    {:reply, {:ok, rate}, state}
  end

  @impl true
  def handle_cast({:update_rates, new_rates}, state) do
    {:noreply, %{state | rates: new_rates, last_updated: DateTime.utc_now()}}
  end

  @impl true
  def handle_info(:refresh_rates, state) do
    new_rates = fetch_latest_rates()
    schedule_rate_refresh()
    {:noreply, %{state | rates: new_rates, last_updated: DateTime.utc_now()}}
  end

  # Private Functions

  defp schedule_rate_refresh do
    Process.send_after(self(), :refresh_rates, :timer.hours(24))
  end

  defp load_initial_rates, do: %{USD: Decimal.new("0.025")}
  defp fetch_latest_rates, do: %{USD: Decimal.new("0.025")}
end
```

### When NOT to Use GenServer

Don't use GenServer for stateless operations:

```elixir
# Bad - GenServer for stateless calculations
defmodule FinancialDomain.Zakat.CalculatorServer do
  use GenServer

  def calculate(wealth, nisab) do
    GenServer.call(__MODULE__, {:calculate, wealth, nisab})
  end

  def handle_call({:calculate, wealth, nisab}, _from, state) do
    result = do_calculation(wealth, nisab)
    {:reply, result, state}
  end
end

# Good - Plain module for stateless calculations
defmodule FinancialDomain.Zakat.Calculator do
  def calculate(wealth, nisab) do
    if Money.greater_than?(wealth, nisab) do
      Money.multiply(wealth, Decimal.new("0.025"))
    else
      Money.new(0, wealth.currency)
    end
  end
end
```

### Task for One-off Work

Use Task for concurrent, one-time operations:

```elixir
defmodule FinancialDomain.Reports.Parallel do
  @moduledoc """
  Generates multiple reports concurrently using Task.
  """

  def generate_all_reports(year, month) do
    tasks = [
      Task.async(fn -> generate_zakat_report(year, month) end),
      Task.async(fn -> generate_donation_report(year, month) end),
      Task.async(fn -> generate_campaign_report(year, month) end),
      Task.async(fn -> generate_financial_summary(year, month) end)
    ]

    # Wait for all tasks to complete
    results = Task.await_many(tasks, :timer.minutes(5))

    {:ok, combine_reports(results)}
  end

  defp generate_zakat_report(_year, _month), do: %{type: :zakat}
  defp generate_donation_report(_year, _month), do: %{type: :donation}
  defp generate_campaign_report(_year, _month), do: %{type: :campaign}
  defp generate_financial_summary(_year, _month), do: %{type: :summary}
  defp combine_reports(reports), do: reports
end
```

### Agent for Simple State

Use Agent for simple state without complex logic:

```elixir
defmodule FinancialDomain.Metrics.Counter do
  @moduledoc """
  Simple counter for tracking donation counts.

  Use Agent because:
  - State is simple (just a map of counters)
  - No complex business logic
  - No message handling needed
  """

  def start_link(_opts) do
    Agent.start_link(fn -> %{} end, name: __MODULE__)
  end

  def increment(category) do
    Agent.update(__MODULE__, fn state ->
      Map.update(state, category, 1, &(&1 + 1))
    end)
  end

  def get_count(category) do
    Agent.get(__MODULE__, fn state ->
      Map.get(state, category, 0)
    end)
  end

  def get_all_counts do
    Agent.get(__MODULE__, & &1)
  end
end
```

## Supervision Tree Design

### Basic Supervision Strategy

```elixir
defmodule FinancialDomain.Application do
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Database connection
      FinancialDomain.Repo,

      # PubSub for real-time communication
      {Phoenix.PubSub, name: FinancialDomain.PubSub},

      # Cache for rates and thresholds
      FinancialDomain.Zakat.RateCache,

      # Metrics counter
      FinancialDomain.Metrics.Counter,

      # Donation processor
      {FinancialDomain.Donations.Processor, []},

      # Web endpoint (supervised by Phoenix)
      FinancialDomainWeb.Endpoint
    ]

    # Strategy: :one_for_one
    # If one child crashes, only restart that child
    opts = [strategy: :one_for_one, name: FinancialDomain.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
```

### Choosing Supervision Strategy

```elixir
defmodule FinancialDomain.Zakat.DistributionSupervisor do
  @moduledoc """
  Supervises Zakat distribution workflow processes.

  Strategy: :one_for_all
  If any component fails, restart all components together.
  Use when components are interdependent.
  """
  use Supervisor

  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @impl true
  def init(_init_arg) do
    children = [
      # These processes depend on each other
      FinancialDomain.Zakat.AllocationCalculator,
      FinancialDomain.Zakat.TransferProcessor,
      FinancialDomain.Zakat.NotificationService
    ]

    # If one fails, restart all (they share state)
    Supervisor.init(children, strategy: :one_for_all)
  end
end

defmodule FinancialDomain.Campaigns.Supervisor do
  @moduledoc """
  Supervises individual campaign workers.

  Strategy: :one_for_one
  Campaigns are independent - if one crashes, others continue.
  """
  use Supervisor

  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @impl true
  def init(_init_arg) do
    children = [
      # Each campaign worker is independent
      {DynamicSupervisor, name: FinancialDomain.Campaigns.WorkerSupervisor, strategy: :one_for_one}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  def start_campaign(campaign_id) do
    child_spec = {FinancialDomain.Campaigns.Worker, campaign_id}
    DynamicSupervisor.start_child(FinancialDomain.Campaigns.WorkerSupervisor, child_spec)
  end
end
```

### Restart Strategies

```elixir
defmodule FinancialDomain.CriticalService do
  use GenServer

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  # In supervisor child spec, configure restart strategy
  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      restart: :permanent,  # Always restart (default for critical services)
      shutdown: 5000,       # Wait 5 seconds for graceful shutdown
      type: :worker
    }
  end

  @impl true
  def init(_opts) do
    {:ok, %{}}
  end
end

defmodule FinancialDomain.TemporaryWorker do
  use GenServer

  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      restart: :temporary,  # Don't restart if process completes normally
      shutdown: :brutal_kill,  # Kill immediately on shutdown
      type: :worker
    }
  end

  # rest of implementation...
end
```

## Context Modules

Context modules organize related functionality following Phoenix context pattern and DDD principles.

### Context Module Structure

```elixir
defmodule FinancialDomain.Donations do
  @moduledoc """
  Donations context - boundary for donation-related operations.

  This context encapsulates:
  - Donation creation and validation
  - Campaign management
  - Donor relationships
  - Payment processing coordination

  External modules should interact through this context, not directly
  with Ecto schemas or internal modules.
  """

  import Ecto.Query
  alias FinancialDomain.Repo
  alias FinancialDomain.Donations.{Donation, Campaign, Donor}

  ## Donations

  @doc """
  Lists all donations with optional filters.
  """
  def list_donations(filters \\ %{}) do
    Donation
    |> apply_filters(filters)
    |> preload([:donor, :campaign])
    |> Repo.all()
  end

  @doc """
  Gets a single donation by ID.
  """
  def get_donation(id) do
    case Repo.get(Donation, id) do
      nil -> {:error, :not_found}
      donation -> {:ok, Repo.preload(donation, [:donor, :campaign])}
    end
  end

  @doc """
  Gets a donation, raising if not found.
  """
  def get_donation!(id) do
    Donation
    |> Repo.get!(id)
    |> Repo.preload([:donor, :campaign])
  end

  @doc """
  Creates a donation with validation.
  """
  def create_donation(attrs) do
    %Donation{}
    |> Donation.changeset(attrs)
    |> Repo.insert()
    |> case do
      {:ok, donation} = result ->
        broadcast_donation_created(donation)
        result

      error ->
        error
    end
  end

  @doc """
  Updates a donation.
  """
  def update_donation(%Donation{} = donation, attrs) do
    donation
    |> Donation.changeset(attrs)
    |> Repo.update()
    |> case do
      {:ok, donation} = result ->
        broadcast_donation_updated(donation)
        result

      error ->
        error
    end
  end

  @doc """
  Deletes a donation.
  """
  def delete_donation(%Donation{} = donation) do
    Repo.delete(donation)
  end

  ## Campaigns

  @doc """
  Lists active campaigns.
  """
  def list_active_campaigns do
    Campaign
    |> where([c], c.status == :active)
    |> where([c], c.end_date > ^DateTime.utc_now())
    |> order_by([c], desc: c.created_at)
    |> Repo.all()
  end

  @doc """
  Creates a campaign.
  """
  def create_campaign(attrs) do
    %Campaign{}
    |> Campaign.changeset(attrs)
    |> Repo.insert()
  end

  ## Donors

  @doc """
  Gets or creates a donor by email.
  """
  def get_or_create_donor(email, attrs \\ %{}) do
    case Repo.get_by(Donor, email: email) do
      nil ->
        attrs
        |> Map.put(:email, email)
        |> create_donor()

      donor ->
        {:ok, donor}
    end
  end

  ## Private Functions

  defp apply_filters(queryable, %{status: status}) do
    where(queryable, [d], d.status == ^status)
  end

  defp apply_filters(queryable, %{campaign_id: campaign_id}) do
    where(queryable, [d], d.campaign_id == ^campaign_id)
  end

  defp apply_filters(queryable, _filters), do: queryable

  defp broadcast_donation_created(donation) do
    Phoenix.PubSub.broadcast(
      FinancialDomain.PubSub,
      "donations",
      {:donation_created, donation}
    )
  end

  defp broadcast_donation_updated(donation) do
    Phoenix.PubSub.broadcast(
      FinancialDomain.PubSub,
      "donations:#{donation.id}",
      {:donation_updated, donation}
    )
  end

  defp create_donor(attrs) do
    %Donor{}
    |> Donor.changeset(attrs)
    |> Repo.insert()
  end
end
```

### Context Boundaries

```elixir
# Good - Contexts interact through public APIs
defmodule FinancialDomain.Zakat do
  def calculate_and_record(wealth_data, user_id) do
    with {:ok, user} <- FinancialDomain.Accounts.get_user(user_id),
         {:ok, calculation} <- calculate_zakat(wealth_data),
         {:ok, payment} <- record_payment(calculation, user) do
      {:ok, payment}
    end
  end

  # Uses Accounts context public API
  defp record_payment(calculation, user) do
    FinancialDomain.Payments.create_payment(%{
      amount: calculation.zakat_amount,
      user_id: user.id,
      type: :zakat
    })
  end
end

# Bad - Direct access to other context's internals
defmodule FinancialDomain.Zakat.Bad do
  alias FinancialDomain.Accounts.User  # Accessing internal schema
  alias FinancialDomain.Repo

  def calculate_and_record(wealth_data, user_id) do
    user = Repo.get(User, user_id)  # Direct database access - breaks boundary!
    # ...
  end
end
```

## Ecto Changeset Patterns

### Basic Changeset

```elixir
defmodule FinancialDomain.Donations.Donation do
  use Ecto.Schema
  import Ecto.Changeset

  schema "donations" do
    field :amount, :decimal
    field :currency, :string
    field :status, Ecto.Enum, values: [:pending, :processing, :completed, :failed]
    field :donor_email, :string
    field :campaign_id, :id

    timestamps()
  end

  @doc """
  Changeset for creating a donation.
  """
  def changeset(donation, attrs) do
    donation
    |> cast(attrs, [:amount, :currency, :status, :donor_email, :campaign_id])
    |> validate_required([:amount, :currency, :donor_email, :campaign_id])
    |> validate_number(:amount, greater_than: 0)
    |> validate_inclusion(:currency, ["USD", "EUR", "GBP", "SAR"])
    |> validate_format(:donor_email, ~r/@/)
    |> foreign_key_constraint(:campaign_id)
    |> unique_constraint([:donor_email, :campaign_id, :amount],
         name: :donations_unique_index,
         message: "Duplicate donation detected"
       )
  end
end
```

### Custom Validations

```elixir
defmodule FinancialDomain.Zakat.Payment do
  use Ecto.Schema
  import Ecto.Changeset

  schema "zakat_payments" do
    field :wealth_amount, :decimal
    field :nisab_threshold, :decimal
    field :zakat_amount, :decimal
    field :currency, :string
    field :payment_date, :date
    field :user_id, :id

    timestamps()
  end

  def changeset(payment, attrs) do
    payment
    |> cast(attrs, [:wealth_amount, :nisab_threshold, :zakat_amount, :currency, :payment_date, :user_id])
    |> validate_required([:wealth_amount, :nisab_threshold, :currency, :user_id])
    |> validate_number(:wealth_amount, greater_than: 0)
    |> validate_number(:nisab_threshold, greater_than: 0)
    |> validate_zakat_calculation()
    |> validate_minimum_wealth()
    |> validate_payment_date()
  end

  defp validate_zakat_calculation(changeset) do
    wealth = get_field(changeset, :wealth_amount)
    nisab = get_field(changeset, :nisab_threshold)
    zakat = get_field(changeset, :zakat_amount)

    if wealth && nisab && zakat do
      expected_zakat = if Decimal.compare(wealth, nisab) == :gt do
        Decimal.mult(wealth, Decimal.new("0.025"))
      else
        Decimal.new(0)
      end

      if Decimal.equal?(zakat, expected_zakat) do
        changeset
      else
        add_error(changeset, :zakat_amount, "Zakat calculation incorrect")
      end
    else
      changeset
    end
  end

  defp validate_minimum_wealth(changeset) do
    wealth = get_field(changeset, :wealth_amount)
    nisab = get_field(changeset, :nisab_threshold)

    if wealth && nisab && Decimal.compare(wealth, nisab) != :gt do
      add_error(changeset, :wealth_amount, "Wealth must exceed nisab threshold for Zakat")
    else
      changeset
    end
  end

  defp validate_payment_date(changeset) do
    payment_date = get_field(changeset, :payment_date)
    today = Date.utc_today()

    if payment_date && Date.compare(payment_date, today) == :gt do
      add_error(changeset, :payment_date, "Payment date cannot be in the future")
    else
      changeset
    end
  end
end
```

### Changeset Composition

```elixir
defmodule FinancialDomain.Accounts.User do
  use Ecto.Schema
  import Ecto.Changeset

  schema "users" do
    field :email, :string
    field :full_name, :string
    field :password_hash, :string
    field :password, :string, virtual: true
    field :password_confirmation, :string, virtual: true
    field :tax_id, :string
    field :verified, :boolean, default: false

    timestamps()
  end

  @doc """
  Basic changeset for user registration.
  """
  def registration_changeset(user, attrs) do
    user
    |> cast(attrs, [:email, :full_name, :password, :password_confirmation])
    |> validate_required([:email, :full_name, :password])
    |> validate_email()
    |> validate_password()
    |> hash_password()
  end

  @doc """
  Changeset for profile updates (no password).
  """
  def profile_changeset(user, attrs) do
    user
    |> cast(attrs, [:full_name, :tax_id])
    |> validate_required([:full_name])
    |> validate_tax_id()
  end

  @doc """
  Changeset for verification.
  """
  def verification_changeset(user) do
    change(user, verified: true)
  end

  # Private validation functions

  defp validate_email(changeset) do
    changeset
    |> validate_format(:email, ~r/@/)
    |> validate_length(:email, max: 255)
    |> unique_constraint(:email)
  end

  defp validate_password(changeset) do
    changeset
    |> validate_length(:password, min: 8, max: 100)
    |> validate_confirmation(:password, message: "Passwords do not match")
  end

  defp validate_tax_id(changeset) do
    changeset
    |> validate_format(:tax_id, ~r/^\d{3}-\d{2}-\d{4}$/,
         message: "Tax ID must be in format XXX-XX-XXXX"
       )
  end

  defp hash_password(changeset) do
    case get_change(changeset, :password) do
      nil ->
        changeset

      password ->
        changeset
        |> put_change(:password_hash, Bcrypt.hash_pwd_salt(password))
        |> delete_change(:password)
        |> delete_change(:password_confirmation)
    end
  end
end
```

## Testing Best Practices

### ExUnit Structure

```elixir
defmodule FinancialDomain.Zakat.CalculatorTest do
  use ExUnit.Case, async: true
  doctest FinancialDomain.Zakat.Calculator

  alias FinancialDomain.Zakat.Calculator
  alias FinancialDomain.Money

  describe "calculate/2" do
    test "calculates 2.5% zakat for wealth above nisab" do
      wealth = Money.new(10000, :USD)
      nisab = Money.new(5000, :USD)

      assert {:ok, zakat} = Calculator.calculate(wealth, nisab)
      assert Money.equal?(zakat, Money.new(250, :USD))
    end

    test "returns zero for wealth below nisab" do
      wealth = Money.new(3000, :USD)
      nisab = Money.new(5000, :USD)

      assert {:ok, zakat} = Calculator.calculate(wealth, nisab)
      assert Money.equal?(zakat, Money.new(0, :USD))
    end

    test "returns zero for wealth equal to nisab" do
      wealth = Money.new(5000, :USD)
      nisab = Money.new(5000, :USD)

      assert {:ok, zakat} = Calculator.calculate(wealth, nisab)
      assert Money.equal?(zakat, Money.new(0, :USD))
    end

    test "returns error for negative wealth" do
      wealth = Money.new(-1000, :USD)
      nisab = Money.new(5000, :USD)

      assert {:error, message} = Calculator.calculate(wealth, nisab)
      assert message =~ "cannot be negative"
    end

    test "returns error for currency mismatch" do
      wealth = Money.new(10000, :USD)
      nisab = Money.new(5000, :EUR)

      assert {:error, message} = Calculator.calculate(wealth, nisab)
      assert message =~ "Currency mismatch"
    end
  end
end
```

### Doctests

```elixir
defmodule FinancialDomain.Money do
  @moduledoc """
  Money data type for precise financial calculations.

  ## Examples

      iex> alias FinancialDomain.Money
      iex> money = Money.new(100, :USD)
      iex> Money.to_string(money)
      "USD 100.00"

      iex> m1 = Money.new(100, :USD)
      iex> m2 = Money.new(50, :USD)
      iex> Money.add(m1, m2)
      %Money{amount: Decimal.new("150.00"), currency: :USD}

  """

  defstruct [:amount, :currency]

  @type t :: %__MODULE__{
    amount: Decimal.t(),
    currency: atom()
  }

  @doc """
  Creates a new Money struct.

  ## Examples

      iex> Money.new(100, :USD)
      %Money{amount: Decimal.new("100.00"), currency: :USD}

      iex> Money.new(0, :EUR)
      %Money{amount: Decimal.new("0.00"), currency: :EUR}

  """
  def new(amount, currency) when is_number(amount) do
    %__MODULE__{
      amount: Decimal.new(amount),
      currency: currency
    }
  end

  # Additional functions...
end
```

### Testing Context Modules

```elixir
defmodule FinancialDomain.DonationsTest do
  use FinancialDomain.DataCase, async: true

  alias FinancialDomain.Donations

  describe "list_donations/1" do
    test "returns all donations" do
      donation1 = donation_fixture()
      donation2 = donation_fixture()

      donations = Donations.list_donations()

      assert length(donations) == 2
      assert Enum.any?(donations, &(&1.id == donation1.id))
      assert Enum.any?(donations, &(&1.id == donation2.id))
    end

    test "filters by status" do
      _pending = donation_fixture(%{status: :pending})
      completed = donation_fixture(%{status: :completed})

      donations = Donations.list_donations(%{status: :completed})

      assert length(donations) == 1
      assert hd(donations).id == completed.id
    end

    test "filters by campaign_id" do
      campaign = campaign_fixture()
      donation = donation_fixture(%{campaign_id: campaign.id})
      _other = donation_fixture()  # Different campaign

      donations = Donations.list_donations(%{campaign_id: campaign.id})

      assert length(donations) == 1
      assert hd(donations).id == donation.id
    end
  end

  describe "create_donation/1" do
    test "creates donation with valid attributes" do
      campaign = campaign_fixture()

      attrs = %{
        amount: "100.00",
        currency: "USD",
        donor_email: "donor@example.com",
        campaign_id: campaign.id
      }

      assert {:ok, donation} = Donations.create_donation(attrs)
      assert Decimal.equal?(donation.amount, Decimal.new("100.00"))
      assert donation.currency == "USD"
      assert donation.donor_email == "donor@example.com"
    end

    test "returns error with invalid amount" do
      campaign = campaign_fixture()

      attrs = %{
        amount: "-10.00",
        currency: "USD",
        donor_email: "donor@example.com",
        campaign_id: campaign.id
      }

      assert {:error, changeset} = Donations.create_donation(attrs)
      assert "must be greater than 0" in errors_on(changeset).amount
    end
  end

  # Test fixtures
  defp donation_fixture(attrs \\ %{}) do
    campaign = Map.get_lazy(attrs, :campaign, &campaign_fixture/0)

    {:ok, donation} =
      attrs
      |> Enum.into(%{
        amount: "50.00",
        currency: "USD",
        donor_email: "test@example.com",
        campaign_id: campaign.id,
        status: :pending
      })
      |> Donations.create_donation()

    donation
  end

  defp campaign_fixture do
    # Create and return a campaign
    %{id: 1, name: "Test Campaign", status: :active}
  end
end
```

## Code Organization

### Directory Structure

```
financial_domain/
├── lib/
│   ├── financial_domain/
│   │   ├── application.ex         # OTP Application
│   │   ├── repo.ex                # Ecto Repository
│   │   │
│   │   ├── zakat/                 # Zakat bounded context
│   │   │   ├── calculator.ex
│   │   │   ├── payment.ex
│   │   │   └── rate_cache.ex
│   │   │
│   │   ├── donations/             # Donations bounded context
│   │   │   ├── campaign.ex
│   │   │   ├── donation.ex
│   │   │   ├── donor.ex
│   │   │   └── processor.ex
│   │   │
│   │   ├── accounts/              # Accounts bounded context
│   │   │   └── user.ex
│   │   │
│   │   └── shared/                # Shared utilities
│   │       ├── money.ex
│   │       └── validators.ex
│   │
│   ├── financial_domain_web/      # Web interface
│   │   ├── controllers/
│   │   ├── views/
│   │   └── router.ex
│   │
│   └── financial_domain.ex        # Public API facade
│
├── test/
│   ├── financial_domain/
│   │   ├── zakat/
│   │   ├── donations/
│   │   └── accounts/
│   │
│   ├── financial_domain_web/
│   │
│   ├── support/
│   │   ├── data_case.ex
│   │   └── fixtures.ex
│   │
│   └── test_helper.exs
│
├── config/
│   ├── config.exs
│   ├── dev.exs
│   ├── test.exs
│   └── prod.exs
│
└── mix.exs
```

### Public API Facade

```elixir
defmodule FinancialDomain do
  @moduledoc """
  Public API for Financial Domain.

  This module provides a unified interface to domain functionality.
  External consumers should use this module rather than internal contexts.
  """

  # Delegate to Zakat context
  defdelegate calculate_zakat(wealth, nisab), to: FinancialDomain.Zakat.Calculator, as: :calculate
  defdelegate list_zakat_payments(user_id), to: FinancialDomain.Zakat

  # Delegate to Donations context
  defdelegate create_donation(attrs), to: FinancialDomain.Donations
  defdelegate list_donations(filters), to: FinancialDomain.Donations
  defdelegate get_donation(id), to: FinancialDomain.Donations

  # Delegate to Accounts context
  defdelegate register_user(attrs), to: FinancialDomain.Accounts, as: :create_user
  defdelegate authenticate_user(email, password), to: FinancialDomain.Accounts
end
```

## Best Practices Summary

### Do's

- ✅ Use descriptive, consistent naming (PascalCase modules, snake_case functions)
- ✅ Leverage GenServer for stateful processes
- ✅ Design supervision trees with appropriate restart strategies
- ✅ Organize code into contexts following DDD principles
- ✅ Validate data with Ecto changesets
- ✅ Write comprehensive tests with descriptive names
- ✅ Use doctests for documentation examples
- ✅ Make functions small and focused (single responsibility)
- ✅ Use pattern matching for control flow
- ✅ Leverage the pipe operator for data transformations

### Don'ts

- ❌ Don't use GenServer for stateless operations
- ❌ Don't bypass context boundaries with direct Repo access
- ❌ Don't skip validation in changesets
- ❌ Don't write tests that depend on execution order
- ❌ Don't use abbreviations in variable names
- ❌ Don't nest functions deeply (use pipe operator)
- ❌ Don't ignore supervision tree design
- ❌ Don't mix business logic into controllers or views
- ❌ Don't use exceptions for control flow
- ❌ Don't forget to document public APIs

## Related Topics

- [Idioms](./ex-so-stla-el__idioms.md) - Elixir patterns and idioms
- [Anti-Patterns](./ex-so-stla-el__anti-patterns.md) - Common mistakes to avoid
- [OTP GenServer](./ex-so-stla-el__otp-genserver.md) - Deep dive into GenServer
- [OTP Supervisor](./ex-so-stla-el__otp-supervisor.md) - Supervision tree patterns
- [Domain-Driven Design](./ex-so-stla-el__domain-driven-design.md) - DDD in Elixir
- [Test-Driven Development](./ex-so-stla-el__test-driven-development.md) - Testing strategies

## Sources

- [Elixir Style Guide](https://github.com/christopheradams/elixir_style_guide)
- [Phoenix Contexts Guide](https://hexdocs.pm/phoenix/contexts.html)
- [Ecto Best Practices](https://hexdocs.pm/ecto/Ecto.html)
- [ExUnit Documentation](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Programming Phoenix ≥ 1.4](https://pragprog.com/titles/phoenix14/programming-phoenix-1-4/)

---

**Last Updated**: 2025-01-23
**Elixir Version**: 1.18.0+
