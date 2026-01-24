---
title: "Protocols and Behaviours"
description: Two mechanisms for polymorphism in Elixir - protocols for ad-hoc polymorphism and behaviours for compile-time contracts
category: explanation
subcategory: stack-lang
tags:
  - elixir
  - protocols
  - behaviours
  - polymorphism
  - ad-hoc-polymorphism
  - compile-time-contracts
  - design-patterns
related:
  - ./ex-so-stla-el__idioms.md
  - ./ex-so-stla-el__best-practices.md
  - ./ex-so-stla-el__type-safety.md
principles:
  - explicit-over-implicit
  - simplicity-over-complexity
last_updated: 2026-01-23
---

# Protocols and Behaviours

Elixir provides two mechanisms for polymorphism: **protocols** for ad-hoc polymorphism (extending types after definition) and **behaviours** for compile-time polymorphism (defining contracts for modules). Understanding when and how to use each is essential for building extensible, maintainable systems.

**Quick Reference**:

- [Protocols](#protocols)
  - [Defining Protocols](#defining-protocols)
  - [Implementing Protocols](#implementing-protocols)
  - [Built-in Protocols](#built-in-protocols)
  - [Protocol Consolidation](#protocol-consolidation)
- [Behaviours](#behaviours)
  - [Defining Behaviours](#defining-behaviours)
  - [Implementing Behaviours](#implementing-behaviours)
  - [Behaviour Callbacks](#behaviour-callbacks)
  - [Optional Callbacks](#optional-callbacks)
- [Protocols vs Behaviours](#protocols-vs-behaviours)
- [Common Patterns](#common-patterns)
  - [Strategy Pattern with Behaviours](#strategy-pattern-with-behaviours)
  - [Adapter Pattern with Protocols](#adapter-pattern-with-protocols)
  - [Plugin Systems](#plugin-systems)
- [Financial Domain Examples](#financial-domain-examples)
- [Best Practices](#best-practices)
- [Anti-patterns](#anti-patterns)
- [Testing](#testing)
- [Performance Considerations](#performance-considerations)
- [Related Topics](#related-topics)
- [Sources](#sources)

## Protocols

Protocols enable **ad-hoc polymorphism** by allowing you to extend existing types with new functionality without modifying their original definitions. This is similar to interfaces in other languages but more flexible.

The following diagram illustrates how protocol dispatch works at runtime:

```mermaid
graph TD
    Call[Protocol Call<br/>Auditable.audit_trail entity]
    TypeDetect[Runtime Type Detection]

    Donation[Donation Implementation<br/>defimpl Auditable, for: Donation]
    Zakat[ZakatPayment Implementation<br/>defimpl Auditable, for: ZakatPayment]
    Campaign[Campaign Implementation<br/>defimpl Auditable, for: Campaign]
    Fallback[Fallback: Any<br/>defimpl Auditable, for: Any]

    Result[Execute Implementation<br/>Return Result]

    Call --> TypeDetect
    TypeDetect -->|%Donation{}| Donation
    TypeDetect -->|%ZakatPayment{}| Zakat
    TypeDetect -->|%Campaign{}| Campaign
    TypeDetect -->|Other types| Fallback

    Donation --> Result
    Zakat --> Result
    Campaign --> Result
    Fallback --> Result

    style Call fill:#0173B2,stroke:#023B5A,color:#FFF
    style TypeDetect fill:#DE8F05,stroke:#8A5903,color:#FFF
    style Donation fill:#029E73,stroke:#01593F,color:#FFF
    style Zakat fill:#029E73,stroke:#01593F,color:#FFF
    style Campaign fill:#029E73,stroke:#01593F,color:#FFF
    style Fallback fill:#CA9161,stroke:#7D5A3D,color:#FFF
    style Result fill:#0173B2,stroke:#023B5A,color:#FFF
```

The following sequence diagram shows the complete protocol implementation flow:

```mermaid
sequenceDiagram
    participant Client
    participant Protocol as Auditable Protocol
    participant TypeCheck as Type Detection
    participant Impl as Donation Implementation

    Client->>Protocol: audit_trail(donation)
    Protocol->>TypeCheck: Determine type
    TypeCheck->>TypeCheck: Check struct type<br/>%Donation{}
    TypeCheck->>Impl: Route to Donation impl
    Impl->>Impl: Execute audit_trail/1
    Impl->>Impl: Access struct fields<br/>format audit string
    Impl-->>Protocol: Return audit trail string
    Protocol-->>Client: Return result

    Note over Protocol,Impl: Polymorphic dispatch based<br/>on runtime type

    style Client fill:#0173B2,stroke:#023B5A,color:#FFF
    style Protocol fill:#DE8F05,stroke:#8A5903,color:#FFF
    style TypeCheck fill:#DE8F05,stroke:#8A5903,color:#FFF
    style Impl fill:#029E73,stroke:#01593F,color:#FFF
```

### Defining Protocols

A protocol defines a contract of functions that can be implemented for different data types:

```elixir
defprotocol FinancialDomain.Auditable do
  @moduledoc """
  Protocol for entities that can be audited for compliance.
  """

  @doc """
  Returns a human-readable audit trail for the entity.
  """
  @spec audit_trail(t()) :: String.t()
  def audit_trail(entity)

  @doc """
  Returns all changes made to the entity since creation.
  """
  @spec change_history(t()) :: [map()]
  def change_history(entity)

  @doc """
  Validates the entity meets compliance requirements.
  """
  @spec validate_compliance(t()) :: {:ok, t()} | {:error, reason :: String.t()}
  def validate_compliance(entity)
end
```

### Implementing Protocols

Implement protocols for specific types using `defimpl`:

```elixir
# Implement for custom struct
defmodule FinancialDomain.Donation do
  @enforce_keys [:id, :amount, :donor_id, :campaign_id, :timestamp]
  defstruct [
    :id,
    :amount,
    :donor_id,
    :campaign_id,
    :timestamp,
    :status,
    changes: []
  ]

  @type t :: %__MODULE__{
          id: String.t(),
          amount: Money.t(),
          donor_id: String.t(),
          campaign_id: String.t(),
          timestamp: DateTime.t(),
          status: :pending | :completed | :failed,
          changes: [map()]
        }
end

defimpl FinancialDomain.Auditable, for: FinancialDomain.Donation do
  def audit_trail(%{id: id, amount: amount, donor_id: donor_id, timestamp: timestamp}) do
    """
    Donation Audit Trail
    ====================
    ID: #{id}
    Amount: #{Money.to_string(amount)}
    Donor: #{donor_id}
    Timestamp: #{DateTime.to_iso8601(timestamp)}
    """
  end

  def change_history(%{changes: changes}) do
    changes
  end

  def validate_compliance(donation) do
    with :ok <- validate_amount(donation),
         :ok <- validate_timestamp(donation),
         :ok <- validate_references(donation) do
      {:ok, donation}
    end
  end

  defp validate_amount(%{amount: %Money{amount: amount}}) when amount > 0, do: :ok
  defp validate_amount(_), do: {:error, "Donation amount must be positive"}

  defp validate_timestamp(%{timestamp: timestamp}) do
    if DateTime.compare(timestamp, DateTime.utc_now()) == :lt do
      :ok
    else
      {:error, "Donation timestamp cannot be in the future"}
    end
  end

  defp validate_references(%{donor_id: donor_id, campaign_id: campaign_id})
       when is_binary(donor_id) and is_binary(campaign_id) do
    :ok
  end

  defp validate_references(_), do: {:error, "Invalid donor or campaign reference"}
end

# Implement for another type
defmodule FinancialDomain.ZakatPayment do
  @enforce_keys [:id, :amount, :payer_id, :wealth_type, :timestamp]
  defstruct [:id, :amount, :payer_id, :wealth_type, :timestamp, :calculation_method, changes: []]

  @type t :: %__MODULE__{
          id: String.t(),
          amount: Money.t(),
          payer_id: String.t(),
          wealth_type: :gold | :silver | :cash | :business,
          timestamp: DateTime.t(),
          calculation_method: String.t() | nil,
          changes: [map()]
        }
end

defimpl FinancialDomain.Auditable, for: FinancialDomain.ZakatPayment do
  def audit_trail(%{id: id, amount: amount, payer_id: payer_id, wealth_type: wealth_type}) do
    """
    Zakat Payment Audit Trail
    ==========================
    ID: #{id}
    Amount: #{Money.to_string(amount)}
    Payer: #{payer_id}
    Wealth Type: #{wealth_type}
    """
  end

  def change_history(%{changes: changes}) do
    changes
  end

  def validate_compliance(payment) do
    with :ok <- validate_amount(payment),
         :ok <- validate_wealth_type(payment) do
      {:ok, payment}
    end
  end

  defp validate_amount(%{amount: %Money{amount: amount}}) when amount > 0, do: :ok
  defp validate_amount(_), do: {:error, "Zakat amount must be positive"}

  defp validate_wealth_type(%{wealth_type: type})
       when type in [:gold, :silver, :cash, :business] do
    :ok
  end

  defp validate_wealth_type(_), do: {:error, "Invalid wealth type"}
end
```

Using protocols with different types:

```elixir
# Works with any type that implements Auditable
donation = %FinancialDomain.Donation{
  id: "don_001",
  amount: Money.new(10000, :IDR),
  donor_id: "donor_123",
  campaign_id: "camp_456",
  timestamp: DateTime.utc_now(),
  status: :completed,
  changes: [
    %{field: :status, from: :pending, to: :completed, at: DateTime.utc_now()}
  ]
}

zakat = %FinancialDomain.ZakatPayment{
  id: "zkt_001",
  amount: Money.new(25000, :IDR),
  payer_id: "payer_789",
  wealth_type: :cash,
  timestamp: DateTime.utc_now()
}

# Protocol dispatches to correct implementation
IO.puts(FinancialDomain.Auditable.audit_trail(donation))
IO.puts(FinancialDomain.Auditable.audit_trail(zakat))

# Validate compliance for both
{:ok, validated_donation} = FinancialDomain.Auditable.validate_compliance(donation)
{:ok, validated_zakat} = FinancialDomain.Auditable.validate_compliance(zakat)
```

### Built-in Protocols

Elixir includes several built-in protocols:

```elixir
# String.Chars - converts to string (used by to_string/1)
defimpl String.Chars, for: FinancialDomain.Donation do
  def to_string(%{id: id, amount: amount, donor_id: donor_id}) do
    "Donation[#{id}]: #{Money.to_string(amount)} from #{donor_id}"
  end
end

# Inspect - customizes inspect output (used by inspect/1)
defimpl Inspect, for: FinancialDomain.Donation do
  import Inspect.Algebra

  def inspect(donation, opts) do
    concat([
      "#Donation<",
      to_doc(donation.id, opts),
      ", ",
      to_doc(Money.to_string(donation.amount), opts),
      ">"
    ])
  end
end

# Enumerable - makes data structure enumerable
defmodule FinancialDomain.DonationBatch do
  defstruct donations: []

  @type t :: %__MODULE__{donations: [FinancialDomain.Donation.t()]}
end

defimpl Enumerable, for: FinancialDomain.DonationBatch do
  def count(%{donations: donations}) do
    {:ok, length(donations)}
  end

  def member?(%{donations: donations}, element) do
    {:ok, element in donations}
  end

  def slice(%{donations: donations}) do
    size = length(donations)
    {:ok, size, &Enumerable.List.slice(donations, &1, &2, size)}
  end

  def reduce(%{donations: donations}, acc, fun) do
    Enumerable.List.reduce(donations, acc, fun)
  end
end

# Now can use Enum functions
batch = %FinancialDomain.DonationBatch{donations: [donation]}
Enum.count(batch)  # => 1
Enum.map(batch, & &1.amount)  # => [%Money{amount: 10000, currency: :IDR}]
```

### Protocol Consolidation

For performance in production, consolidate protocols:

```elixir
# In mix.exs
def project do
  [
    # ...
    consolidate_protocols: Mix.env() != :test
  ]
end
```

Consolidation benefits:

- **Faster dispatch**: Protocol calls become direct function calls
- **Smaller code size**: Removes dynamic dispatch overhead
- **Better inlining**: Compiler can optimize better

Check consolidation status:

```elixir
# In production
FinancialDomain.Auditable.__protocol__(:consolidated?)  # => true

# In development
FinancialDomain.Auditable.__protocol__(:consolidated?)  # => false
```

## Behaviours

Behaviours define **compile-time contracts** that modules must implement. They're similar to interfaces or abstract classes in object-oriented languages.

The following diagram shows the behaviour callback structure:

```mermaid
graph TD
    Behaviour[PaymentGateway Behaviour<br/>@callback Definitions]

    CB1[@callback init_payment<br/>amount, metadata<br/>→ {:ok, tx_id} or {:error, reason}]
    CB2[@callback process_payment<br/>transaction_id<br/>→ {:ok, result} or {:error, reason}]
    CB3[@callback refund_payment<br/>transaction_id, amount<br/>→ {:ok, refund_id} or {:error, reason}]
    CB4[@callback get_status<br/>transaction_id<br/>→ {:ok, status} or {:error, reason}]
    CB5[@callback validate_credentials<br/>credentials<br/>→ :ok or {:error, reason}]

    Impl1[Stripe Implementation<br/>@behaviour PaymentGateway<br/>@impl true]
    Impl2[Midtrans Implementation<br/>@behaviour PaymentGateway<br/>@impl true]
    Impl3[Custom Gateway<br/>@behaviour PaymentGateway<br/>@impl true]

    Behaviour --> CB1
    Behaviour --> CB2
    Behaviour --> CB3
    Behaviour --> CB4
    Behaviour --> CB5

    CB1 -.->|must implement| Impl1
    CB2 -.->|must implement| Impl1
    CB3 -.->|must implement| Impl1
    CB4 -.->|must implement| Impl1
    CB5 -.->|must implement| Impl1

    CB1 -.->|must implement| Impl2
    CB2 -.->|must implement| Impl2
    CB3 -.->|must implement| Impl2
    CB4 -.->|must implement| Impl2
    CB5 -.->|must implement| Impl2

    CB1 -.->|must implement| Impl3
    CB2 -.->|must implement| Impl3
    CB3 -.->|must implement| Impl3
    CB4 -.->|must implement| Impl3
    CB5 -.->|must implement| Impl3

    Note1[Compile-time verification:<br/>Missing callbacks = error ❌]
    Behaviour -.-> Note1

    style Behaviour fill:#0173B2,stroke:#023B5A,color:#FFF
    style CB1 fill:#DE8F05,stroke:#8A5903,color:#FFF
    style CB2 fill:#DE8F05,stroke:#8A5903,color:#FFF
    style CB3 fill:#DE8F05,stroke:#8A5903,color:#FFF
    style CB4 fill:#DE8F05,stroke:#8A5903,color:#FFF
    style CB5 fill:#DE8F05,stroke:#8A5903,color:#FFF
    style Impl1 fill:#029E73,stroke:#01593F,color:#FFF
    style Impl2 fill:#029E73,stroke:#01593F,color:#FFF
    style Impl3 fill:#029E73,stroke:#01593F,color:#FFF
    style Note1 fill:#CC78BC,stroke:#8E5484,color:#FFF
```

### Defining Behaviours

Define a behaviour using `@callback`:

```elixir
defmodule FinancialDomain.PaymentGateway do
  @moduledoc """
  Behaviour for payment gateway integrations.
  """

  @doc """
  Initializes a payment transaction.
  """
  @callback init_payment(amount :: Money.t(), metadata :: map()) ::
              {:ok, transaction_id :: String.t()} | {:error, reason :: atom()}

  @doc """
  Processes a payment transaction.
  """
  @callback process_payment(transaction_id :: String.t()) ::
              {:ok, result :: map()} | {:error, reason :: atom()}

  @doc """
  Refunds a completed payment.
  """
  @callback refund_payment(transaction_id :: String.t(), amount :: Money.t()) ::
              {:ok, refund_id :: String.t()} | {:error, reason :: atom()}

  @doc """
  Retrieves payment status.
  """
  @callback get_status(transaction_id :: String.t()) ::
              {:ok, status :: atom()} | {:error, reason :: atom()}

  @doc """
  Validates payment gateway credentials.
  """
  @callback validate_credentials(credentials :: map()) :: :ok | {:error, reason :: atom()}
end
```

### Implementing Behaviours

Implement a behaviour using `@behaviour`:

```elixir
defmodule FinancialDomain.PaymentGateway.Stripe do
  @behaviour FinancialDomain.PaymentGateway

  @impl true
  def init_payment(amount, metadata) do
    # Stripe API call
    with {:ok, intent} <- create_payment_intent(amount, metadata) do
      {:ok, intent.id}
    end
  end

  @impl true
  def process_payment(transaction_id) do
    # Confirm payment intent
    with {:ok, intent} <- confirm_payment_intent(transaction_id) do
      {:ok, %{status: intent.status, amount: intent.amount}}
    end
  end

  @impl true
  def refund_payment(transaction_id, amount) do
    # Create refund
    with {:ok, refund} <- create_refund(transaction_id, amount) do
      {:ok, refund.id}
    end
  end

  @impl true
  def get_status(transaction_id) do
    # Retrieve payment intent
    with {:ok, intent} <- retrieve_payment_intent(transaction_id) do
      {:ok, intent.status}
    end
  end

  @impl true
  def validate_credentials(%{api_key: api_key}) when is_binary(api_key) do
    # Validate Stripe API key
    case test_api_key(api_key) do
      :ok -> :ok
      _error -> {:error, :invalid_credentials}
    end
  end

  def validate_credentials(_), do: {:error, :missing_api_key}

  # Private functions for Stripe API calls
  defp create_payment_intent(_amount, _metadata), do: {:ok, %{id: "pi_123"}}
  defp confirm_payment_intent(_id), do: {:ok, %{status: :succeeded, amount: 10000}}
  defp create_refund(_transaction_id, _amount), do: {:ok, %{id: "re_123"}}
  defp retrieve_payment_intent(_id), do: {:ok, %{status: :succeeded}}
  defp test_api_key(_key), do: :ok
end

defmodule FinancialDomain.PaymentGateway.Midtrans do
  @behaviour FinancialDomain.PaymentGateway

  @impl true
  def init_payment(amount, metadata) do
    # Midtrans API call
    {:ok, "mt_txn_#{:rand.uniform(1000)}"}
  end

  @impl true
  def process_payment(transaction_id) do
    # Process via Midtrans
    {:ok, %{status: :settlement, amount: 10000}}
  end

  @impl true
  def refund_payment(transaction_id, amount) do
    # Midtrans refund
    {:ok, "mt_ref_#{:rand.uniform(1000)}"}
  end

  @impl true
  def get_status(transaction_id) do
    {:ok, :settlement}
  end

  @impl true
  def validate_credentials(%{server_key: key, client_key: _}) when is_binary(key) do
    :ok
  end

  def validate_credentials(_), do: {:error, :missing_credentials}
end
```

### Behaviour Callbacks

Callbacks can have multiple arities and guards:

```elixir
defmodule FinancialDomain.ZakatCalculator do
  @moduledoc """
  Behaviour for Zakat calculation methods.
  """

  @doc """
  Calculates Zakat for given wealth and nisab.
  """
  @callback calculate(wealth :: Money.t(), nisab :: Money.t()) ::
              {:ok, zakat :: Money.t()} | {:error, reason :: atom()}

  @doc """
  Calculates Zakat with custom rate (for different madhabs).
  """
  @callback calculate(wealth :: Money.t(), nisab :: Money.t(), rate :: Decimal.t()) ::
              {:ok, zakat :: Money.t()} | {:error, reason :: atom()}

  @doc """
  Returns the nisab value for the wealth type.
  """
  @callback nisab_value(wealth_type :: atom()) :: Money.t()

  @doc """
  Validates wealth meets Zakat requirements.
  """
  @callback validate_eligibility(wealth :: Money.t(), nisab :: Money.t()) ::
              {:ok, :eligible} | {:error, :below_nisab}
end

defmodule FinancialDomain.ZakatCalculator.Standard do
  @behaviour FinancialDomain.ZakatCalculator

  @standard_rate Decimal.new("0.025")  # 2.5%

  @impl true
  def calculate(wealth, nisab) do
    calculate(wealth, nisab, @standard_rate)
  end

  @impl true
  def calculate(wealth, nisab, rate) do
    with {:ok, :eligible} <- validate_eligibility(wealth, nisab),
         {:ok, currencies_match} <- validate_currencies(wealth, nisab) do
      zakat_amount = Money.multiply(wealth, rate)
      {:ok, zakat_amount}
    end
  end

  @impl true
  def nisab_value(:gold) do
    # 85 grams of gold (approximately)
    Money.new(85_000_000, :IDR)
  end

  def nisab_value(:silver) do
    # 595 grams of silver (approximately)
    Money.new(5_000_000, :IDR)
  end

  def nisab_value(:cash), do: nisab_value(:silver)

  @impl true
  def validate_eligibility(wealth, nisab) do
    if Money.compare(wealth, nisab) == :gt do
      {:ok, :eligible}
    else
      {:error, :below_nisab}
    end
  end

  defp validate_currencies(%Money{currency: c1}, %Money{currency: c2}) when c1 == c2 do
    {:ok, true}
  end

  defp validate_currencies(_, _), do: {:error, :currency_mismatch}
end
```

### Optional Callbacks

Mark callbacks as optional using `@optional_callbacks`:

```elixir
defmodule FinancialDomain.ReportGenerator do
  @callback generate_report(data :: map()) :: {:ok, binary()} | {:error, atom()}

  @callback format_report(data :: map(), format :: atom()) ::
              {:ok, binary()} | {:error, atom()}

  # Optional: custom header/footer
  @callback custom_header(data :: map()) :: String.t()
  @callback custom_footer(data :: map()) :: String.t()

  @optional_callbacks custom_header: 1, custom_footer: 1

  def generate(module, data, format \\ :pdf) do
    with {:ok, content} <- module.format_report(data, format) do
      header = if function_exported?(module, :custom_header, 1) do
        module.custom_header(data)
      else
        default_header()
      end

      footer = if function_exported?(module, :custom_footer, 1) do
        module.custom_footer(data)
      else
        default_footer()
      end

      {:ok, "#{header}\n#{content}\n#{footer}"}
    end
  end

  defp default_header, do: "=== Financial Report ==="
  defp default_footer, do: "=== End of Report ==="
end

defmodule FinancialDomain.ReportGenerator.DonationReport do
  @behaviour FinancialDomain.ReportGenerator

  @impl true
  def generate_report(data) do
    format_report(data, :pdf)
  end

  @impl true
  def format_report(data, :pdf) do
    # Generate PDF
    {:ok, "PDF content"}
  end

  def format_report(data, :csv) do
    # Generate CSV
    {:ok, "CSV content"}
  end

  # Optional callback implemented
  @impl true
  def custom_header(%{campaign: campaign}) do
    "=== Donation Report for #{campaign} ==="
  end

  # custom_footer not implemented - will use default
end
```

## Protocols vs Behaviours

Choose between protocols and behaviours based on your needs:

| Aspect                | Protocols                         | Behaviours                      |
| --------------------- | --------------------------------- | ------------------------------- |
| **Polymorphism Type** | Ad-hoc (data-based)               | Compile-time (module-based)     |
| **Extension Point**   | Data types                        | Modules                         |
| **Dispatch**          | Runtime (based on type)           | Compile-time (module selection) |
| **Use Case**          | Extending existing types          | Defining module contracts       |
| **Example**           | `String.Chars`, `Enumerable`      | `GenServer`, `Plug`             |
| **Verification**      | Runtime errors if not implemented | Compiler warnings               |

**Use Protocols when**:

- You need to add functionality to existing types
- Dispatch should be based on data type
- You want open extension (anyone can implement)
- You're working with heterogeneous collections

**Use Behaviours when**:

- You're defining module contracts
- You need compile-time verification
- You're building plugin systems
- You want to swap implementations (strategy pattern)

Example combining both:

```elixir
# Behaviour for payment processors
defmodule PaymentProcessor do
  @callback process(amount :: Money.t()) :: {:ok, result :: map()} | {:error, atom()}
end

# Protocol for auditable entities
defprotocol Auditable do
  def audit_trail(entity)
end

# Use both
defmodule CreditCardProcessor do
  @behaviour PaymentProcessor

  @impl true
  def process(amount) do
    # Process credit card payment
    {:ok, %{transaction_id: "cc_123", amount: amount}}
  end
end

defmodule Payment do
  defstruct [:id, :amount, :processor, :timestamp]
end

defimpl Auditable, for: Payment do
  def audit_trail(payment) do
    "Payment #{payment.id} processed via #{payment.processor}"
  end
end

# Use behaviour for processing
{:ok, result} = CreditCardProcessor.process(Money.new(10000, :IDR))

# Use protocol for auditing
payment = %Payment{id: "pay_001", amount: Money.new(10000, :IDR), processor: "CreditCard", timestamp: DateTime.utc_now()}
Auditable.audit_trail(payment)
```

## Common Patterns

### Strategy Pattern with Behaviours

Use behaviours to implement the strategy pattern:

```elixir
defmodule FinancialDomain.TaxCalculator do
  @callback calculate_tax(income :: Money.t(), deductions :: Money.t()) ::
              {:ok, tax :: Money.t()} | {:error, atom()}
end

defmodule FinancialDomain.TaxCalculator.Progressive do
  @behaviour FinancialDomain.TaxCalculator

  @impl true
  def calculate_tax(income, deductions) do
    taxable_income = Money.subtract(income, deductions)
    tax = calculate_progressive_tax(taxable_income)
    {:ok, tax}
  end

  defp calculate_progressive_tax(%Money{amount: amount, currency: currency}) do
    tax_amount =
      cond do
        amount <= 50_000_000 -> Decimal.mult(amount, Decimal.new("0.05"))
        amount <= 250_000_000 -> Decimal.mult(amount, Decimal.new("0.15"))
        true -> Decimal.mult(amount, Decimal.new("0.25"))
      end

    Money.new(tax_amount, currency)
  end
end

defmodule FinancialDomain.TaxCalculator.Flat do
  @behaviour FinancialDomain.TaxCalculator

  @flat_rate Decimal.new("0.10")

  @impl true
  def calculate_tax(income, deductions) do
    taxable_income = Money.subtract(income, deductions)
    tax = Money.multiply(taxable_income, @flat_rate)
    {:ok, tax}
  end
end

# Context module uses strategy
defmodule FinancialDomain.Taxation do
  def calculate_tax(income, deductions, strategy \\ FinancialDomain.TaxCalculator.Progressive) do
    strategy.calculate_tax(income, deductions)
  end
end

# Use different strategies
income = Money.new(100_000_000, :IDR)
deductions = Money.new(10_000_000, :IDR)

{:ok, progressive_tax} = FinancialDomain.Taxation.calculate_tax(income, deductions)
{:ok, flat_tax} = FinancialDomain.Taxation.calculate_tax(income, deductions, FinancialDomain.TaxCalculator.Flat)
```

### Adapter Pattern with Protocols

Use protocols to implement the adapter pattern:

```elixir
# Protocol for converting to Money
defprotocol FinancialDomain.Convertible do
  @doc "Converts value to Money"
  def to_money(value)
end

# Adapt integer
defimpl FinancialDomain.Convertible, for: Integer do
  def to_money(value) when value >= 0 do
    Money.new(value, :IDR)
  end
end

# Adapt map
defimpl FinancialDomain.Convertible, for: Map do
  def to_money(%{amount: amount, currency: currency}) do
    Money.new(amount, currency)
  end

  def to_money(%{value: value}) do
    Money.new(value, :IDR)
  end
end

# Adapt tuple
defimpl FinancialDomain.Convertible, for: Tuple do
  def to_money({amount, currency}) do
    Money.new(amount, currency)
  end
end

# Now can accept various formats
defmodule FinancialDomain.Donations do
  alias FinancialDomain.Convertible

  def create_donation(amount, donor_id) do
    money = Convertible.to_money(amount)
    # Create donation with Money
    %{amount: money, donor_id: donor_id}
  end
end

# Works with multiple formats
FinancialDomain.Donations.create_donation(10000, "donor_123")
FinancialDomain.Donations.create_donation({10000, :USD}, "donor_456")
FinancialDomain.Donations.create_donation(%{amount: 10000, currency: :IDR}, "donor_789")
```

### Plugin Systems

Combine behaviours with dynamic module loading:

```elixir
defmodule FinancialDomain.Plugin do
  @callback name() :: String.t()
  @callback version() :: String.t()
  @callback execute(params :: map()) :: {:ok, result :: any()} | {:error, atom()}
end

defmodule FinancialDomain.PluginRegistry do
  use GenServer

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, :ok, Keyword.put(opts, :name, __MODULE__))
  end

  def register_plugin(module) do
    GenServer.call(__MODULE__, {:register, module})
  end

  def list_plugins do
    GenServer.call(__MODULE__, :list)
  end

  def execute_plugin(name, params) do
    GenServer.call(__MODULE__, {:execute, name, params})
  end

  # Callbacks
  def init(:ok) do
    {:ok, %{}}
  end

  def handle_call({:register, module}, _from, state) do
    if validate_plugin(module) do
      name = module.name()
      {:reply, :ok, Map.put(state, name, module)}
    else
      {:reply, {:error, :invalid_plugin}, state}
    end
  end

  def handle_call(:list, _from, state) do
    plugins = Enum.map(state, fn {name, module} ->
      %{name: name, version: module.version()}
    end)
    {:reply, plugins, state}
  end

  def handle_call({:execute, name, params}, _from, state) do
    case Map.fetch(state, name) do
      {:ok, module} ->
        result = module.execute(params)
        {:reply, result, state}
      :error ->
        {:reply, {:error, :plugin_not_found}, state}
    end
  end

  defp validate_plugin(module) do
    Code.ensure_loaded?(module) and
      function_exported?(module, :name, 0) and
      function_exported?(module, :version, 0) and
      function_exported?(module, :execute, 1)
  end
end

# Example plugin
defmodule FinancialDomain.Plugin.ZakatCalculator do
  @behaviour FinancialDomain.Plugin

  @impl true
  def name, do: "zakat_calculator"

  @impl true
  def version, do: "1.0.0"

  @impl true
  def execute(%{wealth: wealth, nisab: nisab}) do
    if Money.compare(wealth, nisab) == :gt do
      zakat = Money.multiply(wealth, Decimal.new("0.025"))
      {:ok, %{zakat: zakat}}
    else
      {:error, :below_nisab}
    end
  end
end

# Use plugin system
{:ok, _pid} = FinancialDomain.PluginRegistry.start_link()
:ok = FinancialDomain.PluginRegistry.register_plugin(FinancialDomain.Plugin.ZakatCalculator)

plugins = FinancialDomain.PluginRegistry.list_plugins()
# => [%{name: "zakat_calculator", version: "1.0.0"}]

{:ok, result} = FinancialDomain.PluginRegistry.execute_plugin(
  "zakat_calculator",
  %{wealth: Money.new(100_000_000, :IDR), nisab: Money.new(85_000_000, :IDR)}
)
```

## Financial Domain Examples

Complete example integrating protocols and behaviours:

```elixir
# Behaviour for transaction processors
defmodule FinancialDomain.TransactionProcessor do
  @callback process(transaction :: map()) :: {:ok, result :: map()} | {:error, atom()}
  @callback validate(transaction :: map()) :: :ok | {:error, atom()}
  @callback rollback(transaction_id :: String.t()) :: :ok | {:error, atom()}
end

# Protocol for serializable entities
defprotocol FinancialDomain.Serializable do
  @doc "Serializes entity to JSON-compatible map"
  def to_json(entity)

  @doc "Deserializes from JSON-compatible map"
  def from_json(data)
end

# Donation struct
defmodule FinancialDomain.Donation do
  defstruct [:id, :amount, :donor_id, :campaign_id, :timestamp, :status]

  @type t :: %__MODULE__{
          id: String.t(),
          amount: Money.t(),
          donor_id: String.t(),
          campaign_id: String.t(),
          timestamp: DateTime.t(),
          status: atom()
        }
end

# Implement protocol for Donation
defimpl FinancialDomain.Serializable, for: FinancialDomain.Donation do
  def to_json(donation) do
    %{
      id: donation.id,
      amount: %{
        value: Money.to_decimal(donation.amount),
        currency: donation.amount.currency
      },
      donor_id: donation.donor_id,
      campaign_id: donation.campaign_id,
      timestamp: DateTime.to_iso8601(donation.timestamp),
      status: to_string(donation.status)
    }
  end

  def from_json(data) do
    %FinancialDomain.Donation{
      id: data["id"],
      amount: Money.new(data["amount"]["value"], String.to_atom(data["amount"]["currency"])),
      donor_id: data["donor_id"],
      campaign_id: data["campaign_id"],
      timestamp: DateTime.from_iso8601(data["timestamp"]) |> elem(1),
      status: String.to_atom(data["status"])
    }
  end
end

# Implement transaction processor behaviour
defmodule FinancialDomain.TransactionProcessor.DonationProcessor do
  @behaviour FinancialDomain.TransactionProcessor

  @impl true
  def process(transaction) do
    with :ok <- validate(transaction),
         {:ok, payment_result} <- process_payment(transaction),
         {:ok, _donation} <- record_donation(transaction, payment_result) do
      {:ok, %{status: :completed, transaction_id: transaction.id}}
    end
  end

  @impl true
  def validate(%{amount: amount, donor_id: donor_id, campaign_id: campaign_id})
      when is_binary(donor_id) and is_binary(campaign_id) do
    if Money.positive?(amount) do
      :ok
    else
      {:error, :invalid_amount}
    end
  end

  def validate(_), do: {:error, :invalid_transaction}

  @impl true
  def rollback(transaction_id) do
    # Reverse transaction
    :ok
  end

  defp process_payment(_transaction), do: {:ok, %{payment_id: "pay_123"}}
  defp record_donation(_transaction, _payment_result), do: {:ok, %{}}
end

# Use both protocol and behaviour
defmodule FinancialDomain.TransactionService do
  alias FinancialDomain.{Serializable, TransactionProcessor}

  def process_and_serialize(transaction, processor) do
    with {:ok, result} <- processor.process(transaction),
         json <- Serializable.to_json(result) do
      {:ok, json}
    end
  end
end

# Example usage
donation = %FinancialDomain.Donation{
  id: "don_001",
  amount: Money.new(50000, :IDR),
  donor_id: "donor_123",
  campaign_id: "camp_456",
  timestamp: DateTime.utc_now(),
  status: :pending
}

# Serialize
json = FinancialDomain.Serializable.to_json(donation)

# Process transaction
{:ok, result} = FinancialDomain.TransactionProcessor.DonationProcessor.process(donation)
```

## Best Practices

1. **Protocol Design**:

```elixir
# ✅ Good - focused protocol
defprotocol Validatable do
  def validate(data)
end

# ❌ Bad - too many responsibilities
defprotocol DataManagement do
  def validate(data)
  def save(data)
  def delete(data)
  def export(data)
end
```

1. **Behaviour Documentation**:

```elixir
# ✅ Good - well-documented behaviour
defmodule PaymentGateway do
  @moduledoc """
  Behaviour for payment gateway integrations.

  ## Example Implementation

      defmodule MyGateway do
        @behaviour PaymentGateway

        @impl true
        def charge(amount, card) do
          # Implementation
        end
      end
  """

  @doc """
  Charges the given amount to the card.

  Returns `{:ok, transaction_id}` on success or `{:error, reason}` on failure.
  """
  @callback charge(amount :: Money.t(), card :: map()) ::
              {:ok, String.t()} | {:error, atom()}
end
```

1. **Use @impl true**:

```elixir
# ✅ Good - marks implementation explicitly
defmodule MyGateway do
  @behaviour PaymentGateway

  @impl true
  def charge(amount, card) do
    # Implementation
  end
end

# ❌ Bad - missing @impl
defmodule MyGateway do
  @behaviour PaymentGateway

  def charge(amount, card) do
    # Typo in function name won't be caught
  end
end
```

1. **Protocol Fallbacks**:

```elixir
# Define fallback for Any
defprotocol Auditable do
  @fallback_to_any true
  def audit_trail(data)
end

defimpl Auditable, for: Any do
  def audit_trail(data) do
    "#{inspect(data.__struct__)}: #{inspect(data)}"
  end
end
```

## Anti-patterns

### 1. God Protocols

```elixir
# ❌ Bad - protocol trying to do everything
defprotocol EntityManager do
  def create(entity)
  def read(entity)
  def update(entity)
  def delete(entity)
  def validate(entity)
  def serialize(entity)
  def deserialize(data)
end

# ✅ Good - focused protocols
defprotocol Persistable do
  def save(entity)
  def load(id)
end

defprotocol Validatable do
  def validate(entity)
end

defprotocol Serializable do
  def to_json(entity)
  def from_json(data)
end
```

### 2. Protocol Over-engineering

```elixir
# ❌ Bad - protocol for single implementation
defprotocol DonationFormatter do
  def format(donation)
end

defimpl DonationFormatter, for: Donation do
  def format(donation), do: "#{donation.id}: #{donation.amount}"
end

# ✅ Good - simple function
defmodule DonationFormatter do
  def format(%Donation{id: id, amount: amount}) do
    "#{id}: #{amount}"
  end
end
```

### 3. Missing Behaviour Validation

```elixir
# ❌ Bad - no behaviour, runtime errors
defmodule PaymentService do
  def process(gateway, amount) do
    # Hope gateway has charge/2
    gateway.charge(amount, %{})
  end
end

# ✅ Good - behaviour enforces contract
defmodule PaymentGateway do
  @callback charge(amount :: Money.t(), card :: map()) :: {:ok, String.t()} | {:error, atom()}
end

defmodule PaymentService do
  @spec process(module(), Money.t()) :: {:ok, String.t()} | {:error, atom()}
  def process(gateway, amount) when is_atom(gateway) do
    gateway.charge(amount, %{})
  end
end
```

## Testing

### Testing Protocols

```elixir
defmodule AuditableTest do
  use ExUnit.Case, async: true

  alias FinancialDomain.{Auditable, Donation}

  describe "Auditable protocol for Donation" do
    setup do
      donation = %Donation{
        id: "don_test",
        amount: Money.new(10000, :IDR),
        donor_id: "donor_test",
        campaign_id: "camp_test",
        timestamp: ~U[2025-01-23 10:00:00Z],
        status: :completed
      }

      {:ok, donation: donation}
    end

    test "audit_trail/1 returns formatted string", %{donation: donation} do
      trail = Auditable.audit_trail(donation)

      assert trail =~ "Donation Audit Trail"
      assert trail =~ "don_test"
      assert trail =~ "IDR 10000"
    end

    test "validate_compliance/1 accepts valid donation", %{donation: donation} do
      assert {:ok, ^donation} = Auditable.validate_compliance(donation)
    end

    test "validate_compliance/1 rejects negative amount" do
      donation = %Donation{
        id: "don_test",
        amount: Money.new(-1000, :IDR),
        donor_id: "donor_test",
        campaign_id: "camp_test",
        timestamp: ~U[2025-01-23 10:00:00Z],
        status: :pending
      }

      assert {:error, "Donation amount must be positive"} = Auditable.validate_compliance(donation)
    end
  end
end
```

### Testing Behaviours

```elixir
defmodule PaymentGatewayTest do
  use ExUnit.Case, async: true

  alias FinancialDomain.PaymentGateway

  # Test helper to verify behaviour implementation
  defp test_payment_gateway(gateway_module) do
    amount = Money.new(10000, :IDR)
    metadata = %{customer_id: "cust_123"}

    # Test init_payment
    assert {:ok, transaction_id} = gateway_module.init_payment(amount, metadata)
    assert is_binary(transaction_id)

    # Test process_payment
    assert {:ok, result} = gateway_module.process_payment(transaction_id)
    assert is_map(result)

    # Test get_status
    assert {:ok, status} = gateway_module.get_status(transaction_id)
    assert is_atom(status)

    # Test refund
    refund_amount = Money.new(5000, :IDR)
    assert {:ok, refund_id} = gateway_module.refund_payment(transaction_id, refund_amount)
    assert is_binary(refund_id)
  end

  test "Stripe gateway implements PaymentGateway behaviour" do
    test_payment_gateway(PaymentGateway.Stripe)
  end

  test "Midtrans gateway implements PaymentGateway behaviour" do
    test_payment_gateway(PaymentGateway.Midtrans)
  end
end
```

### Mocking with Behaviours

```elixir
# Test double implementing behaviour
defmodule MockPaymentGateway do
  @behaviour FinancialDomain.PaymentGateway

  @impl true
  def init_payment(_amount, _metadata) do
    {:ok, "mock_txn_123"}
  end

  @impl true
  def process_payment("mock_txn_123") do
    {:ok, %{status: :success}}
  end

  def process_payment(_) do
    {:error, :not_found}
  end

  @impl true
  def refund_payment(_transaction_id, _amount) do
    {:ok, "mock_ref_123"}
  end

  @impl true
  def get_status(_transaction_id) do
    {:ok, :completed}
  end

  @impl true
  def validate_credentials(_credentials) do
    :ok
  end
end

defmodule DonationServiceTest do
  use ExUnit.Case, async: true

  test "processes donation with mock gateway" do
    amount = Money.new(10000, :IDR)

    # Use mock in test
    {:ok, transaction_id} = MockPaymentGateway.init_payment(amount, %{})
    {:ok, result} = MockPaymentGateway.process_payment(transaction_id)

    assert result.status == :success
  end
end
```

## Performance Considerations

1. **Protocol Consolidation**: Always consolidate in production for better performance
2. **Behaviour Dispatch**: Zero overhead - direct function calls at compile time
3. **Protocol Dispatch**: Small runtime cost for type checking and dispatch
4. **Avoid Protocol Abuse**: Don't use protocols where simple functions suffice

```elixir
# Protocol consolidation config
# mix.exs
def project do
  [
    consolidate_protocols: Mix.env() != :test,
    # ...
  ]
end

# Benchmark protocol vs direct call
Benchee.run(%{
  "protocol call" => fn -> Auditable.audit_trail(donation) end,
  "direct call" => fn -> AuditableImpl.audit_trail(donation) end
})

# Results (consolidated):
# protocol call: ~1.2 µs
# direct call: ~1.0 µs
# Overhead: ~0.2 µs (16%)
```

## Related Topics

- [Functional Programming](ex-so-stla-el__functional-programming.md) - Functional approach to polymorphism
- [Testing](ex-so-stla-el__test-driven-development.md) - Testing protocols and behaviours
- [OTP: GenServer](ex-so-stla-el__otp-genserver.md) - GenServer behaviour implementation
- [Type Safety](ex-so-stla-el__type-safety.md) - Typespecs for callbacks

## Sources

- [Elixir Protocol Documentation](https://hexdocs.pm/elixir/Protocol.html)
- [Elixir Behaviour Module](https://hexdocs.pm/elixir/Module.html#module-behaviours)
- [José Valim - Protocols and Behaviours](https://blog.plataformatec.com.br/2015/05/introducing-reducers/)
- [Elixir School - Protocols](https://elixirschool.com/en/lessons/advanced/protocols)
- [Programming Elixir ≥ 1.6 - Dave Thomas](https://pragprog.com/titles/elixir16/programming-elixir-1-6/)
- [Protocol Consolidation Performance](https://hexdocs.pm/elixir/Protocol.html#module-consolidation)

---

**Last Updated**: 2026-01-23
**Elixir Version**: 1.12+ (baseline), 1.17+ (recommended), 1.19.0 (latest)
**Maintainers**: Platform Documentation Team
