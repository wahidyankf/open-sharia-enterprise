---
title: "Documentation Practices"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000025
description: "Comprehensive guide to Elixir documentation patterns including @moduledoc, @doc, ExDoc generation, and doctests"
tags: ["elixir", "documentation", "exdoc", "doctests", "best-practices"]
prev: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/code-quality-tools"
next: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/deployment-strategies"
---

## Module Documentation with @moduledoc

Use `@moduledoc` to document modules with comprehensive overviews and usage examples.

```elixir
defmodule DonationProcessing do
  @moduledoc """
  Processes charitable donations with Shariah compliance validation.

  # => Module for handling donation transactions
  # => Ensures Islamic finance principles (no riba, proper zakat)

  This module provides functions for:

  * Creating donation records
  * Validating Shariah compliance
  * Processing payment transactions
  * Generating tax receipts

  ## Examples

      iex> DonationProcessing.create_donation(%{amount: 1000, currency: "USD"})
      # => Creates donation with amount validation
      {:ok, %Donation{amount: 1000, currency: "USD", status: :pending}}
      # => Returns tuple with :ok atom and Donation struct

      iex> DonationProcessing.validate_shariah_compliance(%Donation{})
      # => Validates donation meets Islamic finance rules
      {:ok, :compliant}
      # => Returns :compliant status if validation passes
  """

  # Function definitions...
end
# => Complete module documentation visible on hex.pm
# => Examples run as doctests during testing
```

**@moduledoc patterns**:

- **Overview first**: Brief description of module purpose
- **Feature list**: Bullet points for main capabilities
- **Examples section**: Practical usage demonstrations
- **Links to related**: Reference connected modules

## Function Documentation with @doc

Use `@doc` to document individual functions with clear signatures and examples.

```elixir
defmodule DonationProcessing do
  @doc """
  Creates a new donation record with validation.

  # => Function creates donation and validates amount
  # => Returns {:ok, donation} or {:error, reason}

  ## Parameters

  * `attrs` - Map with donation attributes
    * `:amount` (required) - Donation amount (positive integer)
    * `:currency` (required) - ISO 4217 currency code
    * `:donor_id` (required) - UUID of donor
    * `:project_id` (optional) - Target project UUID

  ## Returns

  * `{:ok, %Donation{}}` - Successfully created donation
  * `{:error, changeset}` - Validation failed with errors

  ## Examples

      iex> create_donation(%{amount: 500, currency: "USD", donor_id: "abc123"})
      # => Creates donation with minimum required fields
      {:ok, %Donation{amount: 500, currency: "USD"}}
      # => Returns success tuple with Donation struct

      iex> create_donation(%{amount: -100, currency: "USD"})
      # => Negative amount fails validation
      {:error, %Ecto.Changeset{errors: [amount: {"must be positive", []}]}}
      # => Returns error tuple with changeset containing validation errors
  """
  @spec create_donation(map()) :: {:ok, Donation.t()} | {:error, Ecto.Changeset.t()}
  def create_donation(attrs) do
    # => Validates and creates donation record
    %Donation{}
    |> Donation.changeset(attrs)
    # => Applies validation rules from changeset
    |> Repo.insert()
    # => Persists to database and returns result
  end
end
```

**@doc patterns**:

- **Purpose statement**: What function does
- **Parameters section**: Detailed argument descriptions
- **Returns section**: All possible return values
- **Examples**: Show common and edge cases

## Doctests for Executable Documentation

Doctests run code examples from documentation as automated tests.

```elixir
defmodule DonationAmount do
  @doc """
  Converts donation amount to target currency.

  # => Currency conversion for international donations
  # => Uses current exchange rates

  ## Examples

      iex> DonationAmount.convert(1000, :usd, :idr, 15000.0)
      # => Converts 1000 USD to IDR using rate 15000
      15_000_000
      # => Returns IDR amount (integer, no decimal)

      iex> DonationAmount.convert(0, :usd, :idr, 15000.0)
      # => Zero amount converts to zero
      0
      # => Edge case: no conversion needed

      iex> DonationAmount.convert(100, :usd, :usd, 1.0)
      # => Same currency returns original amount
      100
      # => No conversion when currencies match
  """
  @spec convert(integer(), atom(), atom(), float()) :: integer()
  def convert(amount, from_currency, to_currency, rate) do
    # => Return original amount if same currency
    if from_currency == to_currency do
      amount
    else
      # => Apply exchange rate and round
      round(amount * rate)
      # => Returns integer amount in target currency
    end
  end
end
```

**Running doctests**:

```elixir
# test/donation_amount_test.exs
defmodule DonationAmountTest do
  use ExUnit.Case, async: true
  # => Enables parallel test execution

  doctest DonationAmount
  # => Runs all doctests from module documentation
  # => Each iex> example becomes a test case

  # Additional unit tests...
end
```

**Doctest patterns**:

- **Use `iex>` prompts**: Indicates interactive Elixir code
- **Show expected output**: Next line after prompt
- **Add inline comments**: Explain with `# =>` notation
- **Test edge cases**: Zero values, empty inputs, boundary conditions

## ExDoc Generation for hex.pm

ExDoc generates beautiful HTML documentation published to hex.pm.

```elixir
# mix.exs
defmodule DonationProcessing.MixProject do
  use Mix.Project

  def project do
    [
      app: :donation_processing,
      version: "0.1.0",
      # => Package version for hex.pm
      elixir: "~> 1.15",
      # => Minimum Elixir version requirement

      # Documentation
      name: "DonationProcessing",
      # => Display name in documentation
      source_url: "https://github.com/org/donation_processing",
      # => GitHub repository link
      homepage_url: "https://oseplatform.com/donation",
      # => Project homepage
      docs: [
        # => ExDoc configuration options
        main: "DonationProcessing",
        # => Landing page module
        logo: "assets/logo.png",
        # => Logo displayed in docs
        extras: ["README.md", "CHANGELOG.md", "guides/getting-started.md"],
        # => Additional markdown files
        groups_for_modules: [
          # => Organize modules into groups
          "Core": [
            DonationProcessing,
            DonationProcessing.Validator
          ],
          "Payment": [
            DonationProcessing.Payment,
            DonationProcessing.Receipt
          ]
        ],
        groups_for_functions: [
          # => Group functions by category
          "CRUD Operations": &(&1[:section] == :crud),
          "Validation": &(&1[:section] == :validation)
        ]
      ],

      deps: deps()
    ]
  end

  defp deps do
    [
      {:ex_doc, "~> 0.31", only: :dev, runtime: false}
      # => ExDoc dependency for documentation generation
      # => Only loaded in development, not production
    ]
  end
end
```

**Generating documentation**:

```bash
# Generate HTML documentation
mix docs
# => Builds documentation in doc/ directory
# => Creates searchable HTML with syntax highlighting

# Open documentation locally
open doc/index.html
# => View generated documentation in browser
# => Test appearance before publishing
```

**Publishing to hex.pm**:

```bash
# Publish package with documentation
mix hex.publish
# => Uploads package and documentation to hex.pm
# => Documentation becomes public at hexdocs.pm/donation_processing
```

## Documentation Best Practices

### What to Document

**Public functions (always)**:

```elixir
@doc """
Public API function - always document with @doc.
# => Users depend on this function
# => Needs clear documentation
"""
def public_function(arg), do: # ...
```

**Private functions (selectively)**:

```elixir
@doc false
# => Hides from public documentation
# => Still visible in source code
defp complex_internal_logic(data) do
  # Implementation with inline comments
  # Complex logic explained in comments
end
```

**Module overview (always)**:

```elixir
@moduledoc """
Every module needs @moduledoc explaining purpose.
# => Appears on hex.pm documentation
# => First thing users read
"""
```

### When to Document

**Before writing implementation**:

```elixir
# 1. Write documentation first (TDD for docs)
@doc """
Validates Shariah compliance of donation.
# => Write expected behavior first
"""

# 2. Then implement function
def validate_shariah_compliance(donation) do
  # Implementation follows documentation
end
```

**Document as you code**:

- Add `@moduledoc` when creating module
- Write `@doc` before implementing function
- Add doctests for examples
- Update docs when behavior changes

### Documentation Patterns

**Pattern 1: Example-driven documentation**:

```elixir
@doc """
Calculates zakat amount for donation.

## Examples

    iex> calculate_zakat(10000)
    # => 2.5% of donation amount
    250
    # => Zakat rate according to Shariah

    iex> calculate_zakat(0)
    # => No zakat for zero amount
    0
"""
def calculate_zakat(amount), do: div(amount * 25, 1000)
# => Simple implementation after clear examples
```

**Pattern 2: Parameter validation documentation**:

```elixir
@doc """
Processes donation with validation.

## Parameters

* `donation` - %Donation{} struct (required)
  * Must have positive `:amount`
  * Must have valid `:currency` (ISO 4217)
  * Must have `:donor_id` (UUID format)

## Raises

* `ArgumentError` - If donation invalid
* `Ecto.NoResultsError` - If donor not found
"""
def process_donation(donation) do
  # Implementation with validation
end
```

**Pattern 3: Type-driven documentation**:

```elixir
@typedoc """
Donation struct representing charitable contribution.

# => Custom type with field documentation

## Fields

* `:amount` - Positive integer in smallest currency unit
* `:currency` - Atom representing ISO 4217 code
* `:donor_id` - UUID string identifying donor
* `:status` - One of :pending, :completed, :failed
"""
@type t :: %__MODULE__{
  amount: pos_integer(),
  # => Amount in cents/fils/smallest unit
  currency: atom(),
  # => :usd, :eur, :idr, etc.
  donor_id: String.t(),
  # => UUID v4 format string
  status: :pending | :completed | :failed
  # => Current donation processing status
}
```

## Type Specifications as Documentation

Typespecs document function signatures and enable static analysis.

```elixir
defmodule DonationAPI do
  @typedoc "Donation creation attributes"
  @type donation_attrs :: %{
    amount: pos_integer(),
    # => Positive amount required
    currency: String.t(),
    # => ISO 4217 currency code
    donor_id: String.t(),
    # => Donor UUID
    project_id: String.t() | nil
    # => Optional project assignment
  }

  @typedoc "Result of donation processing"
  @type processing_result ::
    {:ok, Donation.t()}
    # => Success with donation record
    | {:error, :invalid_amount}
    # => Amount validation failed
    | {:error, :unsupported_currency}
    # => Currency not accepted
    | {:error, Ecto.Changeset.t()}
    # => General validation errors

  @doc """
  Creates and processes donation transaction.
  """
  @spec process_donation(donation_attrs()) :: processing_result()
  def process_donation(attrs) do
    # => Implementation with type safety
    # => Dialyzer can verify correctness
  end
end
```

**Typespec patterns**:

- `@type` for public types
- `@typep` for private types
- `@spec` for function signatures
- `@typedoc` for type documentation

## Financial Domain Example: Complete Documentation

```elixir
defmodule DonationReceipt do
  @moduledoc """
  Generates tax-deductible donation receipts.

  # => Module handles receipt generation for donors
  # => Complies with tax authority requirements

  Receipts include:

  * Donation amount and currency
  * Donor information
  * Organization tax ID
  * Receipt date and number
  * Tax deductibility statement

  ## Examples

      iex> donation = %Donation{amount: 5000, currency: "USD"}
      # => Create sample donation
      iex> DonationReceipt.generate(donation)
      # => Generate receipt PDF
      {:ok, %Receipt{number: "2025-0001", amount: 5000}}
      # => Returns receipt with unique number
  """

  @typedoc """
  Receipt record for donation transaction.

  # => Struct contains all receipt information
  # => Generated once per successful donation
  """
  @type t :: %__MODULE__{
    number: String.t(),
    # => Unique receipt number (format: YYYY-NNNN)
    donation_id: String.t(),
    # => Associated donation UUID
    amount: pos_integer(),
    # => Donation amount in smallest currency unit
    currency: String.t(),
    # => ISO 4217 currency code
    generated_at: DateTime.t(),
    # => Receipt generation timestamp (UTC)
    pdf_url: String.t() | nil
    # => S3 URL for PDF download (nil if not uploaded)
  }

  defstruct [:number, :donation_id, :amount, :currency, :generated_at, :pdf_url]

  @doc """
  Generates receipt for completed donation.

  # => Creates receipt record and PDF document
  # => Uploads to S3 storage

  ## Parameters

  * `donation` - %Donation{} with :completed status

  ## Returns

  * `{:ok, %Receipt{}}` - Receipt generated successfully
  * `{:error, :invalid_status}` - Donation not completed
  * `{:error, reason}` - PDF generation or upload failed

  ## Examples

      iex> donation = %Donation{id: "abc", amount: 1000, status: :completed}
      # => Completed donation ready for receipt
      iex> DonationReceipt.generate(donation)
      # => Generates receipt with PDF
      {:ok, %Receipt{number: "2025-0123", amount: 1000}}
      # => Returns receipt with unique number

      iex> pending = %Donation{status: :pending}
      # => Donation not yet completed
      iex> DonationReceipt.generate(pending)
      # => Cannot generate receipt for pending donation
      {:error, :invalid_status}
      # => Returns error tuple
  """
  @spec generate(Donation.t()) :: {:ok, t()} | {:error, atom()}
  def generate(%Donation{status: :completed} = donation) do
    # => Generate receipt number
    number = generate_receipt_number()
    # => Returns format "YYYY-NNNN"

    # => Create receipt record
    receipt = %__MODULE__{
      number: number,
      donation_id: donation.id,
      amount: donation.amount,
      currency: donation.currency,
      generated_at: DateTime.utc_now()
    }
    # => Receipt struct with donation data

    # => Generate PDF document
    with {:ok, pdf_binary} <- generate_pdf(receipt),
         # => PDF generation from template
         {:ok, url} <- upload_to_s3(pdf_binary, number) do
         # => Upload to cloud storage
      {:ok, %{receipt | pdf_url: url}}
      # => Return receipt with PDF URL
    end
  end

  def generate(%Donation{status: status}) do
    # => Invalid status handling
    {:error, :invalid_status}
    # => Returns error for non-completed donations
  end

  @doc false
  # => Private helper hidden from documentation
  defp generate_receipt_number do
    # => Implementation with inline comments
    year = DateTime.utc_now().year
    # => Current year for prefix
    sequence = get_next_sequence()
    # => Database sequence number

    "#{year}-#{String.pad_leading("#{sequence}", 4, "0")}"
    # => Format: "2025-0001"
  end
end
```

## Summary

Elixir documentation practices for production systems:

**Module documentation**:

- Use `@moduledoc` for comprehensive module overviews
- Include examples, features, and related modules
- Write documentation visible on hex.pm

**Function documentation**:

- Use `@doc` for public API functions
- Document parameters, returns, and examples
- Hide private functions with `@doc false`

**Doctests**:

- Write executable examples in documentation
- Test edge cases and common scenarios
- Run with `doctest ModuleName` in tests

**ExDoc generation**:

- Configure in mix.exs with extras and grouping
- Generate with `mix docs` for local review
- Publish to hex.pm with `mix hex.publish`

**Best practices**:

- Document before implementing (documentation-driven)
- Use typespecs for static analysis
- Keep examples realistic and tested
- Update documentation with code changes

**Documentation hierarchy**: @moduledoc → @doc → @typedoc → inline comments → doctests
