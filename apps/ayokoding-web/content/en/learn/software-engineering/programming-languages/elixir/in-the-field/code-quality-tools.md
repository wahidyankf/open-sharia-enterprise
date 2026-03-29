---
title: "Code Quality Tools"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000024
description: "Production code quality tools for Elixir: Credo for consistency, Dialyxir for type checking, Sobelow for security"
tags: ["elixir", "code-quality", "credo", "dialyxir", "sobelow", "static-analysis", "security"]
prev: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/test-driven-development"
next: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/documentation-practices"
---

**Need code quality enforcement for your Elixir codebase?** This guide covers industry-standard tools for maintaining consistency, catching type errors, and preventing security vulnerabilities in production systems.

## Standard Library Has No Linting

Elixir's standard library provides no built-in code quality or linting tools.

**Critical Limitation**: Without external tools, teams cannot:

- Enforce consistent code style across contributors
- Catch type inconsistencies before runtime
- Detect security vulnerabilities in web applications
- Automate quality checks in CI/CD pipelines
- Maintain code consistency as codebases grow

**The Solution**: Three essential tools provide comprehensive quality coverage:

- **Credo** - Static analysis for code consistency and best practices
- **Dialyxir** - Type checking via Dialyzer integration
- **Sobelow** - Security scanning for Phoenix applications

These tools are industry-standard for production Elixir systems.

## Financial Domain Examples

Examples use Shariah-compliant donation platform:

- **Zakat calculation** - Code quality for financial calculations
- **Donation tracking** - Type safety for monetary operations
- **Security scanning** - Vulnerability detection for payment processing

These demonstrate quality tools with real business requirements.

## Credo - Static Analysis

### What Credo Provides

Credo analyzes code for consistency violations, readability issues, and anti-patterns.

**Checks Categories**:

- **Consistency** - Naming conventions, code organization
- **Readability** - Complex functions, unclear logic
- **Refactoring opportunities** - Code smell detection
- **Design patterns** - Best practice violations
- **Warnings** - Potential bugs, deprecated usage

**Installation**:

```elixir
# mix.exs
defp deps do
  [
    {:credo, "~> 1.7", only: [:dev, :test], runtime: false}
  ]                                          # => only: [:dev, :test] prevents production inclusion
end                                          # => runtime: false - compile-time only
```

### Basic Usage

Run Credo to analyze entire codebase.

```elixir
# Run Credo analysis
mix credo                                    # => Analyzes all .ex and .exs files
                                             # => Groups issues by priority (high/normal/low)
                                             # => Suggests fixes for each violation

# Output includes:
# Consistency issues (naming, organization)
# Readability problems (complex functions)
# Refactoring opportunities (code smells)
# Design anti-patterns
```

### Strict Mode Analysis

Strict mode treats all suggestions as violations.

```elixir
# Strict analysis for CI/CD
mix credo --strict                           # => All suggestions become failures
                                             # => CI/CD pipeline fails on any issue
                                             # => Enforces maximum code quality

# Use in pre-commit hooks:
# Prevents committing code with quality issues
```

### Example: Detecting Code Smells

Credo identifies common anti-patterns.

```elixir
# Donation calculation module (BEFORE Credo)
defmodule ZakatCalculator do
  def calculate(amount, rate, adjustment, fee, discount) do
    # => 5 parameters - Credo flags this
    # => Violation: Too many function parameters
    # => Suggestion: Use struct or map for parameters

    result = amount * rate + adjustment - fee - discount
    # => Complex calculation without intermediate variables
    # => Violation: Complex expression hurts readability
    result
  end
end

# AFTER Credo suggestions applied:
defmodule ZakatCalculator do
  @moduledoc """
  Calculates zakat amounts for donations.
  """                                        # => Added module documentation
                                             # => Credo enforces @moduledoc on public modules

  defstruct [:amount, :rate, :adjustment, :fee, :discount]
                                             # => Replaced multiple parameters with struct
                                             # => Improved: Single parameter, clear structure

  def calculate(%__MODULE__{} = params) do   # => Pattern matches struct
    params
    |> apply_rate()                          # => Broke complex calculation into steps
    |> apply_adjustment()                    # => Each step has clear purpose
    |> apply_discount()                      # => Improves readability and testability
  end

  defp apply_rate(%{amount: amount, rate: rate} = params) do
    %{params | amount: amount * rate}       # => Intermediate calculations
  end                                        # => Each function does one thing

  defp apply_adjustment(%{amount: amount, adjustment: adj} = params) do
    %{params | amount: amount + adj}
  end

  defp apply_discount(%{amount: amount, fee: fee, discount: disc} = params) do
    %{params | amount: amount - fee - disc}
  end
end
```

### Configuration File

Customize Credo checks with `.credo.exs`.

```elixir
# .credo.exs in project root
%{
  configs: [
    %{
      name: "default",
      files: %{
        included: ["lib/", "test/"],         # => Analyze lib/ and test/
        excluded: ["deps/", "_build/"]       # => Skip dependencies and build artifacts
      },
      checks: [
        # Enable all default checks
        {Credo.Check.Consistency.TabsOrSpaces},
        {Credo.Check.Design.AliasUsage, false},
                                             # => Disable specific check
                                             # => AliasUsage can be too strict

        # Configure check parameters
        {Credo.Check.Refactor.FunctionArity, max_arity: 4},
                                             # => Limit function parameters to 4
                                             # => Enforces simpler function signatures

        {Credo.Check.Readability.ModuleDoc, false},
                                             # => Disable module doc requirement for tests
                                             # => Only for specific environments
      ]
    }
  ]
}
```

## Dialyxir - Type Checking

### What Dialyxir Provides

Dialyxir integrates Dialyzer (BEAM's type checker) into Elixir workflows.

**Capabilities**:

- **Type inconsistency detection** - Catches mismatched types
- **Dead code detection** - Finds unreachable code paths
- **Contract violations** - Validates `@spec` declarations
- **Cross-module analysis** - Checks types across application boundaries

**Installation**:

```elixir
# mix.exs
defp deps do
  [
    {:dialyxir, "~> 1.4", only: [:dev, :test], runtime: false}
  ]                                          # => only: [:dev, :test] - development tool
end                                          # => runtime: false - compile-time only
```

### Building PLT (Persistent Lookup Table)

Dialyzer requires building type database first.

```elixir
# Build PLT (one-time setup, ~5-10 minutes)
mix dialyzer --plt                           # => Creates PLT file with BEAM/OTP types
                                             # => Analyzes all dependencies
                                             # => Cached for future runs (only updates on changes)

# PLT stored in _build/dev/dialyxir_*       # => Reusable across analysis runs
```

### Basic Type Checking

Run Dialyzer analysis after PLT built.

```elixir
# Analyze entire application
mix dialyzer                                 # => Checks all modules against PLT
                                             # => Reports type inconsistencies
                                             # => Validates @spec declarations

# Typical analysis time: 30 seconds - 2 minutes
# Depends on codebase size
```

### Example: Detecting Type Errors

Dialyxir catches type mismatches at compile time.

```elixir
# Donation processor module
defmodule DonationProcessor do
  @spec process_amount(integer()) :: float() # => Declares integer input, float output
  def process_amount(amount) do
    # Convert to string for display
    to_string(amount)                        # => ERROR: Returns binary, not float!
  end                                        # => Dialyzer detects: @spec declares float return
                                             # => Actual return: binary (String.t())

  # Dialyzer output:
  # The @spec for process_amount/1 declares float() return
  # but the function returns binary()

  @spec calculate_zakat(float(), float()) :: float()
  def calculate_zakat(amount, rate) do
    amount * rate                            # => Correct: float * float = float
  end                                        # => Type check passes

  @spec validate_donation(map()) :: boolean()
  def validate_donation(%{amount: amount}) when amount > 0 do
    :ok                                      # => ERROR: Returns :ok atom, not boolean
  end                                        # => Dialyzer: Expected boolean, got :ok atom
  def validate_donation(_), do: false        # => Type check passes for this clause
end

# FIXED version with correct types:
defmodule DonationProcessor do
  @spec process_amount(integer()) :: String.t()
                                             # => Changed return type to String.t()
  def process_amount(amount) do
    to_string(amount)                        # => Now matches @spec declaration
  end

  @spec validate_donation(map()) :: :ok | :error
                                             # => Changed return type to atoms
  def validate_donation(%{amount: amount}) when amount > 0 do
    :ok                                      # => Matches @spec
  end
  def validate_donation(_), do: :error       # => Both clauses return declared types
end
```

### Dialyzer Configuration

Configure Dialyzer behavior in `mix.exs`.

```elixir
# mix.exs
def project do
  [
    dialyzer: [
      plt_add_apps: [:ex_unit, :mix],        # => Add applications to PLT
                                             # => Enables checking test code

      plt_file: {:no_warn, "priv/plts/dialyzer.plt"},
                                             # => Custom PLT location
                                             # => :no_warn suppresses warnings about old PLT

      flags: [
        :error_handling,                     # => Check error handling patterns
        :underspecs,                         # => Warn on overly permissive @specs
        :unmatched_returns                   # => Detect ignored function returns
      ]
    ]
  ]
end
```

## Sobelow - Security Scanning

### What Sobelow Provides

Sobelow scans Phoenix applications for security vulnerabilities.

**Detection Categories**:

- **SQL injection** - Unsafe query construction
- **XSS vulnerabilities** - Unescaped user input
- **CSRF protection gaps** - Missing CSRF tokens
- **Insecure dependencies** - Known vulnerable packages
- **Configuration issues** - Insecure settings

**Phoenix-Specific**: Designed exclusively for Phoenix framework applications.

**Installation**:

```elixir
# mix.exs
defp deps do
  [
    {:sobelow, "~> 0.13", only: [:dev, :test], runtime: false}
  ]                                          # => Security scanning tool
end                                          # => Development/test only
```

### Basic Security Scan

Run Sobelow to detect vulnerabilities.

```elixir
# Scan entire Phoenix application
mix sobelow                                  # => Analyzes all Phoenix-specific code
                                             # => Routes, controllers, templates
                                             # => Reports security issues with severity

# Output organized by severity:
# High: Critical vulnerabilities (SQL injection, XSS)
# Medium: Configuration issues
# Low: Best practice violations
```

### Verbose Mode Analysis

Detailed vulnerability information with `--verbose`.

```elixir
# Verbose security scan
mix sobelow --verbose                        # => Includes file paths and line numbers
                                             # => Shows vulnerable code snippets
                                             # => Provides remediation suggestions

# Example output:
# SQL Injection (High Severity)
# File: lib/app_web/controllers/donation_controller.ex:15
# Unsafe query construction with user input
# Recommendation: Use Ecto parameterized queries
```

### Example: SQL Injection Detection

Sobelow identifies unsafe database queries.

```elixir
# Donation controller (VULNERABLE)
defmodule AppWeb.DonationController do
  use AppWeb, :controller

  def search(conn, %{"amount" => amount}) do
    # VULNERABLE: String interpolation in SQL
    query = "SELECT * FROM donations WHERE amount > #{amount}"
                                             # => Sobelow HIGH: SQL injection vulnerability
                                             # => User input directly in SQL string
                                             # => Attacker can inject: "0; DROP TABLE donations--"

    result = Ecto.Adapters.SQL.query!(Repo, query)
                                             # => Executes unsafe query

    render(conn, "search.html", donations: result.rows)
  end

  # Sobelow output:
  # SQL Injection (High)
  # Unsafe SQL query with user input interpolation
  # Use Ecto.Query or parameterized queries
end

# FIXED version with safe queries:
defmodule AppWeb.DonationController do
  use AppWeb, :controller
  import Ecto.Query

  def search(conn, %{"amount" => amount}) do
    # SAFE: Ecto parameterized query
    query = from d in Donation,
            where: d.amount > ^amount        # => ^ interpolates safely
                                             # => Ecto escapes user input
                                             # => SQL injection impossible

    donations = Repo.all(query)              # => Executes safe query
                                             # => Sobelow: No issues detected

    render(conn, "search.html", donations: donations)
  end
end
```

### Example: XSS Prevention

Sobelow detects unescaped user input in templates.

```elixir
# Template (VULNERABLE)
# lib/app_web/templates/donation/show.html.heex
<div>
  Donor comment: <%= raw(@donation.comment) %>
                                             # => Sobelow HIGH: XSS vulnerability
                                             # => raw/1 disables HTML escaping
                                             # => User input rendered without sanitization
</div>
# Attacker comment: <script>alert('XSS')</script>
# Renders: <div>Donor comment: <script>alert('XSS')</script></div>
# Script executes in user's browser

# FIXED version with safe rendering:
# lib/app_web/templates/donation/show.html.heex
<div>
  Donor comment: <%= @donation.comment %>    # => Automatic HTML escaping
                                             # => Phoenix escapes all user input by default
                                             # => Sobelow: No issues detected
</div>
# Attacker comment: <script>alert('XSS')</script>
# Renders: &lt;script&gt;alert('XSS')&lt;/script&gt;
# Script displayed as text, not executed
```

### Sobelow Configuration

Configure security scanning in `.sobelow-conf`.

```elixir
# .sobelow-conf in project root
[
  verbose: true,                             # => Show detailed vulnerability info
  private: false,                            # => Skip private function checks
  skip: false,                               # => Don't skip any checks

  # Ignore specific findings (use sparingly!)
  ignore: [
    "Config.HTTPS",                          # => Ignore HTTPS configuration check
                                             # => Only if handled by reverse proxy
  ],

  # Ignore specific files
  ignore_files: [
    "lib/app_web/controllers/health_controller.ex"
                                             # => Skip security checks for health endpoint
  ]
]
```

## Production Integration

### CI/CD Pipeline Integration

Run all quality tools in continuous integration.

```yaml
# .github/workflows/quality.yml
name: Code Quality

on: [push, pull_request]

jobs:
  quality:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v3
      - uses: erlef/setup-beam@v1
        with:
          elixir-version: "1.17.0" # => Match production Elixir version
          otp-version: "27.0" # => Match production OTP version

      - name: Install dependencies
        run: mix deps.get # => Fetch all dependencies

      - name: Compile (warnings as errors)
        run:
          mix compile --warnings-as-errors
          # => Fail on compilation warnings

      - name: Run Credo
        run:
          mix credo --strict # => Strict mode for CI
          # => All suggestions become failures

      - name: Cache PLT
        uses: actions/cache@v3
        with:
          path: priv/plts
          key:
            plt-${{ runner.os }}-${{ hashFiles('mix.lock') }}
            # => Cache Dialyzer PLT
            # => Speeds up CI runs (PLT build is slow)

      - name: Build PLT
        run:
          mix dialyzer --plt # => Build or update PLT
          # => Uses cache when available

      - name: Run Dialyzer
        run:
          mix dialyzer # => Type checking
          # => Fails on type errors

      - name: Run Sobelow
        run:
          mix sobelow --exit # => Security scanning
          # => --exit makes CI fail on findings
```

### Pre-Commit Hook Integration

Run quality checks before every commit.

```bash
# .git/hooks/pre-commit
#!/bin/bash

echo "Running code quality checks..."

# Run Credo (fast)
mix credo --strict || {                      # => Strict analysis
  echo "Credo found issues"                  # => Error message
  exit 1                                     # => Prevent commit
}

# Run Dialyzer (slower, optional for pre-commit)
# mix dialyzer || {
#   echo "Dialyzer found type errors"
#   exit 1
# }

# Run Sobelow for Phoenix apps (fast)
if [ -d "lib/*_web" ]; then                  # => Check if Phoenix app
  mix sobelow --exit || {                    # => Security scan
    echo "Sobelow found security issues"
    exit 1
  }
fi

echo "All quality checks passed!"
exit 0
```

### Pre-Push Optimization

Run expensive checks (Dialyzer) before push instead of commit.

```bash
# .git/hooks/pre-push
#!/bin/bash

echo "Running type checking..."

# Build PLT if missing
if [ ! -f "priv/plts/dialyzer.plt" ]; then   # => Check PLT exists
  echo "Building PLT (first run, may take 5-10 minutes)..."
  mix dialyzer --plt                         # => One-time PLT build
fi

# Run Dialyzer
mix dialyzer || {                            # => Type checking
  echo "Dialyzer found type errors"
  exit 1                                     # => Prevent push
}

echo "Type checking passed!"
exit 0
```

## Tool Comparison

| Tool     | Purpose           | Speed | When to Run       | Blocks Commit |
| -------- | ----------------- | ----- | ----------------- | ------------- |
| Credo    | Style consistency | Fast  | Pre-commit        | Yes           |
| Dialyxir | Type checking     | Slow  | Pre-push, CI/CD   | Optional      |
| Sobelow  | Security scanning | Fast  | Pre-commit, CI/CD | Yes           |

**Recommended Workflow**:

- **Pre-commit**: Credo (strict) + Sobelow
- **Pre-push**: Dialyzer (cached PLT)
- **CI/CD**: All three tools with strict settings

## Common Pitfalls

### Ignoring Quality Tools

**Problem**: Running tools but not fixing issues.

```elixir
# BAD: Disabling all checks
# .credo.exs
checks: [
  {Credo.Check.Readability.ModuleDoc, false},
  {Credo.Check.Design.TagTODO, false},       # => Disabling too many checks
  {Credo.Check.Refactor.FunctionArity, false}
]                                            # => Defeats purpose of quality tools

# GOOD: Fix issues instead of disabling
# Only disable specific checks with clear rationale
checks: [
  {Credo.Check.Design.AliasUsage, false}     # => One specific check
]                                            # => Document why: "Aliases improve readability in our codebase"
```

### Incomplete Type Specs

**Problem**: Missing `@spec` declarations let type errors slip through.

```elixir
# BAD: No @spec (Dialyzer has less context)
def calculate_zakat(amount) do
  amount * 0.025                             # => Dialyzer assumes any type
end                                          # => Won't catch if called with wrong types

# GOOD: Explicit @spec
@spec calculate_zakat(float()) :: float()    # => Clear type contract
def calculate_zakat(amount) do
  amount * 0.025                             # => Dialyzer validates callers pass float
end                                          # => Catches type mismatches at compile time
```

### Ignoring Security Warnings

**Problem**: Marking security issues as false positives without fixing.

```elixir
# BAD: Ignoring SQL injection warning
# .sobelow-conf
ignore: [
  "SQL.Query"                                # => Blanket ignore of SQL issues
]                                            # => Dangerous: Real vulnerabilities ignored

# GOOD: Fix the actual vulnerability
# Controller with safe query
def search(conn, params) do
  query = from d in Donation,
          where: d.amount > ^params["amount"] # => Parameterized query
  Repo.all(query)                            # => No Sobelow warnings
end                                          # => Security issue resolved
```

## Key Takeaways

1. **Elixir has no built-in quality tools** - Requires external dependencies
2. **Credo enforces consistency** - Style, readability, best practices
3. **Dialyxir catches type errors** - Validates `@spec` at compile time
4. **Sobelow prevents vulnerabilities** - Phoenix-specific security scanning
5. **CI/CD integration mandatory** - Automated quality gates in pipeline
6. **Pre-commit hooks prevent issues** - Catch problems before commit
7. **PLT caching critical** - Speeds up Dialyzer in CI/CD
8. **Don't disable checks lightly** - Fix issues instead of ignoring

## Related Content

- [Best Practices](/en/learn/software-engineering/programming-languages/elixir/in-the-field/best-practices) - Production development patterns
- [Type Specifications](/en/learn/software-engineering/programming-languages/elixir/in-the-field/type-specifications) - Writing effective `@spec` declarations
- [Phoenix Framework](/en/learn/software-engineering/programming-languages/elixir/in-the-field/phoenix-framework) - Web application patterns
- [Anti Patterns](/en/learn/software-engineering/programming-languages/elixir/in-the-field/anti-patterns) - Common mistakes to avoid
