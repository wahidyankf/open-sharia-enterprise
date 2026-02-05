---
title: "Hot Code Upgrades"
date: 2026-02-05T00:00:00+07:00
draft: false
weight: 1000031
description: "Understanding hot code upgrades with Relup and Appup files, limitations, and when rolling deployments are preferable"
tags: ["elixir", "hot-code-upgrade", "relup", "appup", "deployment", "production", "erlang"]
prev: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/performance-optimization"
next: "/en/learn/software-engineering/programming-languages/elixir/in-the-field/distributed-systems"
---

**Need zero-downtime deployments for Elixir applications?** This guide teaches hot code upgrade patterns from standard library OTP releases through Relup/Appup files, but emphasizes their complexity and error-prone nature - showing when modern rolling deployment strategies are more appropriate for production systems.

## Why Hot Code Upgrades Matter (Rarely)

Hot code upgrades allow running Erlang/Elixir applications to upgrade without stopping:

- **Legacy telecommunications systems** - 24/7 uptime requirements, cannot restart
- **Embedded systems** - Medical devices, industrial controllers (physical access difficult)
- **Historical context** - Erlang designed for telephone switches (1980s-90s)

Modern deployment reality:

- **Rolling deployments preferred** - Blue-green, canary releases, load balancer rotation
- **Cloud-native patterns** - Kubernetes rolling updates, zero-downtime with multiple instances
- **Complexity vs benefit** - Hot upgrades error-prone, difficult to test, rarely worth maintenance burden
- **State management issues** - Process state transformations complex, version compatibility fragile

**Production question**: Should you invest in hot code upgrade infrastructure? For 95% of applications, the answer is NO - rolling deployments with proper supervision provide better reliability with less complexity.

## When Hot Upgrades Appropriate

Use hot code upgrades ONLY when:

1. **Cannot run multiple instances** - Single-instance constraint (embedded systems)
2. **Cannot afford restart downtime** - Even brief interruption unacceptable
3. **No load balancer rotation possible** - Infrastructure limitation (edge devices)
4. **Legacy system requirement** - Inherited system with hot upgrade dependency

**Financial domain example**: Even donation platforms with financial transactions use rolling deployments - NOT hot upgrades.

## Standard Library Approach

### OTP Release with Appup Files

Hot code upgrades require OTP releases with application upgrade (Appup) and release upgrade (Relup) files.

**Standard Library**: Mix Release + Appup files.

```elixir
# Mix project configuration
defmodule DonationPlatform.MixProject do
  use Mix.Project                                    # => Imports Mix.Project behavior
                                                     # => Provides project/0 callback

  def project do
    [
      app: :donation_platform,                       # => Application name
      version: "1.0.1",                              # => Current version (upgrading to)
                                                     # => Previous version was "1.0.0"
      elixir: "~> 1.17",                             # => Elixir version requirement
      start_permanent: Mix.env() == :prod,           # => Permanent start in production
                                                     # => Application restarts on exit
      deps: deps(),                                  # => Dependencies list
      releases: releases()                           # => Release configuration
    ]
  end

  defp releases do
    [
      donation_platform: [                           # => Release name
        version: "1.0.1",                            # => Must match project version
        applications: [                              # => Applications to include
          donation_platform: :permanent              # => Start mode: permanent
        ],
        steps: [:assemble, :tar]                     # => Build steps
                                                     # => :assemble - creates release
                                                     # => :tar - packages as tarball
      ]
    ]
  end
end
# => Release foundation for hot upgrades
# => Still needs Appup files for version transitions
```

### Application Upgrade (Appup) File Structure

Appup files define version transition instructions.

**Standard Library**: .appup files in src/ directory.

```erlang
# File: apps/donation_platform/src/donation_platform.appup
# Erlang syntax required (Appup uses Erlang format)
{
  "1.0.1",                                           %=> Current version (upgrading to)

  [                                                  %=> Upgrade instructions
    {"1.0.0", [                                      %=> From version 1.0.0
      {load_module, DonationPlatform.Calculator},    %=> Load new Calculator module
                                                     %=> Replaces old code in running VM
      {update, DonationPlatform.Server, {advanced, []}},
                                                     %=> Update GenServer process
                                                     %=> Calls code_change/3 callback
      {add_module, DonationPlatform.NewFeature}      %=> Add entirely new module
                                                     %=> Module didn't exist in 1.0.0
    ]}
  ],

  [                                                  %=> Downgrade instructions
    {"1.0.0", [                                      %=> Back to version 1.0.0
      {load_module, DonationPlatform.Calculator},    %=> Load old Calculator module
      {update, DonationPlatform.Server, {advanced, []}},
                                                     %=> Downgrade GenServer process
      {delete_module, DonationPlatform.NewFeature}   %=> Remove module added in 1.0.1
    ]}
  ]
}.
# => Defines state transformations between versions
# => Both upgrade AND downgrade paths required
```

### GenServer Code Change Callback

Hot upgrades require implementing `code_change/3` callback.

```elixir
defmodule DonationPlatform.Server do
  use GenServer                                      # => GenServer behavior
                                                     # => Provides process callbacks

  # Version 1.0.0 state structure
  defstruct [:donations, :total]                     # => Old state format
                                                     # => No :currency field

  # Version 1.0.1 state structure would be:
  # defstruct [:donations, :total, :currency]        # => New state format
  #                                                  # => Added :currency field

  @impl true
  def code_change("1.0.0", old_state, _extra) do
    # Upgrading FROM 1.0.0 TO 1.0.1
    new_state = %{old_state | currency: "USD"}       # => Add missing :currency field
                                                     # => Default value: "USD"
                                                     # => Transforms state structure
    {:ok, new_state}                                 # => Returns transformed state
                                                     # => Process continues with new code
  end

  def code_change(_old_vsn, state, _extra) do
    # Fallback for other version transitions
    {:ok, state}                                     # => No state transformation
  end
  # => Handles state migration during hot upgrade
  # => Called automatically by OTP upgrade process
  # => MUST handle state structure changes correctly
end
# => Process state survives code upgrade
# => New code operates on transformed state
```

### Release Upgrade (Relup) Generation

Relup files generate from Appup files during release build.

**Standard Library**: Mix Release + Relup generation.

```bash
# Step 1: Build version 1.0.0 release
MIX_ENV=prod mix release                             # => Creates initial release
                                                     # => Output: _build/prod/rel/donation_platform/
                                                     # => Version 1.0.0 release structure

# Step 2: Update version to 1.0.1
# Edit mix.exs: version: "1.0.1"
# Create donation_platform.appup file

# Step 3: Generate relup
cd _build/prod/rel/donation_platform
bin/donation_platform eval ":release_handler.create_RELEASES(~c\".\", ~c\"releases/1.0.1/donation_platform.rel\", [], [{:outdir, ~c\".\"}])"
                                                     # => Generates relup file
                                                     # => Analyzes version differences
                                                     # => Creates upgrade instructions

# => Relup contains low-level VM instructions
# => Derived from Appup specifications
# => Used by release_handler for actual upgrade
```

## Limitations and Complexity

### Error-Prone Nature

Hot code upgrades fail for numerous reasons:

```elixir
# Scenario 1: State structure incompatibility
defmodule DonationPlatform.Server do
  # Version 1.0.0
  defstruct [:total]                                 # => Old state: single :total

  # Version 1.0.1
  defstruct [:totals]                                # => New state: :totals map
                                                     # => Field RENAMED (not added)
                                                     # => code_change/3 cannot handle rename
                                                     # => Upgrade FAILS
end
# => Field renames break hot upgrades
# => Field type changes equally problematic
# => Requires complex migration code

# Scenario 2: Module dependency changes
# If Calculator.ex changes function signatures:
defmodule Calculator do
  # 1.0.0: calculate(amount)
  # 1.0.1: calculate(amount, currency)              # => Signature changed
                                                     # => Calling code MUST also update
                                                     # => Appup must coordinate multiple modules
                                                     # => Easy to miss dependencies
end
# => Dependency upgrades complex
# => All callers need simultaneous update
# => Testing upgrade path difficult

# Scenario 3: Database schema changes
# Hot code upgrade CANNOT handle:
# - Ecto migrations during upgrade
# - Schema changes requiring data transformation
# - Index creation (locks database)
# => Database and code version mismatch
# => Application crashes on startup
# => Requires manual intervention
```

### Testing Challenges

Testing hot upgrades requires production-like setup:

1. **Build both versions** - Complete releases for old AND new versions
2. **Start old version** - Running application with version N
3. **Perform upgrade** - Apply Relup to transition N → N+1
4. **Verify functionality** - All features work after upgrade
5. **Test downgrade** - Apply Relup to transition N+1 → N
6. **Repeat for combinations** - Test N-2 → N, N-3 → N, etc.

**Complexity**: Exponential with version count. Testing all upgrade paths impractical.

## When Rolling Deployments Better

### Blue-Green Deployment Pattern

Modern approach: Run two versions simultaneously, switch traffic.

```elixir
# Configuration for rolling deployment
# File: rel/overlays/vm.args.eex
## Node name with version
-name donation_platform_<%= @release.version %>@127.0.0.1
                                                     # => Unique node name per version
                                                     # => Allows multiple versions running
                                                     # => Load balancer switches traffic

## Cookie for distribution
-setcookie donation_platform_production              # => Shared cookie for clustering
                                                     # => Nodes can communicate
                                                     # => Enables state transfer if needed

# Deploy sequence:
# 1. Start new version (1.0.1) alongside old (1.0.0)
# 2. Health check new version
# 3. Switch load balancer to new version
# 4. Old version drains connections
# 5. Stop old version when no active connections
# => Zero downtime achieved without hot upgrades
# => Simpler to implement and test
# => Easy rollback: switch load balancer back
```

### Advantages Over Hot Upgrades

Rolling deployments provide better production characteristics:

- **Testability** - Standard integration tests verify new version
- **Rollback** - Load balancer switch (seconds), not code downgrade
- **State isolation** - New version starts fresh, no state transformation
- **Database migrations** - Apply before deployment, both versions compatible
- **Monitoring** - Side-by-side comparison of old vs new version metrics
- **Gradual rollout** - Canary deployment: 5% → 50% → 100% traffic

**Financial domain example**: Donation platform deployment.

```elixir
# Kubernetes rolling update (preferred over hot upgrades)
# File: k8s/deployment.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: donation-platform
spec:
  replicas: 3                                        # => Three instances running
  strategy:
    type: RollingUpdate                              # => Rolling update strategy
    rollingUpdate:
      maxSurge: 1                                    # => One extra pod during update
      maxUnavailable: 0                              # => Zero downtime requirement
  template:
    spec:
      containers:
      - name: donation-platform
        image: donation-platform:1.0.1               # => New version image
        readinessProbe:                              # => Health check before traffic
          httpGet:
            path: /health
            port: 4000
# => Kubernetes handles rolling deployment
# => No Appup/Relup files needed
# => Standard deployment practice
```

## Production Recommendations

### Prefer Rolling Deployments

For 95% of applications, use rolling deployments:

1. **Cloud environments** - Kubernetes, ECS, Docker Swarm
2. **Load balanced applications** - Multiple instances behind load balancer
3. **Stateless services** - API servers, web applications
4. **Database-backed systems** - State in database, not process memory

**Only consider hot upgrades when**:

- Running single instance (embedded system constraint)
- Cannot afford ANY downtime (telecommunications legacy systems)
- No load balancer available (edge deployment scenarios)

### If You Must Use Hot Upgrades

Follow strict guidelines:

1. **Keep state transformations simple** - Only add fields, never rename
2. **Test upgrade paths thoroughly** - All version combinations
3. **Maintain upgrade documentation** - Every version transition documented
4. **Plan downgrade procedures** - Rollback strategy for failures
5. **Monitor upgrade process** - Detailed logging and metrics
6. **Limit upgrade complexity** - Consider forced restart for major versions

## Key Takeaways

Hot code upgrades in Elixir:

- **Possible via Relup/Appup** - Standard library support exists
- **Complex and error-prone** - State transformations, dependency coordination
- **Difficult to test** - Requires production-like infrastructure
- **Rarely worth complexity** - Rolling deployments simpler and more reliable
- **Use only when necessary** - Embedded systems, legacy requirements
- **Modern alternative preferred** - Blue-green, canary, rolling deployments

**Financial systems reality**: Even donation platforms with critical financial operations use rolling deployments, NOT hot code upgrades. Zero-downtime achieved through proper architecture (load balancing, multiple instances) rather than runtime code swapping.

**Default recommendation**: Design applications for rolling deployments from the start. Reserve hot upgrades for the rare scenarios where infrastructure truly cannot support multiple instances.
