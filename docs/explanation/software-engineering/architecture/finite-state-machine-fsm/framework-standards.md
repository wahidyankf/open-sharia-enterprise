---
title: "FSM Framework Standards"
description: Spring State Machine, XState configuration standards for OSE Platform
category: explanation
subcategory: architecture
tags:
  - fsm
  - spring-state-machine
  - xstate
principles:
  - automation-over-manual
created: 2026-02-09
---

# FSM Framework Standards

## Prerequisite Knowledge

**REQUIRED**: Complete [AyoKoding FSM Frameworks](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/) before using these standards.

## Spring State Machine (Java)

**REQUIRED for Java applications**.

**Configuration**:

- MUST use `@Configuration` with `@EnableStateMachine`
- MUST use configuration DSL, NOT XML
- MUST persist state for long-running workflows

**Example**:

#### `Java`

```java
@Configuration
@EnableStateMachine
public class ZakatStateMachineConfig extends StateMachineConfigurerAdapter<
    ZakatState, ZakatEvent> {
    // Configuration here
}
```

#### `Kotlin`

```kotlin
@Configuration
@EnableStateMachine
class ZakatStateMachineConfig : StateMachineConfigurerAdapter<ZakatState, ZakatEvent>() {
    // Configuration here
}
```

#### `C#`

```csharp
// C# uses Stateless or custom FSM — no direct Spring State Machine equivalent.
// Shown here with Stateless library (idiomatic C# FSM approach).
namespace Zakat.Infrastructure.StateMachines;

public sealed class ZakatStateMachineConfig
{
    public StateMachine<ZakatState, ZakatEvent> Build() =>
        new StateMachine<ZakatState, ZakatEvent>(ZakatState.Draft);
}
```

## XState (TypeScript)

**REQUIRED for TypeScript applications**.

**Configuration**:

- MUST use `createMachine` with TypeScript types
- MUST visualize with XState Viz
- MUST define guards as pure functions

**Example**:

```typescript
const machine = createMachine({
  id: "campaign",
  initial: "planning",
  states: {
    /* ... */
  },
});
```

## Go

**OPTIONAL**: Use `looplab/fsm` OR hand-rolled.

**MUST**: Use explicit state type (not strings).
