---
title: "DDD Entity Standards"
description: OSE Platform standards for entity identity management and lifecycle tracking
category: explanation
subcategory: architecture
tags:
  - ddd
  - entities
  - identity
principles:
  - explicit-over-implicit
created: 2026-02-09
---

# DDD Entity Standards

## Prerequisite Knowledge

**REQUIRED**: Complete [AyoKoding DDD Entities](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/patterns-and-principles/) before using these standards.

## Purpose

OSE Platform entity standards for identity and lifecycle management.

## REQUIRED: Identity-Based Equality

**REQUIRED**: Entities MUST be compared by identity, not by attributes.

#### `Java`

```java
public class ZakatAssessment {
    private final AssessmentId id;  // Identity
    private Money totalWealth;      // Mutable attributes

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof ZakatAssessment other)) return false;
        return this.id.equals(other.id);  // Identity comparison
    }

    @Override
    public int hashCode() {
        return id.hashCode();
    }
}
```

#### `Kotlin`

```kotlin
class ZakatAssessment(
    val id: AssessmentId,           // Identity
    var totalWealth: Money,         // Mutable attributes
) {
    override fun equals(other: Any?): Boolean =
        other is ZakatAssessment && id == other.id  // Identity comparison

    override fun hashCode(): Int = id.hashCode()
}
```

#### `C#`

```csharp
namespace Ose.Zakat.Domain;

public class ZakatAssessment : IEquatable<ZakatAssessment>
{
    public AssessmentId Id { get; }          // Identity
    public Money TotalWealth { get; set; }   // Mutable attribute

    public ZakatAssessment(AssessmentId id, Money totalWealth)
    {
        Id = id;
        TotalWealth = totalWealth;
    }

    public bool Equals(ZakatAssessment? other) =>
        other is not null && Id == other.Id;  // Identity comparison

    public override bool Equals(object? obj) => Equals(obj as ZakatAssessment);
    public override int GetHashCode() => Id.GetHashCode();
}
```

## Identity Types

**REQUIRED**: Use strongly-typed IDs.

**Good**:

#### `Java`

```java
public record AssessmentId(UUID value) {}
public record DonationId(UUID value) {}
```

#### `Kotlin`

```kotlin
@JvmInline value class AssessmentId(val value: UUID)
@JvmInline value class DonationId(val value: UUID)
```

#### `C#`

```csharp
namespace Ose.Zakat.Domain;

public readonly record struct AssessmentId(Guid Value);
public readonly record struct DonationId(Guid Value);
```

**Bad** (primitive obsession):

#### `Java`

```java
UUID assessmentId;  // Not type-safe
String donationId;  // Can be confused with other strings
```

#### `Kotlin`

```kotlin
val assessmentId: UUID  // Not type-safe
val donationId: String  // Can be confused with other strings
```

#### `C#`

```csharp
Guid assessmentId;    // Not type-safe
string donationId;    // Can be confused with other strings
```

## Lifecycle Tracking

**OPTIONAL**: Entities MAY track lifecycle with FSM when state transitions have business meaning.

**See**: [FSM Standards](../finite-state-machine-fsm/README.md)
