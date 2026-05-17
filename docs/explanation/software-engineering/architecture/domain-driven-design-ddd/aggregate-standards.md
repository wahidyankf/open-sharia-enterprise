---
title: "DDD Aggregate Standards"
description: OSE Platform standards for aggregate design, consistency boundaries, and Islamic finance invariants
category: explanation
subcategory: architecture
tags:
  - ddd
  - aggregates
  - islamic-finance
principles:
  - explicit-over-implicit
  - immutability
created: 2026-02-09
---

# DDD Aggregate Standards

## Prerequisite Knowledge

**REQUIRED**: Complete [AyoKoding DDD Aggregates](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/patterns-and-principles/) before using these standards.

## Purpose

OSE Platform aggregate design standards for Islamic finance systems.

## REQUIRED: Aggregates Enforce Business Invariants

**REQUIRED**: All business rules MUST be enforced by aggregate roots.

### Zakat Assessment Aggregate

#### `Java`

```java
public record ZakatAssessment(
    AssessmentId id,
    UserId userId,
    Money totalWealth,
    Money nisabThreshold,  // 87.48g gold equivalent
    ZakatAmount zakatDue,
    AssessmentStatus status
) {
    // MUST enforce: Zakat is 2.5% of wealth exceeding Nisab
    public ZakatAssessment calculate() {
        if (totalWealth.isLessThan(nisabThreshold)) {
            return withStatus(AssessmentStatus.BELOW_NISAB);
        }
        Money zakatAmount = totalWealth.multiply(0.025);
        return new ZakatAssessment(id, userId, totalWealth,
            nisabThreshold, zakatAmount, AssessmentStatus.CALCULATED);
    }

    // MUST enforce: Cannot pay more than owed
    public ZakatAssessment markAsPaid(Money paidAmount) {
        if (paidAmount.isGreaterThan(zakatDue)) {
            throw new InvalidPaymentException("Cannot overpay Zakat");
        }
        return withStatus(AssessmentStatus.PAID);
    }
}
```

#### `Kotlin`

```kotlin
data class ZakatAssessment(
    val id: AssessmentId,
    val userId: UserId,
    val totalWealth: Money,
    val nisabThreshold: Money,  // 87.48g gold equivalent
    val zakatDue: ZakatAmount,
    val status: AssessmentStatus,
) {
    // MUST enforce: Zakat is 2.5% of wealth exceeding Nisab
    fun calculate(): ZakatAssessment =
        if (totalWealth < nisabThreshold) {
            copy(status = AssessmentStatus.BELOW_NISAB)
        } else {
            val zakatAmount = totalWealth * 0.025
            copy(zakatDue = zakatAmount, status = AssessmentStatus.CALCULATED)
        }

    // MUST enforce: Cannot pay more than owed
    fun markAsPaid(paidAmount: Money): ZakatAssessment {
        require(!paidAmount.isGreaterThan(zakatDue)) {
            "Cannot overpay Zakat"
        }
        return copy(status = AssessmentStatus.PAID)
    }
}
```

#### `C#`

```csharp
namespace Ose.Zakat.Domain;

public sealed record ZakatAssessment(
    AssessmentId Id,
    UserId UserId,
    Money TotalWealth,
    Money NisabThreshold,  // 87.48g gold equivalent
    ZakatAmount ZakatDue,
    AssessmentStatus Status)
{
    // MUST enforce: Zakat is 2.5% of wealth exceeding Nisab
    public ZakatAssessment Calculate() =>
        TotalWealth < NisabThreshold
            ? this with { Status = AssessmentStatus.BelowNisab }
            : this with
            {
                ZakatDue = TotalWealth.Multiply(0.025m),
                Status = AssessmentStatus.Calculated,
            };

    // MUST enforce: Cannot pay more than owed
    public ZakatAssessment MarkAsPaid(Money paidAmount)
    {
        if (paidAmount > ZakatDue)
            throw new InvalidPaymentException("Cannot overpay Zakat");
        return this with { Status = AssessmentStatus.Paid };
    }
}
```

## Transaction Boundaries

**REQUIRED**: One aggregate = One transaction.

**PROHIBITED**: Modifying multiple aggregates in single transaction (use eventual consistency via domain events instead).

## Aggregate Size

**SHOULD**: Keep aggregates small (1-5 entities maximum).

**WHY**: Large aggregates cause performance issues and concurrency conflicts.
