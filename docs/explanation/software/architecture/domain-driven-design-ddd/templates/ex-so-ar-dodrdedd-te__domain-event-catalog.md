---
title: "Domain Event Catalog Template"
description: "Template for documenting domain events including naming conventions, structure, metadata, triggers, consumers, and examples from Tax and Permitted domains"
tags: ["ddd", "template", "domain-events", "event-driven", "event-catalog"]
---

# Domain Event Catalog Template

This template provides a structured approach to documenting domain events, the facts about what happened in your domain model.

## Purpose

Domain Event Catalog documentation:

- **Centralizes event knowledge**: All events in one place
- **Ensures consistency**: Standard structure and naming
- **Guides integration**: Shows which events to subscribe to
- **Supports evolution**: Track event schema versions
- **Enables discovery**: Find events by domain concept or consumer
- **Documents contracts**: Event payloads are integration contracts

## Event Naming Conventions

### Standard Format

```
[BoundedContext].[Aggregate].[PastTenseAction]
```

**Examples**:

- `Tax.Assessment.Finalized`
- `Permitted.Certification.Suspended`
- `Finance.LoanContract.Executed`
- `Payment.Invoice.Paid`

### Naming Rules

1. **Past Tense**: Events are facts that already happened
   - Good: `AssessmentFinalized`, `CertificationIssued`, `ContractExecuted`
   - Bad: `FinalizeAssessment`, `IssueCertification`, `ExecuteContract`
2. **Specific**: Describe what happened, not generic state changes
   - Good: `TaxAssessmentFinalized`, `PermittedCertificationSuspended`
   - Bad: `AssessmentUpdated`, `CertificationChanged`
3. **Domain Language**: Use Ubiquitous Language terms
   - Good: `ThresholdThresholdExceeded`, `HawlPeriodCompleted`
   - Bad: `ThresholdExceeded`, `YearCompleted`
4. **Context Prefix**: Include bounded context for clarity (optional in small systems)
   - Good: `Tax.AssessmentFinalized` or `TaxAssessmentFinalized`
   - Bad: `Finalized`, `Event1`

## Event Structure Template

Every domain event should follow a consistent structure:

```typescript
interface DomainEvent {
  // Metadata (standard across all events)
  eventId: string; // Unique event identifier (UUID)
  eventType: string; // Event type name (e.g., "Tax.Assessment.Finalized")
  aggregateId: string; // ID of aggregate that raised the event
  aggregateType: string; // Type of aggregate (e.g., "TaxAssessment")
  occurredAt: string; // ISO 8601 timestamp when event occurred
  version: number; // Event schema version (for evolution)

  // Correlation & Causation (for distributed tracing)
  metadata: {
    correlationId?: string; // Groups related events across aggregates
    causationId?: string; // Event that caused this event
    userId?: string; // User who triggered the action
    clientId?: string; // Application/service that triggered the action
  };

  // Payload (event-specific data)
  payload: {
    // Event-specific fields here
  };
}
```

## Event Catalog Format

For each event, document:

1. **Event Name**: Following naming conventions
2. **Description**: What happened (business perspective)
3. **Bounded Context**: Which context publishes this event
4. **Aggregate**: Which aggregate raises this event
5. **Triggered By**: Which command(s) cause this event
6. **Payload**: Complete data structure
7. **Payload Fields**: Description of each field
8. **Consumers**: Who subscribes to this event
9. **Schema Version**: Current version number
10. **Version History**: Changes across versions

## Example Events: Tax Management Context

### Event: TaxAssessmentCreated

**Description**: A new Tax assessment has been created for a taxpayer.

**Bounded Context**: Tax Management

**Aggregate**: TaxAssessment

**Triggered By**:

- `CreateTaxAssessment` command
- User initiates annual Tax assessment

**Payload**:

```typescript
{
  eventId: "550e8400-e29b-41d4-a716-446655440000",
  eventType: "Tax.Assessment.Created",
  aggregateId: "assessment-123",
  aggregateType: "TaxAssessment",
  occurredAt: "2024-03-15T10:30:00Z",
  version: 1,
  metadata: {
    correlationId: "corr-456",
    userId: "user-789"
  },
  payload: {
    assessmentId: "assessment-123",
    taxpayerId: "taxpayer-456",
    taxpayerName: "Ahmad Abdullah",
    hawlStart: "2024-01-01",
    hawlEnd: "2024-12-29", // Lunar year
    status: "Draft",
    createdAt: "2024-03-15T10:30:00Z"
  }
}
```

**Payload Fields**:

- `assessmentId` (string, required): Unique identifier for the assessment
- `taxpayerId` (string, required): Reference to taxpayer aggregate
- `taxpayerName` (string, required): Taxpayer's full name (denormalized for convenience)
- `hawlStart` (string, required): Start of lunar year (ISO 8601 date)
- `hawlEnd` (string, required): End of lunar year (ISO 8601 date)
- `status` (string, required): Initial status (always "Draft")
- `createdAt` (string, required): Creation timestamp (ISO 8601)

**Consumers**:

- **Notification Service**: Send confirmation email to taxpayer
- **Analytics Service**: Track assessment creation metrics
- **Audit Log Service**: Record assessment creation for compliance

**Schema Version**: 1

**Version History**:

- **v1 (2024-03-15)**: Initial version

---

### Event: AssetAddedToAssessment

**Description**: A taxable asset has been added to an assessment.

**Bounded Context**: Tax Management

**Aggregate**: TaxAssessment

**Triggered By**:

- `AddAssetToAssessment` command
- User adds asset to their Tax calculation

**Payload**:

```typescript
{
  eventId: "660e8400-e29b-41d4-a716-446655440001",
  eventType: "Tax.Asset.Added",
  aggregateId: "assessment-123",
  aggregateType: "TaxAssessment",
  occurredAt: "2024-03-15T10:35:00Z",
  version: 1,
  metadata: {
    correlationId: "corr-456",
    userId: "user-789"
  },
  payload: {
    assessmentId: "assessment-123",
    assetId: "asset-001",
    category: "Cash",
    description: "Savings account - Bank ABC",
    value: {
      amount: 25000.00,
      currency: "USD"
    },
    hawlStartDate: "2024-01-01",
    isHawlComplete: false,
    addedAt: "2024-03-15T10:35:00Z"
  }
}
```

**Payload Fields**:

- `assessmentId` (string, required): Parent assessment ID
- `assetId` (string, required): Unique asset identifier
- `category` (string, required): Asset type (Cash | Gold | Silver | BusinessInventory | Investment | Receivables | Livestock | Agriculture)
- `description` (string, required): Human-readable asset description
- `value` (object, required): Current market value
  - `amount` (number, required): Numeric value
  - `currency` (string, required): ISO 4217 currency code
- `hawlStartDate` (string, required): When asset acquired or hawl started (ISO 8601 date)
- `isHawlComplete` (boolean, required): Whether full lunar year has passed
- `addedAt` (string, required): When asset was added to assessment (ISO 8601)

**Consumers**:

- **Audit Log Service**: Track asset additions
- **Analytics Service**: Asset composition analysis

**Schema Version**: 1

**Version History**:

- **v1 (2024-03-15)**: Initial version

---

### Event: TaxAssessmentFinalized

**Description**: A Tax assessment has been completed and the Tax liability has been determined.

**Bounded Context**: Tax Management

**Aggregate**: TaxAssessment

**Triggered By**:

- `FinalizeAssessment` command
- Review process completed, assessment approved

**Payload**:

```typescript
{
  eventId: "770e8400-e29b-41d4-a716-446655440002",
  eventType: "Tax.Assessment.Finalized",
  aggregateId: "assessment-123",
  aggregateType: "TaxAssessment",
  occurredAt: "2024-12-30T14:00:00Z",
  version: 2,
  metadata: {
    correlationId: "corr-456",
    userId: "reviewer-555"
  },
  payload: {
    assessmentId: "assessment-123",
    taxpayerId: "taxpayer-456",
    taxpayerName: "Ahmad Abdullah",
    hawlPeriod: {
      start: "2024-01-01",
      end: "2024-12-29"
    },
    totalTaxableWealth: {
      amount: 85000.00,
      currency: "USD"
    },
    thresholdThreshold: {
      amount: 5100.00,
      currency: "USD",
      standard: "gold",
      goldPricePerGram: 60.00
    },
    liability: {
      isObligated: true,
      amount: {
        amount: 2125.00,
        currency: "USD"
      },
      rate: 0.025,
      exemptionReason: null
    },
    assetBreakdown: {
      cash: 25000.00,
      gold: 15000.00,
      silver: 0.00,
      businessInventory: 30000.00,
      investments: 15000.00,
      receivables: 0.00
    },
    finalizedAt: "2024-12-30T14:00:00Z",
    finalizedBy: "reviewer-555"
  }
}
```

**Payload Fields**:

- `assessmentId` (string, required): Assessment identifier
- `taxpayerId` (string, required): Taxpayer reference
- `taxpayerName` (string, required): Taxpayer's full name
- `hawlPeriod` (object, required): Lunar year period
  - `start` (string, required): Start date
  - `end` (string, required): End date
- `totalTaxableWealth` (object, required): Total wealth subject to Tax
  - `amount` (number, required): Total value
  - `currency` (string, required): Currency code
- `thresholdThreshold` (object, required): Threshold used for calculation
  - `amount` (number, required): Threshold value
  - `currency` (string, required): Currency code
  - `standard` (string, required): "gold" or "silver"
  - `goldPricePerGram` (number, optional): Gold price if gold standard used
  - `silverPricePerGram` (number, optional): Silver price if silver standard used
- `liability` (object, required): Calculated Tax obligation
  - `isObligated` (boolean, required): Whether Tax is due
  - `amount` (object, required if obligated): Tax amount due
    - `amount` (number, required): Numeric value
    - `currency` (string, required): Currency code
  - `rate` (number, required): Tax rate applied (typically 0.025 = 2.5%)
  - `exemptionReason` (string, nullable): Reason if exempt (e.g., "Below threshold")
- `assetBreakdown` (object, required): Wealth by category (amounts in assessment currency)
  - `cash` (number, required)
  - `gold` (number, required)
  - `silver` (number, required)
  - `businessInventory` (number, required)
  - `investments` (number, required)
  - `receivables` (number, required)
  - `livestock` (number, optional)
  - `agriculture` (number, optional)
- `finalizedAt` (string, required): Finalization timestamp
- `finalizedBy` (string, required): User who finalized

**Consumers**:

- **Payment Service**: Create payment obligation if Tax due
- **Accounting Service**: Record Tax liability in general ledger
- **Notification Service**: Notify taxpayer of obligation
- **Analytics Service**: Compliance reporting and statistics
- **Tax Receipt Service**: Generate official Tax assessment document

**Schema Version**: 2

**Version History**:

- **v1 (2024-03-15)**: Initial version with basic liability information
- **v2 (2024-06-01)**: Added `assetBreakdown` for detailed reporting, added `finalizedBy` field

---

### Event: ThresholdThresholdExceeded

**Description**: Taxpayer's taxable wealth has exceeded the threshold threshold, triggering Tax obligation.

**Bounded Context**: Tax Management

**Aggregate**: TaxAssessment

**Triggered By**:

- `FinalizeAssessment` command (when calculation determines wealth exceeds threshold)
- Automatic threshold monitoring service

**Payload**:

```typescript
{
  eventId: "880e8400-e29b-41d4-a716-446655440003",
  eventType: "Tax.Threshold.Exceeded",
  aggregateId: "assessment-123",
  aggregateType: "TaxAssessment",
  occurredAt: "2024-12-30T14:00:00Z",
  version: 1,
  metadata: {
    correlationId: "corr-456",
    causationId: "770e8400-e29b-41d4-a716-446655440002" // Caused by TaxAssessmentFinalized
  },
  payload: {
    assessmentId: "assessment-123",
    taxpayerId: "taxpayer-456",
    totalWealth: {
      amount: 85000.00,
      currency: "USD"
    },
    thresholdThreshold: {
      amount: 5100.00,
      currency: "USD"
    },
    excessAmount: {
      amount: 79900.00,
      currency: "USD"
    },
    excessPercentage: 1566.67, // Wealth is 1566.67% of threshold
    occurredAt: "2024-12-30T14:00:00Z"
  }
}
```

**Payload Fields**:

- `assessmentId` (string, required): Assessment identifier
- `taxpayerId` (string, required): Taxpayer reference
- `totalWealth` (object, required): Total taxable wealth
- `thresholdThreshold` (object, required): Threshold threshold value
- `excessAmount` (object, required): Amount over threshold
- `excessPercentage` (number, required): Wealth as percentage of threshold
- `occurredAt` (string, required): When threshold was exceeded

**Consumers**:

- **Analytics Service**: Track threshold exceedance rates
- **Notification Service**: Educational content about Tax obligation

**Schema Version**: 1

**Version History**:

- **v1 (2024-03-15)**: Initial version

---

## Example Events: Permitted Certification Context

### Event: PermittedCertificationIssued

**Description**: A Permitted certification has been issued to a facility after successful audit.

**Bounded Context**: Permitted Certification

**Aggregate**: PermittedCertification

**Triggered By**:

- `IssueCertification` command
- Audit approved, compliance verified

**Payload**:

```typescript
{
  eventId: "990e8400-e29b-41d4-a716-446655440004",
  eventType: "Permitted.Certification.Issued",
  aggregateId: "cert-789",
  aggregateType: "PermittedCertification",
  occurredAt: "2024-04-01T09:00:00Z",
  version: 1,
  metadata: {
    correlationId: "corr-888",
    userId: "auditor-111"
  },
  payload: {
    certificationId: "cert-789",
    certificateNumber: "PERMITTED-2024-001234",
    facilityId: "facility-555",
    facilityName: "Barakah Food Processing Plant",
    facilityAddress: {
      street: "123 Industrial Rd",
      city: "Jakarta",
      country: "Indonesia",
      postalCode: "12345"
    },
    productsCertified: [
      {
        productId: "prod-001",
        productName: "Chicken Nuggets",
        category: "Processed Meat"
      },
      {
        productId: "prod-002",
        productName: "Beef Meatballs",
        category: "Processed Meat"
      }
    ],
    certificationStandard: "JAKIM MS 1500:2019",
    scope: "Food processing - Meat products",
    issuedDate: "2024-04-01",
    expiryDate: "2026-03-31",
    auditId: "audit-333",
    auditScore: 95.5,
    certifyingBody: "Islamic Permitted Certification Agency",
    issuedBy: "auditor-111",
    issuedAt: "2024-04-01T09:00:00Z"
  }
}
```

**Payload Fields**:

- `certificationId` (string, required): Certification aggregate ID
- `certificateNumber` (string, required): Unique certificate number for official documents
- `facilityId` (string, required): Facility reference
- `facilityName` (string, required): Facility name (denormalized)
- `facilityAddress` (object, required): Facility location
- `productsCertified` (array, required): List of products covered
  - `productId` (string, required): Product identifier
  - `productName` (string, required): Product name
  - `category` (string, required): Product category
- `certificationStandard` (string, required): Standard used (e.g., "JAKIM MS 1500:2019")
- `scope` (string, required): Scope of certification
- `issuedDate` (string, required): Certificate issue date
- `expiryDate` (string, required): Certificate expiry date
- `auditId` (string, required): Reference to audit that led to certification
- `auditScore` (number, required): Audit score (0-100)
- `certifyingBody` (string, required): Organization issuing certification
- `issuedBy` (string, required): User who issued certification
- `issuedAt` (string, required): Issuance timestamp

**Consumers**:

- **Facility Management Service**: Update facility certification status
- **Public Registry Service**: Publish to public Permitted certification database
- **Notification Service**: Notify facility of certification
- **Certificate Generation Service**: Generate official certificate document
- **Accounting Service**: Record certification fee

**Schema Version**: 1

**Version History**:

- **v1 (2024-04-01)**: Initial version

---

### Event: PermittedCertificationSuspended

**Description**: A Permitted certification has been suspended due to compliance violations.

**Bounded Context**: Permitted Certification

**Aggregate**: PermittedCertification

**Triggered By**:

- `SuspendCertification` command
- Compliance violation detected during surveillance audit or complaint investigation

**Payload**:

```typescript
{
  eventId: "aa0e8400-e29b-41d4-a716-446655440005",
  eventType: "Permitted.Certification.Suspended",
  aggregateId: "cert-789",
  aggregateType: "PermittedCertification",
  occurredAt: "2024-08-15T11:30:00Z",
  version: 1,
  metadata: {
    correlationId: "corr-999",
    userId: "compliance-officer-222"
  },
  payload: {
    certificationId: "cert-789",
    certificateNumber: "PERMITTED-2024-001234",
    facilityId: "facility-555",
    facilityName: "Barakah Food Processing Plant",
    suspensionReason: "Non-compliant ingredients found during surveillance audit",
    violationType: "Ingredient Violation",
    severity: "High",
    suspendedDate: "2024-08-15",
    suspendedBy: "compliance-officer-222",
    suspendedAt: "2024-08-15T11:30:00Z",
    correctiveActionRequired: true,
    correctiveActionDeadline: "2024-09-15",
    relatedAuditId: "audit-777",
    previousStatus: "Active"
  }
}
```

**Payload Fields**:

- `certificationId` (string, required): Certification identifier
- `certificateNumber` (string, required): Certificate number
- `facilityId` (string, required): Facility reference
- `facilityName` (string, required): Facility name
- `suspensionReason` (string, required): Detailed reason for suspension
- `violationType` (string, required): Category of violation (Ingredient | Process | Documentation | Hygiene | Other)
- `severity` (string, required): Violation severity (Low | Medium | High | Critical)
- `suspendedDate` (string, required): Effective suspension date
- `suspendedBy` (string, required): User who suspended certification
- `suspendedAt` (string, required): Suspension timestamp
- `correctiveActionRequired` (boolean, required): Whether facility must take corrective action
- `correctiveActionDeadline` (string, optional): Deadline for corrective action
- `relatedAuditId` (string, optional): Audit that discovered violation
- `previousStatus` (string, required): Status before suspension

**Consumers**:

- **Public Registry Service**: Update public database, mark certification as suspended
- **Notification Service**: Immediate notification to facility, regulatory authorities
- **Facility Management Service**: Update facility status
- **Compliance Tracking Service**: Track corrective action progress
- **Legal Service**: Document for potential legal proceedings

**Schema Version**: 1

**Version History**:

- **v1 (2024-04-01)**: Initial version

---

## Event Catalog Template

Use this structure for your events:

---

### Event: [EventName]

**Description**: [What happened in business terms]

**Bounded Context**: [Context name]

**Aggregate**: [Aggregate name]

**Triggered By**:

- `[CommandName]` command
- [Business scenario description]

**Payload**:

```typescript
{
  eventId: "[uuid]",
  eventType: "[Context.Aggregate.Action]",
  aggregateId: "[aggregate-id]",
  aggregateType: "[AggregateType]",
  occurredAt: "[ISO 8601 timestamp]",
  version: [number],
  metadata: {
    correlationId: "[optional correlation id]",
    causationId: "[optional causation id]",
    userId: "[optional user id]"
  },
  payload: {
    // Event-specific fields
  }
}
```

**Payload Fields**:

- `[fieldName]` ([type], [required/optional]): [Description]

**Consumers**:

- **[Service Name]**: [What it does with this event]

**Schema Version**: [number]

**Version History**:

- **v[number] ([date])**: [Changes made]

---

## Event Evolution Patterns

### Versioning Strategy

1. **Additive Changes** (compatible): Add new optional fields

```typescript
// v1
{
  assessmentId: "123",
  taxpayerId: "456"
}

// v2 - Added optional field
{
  assessmentId: "123",
  taxpayerId: "456",
  taxpayerName: "Ahmad Abdullah" // New optional field
}
```

1. **Breaking Changes** (incompatible): Create new event version, support both

```typescript
// v1 - Old event
Tax.Assessment.Completed;

// v2 - New event with different structure
Tax.Assessment.Finalized; // Different name, different payload
```

1. **Deprecation**: Support old version for migration period

```typescript
// Consumers receive both old and new events during transition
// Old: Tax.Assessment.Completed (deprecated, will be removed 2025-01-01)
// New: Tax.Assessment.Finalized (use this for new integrations)
```

### Upcasting Pattern

Convert old event versions to current version when reading:

```typescript
class EventUpcaster {
  upcast(event: DomainEvent): DomainEvent {
    if (event.eventType === "Tax.Assessment.Finalized" && event.version === 1) {
      // Upcast v1 to v2
      return {
        ...event,
        version: 2,
        payload: {
          ...event.payload,
          assetBreakdown: this.deriveAssetBreakdown(event.payload), // Compute missing field
          finalizedBy: "unknown", // Default value
        },
      };
    }
    return event;
  }
}
```

## See Also

- [Domain Events](../ex-so-ar-dodrdedd__12-domain-events.md) - Domain event concepts and patterns
- [Aggregates](../ex-so-ar-dodrdedd__09-aggregates.md) - How aggregates raise events
- [Event Sourcing](../ex-so-ar-dodrdedd__12-domain-events.md) - Persisting aggregates as event streams
- [Aggregate Design Template](./ex-so-ar-dodrdedd-te__aggregate-design-template.md) - Document aggregates that raise events
