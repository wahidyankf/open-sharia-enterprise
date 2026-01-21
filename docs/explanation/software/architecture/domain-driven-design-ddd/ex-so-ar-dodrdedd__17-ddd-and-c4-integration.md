# DDD and C4 Integration

## Overview

Domain-Driven Design (DDD) and the C4 Model are complementary approaches: DDD provides patterns for modeling complex domains, while C4 provides a visual language for documenting software architecture. When combined, they create a powerful framework for both designing and communicating system architecture.

This document shows how to map DDD concepts to C4 levels, visualize bounded contexts, aggregates, and domain events using C4 diagrams, and integrate DDD documentation with C4 architecture models.

**Key Mappings:**

- **System Context** ↔ DDD Context Maps (external systems)
- **Container** ↔ DDD Bounded Contexts (deployable units)
- **Component** ↔ DDD Aggregates (internal structure)
- **Dynamic** ↔ DDD Domain Events (behavior over time)

## C4 Model Quick Recap

The C4 Model provides four levels of architectural diagrams:

1. **System Context**: System and external actors
2. **Container**: Applications, databases, message queues
3. **Component**: Internal structure of containers
4. **Code**: Classes, interfaces (rarely used, code itself is better documentation)

**Benefits**:

- Progressive disclosure (zoom in from context to code)
- Standardized notation
- Tool-agnostic (Mermaid, PlantUML, Structurizr)
- Easy to understand for non-technical stakeholders

See [C4 Architecture Model Documentation](../c4-architecture-model/README.md) for comprehensive C4 guidance.

## Mapping DDD to C4 Levels

### Level 1: System Context ↔ Context Maps

**C4 System Context** shows the system and its external dependencies.

**DDD Context Maps** show bounded contexts and their integration patterns.

**Integration**: Use System Context to visualize external systems that integrate with your bounded contexts.

**Example: Tax Calculation System Context**

```mermaid
C4Context
    title System Context: Tax Calculation Platform

    Person(wealthHolder, "Wealth Holder", "Muslim individual calculating tax obligation")
    Person(scholar, "Islamic Scholar", "Verifies tax calculation rules")

    System(taxPlatform, "Tax Calculation Platform", "Calculates tax obligations based on Islamic jurisprudence")

    System_Ext(paymentGateway, "Payment Gateway", "Processes tax payments")
    System_Ext(goldPriceAPI, "Gold Price API", "Provides threshold threshold based on gold price")
    System_Ext(hijriCalendar, "Hijri Calendar Service", "Provides Islamic calendar conversions")

    Rel(wealthHolder, taxPlatform, "Declares wealth, views assessments")
    Rel(scholar, taxPlatform, "Configures tax rates, reviews calculations")
    Rel(taxPlatform, paymentGateway, "Processes payments", "HTTPS/JSON")
    Rel(taxPlatform, goldPriceAPI, "Fetches threshold threshold", "HTTPS/JSON")
    Rel(taxPlatform, hijriCalendar, "Converts dates", "HTTPS/JSON")
```

**DDD Context Map Equivalent**:

- **Tax Calculation** (Core Domain) integrates with:
  - **Payment Processing** (Generic, Conformist relationship)
  - **Commodity Pricing** (Generic, Customer/Supplier)
  - **Calendar Services** (Generic, Published Language)

### Level 2: Container ↔ Bounded Contexts

**C4 Container** shows applications, databases, and services that make up the system.

**DDD Bounded Contexts** define boundaries around cohesive domain models.

**Integration**: Each bounded context typically maps to one or more containers (app + database).

**Example: Tax Calculation Containers (Bounded Contexts)**

```mermaid
C4Container
    title Container: Tax Calculation Platform

    Person(wealthHolder, "Wealth Holder")

    Container_Boundary(taxContext, "Tax Calculation Context (Bounded Context)") {
        Container(taxAPI, "Tax Calculation API", "Node.js/Express", "Provides tax calculation REST API")
        ContainerDb(taxDB, "Tax Database", "PostgreSQL", "Stores tax assessments, declarations")
    }

    Container_Boundary(permittedContext, "Permitted Certification Context (Bounded Context)") {
        Container(permittedAPI, "Permitted Certification API", "Node.js/Express", "Manages permitted product certifications")
        ContainerDb(permittedDB, "Permitted Database", "PostgreSQL", "Stores product certifications")
    }

    Container(eventBus, "Event Bus", "Kafka", "Publishes domain events between contexts")

    System_Ext(paymentGateway, "Payment Gateway")

    Rel(wealthHolder, taxAPI, "Declares wealth, calculates tax", "HTTPS/JSON")
    Rel(taxAPI, taxDB, "Reads/writes assessments", "SQL")
    Rel(taxAPI, eventBus, "Publishes TaxCalculated event", "Kafka")
    Rel(permittedAPI, eventBus, "Subscribes to ProductCertified event", "Kafka")
    Rel(taxAPI, paymentGateway, "Processes tax payments", "HTTPS/JSON")
```

**DDD Bounded Context Mapping**:

- **Tax Calculation Context**: `taxAPI` + `taxDB`
- **Permitted Certification Context**: `permittedAPI` + `permittedDB`
- **Shared Kernel**: `eventBus` (domain events)

**Key Insight**: Bounded contexts are deployment boundaries. Microservices architecture = one bounded context per service.

### Level 3: Component ↔ Aggregates

**C4 Component** shows internal structure of a container.

**DDD Aggregates** are consistency boundaries within bounded contexts.

**Integration**: Components map to aggregates, value objects, domain services, and repositories.

**Example: Tax Calculation API Components**

```mermaid
C4Component
    title Component: Tax Calculation API (Bounded Context)

    Container_Boundary(taxAPI, "Tax Calculation API") {
        Component(taxController, "Tax Calculation Controller", "REST Controller", "Handles HTTP requests for tax")
        Component(calculateUseCase, "Calculate Tax Use Case", "Application Service", "Orchestrates tax calculation")

        Component(assessmentAggregate, "Tax Assessment Aggregate", "Domain Aggregate", "Enforces tax calculation rules")
        Component(thresholdVO, "Threshold Amount", "Value Object", "Gold/silver threshold threshold")
        Component(moneyVO, "Money", "Value Object", "Amount with currency")
        Component(hijriDateVO, "Hijri Date", "Value Object", "Islamic calendar date")

        Component(assessmentRepo, "Assessment Repository", "Repository", "Persists tax assessments")
        Component(eventPublisher, "Event Publisher", "Infrastructure", "Publishes domain events")
    }

    ContainerDb(taxDB, "Tax Database")
    Container(eventBus, "Event Bus")

    Rel(taxController, calculateUseCase, "Calls")
    Rel(calculateUseCase, assessmentAggregate, "Creates, modifies")
    Rel(assessmentAggregate, thresholdVO, "Uses")
    Rel(assessmentAggregate, moneyVO, "Uses")
    Rel(assessmentAggregate, hijriDateVO, "Uses")
    Rel(calculateUseCase, assessmentRepo, "Saves via")
    Rel(calculateUseCase, eventPublisher, "Publishes via")
    Rel(assessmentRepo, taxDB, "Reads/writes", "SQL")
    Rel(eventPublisher, eventBus, "Publishes to", "Kafka")
```

**DDD Aggregate Mapping**:

- **Tax Assessment Aggregate** (root): `assessmentAggregate`
- **Value Objects**: `thresholdVO`, `moneyVO`, `hijriDateVO`
- **Repository**: `assessmentRepo`
- **Application Service**: `calculateUseCase`
- **Presentation**: `taxController`

**Key Insight**: Aggregates are the core components within a bounded context. They enforce business rules and invariants.

### Level 4: Dynamic ↔ Domain Events

**C4 Dynamic** (Sequence Diagrams) show runtime behavior.

**DDD Domain Events** capture business occurrences.

**Integration**: Use sequence diagrams to show how domain events flow between aggregates and bounded contexts.

**Example: Tax Calculation Flow with Domain Events**

```mermaid
sequenceDiagram
    participant WH as Wealth Holder
    participant API as Tax API
    participant UC as Calculate Tax<br/>Use Case
    participant Agg as Tax Assessment<br/>Aggregate
    participant Repo as Assessment<br/>Repository
    participant Bus as Event Bus
    participant Pay as Payment<br/>Service

    WH->>API: POST /assessments<br/>{declarations: [...]}
    API->>UC: execute(dto)

    UC->>Agg: create(holderId, startDate)
    Agg-->>UC: [Assessment, AssessmentCreated event]

    loop For each declaration
        UC->>Agg: declareWealth(type, amount, date)
        Agg-->>UC: [Updated Assessment, WealthDeclared event]
    end

    UC->>Agg: finalize(thresholdThreshold, taxRate)
    Agg-->>UC: [Finalized Assessment, TaxCalculated event]

    UC->>Repo: save(assessment)
    Repo-->>UC: Ok

    UC->>Bus: publish(AssessmentCreated)
    UC->>Bus: publish(WealthDeclared)
    UC->>Bus: publish(TaxCalculated)

    Bus->>Pay: TaxCalculated event
    Pay->>Pay: Create payment invoice

    UC-->>API: Ok(assessment)
    API-->>WH: 201 Created<br/>{assessmentId, taxAmount}
```

**Domain Events**:

1. **AssessmentCreated**: Assessment initialized
2. **WealthDeclared**: Wealth declaration added
3. **TaxCalculated**: Tax amount finalized

**Key Insight**: Domain events enable eventual consistency between aggregates and bounded contexts.

## DDD Patterns in C4 Diagrams

### Bounded Context Canvas ↔ C4 Container

**Bounded Context Canvas** (DDD tool) documents:

- Name, description, strategic classification
- Ubiquitous language terms
- Business decisions and rules
- Key aggregates, entities, value objects
- Inbound/outbound dependencies

**C4 Container Diagram** visualizes:

- Containers (apps, databases)
- Integration points
- Technologies

**Integration**: Use Bounded Context Canvas as specification, C4 Container Diagram as visualization.

**Example**:

```markdown
# Bounded Context: Tax Calculation

**Strategic Classification**: Core Domain

**Description**: Calculates tax obligations based on Islamic jurisprudence rules.

**Ubiquitous Language**:

- Tax: Islamic almsgiving obligation
- Threshold: Minimum wealth threshold for tax obligation
- Hawl: Lunar year period for wealth assessment

**Key Aggregates**:

- TaxAssessment (root)

**Value Objects**:

- Money, HijriDate, ThresholdAmount, TaxRate

**Inbound Dependencies**:

- Wealth Holder (declares wealth via API)

**Outbound Dependencies**:

- Payment Processing (via TaxCalculated event)
- Gold Price API (fetches threshold threshold)
```

**C4 Container Diagram** (from earlier) visualizes this bounded context.

### Context Map ↔ C4 System Context + Container

**DDD Context Map** shows integration patterns:

- Shared Kernel
- Customer/Supplier
- Conformist
- Partnership
- Anti-Corruption Layer
- Open Host Service
- Published Language
- Separate Ways
- Big Ball of Mud

**C4 Diagrams** visualize relationships but don't specify patterns.

**Integration**: Annotate C4 relationships with DDD integration patterns.

**Example: Context Map with Integration Patterns**

```mermaid
C4Container
    title Container: Compliance-Compliant E-Commerce (with DDD Integration Patterns)

    Container_Boundary(taxContext, "Tax Calculation Context") {
        Container(taxAPI, "Tax API", "Node.js")
    }

    Container_Boundary(inventoryContext, "Inventory Management Context") {
        Container(inventoryAPI, "Inventory API", "Node.js")
    }

    Container_Boundary(orderContext, "Order Processing Context") {
        Container(orderAPI, "Order API", "Node.js")
    }

    System_Ext(paymentGateway, "Payment Gateway (External)")

    Rel(taxAPI, inventoryAPI, "Shared Kernel: Money, HijriDate value objects", "Internal API")
    Rel(orderAPI, inventoryAPI, "Customer/Supplier: Orders consume inventory", "REST/JSON")
    Rel(orderAPI, paymentGateway, "Conformist: Adapts to payment API", "HTTPS/JSON")
    Rel(taxAPI, paymentGateway, "Anti-Corruption Layer: Shields domain from payment changes", "HTTPS/JSON")
```

**Key Integration Patterns**:

- **Shared Kernel** (Tax ↔ Inventory): Shared value objects (Money, HijriDate)
- **Customer/Supplier** (Order ↔ Inventory): Order consumes inventory data
- **Conformist** (Order ↔ Payment): Order adapts to payment API
- **Anti-Corruption Layer** (Tax ↔ Payment): Tax protects domain model from payment changes

### Aggregate Boundaries in Component Diagrams

**DDD Aggregates** define transactional consistency boundaries.

**C4 Component Diagrams** show internal structure.

**Integration**: Group components by aggregate boundaries.

**Example: Highlighting Aggregate Boundary**

```mermaid
C4Component
    title Component: Tax Calculation API (Aggregate Boundaries)

    Container_Boundary(taxAPI, "Tax Calculation API") {
        Component(taxController, "Controller", "REST")
        Component(calculateUseCase, "Use Case", "Application Service")

        Boundary(assessmentAgg, "Tax Assessment Aggregate (Consistency Boundary)") {
            Component(assessmentRoot, "Tax Assessment", "Aggregate Root")
            Component(declarationEntity, "Wealth Declaration", "Entity")
            Component(moneyVO, "Money", "Value Object")
            Component(hijriDateVO, "Hijri Date", "Value Object")
        }

        Component(assessmentRepo, "Repository", "Infrastructure")
    }

    Rel(taxController, calculateUseCase, "Calls")
    Rel(calculateUseCase, assessmentRoot, "Modifies via")
    Rel(assessmentRoot, declarationEntity, "Contains")
    Rel(assessmentRoot, moneyVO, "Uses")
    Rel(assessmentRoot, hijriDateVO, "Uses")
    Rel(calculateUseCase, assessmentRepo, "Persists via")
```

**Key Insight**: Aggregate boundary = transactional boundary. All components within the boundary are saved atomically.

## Example: Full DDD-C4 Integration for Tax Calculation

### System Context (External Integration)

```mermaid
C4Context
    title System Context: Tax Calculation Platform

    Person(wealthHolder, "Wealth Holder", "Calculates tax")
    System(taxPlatform, "Tax Calculation Platform", "Calculates tax based on Islamic rules")

    System_Ext(goldAPI, "Gold Price API", "Threshold threshold source")
    System_Ext(paymentGW, "Payment Gateway", "Processes tax payments")

    Rel(wealthHolder, taxPlatform, "Declares wealth")
    Rel(taxPlatform, goldAPI, "Fetches threshold (Customer/Supplier)", "HTTPS")
    Rel(taxPlatform, paymentGW, "Processes payment (Anti-Corruption Layer)", "HTTPS")
```

**DDD Context Map**:

- **Tax Calculation** (Core) ↔ **Gold Pricing** (Generic, Customer/Supplier)
- **Tax Calculation** (Core) ↔ **Payment Processing** (Generic, Anti-Corruption Layer)

### Container (Bounded Contexts)

```mermaid
C4Container
    title Container: Tax Calculation Platform (Bounded Contexts)

    Person(wealthHolder, "Wealth Holder")

    Container_Boundary(taxContext, "Tax Calculation Context (Core Domain)") {
        Container(taxAPI, "Tax API", "Node.js/Express", "Tax calculation REST API")
        ContainerDb(taxDB, "Tax DB", "PostgreSQL", "Assessments, declarations")
    }

    Container(eventBus, "Event Bus", "Kafka", "Domain events")

    System_Ext(goldAPI, "Gold Price API")
    System_Ext(paymentGW, "Payment Gateway")

    Rel(wealthHolder, taxAPI, "Declares wealth", "HTTPS/JSON")
    Rel(taxAPI, taxDB, "Reads/writes", "SQL")
    Rel(taxAPI, eventBus, "Publishes TaxCalculated", "Kafka")
    Rel(taxAPI, goldAPI, "Fetches threshold", "HTTPS/JSON")
    Rel(taxAPI, paymentGW, "Processes payment", "HTTPS/JSON")
```

**Bounded Context**: `taxAPI` + `taxDB` = Tax Calculation Context

### Component (Aggregates)

```mermaid
C4Component
    title Component: Tax Calculation API (Aggregates and Layers)

    Container_Boundary(taxAPI, "Tax Calculation API") {
        Boundary(presentation, "Presentation Layer") {
            Component(taxController, "Tax Controller", "REST Controller")
        }

        Boundary(application, "Application Layer") {
            Component(calculateUseCase, "Calculate Tax Use Case", "Application Service")
        }

        Boundary(domain, "Domain Layer") {
            Boundary(assessmentAgg, "Tax Assessment Aggregate") {
                Component(assessmentRoot, "Tax Assessment", "Aggregate Root")
                Component(declarationEntity, "Wealth Declaration", "Entity")
            }

            Component(moneyVO, "Money", "Value Object")
            Component(thresholdVO, "Threshold Amount", "Value Object")
            Component(hijriDateVO, "Hijri Date", "Value Object")

            Component(assessmentRepoIface, "Assessment Repository Interface", "Port")
        }

        Boundary(infrastructure, "Infrastructure Layer") {
            Component(assessmentRepoImpl, "PostgreSQL Assessment Repository", "Adapter")
            Component(eventPublisher, "Kafka Event Publisher", "Adapter")
        }
    }

    ContainerDb(taxDB, "Tax DB")
    Container(eventBus, "Event Bus")

    Rel(taxController, calculateUseCase, "Calls")
    Rel(calculateUseCase, assessmentRoot, "Modifies")
    Rel(assessmentRoot, declarationEntity, "Contains")
    Rel(assessmentRoot, moneyVO, "Uses")
    Rel(assessmentRoot, thresholdVO, "Uses")
    Rel(assessmentRoot, hijriDateVO, "Uses")
    Rel(calculateUseCase, assessmentRepoIface, "Saves via")
    Rel(assessmentRepoImpl, assessmentRepoIface, "Implements")
    Rel(assessmentRepoImpl, taxDB, "Reads/writes", "SQL")
    Rel(calculateUseCase, eventPublisher, "Publishes via")
    Rel(eventPublisher, eventBus, "Publishes to", "Kafka")
```

**Layered Architecture**:

- **Presentation**: `taxController`
- **Application**: `calculateUseCase`
- **Domain**: `assessmentRoot`, `declarationEntity`, value objects, repository interface
- **Infrastructure**: `assessmentRepoImpl`, `eventPublisher`

**Aggregate**: `assessmentRoot` + `declarationEntity`

### Dynamic (Domain Events)

```mermaid
sequenceDiagram
    participant WH as Wealth Holder
    participant API as Tax Controller
    participant UC as Calculate Tax<br/>Use Case
    participant Agg as Tax Assessment<br/>Aggregate
    participant Repo as Assessment<br/>Repository
    participant Bus as Event Bus

    Note over WH,Bus: Tax Calculation Flow with Domain Events

    WH->>API: POST /assessments<br/>{holderId, declarations}
    API->>UC: execute(CreateAssessmentDTO)

    UC->>Agg: create(holderId, startDate)
    Agg-->>UC: [Assessment, AssessmentCreated]
    Note over Agg: Domain Event: AssessmentCreated

    UC->>Agg: declareWealth(Cash, $10000, date)
    Agg-->>UC: [Updated Assessment, WealthDeclared]
    Note over Agg: Domain Event: WealthDeclared

    UC->>Agg: finalize(thresholdThreshold, taxRate)
    Agg-->>UC: [Finalized Assessment, TaxCalculated]
    Note over Agg: Domain Event: TaxCalculated<br/>taxAmount = $250

    UC->>Repo: save(assessment)
    Repo-->>UC: Ok

    UC->>Bus: publish(AssessmentCreated)
    UC->>Bus: publish(WealthDeclared)
    UC->>Bus: publish(TaxCalculated)

    UC-->>API: Ok(assessment)
    API-->>WH: 201 Created<br/>{assessmentId: "abc", taxAmount: $250}
```

**Domain Events Published**:

1. **AssessmentCreated**: New tax assessment initialized
2. **WealthDeclared**: Wealth declaration added (cash $10,000)
3. **TaxCalculated**: Tax finalized ($250 = $10,000 × 2.5%)

## Cross-Referencing DDD and C4 Documentation

### Linking Bounded Context Documentation to C4 Diagrams

**Bounded Context Documentation** (in `docs/explanation/bounded-contexts/`):

```markdown
# Tax Calculation Bounded Context

**Architecture Diagrams**: See [C4 Container Diagram - Tax Calculation](../c4-architecture-model/README.md#container-diagrams)

**Aggregates**:

- TaxAssessment (see [C4 Component Diagram - Tax API](../c4-architecture-model/README.md#component-diagrams))

**Integration**:

- External: Payment Gateway (Anti-Corruption Layer) - [System Context](../c4-architecture-model/README.md#system-context-diagrams)
- Internal: Shared Kernel with Inventory Management - [Context Map](./ex-so-ar-dodrdedd__04-context-mapping.md)
```

### Linking C4 Diagrams to DDD Documentation

**C4 Container Diagram Documentation**:

```markdown
# C4 Container Diagram: Tax Calculation Platform

**Bounded Contexts**:

- Tax Calculation Context (Core Domain) - [DDD Documentation](./ex-so-ar-dodrdedd__03-bounded-contexts.md)
- Inventory Management Context (Supporting) - [DDD Documentation](./ex-so-ar-dodrdedd__03-bounded-contexts.md)

**Integration Patterns**:

- Tax ↔ Payment Gateway: Anti-Corruption Layer - [Context Mapping](./ex-so-ar-dodrdedd__04-context-mapping.md#anti-corruption-layer)
```

### Linking Aggregate Documentation to C4 Component Diagrams

**Aggregate Documentation** (using template):

```markdown
# Tax Assessment Aggregate

**Architecture Diagram**: [C4 Component Diagram - Tax API](../c4-architecture-model/README.md#component-diagrams)

**Aggregate Root**: TaxAssessment

**Entities**:

- WealthDeclaration

**Value Objects**:

- Money, ThresholdAmount, TaxRate, HijriDate

**Invariants**:

1. Assessment can only be finalized if hawl (lunar year) is complete
2. Total wealth must meet or exceed threshold threshold
3. Tax amount = wealth × tax rate (2.5% standard)

**Domain Events**:

- AssessmentCreated, WealthDeclared, TaxCalculated (see [C4 Dynamic Diagram](../c4-architecture-model/README.md#dynamic-diagrams))
```

## Tools and Notation

### Mermaid C4 Support

Mermaid supports C4 diagrams with `C4Context`, `C4Container`, `C4Component`, and `C4Dynamic` (experimental).

**Example**:

```mermaid
C4Context
    title System Context Example

    Person(user, "User", "Description")
    System(system, "System", "Description")
    System_Ext(external, "External System", "Description")

    Rel(user, system, "Uses", "Protocol")
    Rel(system, external, "Calls", "HTTPS/JSON")
```

**WCAG AA-Compliant Colors** (from [Color Palette](./templates/ex-so-ar-dodrdedd-te__color-palette.md)):

- Blue `#0173B2`: Bounded Contexts, Aggregate Roots
- Teal `#029E73`: Entities, Supporting Subdomains
- Orange `#DE8F05`: Value Objects, Domain Events
- Purple `#CC78BC`: External Systems
- Gray `#808080`: Infrastructure

### PlantUML C4 Support

PlantUML has comprehensive C4 support via C4-PlantUML library.

**Example**:

```plantuml
@startuml
!include https://raw.githubusercontent.com/plantuml-stdlib/C4-PlantUML/master/C4_Container.puml

Person(user, "User")
Container(api, "API", "Node.js", "REST API")
ContainerDb(db, "Database", "PostgreSQL", "Stores data")

Rel(user, api, "Uses", "HTTPS")
Rel(api, db, "Reads/writes", "SQL")

@enduml
```

### Structurizr

Structurizr is a dedicated C4 modeling tool with versioning and workspace management.

**Benefits**:

- DSL for defining diagrams
- Automatic layout
- Version control
- Integration with documentation

**Example DSL**:

```
workspace {
    model {
        user = person "Wealth Holder"
        taxSystem = softwareSystem "Tax Platform" {
            taxAPI = container "Tax API" "Node.js"
            taxDB = container "Tax DB" "PostgreSQL"
        }

        user -> taxAPI "Declares wealth"
        taxAPI -> taxDB "Reads/writes"
    }

    views {
        systemContext taxSystem {
            include *
        }

        container taxSystem {
            include *
        }
    }
}
```

## Best Practices

### 1. Keep DDD and C4 Documentation Synchronized

**Challenge**: Diagrams and documentation drift out of sync.

**Solution**: Co-locate documentation and diagrams, cross-reference liberally.

```
docs/explanation/
├── domain-driven-design-ddd/
│   ├── bounded-contexts/
│   │   ├── tax-calculation.md       # References C4 diagrams
│   │   └── inventory-management.md
│   └── aggregates/
│       └── tax-assessment.md         # References C4 component diagram
├── c4-architecture-model/
│   ├── ex-c4__01-system-context-diagrams.md  # References bounded contexts
│   ├── ex-c4__02-container-diagrams.md       # References bounded contexts
│   └── ex-c4__03-component-diagrams.md       # References aggregates
```

### 2. Use Bounded Contexts as Container Boundaries

**Principle**: One bounded context = one or more containers (typically app + database).

**Anti-Pattern**: Splitting a bounded context across multiple deployable services.

**Example**:

- **Good**: Tax Calculation Context = `taxAPI` + `taxDB`
- **Bad**: Tax Calculation Context = `taxAPI1` + `taxAPI2` + `taxDB1` + `taxDB2` (over-engineered)

### 3. Annotate C4 Relationships with DDD Integration Patterns

**C4 Relationship**:

```
Rel(taxAPI, inventoryAPI, "Shares Money value object", "Internal API")
```

**Add DDD Pattern**:

```
Rel(taxAPI, inventoryAPI, "Shared Kernel: Money, HijriDate", "Internal API")
```

### 4. Use Dynamic Diagrams for Domain Event Flows

**C4 Dynamic Diagrams** (sequence diagrams) naturally show domain events flowing between components.

**Example**:

```mermaid
sequenceDiagram
    participant Agg as Tax Assessment
    participant Bus as Event Bus
    participant Pay as Payment Service

    Agg->>Bus: TaxCalculated event
    Bus->>Pay: TaxCalculated event
    Pay->>Pay: Create payment invoice
```

### 5. Link Aggregate Design Templates to Component Diagrams

**Template**: [Aggregate Design Template](./templates/ex-so-ar-dodrdedd-te__aggregate-design-template.md)

**Component Diagram**: Shows aggregate boundaries visually

**Integration**: Each aggregate documented in template should have corresponding component diagram.

## Summary

DDD and C4 are complementary:

**Mappings:**

- **System Context** ↔ **Context Maps** (external systems)
- **Container** ↔ **Bounded Contexts** (deployment units)
- **Component** ↔ **Aggregates** (internal structure)
- **Dynamic** ↔ **Domain Events** (behavior over time)

**Benefits of Integration:**

- **Visual Communication**: C4 diagrams make DDD concepts accessible to non-technical stakeholders
- **Documentation Coherence**: Cross-references between DDD and C4 docs prevent drift
- **Architectural Clarity**: Bounded contexts as containers, aggregates as components
- **Event Visualization**: Domain events in sequence diagrams

**Best Practices:**

1. Keep DDD and C4 documentation synchronized
2. Use bounded contexts as container boundaries
3. Annotate C4 relationships with DDD integration patterns
4. Use dynamic diagrams for domain event flows
5. Link aggregate design templates to component diagrams

**Tools:**

- **Mermaid**: Simple C4 diagrams in markdown
- **PlantUML**: Comprehensive C4 support
- **Structurizr**: Dedicated C4 modeling tool

## Next Steps

- **[C4 Architecture Model](../c4-architecture-model/README.md)** - Comprehensive C4 documentation
- **[Bounded Contexts](./ex-so-ar-dodrdedd__03-bounded-contexts.md)** - DDD strategic pattern
- **[Aggregates](./ex-so-ar-dodrdedd__09-aggregates.md)** - DDD tactical pattern
- **[Context Mapping](./ex-so-ar-dodrdedd__04-context-mapping.md)** - Integration patterns
- **[Layered Architecture](./ex-so-ar-dodrdedd__15-layered-architecture.md)** - Organizing layers in C4 component diagrams

## References

- Simon Brown, "The C4 Model for Visualising Software Architecture"
- [C4 Model](https://c4model.com/) - Official C4 documentation
- [Structurizr](https://structurizr.com/) - C4 modeling tool
- [C4-PlantUML](https://github.com/plantuml-stdlib/C4-PlantUML) - PlantUML C4 library
- Eric Evans, "Domain-Driven Design" (2003)
- Vaughn Vernon, "Implementing Domain-Driven Design" (2013)

## Related Principles

DDD and C4 integration embodies shared principles:

- **[Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** - Both DDD and C4 make boundaries, relationships, and responsibilities explicit
- **[Simplicity Over Complexity](../../../../../governance/principles/general/simplicity-over-complexity.md)** - Both favor pragmatic simplicity over complex formalism

See [C4 Model](../c4-architecture-model/README.md) and [Software Engineering Principles](../../../../../governance/principles/software-engineering/README.md).
