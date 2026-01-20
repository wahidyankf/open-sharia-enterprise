---
title: "[Container Name] - Component Diagram"
description: Shows the internal structure and components of [Container Name]
category: explanation
subcategory: architecture
tags:
  - c4-model
  - component-diagram
  - level-3
  - your-system-name
created: YYYY-MM-DD
updated: YYYY-MM-DD
---

# [Container Name] - Component Diagram

## Purpose

This diagram zooms into **[Container Name]** to show its internal components. A component is:

- A grouping of related functionality (class, module, service, namespace)
- Encapsulated behind an interface
- Interacts with other components via well-defined contracts

**Audience**: Developers, architects working on this specific container.

## Diagram

```mermaid
graph TD
    %% Define external actor
    ExternalUser[User/External System<br/>Caller]

    %% Define container boundary
    subgraph "[Container Name]<br/>Container: [Technology]"
        Component1[Component 1<br/>Component: [Type]<br/>e.g. Controller, Service, Repository<br/>Responsibilities]
        Component2[Component 2<br/>Component: [Type]<br/>e.g. Business Logic, Validator<br/>Responsibilities]
        Component3[Component 3<br/>Component: [Type]<br/>e.g. Data Access, Mapper<br/>Responsibilities]
        Component4[Component 4<br/>Component: [Type]<br/>e.g. Integration, Client<br/>Responsibilities]
    end

    %% Define external dependencies
    Database[(Database<br/>Container: Technology)]
    ExternalAPI[External API<br/>External System]

    %% Define relationships
    ExternalUser -->|Calls API endpoint<br/>HTTP POST /api/resource| Component1
    Component1 -->|Delegates to<br/>Method call| Component2
    Component2 -->|Validates data via<br/>Method call| Component3
    Component3 -->|Persists data via<br/>SQL INSERT| Database
    Component2 -->|Calls external service via<br/>HTTPS/REST API| Component4
    Component4 -->|Sends request to<br/>HTTPS/REST API| ExternalAPI

    %% Apply WCAG-compliant colors
    style ExternalUser fill:#029E73,stroke:#000000,color:#ffffff
    style Component1 fill:#0173B2,stroke:#000000,color:#ffffff
    style Component2 fill:#0173B2,stroke:#000000,color:#ffffff
    style Component3 fill:#0173B2,stroke:#000000,color:#ffffff
    style Component4 fill:#0173B2,stroke:#000000,color:#ffffff
    style Database fill:#DE8F05,stroke:#000000,color:#ffffff
    style ExternalAPI fill:#CC78BC,stroke:#000000,color:#ffffff
```

## Legend

| Element             | Color  | Description                                      |
| ------------------- | ------ | ------------------------------------------------ |
| **User/External**   | Teal   | Callers from outside this container              |
| **Component**       | Blue   | Internal components (classes, modules, services) |
| **Database**        | Orange | Data stores accessed by this container           |
| **External System** | Purple | External systems this container integrates with  |

## Component Descriptions

### [Component 1: Entry Point / Controller]

- **Type**: [e.g., Controller, Router, Handler]
- **Responsibilities**:
  - [e.g., Handles HTTP requests and responses]
  - [e.g., Request validation and error handling]
  - [e.g., Delegates to business logic layer]
- **Technology**: [e.g., Express.js route handlers, Spring Boot @RestController]
- **Interfaces**:
  - `[Method/Function 1]`: [Description]
  - `[Method/Function 2]`: [Description]

### [Component 2: Business Logic]

- **Type**: [e.g., Service, Use Case, Interactor]
- **Responsibilities**:
  - [e.g., Core business logic for order processing]
  - [e.g., Orchestrates calls to repositories and external services]
  - [e.g., Enforces business rules and invariants]
- **Technology**: [e.g., Plain TypeScript classes, Java service classes]
- **Interfaces**:
  - `[Method/Function 1]`: [Description]
  - `[Method/Function 2]`: [Description]

### [Component 3: Data Access]

- **Type**: [e.g., Repository, Data Access Object (DAO), Mapper]
- **Responsibilities**:
  - [e.g., Abstracts database access]
  - [e.g., Executes SQL queries]
  - [e.g., Maps between domain models and database schemas]
- **Technology**: [e.g., TypeORM repositories, JPA repositories]
- **Interfaces**:
  - `[Method/Function 1]`: [Description]
  - `[Method/Function 2]`: [Description]

### [Component 4: External Integration (Optional)]

- **Type**: [e.g., Client, Adapter, Gateway]
- **Responsibilities**:
  - [e.g., Integrates with external payment gateway]
  - [e.g., Handles retries and error handling for external calls]
  - [e.g., Transforms external API responses to internal models]
- **Technology**: [e.g., Axios HTTP client, Feign client]
- **Interfaces**:
  - `[Method/Function 1]`: [Description]
  - `[Method/Function 2]`: [Description]

## Component Organization

### Layered Architecture (Example)

If using layered architecture (common in OOP systems):

- **Presentation Layer**: [Component 1 - Controller]
- **Business Logic Layer**: [Component 2 - Service]
- **Data Access Layer**: [Component 3 - Repository]
- **Integration Layer**: [Component 4 - External Client]

### Functional Modules (Example)

If using functional programming (Elixir, Haskell):

- **Context/Module 1**: [e.g., Accounts context with user-related functions]
- **Context/Module 2**: [e.g., Orders context with order-related functions]
- **Shared/Common**: [e.g., Validation, Error handling utilities]

## Key Interactions

### [User → Component 1 → Component 2]

1. User sends HTTP request to Component 1 (Controller)
2. Component 1 validates request and delegates to Component 2 (Service)
3. Component 2 executes business logic

### [Component 2 → Component 3 → Database]

1. Component 2 calls Component 3 (Repository) to persist data
2. Component 3 executes SQL query to Database
3. Result is returned back through the chain

## Design Patterns Used

- [e.g., Dependency Injection: Components receive dependencies via constructor]
- [e.g., Repository Pattern: Data access abstracted behind repository interface]
- [e.g., Adapter Pattern: External API wrapped in adapter component]

## Notes

- Add any important notes about component interactions, design decisions, or refactoring plans
- Mention if this is a legacy structure being migrated

## Related Diagrams

- **[Container Diagram](./blank-container-diagram.md)**: Shows where [Container Name] fits in the system
- **[Code Diagram (Optional)](../ex-c4armo__05-level-4-code.md)**: Shows implementation details (UML class diagram)
- **[Dynamic Diagram](./blank-dynamic-diagram.md)**: Shows runtime interactions between components

---

**Template Instructions**:

1. Replace `[Container Name]` with the actual container you're documenting (e.g., "API Service")
2. Replace `[Technology]` with the container's technology stack (e.g., "Node.js/Express")
3. Replace component placeholders (Component1, Component2, etc.) with actual components
4. Update component types based on your paradigm:
   - **OOP**: Controller, Service, Repository, Entity, Factory
   - **FP**: Context, Router, Schema, Function Module
   - **Procedural**: Package, Module, Handler
5. Add or remove components as needed (most containers have 3-8 components)
6. Update relationship labels to describe HOW components interact (method calls, function composition)
7. Fill in "Component Descriptions", "Component Organization", and "Design Patterns" sections
8. Update frontmatter (title, description, tags, created/updated dates)
9. Delete this template instructions section when done

**See**:

- **OOP**: [Level 3: Component Diagram](../ex-c4armo__03-level-3-component.md)
- **FP**: [Level 3: Component (Functional Programming)](../ex-c4armo__04-level-3-component-fp.md)
- **Paradigm Guidance**: [Paradigm Considerations](../ex-c4armo__10-paradigm-considerations.md)
