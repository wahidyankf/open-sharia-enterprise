# Notation and Conventions

### Core Abstractions

The C4 model uses five fundamental elements:

1. **Person**: Human users or actors (internal or external)
2. **Software System**: The highest level of abstraction (the system being documented or external systems)
3. **Container**: Deployable/executable unit (application, database, file system)
4. **Component**: Grouping of related functionality within a container
5. **Relationship**: Line connecting elements showing interaction

### Visual Conventions

**Flexibility First - Notation Independence**:

The C4 model is **deliberately and intentionally notation-independent** - this is a core design principle, not an oversight.

**Why Notation Independence Matters**:

Unlike UML (which prescribes specific shapes, stereotypes, and symbols), C4 model allows teams to choose notation that works for their context:

- **Team Preference**: Use boxes, circles, hexagons - whatever your team finds clearest
- **Tool Flexibility**: Not locked into specific diagramming software
- **Cultural Adaptation**: Different regions and industries have different visual conventions
- **Accessibility**: Teams can adapt notation for accessibility needs (color blindness, print-friendly)
- **Evolution**: Notation can evolve as tools and standards change without invalidating the model

**What C4 Prescribes** (non-negotiable):

1. **Four levels of abstraction**: Context, Container, Component, Code
2. **Element types**: Person, Software System, Container, Component, Relationship
3. **Labeling format**: Name, Type, Technology (for containers/components)
4. **Hierarchical zooming**: Each level zooms into the previous

**What C4 Doesn't Prescribe** (team decides):

- Shape types (rectangles vs. circles vs. hexagons)
- Color schemes (as long as they're distinguishable)
- Layout algorithms (manual vs. automatic)
- Specific notation standards (UML vs. custom)
- Diagram rendering tools (draw.io vs. Mermaid vs. Structurizr)

**Key Principle**: The **model** (abstractions and concepts) is standardized. The **notation** (visual representation) is flexible.

**Emphasis**:

- **Clarity**: Diagrams should be immediately understandable
- **Consistency**: Use the same visual language within and across diagrams
- **Accessibility**: Consider color blindness and black-and-white printing
- **Team Adoption**: Choose notation your team actually uses, not theoretical perfection

**Required Elements**:

Every C4 diagram must include:

1. **Title**: Describing diagram type and scope (e.g., "System Context - Open Sharia Enterprise")
2. **Key/Legend**: Explaining notation used (shapes, colors, line styles, etc.)
3. **Element Types**: Explicitly labeling each element type (Person, Software System, Container, Component)

**Element Labeling**:

Each element should show:

- **Name**: Clear, descriptive name
- **Type**: Element type in brackets (e.g., [Software System], [Container: Next.js])
- **Description**: Brief summary of responsibilities (one line)
- **Technology** (for containers/components): Technology stack or implementation approach

Example:

```
orca-grid-be
[Container: Spring Boot]
Backend services for enterprise applications
```

#### Element Labeling Format Standards

The C4 model recommends a specific three-line format for labeling elements in diagrams:

**Standard Format**:

```
[Name]
[Type: Technology]
Brief description of responsibilities
```

**Line-by-Line Breakdown**:

1. **Line 1 - Name**: The element's unique identifier (e.g., "User Service", "PostgreSQL", "Payment Gateway")
2. **Line 2 - Type with Technology**: Element classification and implementation technology in brackets
   - System Context: `[Software System]` or `[Person]`
   - Container: `[Container: Spring Boot]`, `[Container: React]`, `[Database: PostgreSQL]`
   - Component: `[Component: REST Controller]`, `[Component: Repository]`
3. **Line 3 - Description**: One-line summary of what this element does or provides (focus on responsibilities, not implementation details)

**Good vs. Bad Labeling Examples**:

| Diagram Level            | Good Example                                                                             | Bad Example               | Why Bad Example Fails                                                             |
| ------------------------ | ---------------------------------------------------------------------------------------- | ------------------------- | --------------------------------------------------------------------------------- |
| **System Context**       | `E-Commerce Platform`<br/>`[Software System]`<br/>Enables online shopping                | `System`<br/>Main app     | No type specified, vague name, no description                                     |
| **Container**            | `Order Service`<br/>`[Container: Spring Boot]`<br/>Handles order processing              | `OrderService`<br/>`Java` | No brackets around type, missing "Container" classification, technology too vague |
| **Container (Database)** | `Order Database`<br/>`[Database: PostgreSQL]`<br/>Stores order and payment data          | `DB`<br/>`Postgres`       | Abbreviation unclear, missing database schema purpose                             |
| **Component**            | `OrderController`<br/>`[Component: REST Controller]`<br/>Exposes order management API    | `Controller`<br/>REST     | Too generic name, missing component classification, no description                |
| **Person**               | `Customer`<br/>`[Person]`<br/>Shops online via web/mobile                                | `User`                    | Too generic (which type of user?), no description                                 |
| **External System**      | `Payment Gateway`<br/>`[External System: Stripe API]`<br/>Processes credit card payments | `Stripe`                  | Missing "External System" classification, no responsibility description           |

**Per-Diagram-Type Labeling Rules**:

| Diagram Type       | Name Format                                       | Type Format                         | Technology Detail                                        | Description Focus                  |
| ------------------ | ------------------------------------------------- | ----------------------------------- | -------------------------------------------------------- | ---------------------------------- |
| **System Context** | Business-oriented name (E-Commerce Platform)      | `[Software System]` or `[Person]`   | Omit technology (high-level view)                        | Business capability                |
| **Container**      | Technical component name (Order Service, User DB) | `[Container: Technology]`           | Specific technology (Spring Boot, PostgreSQL)            | Technical responsibility           |
| **Component**      | Code-level name (OrderController, UserRepository) | `[Component: Pattern/Type]`         | Design pattern or type (REST Controller, JPA Repository) | Specific function within container |
| **Code**           | Class/interface name (Order, IOrderRepository)    | `[Class]`, `[Interface]`, `[Table]` | Language-specific (Java, TypeScript)                     | Implementation detail              |

**Technology Specificity Guidelines**:

| Diagram Level      | Technology Detail                    | Examples                                                                                           |
| ------------------ | ------------------------------------ | -------------------------------------------------------------------------------------------------- |
| **System Context** | None - focus on business purpose     | `[Software System]` (NOT `[Software System: Java]`)                                                |
| **Container**      | Specific framework or platform       | `[Container: Spring Boot]`, `[Container: React]`, `[Database: PostgreSQL]`, `[Message Bus: Kafka]` |
| **Component**      | Design pattern or architectural role | `[Component: REST Controller]`, `[Component: Service Layer]`, `[Component: Repository]`            |
| **Code**           | Specific language/framework class    | `[Class: @RestController]`, `[Interface: JpaRepository]`, `[Entity: @Entity]`                      |

**Common Labeling Mistakes to Avoid**:

1. **Missing Brackets**: Writing `Container: Spring Boot` instead of `[Container: Spring Boot]`
2. **No Element Type**: Writing `Spring Boot` instead of `[Container: Spring Boot]`
3. **Vague Technology**: Writing `[Container: Java]` instead of `[Container: Spring Boot]` (be specific about framework, not just language)
4. **Too Much Detail at High Levels**: Writing `[Software System: Microservices with Spring Boot, React, PostgreSQL]` (save details for Container diagram)
5. **Implementation in Description**: Writing "Implemented in Java using Spring Boot" instead of focusing on responsibilities (implementation shown in Type line)
6. **Generic Names**: Using "Service", "Database", "API" without qualifying which service/database/API
7. **Inconsistent Terminology**: Mixing "Service" and "Microservice" for same type of container
8. **Abbreviations**: Using "DB" instead of "Database", "FE" instead of "Frontend"

**Special Cases**:

**External Systems**:

```
Payment Gateway
[External System: Stripe API]
Processes credit card transactions
```

Use `[External System: Technology]` to distinguish from in-scope containers.

**Cloud Services**:

```
S3 Bucket
[Cloud Storage: AWS S3]
Stores uploaded user files
```

Use `[Cloud Storage]`, `[Cloud Database]`, `[Cloud Function]` for managed cloud services.

**Legacy Systems**:

```
Legacy Inventory System
[Software System: Mainframe COBOL]
Manages warehouse stock levels
```

Consider adding "(Legacy)" to description or type to signal technical debt.

**Shared Databases** (anti-pattern in microservices):

```
Shared Database
[Database: PostgreSQL - SHARED]
Used by Order and Payment services
```

Explicitly label shared databases to highlight architectural coupling.

**Best Practices**:

- **Be consistent**: Use the same format and terminology across all diagrams
- **Focus on clarity**: If three lines feel too verbose, omit description for simple elements
- **Align with team conventions**: Adapt format to what your team finds most readable
- **Use full names**: Avoid abbreviations unless universally understood in your domain
- **Update labels when refactoring**: Keep diagram labels synchronized with actual system names
- **Include version numbers for external systems**: E.g., `[External System: Stripe API v2024-11-20]` when API version matters

**Relationship Labeling**:

Lines connecting elements should:

- Be **unidirectional** (show direction with arrow)
- Show **single relationship** per line (not bidirectional)
- Include **descriptive label** (better than generic "Uses")
- Specify **technology/protocol** for inter-process communication (HTTP/REST, gRPC, WebSocket, etc.)

Example relationships:

- "Reads from and writes to [JDBC/SQL]"
- "Makes REST API calls to [HTTPS]"
- "Publishes events to [RabbitMQ/AMQP]"
- "Authenticates using [OAuth 2.0]"

### Color Coding

Colors are not mandated but recommended for clarity:

**Common Conventions**:

- **In-scope system/containers**: Blue or primary color
- **External systems**: Gray or muted color
- **People/actors**: Green or distinct color
- **Databases**: Red or contrasting color
- **Background/grouping**: Light colors for subgraphs

**Accessibility Considerations**:

- Use patterns or labels in addition to color
- Test for color blindness compatibility
- Ensure sufficient contrast for readability
- Consider black-and-white printing

**Example from Open Sharia Enterprise**:

```
- Open Sharia Enterprise Platform: #0077b6 (primary blue)
- External Users: #2a9d8f (teal)
- External Systems: #6a4c93 (purple)
- Backend Services: #e76f51 (coral)
- Test Suites: #f4a261 (orange)
- Infrastructure: #457b9d (slate blue)
- Databases: #9d0208 (dark red)
```
