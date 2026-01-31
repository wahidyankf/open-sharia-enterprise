---
title: "Overview"
date: 2026-01-30T00:00:00+07:00
draft: false
weight: 10000000
---

The C4 Model is a hierarchical approach to visualizing software architecture through four levels of abstraction: Context, Containers, Components, and Code. Created by Simon Brown, it provides a systematic way to communicate architecture to different audiences with varying technical backgrounds.

## 🎯 What is the C4 Model?

The C4 Model breaks down system visualization into four complementary diagrams, each zooming in one level deeper:

**Four Levels of Abstraction:**

1. **Context (Level 1)** - System boundaries and external dependencies
2. **Container (Level 2)** - High-level technology choices and deployable units
3. **Component (Level 3)** - Logical groupings of functionality within containers
4. **Code (Level 4)** - Implementation details (classes, functions, packages)

**Key Principle**: Start with the big picture (Context) and progressively zoom into details as needed. Not every system requires all four levels.

## 📐 Why Use C4 Model?

**Communication Benefits:**

- **Multi-audience**: Different diagrams for different stakeholders (executives see Context, developers see Components)
- **Consistent notation**: Standard shapes and relationships reduce ambiguity
- **Progressive disclosure**: Show only the detail level your audience needs
- **Living documentation**: Diagrams evolve with the system, staying relevant

**Design Benefits:**

- **Clarity**: Forces you to think about boundaries, responsibilities, and dependencies
- **Modularity**: Explicit containers and components promote loose coupling
- **Technology mapping**: Clear picture of where each technology fits
- **Onboarding**: New team members understand the system faster

## 🏗️ The Four Levels Explained

### Level 1: System Context Diagram

**Purpose**: Show how your system fits into the broader environment

**What to Include:**

- Your system (single box)
- External actors (users, other systems, external services)
- Relationships and interactions between them

**Audience**: Everyone (executives, stakeholders, developers, operations)

**Example Scenario**: "Our e-commerce platform interacts with customers (web/mobile), payment gateways, inventory systems, and email services."

**When to Use:**

- Initial architecture discussions
- Presenting to non-technical stakeholders
- Understanding system boundaries
- Planning integrations with external systems

### Level 2: Container Diagram

**Purpose**: Show the high-level technology building blocks of your system

**What to Include:**

- Deployable/runnable units (web apps, mobile apps, databases, microservices, message queues)
- Technology choices for each container (e.g., "React SPA", "Spring Boot API", "PostgreSQL")
- Communication protocols between containers (HTTP/REST, gRPC, message queues)

**Audience**: Technical stakeholders, architects, senior developers

**Example Scenario**: "Our platform consists of a React web app, iOS/Android apps, three Spring Boot microservices (User, Product, Order), PostgreSQL databases, Redis cache, and RabbitMQ message queue."

**When to Use:**

- Architectural planning and design reviews
- Infrastructure provisioning
- Deployment planning
- Technology stack discussions

### Level 3: Component Diagram

**Purpose**: Show major structural building blocks within a container and their interactions

**What to Include:**

- Logical groupings of related functionality (controllers, services, repositories, adapters)
- Relationships between components
- External dependencies (databases, APIs, queues)

**Audience**: Developers, architects working on specific containers

**Example Scenario**: "The Order Service container has components: OrderController (REST API), OrderService (business logic), OrderRepository (database access), PaymentAdapter (external payment integration), NotificationPublisher (message queue publishing)."

**When to Use:**

- Detailed design of individual services/applications
- Code organization decisions
- Understanding service internals
- Dependency management

### Level 4: Code Diagram

**Purpose**: Show implementation-level details (classes, functions, packages)

**What to Include:**

- Class diagrams (UML)
- Package/module structures
- Implementation patterns

**Audience**: Developers actively working on the code

**When to Use:**

- Rarely needed - modern IDEs generate these automatically
- Complex algorithmic logic that benefits from visual documentation
- Design pattern implementation details

**Recommendation**: Skip Level 4 unless absolutely necessary. Code is self-documenting through good naming and structure.

## 🌐 Technology-Agnostic Approach

The C4 Model works with **any technology stack**:

- **Language-agnostic**: Diagrams don't depend on Java, Go, Python, or Node.js
- **Framework-independent**: Applies to Spring Boot, Django, Express, Phoenix equally
- **Cloud-neutral**: Works for AWS, GCP, Azure, or on-premise deployments
- **Tool-flexible**: Draw with any tool (PlantUML, Draw.io, Mermaid, Structurizr)

**Focus**: Architecture and relationships, not implementation details.

## 📏 Best Practices

**Do:**

- ✅ Start with Context diagram - establish system boundaries first
- ✅ Use consistent notation across all diagrams
- ✅ Keep diagrams simple - avoid cluttering with too many elements
- ✅ Use meaningful names for systems, containers, and components
- ✅ Show technology choices explicitly (e.g., "PostgreSQL 14", "Spring Boot 3.2")
- ✅ Update diagrams when architecture changes
- ✅ Include legends explaining shapes and colors
- ✅ Limit to 5-9 elements per diagram (cognitive load management)

**Don't:**

- ❌ Mix abstraction levels in a single diagram
- ❌ Show implementation details in high-level diagrams
- ❌ Create diagrams that duplicate information already in code
- ❌ Use C4 for documenting every tiny detail - focus on significant structures
- ❌ Forget your audience - match diagram detail to their needs
- ❌ Let diagrams become stale - architecture documentation must evolve

## 🔧 Tools for Creating C4 Diagrams

**Structurizr (Recommended)**:

- Purpose-built for C4 Model
- Text-based DSL (diagrams as code)
- Automatic layout and consistency
- Supports all four levels

**PlantUML**:

- Text-based, version-controllable
- Wide IDE support
- C4-PlantUML library available
- Good for developers who prefer code

**Draw.io / Diagrams.net**:

- Visual drag-and-drop
- Free and open-source
- C4 shape libraries available
- Easy collaboration

**Mermaid**:

- Markdown-embeddable
- Renders in GitHub, GitLab, documentation sites
- Simple syntax
- Limited C4-specific features (requires manual formatting)

**LucidChart / Visio**:

- Professional diagramming tools
- C4 templates available
- Collaboration features
- Commercial licenses

## 💡 When to Use C4 Model

**Ideal Scenarios:**

- **Greenfield projects**: Designing architecture from scratch
- **System modernization**: Understanding legacy systems before refactoring
- **Team onboarding**: Helping new developers understand the system
- **Architectural reviews**: Presenting design decisions to stakeholders
- **Multi-team coordination**: Shared understanding across teams
- **Documentation requirements**: Organizations requiring formal architecture docs

**Less Suitable For:**

- **Very simple systems**: Single-file applications don't need four diagram levels
- **Highly dynamic architectures**: Systems changing daily make diagrams obsolete quickly
- **Algorithm documentation**: Use UML sequence diagrams or flowcharts instead
- **Data modeling**: Use ER diagrams for database schemas

## 🚀 Getting Started

**Step-by-Step Approach:**

1. **Start with Context**: Draw your system and its external dependencies
2. **Add Containers**: Break your system into deployable units
3. **Detail Components**: Zoom into critical containers and show internal structure
4. **Skip Code**: Let your IDE generate code-level diagrams if needed

**First Diagram Checklist:**

- [ ] Identified system boundaries
- [ ] Listed all external actors and systems
- [ ] Defined major containers (web app, API, database, etc.)
- [ ] Chose appropriate technology for each container
- [ ] Mapped communication protocols
- [ ] Kept diagrams simple and focused

## 🔗 Related Content

- [**System Design Cases**](/en/learn/software-engineering/system-design/cases) - See C4 principles applied in real-world designs
- [**Domain-Driven Design**](/en/learn/software-engineering/architecture/domain-driven-design-ddd) - Complements C4 with domain modeling
- [**Finite State Machine**](/en/learn/software-engineering/architecture/finite-state-machine-fsm) - Use for component-level behavior modeling

## 📚 Further Reading

**Official Resources:**

- [C4 Model Official Website](https://c4model.com/) - Comprehensive documentation and examples
- [Structurizr](https://structurizr.com/) - Simon Brown's tool for C4 diagrams
- [C4-PlantUML](https://github.com/plantuml-stdlib/C4-PlantUML) - PlantUML library for C4

**Books:**

- _Software Architecture for Developers_ by Simon Brown - Creator of C4 Model
- _Visualising Software Architecture_ by Simon Brown - Deep dive into C4

**Community:**

- [C4 Model Examples](https://github.com/structurizr/examples) - Real-world C4 diagram examples
- [Awesome C4](https://github.com/mariovtech/awesome-c4-model) - Curated C4 resources

---

**Key Takeaway**: The C4 Model provides a systematic, scalable way to visualize software architecture. Start with the big picture (Context), progressively zoom in (Container, Component), and only go to code level when absolutely necessary. Use the right diagram for the right audience.
