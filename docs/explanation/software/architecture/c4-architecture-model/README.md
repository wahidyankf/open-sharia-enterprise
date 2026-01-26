---
title: "C4 Architecture Model"
description: Comprehensive explanation of the C4 model for visualizing software architecture through hierarchical abstraction levels
category: explanation
subcategory: architecture
tags:
  - c4-model
  - architecture
  - diagrams
  - visualization
  - software
  - documentation
  - simon-brown
created: 2026-01-18
updated: 2026-01-19
---

# C4 Architecture Model

Comprehensive explanation of the C4 model for visualizing software architecture through hierarchical abstraction levels.

## Overview

The **C4 model** is an easy-to-learn, developer-friendly approach to software architecture diagramming created by Simon Brown. It provides a systematic way to visualize software architecture at different levels of detail, making it accessible to both technical and non-technical audiences.

The name "C4" comes from the four hierarchical levels that form the core of the model:

1. **Context** - How the system fits into the world
2. **Container** - The high-level technical building blocks
3. **Component** - The internal structure of containers
4. **Code** - Implementation details and class structures

Unlike heavyweight modeling approaches that can overwhelm developers, the C4 model focuses on simplicity and practicality while maintaining enough rigor to be useful for architecture documentation.

## History and Origins

Simon Brown created the C4 model between 2006 and 2011 based on his experiences teaching software architecture. The diagram types were formally named in 2010, and the "C4" name was adopted in 2011. The model evolved from UML (Unified Modeling Language) and the 4+1 architectural view model, but deliberately simplifies these concepts to make architecture more accessible to modern development teams.

## Purpose and Benefits

### Why C4 Model Exists

The C4 model addresses several common challenges in software architecture documentation:

1. **Inconsistent Abstractions**: Teams often mix different levels of detail in single diagrams, creating confusion
2. **Over-complex Notations**: Formal modeling languages like UML can be intimidating and incompatible with agile workflows
3. **Missing Documentation**: Many teams skip architecture documentation entirely due to perceived complexity
4. **Poor Communication**: Architecture diagrams fail to effectively communicate with diverse audiences

### Key Benefits

**Developer-Friendly Approach:**

- Simple enough for any developer to understand and create
- No extensive training required unlike UML or ArchiMate
- Fits naturally into agile and continuous delivery workflows
- Focuses on practical communication over formal correctness

**Hierarchical Abstraction:**

- Different levels of zoom for different audiences
- Executive stakeholders see context diagrams
- Developers and architects see component and code diagrams
- Each level provides appropriate detail without overwhelming

**Notation Independence:**

- Not tied to specific diagramming tools or syntax
- Works with any diagramming tool (draw.io, Mermaid, PlantUML, etc.)
- Emphasizes clarity over strict notation rules
- Teams can adapt visual conventions to their needs

**Progressive Disclosure:**

- Start with high-level context and drill down as needed
- You don't need all four levels - use what adds value
- Most teams find context and container diagrams sufficient
- Component and code diagrams reserved for complex areas

## Software Engineering Principles

The C4 model naturally aligns with core software engineering principles:

1. **[Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** - C4 enforces explicit labels on all elements and relationships, clear system boundaries, visible technology choices, and named communication protocols. Every diagram element must explicitly state what it is and how it interacts with others, eliminating hidden assumptions that plague architecture documentation.

2. **[Simplicity Over Complexity](../../../../../governance/principles/general/simplicity-over-complexity.md)** - C4 deliberately uses simple boxes and lines instead of complex UML notation, reducing cognitive load and making diagrams accessible to all stakeholders. The four-level hierarchy provides progressive disclosure—you only add complexity where it adds value, avoiding over-engineered documentation.

3. **[Automation Over Manual](../../../../../governance/principles/software-engineering/automation-over-manual.md)** - C4 supports diagram-as-code approaches with Structurizr DSL, PlantUML, and Mermaid, enabling version-controlled architecture documentation that integrates with CI/CD pipelines. Automated diagram generation from code keeps documentation synchronized with implementation.

4. **[Reproducibility First](../../../../../governance/principles/software-engineering/reproducibility.md)** - C4 provides standardized notation and consistent conventions across all diagram types, enabling teams to produce reproducible documentation. The same abstractions and notation rules apply regardless of tool or team member, ensuring consistent communication.

5. **[Immutability Over Mutability](../../../../../governance/principles/software-engineering/immutability.md)** - C4 can effectively model immutable architecture patterns like functional core/imperative shell, event sourcing, and CQRS. The model naturally represents data flow in systems that favor immutable data structures.

See [Best Practices](./ex-so-ar-c4armo__14-best-practices.md) for detailed examples of how these principles apply to C4 diagrams.

## Documentation Structure

This C4 model documentation is organized into multiple interconnected guides:

### Core Levels

1. **[Level 1: System Context](./ex-so-ar-c4armo__01-level-1-system-context.md)** - Shows how the software system fits into the world
2. **[Level 2: Container](./ex-so-ar-c4armo__02-level-2-container.md)** - Shows the high-level technical building blocks
3. **[Level 3: Component](./ex-so-ar-c4armo__03-level-3-component.md)** - Shows internal structure using object-oriented approaches
4. **[Level 3: Component (Functional Programming)](./ex-so-ar-c4armo__04-level-3-component-fp.md)** - Shows internal structure using functional programming approaches
5. **[Level 4: Code](./ex-so-ar-c4armo__05-level-4-code.md)** - Provides implementation details for object-oriented code
6. **[Level 4: Code (Functional Programming)](./ex-so-ar-c4armo__06-level-4-code-fp.md)** - Provides implementation details for functional code

### Additional Perspectives

1. **[Supplementary Diagrams](./ex-so-ar-c4armo__07-supplementary-diagrams.md)** - System Landscape, Dynamic, and Deployment diagrams
2. **[Notation and Conventions](./ex-so-ar-c4armo__08-notation-and-conventions.md)** - Labeling, color coding, and notation standards
3. **[Best Practices](./ex-so-ar-c4armo__09-best-practices.md)** - When to use C4, comparisons, common mistakes, and limitations

### Cross-Cutting Concerns

1. **[Paradigm Considerations](./ex-so-ar-c4armo__10-paradigm-considerations.md)** - How C4 applies to OOP vs FP and other paradigms
2. **[Frequently Asked Questions](./ex-so-ar-c4armo__11-faq.md)** - Common questions and answers about using C4
3. **[Simple Application Examples](./ex-so-ar-c4armo__12-simple-app-examples.md)** - When diagrams add value vs. overkill for straightforward systems
4. **[Event-Driven Architectures](./ex-so-ar-c4armo__13-event-driven-architectures.md)** - Applying C4 to event sourcing, CQRS, saga patterns, and event streaming

## Quick Start Guide

Choose your path based on available time and learning goals:

### 5-Minute Quick Start (Complete Beginner)

**Goal**: Understand if C4 is relevant for your project.

**Path**:

1. Read **[README Overview](#overview)** - Understand the four levels
2. Skim **[Best Practices: When to Use C4](./ex-so-ar-c4armo__09-best-practices.md#when-to-use-the-c4-model)** - Identify if C4 fits your system
3. Review **[Simple App Examples: Decision Matrix](./ex-so-ar-c4armo__12-simple-app-examples.md#decision-matrix-when-to-create-diagrams)** - Check which diagrams you need

**Outcome**: Know whether C4 is appropriate for your system and which diagrams to create.

**Next Steps**:

- If C4 is relevant → Follow 30-Minute Learning Path
- If your system is too simple → Consider [Simple App Examples](./ex-so-ar-c4armo__12-simple-app-examples.md#when-not-to-create-diagrams)
- If you need deeper understanding → Jump to 2-Hour Deep Dive

### 30-Minute Learning Path (Practical Introduction)

**Goal**: Create your first two C4 diagrams.

**Path**:

1. **Understand the Basics**:
   - Read **[Level 1: System Context](./ex-so-ar-c4armo__01-level-1-system-context.md)** - Big picture view
   - Read **[Level 2: Container](./ex-so-ar-c4armo__02-level-2-container.md)** - Technical building blocks
2. **Create Diagrams**:
   - Sketch a **System Context diagram** for your system:
     - One box for your system
     - Boxes for users (people)
     - Boxes for external systems
     - Labeled relationships
   - Sketch a **Container diagram**:
     - Boxes for deployable units (apps, databases, services)
     - Technology labels (e.g., "Container: Spring Boot")
     - Communication protocols (HTTP/REST, SQL, etc.)
3. **Review Conventions**:
   - Skim **[Notation and Conventions](./ex-so-ar-c4armo__08-notation-and-conventions.md)** - Standard practices

**Outcome**: Rough Context and Container diagrams that communicate your architecture.

**Next Steps**:

- Refine diagrams based on team feedback
- For complex containers → Read Level 3 (Component)
- For comprehensive understanding → Follow 2-Hour Deep Dive

### 2-Hour Deep Dive (Comprehensive Understanding)

**Goal**: Master C4 model and create production-ready diagrams.

**Path**:

**Part 1: Core Concepts and Practices**

1. **Foundation**:
   - Read **[Level 1: System Context](./ex-so-ar-c4armo__01-level-1-system-context.md)** - System boundaries
   - Read **[Level 2: Container](./ex-so-ar-c4armo__02-level-2-container.md)** - Deployment units
   - Read **[Level 3: Component](./ex-so-ar-c4armo__03-level-3-component.md)** - Internal structure
2. **Best Practices**:
   - Read **[Best Practices: When to Use C4](./ex-so-ar-c4armo__09-best-practices.md#when-to-use-the-c4-model)** - Ideal use cases
   - Read **[Best Practices: When C4 May Not Fit](./ex-so-ar-c4armo__09-best-practices.md#when-c4-may-not-fit)** - Limitations
   - Review **[Simple App Examples](./ex-so-ar-c4armo__12-simple-app-examples.md)** - Real-world scenarios
3. **Supplementary Diagrams**:
   - Read **[Supplementary Diagrams](./ex-so-ar-c4armo__07-supplementary-diagrams.md)** - Dynamic, Deployment, Landscape

**Part 2: Implementation and Standards**

1. **Notation Standards**:
   - Read **[Notation and Conventions](./ex-so-ar-c4armo__08-notation-and-conventions.md)** - Labeling, colors, shapes
   - Review **[Repository Diagram Convention](../../../../../governance/conventions/formatting/diagrams.md)** - Accessibility requirements
2. **Create Production Diagrams**:
   - **Context Diagram**: Include all external systems and users
   - **Container Diagram**: Show all deployable units with technology stacks
   - **Component Diagram**: Detail one complex container
3. **Review and Refine**:
   - Check against **[Common Mistakes](./ex-so-ar-c4armo__09-best-practices.md#common-mistakes-to-avoid)**
   - Validate accessibility (WCAG-compliant colors)
   - Get team feedback

**Outcome**: Production-ready C4 diagrams following repository standards.

**Next Steps**:

- Integrate diagrams into architecture documentation
- Schedule regular diagram reviews
- Read paradigm-specific guides if applicable

### Path for Specific Use Cases

Choose documentation based on your system characteristics:

#### By Architecture Style

- **Object-Oriented Systems** (Java, C#, Python classes):
  - Follow: [Level 1](./ex-so-ar-c4armo__01-level-1-system-context.md) → [Level 2](./ex-so-ar-c4armo__02-level-2-container.md) → [Level 3 OOP](./ex-so-ar-c4armo__03-level-3-component.md) → [Level 4 OOP](./ex-so-ar-c4armo__05-level-4-code.md)
  - See: [Paradigm Considerations: OOP](./ex-so-ar-c4armo__10-paradigm-considerations.md)
- **Functional Programming Systems** (Elixir, Haskell, Scala):
  - Follow: [Level 1](./ex-so-ar-c4armo__01-level-1-system-context.md) → [Level 2](./ex-so-ar-c4armo__02-level-2-container.md) → [Level 3 FP](./ex-so-ar-c4armo__04-level-3-component-fp.md) → [Level 4 FP](./ex-so-ar-c4armo__06-level-4-code-fp.md)
  - See: [Paradigm Considerations: FP](./ex-so-ar-c4armo__10-paradigm-considerations.md)
- **Event-Driven / CQRS Systems**:
  - Start with: [Supplementary Diagrams: Dynamic](./ex-so-ar-c4armo__07-supplementary-diagrams.md#dynamic-diagram)
  - Key focus: Message flows, event sourcing, saga patterns
- **Microservices Architectures**:
  - Start with: [Level 2 Container](./ex-so-ar-c4armo__02-level-2-container.md) - Service boundaries critical
  - See: [Simple App Examples: Small Microservices](./ex-so-ar-c4armo__12-simple-app-examples.md#example-3-small-microservices-when-container-diagram-adds-value)

#### By System Complexity

- **Simple Systems** (1-5 deployable units, small team):
  - Read: [Simple App Examples](./ex-so-ar-c4armo__12-simple-app-examples.md) - Understand when diagrams add value
  - Create: Context + Container only (skip Component and Code)
  - Consider: README might be sufficient
- **Medium Systems** (6-15 deployable units, medium team):
  - Create: Context, Container, selective Component diagrams
  - Focus: Integration points and complex containers
- **Large Systems** (16+ deployable units, large team):
  - Create: System Landscape, multiple Context diagrams (per domain)
  - Focus: Bounded context boundaries, integration patterns
  - See: [Best Practices: Scaling Considerations](./ex-so-ar-c4armo__09-best-practices.md#when-to-use-the-c4-model)

#### By Documentation Need

- **Enterprise Architecture Overview**:
  - Start: [Supplementary Diagrams: System Landscape](./ex-so-ar-c4armo__07-supplementary-diagrams.md#system-landscape-diagram)
  - Show: Multiple systems across organization
- **Infrastructure Planning**:
  - Focus: [Supplementary Diagrams: Deployment](./ex-so-ar-c4armo__07-supplementary-diagrams.md#deployment-diagram)
  - Show: Containers mapped to infrastructure, scaling strategies
- **Workflow Documentation**:
  - Use: [Supplementary Diagrams: Dynamic](./ex-so-ar-c4armo__07-supplementary-diagrams.md#dynamic-diagram)
  - Show: Time-ordered interactions, error handling paths
- **Developer Onboarding**:
  - Create: Context (system overview), Container (technical stack), Component (complex areas)
  - Focus: Clear labels, technology indicators, communication patterns

#### By Tool and Notation

- **Mermaid Users**:
  - See: [Best Practices: Standard Mermaid vs. Experimental C4 Syntax](./ex-so-ar-c4armo__09-best-practices.md#standard-mermaid-vs-experimental-c4-syntax)
  - Use: `graph TB` syntax with WCAG color palette
  - Check: [Repository Diagram Convention](../../../../../governance/conventions/formatting/diagrams.md)
- **Structurizr Users**:
  - See: [Best Practices: Tooling Philosophy](./ex-so-ar-c4armo__09-best-practices.md#tooling-philosophy-modeling-vs-diagramming)
  - Benefit: Model-first approach, automatic layout
- **draw.io / Lucidchart Users**:
  - See: [Notation and Conventions](./ex-so-ar-c4armo__08-notation-and-conventions.md)
  - Use: C4 stencils or templates for consistency

## Related Documentation

**Software Engineering Principles**:

- **[Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** - C4's explicit labeling and visible relationships eliminate architecture ambiguity
- **[Simplicity Over Complexity](../../../../../governance/principles/general/simplicity-over-complexity.md)** - Simple boxes and lines instead of complex UML notation
- **[Automation Over Manual](../../../../../governance/principles/software-engineering/automation-over-manual.md)** - Diagram-as-code with Structurizr DSL, PlantUML, and Mermaid
- **[Reproducibility First](../../../../../governance/principles/software-engineering/reproducibility.md)** - Standardized notation ensures consistent documentation
- **[Software Engineering Principles](../../../../../governance/principles/software-engineering/README.md)** - Comprehensive documentation of all principles

**Repository Context**:

- **Repository Context**: [C4 Model in This Repository](./ex-so-ar-c4armo__09-best-practices.md#c4-model-in-this-repository)
- **Further Learning**: [External Resources](./ex-so-ar-c4armo__11-faq.md#further-learning)
- **Conventions**: [Diagram and Schema Convention](../../../../../governance/conventions/formatting/diagrams.md)
