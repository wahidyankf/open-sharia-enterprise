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
  - software-design
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

## Documentation Structure

This C4 model documentation is organized into multiple interconnected guides:

### Core Levels

1. **[Level 1: System Context](./ex-c4armo__level-1-system-context.md)** - Shows how the software system fits into the world
2. **[Level 2: Container](./ex-c4armo__level-2-container.md)** - Shows the high-level technical building blocks
3. **[Level 3: Component](./ex-c4armo__level-3-component.md)** - Shows internal structure using object-oriented approaches
4. **[Level 3: Component (Functional Programming)](./ex-c4armo__level-3-component-fp.md)** - Shows internal structure using functional programming approaches
5. **[Level 4: Code](./ex-c4armo__level-4-code.md)** - Provides implementation details for object-oriented code
6. **[Level 4: Code (Functional Programming)](./ex-c4armo__level-4-code-fp.md)** - Provides implementation details for functional code

### Additional Perspectives

1. **[Supplementary Diagrams](./ex-c4armo__supplementary-diagrams.md)** - System Landscape, Dynamic, and Deployment diagrams
2. **[Notation and Conventions](./ex-c4armo__notation-and-conventions.md)** - Labeling, color coding, and notation standards
3. **[Best Practices](./ex-c4armo__best-practices.md)** - When to use C4, comparisons, common mistakes, and limitations

### Cross-Cutting Concerns

1. **[Paradigm Considerations](./ex-c4armo__paradigm-considerations.md)** - How C4 applies to OOP vs FP and other paradigms
2. **[Frequently Asked Questions](./ex-c4armo__faq.md)** - Common questions and answers about using C4

## Quick Start Guide

**For newcomers to C4**:

1. Start with [Level 1: System Context](./ex-c4armo__level-1-system-context.md) - understand the big picture
2. Move to [Level 2: Container](./ex-c4armo__level-2-container.md) - understand technical building blocks
3. Read [Best Practices](./ex-c4armo__best-practices.md) - learn when to create each diagram type

**For specific use cases**:

- **Object-Oriented systems**: Follow Levels 1 → 2 → 3 → 4 (OOP versions)
- **Functional Programming systems**: Follow Levels 1 → 2 → 3 FP → 4 FP
- **Enterprise architecture**: Start with [Supplementary Diagrams](./ex-c4armo__supplementary-diagrams.md#system-landscape-diagram)
- **Infrastructure planning**: See [Supplementary Diagrams](./ex-c4armo__supplementary-diagrams.md#deployment-diagram)

## Related Documentation

- **Repository Context**: [C4 Model in This Repository](./ex-c4armo__best-practices.md#c4-model-in-this-repository)
- **Further Learning**: [External Resources](./ex-c4armo__faq.md#further-learning)
- **Conventions**: [Diagram and Schema Convention](../../../governance/conventions/formatting/diagrams.md)
