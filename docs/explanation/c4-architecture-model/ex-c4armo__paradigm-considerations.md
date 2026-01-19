# Paradigm Considerations

The C4 model is paradigm-agnostic by design. The hierarchical zoom levels (Context → Container → Component → Code) apply equally to object-oriented, functional, procedural, and other programming paradigms. The paradigm affects HOW you implement components and code, not WHAT the architectural structure looks like.

### Object-Oriented Programming (OOP)

In object-oriented systems, C4 components manifest as:

- **Classes and objects**: Stateful entities with encapsulated behavior
- **Inheritance hierarchies**: Abstract classes, interfaces, polymorphism
- **Design patterns**: Dependency injection, factories, strategy, observer
- **State management**: Instance variables, mutable object state
- **Error handling**: Exceptions, try/catch blocks
- **Modularity**: Packages, namespaces, modules containing related classes

**Example Technologies**: Java/Spring Boot, C#/.NET, Python/Django, Ruby/Rails, TypeScript/NestJS

### Functional Programming (FP)

In functional systems, C4 components manifest as:

- **Modules with pure functions**: Stateless functions that transform data
- **Data structures**: Immutable structs, algebraic data types, pattern matching
- **Function composition**: Pipelines, higher-order functions, monads
- **State management**: Isolated processes (Actor model), persistent data structures
- **Error handling**: Result types (`Either`, `Option`), railway-oriented programming
- **Modularity**: Modules, contexts, namespaces containing related functions

**Example Technologies**: Elixir/Phoenix, Haskell, OCaml, Clojure, Scala/FP, F#

### C4 Model Across Paradigms - Order Service Example

The following table shows how the same Order Service architecture maps to C4 levels in both paradigms:

| C4 Level           | What It Describes                          | OOP Implementation (Spring Boot) | FP Implementation (Elixir/Phoenix) | Paradigm Impact |
| ------------------ | ------------------------------------------ | -------------------------------- | ---------------------------------- | --------------- |
| **System Context** | Order Service and external actors/systems  | Same - external boundaries       | Same - external boundaries         | **None**        |
| **Container**      | Order Service API, Database, Message Queue | Same - deployment units          | Same - deployment units            | **None**        |
| **Component**      | Internal modules/layers of Order Service   | Classes, Services, Repositories  | Contexts, Routers, GenServers      | **High**        |
| **Code**           | Implementation details within components   | UML Class Diagrams, JPA Entities | Function Signatures, Ecto Schemas  | **High**        |

**Key Insight**: The paradigm only affects **Component** and **Code** levels. System Context and Container diagrams look identical regardless of whether you use OOP, FP, or any other paradigm. The architectural boundaries (what talks to what) remain the same; only the internal organization changes.

### Applying C4 Model to Your Paradigm

When creating C4 diagrams for your system:

**Levels 1-2 (Context, Container)** - Paradigm-independent:

- Focus on **what exists** and **where it runs**
- These levels are the same across all paradigms
- Example: "Order Service API" is a container whether implemented in Java or Elixir

**Level 3 (Component)** - Paradigm-aware:

- **OOP**: Show services, repositories, controllers as components
- **FP**: Show contexts, routers, GenServers as components
- **Mixed**: Some teams use OOP for infrastructure, FP for business logic - show both

**Level 4 (Code)** - Paradigm-specific:

- **OOP**: Use UML class diagrams, show inheritance, show mutable state
- **FP**: Use function signature diagrams, show data transformations, show pure/impure boundaries
- **Either**: Use notation that matches your implementation language

### When to Use OOP vs FP (Balanced Perspective)

The C4 model doesn't prescribe paradigms. Choose based on:

**Favor OOP when**:

- Team expertise is primarily object-oriented
- Domain complexity benefits from encapsulation (complex state machines, rich domain models)
- Ecosystem integration requires OOP frameworks (enterprise Java, .NET)
- Gradual state mutation is natural fit for domain (simulations, games)

**Favor FP when**:

- Concurrency and parallelism are critical (FP's immutability eliminates race conditions)
- Business logic complexity benefits from pure functions (easier testing, reasoning)
- Data transformations dominate (ETL pipelines, stream processing)
- Fault tolerance is paramount (OTP supervision trees, self-healing systems)

**Favor Hybrid when**:

- Different subsystems have different needs (OOP for UI, FP for business logic)
- Transitioning between paradigms (gradual adoption)
- Language supports both well (Scala, TypeScript, Python, Kotlin)

**Reality**: Most successful systems use **pragmatic combinations** rather than paradigm purity. The C4 model helps you document whatever you build, regardless of paradigm choices.

### Cross-References to Examples in This Documentation

**OOP Examples**:

- Component Diagram: See "Example: Order Service Components" in [Level 3: Component Diagram](./ex-c4armo__level-3-component.md#example-order-service-components-java-spring-boot-microservice)
- Code Diagram: See "Example 1" and "Example 2" in [Level 4: Code Diagram](./ex-c4armo__level-4-code.md)

**FP Examples**:

- Component Diagram: See "Functional Programming Approach" in [Level 3: Component (FP)](./ex-c4armo__level-3-component-fp.md)
- Code Diagram: See "Functional Programming Approach" in [Level 4: Code (FP)](./ex-c4armo__level-4-code-fp.md)

**Paradigm-Agnostic Examples**:

- System Context: See examples in [Level 1: System Context](./ex-c4armo__level-1-system-context.md)
- Container: See examples in [Level 2: Container](./ex-c4armo__level-2-container.md)
