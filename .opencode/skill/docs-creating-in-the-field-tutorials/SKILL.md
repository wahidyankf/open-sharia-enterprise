---
name: docs-creating-in-the-field-tutorials
description: Comprehensive guide for creating in-the-field production implementation guides - production-ready code with 20-40 guides following standard library first principle, framework integration, and enterprise patterns. Essential for creating production tutorials for programming languages on educational platforms
---

# In-the-Field Tutorial Creation Skill

## Purpose

This Skill provides comprehensive guidance for creating **in-the-field tutorials** - production implementation guides designed for developers with foundational knowledge ready to apply concepts in production environments using industry-standard frameworks and enterprise patterns.

**When to use this Skill:**

- Creating in-the-field tutorials for programming languages
- Writing production-ready code with framework integration
- Designing standard library→framework progression guides
- Teaching enterprise patterns and production practices
- Targeting experienced developers with by-example/by-concept foundation

## Core Concepts

### What is In-the-Field?

**In-the-field tutorials** are production implementation guides that achieve production readiness through 20-40 guides covering real-world scenarios with standard library→framework progression.

**NOT a replacement for**:

- By-example (which provides 95% language coverage through code-first examples)
- By-concept (which provides narrative explanations of fundamentals)
- Quick Start (which is 5-30% coverage touchpoints)

**Target Audience**:

- **Developers with foundation**: Completed by-example and/or by-concept
- **Ready for production**: Need to apply concepts in real systems
- **Framework selection**: Want informed decisions about tools
- **Enterprise patterns**: Need industry-standard practices

### Standard Library First Principle

**CRITICAL**: In-the-field tutorials MUST teach standard library/built-in approaches first, THEN introduce production frameworks with clear rationale.

**Progression pattern**:

1. **Show standard library approach** - Demonstrate built-in capabilities with full code
2. **Identify limitations** - Explain why standard approach insufficient for production
3. **Introduce framework** - Show how framework addresses limitations
4. **Compare trade-offs** - Discuss complexity, learning curve, maintenance

**Example progression** (Testing):

```markdown
## Testing in Production

### Standard Library: assert Keyword

Java provides `assert` keyword for runtime assertions...

[Code example with annotations]

**Limitations for production**:

- No test organization (all tests in main method)
- No reporting (just exceptions or silence)
- Manual execution (no test runner)

### Production Framework: JUnit 5

JUnit 5 provides test organization, reporting, automation...

[Code example with annotations]

**Trade-offs**:

- External dependency (2MB) vs organized tests
- Learning curve vs powerful features
- Justification: Worth it for production systems

### When to Use Each:

- assert: Simple scripts, internal tools
- JUnit: Production code, CI/CD, team projects
```

## Guide Structure

Every in-the-field guide follows this structure:

### Part 1: Why It Matters (2-3 paragraphs)

Establish production relevance and motivation.

### Part 2: Standard Library First (MANDATORY)

- Complete, runnable standard library example
- Annotation density: 1.0-2.25 per code line
- Explanation of how standard approach works
- **Limitations section**: Why insufficient for production

### Part 3: Production Framework Introduction

- Installation/setup steps (Maven/Gradle dependency)
- Production-grade code with error handling
- Configuration and best practices
- Integration testing example
- Comparison with standard library approach

### Part 4: Mermaid Diagram (when appropriate)

- Architecture patterns
- Data flow diagrams
- **Progression diagrams**: Standard library → Framework → Production
- Use accessible color palette

### Part 5: Production Patterns and Best Practices

- Design patterns specific to topic
- Error handling strategies
- Security considerations
- Performance implications
- Common pitfalls to avoid

### Part 6: Trade-offs and When to Use

- Complexity vs capability trade-off
- Learning curve considerations
- Maintenance implications
- Performance impact
- When simpler approaches suffice

## Annotation Density Standards

### The 1.0-2.25 Rule

**Same as by-example**: Target 1.0-2.25 comment lines per code line PER CODE BLOCK

**Measurement**: Each code block is measured independently

**Annotations focus on**:

- Framework behavior (what framework does)
- Configuration impact (how settings affect behavior)
- Integration points (where components connect)
- Security implications (why this approach is secure)
- Performance characteristics (resource usage, bottlenecks)

**Example** (JUnit code):

```java
@Test
void transfer_shouldMoveMoneyBetweenAccounts() {
    // => @Test marks method for JUnit discovery
    // => Test runner executes this method
    // => Package-private visibility sufficient

    Account source = new Account("A", Money.of(100));
    // => source starts with 100 units
    // => Creates source account for test

    Account target = new Account("B", Money.of(50));
    // => target starts with 50 units
    // => Creates target account for test

    transferService.transfer(source, target, Money.of(30));
    // => Transfers 30 from source to target
    // => Invokes method under test

    assertEquals(Money.of(70), source.balance());
    // => Verifies source reduced by 30
    // => assertEquals throws AssertionFailedError if false

    assertEquals(Money.of(80), target.balance());
    // => Verifies target increased by 30
    // => Test passes if both assertions succeed
}
```

**Density**: 6 code lines, 12 annotation lines = 2.0 density (within 1.0-2.25 target)

## Production Code Quality Standards

**CRITICAL**: In-the-field code is production-ready, not educational simplifications.

### Code Completeness Requirements

- **Error handling**: try-with-resources, proper exceptions
- **Resource management**: Always close connections, streams
- **Logging**: Production logging at appropriate levels (SLF4J)
- **Security**: Input validation, secret management, secure defaults
- **Configuration**: Externalized configuration, no hardcoded values
- **Testing**: Integration tests demonstrating framework usage

## Guide Count: 20-40 Production Guides

**Target range**: 20-40 guides per language or framework

**Rationale**:

- 20-40 guides covers major production patterns without overwhelming
- Each guide addresses a specific production scenario with depth
- Fewer guides than by-example (20-40 vs 75-85) because guides cover broader topics
- Range allows flexibility based on ecosystem maturity

**Topic categories**:

- Foundation: Build tools, linting, logging (3-5 guides)
- Quality: TDD, BDD, static analysis (2-4 guides)
- Core Concepts: Design principles, patterns (3-5 guides)
- Security: Authentication, authorization (2-4 guides)
- Data: SQL, NoSQL, caching (3-5 guides)
- Integration: APIs, messaging (3-5 guides)
- Advanced: Reactive, concurrency (3-5 guides)
- Deployment: Docker, Kubernetes, CI/CD (2-4 guides)

## Diagram Standards

### Diagram Frequency Target

**Guideline**: 10-20 diagrams total (25-50% of 20-40 guides)

**When to include diagrams**:

- Architecture patterns (microservices, event-driven, layered)
- Deployment topologies (Docker, Kubernetes, cloud)
- Data flow across systems (API → service → database)
- State machines (TDD Red-Green-Refactor, workflow states)
- **Progression patterns**: Standard library → Framework → Production
- Security flows (authentication, authorization, token validation)
- CI/CD pipeline flows

**Accessible color palette** (MANDATORY):

- Blue: #0173B2
- Orange: #DE8F05
- Teal: #029E73
- Purple: #CC78BC
- Brown: #CA9161

## Common Mistakes

### ❌ Mistake 1: Framework without standard library first

**Wrong**: Jump directly to Spring Boot without showing HttpClient first

**Right**: Show java.net.http.HttpClient, explain limitations, then introduce Spring Boot

### ❌ Mistake 2: Simplified tutorial code instead of production code

**Wrong**: Omit error handling to keep example simple

**Right**: Include full try-with-resources, proper exception handling, logging

### ❌ Mistake 3: Generic framework justifications

**Wrong**: "JUnit is industry standard, everyone uses it"

**Right**: "JUnit provides test organization (no main method), reporting (pass/fail), automation (Maven integration)"

### ❌ Mistake 4: Missing trade-off discussion

**Wrong**: Only show framework approach

**Right**: Compare standard library vs framework with when to use each

## References

**Primary Convention**: [In-the-Field Tutorial Convention](../../../governance/conventions/tutorials/in-the-field.md)

**Related Conventions**:

- [Tutorial Naming Convention](../../../governance/conventions/tutorials/naming.md) - In-the-field type definition
- [Hugo ayokoding Convention](../../../governance/conventions/hugo/ayokoding.md) - Code annotation standards

**Related Skills**:

- `apps-ayokoding-web-developing-content` - ayokoding-web specific patterns
- `docs-creating-accessible-diagrams` - Accessible diagram creation

---

This Skill packages critical in-the-field tutorial creation knowledge for production implementation guides. For comprehensive details, consult the primary convention document.
