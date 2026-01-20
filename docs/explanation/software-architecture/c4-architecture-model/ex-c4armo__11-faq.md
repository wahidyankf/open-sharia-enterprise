# Frequently Asked Questions

### General C4 Model Questions

**Q: Do different team members create different C4 diagram levels?**

**A**: No - this is a common misconception. The C4 model is NOT a delegation framework where architects create context diagrams, team leads create container diagrams, and developers create component diagrams.

**Reality**:

- **Any team member** can create any C4 diagram level
- **Collaboration is encouraged**: Architects and developers work together on all levels
- **Diagram ownership** follows code ownership: teams owning a system typically own its diagrams
- **Level choice** depends on need, not role: create context diagrams for stakeholder presentations, component diagrams for complex areas

The hierarchical levels are for **abstraction and zoom**, not organizational hierarchy.

**Q: How many C4 diagrams should I create?**

**A**: Minimum viable documentation approach:

**For small systems** (single application):

- 1 System Context diagram (mandatory)
- 1 Container diagram (highly recommended)
- 0-2 Component diagrams (only for complex containers)
- 0-1 Code diagrams (rarely needed)
- 0-1 Deployment diagrams (if infrastructure matters)

**For medium systems** (microservices, 5-10 services):

- 1 System Context diagram
- 1 Container diagram
- 2-4 Component diagrams (for critical services)
- 1-2 Dynamic diagrams (for key workflows)
- 1 Deployment diagram (production)

**For large systems** (enterprise, 20+ services):

- 1 System Landscape diagram
- 3-5 System Context diagrams (per bounded context)
- 3-5 Container diagrams (per bounded context)
- 5-10 Component diagrams (selective, for complex services)
- 3-5 Dynamic diagrams (authentication, critical workflows)
- 2-3 Deployment diagrams (dev, staging, prod)

**Golden rule**: Only create diagrams that help someone understand the system. More diagrams ≠ better documentation.

**Q: When should I split a large diagram into multiple smaller diagrams?**

**A**: Split when a diagram becomes too large to be useful:

**Size thresholds**:

- **Context diagram**: 15+ external systems → create system landscape or split by integration domain
- **Container diagram**: 30+ containers → split by bounded context or business domain
- **Component diagram**: 20+ components → this container is too complex, consider refactoring or splitting into multiple focused diagrams

**Cognitive Load Guideline**:

The C4 FAQ uses "600 components rather than 6" as a hypothetical example of complexity. While there's no hard numerical threshold, large diagrams create high cognitive load and become hard to interpret. The official guidance: split complex diagrams when they become cluttered, regardless of exact element count.

**Readability test**:

- Can someone quickly understand this diagram?
- Can you print this diagram on A4/Letter paper and read it?
- Can you present this diagram in a meeting without zooming?

If answer is "no" to any question, split the diagram.

**Q: Should I use C4 to document libraries, frameworks, or SDKs?**

**A**: Generally no - C4 is for software **systems** that run/deploy, not reusable libraries.

**Don't use C4 for**:

- npm packages (React components, utility libraries)
- Python packages (data science libraries, SDKs)
- Java libraries (Apache Commons, Guava)
- Mobile SDKs (analytics SDKs, payment SDKs)

**Why**: Libraries don't have "containers" (they're not deployed) or "system context" (they're consumed, not standalone).

**Use instead**:

- API documentation (Javadoc, JSDoc, Sphinx)
- Architecture Decision Records (ADRs)
- Package dependency diagrams
- Design pattern documentation

**Exception**: If your library includes deployable components:

- CLI tools (can be documented with C4)
- Backend services that SDK calls (C4 applies to backend)
- Example applications that use the library (C4 for example apps)

**Q: Can I use C4 model with arc42 architecture documentation framework?**

**A**: Yes - they complement each other perfectly.

**Integration mapping**:

| arc42 Section                                | C4 Diagram Type    | Notes                        |
| -------------------------------------------- | ------------------ | ---------------------------- |
| **Section 3: Context and Scope**             | System Context     | arc42 Section 3 = C4 Level 1 |
| **Section 5: Building Block View (Level 1)** | Container Diagram  | arc42 Level 1 = C4 Level 2   |
| **Section 5: Building Block View (Level 2)** | Component Diagram  | arc42 Level 2 = C4 Level 3   |
| **Section 5: Building Block View (Level 3)** | Code Diagram       | arc42 Level 3 = C4 Level 4   |
| **Section 6: Runtime View**                  | Dynamic Diagram    | C4 supplementary diagrams    |
| **Section 7: Deployment View**               | Deployment Diagram | C4 supplementary diagrams    |

**Benefits of combining**:

- arc42 provides comprehensive template (decisions, quality requirements, risks)
- C4 provides clear visualization approach
- C4 diagrams slot directly into arc42 sections
- arc42 adds context C4 doesn't cover (ADRs, quality attributes, glossary)

**Q: How do I keep C4 diagrams up to date?**

**A**: Make diagram updates part of your development workflow.

**Strategies**:

1. **Definition of Done**: Include "update architecture diagrams" in DoD for features that change architecture
2. **Code Review Checklist**: Reviewers check if Container/Component diagrams need updates
3. **Quarterly Reviews**: Schedule architecture review sessions every 3 months to audit diagrams
4. **Diagram-as-Code**: Use Structurizr DSL or PlantUML to version control diagrams alongside code
5. **Automated Generation**: Generate Container diagrams from code annotations (limited tooling available)
6. **Simplify Diagrams**: Fewer diagrams = less maintenance burden

**Warning signs diagrams are stale**:

- Diagram shows containers/services that no longer exist
- New team members say "this doesn't match reality"
- Deployment diagram shows old infrastructure (pre-Kubernetes migration)
- Technology labels reference deprecated frameworks

**Maintenance schedule**:

- **High-level diagrams** (Context, Container): Review quarterly or when major changes occur
- **Detailed diagrams** (Component, Code): Review as needed, may become stale quickly
- **Dynamic diagrams**: Review when workflows change significantly
- **Deployment diagrams**: Review when infrastructure changes (migrations, scaling events)

**Q: What's the difference between C4 "containers" and Docker containers?**

**A**: They're different concepts that sometimes overlap.

**C4 Container** (architectural abstraction):

- **Definition**: Separately deployable/executable unit
- **Examples**: Web application, mobile app, database, serverless function, CLI tool
- **Not limited to Docker**: Can be VM, physical server, desktop application, mobile app

**Docker Container** (implementation technology):

- **Definition**: Lightweight OS-level virtualization using Docker
- **Examples**: Docker image running on Docker Engine
- **Specific technology**: One of many ways to deploy software

**Relationship**:

- **One-to-one**: Often a C4 container is deployed as a Docker container (microservice in Docker)
- **One-to-many**: A C4 container (mobile app) has no Docker containers
- **Many-to-one**: Multiple C4 components might share one Docker container (monolith)

**Example**:

System with 4 C4 containers:

1. **React Web App** (C4 container) → runs as Docker container in Kubernetes
2. **Spring Boot API** (C4 container) → runs as Docker container in Kubernetes
3. **PostgreSQL Database** (C4 container) → runs as Docker container in Kubernetes
4. **iOS Mobile App** (C4 container) → NO Docker container (deployed to App Store)

Result: 3 Docker containers, 4 C4 containers.

**Q: How detailed should element descriptions be?**

**A**: Keep descriptions to one line focusing on responsibilities, not implementation.

**Too brief** (bad):

- "API" - what API? What does it do?
- "Database" - what data? What schema?

**Too detailed** (bad):

- "Spring Boot microservice implemented in Java 17 using Spring Data JPA for PostgreSQL persistence with connection pooling configured via HikariCP and OAuth2 security via Spring Security module"

**Just right** (good):

- "Order Service - Handles order creation, payment processing, and fulfillment coordination"
- "Order Database - Stores order history, line items, and payment transactions"

**Rule**: If you can't quickly scan the description, it's too long.

**Q: Should I include external libraries in Component diagrams?**

**A**: Generally no - Component diagrams show **your code's** internal structure, not third-party libraries.

**Don't show**:

- Spring Framework, Express.js, React (these are the technology choice, not components)
- Jackson, Lodash, Axios (utility libraries used everywhere)
- Log4j, Winston (cross-cutting infrastructure)

**Do show**:

- Your `OrderController`, `PaymentService`, `UserRepository` components
- Your custom modules and packages
- Integration points where you call external systems (show the integration component, not the library)

**Exception**: Show library if it's architecturally significant:

- Circuit breaker library (Resilience4j) if resilience is key architectural concern
- GraphQL server library if it defines API layer architecture
- ORM framework (Hibernate, Sequelize) if data access strategy matters

**Q: How do I document microservices that share code via shared libraries?**

**A**: Show shared libraries as external dependencies, not duplicate components.

**Approach**:

1. **Container Diagram**: Each microservice is separate container (don't show shared library)
2. **Component Diagram**: Show library as external dependency
3. **External Dependency Notation**:

```
Shared Validation Library
[External Library: npm package]
Business rule validation used by multiple services
```

**Anti-pattern**: Duplicating shared library components in every microservice's component diagram. This creates maintenance burden and obscures what's truly service-specific.

**Alternative**: Create a separate C4 diagram set for the shared library if it's complex enough to warrant documentation.

**Q: Should I use C4 for frontend applications?**

**A**: Yes - C4 works well for frontend applications.

**Frontend as Container**:

Show frontend application as container in Container diagram:

```
Web Application
[Container: React SPA]
Customer-facing web interface
```

**Frontend Component Diagram**:

Show internal structure:

- **UI Components**: HeaderComponent, ProductListComponent, CheckoutComponent
- **State Management**: Redux store, Context providers
- **API Clients**: OrderAPIClient, UserAPIClient
- **Routing**: React Router configuration

**Frontend-specific considerations**:

- Show API integration points (which backend services does frontend call?)
- Show state management architecture if complex (Redux, MobX, Context API)
- Show routing structure if application has many routes
- Consider showing authentication flow in Dynamic diagram

**Q: What if my team doesn't have time to create diagrams?**

**A**: Create only what adds value; don't aim for perfection.

**Minimum viable C4 documentation**:

1. **System Context diagram**: Whiteboard sketch showing system, users, external systems
2. **Container diagram**: List all deployable units with technology choices

That's it. Two diagrams. This covers 80% of what stakeholders need.

**Don't create**:

- Component diagrams if code is self-explanatory
- Code diagrams (almost never needed)
- Deployment diagrams if infrastructure is standard (Kubernetes, AWS)

**Recommendation**: Start with Context + Container. Add more diagrams only when asked "how does X work internally?" or "what happens during Y workflow?"

## Further Learning

### Official Resources

**Primary Documentation**:

- [C4 Model Official Website](https://c4model.com/) - Comprehensive guide and examples
- [Simon Brown's Website](https://simonbrown.je/) - Creator's blog and presentations
- [C4 Model FAQ](https://c4model.com/faq) - Common questions and best practices

**Books**:

- [The C4 Model for Visualising Software Architecture](https://leanpub.com/visualising-software-architecture) - Comprehensive book by Simon Brown
- [The C4 Model (O'Reilly, 2026)](https://www.oreilly.com/library/view/the-c4-model/9798341660113/) - New edition with early access

**Additional Materials**:

- [C4 Model Wikipedia](https://en.wikipedia.org/wiki/C4_model) - Overview and history
- [Diagrams | C4 model](https://c4model.com/diagrams) - Detailed diagram type explanations
- [Notation | C4 model](https://c4model.com/diagrams/notation) - Visual conventions guide

### Community and Tools

**Diagramming Tools**:

- **Structurizr**: Purpose-built tool for C4 diagrams (by Simon Brown)
- **PlantUML**: C4-PlantUML library for text-based diagrams
- **Mermaid**: C4 diagram support in markdown-based tool
- **draw.io**: Visual diagramming tool with C4 stencils
- **Lucidchart**: Commercial tool with C4 templates

**Resources**:

- [C4-PlantUML GitHub](https://github.com/plantuml-stdlib/C4-PlantUML) - PlantUML integration
- Workshop materials and conference talks by Simon Brown
- Case studies from organizations using C4 model

## Related Documentation

**Repository Architecture**:

- [System Architecture Reference](../../reference/re__system-architecture.md) - C4 diagrams for OSE platform
- [Monorepo Structure Reference](../../reference/re__monorepo-structure.md) - Nx workspace architecture
- [Repository Governance Architecture](../../../../governance/repository-governance-architecture.md) - Six-layer governance

**Documentation Standards**:

- [Diátaxis Framework](../../../../governance/conventions/meta/diataxis-framework.md) - Documentation organization
- [Diagrams Convention](../../../../governance/conventions/formatting/diagrams.md) - Mermaid diagram standards
- [Accessible Diagrams Skill](../../../.claude/skills/docs-creating-accessible-diagrams/SKILL.md) - WCAG-compliant color palette

**Development Practices**:

- [Simplicity Over Complexity Principle](../../../../governance/principles/general/simplicity-over-complexity.md) - Why C4 model fits our values
- [Explicit Over Implicit Principle](../../../../governance/principles/software-engineering/explicit-over-implicit.md) - Clear architecture documentation

## Sources

This document was created using information from the following authoritative sources:

- [Home | C4 model](https://c4model.com/) - Official C4 model website
- [FAQ | C4 model](https://c4model.com/faq) - Frequently asked questions and best practices
- [Diagrams | C4 model](https://c4model.com/diagrams) - Diagram types and usage guide
- [Notation | C4 model](https://c4model.com/diagrams/notation) - Visual notation conventions
- [The C4 Model [Book]](https://www.oreilly.com/library/view/the-c4-model/9798341660113/) - O'Reilly book (2026 edition)
- [Simon Brown](https://simonbrown.je) - Creator's website and resources
- [C4 model - Wikipedia](https://en.wikipedia.org/wiki/C4_model) - Overview and history
- [What is C4 Model? Complete Guide for Software Architecture](https://miro.com/diagramming/c4-model-for-software-architecture/) - Miro guide
- [C4 Model: Importance, Use Cases, and Examples](https://www.codesee.io/learning-center/c4-model) - CodeSee learning center
- [The C4 Model for Software Architecture - InfoQ](https://www.infoq.com/articles/C4-architecture-model/) - InfoQ article

---

**Document Metadata**:

- **Created**: 2026-01-18
- **Author**: Claude Code (AI agent)
- **License**: MIT (same as repository)
- **Validation**: Web-searched and verified against official C4 model sources
