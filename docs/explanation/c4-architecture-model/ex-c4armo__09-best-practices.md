# Best Practices and Guidance

Comprehensive guide on when to use the C4 model, comparisons with other approaches, and common pitfalls to avoid.

## When to Use the C4 Model

### Ideal Use Cases

**Custom-Built Software Systems**:

The C4 model works best for:

- Custom applications developed in-house
- Microservices architectures
- Web applications and APIs
- Mobile applications
- Cloud-native systems
- Monorepo architectures (like Nx workspaces)

**Team Contexts**:

Particularly valuable when:

- Teams find UML/ArchiMate too complex or heavyweight
- You need architecture documentation that developers will actually maintain
- Multiple audiences need different levels of detail
- Agile/continuous delivery workflows require lightweight documentation
- New team members need onboarding materials

### When C4 May Not Fit

**Not Universally Applicable**:

The C4 model is designed for custom-built software systems. It has significant limitations for certain system types and contexts where different approaches are more appropriate.

**System Types Where C4 Struggles**:

**Embedded Systems and Firmware**:

C4 focuses on software architecture, but embedded systems require modeling hardware-software interactions, timing constraints, memory layouts, and real-time behavior. For these systems:

- Use hardware description languages (HDLs) for hardware components
- Use timing diagrams for real-time constraints
- Use memory maps for resource allocation
- Consider UML deployment diagrams with hardware nodes
- C4 can document the software layers (e.g., RTOS, drivers, application) but misses critical hardware integration

**Heavily Customized Enterprise Platforms**:

SAP, Salesforce, Oracle E-Business Suite, and similar platforms with extensive customization present challenges:

- **Problem**: C4 shows custom software systems. When 90% is platform-provided and 10% is customization, C4 becomes awkward
- **Alternative**: Document customizations separately (custom objects, workflows, integrations) rather than trying to model the entire platform
- **When C4 works**: If you build custom applications that integrate with these platforms (show platform as external system)
- **When C4 doesn't work**: If you're configuring the platform without writing custom code

**Infrastructure-as-Code Only**:

Systems defined purely through infrastructure configuration (Terraform, CloudFormation) without custom application code:

- C4 models software systems, not infrastructure topology
- Use cloud provider architecture diagrams instead
- If you do have custom software deployed on this infrastructure, C4 applies to the software, not the IaC definitions

**Libraries, Frameworks, and SDKs**:

C4 is for software **systems** (deployed, running applications), not software **libraries**:

- **Don't use C4 for**: Documenting a React component library, npm package, or Python SDK
- **Reason**: Libraries don't "run" - they're consumed by systems
- **Alternative**: Use package diagrams, API documentation, or design pattern documentation
- **Exception**: If your library requires a server component or CLI tool, C4 can document those deployable parts

**Pure Data Pipelines**:

ETL/ELT pipelines that only transform and move data without business logic:

- C4 can show pipeline architecture (containers for ingestion, transformation, storage)
- But data flow diagrams or DAG visualizations (Apache Airflow, dbt) may be more appropriate
- Consider C4 when pipelines include significant custom logic or microservices

**System Type Suitability Table**:

| System Type                            | C4 Suitability | Reason                                                   | Better Alternative                                   |
| -------------------------------------- | -------------- | -------------------------------------------------------- | ---------------------------------------------------- |
| **Custom Web Application**             | Excellent      | Core use case - custom software with clear boundaries    | N/A                                                  |
| **Microservices Architecture**         | Excellent      | Multiple deployable units, clear container boundaries    | N/A                                                  |
| **Mobile + Backend System**            | Excellent      | Multiple containers (mobile app, backend API, database)  | N/A                                                  |
| **Embedded System with Firmware**      | Poor           | Hardware-software integration, timing constraints        | Hardware diagrams, timing diagrams                   |
| **SAP with Heavy Customization**       | Poor           | Platform-provided functionality dominates                | SAP-specific modeling, integration diagrams          |
| **Salesforce with Apex Code**          | Fair           | Can model custom Apex services, but feels awkward        | Salesforce architecture diagrams                     |
| **Infrastructure-as-Code (Terraform)** | Poor           | Infrastructure configuration, not running software       | Cloud provider diagrams                              |
| **React Component Library**            | Poor           | Not a deployed system, consumed by applications          | API documentation, Storybook                         |
| **ETL Data Pipeline**                  | Fair           | Can show pipeline containers, but data flow matters more | Data flow diagrams, DAG visualization                |
| **Serverless Functions (AWS Lambda)**  | Good           | Each function is a container, clear boundaries           | Consider simpler function diagrams for small systems |
| **Monolithic Desktop Application**     | Good           | Single container with components, clear structure        | N/A                                                  |
| **IoT System (Edge + Cloud)**          | Good           | Show edge devices, gateway, cloud backend separately     | N/A                                                  |

**Supplementary Diagrams Needed**:

C4 focuses on **static structure**. You'll need other diagram types for:

- **Business Processes**: Use BPMN (Business Process Model and Notation)
- **State Machines**: Use UML state diagrams
- **Data Models**: Use Entity-Relationship Diagrams
- **Data Flow**: Use data flow diagrams (DFD)
- **Network Topology**: Use infrastructure diagrams
- **Sequence of Operations**: Use C4 dynamic diagrams or UML sequence diagrams

**Scaling Considerations**:

For very large systems with high complexity:

- **Don't create monolithic diagrams**: Single diagrams with hundreds of elements become unreadable
- **Split by bounded context**: Create separate C4 diagram sets for each business domain (e.g., Order Management, User Management, Payments)
- **Use system landscape diagrams**: Organizational overview showing how multiple systems relate
- **Focus on integration points**: For large microservices architectures, document key integration patterns rather than every service
- **Selective component diagrams**: Only create component diagrams for complex or critical containers

**When to Question C4 Usage**:

- If your "container diagram" has 50+ containers, consider splitting by domain
- If you're spending more time maintaining diagrams than code, simplify
- If stakeholders don't understand the diagrams, you may be over-documenting
- If the system is simple (3-4 components), C4 may be overkill - a single diagram might suffice

## Comparison to Other Approaches

### C4 vs. UML (Unified Modeling Language)

**UML**:

- **Pros**: Comprehensive, standardized, supports many diagram types
- **Cons**: Complex, steep learning curve, can be overwhelming for teams

**C4**:

- **Pros**: Simple, developer-friendly, easier to learn and maintain
- **Cons**: Less comprehensive, focuses only on structure (not behavior extensively)

**Relationship**:

C4 is a **simplified version** of UML concepts:

- C4 component diagrams ≈ UML component diagrams (but simpler)
- C4 code diagrams often use UML class diagrams
- C4 dynamic diagrams ≈ UML sequence diagrams

**When to Choose**:

- **Use UML** if your team already successfully uses it and has expertise
- **Use C4** if UML feels too heavyweight or incompatible with your agile workflow
- **Use Both** - C4 for high-level architecture, UML for detailed design

### C4 vs. arc42

**arc42**:

- Template-based architecture documentation framework
- Comprehensive sections for all architecture concerns
- More than just diagrams - includes decisions, quality requirements, etc.

**C4**:

- Focused specifically on architecture visualization
- Hierarchical diagram approach
- Less prescriptive about documentation structure

**Relationship**:

C4 and arc42 **complement each other**:

- **C4 System Context** → arc42 "Context and Scope"
- **C4 Container Diagram** → arc42 "Building Block View (Level 1)"
- **C4 Component Diagram** → arc42 "Building Block View (Level 2)"
- **C4 Code Diagram** → arc42 "Building Block View (Level 3)"

**When to Choose**:

- **Use arc42** for comprehensive architecture documentation framework
- **Use C4** for architecture visualization specifically
- **Use Both** - C4 diagrams within arc42 structure

### C4 vs. 4+1 View Model

**4+1 View Model**:

- Five views: Logical, Development, Process, Physical, and Scenarios (+1)
- Academic and comprehensive
- Separates concerns across multiple views

**C4**:

- Four hierarchical levels of the same system
- Single progression from context to code
- Simpler and more pragmatic

**Relationship**:

C4 evolved from 4+1 but deliberately simplifies:

- C4 focuses on hierarchical zoom levels
- 4+1 separates different architectural perspectives
- C4 prioritizes ease of use over comprehensive coverage

## Best Practices

### Creating Effective C4 Diagrams

**Start High, Drill Down**:

1. Always begin with System Context diagram
2. Create Container diagram for overall architecture
3. Only create Component diagrams where complexity justifies it
4. Reserve Code diagrams for truly complex components

**You Don't Need All Four Levels**:

Most teams find **Context + Container diagrams sufficient** for documentation needs. Component and Code diagrams add value only for:

- Complex containers with significant internal structure
- Areas requiring detailed documentation
- Legacy code needing explanation
- Onboarding-critical components

**Focus on Value**:

Ask: "Does this diagram help someone understand the system better?"

If not, skip it. Over-documentation wastes time and becomes outdated quickly.

### Diagram Quality Guidelines

**Keep Diagrams Focused**:

- **Small Systems**: 1 context diagram, 1 container diagram
- **Medium Systems**: 1 context, 1 container, 2-3 component diagrams
- **Large Systems**: 1 landscape, multiple context diagrams (per bounded context), focused container/component diagrams

**Avoid Clutter**:

- Don't show hundreds of elements in one diagram
- Split large systems into multiple focused diagrams
- Group related containers into subgraphs
- Show only relevant relationships (not every possible connection)

**Use Consistent Notation**:

- Establish team conventions for shapes and colors
- Document your notation choices in diagram keys
- Apply same style across all diagrams
- Update diagrams when notation changes

**Keep Diagrams Updated**:

- Review during architecture changes
- Update as part of feature development
- Automate diagram generation where possible
- Don't create diagrams you won't maintain

### Team Collaboration

**Shared Ownership**:

- Don't assume architects create container diagrams and developers create code diagrams
- Encourage team collaboration on all levels
- Use diagrams as communication tools, not deliverables
- Iterate and refine diagrams together

**Tool Selection**:

- Choose tools that team actually uses (not perfect tools they ignore)
- Prefer tools that integrate with version control
- Consider diagram-as-code approaches (Mermaid, PlantUML, Structurizr DSL)
- Balance power with ease of use

**Documentation Culture**:

- Make architecture documentation part of definition of done
- Review diagrams during code reviews
- Use diagrams in team meetings and presentations
- Celebrate good documentation examples

### Tooling Philosophy: Modeling vs. Diagramming

Understanding the distinction between **modeling tools** and **diagramming tools** helps you choose the right approach for C4 diagrams.

**Modeling Tools** (model-first approach):

- **Concept**: Create a model (data structure), generate diagrams from model
- **Example**: Structurizr (official C4 tool), Enterprise Architect with C4 plugin
- **Workflow**: Define systems, containers, components, and relationships in code or UI → tool renders diagrams automatically
- **Advantages**:
  - Single source of truth (model)
  - Multiple views generated from same model
  - Consistency enforced automatically
  - Easier to keep synchronized across diagrams
- **Disadvantages**:
  - Steeper learning curve (learn modeling DSL or API)
  - Less visual freedom (layout controlled by tool)
  - Tool lock-in (model format specific to tool)

**Diagramming Tools** (diagram-first approach):

- **Concept**: Manually draw diagrams using shapes and connectors
- **Examples**: draw.io, Mermaid, PlantUML, Lucidchart, Excalidraw
- **Workflow**: Drag boxes, draw arrows, add labels → create diagram directly
- **Advantages**:
  - Easy to learn (visual interface or simple syntax)
  - Full control over layout and appearance
  - Wide tool choice, no lock-in
  - Familiar to most developers
- **Disadvantages**:
  - No single source of truth (each diagram independent)
  - Manual synchronization across diagrams
  - Inconsistencies possible (same element different names)
  - More maintenance burden

**Structurizr: Official C4 Modeling Tool**:

Structurizr was created by Simon Brown (C4 model creator) specifically for C4 diagrams:

- **Structurizr DSL**: Text-based language for defining architecture models
- **Structurizr Lite**: Free, open-source, self-hosted version
- **Structurizr Cloud**: Commercial SaaS offering with collaboration features
- **Benefits**:
  - Purpose-built for C4 model
  - Automatic layout with manual override
  - Multiple diagram types from single model
  - Version control friendly (DSL is text)
- **When to use**: Teams serious about architecture documentation, willing to invest in learning DSL

**Tool Comparison Table (2026 Recommendations)**:

| Tool            | Type        | Cost                       | C4 Support                 | Learning Curve | Version Control   | Team Collaboration           | Best For                                    |
| --------------- | ----------- | -------------------------- | -------------------------- | -------------- | ----------------- | ---------------------------- | ------------------------------------------- |
| **Structurizr** | Modeling    | Free (Lite) / Paid (Cloud) | Native (official)          | Medium-High    | Excellent (DSL)   | Good (Cloud)                 | Serious C4 practitioners, large teams       |
| **Mermaid**     | Diagramming | Free                       | Good (experimental)        | Low-Medium     | Excellent (text)  | Good (GitHub/Markdown)       | Documentation-as-code, open source projects |
| **PlantUML**    | Diagramming | Free                       | Good (C4-PlantUML library) | Medium         | Excellent (text)  | Good (any text editor)       | Java teams, enterprise developers           |
| **draw.io**     | Diagramming | Free                       | Fair (C4 stencils)         | Low            | Fair (XML format) | Good (desktop/web)           | Quick ad-hoc diagrams, non-technical teams  |
| **Lucidchart**  | Diagramming | Paid                       | Fair (templates)           | Low            | Fair              | Excellent (real-time collab) | Distributed teams, non-developers           |
| **Excalidraw**  | Diagramming | Free                       | Fair (manual)              | Very Low       | Good (JSON)       | Good (shareable links)       | Lightweight whiteboarding, sketches         |

**Diagram-as-Code: 2026 Best Practices**:

Modern teams prefer text-based diagram tools that integrate with version control:

**Recommended Stack**:

1. **Primary: Mermaid** (in Markdown files)
   - Native GitHub rendering
   - Simple syntax, low learning curve
   - C4 diagram type supported (experimental status as of 2026)
   - Integrates with documentation sites (Hugo, Jekyll, MkDocs)
2. **Advanced: Structurizr DSL** (for complex systems)
   - Model-first approach
   - Automatic diagram generation
   - Better for large systems (20+ containers)
3. **Prototyping: draw.io or Excalidraw** (for quick sketches)
   - Whiteboard sessions
   - Iterate quickly before codifying in Mermaid/Structurizr

**This Repository's Approach (Mermaid)**:

Open Sharia Enterprise uses **Mermaid diagrams** for C4 model documentation:

**Why Mermaid**:

- **GitHub-native**: Renders automatically in markdown files
- **Version controlled**: Text format diffs well in Git
- **Accessible**: WCAG-compliant color palette enforced
- **Portable**: Works in Obsidian, VS Code, documentation sites
- **No build step**: Renders client-side in browser

**Trade-offs Accepted**:

- **Manual synchronization**: Each diagram independent, must update related diagrams manually
- **No automatic layout**: Must arrange elements by hand
- **Limited C4 features**: Mermaid C4 support simpler than Structurizr
- **Experimental status**: Mermaid C4 syntax may change in future releases (experimental as of 2026)

**When to Switch**:

If this repository grows beyond 30 containers or requires complex cross-cutting views, consider migrating to Structurizr DSL while keeping Mermaid for simple diagrams.

### Practical Workflow: Creating C4 Diagrams

**Step-by-Step Process for New Systems**:

**Phase 1: Initial Documentation (First Week)**

1. **Gather Context** (30 minutes)
   - Interview stakeholders: who uses the system? What external systems does it depend on?
   - List all users, external systems, and integration points
2. **Create System Context Diagram** (30 minutes)
   - Draw the system (one box)
   - Add users (people boxes)
   - Add external systems (other system boxes)
   - Connect with labeled relationships
   - Review with team for accuracy
3. **Create Container Diagram** (1 hour)
   - List all deployable units (apps, databases, services)
   - Identify technology stack for each container
   - Draw containers with technology labels
   - Connect containers showing communication protocols
   - Review with developers for completeness

**Deliverable**: 2 diagrams in 2 hours. You now have minimum viable architecture documentation.

**Phase 2: Detailed Documentation (As Needed)**

1. **Identify Complex Containers** (15 minutes)
   - Which containers have non-obvious internal structure?
   - Which containers cause confusion for new team members?
   - Which containers require component-level understanding?
2. **Create Component Diagrams** (1 hour per container)
   - For each complex container:
     - List major components (controllers, services, repositories)
     - Show component relationships
     - Indicate external dependencies
   - Review with developers who work on that container
3. **Document Key Workflows** (1 hour per workflow)
   - Identify critical user journeys or processes
   - Create Dynamic diagram (sequence or collaboration)
   - Show error paths and edge cases
   - Review with developers and QA

**Phase 3: Infrastructure Documentation (Before Deployment)**

1. **Create Deployment Diagram** (1-2 hours)
   - Document production infrastructure
   - Show containers mapped to infrastructure
   - Identify scaling strategy
   - Document external dependencies (cloud services, third-party APIs)

**Completeness Checklists**:

**System Context Diagram Checklist**:

- [ ] System clearly identified with name and description
- [ ] All user types shown (internal and external)
- [ ] All external systems shown (third-party, legacy, other teams)
- [ ] Relationships labeled with purpose (not just "uses")
- [ ] Diagram has title and legend
- [ ] Reviewed with stakeholders for accuracy

**Container Diagram Checklist**:

- [ ] All deployable units identified (apps, services, databases, message queues)
- [ ] Technology stack specified for each container (`[Container: Spring Boot]`)
- [ ] Communication protocols labeled (`HTTP/REST`, `gRPC`, `AMQP`)
- [ ] External systems from context diagram included
- [ ] Database-per-service pattern visible (if microservices)
- [ ] Diagram has title and legend
- [ ] Reviewed with development team

**Component Diagram Checklist** (per container):

- [ ] Major components identified (controllers, services, repositories, clients)
- [ ] Component responsibilities described
- [ ] Design patterns noted (`[Component: REST Controller]`)
- [ ] External dependencies shown (other containers, external systems)
- [ ] Database access components identified
- [ ] Diagram has title indicating which container
- [ ] Reviewed with container developers

**Deployment Diagram Checklist**:

- [ ] Infrastructure type specified (Kubernetes, AWS ECS, on-premise VMs)
- [ ] All containers mapped to infrastructure nodes
- [ ] Scaling strategy visible (replicas, auto-scaling groups)
- [ ] Network boundaries shown (VPCs, subnets, security groups)
- [ ] External services mapped to infrastructure (S3, RDS, external APIs)
- [ ] Environment clearly labeled (dev, staging, production)
- [ ] Reviewed with DevOps/infrastructure team

**Diagram Evolution as System Grows**:

**Startup Phase** (1-5 containers):

- 1 System Context
- 1 Container Diagram
- 0-1 Component Diagrams

**Growth Phase** (6-15 containers):

- 1 System Context
- 1 Container Diagram (may split by domain)
- 2-3 Component Diagrams (critical services)
- 1-2 Dynamic Diagrams (key workflows)
- 1 Deployment Diagram (production)

**Maturity Phase** (16+ containers):

- 1 System Landscape
- 2-4 System Context Diagrams (per bounded context)
- 2-4 Container Diagrams (per bounded context)
- 5-8 Component Diagrams (selective, complex services only)
- 3-5 Dynamic Diagrams (authentication, critical transactions)
- 2-3 Deployment Diagrams (dev, staging, prod)

**Maintenance Cadence**:

- **Weekly**: Review during sprint planning if architecture changes planned
- **Monthly**: Quick audit - do diagrams match reality?
- **Quarterly**: Comprehensive review - update all stale diagrams
- **On Major Changes**: Update immediately when deploying significant architecture changes

## Limitations and Scope

### What C4 Doesn't Cover

**Static Structure Only**:

C4 primarily focuses on **static architecture** (boxes and lines showing structure). For runtime behavior, supplement with:

- **Dynamic Diagrams** (C4 supplementary type)
- **Sequence Diagrams** (UML)
- **BPMN Process Flows**
- **State Machine Diagrams**

**Not a Complete Documentation Framework**:

C4 doesn't address:

- Architecture Decision Records (ADRs)
- Quality attribute scenarios
- Architectural principles and constraints
- Technology radar or standards
- Deployment procedures
- Operational runbooks

Consider combining C4 with comprehensive frameworks like arc42 for full coverage.

### Common Mistakes to Avoid

**Mixing Abstraction Levels**:

- Don't mix context-level and component-level details in same diagram
- Keep each diagram focused on its level
- Use hierarchical links between diagrams instead

**Over-Documenting**:

- Creating diagrams for every container (when many are simple)
- Documenting implementation details that change frequently
- Making diagrams no one reads or maintains

**Under-Labeling**:

- Missing diagram titles or keys
- Unlabeled relationships ("uses" doesn't say much)
- No technology indicators on containers
- Missing element type specifications

**Stale Documentation**:

- Creating diagrams once and never updating
- Diagrams contradicting actual architecture
- No process for keeping diagrams current

**Tool Obsession**:

- Spending more time on tooling than documentation
- Choosing complex tools that team won't adopt
- Over-engineering diagram generation

## C4 Model in This Repository

### Current Usage

The Open Sharia Enterprise platform uses C4 model for architecture documentation in:

**Reference Documentation**: [docs/reference/re\_\_system-architecture.md](../../reference/re__system-architecture.md)

**C4 Levels Implemented**:

1. **System Context** (Level 1): Shows OSE platform, users, and external systems
2. **Container Diagram** (Level 2): Shows 8 applications across technology stacks
3. **Component Diagrams** (Level 3): Detailed breakdowns for:
   - dolphin-be (Spring Boot backend)
   - dolphin-fe (Next.js frontend)
   - ayokoding-cli (Go CLI tool)
   - butler-cli (Go CLI tool)
   - ose-platform-web (Hugo static site)
   - ayokoding-web (Hugo static site)
   - E2E test suites (Playwright)
4. **Code Diagrams** (Level 4): Includes:
   - Database ER diagrams for dolphin-be
   - Class diagrams for Spring Boot architecture
   - Component hierarchies for React/Next.js
   - Package structures for Go applications

**Supplementary Diagrams**:

- **Dynamic Diagrams**: Sequence diagrams for authentication flows, transaction processing, content pipeline
- **Deployment Diagrams**: Multi-environment deployment architecture (local, dev, staging, prod)

### Diagramming Approach

**Tool**: Mermaid diagrams (text-based, version-controlled)

**Color Palette**: WCAG-compliant, color-blind friendly palette from [governance/conventions/formatting/diagrams.md](../../../governance/conventions/formatting/diagrams.md)

**Accessibility**: All diagrams include proper legends and contrast ratios meeting WCAG AA standards

### Learning from Examples

To see C4 model in practice, review:

1. **System Context Diagram**: High-level view of platform and integrations
2. **Container Diagram**: Technical building blocks and communication patterns
3. **Component Diagrams**: Internal architecture of complex containers
4. **Sequence Diagrams**: Runtime flows and interactions

These diagrams demonstrate how C4 model scales from simple overview to detailed implementation documentation while maintaining clarity at each level.
