# Best Practices

> **Companion Document**: For common mistakes to avoid, see [Antipatterns](./ex-soen-ar-c4armo__15-anti-patterns.md)

## Overview

This document provides comprehensive guidance on best practices when using the C4 model for software architecture documentation. Understanding these principles helps create clear, maintainable, and valuable architecture diagrams that effectively communicate system structure to all stakeholders.

The C4 model's strength lies in its simplicity and pragmatism, but these same qualities can be undermined by misuse. This guide draws from real-world experience and community best practices to help teams maximize the value of their architecture documentation while avoiding common pitfalls.

## Software Engineering Principles Alignment

C4 best practices directly implement core software engineering principles:

- **[Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** - The "Avoid Ambiguity" best practice requires explicit labels, clear technology choices, and descriptive relationships on every diagram element. No hidden assumptions allowed.

- **[Simplicity Over Complexity](../../../../../governance/principles/general/simplicity-over-complexity.md)** - The "Keep It Simple" best practice enforces simple boxes and lines instead of complex UML notation. The "Focus on Necessary Diagrams Only" practice prevents over-documentation.

- **[Automation Over Manual](../../../../../governance/principles/software-engineering/automation-over-manual.md)** - The "Iterative Refinement" best practice encourages diagram-as-code with Structurizr DSL, PlantUML, or Mermaid, enabling version control and CI/CD integration.

- **[Reproducibility First](../../../../../governance/principles/software-engineering/reproducibility.md)** - The "Maintain Consistency" best practice establishes standardized naming, colors, and conventions that enable reproducible documentation across teams and systems.

Each best practice below demonstrates how these principles apply to architecture documentation.

## Core Best Practices

### 1. Avoid Ambiguity

**Principle**: Every diagram should be immediately understandable without requiring extensive explanation or context.

This best practice directly implements **[Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)**. By requiring explicit labels on all diagram elements, clear technology choices in square brackets, and descriptive relationship names, we eliminate hidden assumptions that plague architecture documentation. Every stakeholder should understand system boundaries, interactions, and technology decisions without needing to ask clarifying questions.

**Key Guidelines**:

- Include descriptive titles that clearly state what the diagram shows
- Add legends to explain symbols, colors, and line styles
- Label all relationships with verbs describing the interaction
- Provide technology choices in square brackets (e.g., "[Spring Boot]")
- Add sufficient descriptive text to elements and relationships

**Example - Clear Context Diagram**:

```mermaid
C4Context
    title System Context Diagram for Tax Management Platform

    Person(taxpayer, "Taxpayer", "Individual or organization calculating and paying taxes")
    Person(admin, "Platform Admin", "Manages tax distribution and beneficiary verification")

    System(tax, "Tax Management Platform", "Calculates tax obligations, processes payments, and manages distribution to verified beneficiaries")

    System_Ext(payment, "Payment Gateway", "Processes payment transactions [Stripe/PayPal]")
    System_Ext(bank, "Banking API", "Provides account balance data for tax calculation")
    System_Ext(regulation, "Regulation Database", "Provides regulatory rulings and calculation methodologies")
    System_Ext(beneficiary, "Beneficiary Verification Service", "Verifies eligibility of tax recipients")

    Rel(taxpayer, tax, "Calculates taxes, Makes payments", "HTTPS/JSON")
    Rel(admin, tax, "Manages beneficiaries, Reviews distributions", "HTTPS/JSON")
    Rel(tax, payment, "Processes payments", "REST API")
    Rel(tax, bank, "Retrieves account balances", "REST API/OAuth2")
    Rel(tax, regulation, "Queries tax rulings", "REST API")
    Rel(tax, beneficiary, "Verifies recipient eligibility", "REST API")

    UpdateElementStyle(tax, $fontColor="white", $bgColor="green", $borderColor="darkgreen")
    UpdateRelStyle(taxpayer, tax, $textColor="blue", $lineColor="blue")
    UpdateRelStyle(admin, tax, $textColor="blue", $lineColor="blue")
```

**Benefits**:

- Stakeholders immediately understand system boundaries and interactions
- No verbal explanation required during presentations
- Diagrams remain useful when reviewed months later
- Reduces misinterpretation and miscommunication

### 2. Keep It Simple

**Principle**: C4 is deliberately simple and pragmatic. Don't overcomplicate diagrams with unnecessary notation or complexity.

This best practice embodies **[Simplicity Over Complexity](../../../../../governance/principles/general/simplicity-over-complexity.md)**. C4 deliberately uses simple boxes and lines instead of complex UML notation, reducing cognitive load for all audiences. The four-level hierarchy provides progressive disclosure—you add detail only where it creates value, avoiding the over-engineering trap that makes documentation intimidating and unmaintainable.

**Key Guidelines**:

- Use simple boxes and lines - avoid complex UML notation
- Show only relevant elements for the current abstraction level
- Limit the number of elements per diagram (5-9 for Context, 10-15 for Container)
- Focus on the 80/20 rule - document what matters most
- Prefer multiple simple diagrams over one complex diagram

**Example - Simple Container Diagram**:

```mermaid
C4Container
    title Container Diagram for Product Certification System (Core Containers Only)

    Person(applicant, "Business Owner", "Applies for product certification")
    Person(auditor, "Product Auditor", "Conducts facility inspections and reviews")

    Container(web, "Web Application", "React", "Provides certification application and tracking interface")
    Container(api, "API Gateway", "Spring Cloud Gateway", "Routes requests and handles authentication")
    Container(cert_service, "Certification Service", "Spring Boot", "Manages certification lifecycle and business logic")
    Container(db, "Certification Database", "PostgreSQL", "Stores applications, audits, and certificates")
    Container(file_storage, "Document Storage", "AWS S3", "Stores inspection photos and compliance documents")

    System_Ext(email, "Email Service", "Sends notifications to applicants and auditors")

    Rel(applicant, web, "Submits application, Views status", "HTTPS")
    Rel(auditor, web, "Reviews applications, Uploads audit reports", "HTTPS")
    Rel(web, api, "Makes API calls", "HTTPS/JSON")
    Rel(api, cert_service, "Routes requests", "REST")
    Rel(cert_service, db, "Reads/Writes certification data", "JDBC")
    Rel(cert_service, file_storage, "Stores/Retrieves documents", "AWS SDK")
    Rel(cert_service, email, "Sends notifications", "SMTP")

    UpdateElementStyle(cert_service, $fontColor="white", $bgColor="green", $borderColor="darkgreen")
```

**Benefits**:

- Easier to understand and maintain
- Faster to create and update
- More likely to be kept current
- Better suited for presentations and discussions

### 3. Focus on Necessary Diagrams Only

**Principle**: Not every system needs all four levels of C4 diagrams. Create only what provides value.

**Decision Matrix**:

| Diagram Level      | When to Create                                               | When to Skip                                                     |
| ------------------ | ------------------------------------------------------------ | ---------------------------------------------------------------- |
| **System Context** | Always create for any system                                 | Never skip                                                       |
| **Container**      | Create for any multi-container system                        | Skip for single-deployment systems (e.g., monolithic mobile app) |
| **Component**      | Create for complex containers needing internal documentation | Skip for simple containers or when code is self-documenting      |
| **Code**           | Rarely create; use existing UML/ER diagrams if needed        | Skip unless specific regulatory or educational need              |

**Example - Minimal Viable Documentation Set**:

For a simple Product lookup mobile app:

1. **System Context** (Required):
   - Shows mobile app, users, and external product database API
   - Sufficient for stakeholder communication

2. **Container** (Skip):
   - Single mobile application deployment
   - No value in showing internal app architecture in Container diagram

3. **Component** (Optional):
   - Could document internal app architecture if complex
   - Better served by mobile app architecture diagrams (MVC/MVVM)

For a complex banking platform:

1. **System Context** (Required):
   - Shows integration with external systems (payment gateways, credit bureaus, central bank)

2. **Container** (Required):
   - Shows web apps, mobile apps, microservices, databases, message queues
   - Critical for understanding deployment and scaling

3. **Component** (Selective):
   - Create only for complex services (e.g., loan financing calculator)
   - Skip for simple CRUD services

4. **Code** (Rarely):
   - Create only for critical algorithms (e.g., Securities transaction flow)

### 4. Maintain Consistency

**Principle**: Use consistent naming, symbols, colors, and conventions across all diagrams in a system or organization.

This best practice implements both **[Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** and **[Reproducibility First](../../../../../governance/principles/software-engineering/reproducibility.md)**. Consistent naming makes system elements explicit and unambiguous across abstraction levels. Standardized notation and color conventions enable different team members to produce reproducible documentation—the same system will be diagrammed consistently regardless of who creates the diagram.

**Consistency Guidelines**:

**Naming Conventions**:

- Use business terminology, not technical jargon
- Be consistent with plurality (e.g., "User" vs "Users")
- Match names across abstraction levels (Container names should match in Component diagrams)

**Color Conventions**:

- Gray: External systems/containers
- Blue: User-facing applications
- Green: Backend services/APIs
- Orange: Data stores
- Red: Security/authentication components

**Technology Labels**:

- Always include in square brackets: `[Spring Boot]`, `[React]`, `[PostgreSQL]`
- Use consistent technology names across diagrams
- Include version numbers only when relevant (e.g., `[Java 17]`)

**Example - Consistent Naming Across Levels**:

```mermaid
C4Context
    title System Context - Bond Management Platform

    Person(investor, "Bond Investor", "Invests in compliant bonds")
    System(bond_platform, "Bond Management Platform", "Manages bonds issuance, trading, and profit distribution")
    System_Ext(custody, "Asset Custody Service", "Holds underlying assets for bonds")

    Rel(investor, bond_platform, "Trades bonds, Receives profit distributions")
    Rel(bond_platform, custody, "Verifies asset ownership")

    UpdateElementStyle(bond_platform, $fontColor="white", $bgColor="green", $borderColor="darkgreen")
```

**Corresponding Container Diagram**:

```mermaid
C4Container
    title Container Diagram - Bond Management Platform

    Person(investor, "Bond Investor")

    System_Boundary(bond_platform, "Bond Management Platform") {
        Container(web, "Investor Portal", "React", "Web interface for bond trading")
        Container(trading_api, "Trading Service", "Spring Boot", "Handles bond buy/sell orders")
        Container(profit_api, "Profit Distribution Service", "Spring Boot", "Calculates and distributes profits")
        Container(db, "Bond Database", "PostgreSQL", "Stores bond data and transactions")
    }

    System_Ext(custody, "Asset Custody Service")

    Rel(investor, web, "Trades bonds", "HTTPS")
    Rel(web, trading_api, "Submits orders", "REST/JSON")
    Rel(trading_api, db, "Stores trades", "JDBC")
    Rel(profit_api, db, "Reads bond holdings", "JDBC")
    Rel(trading_api, custody, "Verifies assets", "REST API")

    UpdateElementStyle(trading_api, $fontColor="white", $bgColor="green", $borderColor="darkgreen")
    UpdateElementStyle(profit_api, $fontColor="white", $bgColor="green", $borderColor="darkgreen")
```

**Note**: System name "Bond Management Platform" is identical across Context and Container diagrams. Services within the platform use consistent naming (e.g., "Trading Service" not "Trade Service" or "Trading API").

### 5. Iterative Refinement

**Principle**: Start with rough diagrams and refine them through collaboration and feedback.

This best practice demonstrates **[Automation Over Manual](../../../../../governance/principles/software-engineering/automation-over-manual.md)**. By adopting diagram-as-code with tools like Structurizr DSL, PlantUML, or Mermaid, teams can version control diagrams, automate updates through CI/CD pipelines, and keep architecture documentation synchronized with code changes. Iterative refinement becomes efficient when diagrams are text files that can be reviewed, merged, and automatically validated.

**Iterative Process**:

1. **Initial Draft** (5-10 minutes):
   - Sketch boxes and lines on whiteboard or paper
   - Identify major elements and relationships
   - No colors, minimal labels

2. **Digital Version** (15-20 minutes):
   - Convert to C4 diagram using Structurizr DSL, PlantUML, or Mermaid
   - Add technology choices and basic descriptions
   - Share with team for initial feedback

3. **First Refinement** (after team review):
   - Clarify ambiguous relationships
   - Add missing external systems or containers
   - Improve element descriptions
   - Add legend if needed

4. **Stakeholder Review**:
   - Present to broader audience (architects, product managers, executives)
   - Gather questions and confusion points
   - Note what needs better explanation

5. **Final Polish**:
   - Add colors and visual hierarchy
   - Enhance descriptions based on feedback
   - Ensure consistency with other diagrams
   - Add diagram to documentation repository

6. **Continuous Updates**:
   - Review during architecture reviews
   - Update when system changes significantly
   - Schedule quarterly reviews to prevent drift

**Example - Evolution of a Diagram**:

**Version 1 (Initial Whiteboard Sketch)**:

```
[Taxpayer] --> [tax System] --> [Payment Gateway]
              |
              v
         [Database]
```

**Version 2 (Basic Digital Diagram)**:

```mermaid
C4Context
    Person(taxpayer, "Taxpayer")
    System(tax, "Tax Management System")
    System_Ext(payment, "Payment Gateway")
    System_Ext(bank, "Banking API")

    Rel(taxpayer, tax, "Uses")
    Rel(tax, payment, "Processes payments")
    Rel(tax, bank, "Gets account data")
```

**Version 3 (After Feedback - Missing Scholars)**:

```mermaid
C4Context
    title Tax Management Platform - System Context

    Person(taxpayer, "Taxpayer", "Individual calculating tax")
    Person(admin, "Administrator")

    System(tax, "Tax Management Platform", "Calculates tax and manages distribution")

    System_Ext(payment, "Payment Gateway", "Stripe API")
    System_Ext(bank, "Banking API", "Retrieves account balances")
    System_Ext(scholar, "Regulation Database", "Provides regulatory rulings")

    Rel(taxpayer, tax, "Calculates tax, Makes payments", "HTTPS")
    Rel(admin, tax, "Manages beneficiaries", "HTTPS")
    Rel(tax, payment, "Processes payments", "REST API")
    Rel(tax, bank, "Retrieves balances", "REST API")
    Rel(tax, scholar, "Queries rulings", "REST API")

    UpdateElementStyle(tax, $fontColor="white", $bgColor="green", $borderColor="darkgreen")
```

### 6. Diagram-as-Code Approach

**Principle**: Store diagrams as code in version control alongside application code for better collaboration, automation, and maintenance.

**Benefits**:

- **Version Control**: Track changes over time with Git
- **Collaboration**: Review diagram changes through pull requests
- **Automation**: Generate diagrams automatically in CI/CD pipelines
- **Consistency**: Enforce standards through linting and validation
- **Search**: Find diagrams using code search tools
- **Documentation**: Keep diagrams close to code they document

**Recommended Tools**:

1. **Structurizr DSL** (Recommended for large enterprises):
   - Domain-specific language for C4 diagrams
   - Strong tooling and validation
   - Official C4 model support

2. **PlantUML with C4-PlantUML** (Good for existing PlantUML users):
   - Extends familiar PlantUML syntax
   - Wide tool support

3. **Mermaid C4 Diagrams** (Good for GitHub/GitLab users):
   - Native support in GitHub/GitLab markdown
   - Simpler syntax, less feature-rich

**Example - Structurizr DSL**:

```dsl
workspace "Product Certification Platform" {
    model {
        applicant = person "Business Owner" "Applies for product certification"
        auditor = person "Product Auditor" "Conducts facility inspections"

        certificationSystem = softwareSystem "Product Certification System" {
            webApp = container "Web Application" "React" "Provides certification application interface"
            apiGateway = container "API Gateway" "Spring Cloud Gateway" "Routes requests"
            certService = container "Certification Service" "Spring Boot" "Manages certification lifecycle"
            database = container "Certification Database" "PostgreSQL" "Stores certification data"
        }

        emailService = softwareSystem "Email Service" "Sends notifications" "External"

        applicant -> webApp "Submits application"
        auditor -> webApp "Reviews applications"
        webApp -> apiGateway "Makes API calls" "HTTPS/JSON"
        apiGateway -> certService "Routes requests" "REST"
        certService -> database "Reads/Writes data" "JDBC"
        certService -> emailService "Sends notifications" "SMTP"
    }

    views {
        systemContext certificationSystem {
            include *
            autolayout lr
        }

        container certificationSystem {
            include *
            autolayout lr
        }
    }
}
```

**Integration with CI/CD**:

```yaml
# .github/workflows/docs.yml
name: Generate Architecture Diagrams

on:
  push:
    paths:
      - "docs/architecture/**/*.dsl"
      - "docs/architecture/**/*.puml"

jobs:
  generate-diagrams:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Generate Structurizr diagrams
        run: |
          docker run -v $(pwd):/workspace structurizr/cli export \
            -workspace /workspace/docs/architecture/workspace.dsl \
            -format png -output /workspace/docs/architecture/diagrams

      - name: Commit generated diagrams
        run: |
          git config user.name "GitHub Actions"
          git config user.email "actions@github.com"
          git add docs/architecture/diagrams
          git commit -m "docs: regenerate architecture diagrams" || exit 0
          git push
```

### 7. Proper Use of Abstraction Levels

**Principle**: Each C4 level should focus on its specific abstraction without mixing concerns from other levels.

**Level-Specific Guidelines**:

**System Context**:

- Show only software systems (not containers or components)
- External systems should be high-level (e.g., "Payment Gateway" not "Stripe API Server")
- Focus on what systems do, not how they work

**Container**:

- Show runtime containers (deployable units)
- Each container should be independently deployable/scalable
- Include databases, file stores, message queues
- Don't show implementation details (classes, functions)

**Component**:

- Show major structural components within a container
- Focus on high-level groupings (controllers, services, repositories)
- Don't show every class or module
- Stop before reaching code-level details

**Code**:

- Rarely needed
- Show actual code entities (classes, interfaces)
- Use standard UML class diagrams or ER diagrams instead

**Example - Correct Abstraction Level Usage**:

**System Context** (Correct - Shows Systems):

```mermaid
C4Context
    title System Context - Installment Purchase Financing Platform

    Person(customer, "Customer", "Applies for compliant financing")

    System(loan, "Installment Purchase Financing Platform", "Manages cost-plus financing applications and contracts")

    System_Ext(crm, "CRM System", "Customer relationship management")
    System_Ext(credit, "Credit Bureau", "Credit scoring and history")
    System_Ext(asset, "Asset Registry", "Verifies asset ownership and pricing")
    System_Ext(bank, "Core Banking System", "Processes fund transfers")

    Rel(customer, loan, "Applies for financing", "HTTPS")
    Rel(loan, crm, "Retrieves customer data", "REST API")
    Rel(loan, credit, "Checks credit score", "REST API")
    Rel(loan, asset, "Verifies asset details", "REST API")
    Rel(loan, bank, "Initiates fund transfer", "ISO 20022")

    UpdateElementStyle(loan, $fontColor="white", $bgColor="green", $borderColor="darkgreen")
```

**Container** (Correct - Shows Deployable Units):

```mermaid
C4Container
    title Container Diagram - Installment Purchase Financing Platform

    Person(customer, "Customer")

    System_Boundary(loan, "Installment Purchase Financing Platform") {
        Container(web, "Customer Portal", "React", "Web application for financing applications")
        Container(mobile, "Mobile App", "React Native", "Mobile application for customers")
        Container(api_gateway, "API Gateway", "Kong", "Routes and secures API requests")
        Container(application_service, "Application Service", "Spring Boot", "Processes financing applications")
        Container(contract_service, "Contract Service", "Spring Boot", "Generates compliant contracts")
        Container(workflow_engine, "Workflow Engine", "Camunda", "Orchestrates approval workflow")
        Container(db, "Application Database", "PostgreSQL", "Stores applications and contracts")
        Container(cache, "Cache", "Redis", "Caches customer and pricing data")
    }

    System_Ext(credit, "Credit Bureau")

    Rel(customer, web, "Applies for financing", "HTTPS")
    Rel(customer, mobile, "Applies for financing", "HTTPS")
    Rel(web, api_gateway, "Makes API calls", "REST/JSON")
    Rel(mobile, api_gateway, "Makes API calls", "REST/JSON")
    Rel(api_gateway, application_service, "Routes requests", "HTTP")
    Rel(application_service, workflow_engine, "Starts approval workflow", "REST")
    Rel(application_service, contract_service, "Generates contract", "REST")
    Rel(application_service, db, "Stores application", "JDBC")
    Rel(contract_service, db, "Stores contract", "JDBC")
    Rel(application_service, cache, "Caches data", "Redis Protocol")
    Rel(application_service, credit, "Checks credit", "REST API")

    UpdateElementStyle(application_service, $fontColor="white", $bgColor="green", $borderColor="darkgreen")
    UpdateElementStyle(contract_service, $fontColor="white", $bgColor="green", $borderColor="darkgreen")
```

**Component** (Correct - Shows Major Structural Components):

```mermaid
C4Component
    title Component Diagram - Application Service

    Container_Boundary(application_service, "Application Service") {
        Component(controller, "Application Controller", "Spring MVC", "Handles HTTP requests")
        Component(validation, "Application Validator", "Spring Component", "Validates regulatory compliance")
        Component(pricing, "Pricing Calculator", "Spring Component", "Calculates Installment Purchase markup")
        Component(service, "Application Manager", "Spring Service", "Core business logic")
        Component(repository, "Application Repository", "Spring Data JPA", "Data access layer")
    }

    ContainerDb(db, "Application Database", "PostgreSQL")
    Container(workflow_engine, "Workflow Engine", "Camunda")
    Container(contract_service, "Contract Service", "Spring Boot")

    Rel(controller, validation, "Validates application", "Method call")
    Rel(controller, service, "Processes application", "Method call")
    Rel(service, pricing, "Calculates markup", "Method call")
    Rel(service, repository, "Stores application", "Method call")
    Rel(service, workflow_engine, "Starts workflow", "REST")
    Rel(service, contract_service, "Generates contract", "REST")
    Rel(repository, db, "Reads/Writes data", "JDBC")

    UpdateElementStyle(service, $fontColor="white", $bgColor="green", $borderColor="darkgreen")
    UpdateElementStyle(pricing, $fontColor="white", $bgColor="orange", $borderColor="darkorange")
```

## Summary of Best Practices

| Practice                        | Key Guideline                                       | Benefit                             |
| ------------------------------- | --------------------------------------------------- | ----------------------------------- |
| **Avoid Ambiguity**             | Include titles, labels, legends, technology choices | Self-explanatory diagrams           |
| **Keep It Simple**              | Use simple boxes/lines, limit elements per diagram  | Easy to understand and maintain     |
| **Focus on Necessary Diagrams** | Create only Context + Container for most systems    | Avoid documentation overhead        |
| **Maintain Consistency**        | Use consistent naming, colors, technology labels    | Professional, unified documentation |
| **Iterative Refinement**        | Start rough, refine through feedback                | Collaborative, evolving diagrams    |
| **Diagram-as-Code**             | Store in version control, generate in CI/CD         | Automated, versioned, searchable    |
| **Proper Abstraction Levels**   | Keep each level focused on its specific concern     | Clear separation of concerns        |

## Practical Checklist

Before finalizing a C4 diagram, verify:

**General Quality**:

- [ ] Diagram has a clear, descriptive title
- [ ] All elements have meaningful names
- [ ] All relationships have verb labels describing the interaction
- [ ] Technology choices are included in square brackets
- [ ] Legend is provided if colors/symbols have specific meaning
- [ ] Diagram is stored as code in version control

**Abstraction Level**:

- [ ] Diagram uses only elements appropriate for its level (System/Container/Component)
- [ ] No mixing of abstraction levels in a single diagram
- [ ] External systems are shown as black boxes (no internal details)
- [ ] Subsystems represent real boundaries, not arbitrary groupings

**Clarity**:

- [ ] Diagram is understandable without verbal explanation
- [ ] Number of elements is manageable (5-9 for Context, 10-15 for Container)
- [ ] Layout is clean with minimal line crossings
- [ ] Colors enhance understanding rather than add confusion

**Consistency**:

- [ ] Naming matches across diagram levels
- [ ] Technology labels are consistent across diagrams
- [ ] Visual style matches other diagrams in the system
- [ ] Business terminology is used, not technical jargon

## Further Reading

**Official C4 Resources**:

- C4 Model Website: <https://c4model.com/>
- C4 Model FAQ: <https://c4model.com/#faq>
- Structurizr (C4 tooling): <https://structurizr.com/>

**Community Resources**:

- "Misuses and Mistakes of the C4 Model": <https://www.workingsoftware.dev/misuses-and-mistakes-of-the-c4-model/>
- "The C4 Model for Visualising Software Architecture" (InfoQ): <https://www.infoq.com/articles/C4-architecture-model/>

**Related Documentation**:

- System Context Diagrams: [ex-soen-ar-c4armo\_\_02-system-context-diagrams.md](./ex-soen-ar-c4armo__01-level-1-system-context.md)
- Container Diagrams: [ex-soen-ar-c4armo\_\_03-container-diagrams.md](./ex-soen-ar-c4armo__02-level-2-container.md)
- Component Diagrams: [ex-soen-ar-c4armo\_\_04-component-diagrams.md](./ex-soen-ar-c4armo__03-level-3-component.md)
- Antipatterns: [ex-soen-ar-c4armo\_\_15-anti-patterns.md](./ex-soen-ar-c4armo__15-anti-patterns.md)

## Related Principles

C4 best practices demonstrate alignment with core software engineering principles:

- **[Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** - "Avoid Ambiguity" and "Maintain Consistency" practices require explicit labels, clear technology choices, and visible relationships on all diagrams, eliminating architecture assumptions.

- **[Simplicity Over Complexity](../../../../../governance/principles/general/simplicity-over-complexity.md)** - "Keep It Simple" and "Focus on Necessary Diagrams Only" practices enforce simple notation and progressive disclosure, creating accessible documentation without over-engineering.

- **[Automation Over Manual](../../../../../governance/principles/software-engineering/automation-over-manual.md)** - "Iterative Refinement" practice encourages diagram-as-code (Structurizr DSL, PlantUML, Mermaid) for version control and CI/CD integration.

- **[Reproducibility First](../../../../../governance/principles/software-engineering/reproducibility.md)** - "Maintain Consistency" practice establishes standardized naming and notation conventions that enable reproducible documentation across teams.

See [Software Engineering Principles](../../../../../governance/principles/software-engineering/README.md) for comprehensive documentation.

## Conclusion

The C4 model's power comes from its simplicity and pragmatism. Following these best practices ensures your architecture documentation remains clear, valuable, and maintainable. Remember:

- **Simple is better than complex** - Use basic shapes and clear labels
- **Consistency builds understanding** - Standardize naming and visual style
- **Focus on value** - Create only diagrams that serve a purpose
- **Iterate and refine** - Start rough and improve through feedback
- **Automate everything** - Use diagram-as-code for better collaboration

When in doubt, return to the core principle: **Can someone unfamiliar with the system understand this diagram without my explanation?** If not, refine until the answer is yes.
