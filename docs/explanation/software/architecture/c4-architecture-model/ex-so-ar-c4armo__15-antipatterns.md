# Antipatterns

> **Companion Document**: For positive guidance on what to do, see [Best Practices](./ex-so-ar-c4armo__14-best-practices.md)

## Overview

This document identifies common antipatterns and mistakes when using the C4 model for software architecture documentation. Understanding what to avoid is just as important as knowing what to do. These antipatterns undermine the clarity and value that the C4 model is designed to provide.

The C4 model's strength lies in its simplicity and pragmatism. When misused through these antipatterns, it loses its effectiveness and becomes just another confusing architecture notation. This guide helps teams recognize and avoid common pitfalls that reduce the value of their architecture documentation.

## Common Antipatterns

### 1. Mixing Container and Component Abstractions

**Problem**: Showing both containers (deployable units) and components (internal code structures) in the same diagram.

**Why It's Wrong**:

- Violates separation of abstraction levels
- Creates confusion about deployment boundaries
- Makes diagrams harder to understand
- Loses the clarity that makes C4 valuable

**Example - Incorrect Mixing**:

```
❌ Container Diagram (WRONG - Mixes Containers and Components)

[Web Application - React] ---> [API Gateway - Kong]
[Mobile App - React Native] -> [API Gateway - Kong]

[API Gateway] ---> [UserController - Spring MVC]  ⚠️ Component, not Container!
[API Gateway] ---> [OrderController - Spring MVC]  ⚠️ Component, not Container!

[UserController] ---> [User Service - Spring Boot]  ✓ Container
[OrderController] ---> [Order Service - Spring Boot]  ✓ Container
```

**Correct Approach**:

Create separate diagrams for each abstraction level:

**Container Diagram**:

```mermaid
C4Container
    title Container Diagram - Qard Hasan Microfinance Platform (Correct)

    Person(borrower, "Borrower")

    System_Boundary(qard, "Qard Hasan Platform") {
        Container(web, "Web Application", "React", "Borrower interface")
        Container(api_gateway, "API Gateway", "Kong", "Routes API requests")
        Container(loan_service, "Loan Service", "Spring Boot", "Manages interest-free loans")
        Container(repayment_service, "Repayment Service", "Spring Boot", "Tracks loan repayments")
        Container(db, "Loan Database", "PostgreSQL", "Stores loan data")
    }

    Rel(borrower, web, "Applies for loan", "HTTPS")
    Rel(web, api_gateway, "Makes API calls", "REST/JSON")
    Rel(api_gateway, loan_service, "Routes loan requests", "HTTP")
    Rel(api_gateway, repayment_service, "Routes repayment requests", "HTTP")
    Rel(loan_service, db, "Stores loans", "JDBC")
    Rel(repayment_service, db, "Stores repayments", "JDBC")

    UpdateElementStyle(loan_service, $fontColor="white", $bgColor="green", $borderColor="darkgreen")
    UpdateElementStyle(repayment_service, $fontColor="white", $bgColor="green", $borderColor="darkgreen")
```

**Component Diagram for Loan Service** (Separate diagram):

```mermaid
C4Component
    title Component Diagram - Loan Service

    Container_Boundary(loan_service, "Loan Service") {
        Component(loan_controller, "Loan Controller", "Spring MVC", "Handles loan requests")
        Component(eligibility_validator, "Eligibility Validator", "Spring Component", "Validates borrower eligibility")
        Component(loan_manager, "Loan Manager", "Spring Service", "Core loan business logic")
        Component(loan_repository, "Loan Repository", "Spring Data JPA", "Data access")
    }

    ContainerDb(db, "Loan Database", "PostgreSQL")

    Rel(loan_controller, eligibility_validator, "Validates eligibility", "Method call")
    Rel(loan_controller, loan_manager, "Processes loan", "Method call")
    Rel(loan_manager, loan_repository, "Stores loan", "Method call")
    Rel(loan_repository, db, "Reads/Writes", "JDBC")

    UpdateElementStyle(loan_manager, $fontColor="white", $bgColor="green", $borderColor="darkgreen")
```

### 2. Adding Arbitrary Abstraction Levels

**Problem**: Creating custom abstraction levels between the standard four C4 levels (e.g., "Service", "Module", "Subsystem").

**Why It's Wrong**:

- Defeats the purpose of a standard model
- Creates confusion about what each diagram shows
- Makes it harder to onboard new team members
- Reduces interoperability with C4 tooling

**Common Violations**:

- "Service Layer" diagram (between Container and Component)
- "Module" diagram (between Component and Code)
- "Microservice Group" diagram (between System Context and Container)

**Example - Incorrect Custom Level**:

```
❌ "Service Layer" Diagram (NOT a standard C4 level)

This diagram shows "services" which are neither Containers nor Components
```

**Correct Approach**:

Use standard C4 levels with clear boundaries:

1. If "services" are deployable units → Use Container diagram
2. If "services" are code groupings within a container → Use Component diagram
3. If you need both views → Create both Container and Component diagrams

### 3. Overusing Subsystems

**Problem**: Creating "subsystem" groupings that don't represent actual deployment or logical boundaries.

**Why It's Wrong**:

- Adds visual complexity without adding clarity
- Often represents organizational structure rather than system structure
- Can hide important direct relationships
- Makes diagrams harder to understand

**Example - Incorrect Subsystem Usage**:

```mermaid
C4Container
    title Container Diagram - Wakaf Management Platform (Incorrect - Overusing Subsystems)

    Person(donor, "Donor")

    System_Boundary(wakaf, "Wakaf Management Platform") {
        Container_Boundary(frontend_subsystem, "Frontend Subsystem") {
            Container(web, "Web App", "React")
            Container(mobile, "Mobile App", "React Native")
        }

        Container_Boundary(backend_subsystem, "Backend Subsystem") {
            Container(api, "API Gateway", "Kong")
            Container(wakaf_service, "Wakaf Service", "Spring Boot")
            Container(beneficiary_service, "Beneficiary Service", "Spring Boot")
        }

        Container_Boundary(data_subsystem, "Data Subsystem") {
            Container(db, "Database", "PostgreSQL")
            Container(cache, "Cache", "Redis")
        }
    }

    Rel(donor, web, "Donates Wakaf", "HTTPS")
    Rel(web, api, "Makes API calls", "REST")
```

**Problem**: The "subsystems" add no value - they just group similar container types.

**Correct Approach**:

```mermaid
C4Container
    title Container Diagram - Wakaf Management Platform (Correct - No Unnecessary Subsystems)

    Person(donor, "Wakaf Donor", "Donates to endowment projects")
    Person(beneficiary, "Beneficiary", "Receives Wakaf benefits")

    System_Boundary(wakaf, "Wakaf Management Platform") {
        Container(web, "Web Application", "React", "Donor and beneficiary interface")
        Container(mobile, "Mobile App", "React Native", "Mobile donor interface")
        Container(api, "API Gateway", "Kong", "Routes and secures requests")
        Container(wakaf_service, "Wakaf Service", "Spring Boot", "Manages Wakaf endowments and distributions")
        Container(beneficiary_service, "Beneficiary Service", "Spring Boot", "Manages beneficiary verification")
        Container(db, "Wakaf Database", "PostgreSQL", "Stores endowment and beneficiary data")
        Container(cache, "Cache", "Redis", "Caches frequently accessed data")
    }

    System_Ext(property_registry, "Property Registry", "Verifies Wakaf property ownership")

    Rel(donor, web, "Donates Wakaf", "HTTPS")
    Rel(donor, mobile, "Donates Wakaf", "HTTPS")
    Rel(beneficiary, web, "Applies for benefits", "HTTPS")
    Rel(web, api, "Makes API calls", "REST/JSON")
    Rel(mobile, api, "Makes API calls", "REST/JSON")
    Rel(api, wakaf_service, "Routes requests", "HTTP")
    Rel(api, beneficiary_service, "Routes requests", "HTTP")
    Rel(wakaf_service, db, "Stores endowments", "JDBC")
    Rel(wakaf_service, cache, "Caches data", "Redis Protocol")
    Rel(beneficiary_service, db, "Stores beneficiaries", "JDBC")
    Rel(wakaf_service, property_registry, "Verifies property", "REST API")

    UpdateElementStyle(wakaf_service, $fontColor="white", $bgColor="green", $borderColor="darkgreen")
    UpdateElementStyle(beneficiary_service, $fontColor="white", $bgColor="green", $borderColor="darkgreen")
```

**When Subsystems ARE Appropriate**:

Subsystems make sense when they represent actual logical or physical boundaries:

```mermaid
C4Container
    title Container Diagram - Multi-Tenant Islamic Banking Platform (Correct Subsystem Use)

    Person(customer_a, "Customer (Tenant A)")
    Person(customer_b, "Customer (Tenant B)")

    System_Boundary(platform, "Islamic Banking Platform") {
        Container(router, "Tenant Router", "Nginx", "Routes requests to correct tenant")

        Container_Boundary(tenant_a, "Tenant A Infrastructure") {
            Container(web_a, "Web App A", "React", "Tenant A customer portal")
            Container(api_a, "API A", "Spring Boot", "Tenant A business logic")
            Container(db_a, "Database A", "PostgreSQL", "Tenant A data")
        }

        Container_Boundary(tenant_b, "Tenant B Infrastructure") {
            Container(web_b, "Web App B", "React", "Tenant B customer portal")
            Container(api_b, "API B", "Spring Boot", "Tenant B business logic")
            Container(db_b, "Database B", "PostgreSQL", "Tenant B data")
        }
    }

    Rel(customer_a, router, "Uses banking services", "HTTPS")
    Rel(customer_b, router, "Uses banking services", "HTTPS")
    Rel(router, web_a, "Routes Tenant A requests", "HTTPS")
    Rel(router, web_b, "Routes Tenant B requests", "HTTPS")
    Rel(web_a, api_a, "Makes API calls", "REST")
    Rel(web_b, api_b, "Makes API calls", "REST")
    Rel(api_a, db_a, "Stores data", "JDBC")
    Rel(api_b, db_b, "Stores data", "JDBC")

    UpdateElementStyle(api_a, $fontColor="white", $bgColor="green", $borderColor="darkgreen")
    UpdateElementStyle(api_b, $fontColor="white", $bgColor="green", $borderColor="darkgreen")
```

Here, subsystems represent actual deployment isolation between tenants.

### 4. Detailing External Containers

**Problem**: Showing internal implementation details of external systems that you don't own or control.

**Why It's Wrong**:

- Violates abstraction boundaries (external systems should be black boxes)
- Creates maintenance burden when external systems change
- Adds clutter without adding value
- Confuses ownership and responsibility

**Example - Incorrect External System Detail**:

```
❌ System Context (WRONG - Shows internal details of external system)

[Takaful Platform] ---> [Payment Gateway Web Server]
                   ---> [Payment Gateway Database]
                   ---> [Payment Gateway Message Queue]

⚠️ Shows internal containers of external "Payment Gateway" system
```

**Correct Approach**:

```mermaid
C4Context
    title System Context - Takaful Management Platform (Correct - External Systems as Black Boxes)

    Person(policyholder, "Policyholder", "Purchases and manages Takaful policies")
    Person(operator, "Takaful Operator", "Manages risk pool and claims")

    System(takaful, "Takaful Management Platform", "Manages cooperative insurance policies and claims")

    System_Ext(payment, "Payment Gateway", "Processes premium payments and claim disbursements")
    System_Ext(actuarial, "Actuarial System", "Provides risk calculations and pricing")
    System_Ext(reinsurance, "Retakaful Platform", "Manages reinsurance arrangements")
    System_Ext(regulatory, "Regulatory Reporting System", "Receives compliance reports")

    Rel(policyholder, takaful, "Purchases policies, Files claims", "HTTPS")
    Rel(operator, takaful, "Manages risk pool, Approves claims", "HTTPS")
    Rel(takaful, payment, "Processes payments", "REST API")
    Rel(takaful, actuarial, "Calculates premiums", "REST API")
    Rel(takaful, reinsurance, "Arranges Retakaful", "REST API")
    Rel(takaful, regulatory, "Submits reports", "SFTP")

    UpdateElementStyle(takaful, $fontColor="white", $bgColor="green", $borderColor="darkgreen")
```

**When to Show External System Details**:

Only show external system internals if:

1. You own/control the external system (e.g., legacy system you're integrating)
2. You're documenting integration architecture and need to show specific integration points
3. You're creating a Container diagram that zooms into a previously external system

### 5. Ambiguous Diagrams Without Legends or Labels

**Problem**: Creating diagrams without sufficient context, labels, or explanations.

**Why It's Wrong**:

- Requires verbal explanation (diagrams should be self-explanatory)
- Different stakeholders interpret diagrams differently
- Diagrams lose value when reviewed later
- Wastes time in meetings explaining unclear elements

**Example - Ambiguous Diagram**:

```
❌ Ambiguous Container Diagram (Missing context)

[Web] ---> [API] ---> [DB]
 |
 v
[Queue]

⚠️ No title, no technology choices, no relationship labels
⚠️ Unclear what "API" does or why Queue is needed
```

**Correct Approach**:

```mermaid
C4Container
    title Container Diagram for Sadaqah Donation Platform

    Person(donor, "Donor", "Makes charitable donations")
    Person(charity_admin, "Charity Administrator", "Manages donation campaigns and beneficiaries")

    System_Boundary(sadaqah, "Sadaqah Donation Platform") {
        Container(web, "Donor Portal", "React, TypeScript", "Allows donors to browse campaigns and make donations")
        Container(admin_web, "Admin Portal", "React, TypeScript", "Campaign and beneficiary management interface")
        Container(api, "Donation API", "Node.js, Express", "Handles donation processing and campaign management")
        Container(notification_worker, "Notification Worker", "Node.js", "Sends donation receipts and campaign updates")
        Container(db, "Donation Database", "PostgreSQL", "Stores campaigns, donations, and donor data")
        Container(queue, "Message Queue", "RabbitMQ", "Asynchronous notification processing")
        Container(storage, "Receipt Storage", "AWS S3", "Stores PDF donation receipts")
    }

    System_Ext(payment, "Payment Gateway", "Stripe API for payment processing")
    System_Ext(email, "Email Service", "SendGrid for email notifications")

    Rel(donor, web, "Browses campaigns, Makes donations", "HTTPS")
    Rel(charity_admin, admin_web, "Creates campaigns, Manages beneficiaries", "HTTPS")
    Rel(web, api, "Submits donations", "REST API/JSON")
    Rel(admin_web, api, "Manages campaigns", "REST API/JSON")
    Rel(api, db, "Stores donations and campaigns", "SQL/JDBC")
    Rel(api, payment, "Processes payments", "REST API")
    Rel(api, queue, "Publishes notification events", "AMQP")
    Rel(notification_worker, queue, "Consumes notification events", "AMQP")
    Rel(notification_worker, db, "Reads donation details", "SQL/JDBC")
    Rel(notification_worker, storage, "Stores receipt PDFs", "AWS SDK")
    Rel(notification_worker, email, "Sends emails", "SMTP API")

    UpdateElementStyle(api, $fontColor="white", $bgColor="green", $borderColor="darkgreen")
    UpdateElementStyle(notification_worker, $fontColor="white", $bgColor="green", $borderColor="darkgreen")
```

**Legend** (when colors/symbols have specific meaning):

```markdown
### Diagram Legend

**Colors**:

- Green: Core platform services
- Blue: User-facing applications
- Orange: Data stores
- Gray: External systems

**Relationships**:

- Solid lines: Synchronous calls
- Dashed lines: Asynchronous messaging

**Technologies**:

- All technology choices shown in square brackets [Technology Name]
```

### 6. Using C4 for Wrong System Types

**Problem**: Applying C4 model to systems where it doesn't fit well.

**Why It's Wrong**:

- C4 is designed for software systems with runtime deployment
- Some systems don't have clear container/component abstractions
- Forces artificial modeling that doesn't reflect reality

**Inappropriate Use Cases**:

1. **Embedded Systems**: Firmware with no clear deployment separation
2. **Libraries/SDKs**: Reusable code without independent deployment
3. **Infrastructure as Code**: Terraform/CloudFormation templates
4. **Data Pipelines**: ETL workflows (better suited for flow diagrams)
5. **Mobile Apps (Simple)**: Single deployment unit with no backend

**Example - Inappropriate Use**:

```
❌ C4 Container Diagram for Simple Mobile App (WRONG)

Container: Mobile App [React Native]
Container: Local Database [SQLite]
Container: User Interface Layer [React Components]
Container: Business Logic Layer [JavaScript Classes]

⚠️ These aren't separate deployable containers - they're all part of one mobile app
⚠️ Better to skip Container diagram or use mobile app architecture patterns (MVVM, MVC)
```

**When C4 IS Appropriate for Mobile**:

When the mobile app is part of a larger distributed system:

```mermaid
C4Container
    title Container Diagram - Islamic Finance Advisory Platform (Correct - Mobile as One Container)

    Person(user, "User", "Seeks Sharia-compliant financial advice")

    System_Boundary(advisory, "Islamic Finance Advisory Platform") {
        Container(ios_app, "iOS App", "Swift, SwiftUI", "Native iOS client for financial advice")
        Container(android_app, "Android App", "Kotlin, Jetpack Compose", "Native Android client")
        Container(api, "Advisory API", "Spring Boot", "Provides personalized financial advice")
        Container(ml_service, "Recommendation Engine", "Python, TensorFlow", "Machine learning-based advice recommendations")
        Container(db, "User Database", "PostgreSQL", "Stores user profiles and advice history")
    }

    System_Ext(fatwa_db, "Fatwa Database", "External Sharia rulings database")

    Rel(user, ios_app, "Seeks advice", "Touch/Gestures")
    Rel(user, android_app, "Seeks advice", "Touch/Gestures")
    Rel(ios_app, api, "Requests advice", "REST API/HTTPS")
    Rel(android_app, api, "Requests advice", "REST API/HTTPS")
    Rel(api, ml_service, "Generates recommendations", "gRPC")
    Rel(api, db, "Stores user data", "JDBC")
    Rel(api, fatwa_db, "Retrieves Sharia rulings", "REST API")
    Rel(ml_service, db, "Reads historical data", "JDBC")

    UpdateElementStyle(api, $fontColor="white", $bgColor="green", $borderColor="darkgreen")
    UpdateElementStyle(ml_service, $fontColor="white", $bgColor="orange", $borderColor="darkorange")
```

Here, mobile apps are legitimate containers in a distributed system.

**Better Alternatives for Inappropriate Cases**:

- **Embedded Systems**: Hardware architecture diagrams, state machines
- **Libraries**: Package diagrams, dependency graphs
- **IaC**: Resource topology diagrams, deployment diagrams
- **Data Pipelines**: Data flow diagrams, workflow diagrams
- **Simple Mobile Apps**: MVVM/MVC architecture diagrams

## Summary of Antipatterns

| Antipattern                    | Problem                                        | Solution                                     |
| ------------------------------ | ---------------------------------------------- | -------------------------------------------- |
| **Mixing Abstractions**        | Containers and Components in same diagram      | Create separate diagrams for each level      |
| **Arbitrary Levels**           | Custom levels between standard C4 levels       | Stick to four standard levels                |
| **Overusing Subsystems**       | Groupings that don't represent real boundaries | Use subsystems only for actual isolation     |
| **Detailing External Systems** | Showing internals of systems you don't own     | Keep external systems as black boxes         |
| **Ambiguous Diagrams**         | Missing labels, legends, context               | Add sufficient descriptive text              |
| **Wrong System Types**         | Using C4 for embedded systems, libraries, IaC  | Use C4 only for distributed software systems |

## Practical Avoidance Checklist

Before finalizing a C4 diagram, verify you're NOT doing these:

**Abstraction Level Violations**:

- [ ] ❌ Mixing containers and components in same diagram
- [ ] ❌ Creating custom levels between standard C4 levels
- [ ] ❌ Showing internal details of external systems
- [ ] ❌ Using subsystems that don't represent real boundaries

**Clarity Violations**:

- [ ] ❌ Missing diagram title
- [ ] ❌ Missing technology choices in square brackets
- [ ] ❌ Unlabeled relationships
- [ ] ❌ Ambiguous element names (e.g., "API", "Service", "Handler")
- [ ] ❌ No legend when using colors/symbols with specific meaning

**Scope Violations**:

- [ ] ❌ Too many elements (>15 in Context, >20 in Container)
- [ ] ❌ Too much detail for the abstraction level
- [ ] ❌ Irrelevant elements that don't serve the diagram's purpose

**Consistency Violations**:

- [ ] ❌ Inconsistent naming across diagram levels
- [ ] ❌ Mixing terminology (technical jargon with business terms)
- [ ] ❌ Inconsistent technology naming
- [ ] ❌ Different visual styles within same documentation set

**Inappropriate Use**:

- [ ] ❌ Using C4 for embedded systems
- [ ] ❌ Using C4 for libraries/SDKs
- [ ] ❌ Using C4 for simple mobile apps (no backend)
- [ ] ❌ Using C4 for data pipelines (use flow diagrams instead)
- [ ] ❌ Using C4 for infrastructure as code (use topology diagrams)

## Further Reading

**Official C4 Resources**:

- C4 Model Website: <https://c4model.com/>
- C4 Model FAQ: <https://c4model.com/#faq>
- Structurizr (C4 tooling): <https://structurizr.com/>

**Community Resources**:

- "Misuses and Mistakes of the C4 Model": <https://www.workingsoftware.dev/misuses-and-mistakes-of-the-c4-model/>
- "The C4 Model for Visualising Software Architecture" (InfoQ): <https://www.infoq.com/articles/C4-architecture-model/>

**Related Documentation**:

- Best Practices: [ex-so-ar-c4armo\_\_14-best-practices.md](./ex-so-ar-c4armo__14-best-practices.md)
- System Context Diagrams: [ex-so-ar-c4armo\_\_01-level-1-system-context.md](./ex-so-ar-c4armo__01-level-1-system-context.md)
- Container Diagrams: [ex-so-ar-c4armo\_\_02-level-2-container.md](./ex-so-ar-c4armo__02-level-2-container.md)
- Component Diagrams: [ex-so-ar-c4armo\_\_03-level-3-component.md](./ex-so-ar-c4armo__03-level-3-component.md)

## Conclusion

Avoiding these antipatterns is crucial for maintaining the value and clarity that the C4 model provides. Remember:

- **Respect abstraction levels** - Don't mix containers and components
- **Keep it standard** - Stick to the four C4 levels
- **Make it clear** - Add titles, labels, legends, and descriptions
- **Know when to use C4** - Only for distributed software systems
- **External systems are black boxes** - Don't show what you don't own

When you catch yourself doing any of these antipatterns, step back and ask: **Am I adding clarity or confusion?** If the answer is confusion, simplify and refocus on the core C4 principles. For positive guidance, refer to the [Best Practices](./ex-so-ar-c4armo__14-best-practices.md) document.
