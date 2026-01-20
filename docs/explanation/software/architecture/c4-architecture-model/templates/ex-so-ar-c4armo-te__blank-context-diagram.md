---
title: "[Your System Name] - System Context Diagram"
description: Shows how [Your System Name] fits into the world with users and external systems
category: explanation
subcategory: architecture
tags:
  - c4-model
  - context-diagram
  - level-1
  - your-system-name
created: YYYY-MM-DD
updated: YYYY-MM-DD
---

# [Your System Name] - System Context Diagram

## Purpose

This diagram shows the big picture of how **[Your System Name]** fits into the world around it. It identifies:

- The software system in scope
- The people (users/actors/roles) who use it
- The other software systems it interacts with

**Audience**: Non-technical stakeholders, product owners, executives, new team members.

## Diagram

```mermaid
graph TD
    %% Define people (actors/users)
    User1[User Role 1<br/>Person<br/>Description of user role]
    User2[User Role 2<br/>Person<br/>Description of user role]

    %% Define your system (the system in scope)
    YourSystem[[Your System Name]<br/>Software System<br/>Brief description of what your system does]

    %% Define external systems
    ExternalSystem1[External System 1<br/>External Software System<br/>Brief description]
    ExternalSystem2[External System 2<br/>External Software System<br/>Brief description]

    %% Define relationships
    User1 -->|Describe interaction<br/>e.g. Views reports, Manages data| YourSystem
    User2 -->|Describe interaction<br/>e.g. Configures settings| YourSystem

    YourSystem -->|Describe interaction<br/>e.g. Sends notifications via<br/>HTTPS/REST API| ExternalSystem1
    YourSystem -->|Describe interaction<br/>e.g. Retrieves user data via<br/>HTTPS/GraphQL API| ExternalSystem2

    %% Apply WCAG-compliant colors
    style User1 fill:#029E73,stroke:#000000,color:#ffffff
    style User2 fill:#029E73,stroke:#000000,color:#ffffff
    style YourSystem fill:#0173B2,stroke:#000000,color:#ffffff
    style ExternalSystem1 fill:#CC78BC,stroke:#000000,color:#ffffff
    style ExternalSystem2 fill:#CC78BC,stroke:#000000,color:#ffffff
```

## Legend

| Element             | Color  | Description                                          |
| ------------------- | ------ | ---------------------------------------------------- |
| **Person**          | Teal   | Users, actors, or roles (internal or external)       |
| **Your System**     | Blue   | The software system you are documenting              |
| **External System** | Purple | External software systems (third-party, other teams) |

## System Scope

**[Your System Name]** is responsible for:

- [Responsibility 1: e.g., Managing customer orders]
- [Responsibility 2: e.g., Processing payments]
- [Responsibility 3: e.g., Generating reports]

**Out of Scope**:

- [What this system does NOT do: e.g., User authentication (handled by Auth Service)]
- [What this system does NOT do: e.g., Email delivery (handled by Email Service)]

## Key Relationships

### People

- **[User Role 1]**: [Description of who they are and what they do with the system]
- **[User Role 2]**: [Description of who they are and what they do with the system]

### External Systems

- **[External System 1]**: [What it does and why your system integrates with it]
- **[External System 2]**: [What it does and why your system integrates with it]

## Notes

- Add any important notes about system boundaries, compliance requirements, or architectural decisions
- Mention if this is a new system, a replacement for legacy, or an enhancement

## Related Diagrams

- **[Container Diagram](./ex-so-ar-c4armo-te__blank-container-diagram.md)**: Shows the high-level technical building blocks of [Your System Name]
- **[Deployment Diagram](./ex-so-ar-c4armo-te__blank-deployment-diagram.md)**: Shows how [Your System Name] is deployed to infrastructure

---

**Template Instructions**:

1. Replace `[Your System Name]` with your actual system name
2. Replace `User Role 1`, `User Role 2` with actual user roles (e.g., "Customer", "Administrator")
3. Replace `External System 1`, `External System 2` with actual external systems (e.g., "Payment Gateway", "Email Service")
4. Update relationship labels to describe HOW systems communicate (include protocol/method)
5. Fill in "System Scope", "Key Relationships", and "Notes" sections
6. Update frontmatter (title, description, tags, created/updated dates)
7. Delete this template instructions section when done

**See**: [Level 1: System Context](../ex-so-ar-c4armo__01-level-1-system-context.md) for detailed guidance.
