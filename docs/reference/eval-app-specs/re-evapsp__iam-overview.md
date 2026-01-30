# IAM System Overview

## What is IAM?

Identity and Access Management (IAM) system for authentication, authorization, session management, and audit trails. This mini app specification evaluates programming languages and frameworks through foundational security infrastructure.

## Why IAM?

IAM was selected as the initial mini app specification after evaluating six fintech/banking/enterprise alternatives (Digital Wallet, Loan Origination, Payment Authorization, Expense Approval, Fund Transfer, Account Opening/KYC).

**Key reasons**:

1. **Universal Applicability**: Every application needs authentication and authorization
2. **Infrastructure Reusability**: Can serve as the auth layer for other mini apps
3. **Security-First Design**: Naturally exercises CIA + non-repudiation principles
4. **Critical Scalability**: Auth happens on every request, exposing caching and performance patterns
5. **Complete Feature Set**: Authentication, authorization, session management, audit trails, MFA
6. **Language Differentiators**: Reveals cryptography, token management, and security primitive handling
7. **Demo-Sized Scope**: Comprehensive but achievable
8. **Foundation for Growth**: Future mini apps can authenticate against this IAM

## Core Features

**User Management**

- User lifecycle states (invited → pending → active → suspended → locked → deactivated)
- Role-based access control (RBAC)
- Permission system (resource + action + scope)

**Authentication**

- Username/password authentication
- Multi-factor authentication (MFA/TOTP)
- Session management with token refresh
- Password reset workflows
- Account lockout after failed attempts

**Authorization**

- Role assignment and inheritance
- Permission checks (field-level and endpoint-level)
- API key management for service accounts

**Security & Compliance**

- Comprehensive audit logging (all state changes, user actions)
- Security event tracking (failed logins, suspicious activity)
- CIA principles (Confidentiality, Integrity, Availability)
- Non-repudiation (immutable audit trails, digital signatures)

## Technology Requirements

**API Layer**

- REST API (OpenAPI/Swagger documentation)
- GraphQL API (schema, resolvers, playground)

**Database**

- PostgreSQL with proper indexing
- Migration system
- Transaction support

**Security**

- Password hashing (Bcrypt/Argon2)
- JWT token signing
- Rate limiting
- CSRF protection
- Input validation

**Testing**

- Unit tests (business logic, FSM transitions)
- Integration tests (database, service layer)
- API tests (REST and GraphQL endpoints)
- Security tests (injection prevention, auth bypass)

## Planned Implementations

Three language/framework combinations will evaluate this specification:

**1. Java + Spring Boot** (`apps-labs/mini-apps-java-springboot/`)

- Focus: Spring Security, JPA/Hibernate, Bean Validation, JCA/JCE
- Expected strengths: Mature ecosystem, strong type safety
- Expected challenges: Boilerplate, verbose configuration

**2. Elixir + Phoenix** (`apps-labs/mini-apps-elixir-phoenix/`)

- Focus: Guardian, Ecto, Changesets, OTP, GenServer
- Expected strengths: Concurrency model, pattern matching, fault tolerance
- Expected challenges: OTP learning curve, smaller ecosystem

**3. Go + Standard Library** (`apps-labs/mini-apps-go-raw/`)

- Focus: net/http, crypto package, database/sql, goroutines
- Expected strengths: Minimal dependencies, fast startup, excellent stdlib
- Expected challenges: Manual implementation, middleware boilerplate

## Evaluation Focus

Each implementation will be assessed on:

- Developer experience (setup, code organization, tooling)
- Implementation quality (clarity, boilerplate, abstractions)
- Performance (startup time, latency, throughput)
- Security implementation (built-in features, library quality)
- Ecosystem maturity (packages, deployment, community)

## Success Criteria

An implementation is complete when:

- All API endpoints function correctly (REST and GraphQL)
- All FSM state transitions are implemented and validated
- All CIA + non-repudiation principles are demonstrated
- Database schema matches specification
- Test coverage exceeds 80%
- Documentation is complete
- Application runs without errors

## Related Documentation

- [Mini Apps Index](./README.md) - Comparison matrix and requirements framework
- Detailed specifications will be added as separate documents for API, database schema, FSM, and security requirements
