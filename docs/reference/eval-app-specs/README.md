# Evaluation App Specifications

## Overview

This directory contains specifications for evaluation applications designed to assess programming languages and frameworks through practical implementation. Each specification defines standardized requirements that exercise core backend capabilities while remaining small enough for demonstration purposes.

Evaluation app specifications provide:

- **Consistent Benchmarks**: Compare languages/frameworks on equal footing
- **Real-World Scenarios**: Fintech, banking, and enterprise domains with actual scalability challenges
- **Comprehensive Coverage**: API design, databases, state machines, security, audit trails
- **Practical Learning**: Hands-on evaluation of ergonomics and ecosystem maturity

## Current Specification: IAM System

### [IAM System](re-evapsp__iam-overview.md)

Identity and Access Management system with authentication, authorization, and audit trails.

**Key Features**:

- User lifecycle FSM (invited → active → suspended → locked → deactivated)
- Session management with token refresh
- Role-based access control (RBAC)
- API key management
- MFA support
- Comprehensive audit logging

**Why This Domain**: IAM is foundational infrastructure for all applications. It naturally exercises security principles (CIA + non-repudiation) and has critical scalability requirements (auth on every request).

**Metrics**: Medium Complexity | FSM Depth ⭐⭐⭐⭐ | Security Focus ⭐⭐⭐⭐⭐

See the [IAM overview](re-evapsp__iam-overview.md) for complete details on features, technology requirements, and planned implementations.

## Planned Implementations

The following language and framework combinations will be implemented:

```
apps-labs/
├── mini-apps-java-springboot/      # Java + Spring Boot
├── mini-apps-elixir-phoenix/       # Elixir + Phoenix
└── mini-apps-go-raw/               # Go + stdlib (no framework)
```

**Naming Pattern**: `mini-apps-{prog-lang}-{framework}`

**Note**: `go-raw` uses Go standard library without a web framework to evaluate language ergonomics and built-in capabilities.

See [IAM Overview - Planned Implementations](re-evapsp__iam-overview.md#planned-implementations) for detailed evaluation focus, expected strengths, and challenges for each implementation.

## Requirements Framework

All mini app specifications follow a consistent framework for evaluation.

### Common Requirements

Every specification must include:

**1. API Layer**

- RESTful HTTP endpoints
- GraphQL API (schema, resolvers, queries, mutations)
- Request/response validation (REST and GraphQL)
- Error handling with proper HTTP status codes (REST) and GraphQL errors
- API documentation (OpenAPI/Swagger for REST, GraphQL schema/playground for GraphQL)

**2. Data Persistence**

- PostgreSQL database
- Connection pooling and transaction support
- Migration system for schema versioning
- Proper indexing and foreign key constraints

**3. Finite State Machine**

- Explicit state machine implementation (not just status field updates)
- Validation of state transitions
- Side effects on transitions (notifications, updates)
- State history tracking in audit logs
- Idempotency for transition operations

**4. Security: CIA + Non-Repudiation**

- **Confidentiality**: Authentication, authorization, encryption at rest/transit
- **Integrity**: Input validation, SQL injection prevention, audit logging
- **Availability**: Error handling, rate limiting, health checks, connection pooling
- **Non-Repudiation**: Immutable audit trails, digital signatures, timestamped actions

**5. Audit Trail**

- Comprehensive logging of all state changes
- User/IP/timestamp tracking for all actions
- Immutable audit log design
- Query interface for compliance reporting

**6. Testing**

- Unit tests (business logic, FSM transitions, utility functions)
- Integration tests (database transactions, service layer interactions, external dependencies)
- API tests (HTTP endpoints using native language/framework testing tools)
- Security tests (injection prevention, auth bypass attempts, authorization boundaries)
- Minimum 70% code coverage

**Note**: API tests must use the language/framework's native testing capabilities (e.g., Supertest for Node.js, MockMvc for Spring Boot, Phoenix test helpers for Elixir, httptest for Go).

### Evaluation Criteria

Assess each implementation across five dimensions:

**Developer Experience**

- Setup complexity and onboarding
- Code organization and natural patterns
- Type safety (compile-time vs runtime errors)
- IDE support and debugging tools
- Framework documentation quality

**Implementation Quality**

- Code clarity and maintainability
- Boilerplate requirements
- Natural abstractions for common patterns
- Error handling ergonomics
- Testing ease and tooling

**Performance Characteristics**

- Application startup time
- Request latency (p50, p95, p99)
- Throughput (requests/second)
- Memory footprint
- Build time

**Security Implementation**

- Built-in framework security features
- Third-party security library quality
- Vulnerability management ecosystem
- Alignment with security best practices

**Ecosystem Maturity**

- Package management quality
- Database library ecosystem
- Deployment options (containers, cloud platforms)
- Community support and activity
- Long-term viability and adoption trends

### Success Criteria

An implementation is complete when:

- All API endpoints function correctly
- All FSM state transitions are implemented and validated
- All CIA + non-repudiation principles are demonstrated
- Database schema matches specification
- Test coverage exceeds 70%
- Documentation is complete and accurate
- Application runs without errors in local development environment

## Selection Rationale

### Why IAM Was Selected

After evaluating six fintech/banking/enterprise alternatives, **IAM System** was chosen as the initial specification because:

1. **Universal Applicability**: Every application needs authentication and authorization
2. **Infrastructure Reusability**: Can serve as the auth layer for implementing other mini apps (wallet, loans, payments)
3. **Security-First Design**: Naturally exercises all CIA + non-repudiation principles without forcing them
4. **Critical Scalability**: Auth happens on every request, demonstrating caching and performance patterns
5. **Complete Feature Set**: Covers authentication, authorization, session management, audit trails, and MFA
6. **Language Differentiators**: Reveals how languages handle cryptography, token management, and security primitives
7. **Demo-Sized Scope**: Comprehensive enough to be meaningful, focused enough to complete
8. **Foundation for Growth**: Future mini apps can authenticate against this IAM instead of reimplementing auth

IAM provides the best balance of complexity, reusability, and learning value for evaluating programming languages and frameworks.

### Considered Alternatives

Before selecting IAM, the following fintech/banking/enterprise domains were evaluated:

**Digital Wallet System**

- **Domain**: Fintech e-wallet (peer-to-peer transfers, deposits, withdrawals)
- **Key Features**: Wallet state FSM, transaction state FSM, double-entry bookkeeping, balance caching
- **Strengths**: High transaction volume, clear business logic, real-world scalability challenges
- **Why Not Selected**: While excellent for business logic evaluation, IAM is more universally applicable and can serve as authentication infrastructure for wallet implementations.

**Loan Origination System**

- **Domain**: Bank loan application processing
- **Key Features**: Multi-stage approval workflow, document management, credit check integration, disbursement tracking
- **Strengths**: Complex approval FSM, document handling, policy engine, compliance requirements
- **Why Not Selected**: More complex than needed for initial evaluation, requires external service mocking (credit bureaus), less universally reusable.

**Payment Authorization System**

- **Domain**: Simplified payment gateway (authorize/capture flow)
- **Key Features**: Payment lifecycle FSM, merchant management, tokenization, idempotency, settlement
- **Strengths**: High throughput requirements, critical security needs, demonstrates async processing
- **Why Not Selected**: Overlaps significantly with wallet system, more domain-specific, requires card network simulation.

**Multi-Level Expense Approval System**

- **Domain**: Enterprise expense claim processing
- **Key Features**: Multi-level approval chain, receipt management, policy compliance, reimbursement tracking
- **Strengths**: Clear workflow FSM, document handling, approval patterns, reporting
- **Why Not Selected**: More enterprise-focused than fintech, less critical scalability requirements, simpler security needs.

**Fund Transfer System**

- **Domain**: Interbank-style fund transfers
- **Key Features**: Double-entry bookkeeping, transfer FSM with rollback, clearing/settlement, eventual consistency
- **Strengths**: Complex state machine, demonstrates distributed transactions, high volume
- **Why Not Selected**: Requires understanding of banking domain concepts, more complex than needed, overlaps with wallet functionality.

**Account Opening System (KYC)**

- **Domain**: Bank account onboarding with compliance
- **Key Features**: KYC document verification, identity checks, compliance workflow, consent management
- **Strengths**: Regulatory compliance demonstration, document encryption, verification workflow
- **Why Not Selected**: Requires external KYC service integration, more specialized domain knowledge, less reusable as infrastructure.

## Implementation Guide

### Location and Naming

All implementations reside in the `apps-labs/` directory (outside the Nx monorepo):

```
apps-labs/
├── README.md
├── mini-apps-java-springboot/      # Java + Spring Boot IAM
├── mini-apps-elixir-phoenix/       # Elixir + Phoenix IAM
└── mini-apps-go-raw/               # Go + stdlib IAM
```

**Naming Pattern**: `mini-apps-{prog-lang}-{framework}`

### Required Files

Each implementation must include:

**Documentation**

- `README.md` - Setup instructions, architecture overview, evaluation notes
- `API.md` - API documentation or OpenAPI spec
- `EVALUATION.md` - Assessment against evaluation criteria

**Configuration**

- Database migration files
- Environment configuration examples (`.env.example`)
- Docker setup (optional but recommended)

**Code Quality**

- Proper error handling and logging
- Security best practices
- Clean code organization
- Comprehensive testing

### Implementation Notes

- Implementations are exploratory and not production-ready
- Focus on ergonomics and patterns, not premature optimization
- Each implementation is independent and self-contained
- Cross-reference the specification in implementation README files

## Adding New Specifications

When creating additional mini app specifications:

1. Choose a fintech/banking/enterprise domain
2. Ensure it exercises all core requirements (API, DB, FSM, Security, Audit)
3. Define clear, non-trivial business flows requiring state machines
4. Include real-world scalability considerations
5. Keep scope demo-sized
6. Follow the naming pattern: `re-eas__[domain-name].md`
7. Update this README with the new specification
8. Document selection rationale and alternatives considered

## Related Documentation

- [Core Principles](./README.md) - Foundational values guiding all development
- [Development Practices](./README.md) - Software practices and standards
- [Monorepo Structure](../re__monorepo-structure.md) - Understanding the repository organization
