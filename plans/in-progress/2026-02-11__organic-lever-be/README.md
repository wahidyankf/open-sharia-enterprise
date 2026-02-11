# Organic Lever Backend Initialization

**Status**: In Progress
**Created**: 2026-02-11

## Overview

Initialize a Java Spring Boot application using Maven in the `apps/` folder named "organic-lever-be". The application will serve as a backend service providing REST API endpoints. The name "organic-lever-be" (organic lever backend) represents a foundational service that can be leveraged to build more complex features organically over time.

**Location**: `apps/organic-lever-be/`

**Delivery Type**: Single commit

**Technology Stack**:

- Java 25 (LTS)
- Spring Boot 4.0.x (Jakarta EE 10 - uses jakarta.* namespace, not javax.*)
- Maven (build tool)
- Port: 8100

## Problem Statement

The open-sharia-enterprise platform needs a lightweight backend service that can serve as a foundation for API development. This application will:

- Provide a simple REST API endpoint for validation purposes
- Demonstrate proper Spring Boot project setup
- Serve as a template for future backend services
- Enable testing of the Nx monorepo integration with Java/Maven

This plan establishes the foundational backend infrastructure with a minimal but production-ready setup.

## Prerequisites

**Development Environment**:

- Java 25 (recommended LTS) or Java 17+ (minimum required by Spring Boot 4.0)
- Maven 3.8+
- Node.js 24.11.1 (for Nx monorepo)
- npm 11.6.3

**Repository Setup**:

- Nx monorepo structure exists in open-sharia-enterprise
- apps/ directory available for new applications
- Nx workspace configured (nx.json, package.json)

**Access Requirements**:

- Write access to repository
- Ability to create new branches (if needed for testing)

**External Dependencies**:

- None for initial setup (no database required)

**Team Dependencies**:

- None (self-contained initialization)

## Goals

- Set up a Spring Boot Maven project with proper structure
- Configure basic Spring Boot dependencies
- Create initial application properties for dev and production profiles (port 8100)
- Set up project.json for Nx integration
- Create `/api/v1/hello` endpoint returning `{message: "world"}`
- Create basic health check endpoint
- Add basic logging configuration
- Set up proper .gitignore for Java/Maven
- Create README.md with setup and development instructions

## Context

The open-sharia-enterprise platform requires a simple backend service to demonstrate Spring Boot integration within the Nx monorepo. This application will serve as a minimal viable backend with a single REST endpoint, providing a foundation that can be extended with additional features as needed.

Unlike the more complex `orca-grid-be` (Knowledge Management System), this service is intentionally kept simple and focused on demonstrating basic API functionality without database dependencies.

## Quick Links

- [Requirements](./requirements.md) - Detailed requirements and acceptance criteria
- [Technical Documentation](./tech-docs.md) - Architecture and implementation details
- [Delivery Plan](./delivery.md) - Milestones, deliverables, and validation

## Key Differences from Other Backend Services

**Compared to orca-grid-be**:

- Simpler scope: Single API endpoint
- No database: Pure stateless service
- Different port: 8100 (vs 8080 for orca-grid-be)
- Focused on API basics: Foundation for future expansion

## Next Steps After Initial Setup

Once this plan is completed, future work could include:

1. Add additional REST API endpoints
2. Implement request/response validation
3. Add comprehensive integration tests
4. Set up continuous integration pipeline
5. Add API documentation (OpenAPI/Swagger)
6. Implement rate limiting
7. Add monitoring and observability (Prometheus, Grafana)
8. Optionally add database integration (Spring Data JPA, PostgreSQL)
