# TypeScript DDD Templates

Reusable TypeScript templates for Domain-Driven Design patterns. These templates follow OSE Platform conventions and best practices.

## Available Templates

- **domain-entity.template.ts** - Domain entity with identity and lifecycle
- **value-object.template.ts** - Immutable value object
- **aggregate-root.template.ts** - Aggregate root entity
- **domain-event.template.ts** - Domain event for significant occurrences
- **repository-interface.template.ts** - Repository abstraction
- **service-layer.template.ts** - Domain service
- **use-case.template.ts** - Application use case
- **dto.template.ts** - Data Transfer Object
- **api-controller.template.ts** - API controller with validation
- **error-hierarchy.template.ts** - Custom error types
- **tsconfig.template.json** - TypeScript strict configuration

## Usage

1. Copy template file to your project
2. Replace placeholders (e.g., `EntityName`, `propertyName`)
3. Customize for your domain
4. Implement required logic

## Template Conventions

- Use Result pattern for error handling
- Immutable value objects with Object.freeze()
- Branded types for domain IDs
- Explicit validation in constructors
- Private constructors with static factory methods
- TypeScript strict mode enabled
