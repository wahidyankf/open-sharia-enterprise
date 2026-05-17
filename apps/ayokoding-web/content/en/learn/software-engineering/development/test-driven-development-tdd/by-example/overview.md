---
title: "Overview"
date: 2026-01-31T00:00:00+07:00
draft: false
weight: 10000000
description: "Learn Test-Driven Development through practical code examples covering Red-Green-Refactor cycle, testing patterns, and production TDD workflows"
tags: ["tdd", "test-driven-development", "tutorial", "by-example", "unit-testing", "refactoring", "design"]
---

**Want to master Test-Driven Development through practical examples?** This by-example guide teaches TDD through annotated code examples organized by complexity level.

## What Is TDD By-Example Learning?

TDD by-example learning is a **code-first approach** where you learn through practical implementations of the Red-Green-Refactor cycle rather than narrative explanations. Each example shows:

- **Failing tests first** - Red phase demonstrating test-first thinking
- **Minimal implementation** - Green phase making tests pass with simplest code
- **Refactoring** - Improving design while keeping tests green
- **Production patterns** - Proven approaches from industry leaders

This approach is **ideal for developers** who want to master test-first development through hands-on practice.

## Learning Path

The TDD by-example tutorial guides you through examples organized into three progressive levels, from basic unit testing to enterprise-scale TDD implementations.

## Coverage Philosophy

This by-example guide provides practical coverage of TDD through annotated examples. The focus is on **implementing TDD workflows**, not just theory.

### What's Covered

- **TDD fundamentals** - Red-Green-Refactor cycle, test-first thinking
- **Testing patterns** - Assertions, fixtures, test organization
- **Test doubles** - Mocks, stubs, fakes, spies
- **Advanced techniques** - Asynchronous testing, property-based testing, mutation testing
- **Production patterns** - TDD with databases, web apps, microservices
- **Legacy code** - Characterization tests, refactoring strategies
- **Enterprise TDD** - Test architecture, scaling patterns, performance

### What's NOT Covered

- BDD and acceptance testing (see BDD tutorial for stakeholder collaboration)
- Manual testing techniques (see testing fundamentals)
- Framework documentation beyond common patterns

## Prerequisites

- Programming experience in at least one language (Java, JavaScript, Python, Go, or similar)
- Basic understanding of functions, classes, and control flow
- Familiarity with testing concepts (assertions, test runners)

## Structure of Each Example

Every example follows a consistent format:

1. **Brief Explanation**: What TDD concept the example demonstrates
2. **Test Code (Red)**: Failing test showing requirements
3. **Implementation (Green)**: Minimal code to pass the test
4. **Refactored Version**: Improved design preserving behavior
5. **Key Takeaway**: The core TDD principle to retain

This structure provides the complete TDD cycle in every example, reinforcing the rhythm of test-first development.

## Examples by Level

### Beginner (Examples 1–30)

- [Example 1: Your First Test (Hello World TDD)](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/beginner#example-1-your-first-test-hello-world-tdd)
- [Example 2: Red-Green-Refactor Cycle](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/beginner#example-2-red-green-refactor-cycle)
- [Example 3: Testing Primitive Types (Numbers)](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/beginner#example-3-testing-primitive-types-numbers)
- [Example 4: Testing Strings](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/beginner#example-4-testing-strings)
- [Example 5: Testing Booleans and Truthiness](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/beginner#example-5-testing-booleans-and-truthiness)
- [Example 6: Testing Arrays - Basic Operations](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/beginner#example-6-testing-arrays---basic-operations)
- [Example 7: Testing Objects - Property Access](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/beginner#example-7-testing-objects---property-access)
- [Example 8: Test Fixtures - Setup and Teardown](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/beginner#example-8-test-fixtures---setup-and-teardown)
- [Example 9: Single Responsibility Principle in Tests](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/beginner#example-9-single-responsibility-principle-in-tests)
- [Example 10: Testing Edge Cases - Null and Undefined](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/beginner#example-10-testing-edge-cases---null-and-undefined)
- [Example 11: Testing Boundaries - Numbers](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/beginner#example-11-testing-boundaries---numbers)
- [Example 12: Testing Error Conditions](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/beginner#example-12-testing-error-conditions)
- [Example 13: Arrange-Act-Assert (AAA) Pattern](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/beginner#example-13-arrange-act-assert-aaa-pattern)
- [Example 14: Given-When-Then Test Structure](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/beginner#example-14-given-when-then-test-structure)
- [Example 15: Test Organization - Describe Blocks](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/beginner#example-15-test-organization---describe-blocks)
- [Example 16: Test Naming Conventions](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/beginner#example-16-test-naming-conventions)
- [Example 17: DRY Principle in Tests](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/beginner#example-17-dry-principle-in-tests)
- [Example 18: Testing Pure Functions](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/beginner#example-18-testing-pure-functions)
- [Example 19: Testing with Multiple Assertions (Same Concept)](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/beginner#example-19-testing-with-multiple-assertions-same-concept)
- [Example 20: Test-First Thinking Exercise](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/beginner#example-20-test-first-thinking-exercise)
- [Example 21: TDD Workflow Demonstration](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/beginner#example-21-tdd-workflow-demonstration)
- [Example 22: Common Beginner Mistake - Testing Implementation](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/beginner#example-22-common-beginner-mistake---testing-implementation)
- [Example 23: Testing Function Return Values](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/beginner#example-23-testing-function-return-values)
- [Example 24: Testing Side Effects (State Changes)](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/beginner#example-24-testing-side-effects-state-changes)
- [Example 25: Testing Output Messages](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/beginner#example-25-testing-output-messages)
- [Example 26: Testing with Simple Assertions](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/beginner#example-26-testing-with-simple-assertions)
- [Example 27: Basic Refactoring with Tests](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/beginner#example-27-basic-refactoring-with-tests)
- [Example 28: Testing Collections - Filtering](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/beginner#example-28-testing-collections---filtering)
- [Example 29: Testing Transformations (Map)](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/beginner#example-29-testing-transformations-map)
- [Example 30: Testing Aggregation (Reduce)](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/beginner#example-30-testing-aggregation-reduce)

### Intermediate (Examples 31–58)

- [Example 31: Introduction to Test Doubles - Stubs](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/intermediate#example-31-introduction-to-test-doubles---stubs)
- [Example 32: Test Doubles - Mocks](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/intermediate#example-32-test-doubles---mocks)
- [Example 33: Test Doubles - Spies](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/intermediate#example-33-test-doubles---spies)
- [Example 34: Test Doubles - Fakes](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/intermediate#example-34-test-doubles---fakes)
- [Example 35: Dependency Injection for Testability](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/intermediate#example-35-dependency-injection-for-testability)
- [Example 36: Testing Promises - Basic Resolution](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/intermediate#example-36-testing-promises---basic-resolution)
- [Example 37: Testing Async/Await Patterns](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/intermediate#example-37-testing-asyncawait-patterns)
- [Example 38: Testing Callbacks](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/intermediate#example-38-testing-callbacks)
- [Example 39: Testing Timers and Delays](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/intermediate#example-39-testing-timers-and-delays)
- [Example 40: Testing HTTP Requests - Mocking Fetch](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/intermediate#example-40-testing-http-requests---mocking-fetch)
- [Example 41: Testing with In-Memory Databases](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/intermediate#example-41-testing-with-in-memory-databases)
- [Example 42: Property-Based Testing Introduction](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/intermediate#example-42-property-based-testing-introduction)
- [Example 43: Mutation Testing Concepts](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/intermediate#example-43-mutation-testing-concepts)
- [Example 44: Test Coverage Analysis](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/intermediate#example-44-test-coverage-analysis)
- [Example 45: TDD with Express.js Routes](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/intermediate#example-45-tdd-with-expressjs-routes)
- [Example 46: TDD with React Components](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/intermediate#example-46-tdd-with-react-components)
- [Example 47: Testing Event-Driven Code](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/intermediate#example-47-testing-event-driven-code)
- [Example 48: Testing State Machines](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/intermediate#example-48-testing-state-machines)
- [Example 49: Parameterized Tests (test.each)](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/intermediate#example-49-parameterized-tests-testeach)
- [Example 50: Snapshot Testing Use Cases](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/intermediate#example-50-snapshot-testing-use-cases)
- [Example 51: Testing File I/O Operations](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/intermediate#example-51-testing-file-io-operations)
- [Example 52: Testing with Environment Variables](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/intermediate#example-52-testing-with-environment-variables)
- [Example 53: CI/CD Integration for TDD](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/intermediate#example-53-cicd-integration-for-tdd)
- [Example 54: Test Performance Optimization](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/intermediate#example-54-test-performance-optimization)
- [Example 55: Flaky Test Detection and Fixes](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/intermediate#example-55-flaky-test-detection-and-fixes)
- [Example 56: Test Data Builders Pattern](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/intermediate#example-56-test-data-builders-pattern)
- [Example 57: Object Mother Pattern](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/intermediate#example-57-object-mother-pattern)
- [Example 58: London vs Chicago TDD Schools](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/intermediate#example-58-london-vs-chicago-tdd-schools)

### Advanced (Examples 59–85)

- [Example 59: Characterization Tests for Legacy Code](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/advanced#example-59-characterization-tests-for-legacy-code)
- [Example 60: Approval Testing for Complex Outputs](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/advanced#example-60-approval-testing-for-complex-outputs)
- [Example 61: Working with Seams in Untestable Code](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/advanced#example-61-working-with-seams-in-untestable-code)
- [Example 62: Dependency Breaking Techniques](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/advanced#example-62-dependency-breaking-techniques)
- [Example 63: TDD for Microservices - Service Isolation](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/advanced#example-63-tdd-for-microservices---service-isolation)
- [Example 64: Contract Testing for Microservices](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/advanced#example-64-contract-testing-for-microservices)
- [Example 65: Testing Distributed Systems - Eventual Consistency](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/advanced#example-65-testing-distributed-systems---eventual-consistency)
- [Example 66: Testing Event Sourcing Systems](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/advanced#example-66-testing-event-sourcing-systems)
- [Example 67: Testing CQRS Patterns](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/advanced#example-67-testing-cqrs-patterns)
- [Example 68: TDD in Polyglot Environments](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/advanced#example-68-tdd-in-polyglot-environments)
- [Example 69: Performance-Sensitive TDD](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/advanced#example-69-performance-sensitive-tdd)
- [Example 70: Security Testing with TDD](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/advanced#example-70-security-testing-with-tdd)
- [Example 71: TDD Anti-Patterns - Testing Implementation Details](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/advanced#example-71-tdd-anti-patterns---testing-implementation-details)
- [Example 72: Test-Induced Design Damage](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/advanced#example-72-test-induced-design-damage)
- [Example 73: Scaling TDD Across Teams](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/advanced#example-73-scaling-tdd-across-teams)
- [Example 74: TDD Coaching and Mentoring](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/advanced#example-74-tdd-coaching-and-mentoring)
- [Example 75: ROI Measurement for TDD](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/advanced#example-75-roi-measurement-for-tdd)
- [Example 76: TDD in Regulated Industries](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/advanced#example-76-tdd-in-regulated-industries)
- [Example 77: Compliance Testing Patterns](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/advanced#example-77-compliance-testing-patterns)
- [Example 78: TDD for Machine Learning Systems](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/advanced#example-78-tdd-for-machine-learning-systems)
- [Example 79: Testing AI/ML Model Behavior](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/advanced#example-79-testing-aiml-model-behavior)
- [Example 80: Evolutionary Architecture with TDD](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/advanced#example-80-evolutionary-architecture-with-tdd)
- [Example 81: TDD in Continuous Deployment](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/advanced#example-81-tdd-in-continuous-deployment)
- [Example 82: Production Testing Patterns](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/advanced#example-82-production-testing-patterns)
- [Example 83: Testing with Feature Flags](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/advanced#example-83-testing-with-feature-flags)
- [Example 84: Synthetic Monitoring and Production Tests](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/advanced#example-84-synthetic-monitoring-and-production-tests)
- [Example 85: TDD Anti-Pattern Recovery - Abandoned Test Suites](/en/learn/software-engineering/development/test-driven-development-tdd/by-example/advanced#example-85-tdd-anti-pattern-recovery---abandoned-test-suites)
