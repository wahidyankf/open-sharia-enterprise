---
title: "Overview"
date: 2026-01-31T00:00:00+07:00
draft: false
weight: 10000000
description: "Learn Behavior-Driven Development through practical code examples covering Gherkin syntax, framework integration, and production BDD patterns"
tags: ["bdd", "behavior-driven-development", "tutorial", "by-example", "gherkin", "cucumber", "testing"]
---

**Want to master Behavior-Driven Development through practical examples?** This by-example guide teaches BDD through annotated code and scenario examples organized by complexity level.

## What Is BDD By-Example Learning?

BDD by-example learning is a **code-first approach** where you learn through practical implementations of BDD scenarios and frameworks rather than narrative explanations. Each example shows:

- **Gherkin scenarios** - Natural language specifications that stakeholders can read
- **Working code** - Runnable step definitions and test implementations
- **Framework integration** - How BDD fits into real testing workflows
- **Production patterns** - Proven approaches from industry leaders

This approach is **ideal for developers and testers** who want to implement effective collaboration through executable specifications.

## Learning Path

The BDD by-example tutorial guides you through examples organized into three progressive levels, from basic Gherkin syntax to enterprise-scale BDD implementations.

## Coverage Philosophy

This by-example guide provides practical coverage of BDD through annotated examples. The focus is on **implementing BDD workflows**, not just theory.

### What's Covered

- **Gherkin fundamentals** - Given-When-Then, scenarios, features, backgrounds
- **Step definitions** - Mapping natural language to executable code
- **Data-driven scenarios** - Scenario outlines, example tables, data tables
- **Framework integration** - Cucumber (Java/JavaScript), SpecFlow (.NET), Behave (Python)
- **Production patterns** - Page Object Model, API testing, database testing, CI/CD integration
- **Advanced BDD** - Microservices scenarios, event-driven BDD, living documentation

### What's NOT Covered

- Manual testing techniques (see testing fundamentals for manual approaches)
- Unit testing details (see TDD tutorial for unit testing focus)
- Framework-specific advanced features beyond common patterns

## Prerequisites

- Programming experience in at least one language (Java, JavaScript, Python, or C#)
- Basic understanding of automated testing concepts
- Familiarity with web applications and APIs

## Structure of Each Example

Every example follows a consistent format:

1. **Brief Explanation**: What BDD concept the example demonstrates
2. **Gherkin Scenario**: Natural language specification
3. **Annotated Code**: Step definitions with inline comments
4. **Example Execution**: How the test runs and what it validates
5. **Key Takeaway**: The core BDD principle to retain

This structure provides specification, implementation, and understanding all in one place.

## Examples by Level

### Beginner (Examples 1–30)

- [Example 1: Hello World BDD - First Feature File](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/beginner#example-1-hello-world-bdd---first-feature-file)
- [Example 2: Given-When-Then Structure](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/beginner#example-2-given-when-then-structure)
- [Example 3: Multiple Scenarios in One Feature](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/beginner#example-3-multiple-scenarios-in-one-feature)
- [Example 4: Background - Shared Setup Steps](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/beginner#example-4-background---shared-setup-steps)
- [Example 5: And & But Keywords for Readability](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/beginner#example-5-and--but-keywords-for-readability)
- [Example 6: Data Tables in Steps](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/beginner#example-6-data-tables-in-steps)
- [Example 7: Scenario Outline with Examples Table](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/beginner#example-7-scenario-outline-with-examples-table)
- [Example 8: Tags for Organizing Scenarios](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/beginner#example-8-tags-for-organizing-scenarios)
- [Example 9: Comments in Feature Files](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/beginner#example-9-comments-in-feature-files)
- [Example 10: Step Definition Basics (TypeScript)](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/beginner#example-10-step-definition-basics-typescript)
- [Example 11: Cucumber Expressions - String Parameters](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/beginner#example-11-cucumber-expressions---string-parameters)
- [Example 12: Multiple Parameters in One Step](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/beginner#example-12-multiple-parameters-in-one-step)
- [Example 13: Data Tables in Step Definitions](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/beginner#example-13-data-tables-in-step-definitions)
- [Example 14: Before Hook - Setup Before Scenarios](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/beginner#example-14-before-hook---setup-before-scenarios)
- [Example 15: After Hook - Teardown After Scenarios](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/beginner#example-15-after-hook---teardown-after-scenarios)
- [Example 16: World Object - Sharing State Between Steps](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/beginner#example-16-world-object---sharing-state-between-steps)
- [Example 17: Custom Parameter Types](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/beginner#example-17-custom-parameter-types)
- [Example 18: Pending Steps - Work in Progress](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/beginner#example-18-pending-steps---work-in-progress)
- [Example 19: Step Definition Patterns - Optional Text](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/beginner#example-19-step-definition-patterns---optional-text)
- [Example 20: Alternative Text in Steps](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/beginner#example-20-alternative-text-in-steps)
- [Example 21: Async Step Definitions](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/beginner#example-21-async-step-definitions)
- [Example 22: Retrying Failed Scenarios](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/beginner#example-22-retrying-failed-scenarios)
- [Example 23: Conditional Steps with Step Definitions](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/beginner#example-23-conditional-steps-with-step-definitions)
- [Example 24: Sharing Step Definitions Across Features](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/beginner#example-24-sharing-step-definitions-across-features)
- [Example 25: Simple Assertions with Chai](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/beginner#example-25-simple-assertions-with-chai)
- [Example 26: Testing Error Messages](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/beginner#example-26-testing-error-messages)
- [Example 27: Background vs Before Hook - When to Use Which](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/beginner#example-27-background-vs-before-hook---when-to-use-which)
- [Example 28: Simple BDD Workflow Pattern](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/beginner#example-28-simple-bdd-workflow-pattern)
- [Example 29: Organizing Feature Files by Domain](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/beginner#example-29-organizing-feature-files-by-domain)
- [Example 30: Running Specific Scenarios from CLI](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/beginner#example-30-running-specific-scenarios-from-cli)

### Intermediate (Examples 31–58)

- [Example 31: Page Object Model - Separating UI Logic from Tests](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/intermediate#example-31-page-object-model---separating-ui-logic-from-tests)
- [Example 32: API Testing with REST Client](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/intermediate#example-32-api-testing-with-rest-client)
- [Example 33: Database Testing - Verifying Data State](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/intermediate#example-33-database-testing---verifying-data-state)
- [Example 34: Cucumber-JVM (Java) - Cross-Platform BDD](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/intermediate#example-34-cucumber-jvm-java---cross-platform-bdd)
- [Example 35: SpecFlow (C#) - BDD in .NET Ecosystem](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/intermediate#example-35-specflow-c---bdd-in-net-ecosystem)
- [Example 36: Behave (Python) - Pythonic BDD](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/intermediate#example-36-behave-python---pythonic-bdd)
- [Example 37: Parameterized Scenarios with Complex Data](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/intermediate#example-37-parameterized-scenarios-with-complex-data)
- [Example 38: Custom Matchers for Domain-Specific Assertions](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/intermediate#example-38-custom-matchers-for-domain-specific-assertions)
- [Example 39: Test Doubles - Mocks and Stubs in BDD](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/intermediate#example-39-test-doubles---mocks-and-stubs-in-bdd)
- [Example 40: BDD in CI/CD Pipeline Configuration](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/intermediate#example-40-bdd-in-cicd-pipeline-configuration)
- [Example 41: Parallel Test Execution for Speed](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/intermediate#example-41-parallel-test-execution-for-speed)
- [Example 42: Test Data Management with Fixtures](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/intermediate#example-42-test-data-management-with-fixtures)
- [Example 43: Flaky Test Prevention Strategies](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/intermediate#example-43-flaky-test-prevention-strategies)
- [Example 44: Living Documentation with Cucumber Reports](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/intermediate#example-44-living-documentation-with-cucumber-reports)
- [Example 45: Cross-Browser Testing with BDD](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/intermediate#example-45-cross-browser-testing-with-bdd)
- [Example 46: Mobile App Testing with Appium and BDD](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/intermediate#example-46-mobile-app-testing-with-appium-and-bdd)
- [Example 47: GraphQL API Testing with BDD](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/intermediate#example-47-graphql-api-testing-with-bdd)
- [Example 48: WebSocket Testing with Real-Time Events](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/intermediate#example-48-websocket-testing-with-real-time-events)
- [Example 49: File Upload/Download Testing](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/intermediate#example-49-file-uploaddownload-testing)
- [Example 50: Email Testing with Mail Trap](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/intermediate#example-50-email-testing-with-mail-trap)
- [Example 51: PDF/Document Validation](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/intermediate#example-51-pdfdocument-validation)
- [Example 52: Performance Testing with BDD Scenarios](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/intermediate#example-52-performance-testing-with-bdd-scenarios)
- [Example 53: Security Testing - Authentication & Authorization](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/intermediate#example-53-security-testing---authentication--authorization)
- [Example 54: Multi-Environment Testing (Dev, Staging, Prod)](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/intermediate#example-54-multi-environment-testing-dev-staging-prod)
- [Example 55: Test Reporting and Analytics](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/intermediate#example-55-test-reporting-and-analytics)
- [Example 56: BDD with Docker Containers](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/intermediate#example-56-bdd-with-docker-containers)
- [Example 57: Integration Testing Patterns](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/intermediate#example-57-integration-testing-patterns)
- [Example 58: Contract Testing Basics with Pact](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/intermediate#example-58-contract-testing-basics-with-pact)

### Advanced (Examples 59–85)

- [Example 59: BDD in Microservices - Service-to-Service Communication](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/advanced#example-59-bdd-in-microservices---service-to-service-communication)
- [Example 60: Event-Driven BDD with Message Brokers](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/advanced#example-60-event-driven-bdd-with-message-brokers)
- [Example 61: SAGA Pattern - Distributed Transaction Coordination](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/advanced#example-61-saga-pattern---distributed-transaction-coordination)
- [Example 62: BDD for Distributed Tracing](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/advanced#example-62-bdd-for-distributed-tracing)
- [Example 63: Chaos Engineering with BDD - Testing Resilience](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/advanced#example-63-chaos-engineering-with-bdd---testing-resilience)
- [Example 64: BDD Anti-Patterns and Refactoring](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/advanced#example-64-bdd-anti-patterns-and-refactoring)
- [Example 65: Custom Gherkin Dialects for Domain-Specific Languages](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/advanced#example-65-custom-gherkin-dialects-for-domain-specific-languages)
- [Example 66: BDD Metaprogramming - Dynamic Step Generation](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/advanced#example-66-bdd-metaprogramming---dynamic-step-generation)
- [Example 67: Living Documentation at Scale - Automated Spec Generation](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/advanced#example-67-living-documentation-at-scale---automated-spec-generation)
- [Example 68: BDD in Legacy System Modernization](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/advanced#example-68-bdd-in-legacy-system-modernization)
- [Example 69: A/B Testing Scenarios - Feature Variant Verification](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/advanced#example-69-ab-testing-scenarios---feature-variant-verification)
- [Example 70: Feature Flag Testing - Progressive Rollout Verification](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/advanced#example-70-feature-flag-testing---progressive-rollout-verification)
- [Example 71: Blue-Green Deployment Testing with BDD](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/advanced#example-71-blue-green-deployment-testing-with-bdd)
- [Example 72: Canary Release Testing - Gradual Traffic Shifting](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/advanced#example-72-canary-release-testing---gradual-traffic-shifting)
- [Example 73: BDD for Observability - Metrics, Logs, Traces](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/advanced#example-73-bdd-for-observability---metrics-logs-traces)
- [Example 74: Multi-Tenant Testing - Data Isolation Verification](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/advanced#example-74-multi-tenant-testing---data-isolation-verification)
- [Example 75: Data Privacy Testing - GDPR/CCPA Compliance](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/advanced#example-75-data-privacy-testing---gdprccpa-compliance)
- [Example 76: BDD in Regulated Industries - Healthcare HL7 FHIR](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/advanced#example-76-bdd-in-regulated-industries---healthcare-hl7-fhir)
- [Example 77: BDD in Finance - PCI-DSS Compliance Testing](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/advanced#example-77-bdd-in-finance---pci-dss-compliance-testing)
- [Example 78: Enterprise Test Architecture - Shared Step Libraries](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/advanced#example-78-enterprise-test-architecture---shared-step-libraries)
- [Example 79: BDD Governance and Standardization](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/advanced#example-79-bdd-governance-and-standardization)
- [Example 80: Scaling BDD Across Teams - Federated Ownership](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/advanced#example-80-scaling-bdd-across-teams---federated-ownership)
- [Example 81: BDD Coaching and Training Patterns](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/advanced#example-81-bdd-coaching-and-training-patterns)
- [Example 82: ROI Measurement for BDD Initiatives](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/advanced#example-82-roi-measurement-for-bdd-initiatives)
- [Example 83: BDD in Continuous Discovery - Product Experimentation](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/advanced#example-83-bdd-in-continuous-discovery---product-experimentation)
- [Example 84: BDD for Machine Learning Models - Model Validation](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/advanced#example-84-bdd-for-machine-learning-models---model-validation)
- [Example 85: Production Case Study - E-Commerce Platform BDD Transformation](/en/learn/software-engineering/development/behavior-driven-development-bdd/by-example/advanced#example-85-production-case-study---e-commerce-platform-bdd-transformation)
