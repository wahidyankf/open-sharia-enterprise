# Test-Driven Development (TDD) Documentation

Comprehensive documentation on Test-Driven Development patterns, principles, and practices for building robust, well-tested software systems. This documentation covers the Red-Green-Refactor cycle, testing strategies, and TDD application in various contexts including Sharia-compliant business systems, functional programming, and domain-driven design.

## What is Test-Driven Development?

Test-Driven Development (TDD) is a software development approach where tests are written **before** the production code. Popularized by Kent Beck as part of Extreme Programming (XP) in the early 2000s, TDD provides a disciplined process for building software that is thoroughly tested, well-designed, and maintainable.

**Key Principles:**

- **Red-Green-Refactor**: Write failing test → Make it pass → Improve design
- **Test First**: Tests drive design and implementation decisions
- **Small Steps**: Incremental progress with continuous feedback loops
- **Regression Safety**: Comprehensive test suite prevents breaking changes
- **Living Documentation**: Tests document expected behavior in executable form
- **Design Feedback**: Test difficulty signals design problems early

**Core Philosophy:**

TDD is not just about testing—it's fundamentally a design technique. By writing tests first, you:

1. **Clarify requirements** before writing code
2. **Design interfaces** from the consumer's perspective
3. **Ensure testability** by construction, not afterthought
4. **Build confidence** to refactor and improve continuously
5. **Document behavior** through executable examples

### History and Evolution

- **1990s**: Kent Beck develops TDD practices at Chrysler Comprehensive Compensation (C3) project
- **2002**: Kent Beck publishes "Test Driven Development: By Example" - the foundational text
- **2000s**: TDD becomes core practice in Agile and XP methodologies
- **2009**: "Growing Object-Oriented Software, Guided by Tests" advances TDD for enterprise systems
- **2010s**: TDD expands to functional programming with property-based testing
- **2020s**: TDD integrates with Domain-Driven Design and modern architectures

### When to Use TDD: Decision Matrix

Use this matrix to determine if TDD is appropriate for your project. Score each dimension from 1-5, then calculate total.

| Dimension                     | Score 1-2 (Low)             | Score 3 (Medium)                | Score 4-5 (High)                        |
| ----------------------------- | --------------------------- | ------------------------------- | --------------------------------------- |
| **Business Logic Complexity** | Simple CRUD operations      | Moderate rules (3-5 conditions) | Complex rules with many edge cases      |
| **Domain Criticality**        | Internal tools, low impact  | Business features               | Regulatory compliance, safety-critical  |
| **Code Longevity**            | Spike/prototype, < 3 months | 6-12 month lifespan             | Long-lived (> 1 year), frequent changes |
| **Team TDD Experience**       | No experience               | Some training                   | Practiced in TDD                        |
| **Refactoring Frequency**     | Static requirements         | Occasional changes              | Frequent refactoring expected           |
| **Collaboration Complexity**  | Solo developer              | Small team (2-4)                | Large team or distributed               |

**Scoring Guide:**

- **24-30 points**: TDD strongly recommended - high complexity, criticality, or change frequency
- **16-23 points**: TDD recommended - moderate complexity with some critical aspects
- **10-15 points**: TDD optional - evaluate based on team preference and learning goals
- **6-9 points**: Consider lightweight testing - TDD may be overhead

**Example: Zakat Calculation System**

- Business Logic Complexity: **5** (multiple asset types, nisab thresholds, Hawl calculations)
- Domain Criticality: **5** (religious obligation, regulatory reporting)
- Code Longevity: **5** (long-lived system, evolving Shariah interpretations)
- Team TDD Experience: **3** (moderate training)
- Refactoring Frequency: **4** (changing business rules)
- Collaboration Complexity: **4** (developers + Shariah scholars)

**Total: 26 points** → TDD strongly recommended

### When to Use TDD

TDD is ideal for:

- **Complex business logic** requiring high correctness (e.g., Zakat calculations, interest-free financing)
- **Long-lived systems** with frequent changes and multiple maintainers
- **Regulatory compliance** or safety-critical applications (e.g., Halal certification)
- **Domain-Driven Design** projects (testing aggregates, value objects, domain events)
- **Codebases requiring confidence** in refactoring and continuous improvement
- **Teams seeking faster feedback loops** and reduced debugging time
- **API development** where contracts must be stable and well-tested

### When NOT to Use TDD

TDD may not be appropriate for:

- **Prototypes and spikes** where you're exploring solutions (write tests after validating approach)
- **Simple CRUD operations** with minimal logic (standard integration tests may suffice)
- **Highly visual UI work** where design iteration is primary focus (use E2E tests sparingly)
- **Trivial glue code** connecting well-tested components
- **Throwaway code** with lifespan < 1 month
- **Learning new technology** - understand basics first, then apply TDD

**Note**: Even in these cases, having **some** tests is valuable. The question is whether to write tests **first** (TDD) or **after** (test-later).

### Benefits of TDD

1. **Better Design**: Writing tests first forces you to think about interfaces, dependencies, and separation of concerns
2. **Living Documentation**: Tests serve as executable examples showing how code should be used
3. **Regression Safety**: Comprehensive test suite catches breaking changes immediately
4. **Faster Debugging**: When tests fail, the problem is localized to recent changes
5. **Confidence in Refactoring**: Tests act as a safety net when improving code structure
6. **Reduced Defects**: Research shows 40-90% defect reduction in TDD codebases (see [Research Evidence](#research-evidence) below)
7. **Design Feedback**: Difficulty writing tests signals design problems (tight coupling, hidden dependencies)

### Research Evidence

Empirical studies from multiple organizations demonstrate TDD's effectiveness in reducing defects:

**IBM (2003)** - Williams, L., Maximilien, E.M., Vouk, M.
[Test-driven development as a defect-reduction practice](https://collaboration.csc.ncsu.edu/laurie/Papers/williamsltestDrivenDevelopment.pdf)
_IEEE International Symposium on Software Reliability Engineering (ISSRE 2003)_

- **Finding**: 40% fewer defects during functional verification and regression tests
- **Context**: IBM Retail Store Solutions team compared TDD vs. ad-hoc unit testing
- **Coverage**: Achieved 86% automated unit test coverage (target: 80%)
- **Productivity**: No negative impact on team productivity

**Microsoft & IBM (2008)** - Nagappan, N., Maximilien, E.M., Bhat, T., Williams, L.
[Realizing quality improvement through test driven development: results and experiences of four industrial teams](https://www.microsoft.com/en-us/research/wp-content/uploads/2009/10/Realizing-Quality-Improvement-Through-Test-Driven-Development-Results-and-Experiences-of-Four-Industrial-Teams-nagappan_tdd.pdf)
_Empirical Software Engineering 13, 289–302 (2008)_

- **Finding**: 40-90% reduction in pre-release defect density
- **Teams**: Three Microsoft teams (Windows, MSN, DevDiv) and one IBM team
- **Trade-off**: 15-35% increase in initial development time
- **Long-term**: Development time increase offset by reduced maintenance costs

**North Carolina State University (2003)** - Williams, L., Maximilien, E.M.
[Assessing test-driven development at IBM](https://www.researchgate.net/publication/4016736_Assessing_test-driven_development_at_IBM)
_Proceedings of the 25th International Conference on Software Engineering (ICSE 2003)_

- **Finding**: ~50% defect reduction compared to traditional development
- **Context**: Industrial case study with IBM development teams
- **Methodology**: Comparative analysis of similar projects with and without TDD

These peer-reviewed studies provide strong empirical evidence for TDD's effectiveness in industrial software development contexts.

## Documentation Structure

This documentation is organized into foundational concepts, core practices, advanced integration, and meta-guidance.

### Foundation

Core TDD concepts and workflow:

- **[01. Introduction and Philosophy](./ex-so-de-tedrdeve__01-introduction-and-philosophy.md)** - Overview, history, when to use TDD, benefits, misconceptions
- **[02. Red-Green-Refactor Cycle](./ex-so-de-tedrdeve__02-red-green-refactor-cycle.md)** - The fundamental TDD workflow ⭐ **Most Important**
- **[03. Test Types and Pyramid](./ex-so-de-tedrdeve__03-test-types-and-pyramid.md)** - Unit, integration, E2E tests and testing strategy
- **[README (this file)]** - Navigation and learning paths

### Core Practices

Essential TDD techniques for everyday development:

- **[04. Unit Testing Fundamentals](./ex-so-de-tedrdeve__04-unit-testing-fundamentals.md)** - Test structure, isolation, AAA pattern
- **[05. Test Doubles](./ex-so-de-tedrdeve__05-test-doubles.md)** - Mocks, stubs, spies, fakes - when to use each
- **[06. Testing Patterns](./ex-so-de-tedrdeve__06-testing-patterns.md)** - AAA, Given-When-Then, table-driven tests, snapshot testing
- **[07. Test Data Builders](./ex-so-de-tedrdeve__07-test-data-builders.md)** - Builder pattern and Object Mother for test fixtures
- **[08. Assertion Patterns](./ex-so-de-tedrdeve__08-assertion-patterns.md)** - Effective assertions, custom matchers, mutation testing
- **[09. Integration Testing](./ex-so-de-tedrdeve__09-integration-testing.md)** - Testing with databases, APIs, and contract testing
- **[10. End-to-End Testing](./ex-so-de-tedrdeve__10-end-to-end-testing.md)** - Full system verification, when to use sparingly

### Advanced Integration

TDD in complex contexts:

- **[11. TDD and Functional Programming](./ex-so-de-tedrdeve__11-tdd-and-functional-programming.md)** - Testing pure functions, property-based testing
- **[12. TDD and DDD](./ex-so-de-tedrdeve__12-tdd-and-ddd.md)** - Testing aggregates, value objects, domain events, repositories
- **[13. Legacy Code and Characterization Tests](./ex-so-de-tedrdeve__13-legacy-code-and-characterization-tests.md)** - Adding tests to existing code, approval testing
- **[14. Refactoring with Tests](./ex-so-de-tedrdeve__14-refactoring-with-tests.md)** - Safe refactoring patterns with test coverage

### Meta & Guidance

Best practices and anti-patterns:

- **[15. Decision Trees and Best Practices](./ex-so-de-tedrdeve__15-decision-trees-and-best-practices.md)** - When to use each test type, coverage strategies, test observability
- **[16. TDD in Nx Monorepo](./ex-so-de-tedrdeve__16-tdd-in-nx-monorepo.md)** - Testing in Nx workspace, affected tests, shared utilities
- **[17. FAQ](./ex-so-de-tedrdeve__17-faq.md)** - Common questions about TDD practice

### Templates

Reusable templates for applying TDD:

- **[Templates Directory](./templates/)** - Practical templates for TDD workflows:
  - **[Unit Test Template](./templates/ex-so-de-tedrdeve-te__unit-test-template.md)** - Standard unit test structure with AAA pattern
  - **[Integration Test Template](./templates/ex-so-de-tedrdeve-te__integration-test-template.md)** - Database and API integration tests
  - **[Test Data Builder Template](./templates/ex-so-de-tedrdeve-te__test-data-builder-template.md)** - Builder pattern for test fixtures
  - **[Property-Based Test Template](./templates/ex-so-de-tedrdeve-te__property-based-test-template.md)** - Property-based testing with fast-check
  - **[TDD Workflow Checklist](./templates/ex-so-de-tedrdeve-te__tdd-workflow-checklist.md)** - Step-by-step Red-Green-Refactor guide
  - **[Coverage Planning Canvas](./templates/ex-so-de-tedrdeve-te__coverage-planning-canvas.md)** - Module-level test coverage strategy
  - **[Test Organization Template](./templates/ex-so-de-tedrdeve-te__test-organization-template.md)** - Test suite file structure and naming
  - **[CI/CD Pipeline Template](./templates/ex-so-de-tedrdeve-te__ci-cd-pipeline-template.md)** - Automated testing in continuous integration
  - **[Starter Full Test Suite](./templates/ex-so-de-tedrdeve-te__starter-full-test-suite.md)** - Complete test suite example

## Learning Paths

Choose a learning path based on your goals and available time.

### 5-Minute Quick Start: Should I Use TDD?

**Goal**: Determine if TDD is appropriate for your project.

**Path**:

1. Read [Decision Matrix](#when-to-use-tdd-decision-matrix) above (3 minutes)
2. Review [When to Use TDD](#when-to-use-tdd) vs. [When NOT to Use TDD](#when-not-to-use-tdd) (2 minutes)

**Outcome**: Clear decision on adopting TDD with quantitative scoring.

### 30-Minute Practical: Write Your First Test

**Goal**: Understand the Red-Green-Refactor cycle and write your first TDD test.

**Path**:

1. **Core Cycle** (10 minutes)
   - [Red-Green-Refactor Cycle](./ex-so-de-tedrdeve__02-red-green-refactor-cycle.md) - The fundamental TDD workflow

2. **Testing Fundamentals** (15 minutes)
   - [Unit Testing Fundamentals](./ex-so-de-tedrdeve__04-unit-testing-fundamentals.md) - Test structure and isolation (10 min)
   - [Testing Patterns - AAA](./ex-so-de-tedrdeve__06-testing-patterns.md#arrange-act-assert-pattern) - Arrange-Act-Assert pattern (5 min)

3. **Apply** (5 minutes)
   - Write one test for Zakat nisab threshold validation
   - Follow Red-Green-Refactor: failing test → make it pass → refactor

**Outcome**: Working test following TDD cycle, hands-on experience with Red-Green-Refactor.

### 2-Hour Deep Dive: Master TDD

**Goal**: Comprehensive understanding of TDD practices and patterns.

**Foundation** (30 minutes):

1. [Introduction and Philosophy](./ex-so-de-tedrdeve__01-introduction-and-philosophy.md) (10 min)
2. [Red-Green-Refactor Cycle](./ex-so-de-tedrdeve__02-red-green-refactor-cycle.md) (15 min)
3. [Test Types and Pyramid](./ex-so-de-tedrdeve__03-test-types-and-pyramid.md) (5 min)

**Core Practices** (60 minutes):

1. [Unit Testing Fundamentals](./ex-so-de-tedrdeve__04-unit-testing-fundamentals.md) (12 min)
2. [Test Doubles](./ex-so-de-tedrdeve__05-test-doubles.md) - Mocks, stubs, spies (12 min)
3. [Testing Patterns](./ex-so-de-tedrdeve__06-testing-patterns.md) (10 min)
4. [Test Data Builders](./ex-so-de-tedrdeve__07-test-data-builders.md) (8 min)
5. [Assertion Patterns](./ex-so-de-tedrdeve__08-assertion-patterns.md) (8 min)
6. [Integration Testing](./ex-so-de-tedrdeve__09-integration-testing.md) (10 min)

**Integration** (20 minutes):

1. [TDD and Functional Programming](./ex-so-de-tedrdeve__11-tdd-and-functional-programming.md) OR [TDD and DDD](./ex-so-de-tedrdeve__12-tdd-and-ddd.md) (15 min)
2. [Refactoring with Tests](./ex-so-de-tedrdeve__14-refactoring-with-tests.md) (5 min)

**Best Practices** (10 minutes):

1. [Decision Trees and Best Practices](./ex-so-de-tedrdeve__15-decision-trees-and-best-practices.md) (7 min)

**Outcome**: Full understanding of TDD workflow, testing patterns, and when to apply different test types.

### By Paradigm: Object-Oriented vs. Functional Programming

**For OOP Developers**:

Traditional TDD with class-based examples:

1. [Introduction and Philosophy](./ex-so-de-tedrdeve__01-introduction-and-philosophy.md)
2. [Red-Green-Refactor Cycle](./ex-so-de-tedrdeve__02-red-green-refactor-cycle.md)
3. [Unit Testing Fundamentals](./ex-so-de-tedrdeve__04-unit-testing-fundamentals.md) - Focus on OOP examples
4. [Test Doubles](./ex-so-de-tedrdeve__05-test-doubles.md) - Mocking dependencies in classes
5. [TDD and DDD](./ex-so-de-tedrdeve__12-tdd-and-ddd.md) - Testing aggregates and entities
6. [Testing Patterns](./ex-so-de-tedrdeve__06-testing-patterns.md) - AAA pattern for methods

**For FP Developers**:

TDD adapted for functional programming:

1. [Introduction and Philosophy](./ex-so-de-tedrdeve__01-introduction-and-philosophy.md)
2. [Red-Green-Refactor Cycle](./ex-so-de-tedrdeve__02-red-green-refactor-cycle.md)
3. [TDD and Functional Programming](./ex-so-de-tedrdeve__11-tdd-and-functional-programming.md) ⭐ **Start Here**
4. [Unit Testing Fundamentals](./ex-so-de-tedrdeve__04-unit-testing-fundamentals.md) - Testing pure functions
5. [Testing Patterns](./ex-so-de-tedrdeve__06-testing-patterns.md) - Property-based testing
6. [Test Data Builders](./ex-so-de-tedrdeve__07-test-data-builders.md) - Immutable fixture builders

**Key Differences**:

- **OOP**: Testing behavior through methods, mocking dependencies, state verification
- **FP**: Testing pure functions, property-based testing, immutable data, no mocking needed

### By Role: Backend, Frontend, QA, Architects

**For Backend Developers**:

Focus on API and business logic testing:

1. [Red-Green-Refactor Cycle](./ex-so-de-tedrdeve__02-red-green-refactor-cycle.md)
2. [Unit Testing Fundamentals](./ex-so-de-tedrdeve__04-unit-testing-fundamentals.md) - Testing business logic
3. [Test Doubles](./ex-so-de-tedrdeve__05-test-doubles.md) - Mocking external services
4. [Integration Testing](./ex-so-de-tedrdeve__09-integration-testing.md) - Database and API integration
5. [TDD and DDD](./ex-so-de-tedrdeve__12-tdd-and-ddd.md) - Domain model testing
6. [TDD in Nx Monorepo](./ex-so-de-tedrdeve__16-tdd-in-nx-monorepo.md) - Backend service testing

**For Frontend Developers**:

Focus on component and UI testing:

1. [Red-Green-Refactor Cycle](./ex-so-de-tedrdeve__02-red-green-refactor-cycle.md)
2. [Unit Testing Fundamentals](./ex-so-de-tedrdeve__04-unit-testing-fundamentals.md) - Component logic testing
3. [Test Doubles](./ex-so-de-tedrdeve__05-test-doubles.md) - Mocking API calls
4. [Testing Patterns](./ex-so-de-tedrdeve__06-testing-patterns.md) - Snapshot testing for components
5. [End-to-End Testing](./ex-so-de-tedrdeve__10-end-to-end-testing.md) - User flow testing with Playwright
6. [Decision Trees and Best Practices](./ex-so-de-tedrdeve__15-decision-trees-and-best-practices.md) - When to use E2E vs integration tests

**For QA Engineers**:

Focus on comprehensive testing strategies:

1. [Test Types and Pyramid](./ex-so-de-tedrdeve__03-test-types-and-pyramid.md) - Testing strategy overview
2. [Integration Testing](./ex-so-de-tedrdeve__09-integration-testing.md) - Cross-component verification
3. [End-to-End Testing](./ex-so-de-tedrdeve__10-end-to-end-testing.md) - Full system testing
4. [Testing Patterns](./ex-so-de-tedrdeve__06-testing-patterns.md) - Table-driven and parameterized tests
5. [Decision Trees and Best Practices](./ex-so-de-tedrdeve__15-decision-trees-and-best-practices.md) - Coverage strategies

**For Software Architects**:

Focus on testing strategies and system design:

1. [Introduction and Philosophy](./ex-so-de-tedrdeve__01-introduction-and-philosophy.md) - TDD benefits for design
2. [Test Types and Pyramid](./ex-so-de-tedrdeve__03-test-types-and-pyramid.md) - Architectural testing strategy
3. [Decision Trees and Best Practices](./ex-so-de-tedrdeve__15-decision-trees-and-best-practices.md) - Test observability, coverage planning
4. [Integration Testing](./ex-so-de-tedrdeve__09-integration-testing.md) - Contract testing for bounded contexts
5. [TDD and DDD](./ex-so-de-tedrdeve__12-tdd-and-ddd.md) - Aligning tests with domain boundaries
6. [TDD in Nx Monorepo](./ex-so-de-tedrdeve__16-tdd-in-nx-monorepo.md) - Monorepo testing architecture

### By Codebase Maturity: Greenfield vs. Brownfield

**For Greenfield Projects** (starting from scratch):

Full TDD from day one:

1. [Introduction and Philosophy](./ex-so-de-tedrdeve__01-introduction-and-philosophy.md) - Why TDD from the start
2. [Red-Green-Refactor Cycle](./ex-so-de-tedrdeve__02-red-green-refactor-cycle.md) - Establish rhythm early
3. [Unit Testing Fundamentals](./ex-so-de-tedrdeve__04-unit-testing-fundamentals.md) - Build testable code from start
4. [Test Data Builders](./ex-so-de-tedrdeve__07-test-data-builders.md) - Set up fixture infrastructure
5. [TDD Workflow Checklist Template](./templates/ex-so-de-tedrdeve-te__tdd-workflow-checklist.md) - Establish team practices
6. [TDD in Nx Monorepo](./ex-so-de-tedrdeve__16-tdd-in-nx-monorepo.md) - Configure testing from start

**For Brownfield Projects** (existing codebase without tests):

Incremental adoption of TDD:

1. [Legacy Code and Characterization Tests](./ex-so-de-tedrdeve__13-legacy-code-and-characterization-tests.md) ⭐ **Start Here** - Working with untested code
2. [Red-Green-Refactor Cycle](./ex-so-de-tedrdeve__02-red-green-refactor-cycle.md) - Apply TDD to new features
3. [Refactoring with Tests](./ex-so-de-tedrdeve__14-refactoring-with-tests.md) - Safe improvement of existing code
4. [Integration Testing](./ex-so-de-tedrdeve__09-integration-testing.md) - High-level tests for legacy boundaries
5. [Coverage Planning Canvas Template](./templates/ex-so-de-tedrdeve-te__coverage-planning-canvas.md) - Prioritize critical paths
6. [Decision Trees and Best Practices](./ex-so-de-tedrdeve__15-decision-trees-and-best-practices.md) - Practical strategies for incremental adoption

**Migration Strategy**:

1. Start with characterization tests to lock in current behavior
2. Apply TDD to all new features and bug fixes
3. Add tests before refactoring any existing code
4. Gradually increase coverage in high-risk areas
5. Use Strangler Fig pattern to replace legacy code with TDD-developed replacements

### By Domain: Islamic Finance, Business Logic, APIs

**For Islamic Finance Applications**:

Testing Sharia-compliant business rules:

1. [Introduction and Philosophy](./ex-so-de-tedrdeve__01-introduction-and-philosophy.md) - TDD for regulatory compliance
2. [Red-Green-Refactor Cycle](./ex-so-de-tedrdeve__02-red-green-refactor-cycle.md) - Zakat calculation example
3. [TDD and DDD](./ex-so-de-tedrdeve__12-tdd-and-ddd.md) - Islamic finance domain models (Murabaha, Sukuk)
4. [Test Data Builders](./ex-so-de-tedrdeve__07-test-data-builders.md) - Builders for Islamic financial instruments
5. [Testing Patterns](./ex-so-de-tedrdeve__06-testing-patterns.md) - Table-driven tests for multiple Zakat asset types
6. [TDD and Functional Programming](./ex-so-de-tedrdeve__11-tdd-and-functional-programming.md) - Property-based testing for Riba detection

**Example Domains Covered**:

- **Zakat Calculation**: Nisab thresholds, Hawl periods, asset types
- **Halal Certification**: Supply chain tracking, certification authority verification
- **Islamic Accounting**: Riba detection, profit-sharing ratios, contract validity
- **Murabaha**: Cost-plus financing, profit calculations
- **Sukuk**: Asset-backed securities, compliance verification
- **Takaful**: Risk sharing, surplus distribution
- **Waqf**: Endowment management, beneficiary allocation

**Islamic Finance Scholarly References**:

The Islamic finance examples throughout this documentation are based on established principles from Islamic jurisprudence and contemporary banking practice. For authoritative guidance on these concepts, consult:

- **Zakat**: Traditional rate of 2.5% (1/40th) on wealth above nisab threshold (equivalent to 85-87.48 grams of gold). See [Islamic Relief Zakat Calculator](https://islamic-relief.org/zakat-calculator/) and [DailyNisab](https://dailynisab.org/) for current calculations.

- **Murabaha Contracts**: Cost-plus financing with disclosed markup. Academic treatment in:
  - Abbasi, T., & Linnenluecke, M. K. (2023). ["An Overview of Islamic Accounting: The Murabaha Contract"](https://www.mdpi.com/1911-8074/16/7/335). _Journal of Risk and Financial Management, 16_(7), 335.
  - [Institute of Islamic Banking and Insurance - Murabaha](https://islamic-banking.com/murabaha/)

- **Comprehensive References**:
  - El-Gamal, M. A. (2006). _Islamic Finance: Law, Economics, and Practice_. Cambridge University Press.
  - Nethercott, C., & Eisenberg, D. (2012). [_Islamic Finance: Law and Practice_](https://global.oup.com/academic/product/islamic-finance-9780198725237). Oxford University Press.
  - Hassan, M. K. (Ed.). [_Handbook of Islamic Banking_](https://www.isfin.net/sites/isfin.com/files/handbook_of_islamic_banking.pdf). Edward Elgar Publishing.

**Note**: The specific business rules in code examples (e.g., markup limits, validation thresholds) are simplified for pedagogical purposes and should not be taken as authoritative Islamic legal rulings. Consult qualified Islamic scholars and regulatory bodies for production implementations.

## Relationship to Other Documentation

TDD integrates with other software design approaches in this repository:

### Domain-Driven Design (DDD)

**[Domain-Driven Design Documentation](../../architecture/domain-driven-design-ddd/README.md)**

- **Strategic Testing**: Tests verify bounded context boundaries and integration patterns
- **Aggregate Testing**: TDD ensures aggregate invariants are enforced ([File 12](./ex-so-de-tedrdeve__12-tdd-and-ddd.md))
- **Value Object Testing**: Immutability and equality tests for domain primitives
- **Domain Events**: Event-driven testing verifies business occurrences
- **Ubiquitous Language**: Test names use domain terminology from glossary

**Key Integration**: File 12 provides comprehensive guidance on testing DDD patterns.

### C4 Architecture Model

**[C4 Architecture Model Documentation](../../architecture/c4-architecture-model/README.md)**

- **Component Testing**: Unit tests align with C4 Component diagrams
- **Container Testing**: Integration tests verify container interactions
- **System Testing**: E2E tests validate system context behavior
- **Test Mapping**: [File 16](./ex-so-de-tedrdeve__15-decision-trees-and-best-practices.md) maps test types to C4 levels

**Test Coverage by C4 Level**:

- **System Context**: E2E tests for external integrations
- **Container**: API integration tests, contract testing
- **Component**: Unit tests for business logic components
- **Code**: Function-level unit tests

### Functional Programming

**[Functional Programming Principles](../../../../../governance/development/pattern/functional-programming.md)**

- **Pure Function Testing**: Simple assertions without mocking ([File 11](./ex-so-de-tedrdeve__11-tdd-and-functional-programming.md))
- **Property-Based Testing**: Verify invariants across input ranges with fast-check
- **Immutability Testing**: Value equality and structural sharing
- **Railway-Oriented Programming**: Testing Result/Either types for error handling
- **Composition Testing**: Verify composed functions maintain properties

**Key Integration**: File 11 shows how TDD naturally fits functional programming paradigms.

### Implementation Workflow

**[Implementation Workflow](../../../../../governance/development/workflow/implementation.md)**

TDD aligns with the "Make it work → Make it right → Make it fast" philosophy:

1. **Make it work** (Red-Green): Write failing test, make it pass with simplest implementation
2. **Make it right** (Refactor): Improve design while tests provide safety net
3. **Make it fast** (Optimize): Measure performance with tests ensuring correctness

TDD provides the feedback loops and safety net that enable this iterative workflow.

## Further Learning Resources

### Books

- **Kent Beck, "Test Driven Development: By Example" (2002)** - The foundational text, still highly relevant
- **Steve Freeman & Nat Pryce, "Growing Object-Oriented Software, Guided by Tests" (2009)** - Advanced TDD for enterprise systems
- **Michael Feathers, "Working Effectively with Legacy Code" (2004)** - TDD techniques for existing codebases
- **Gerard Meszaros, "xUnit Test Patterns" (2007)** - Comprehensive test pattern catalog
- **Mark Seemann, "Code That Fits in Your Head" (2021)** - Modern TDD with functional programming influences

### Online Resources

- **Martin Fowler's Testing Articles**: [martinfowler.com/testing](https://martinfowler.com/testing/) - Canonical testing guidance
- **Kent Beck on TDD**: Original writings and talks on TDD philosophy
- **TestPyramid.com**: Interactive guide to testing strategies
- **fast-check Documentation**: Property-based testing for JavaScript/TypeScript

### Related Repository Documentation

- **[Domain-Driven Design](../../architecture/domain-driven-design-ddd/README.md)** - Testing domain models
- **[C4 Architecture Model](../../architecture/c4-architecture-model/README.md)** - Architectural testing strategies
- **[Functional Programming Principles](../../../../../governance/development/pattern/functional-programming.md)** - FP testing approaches
- **[Implementation Workflow](../../../../../governance/development/workflow/implementation.md)** - TDD in development process
- **[Code Quality Standards](../../../../../governance/development/quality/code.md)** - Testing in quality assurance
- **[Nx Monorepo Documentation](../../../../../docs/reference/re__monorepo-structure.md)** - Testing in monorepo context

## Document Metadata

- **Category**: Explanation
- **Subcategory**: Software Design
- **Tags**: Test-Driven Development, TDD, Red-Green-Refactor, Testing, Unit Tests, Integration Tests, Property-Based Testing, Software Quality, Islamic Finance
- **Last Updated**: 2026-01-20
- **Status**: Active
- **Related Documentation**:
  - [Domain-Driven Design](../../architecture/domain-driven-design-ddd/README.md)
  - [C4 Architecture Model](../../architecture/c4-architecture-model/README.md)
  - [Functional Programming Principles](../../../../../governance/development/pattern/functional-programming.md)
  - [Implementation Workflow](../../../../../governance/development/workflow/implementation.md)
- **Islamic Finance Examples**: Zakat, Halal Certification, Islamic Accounting, Murabaha, Sukuk, Takaful, Waqf
- **Learning Paths**: 7 paths covering quick start, practical application, deep dive, paradigm-specific, role-specific, codebase maturity, and domain-specific guidance
