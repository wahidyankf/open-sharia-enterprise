# TDD Workflow Checklist

## Metadata

- **Parent Directory**: [Test-Driven Development (TDD)](../README.md)
- **Main Topic**: [Test-Driven Development (TDD)](../README.md)
- **Related Templates**:
  - [Unit Test Template](./ex-sode-tedrdeve-te__unit-test-template.md)
  - [Coverage Planning Canvas](./ex-sode-tedrdeve-te__coverage-planning-canvas.md)
  - [Test Organization Template](./ex-sode-tedrdeve-te__test-organization-template.md)
- **Use Case**: Step-by-step checklist for practicing TDD
- **Template Size**: ~12 KB
- **Complexity**: Beginner to Intermediate

## Overview

This checklist provides a systematic workflow for practicing Test-Driven Development following the Red-Green-Refactor cycle. Use this checklist to ensure you're following TDD discipline and not skipping important steps.

## Before Starting TDD Session

### Environment Setup

- [ ] Development environment is ready (IDE, test runner, dependencies installed)
- [ ] Test framework configured (Jest, Vitest, or similar)
- [ ] Can run tests with single command (`npm test` or similar)
- [ ] Tests run fast (under 2 seconds for small test suites)
- [ ] Test watcher enabled for instant feedback (optional but recommended)

### Planning

- [ ] Requirement or user story is clear
- [ ] Acceptance criteria defined
- [ ] Identified smallest testable behavior to implement
- [ ] Understand the domain model and business rules
- [ ] Know what "done" looks like for this feature

### Test Data Preparation

- [ ] Test data builders available (or plan to create them)
- [ ] Example test data prepared for common scenarios
- [ ] Edge cases identified
- [ ] Error cases identified

## Red-Green-Refactor Cycle

Repeat this cycle for each small behavior until feature is complete.

### Step 1: RED - Write a Failing Test

- [ ] Write a single test for the smallest next behavior
- [ ] Test name describes the expected behavior
- [ ] Test follows AAA pattern (Arrange-Act-Assert)
- [ ] Test is focused on one behavior only
- [ ] Test uses meaningful variable names
- [ ] Test uses test data builders or fixtures for complex objects
- [ ] Run the test and verify it FAILS for the right reason
- [ ] Failure message is clear and helpful
- [ ] No compilation errors (production code may not exist yet, but types should)

**Common Mistakes to Avoid**:

- [ ] Did NOT write multiple tests at once
- [ ] Did NOT write production code before test
- [ ] Did NOT copy-paste test without understanding it
- [ ] Did NOT make test too complex

### Step 2: GREEN - Make the Test Pass (Minimal Implementation)

- [ ] Write the simplest code to make the test pass
- [ ] Focus on making test pass, not on perfect design
- [ ] Use fake implementations or hardcoded values if needed
- [ ] Run the test and verify it PASSES
- [ ] Run ALL tests and verify they all pass
- [ ] No skipped or ignored tests

**Implementation Strategies**:

- [ ] Fake it: Return hardcoded value
- [ ] Obvious implementation: Write the straightforward code
- [ ] Triangulation: Add more tests to force generalization

**Common Mistakes to Avoid**:

- [ ] Did NOT over-engineer the solution
- [ ] Did NOT add extra features not tested
- [ ] Did NOT refactor while still in green phase
- [ ] Did NOT skip running all tests

### Step 3: REFACTOR - Improve the Code

- [ ] Identify code smells or duplication
- [ ] Refactor production code for better design
- [ ] Refactor test code for better readability
- [ ] Extract test data builders if needed
- [ ] Apply design patterns if appropriate
- [ ] Improve naming (variables, functions, classes)
- [ ] Run all tests after each refactoring step
- [ ] Verify all tests still pass

**Refactoring Checklist**:

- [ ] Remove duplication (DRY principle)
- [ ] Improve naming for clarity
- [ ] Extract methods/functions for better organization
- [ ] Simplify complex conditionals
- [ ] Apply SOLID principles where appropriate
- [ ] Ensure immutability for value objects
- [ ] Use domain language in code

**Common Mistakes to Avoid**:

- [ ] Did NOT change behavior during refactoring
- [ ] Did NOT skip running tests after refactoring
- [ ] Did NOT refactor without tests passing
- [ ] Did NOT make refactoring too big (keep changes small)

### Step 4: REPEAT

- [ ] Commit the changes (small, focused commits)
- [ ] Identify next smallest behavior to implement
- [ ] Go back to RED phase

## After Completing Feature

### Code Quality

- [ ] All tests pass
- [ ] No skipped or ignored tests
- [ ] No commented-out code
- [ ] Code coverage is adequate (aim for 80%+ for critical paths)
- [ ] No unnecessary complexity
- [ ] Code is readable and self-documenting
- [ ] Domain language used consistently

### Test Quality

- [ ] Test names clearly describe behavior
- [ ] Tests are independent (can run in any order)
- [ ] Tests are fast (unit tests under 100ms each)
- [ ] No flaky tests (tests pass consistently)
- [ ] Tests use appropriate assertions
- [ ] Test data is clear and minimal
- [ ] Edge cases covered
- [ ] Error cases covered

### Documentation

- [ ] Public API documented (if creating library)
- [ ] Complex business rules documented
- [ ] README updated (if needed)
- [ ] Examples added for common use cases

### Integration

- [ ] Run full test suite
- [ ] Integration tests added (if needed)
- [ ] CI pipeline passes
- [ ] Code reviewed (if team practice)
- [ ] Merged to main branch

## Islamic Finance TDD Example Checklist

### Example: Implementing Zakat Calculator

#### Planning Phase

- [ ] **Requirement**: Calculate Zakat (2.5%) on wealth above nisab threshold
- [ ] **Acceptance Criteria**:
  - [ ] Zakat is 2.5% of wealth when wealth >= nisab
  - [ ] Zakat is zero when wealth < nisab
  - [ ] Currency is preserved in result
  - [ ] Negative amounts are rejected

#### Cycle 1: Zakat calculation above nisab

**RED**:

- [ ] Write test: "should calculate 2.5% when wealth is above nisab"
- [ ] Test expects 250 USD zakat for 10,000 USD wealth (nisab 2,000 USD)
- [ ] Run test: FAILS (calculateZakat method doesn't exist)

**GREEN**:

- [ ] Create ZakatCalculator class
- [ ] Implement calculateZakat method: return wealth \* 0.025
- [ ] Run test: PASSES

**REFACTOR**:

- [ ] Extract ZAKAT_RATE constant (0.025)
- [ ] Run tests: PASS

#### Cycle 2: Zakat is zero below nisab

**RED**:

- [ ] Write test: "should return zero when wealth is below nisab"
- [ ] Run test: FAILS (returns 25 USD instead of 0)

**GREEN**:

- [ ] Add if condition: if (wealth < nisab) return zero
- [ ] Run test: PASSES

**REFACTOR**:

- [ ] Use Money.isLessThan method instead of < operator
- [ ] Run tests: PASS

#### Cycle 3: Wealth equals nisab (boundary case)

**RED**:

- [ ] Write test: "should calculate zakat when wealth equals nisab"
- [ ] Run test: PASSES (no refactoring needed, existing code handles it)

**GREEN**:

- [ ] Already passes (use >= in condition)

**REFACTOR**:

- [ ] No refactoring needed

#### Cycle 4: Currency preservation

**RED**:

- [ ] Write test: "should preserve currency in result"
- [ ] Test with EUR currency
- [ ] Run test: PASSES (already implemented correctly)

**GREEN**:

- [ ] Already passes

**REFACTOR**:

- [ ] No refactoring needed

#### Completion

- [ ] All tests pass (4/4)
- [ ] Coverage: 100% of ZakatCalculator
- [ ] Commit: "feat(zakat): add zakat calculator with nisab threshold"

## Team Adoption Checklist

### For Developers New to TDD

- [ ] Understand the Red-Green-Refactor cycle
- [ ] Practice on small katas (String Calculator, FizzBuzz, etc.)
- [ ] Pair program with experienced TDD practitioner
- [ ] Start with simple units (pure functions, value objects)
- [ ] Use this checklist for first 10 features
- [ ] Gradually internalize the workflow

### For Teams Adopting TDD

- [ ] Team agrees on TDD as standard practice
- [ ] Test framework selected and configured
- [ ] Test coverage goals defined
- [ ] Code review includes test quality checks
- [ ] CI pipeline enforces tests must pass
- [ ] Pair programming encouraged for knowledge sharing
- [ ] Regular retrospectives on TDD practice
- [ ] Celebrate successes and learn from failures

## Code Review Checklist for Tests

When reviewing code written with TDD, check:

### Test Quality

- [ ] Every production code change has corresponding test
- [ ] Tests are clear and describe behavior
- [ ] Tests follow AAA pattern
- [ ] Tests are independent
- [ ] No duplicate test code (use test builders)
- [ ] Edge cases are tested
- [ ] Error cases are tested

### Implementation Quality

- [ ] Code is simple and focused
- [ ] No over-engineering
- [ ] No untested code paths
- [ ] No commented-out tests
- [ ] No skipped tests without justification

### TDD Process Evidence

- [ ] Small, focused commits
- [ ] Test commits precede implementation commits (if separate)
- [ ] Refactoring commits don't change behavior
- [ ] Commit messages describe what and why

## Common TDD Mistakes and Solutions

### Mistake 1: Writing Multiple Tests Before Implementation

**Problem**: Writing many tests at once before implementing

**Solution**: Write ONE test, make it pass, then write next test

### Mistake 2: Skipping the Red Phase

**Problem**: Writing test that passes immediately

**Solution**: Always run test and verify it fails first

### Mistake 3: Overcomplicating the Implementation

**Problem**: Writing perfect solution on first try

**Solution**: Use fake-it or obvious implementation, refactor later

### Mistake 4: Not Refactoring

**Problem**: Leaving duplicate or messy code

**Solution**: Always refactor after green, even small improvements

### Mistake 5: Large Test Jumps

**Problem**: Testing too much behavior at once

**Solution**: Break into smaller, incremental tests

### Mistake 6: Testing Implementation Details

**Problem**: Tests coupled to implementation

**Solution**: Test behavior, not implementation

### Mistake 7: Slow Tests

**Problem**: Tests take too long to run

**Solution**: Keep unit tests fast, use test doubles for dependencies

### Mistake 8: Not Running All Tests

**Problem**: Only running current test

**Solution**: Always run full suite before committing

## Daily TDD Practice Checklist

### Morning Routine

- [ ] Pull latest changes
- [ ] Run full test suite to verify starting point
- [ ] Review work plan for the day
- [ ] Identify first behavior to test

### During Development

- [ ] Follow Red-Green-Refactor for each behavior
- [ ] Commit after each complete cycle (or few cycles)
- [ ] Take breaks between cycles to stay focused
- [ ] Keep test runs fast (under 2 seconds)

### End of Day

- [ ] All tests pass
- [ ] No work-in-progress tests left failing
- [ ] Commit or stash changes
- [ ] Push to remote (if team practice)
- [ ] Note where you left off for tomorrow

## Summary

**Key Principles**:

1. **Red**: Write failing test first
2. **Green**: Minimal code to pass
3. **Refactor**: Improve design while keeping tests green
4. **Small Steps**: One behavior at a time
5. **Fast Feedback**: Tests run in seconds
6. **Always Run All Tests**: Prevent regressions
7. **Commit Frequently**: Small, focused commits

**Benefits**:

- [ ] High test coverage (natural outcome of process)
- [ ] Better design (refactoring with safety net)
- [ ] Fewer bugs (tests catch issues early)
- [ ] Living documentation (tests describe behavior)
- [ ] Confidence in changes (tests verify correctness)

Use this checklist until TDD becomes second nature. Print it, keep it visible, and refer to it frequently during development.
