# Anti-Patterns in Development Patterns

> **Companion Document**: For positive guidance on what to do, see [Best Practices](./best-practices.md)

## Overview

Understanding common mistakes in pattern application helps teams build more maintainable, reliable, and effective systems. These anti-patterns cause complexity, quality issues, and workflow problems.

## Purpose

This document provides:

- Common anti-patterns in pattern application
- Examples of problematic implementations
- Solutions and corrections for each anti-pattern
- Workflow and code quality considerations

## Common Anti-Patterns

### Anti-Pattern 1: God Agent in Maker-Checker-Fixer

**Problem**: Single agent trying to create, validate, and fix content.

**Bad Example:**

```yaml
---
name: docs-everything
description: Creates, validates, and fixes documentation
tools: [Read, Write, Edit, Glob, Grep, Bash, WebFetch]
---
```

**Solution:**

```yaml
# Separate agents with single responsibilities
---
name: docs-maker
tools: [Read, Write, Glob]
---
---
name: docs-checker
tools: [Read, Glob, Grep, Write, Bash]
---
---
name: docs-fixer
tools: [Read, Edit, Glob, Grep, Write, Bash]
---
```

**Rationale:**

- Single responsibility per agent
- Easier to test and maintain
- Clear workflow boundaries
- Reusable across different contexts

### Anti-Pattern 2: Skipping Validation Workflow

**Problem**: Deploying content without running checker.

**Bad Example:**

```markdown
1. Maker creates content
2. Deploy immediately (NO VALIDATION!)
```

**Solution:**

```markdown
1. Maker creates content
2. Checker validates quality → audit report
3. Review audit report
4. Fixer applies fixes (if needed)
5. Re-check (optional)
6. Deploy
```

**Rationale:**

- Catches issues before publication
- Provides audit trail
- Systematic quality improvement
- Prevents broken production content

### Anti-Pattern 3: Applying All Fixes Blindly

**Problem**: Fixer applies fixes without confidence assessment.

**Bad Example:**

```bash
# Apply all fixes (DO NOT DO THIS)
for finding in $FINDINGS; do
  apply_fix "$finding"  # No confidence check!
done
```

**Solution:**

```bash
# Assess confidence before fixing
if [ "$CONFIDENCE" = "HIGH" ]; then
  apply_fix "$finding"
elif [ "$CONFIDENCE" = "MEDIUM" ]; then
  report_manual_review "$finding"
elif [ "$CONFIDENCE" = "FALSE_POSITIVE" ]; then
  report_false_positive "$finding"
fi
```

**Rationale:**

- Prevents incorrect automated fixes
- Requires human judgment for uncertainty
- Safe remediation process
- Maintains quality control

### Anti-Pattern 4: Mutating Shared State

**Problem**: Mutating data structures instead of creating new ones.

**Bad Example:**

```typescript
// Mutation (problematic)
function processItems(items: Item[]): void {
  items.forEach((item) => {
    item.processed = true; // Mutates original!
  });
}

const original = [{ id: 1, processed: false }];
processItems(original);
// original is now mutated - unexpected side effects!
```

**Solution:**

```typescript
// Immutable approach
function processItems(items: Item[]): Item[] {
  return items.map((item) => ({
    ...item,
    processed: true,
  }));
}

const original = [{ id: 1, processed: false }];
const processed = processItems(original);
// original unchanged, new array returned
```

**Rationale:**

- Prevents unexpected side effects
- Easier to reason about code
- Supports functional composition
- Better testability

### Anti-Pattern 5: Impure Functions with Hidden Dependencies

**Problem**: Functions depending on external state.

**Bad Example:**

```typescript
// Impure function (hidden dependency)
let taxRate = 0.1;
function calculateTotal(items: Item[]): number {
  return items.reduce((sum, item) => sum + item.price, 0) * (1 + taxRate);
}

// Change in global state affects function output
taxRate = 0.2;
calculateTotal(items); // Different result for same input!
```

**Solution:**

```typescript
// Pure function (explicit dependency)
function calculateTotal(items: Item[], taxRate: number): number {
  return items.reduce((sum, item) => sum + item.price, 0) * (1 + taxRate);
}

// Deterministic - same inputs always produce same output
calculateTotal(items, 0.1);
calculateTotal(items, 0.2);
```

**Rationale:**

- Deterministic behavior
- Easier to test
- No hidden dependencies
- Supports memoization

### Anti-Pattern 6: Monolithic Functions

**Problem**: Large functions doing too many things.

**Bad Example:**

```typescript
// Monolithic function (DO NOT DO THIS)
function processUserData(users: User[]): ProcessedData {
  // 200 lines of complex logic
  // Validation, transformation, filtering, sorting, aggregation...
  // All mixed together
}
```

**Solution:**

```typescript
// Composed from small functions
const validateUser = (user: User) => user.email && user.name;
const isActive = (user: User) => user.status === "active";
const toDTO = (user: User) => ({ id: user.id, name: user.name });

function processUserData(users: User[]): ProcessedData {
  return users.filter(validateUser).filter(isActive).map(toDTO);
}
```

**Rationale:**

- Small, testable units
- Reusable building blocks
- Clear intent and naming
- Easier to maintain

### Anti-Pattern 7: Ignoring False Positive Feedback

**Problem**: Not using fixer reports to improve checkers.

**Bad Example:**

```markdown
# Workflow (missing feedback loop)

1. Checker flags issue (potential false positive)
2. Fixer detects false positive
3. Skip fix, move on
4. NO feedback to checker
5. Same false positive next run (repeated waste!)
```

**Solution:**

```markdown
# Workflow (with improvement loop)

1. Checker flags issue
2. Fixer re-validates → detects FALSE_POSITIVE
3. Fixer reports false positive with suggestion
4. User updates checker logic
5. Next run: improved accuracy
```

**Rationale:**

- Continuous improvement
- Reduces false positive rate
- Checkers become more accurate
- Systematic quality enhancement

### Anti-Pattern 8: No Criticality Categorization

**Problem**: Treating all issues as equally important.

**Bad Example:**

```markdown
## Issues (15)

- Broken authentication endpoint
- Missing alt text
- Typo in paragraph
- SQL injection vulnerability
- Extra whitespace
  (All treated equally - no prioritization!)
```

**Solution:**

```markdown
## CRITICAL Issues (2)

- Broken authentication endpoint
- SQL injection vulnerability

## HIGH Issues (4)

- Missing alt text on images

## MEDIUM Issues (6)

- Typo in paragraph

## LOW Issues (3)

- Extra whitespace
```

**Rationale:**

- Clear prioritization
- Critical issues fixed first
- Efficient resource allocation
- Aligns with fix priority matrix

### Anti-Pattern 9: Side Effects Throughout Codebase

**Problem**: Side effects mixed with business logic.

**Bad Example:**

```typescript
// Side effects everywhere (DO NOT DO THIS)
function calculateDiscount(user: User): number {
  db.logAccess(user.id); // Side effect!
  const discount = user.loyaltyPoints / 100;
  emailService.send(user.email, "Discount calculated"); // Side effect!
  return discount;
}
```

**Solution:**

```typescript
// Pure core
function calculateDiscount(loyaltyPoints: number): number {
  return loyaltyPoints / 100;
}

// Imperative shell (side effects at boundary)
async function applyDiscountWithLogging(user: User): Promise<number> {
  await db.logAccess(user.id); // Side effect isolated
  const discount = calculateDiscount(user.loyaltyPoints); // Pure
  await emailService.send(user.email, "Discount calculated"); // Side effect isolated
  return discount;
}
```

**Rationale:**

- Easier to test pure logic
- Side effects isolated and controlled
- Clear separation of concerns
- Better code organization

### Anti-Pattern 10: Using Maker Instead of Fixer

**Problem**: Using maker for validation-driven fixes.

**Bad Example:**

```markdown
User: "Fix issues from the latest audit report"
→ Use docs-maker (WRONG - maker is for user-driven creation!)
```

**Solution:**

```markdown
User: "Fix issues from the latest audit report"
→ Use docs-fixer (CORRECT - fixer is validation-driven)
```

**Rationale:**

- Clear workflow boundaries
- Makers handle comprehensive creation
- Fixers handle validated remediation
- Prevents tool misuse

## Summary of Anti-Patterns

| Anti-Pattern                 | Problem                     | Solution                          |
| ---------------------------- | --------------------------- | --------------------------------- |
| **God Agent**                | Too many responsibilities   | Separate maker/checker/fixer      |
| **Skipping Validation**      | No quality gate             | Always run checker                |
| **Blind Fixes**              | Incorrect automated changes | Assess confidence first           |
| **Mutating Shared State**    | Unexpected side effects     | Use immutable operations          |
| **Impure Functions**         | Hidden dependencies         | Explicit parameters               |
| **Monolithic Functions**     | Hard to test and maintain   | Compose small functions           |
| **Ignoring False Positives** | Repeated errors             | Feedback loop for improvement     |
| **No Criticality**           | Equal treatment of issues   | Categorize by importance          |
| **Side Effects Everywhere**  | Mixed concerns              | Functional core, imperative shell |
| **Wrong Tool Selection**     | Mismatched workflow         | Maker vs fixer clarity            |

## Related Documentation

- [Maker-Checker-Fixer Pattern](./maker-checker-fixer.md) - Complete pattern documentation
- [Functional Programming Practices](./functional-programming.md) - Functional programming guide
- [Best Practices](./best-practices.md) - Recommended patterns
- [Criticality Levels Convention](../quality/criticality-levels.md) - Issue prioritization
- [Fixer Confidence Levels Convention](../quality/fixer-confidence-levels.md) - Confidence assessment

## Conclusion

Avoiding these anti-patterns ensures:

- Clear agent responsibilities
- Systematic quality validation
- Safe automated remediation
- Maintainable functional code
- Continuous improvement cycles
- Effective prioritization
- Isolated side effects
- Correct tool selection

When applying patterns, ask: **Am I adding clarity or complexity?** If complexity, refactor to follow pattern development best practices.

## Principles Applied

- **Immutability Over Mutability**: Avoid mutation, use immutable operations
- **Pure Functions Over Side Effects**: Isolate side effects, pure core logic
- **Simplicity Over Complexity**: Single responsibility, small functions
- **Automation Over Manual**: Systematic workflows, confidence-based fixing
