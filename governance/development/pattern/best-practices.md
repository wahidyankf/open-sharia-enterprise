# Best Practices for Development Patterns

> **Companion Document**: For common mistakes to avoid, see [Anti-Patterns](./anti-patterns.md)

## Overview

## Principles Respected

This companion document respects:

- **[Simplicity Over Complexity](../../principles/general/simplicity-over-complexity.md)**: Provides practical examples of simple vs complex approaches
- **[Explicit Over Implicit](../../principles/software-engineering/explicit-over-implicit.md)**: Makes patterns and anti-patterns explicit through clear examples

## Conventions Implemented/Respected

This companion document supports the conventions in this directory by providing practical examples and guidance.

This document outlines best practices for applying the Maker-Checker-Fixer pattern and functional programming practices. Following these practices ensures high-quality, maintainable, and reliable code.

## Purpose

## Principles Respected

This companion document respects:

- **[Simplicity Over Complexity](../../principles/general/simplicity-over-complexity.md)**: Provides practical examples of simple vs complex approaches
- **[Explicit Over Implicit](../../principles/software-engineering/explicit-over-implicit.md)**: Makes patterns and anti-patterns explicit through clear examples

## Conventions Implemented/Respected

This companion document supports the conventions in this directory by providing practical examples and guidance.

Provide actionable guidance for:

- Maker-Checker-Fixer workflow execution
- Functional programming implementation
- Pattern selection and application
- Code organization and maintainability
- Quality workflow integration

## Best Practices

### Practice 1: Single Responsibility Per Agent Role

**Principle**: Each agent in Maker-Checker-Fixer has one clear responsibility.

**Good Example:**

```yaml
# Maker - Creates content only
---
name: docs-maker
description: Creates documentation following conventions
tools: [Read, Write, Glob]
---
# Checker - Validates content only
---
name: docs-checker
description: Validates documentation quality
tools: [Read, Glob, Grep, Write, Bash]
---
# Fixer - Applies fixes only
---
name: docs-fixer
description: Applies validated fixes
tools: [Read, Edit, Glob, Grep, Write, Bash]
---
```

**Bad Example:**

```yaml
# God agent (DO NOT DO THIS)
---
name: docs-everything
description: Creates, validates, and fixes documentation
tools: [Read, Write, Edit, Glob, Grep, Bash]
---
```

**Rationale:**

- Clear separation of concerns
- Easier to test and maintain
- Reusable across workflows
- Prevents responsibility overlap

### Practice 2: Use Makers for User-Driven Content Creation

**Principle**: Invoke makers when user requests content creation or updates.

**Good Example:**

```markdown
User: "Create new tutorial about Docker"
→ Use docs-maker (user-driven creation)

User: "Add section on volumes to Docker tutorial"
→ Use docs-maker (user-driven update)
```

**Bad Example:**

```markdown
User: "Create new tutorial about Docker"
→ Use docs-fixer (WRONG - fixer is validation-driven, not user-driven!)
```

**Rationale:**

- Makers handle comprehensive creation
- Makers update all dependencies
- Makers provide production-ready content
- Clear workflow boundaries

### Practice 3: Use Checkers for Validation Workflow

**Principle**: Run checkers after creation or before publication.

**Good Example:**

```markdown
1. Maker creates content
2. User reviews content
3. Checker validates quality → generates audit report
4. User reviews audit report
5. Fixer applies validated fixes (if needed)
```

**Bad Example:**

```markdown
1. Maker creates content
2. Deploy immediately (NO VALIDATION!)
```

**Rationale:**

- Catches issues before publication
- Provides audit trail
- Enables systematic quality improvement
- Validates conventions compliance

### Practice 4: Apply Only HIGH Confidence Fixes Automatically

**Principle**: Fixers skip MEDIUM confidence and FALSE_POSITIVE findings.

**Good Example:**

```bash
# Fixer logic
if [ "$CONFIDENCE" = "HIGH" ]; then
  apply_fix "$finding"
elif [ "$CONFIDENCE" = "MEDIUM" ]; then
  echo "SKIP: Needs manual review"
elif [ "$CONFIDENCE" = "FALSE_POSITIVE" ]; then
  echo "SKIP: False positive detected"
fi
```

**Bad Example:**

```bash
# Apply all fixes blindly (DO NOT DO THIS)
for finding in $FINDINGS; do
  apply_fix "$finding"  # No confidence assessment!
done
```

**Rationale:**

- Safe automated remediation
- Prevents incorrect fixes
- Requires human judgment for uncertainty
- Maintains quality control

### Practice 5: Use Immutable Data Structures

**Principle**: Prefer immutable operations over mutation.

**Good Example:**

```typescript
// Immutable array operations
const newItems = [...items, newItem]; // Spread operator
const filtered = items.filter((x) => x.active); // Returns new array
const mapped = items.map((x) => ({ ...x, processed: true })); // New objects
```

**Bad Example:**

```typescript
// Mutation (avoid when possible)
items.push(newItem); // Mutates original
items[0].processed = true; // Direct mutation
```

**Rationale:**

- Easier to reason about code
- Prevents unexpected side effects
- Supports functional composition
- Improves testability

### Practice 6: Write Pure Functions

**Principle**: Functions should depend only on inputs, not external state.

**Good Example:**

```typescript
// Pure function
function calculateTotal(items: Item[]): number {
  return items.reduce((sum, item) => sum + item.price, 0);
}
```

**Bad Example:**

```typescript
// Impure function (depends on external state)
let discount = 0.1;
function calculateTotal(items: Item[]): number {
  return items.reduce((sum, item) => sum + item.price, 0) * (1 - discount);
}
```

**Rationale:**

- Deterministic output for same input
- Easier to test
- No hidden dependencies
- Supports memoization

### Practice 7: Compose Small Functions

**Principle**: Build complex behavior from small, composable functions.

**Good Example:**

```typescript
const isActive = (user: User) => user.status === "active";
const hasEmail = (user: User) => !!user.email;
const canReceiveEmail = (user: User) => isActive(user) && hasEmail(user);

const emailableUsers = users.filter(canReceiveEmail);
```

**Bad Example:**

```typescript
// Monolithic function
const emailableUsers = users.filter((user) => {
  // 50 lines of complex logic...
});
```

**Rationale:**

- Reusable building blocks
- Easier to test individual functions
- Clear intent and naming
- Supports functional pipelines

### Practice 8: Use Criticality Levels for Prioritization

**Principle**: Checkers categorize findings by criticality (CRITICAL/HIGH/MEDIUM/LOW).

**Good Example:**

```markdown
## CRITICAL Issues (2)

- [ ] Broken authentication endpoint
- [ ] SQL injection vulnerability

## HIGH Issues (5)

- [ ] Missing alt text on images
- [ ] Incorrect frontmatter dates
```

**Bad Example:**

```markdown
## Issues (7)

- Broken authentication endpoint
- Missing alt text on images
- Typo in paragraph
  (All treated equally - no prioritization!)
```

**Rationale:**

- Clear prioritization of fixes
- Critical issues fixed first
- Efficient resource allocation
- Aligns with fix priority matrix

### Practice 9: Iterative Improvement via False Positive Feedback

**Principle**: Use fixer's false positive reports to improve checkers.

**Good Example:**

```markdown
## Workflow

1. Checker flags issue
2. Fixer re-validates → detects FALSE_POSITIVE
3. Fixer reports false positive with improvement suggestion
4. User updates checker logic
5. Next run: improved accuracy
```

**Bad Example:**

```markdown
# Ignore false positives (DO NOT DO THIS)

1. Checker flags issue
2. Fixer detects false positive
3. Skip fix, move on
4. No feedback to checker
5. Same false positive next run
```

**Rationale:**

- Continuous improvement cycle
- Checkers become more accurate over time
- Reduces false positive rate
- Systematic quality enhancement

### Practice 10: Functional Core, Imperative Shell

**Principle**: Pure logic in core, side effects at boundaries.

**Good Example:**

```typescript
// Pure core
function validateUser(user: User): ValidationResult {
  // Pure validation logic
}

// Imperative shell
async function saveUser(user: User): Promise<void> {
  const result = validateUser(user); // Pure
  if (result.isValid) {
    await db.save(user); // Side effect at boundary
  }
}
```

**Bad Example:**

```typescript
// Mixed concerns
function validateAndSaveUser(user: User): void {
  // Validation mixed with database access
  if (user.email && user.name) {
    db.save(user); // Side effect in validation logic!
  }
}
```

**Rationale:**

- Clear separation of pure and impure code
- Easier to test core logic
- Side effects isolated and controlled
- Better code organization

## Related Documentation

- [Maker-Checker-Fixer Pattern](./maker-checker-fixer.md) - Complete pattern documentation
- [Functional Programming Practices](./functional-programming.md) - Functional programming guide
- [Anti-Patterns](./anti-patterns.md) - Common mistakes to avoid
- [Criticality Levels Convention](../quality/criticality-levels.md) - Issue prioritization
- [Fixer Confidence Levels Convention](../quality/fixer-confidence-levels.md) - Confidence assessment

## Summary

Following these best practices ensures:

1. Single responsibility per agent role
2. Use makers for user-driven creation
3. Use checkers for validation workflow
4. Apply only HIGH confidence fixes
5. Use immutable data structures
6. Write pure functions
7. Compose small functions
8. Use criticality levels for prioritization
9. Iterative improvement via feedback
10. Functional core, imperative shell

Patterns applied following these practices are maintainable, reliable, and continuously improving.

## Principles Applied

- **Immutability Over Mutability**: Immutable data structures, pure functions
- **Pure Functions Over Side Effects**: Functional core, imperative shell
- **Simplicity Over Complexity**: Single responsibility, small composable functions
- **Automation Over Manual**: Systematic validation and remediation
