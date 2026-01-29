# Anti-Patterns in Quality Development

> **Companion Document**: For positive guidance on what to do, see [Best Practices](./best-practices.md)

## Overview

## Principles Respected

This companion document respects:

- **[Simplicity Over Complexity](../../principles/general/simplicity-over-complexity.md)**: Provides practical examples of simple vs complex approaches
- **[Explicit Over Implicit](../../principles/software-engineering/explicit-over-implicit.md)**: Makes patterns and anti-patterns explicit through clear examples

## Conventions Implemented/Respected

This companion document supports the conventions in this directory by providing practical examples and guidance.

Understanding common mistakes in quality development helps teams build more reliable, maintainable, and consistent codebases. These anti-patterns cause quality issues, technical debt, and maintenance burden.

## Purpose

## Principles Respected

This companion document respects:

- **[Simplicity Over Complexity](../../principles/general/simplicity-over-complexity.md)**: Provides practical examples of simple vs complex approaches
- **[Explicit Over Implicit](../../principles/software-engineering/explicit-over-implicit.md)**: Makes patterns and anti-patterns explicit through clear examples

## Conventions Implemented/Respected

This companion document supports the conventions in this directory by providing practical examples and guidance.

This document provides:

- Common anti-patterns in quality development
- Examples of problematic implementations
- Solutions and corrections for each anti-pattern
- Quality and maintenance considerations

## Common Anti-Patterns

### Anti-Pattern 1: Manual Quality Checks

**Problem**: Relying on developers to remember quality checks.

**Bad Example:**

```bash
# Developer's manual workflow (error-prone)
# 1. Write code
# 2. Manually run prettier (sometimes forgotten)
# 3. Manually run eslint (sometimes forgotten)
# 4. Commit (with inconsistent formatting!)
```

**Solution:**

```json
// package.json - Automated hooks
{
  "lint-staged": {
    "*.{ts,js}": ["prettier --write", "eslint --fix"]
  }
}
```

**Rationale:**

- Manual checks are forgotten
- Inconsistent code quality
- Wastes code review time
- Automation is reliable

### Anti-Pattern 2: No Issue Prioritization

**Problem**: Treating all findings as equally important.

**Bad Example:**

```markdown
## All Issues (43)

1. Broken authentication (CRITICAL!)
2. Missing alt text
3. Extra whitespace
   ...
4. Optional code cleanup
   (No categorization - what to fix first?)
```

**Solution:**

```markdown
## CRITICAL Issues (1)

- Broken authentication endpoint

## HIGH Issues (8)

- Missing alt text on images

## MEDIUM Issues (15)

- Style inconsistencies

## LOW Issues (19)

- Optional improvements
```

**Rationale:**

- Critical issues need immediate attention
- Clear prioritization
- Efficient resource allocation
- Business impact visibility

### Anti-Pattern 3: Applying Fixes Without Confidence Assessment

**Problem**: Automated fixer applies all fixes without validation.

**Bad Example:**

```bash
# Blind fixes (DO NOT DO THIS)
for finding in $(cat audit.md); do
  apply_fix "$finding"  # No validation!
done
```

**Solution:**

```bash
# Re-validate and assess confidence
for finding in $FINDINGS; do
  if revalidate "$finding"; then
    confidence=$(assess_confidence "$finding")
    if [ "$confidence" = "HIGH" ]; then
      apply_fix "$finding"
    fi
  fi
done
```

**Rationale:**

- Prevents incorrect automated changes
- Validates finding still exists
- Requires human judgment for uncertainty
- Safe remediation

### Anti-Pattern 4: Deleting Content Without Preservation

**Problem**: Removing files during refactoring without saving knowledge.

**Bad Example:**

```bash
# Delete old docs (DO NOT DO THIS)
rm old-architecture.md
rm legacy-api-docs.md
rm deprecated-guide.md
# Knowledge lost forever!
```

**Solution:**

```markdown
## Refactoring Process

1. Read all old docs thoroughly
2. Extract unique knowledge
3. Migrate to new structure
4. Archive originals (not delete)
5. Verify no knowledge lost
```

**Rationale:**

- Documentation is valuable
- Knowledge preserved
- Can reference later
- Respects Documentation First principle

### Anti-Pattern 5: Running All Tests in Pre-Push

**Problem**: Pre-push hook runs entire test suite (slow).

**Bad Example:**

```bash
# .husky/pre-push
nx test  # Runs ALL tests (5+ minutes!)
# Developers skip hook due to slowness
```

**Solution:**

```bash
# .husky/pre-push
nx affected:test --base=origin/main
# Only affected tests (30 seconds)
```

**Rationale:**

- Fast feedback encourages usage
- Runs only relevant tests
- Prevents hook bypass
- Maintains quality gate

### Anti-Pattern 6: Ad-Hoc Validation Logic

**Problem**: Each validator implements different patterns.

**Bad Example:**

```bash
# Validator 1
grep -E "pattern" file

# Validator 2
awk '{print $1}' file | some_command

# Validator 3
python custom_script.py file

# No consistency, hard to maintain
```

**Solution:**

```bash
# Standardized validation pattern
validate_field() {
  local file=$1
  local field=$2
  # Standard extraction and validation
  # Consistent error reporting
  # Reusable across validators
}
```

**Rationale:**

- Consistent validation patterns
- Easier to maintain
- Reduces duplication
- Clear methodology

### Anti-Pattern 7: Ignoring Criticality in Fix Execution

**Problem**: Fixing issues in random order instead of priority.

**Bad Example:**

```bash
# Random fix order (DO NOT DO THIS)
for file in $(ls); do
  fix_issues "$file"  # Low priority might be fixed before critical!
done
```

**Solution:**

```bash
# Priority-based execution
fix_p0_blockers()      # CRITICAL + HIGH confidence
fix_p1_urgent()        # HIGH + HIGH confidence
fix_p2_normal()        # MEDIUM + HIGH confidence
# P3-P4 are suggestions only
```

**Rationale:**

- Blockers fixed first
- Efficient resource use
- Clear escalation
- Business impact aligned

### Anti-Pattern 8: No Quality Gates in CI

**Problem**: CI passes even with quality violations.

**Bad Example:**

```yaml
# .github/workflows/ci.yml
- name: Lint
  run: npm run lint || true # Always passes!

- name: Test
  run: npm test || echo "Tests failed, but continuing..."
```

**Solution:**

```yaml
# .github/workflows/ci.yml
- name: Lint
  run: npm run lint -- --max-warnings=0

- name: Test
  run: nx affected:test --base=origin/main
```

**Rationale:**

- Quality gate enforcement
- Prevents bad code merging
- Team accountability
- Maintains codebase health

### Anti-Pattern 9: Undocumented Validation Rules

**Problem**: Validation rules exist without explanation.

**Bad Example:**

```bash
# Validator
if ! validate_rule_x "$file"; then
  echo "Validation failed"  # Why? What's rule X?
fi
```

**Solution:**

```markdown
## Validation: Rule X - Alt Text Required

**Rule**: All images must have descriptive alt text.

**Rationale**:

- WCAG AA compliance
- Screen reader accessibility
- SEO benefits

**Example**: `<img src="photo.jpg" alt="Description" />`
```

**Rationale:**

- Clear purpose and context
- Easier to maintain
- Educational for team
- Enables informed decisions

### Anti-Pattern 10: Formatting Entire Repo on Every Commit

**Problem**: Pre-commit hook formats all files, not just staged.

**Bad Example:**

```bash
# .husky/pre-commit
prettier --write .  # Formats ALL files (slow!)
git add .           # Stages unintended changes!
```

**Solution:**

```json
// package.json
{
  "lint-staged": {
    "*.md": ["prettier --write"]
  }
}
```

**Rationale:**

- Fast pre-commit (only staged files)
- No unintended changes
- Gradual quality improvement
- Developer-friendly

## Summary of Anti-Patterns

| Anti-Pattern              | Problem                     | Solution                          |
| ------------------------- | --------------------------- | --------------------------------- |
| **Manual Quality Checks** | Inconsistent, forgotten     | Automated git hooks               |
| **No Prioritization**     | Equal treatment of issues   | Criticality levels                |
| **Blind Fixes**           | Incorrect automated changes | Confidence assessment             |
| **Deleting Content**      | Knowledge loss              | Content preservation              |
| **Running All Tests**     | Slow pre-push               | Affected tests only               |
| **Ad-Hoc Validation**     | Inconsistent patterns       | Standardized methodology          |
| **Ignoring Criticality**  | Random fix order            | Priority-based execution          |
| **No CI Quality Gates**   | Bad code merges             | Fail build on violations          |
| **Undocumented Rules**    | Unclear purpose             | Document rules and rationale      |
| **Format All Files**      | Slow, unintended changes    | Lint-staged for staged files only |

## Related Documentation

- [Code Quality Convention](./code.md) - Automated quality tools and git hooks
- [Criticality Levels Convention](./criticality-levels.md) - Issue categorization
- [Fixer Confidence Levels Convention](./fixer-confidence-levels.md) - Confidence assessment
- [Repository Validation Methodology](./repository-validation.md) - Validation patterns
- [Best Practices](./best-practices.md) - Recommended patterns

## Conclusion

Avoiding these anti-patterns ensures:

- Automated quality enforcement
- Clear issue prioritization
- Safe automated remediation
- Preserved documentation value
- Fast feedback loops
- Consistent validation patterns
- Priority-based fix execution
- Strong CI quality gates
- Well-documented rules
- Efficient incremental quality

When implementing quality processes, ask: **Am I adding automation or friction?** If friction, refactor to follow quality development best practices.

## Principles Applied

- **Automation Over Manual**: Git hooks, CI gates, automated validation
- **Documentation First**: Preserve content, document validation rules
- **Explicit Over Implicit**: Clear criticality, documented rationale
- **Simplicity Over Complexity**: Incremental quality, affected tests only
