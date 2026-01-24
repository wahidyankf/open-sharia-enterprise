# Anti-Patterns in Infrastructure Development

> **Companion Document**: For positive guidance on what to do, see [Best Practices](./best-practices.md)

## Overview

Understanding common mistakes in development infrastructure management helps teams build more organized, traceable, and maintainable systems. These anti-patterns cause clutter, traceability issues, and operational problems.

## Purpose

This document provides:

- Common anti-patterns in infrastructure development
- Examples of problematic implementations
- Solutions and corrections for each anti-pattern
- Organizational and operational considerations

## Common Anti-Patterns

### Anti-Pattern 1: Scattered Temporary Files

**Problem**: Creating temporary files in repository root or random locations.

**Bad Example:**

```bash
# Temporary files scattered everywhere
temp-report.md
validation-output.txt
/docs/temp-analysis.json
/apps/scratch-notes.txt
```

**Solution:**

```bash
# Organized in designated directories
generated-reports/docs__a1b2c3__2025-12-14--20-45__audit.md
local-temp/scratch-notes.txt
local-temp/analysis.json
```

**Rationale:**

- Repository clutter makes navigation hard
- Can't easily find or clean temporary files
- Risk of accidentally committing temporary data
- Designated directories are gitignored

### Anti-Pattern 2: Using Placeholder UUID and Timestamps

**Problem**: Using hardcoded placeholder values instead of generating real UUIDs.

**Bad Example:**

```bash
# Placeholder values (DO NOT DO THIS)
UUID="abc123"
TIMESTAMP="2025-12-14--00-00"
REPORT="generated-reports/docs__${UUID}__${TIMESTAMP}__audit.md"
```

**Solution:**

```bash
# Generate real values
UUID=$(uuidgen | tr '[:upper:]' '[:lower:]' | head -c 6)
TIMESTAMP=$(TZ='Asia/Jakarta' date +"%Y-%m-%d--%H-%M")
REPORT="generated-reports/docs__${UUID}__${TIMESTAMP}__audit.md"
```

**Rationale:**

- Placeholder timestamps defeat audit trail purpose
- Same UUID causes file collisions in parallel execution
- Can't sort chronologically with fake timestamps
- Debugging requires real creation times

### Anti-Pattern 3: Buffering Reports in Memory

**Problem**: Collecting findings in memory and writing report only at the end.

**Bad Example:**

```bash
# Buffer findings (DO NOT DO THIS)
findings=""
for file in $FILES; do
  result=$(validate "$file")
  findings+="$result\n"
done

# Write once at end (lost if context compacted!)
echo "$findings" > "$REPORT"
```

**Solution:**

```bash
# Write progressively
echo "# Audit Report" > "$REPORT"
for file in $FILES; do
  result=$(validate "$file")
  echo "## $file" >> "$REPORT"
  echo "Result: $result" >> "$REPORT"
done
```

**Rationale:**

- Findings lost during context compaction
- No progress visibility during long audits
- Can't debug incomplete runs
- Progressive writing ensures persistence

### Anti-Pattern 4: Missing Write or Bash Tools

**Problem**: Checker agent lacks required tools for report generation.

**Bad Example:**

```yaml
---
name: docs-checker
description: Validates documentation
tools: [Read, Glob, Grep] # MISSING Write and Bash!
---
```

**Solution:**

```yaml
---
name: docs-checker
description: Validates documentation
tools: [Read, Glob, Grep, Write, Bash]
---
```

**Rationale:**

- Write tool creates report files
- Bash tool generates UTC+7 timestamps
- Mandatory for all checker agents
- Consistent tool permissions across agents

### Anti-Pattern 5: Global Execution Tracking

**Problem**: Using single global tracking file for all workflows.

**Bad Example:**

```bash
# Global tracking file (causes race conditions)
CHAIN_FILE="generated-reports/.execution-chain"
# All workflows share same file!
```

**Solution:**

```bash
# Scope-based tracking files
SCOPE="${EXECUTION_SCOPE:-docs}"
CHAIN_FILE="generated-reports/.execution-chain-${SCOPE}"
```

**Rationale:**

- Concurrent workflows overwrite each other's data
- Parent tracking breaks across scopes
- Race conditions in parallel execution
- Scope isolation prevents contamination

### Anti-Pattern 6: Mismatched Audit and Fix Reports

**Problem**: Fixer uses different UUID or timestamp than source audit.

**Bad Example:**

```bash
# Audit report
AUDIT="generated-reports/docs__a1b2c3__2025-12-14--20-45__audit.md"

# Fix report with NEW UUID and timestamp (DO NOT DO THIS)
FIX="generated-reports/docs__d4e5f6__2025-12-14--21-00__fix.md"
```

**Solution:**

```bash
# Extract UUID and timestamp from audit filename
BASENAME=$(basename "$AUDIT" .md)
UUID=$(echo "$BASENAME" | awk -F'__' '{print $2}')
TIMESTAMP=$(echo "$BASENAME" | awk -F'__' '{print $3}')

# Fix report uses SAME UUID and timestamp
FIX="generated-reports/docs__${UUID}__${TIMESTAMP}__fix.md"
```

**Rationale:**

- Can't match fix report to source audit
- Breaks audit trail
- Complicates debugging
- Same UUID+timestamp enables exact pairing

### Anti-Pattern 7: Vague Acceptance Criteria

**Problem**: Writing ambiguous, non-testable acceptance criteria.

**Bad Example:**

```markdown
The system should work well and be fast.
Users should have a good experience.
```

**Solution:**

```gherkin
Scenario: User views dashboard
  Given a logged-in user
  When the user navigates to dashboard
  Then the page loads in under 2 seconds
  And all widgets display current data
```

**Rationale:**

- Vague criteria can't be tested
- No clear definition of "done"
- Can't automate validation
- Gherkin provides executable specifications

### Anti-Pattern 8: Never Cleaning Temporary Files

**Problem**: Accumulating thousands of old temporary files.

**Bad Example:**

```bash
# Never clean up
ls generated-reports/ | wc -l
# Output: 5,847 files (most months old!)
```

**Solution:**

```bash
# Periodic cleanup
find generated-reports/ -name "*.md" -mtime +30 -exec mv {} archive/ \;
find local-temp/ -mtime +7 -delete
```

**Rationale:**

- Directory bloat slows file system
- Hard to find recent reports
- Wastes disk space
- Regular cleanup maintains hygiene

### Anti-Pattern 9: Conversation-Only Output

**Problem**: Checker outputs findings in conversation without writing report file.

**Bad Example:**

```markdown
## Agent: docs-checker

Validation complete! Found 15 issues:

1. Missing alt text in image
2. Broken link to /docs/guide
   ...
   (No file written - findings lost during context compaction!)
```

**Solution:**

```bash
# Write findings to report file
REPORT="generated-reports/docs__${UUID}__${TIMESTAMP}__audit.md"
echo "# Validation Report" > "$REPORT"
echo "## Issues Found" >> "$REPORT"
echo "1. Missing alt text in image" >> "$REPORT"
# ... continue writing to file
```

**Rationale:**

- Conversation findings lost during compaction
- No audit trail
- Can't pass to fixer agents
- Report files persist regardless of context

### Anti-Pattern 10: Undocumented Long-Lived Temporary Files

**Problem**: Mysterious temporary files with no explanation.

**Bad Example:**

```bash
# What are these files?
local-temp/cache-v3.bin
local-temp/data-final-2025.json
local-temp/temp-backup-v2.tar.gz
```

**Solution:**

```bash
# local-temp/cache/README.md
# API Response Cache
#
# Contains cached API responses for development.
# Regenerated if older than 1 hour.
# Safe to delete - recreated as needed.
```

**Rationale:**

- Purpose unclear without documentation
- New team members don't know if safe to delete
- Retention policies unknown
- Documentation prevents confusion

## Summary of Anti-Patterns

| Anti-Pattern                  | Problem                | Solution                           |
| ----------------------------- | ---------------------- | ---------------------------------- |
| **Scattered Temporary Files** | Repository clutter     | Use designated directories         |
| **Placeholder UUIDs**         | Defeats audit trail    | Generate real UUIDs and timestamps |
| **Buffering Reports**         | Lost during compaction | Write progressively                |
| **Missing Tools**             | Can't generate reports | Add Write and Bash tools           |
| **Global Tracking**           | Race conditions        | Scope-based tracking               |
| **Mismatched Reports**        | Breaks audit trail     | Use same UUID and timestamp        |
| **Vague Criteria**            | Not testable           | Use Gherkin format                 |
| **Never Cleaning Up**         | Directory bloat        | Periodic cleanup                   |
| **Conversation-Only Output**  | Lost during compaction | Write report files                 |
| **Undocumented Temp Files**   | Purpose unclear        | Add README documentation           |

## Related Documentation

- [Temporary Files Convention](./temporary-files.md) - Complete temporary file standards
- [Acceptance Criteria Convention](./acceptance-criteria.md) - Gherkin acceptance criteria guide
- [Best Practices](./best-practices.md) - Recommended patterns
- [Explicit Over Implicit Principle](../../principles/software-engineering/explicit-over-implicit.md) - Why clear organization matters

## Conclusion

Avoiding these anti-patterns ensures:

- Organized temporary file structure
- Traceable audit trails
- Persistent report generation
- Testable acceptance criteria
- Concurrent execution support
- Clean workspace hygiene
- Clear documentation
- Reliable infrastructure

When managing infrastructure, ask: **Am I adding clarity or clutter?** If clutter, refactor to follow infrastructure development best practices.

## Principles Applied

- **Explicit Over Implicit**: Clear file locations, documented purposes
- **Automation Over Manual**: Progressive writing, automated tracking
- **Simplicity Over Complexity**: Designated directories, simple naming
