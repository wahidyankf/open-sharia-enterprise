# Best Practices for Infrastructure Development

> **Companion Document**: For common mistakes to avoid, see [Anti-Patterns](./anti-patterns.md)

## Overview

This document outlines best practices for managing development infrastructure, including temporary files, build artifacts, and acceptance criteria. Following these practices ensures organized, traceable, and testable development processes.

## Purpose

Provide actionable guidance for:

- Temporary file organization
- Report generation and tracking
- Acceptance criteria authoring
- Build artifact management
- Development infrastructure patterns

## Best Practices

### Practice 1: Use Designated Temporary Directories

**Principle**: All temporary files go in `generated-reports/` or `local-temp/`, never repository root.

**Good Example:**

```bash
# Validation report
generated-reports/docs__a1b2c3__2025-12-14--20-45__audit.md

# Scratch work
local-temp/draft-analysis.txt
```

**Bad Example:**

```bash
# Scattered temporary files (DO NOT DO THIS)
temp-report.md
validation-output.txt
analysis-2025-12-14.json
```

**Rationale:**

- Clear organization prevents clutter
- Easy cleanup (both gitignored)
- Predictable file locations
- Separates temporary from permanent content

### Practice 2: Follow Standardized Report Naming

**Principle**: Use 4-part pattern: `{agent-family}__{uuid-chain}__{timestamp}__{type}.md`

**Good Example:**

```bash
# Generate UUID and timestamp
UUID=$(uuidgen | tr '[:upper:]' '[:lower:]' | head -c 6)
TIMESTAMP=$(TZ='Asia/Jakarta' date +"%Y-%m-%d--%H-%M")

# Create report
REPORT="generated-reports/docs__${UUID}__${TIMESTAMP}__audit.md"
```

**Bad Example:**

```bash
# Placeholder values (DO NOT DO THIS)
REPORT="generated-reports/docs__abc123__2025-12-14--00-00__audit.md"
```

**Rationale:**

- Unique UUIDs prevent file collisions
- Accurate timestamps enable chronological sorting
- Standardized pattern aids automation
- Audit trail for all validation runs

### Practice 3: Write Reports Progressively

**Principle**: Update report files continuously during execution, not at the end.

**Good Example:**

```bash
# Initialize report immediately
echo "# Audit Report" > "$REPORT"
echo "**Status**: In Progress" >> "$REPORT"

# Write findings as discovered
for file in $FILES; do
  result=$(validate "$file")
  echo "## $file" >> "$REPORT"
  echo "Result: $result" >> "$REPORT"
done

# Update final status
sed -i 's/In Progress/Complete/' "$REPORT"
```

**Bad Example:**

```bash
# Buffer in memory (DO NOT DO THIS)
findings=""
for file in $FILES; do
  findings+="$(validate "$file")\n"
done

# Write at end (lost if context compacted!)
echo "$findings" > "$REPORT"
```

**Rationale:**

- Survives context compaction during long audits
- Provides real-time progress visibility
- Enables debugging of incomplete runs
- Critical for AI agent reliability

### Practice 4: Generate Real UUIDs and Timestamps

**Principle**: Execute bash commands for actual values, never use placeholders.

**Good Example:**

```bash
# Generate real UUID
UUID=$(uuidgen | tr '[:upper:]' '[:lower:]' | head -c 6)
# Example output: a1b2c3

# Generate current timestamp
TIMESTAMP=$(TZ='Asia/Jakarta' date +"%Y-%m-%d--%H-%M")
# Example output: 2025-12-14--16-43
```

**Bad Example:**

```bash
# Placeholder values (DO NOT DO THIS)
UUID="abc123"
TIMESTAMP="2025-12-14--00-00"
```

**Rationale:**

- Unique UUIDs prevent parallel execution collisions
- Accurate timestamps enable audit trails
- Debugging requires real creation times
- Placeholders defeat the purpose of tracking

### Practice 5: Use Scope-Based Execution Tracking

**Principle**: Track execution chains within scopes to handle concurrent workflows.

**Good Example:**

```bash
# Determine scope
SCOPE="${EXECUTION_SCOPE:-docs}"

# Read parent chain from scope-specific file
CHAIN_FILE="generated-reports/.execution-chain-${SCOPE}"
if [ -f "$CHAIN_FILE" ]; then
  read PARENT_TIME PARENT_CHAIN < "$CHAIN_FILE"
  TIME_DIFF=$(($(date +%s) - PARENT_TIME))

  if [ $TIME_DIFF -lt 30 ]; then
    UUID_CHAIN="${PARENT_CHAIN}_${MY_UUID}"
  else
    UUID_CHAIN="${MY_UUID}"
  fi
else
  UUID_CHAIN="${MY_UUID}"
fi
```

**Bad Example:**

```bash
# Global tracking file (causes race conditions)
CHAIN_FILE="generated-reports/.execution-chain"
# All workflows share same file - parent tracking breaks!
```

**Rationale:**

- Isolates concurrent workflow executions
- Prevents cross-contamination between scopes
- Enables accurate parent-child hierarchy
- Handles parallel execution correctly

### Practice 6: Write Gherkin Acceptance Criteria

**Principle**: Use Given-When-Then format for testable requirements.

**Good Example:**

```gherkin
Scenario: User logs in with valid credentials
  Given a registered user with email "user@example.com"
  When the user submits login form with correct password
  Then the user is redirected to dashboard
  And a session token is created
```

**Bad Example:**

```markdown
The system should allow users to log in.
```

**Rationale:**

- Testable and executable specifications
- Clear setup, action, and expected outcome
- Enables automated testing
- Reduces ambiguity in requirements

### Practice 7: Require Write and Bash Tools for Report Generators

**Principle**: Checker agents MUST have both Write and Bash tools in frontmatter.

**Good Example:**

```yaml
---
name: docs-checker
description: Validates documentation quality
tools: [Read, Glob, Grep, Write, Bash]
model: sonnet
---
```

**Bad Example:**

```yaml
---
name: docs-checker
description: Validates documentation quality
tools: [Read, Glob, Grep] # MISSING Write and Bash!
---
```

**Rationale:**

- Write tool creates report files
- Bash tool generates UTC+7 timestamps
- Mandatory for audit report generation
- Consistency across all checker agents

### Practice 8: Pair Audit and Fix Reports with Same UUID and Timestamp

**Principle**: Fixer reports use same UUID-chain and timestamp as source audit.

**Good Example:**

```bash
# Audit report
AUDIT="generated-reports/docs__a1b2c3__2025-12-14--20-45__audit.md"

# Fix report (same UUID and timestamp)
FIX="generated-reports/docs__a1b2c3__2025-12-14--20-45__fix.md"
```

**Bad Example:**

```bash
# Fix report with new timestamp (DO NOT DO THIS)
FIX="generated-reports/docs__d4e5f6__2025-12-14--21-00__fix.md"
# Can't match to source audit!
```

**Rationale:**

- Clear audit-fix traceability
- Enables exact report matching
- Supports debugging and review
- Maintains complete audit trail

### Practice 9: Clean Up Temporary Files Periodically

**Principle**: Remove old temporary files to prevent accumulation.

**Good Example:**

```bash
# Archive old reports (>30 days)
find generated-reports/ -name "*.md" -mtime +30 -exec mv {} archive/ \;

# Clean scratch files (>7 days)
find local-temp/ -mtime +7 -delete
```

**Bad Example:**

```bash
# Never clean up (thousands of old files accumulate)
```

**Rationale:**

- Prevents directory bloat
- Faster file system operations
- Easier to find recent reports
- Maintains workspace hygiene

### Practice 10: Document Temporary File Purposes

**Principle**: Add README or comments explaining long-lived temporary files.

**Good Example:**

```bash
# local-temp/cache/README.md
# Performance Cache
#
# This directory contains cached API responses for development.
# Files are regenerated automatically if older than 1 hour.
# Safe to delete - will be recreated as needed.
```

**Bad Example:**

```bash
# Mysterious temporary files with no explanation
local-temp/data-2025.json
local-temp/cache-v3.bin
local-temp/temp-final-v2.txt
```

**Rationale:**

- Clear purpose reduces confusion
- Easier onboarding for new team members
- Prevents accidental deletion of important data
- Documents retention policies

## Related Documentation

- [Temporary Files Convention](./temporary-files.md) - Complete temporary file standards
- [Acceptance Criteria Convention](./acceptance-criteria.md) - Gherkin acceptance criteria guide
- [Anti-Patterns](./anti-patterns.md) - Common mistakes to avoid
- [Explicit Over Implicit Principle](../../principles/software-engineering/explicit-over-implicit.md) - Why clear organization matters

## Summary

Following these best practices ensures:

1. Use designated temporary directories
2. Follow standardized report naming
3. Write reports progressively
4. Generate real UUIDs and timestamps
5. Use scope-based execution tracking
6. Write Gherkin acceptance criteria
7. Require Write and Bash tools for reports
8. Pair audit and fix reports correctly
9. Clean up temporary files periodically
10. Document temporary file purposes

Infrastructure built following these practices is organized, traceable, testable, and maintainable.

## Principles Applied

- **Explicit Over Implicit**: Clear file organization, standardized naming
- **Automation Over Manual**: Progressive report writing, automated tracking
- **Simplicity Over Complexity**: Two directories for all temporary files
