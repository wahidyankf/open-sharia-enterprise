---
title: "Timestamp Format Convention"
description: Standard timestamp format using UTC+7 (Indonesian WIB Time)
category: explanation
subcategory: conventions
tags:
  - conventions
  - timestamps
  - timezone
  - formatting
created: 2025-11-30
updated: 2025-11-30
---

# ⏰ Timestamp Format Convention

## 📋 Overview

All timestamps in this repository use **UTC+7 (WIB - Western Indonesian Time)** by default with ISO 8601 format.

## Standard Format

**Format:** `YYYY-MM-DDTHH:MM:SS+07:00`

**Examples:**

- `2025-11-30T22:45:00+07:00` (10:45 PM on November 30, 2025)
- `2025-01-15T09:30:00+07:00` (9:30 AM on January 15, 2025)

## 🎯 Why UTC+7?

**Reasons for standardizing on Indonesian time:**

1. **Team location** - Development team operates in Indonesian timezone
2. **Business context** - Fintech application serves Indonesian market
3. **Clarity** - Eliminates timezone confusion in logs and cache files
4. **Consistency** - Single timezone across all project artifacts

## Where This Applies

**Use UTC+7 timestamps in:**

- Cache files (e.g., `docs/metadata/external-links-status.yaml`)
- Metadata files (any operational data files)
- Log files (application logs, build logs)
- Documentation timestamps (frontmatter `created`/`updated` fields)
- Manual timestamps in code comments (when needed)
- Configuration files requiring timestamps

## Exceptions

**Do NOT use UTC+7 in:**

- **Git commits** - Git uses its own timestamp format (UTC with author timezone), leave as-is
- **ISO 8601 UTC** - When integrating with external APIs that require UTC (use `Z` suffix)
- **User-facing timestamps** - Use user's local timezone in UI/UX
- **Database timestamps** - Follow database conventions (usually UTC for storage, convert on display)

## Implementation Examples

### Cache Files

```yaml
version: 1.0.0
lastFullScan: 2025-11-30T22:45:00+07:00
lastChecked: 2025-11-30T07:00:00+07:00
```

### Documentation Frontmatter

```yaml
---
created: 2025-11-30
updated: 2025-11-30
---
```

**Note**: Documentation frontmatter uses date-only format (YYYY-MM-DD) for simplicity. Full timestamps with time component should use the standard format above.

### Code Comments (when needed)

```typescript
// Last verified: 2025-11-30T22:45:00+07:00
const API_ENDPOINT = "https://example.com";
```

### Log Files

```
[2025-11-30T22:45:00+07:00] INFO: Application started
[2025-11-30T22:45:15+07:00] DEBUG: Database connection established
```

## Format Specification

**Components:**

- `YYYY` - 4-digit year
- `MM` - 2-digit month (01-12)
- `DD` - 2-digit day (01-31)
- `T` - Separator between date and time
- `HH` - 2-digit hour (00-23) in 24-hour format
- `MM` - 2-digit minute (00-59)
- `SS` - 2-digit second (00-59)
- `+07:00` - Timezone offset from UTC (UTC+7 for WIB)

**Standards compliance:**

- ISO 8601 compliant
- RFC 3339 compliant
- Parseable by standard libraries (JavaScript Date, Python datetime, etc.)

## Converting from UTC

If you have a UTC timestamp and need to convert to WIB:

**UTC to WIB:** Add 7 hours

**Examples:**

- `2025-11-30T15:45:00Z` (UTC) → `2025-11-30T22:45:00+07:00` (WIB)
- `2025-11-30T00:00:00Z` (UTC) → `2025-11-30T07:00:00+07:00` (WIB)

## Validation

**Valid timestamps:**

- `2025-11-30T22:45:00+07:00`
- `2025-01-01T00:00:00+07:00`
- `2025-12-31T23:59:59+07:00`

**Invalid timestamps:**

- `2025-11-30T22:45:00Z` (wrong timezone - this is UTC)
- `2025-11-30T22:45:00` (missing timezone offset)
- `2025-11-30 22:45:00+07:00` (space instead of T separator)
- `30-11-2025T22:45:00+07:00` (wrong date format)

## 📚 Related Conventions

- [File Naming Convention](./ex-co__file-naming-convention.md) - Date format in filenames
- [Journals Format Convention](./ex-co__journals-format.md) - Daily journal timestamp format

## See Also

- **ISO 8601**: International standard for date and time representation
- **RFC 3339**: Internet timestamp format specification
- **WIB**: Western Indonesian Time (Waktu Indonesia Barat)
