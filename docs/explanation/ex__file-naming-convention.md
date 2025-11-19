---
title: "Documentation File Naming Convention"
description: Systematic file naming for open-sharia-fintech documentation
category: explanation
tags:
  - naming-convention
  - file-structure
  - organization
  - documentation
created: 2025-11-19
updated: 2025-11-19
---

# Documentation File Naming Convention

A systematic approach to naming files in the open-sharia-fintech documentation that ensures clarity, organization, and discoverability while maintaining a logical hierarchy.

## Overview

The naming convention serves three critical purposes:

1. **Hierarchical Organization** - File names encode the folder path, making it possible to identify a file's location just by reading the filename
2. **Discoverability** - Consistent prefixes make it easy to find related files across the documentation
3. **Obsidian Compatibility** - Since Obsidian resolves wiki links by filename (not path), this system creates unambiguous identifiers

## Scope

This naming convention applies to **all files in the `docs/` directory** (the Obsidian vault):

- `docs/tutorials/` - Learning-oriented guides
- `docs/how-to/` - Problem-solving guides
- `docs/reference/` - Technical reference documentation
- `docs/explanation/` - Conceptual documentation
- `docs/journals/` - Daily notes (uses different pattern: `YYYY-MM/YYYY-MM-DD.md`)

**File types covered:**

- Markdown files (`.md`)
- Images (`.png`, `.jpg`, `.svg`, `.gif`, etc.)
- Diagrams (`.excalidraw`, `.mmd`, `.drawio`)
- Any other documentation assets

## The Universal Pattern

All files follow this pattern:

```
[hierarchical-prefix]__[content-identifier].[extension]
```

**Example**: `tu-au-se__oauth2-flow.md`

Breaking this down:

- `tu-au-se` = hierarchical prefix (tutorials → authentication → security)
- `__` = double underscore separator
- `oauth2-flow` = content identifier (the actual name)
- `.md` = file extension

**Why this pattern?**

Obsidian resolves wiki links by filename only, not by folder path. Without encoded paths, two files with the same name in different folders would create ambiguity. By encoding the folder hierarchy in the filename, we make all files globally unique and self-documenting.

## How to Build a Prefix

The prefix encodes the folder path using abbreviations separated by single hyphens. Each level of nesting adds another abbreviation segment.

### Root Directory Prefixes

The four main Diátaxis categories have short prefixes:

- `tu` - Tutorials (`docs/tutorials/`)
- `ht` - How-To (`docs/how-to/`)
- `re` - Reference (`docs/reference/`)
- `ex` - Explanation (`docs/explanation/`)

### Abbreviation Strategy

Use a systematic 2-letter encoding rule for subdirectories:

1. **2 letters per word** (universal rule)
   - Take first 2 letters of each word
   - Examples: `authentication` → `au`, `api` → `ap`, `config` → `co`

2. **Multi-word directories: concatenate without internal hyphens**
   - `sharia-compliance` → `shco` (sh+co)
   - `api-endpoints` → `apen` (ap+en)
   - `database-schema` → `dasc` (da+sc)

3. **Single character words get underscore suffix**
   - `x` → `x_`, `v` → `v_`, `1` → `1_`
   - Example: `api-v2` → `apv2_` (ap+v+2\_)

### Examples

```
File at:     docs/tutorials/getting-started.md
Prefix:      tu
Pattern:     tu__getting-started.md

File at:     docs/tutorials/authentication/oauth2.md
Prefix:      tu-au
Pattern:     tu-au__oauth2.md

File at:     docs/how-to/deploy/kubernetes.md
Prefix:      ht-de
Pattern:     ht-de__kubernetes.md

File at:     docs/reference/api/endpoints/transactions.md
Prefix:      re-ap-en
Pattern:     re-ap-en__transactions.md

File at:     docs/explanation/sharia-compliance/murabaha.md
Prefix:      ex-shco
Pattern:     ex-shco__murabaha.md
```

## The `__` Separator

The double underscore creates a critical visual boundary:

- **Left side** = Where the file lives (organizational structure)
- **Right side** = What the file actually is (content identifier)

**Why double underscore?**

- Creates obvious visual clarity when scanning filenames
- Distinguishes from single hyphens used in content names
- Makes parsing trivial: split on `__` to separate structure from content

## General Naming Rules

### Kebab-Case Format

All filenames use lowercase with hyphens as separators (no spaces, mixed case, or underscores):

```markdown
✅ Good:

- tu\_\_getting-started-with-authentication.md
- ht-ap\_\_configure-rate-limiting.md
- re-ap-en\_\_transaction-endpoints.md
- ex-shco\_\_murabaha-contract-structure.png

❌ Bad:

- Getting_Started.md (mixed case, underscores)
- configure Rate Limiting.md (spaces, mixed case)
- TransactionEndpoints.md (camelCase)
```

### File Extensions

Keep the original file extension on all files:

- Markdown: `.md`
- Images: `.png`, `.jpg`, `.svg`, `.gif`
- Diagrams: `.excalidraw`, `.mmd`, `.drawio`
- PDFs: `.pdf`

### Sequential Numbering

For ordered content, use zero-padded numeric prefixes within the content identifier:

```
tu-qu__00-introduction.md
tu-qu__01-setup-environment.md
tu-qu__02-first-transaction.md
tu-qu__10-advanced-concepts.md
```

### Date-Based Files

Use ISO 8601 format (`YYYY-MM-DD`) for date components:

```
journals/2025-11/2025-11-19.md
ht-de__release-process-2025-11.md
```

## Wiki Link Syntax

In wiki links, **omit the `.md` extension**:

```markdown
✅ Correct:
[[tu__getting-started-with-authentication]]
[[ex-shco__murabaha-contract-structure|Murabaha Structure]]
[[re-ap-en__transaction-endpoints|Transaction API]]

❌ Incorrect:
[[getting-started.md]]
[[tu__getting-started-with-authentication.md]]
```

## Special Cases

### Index Files

Each category and subcategory should have an index file:

```
docs/tutorials/tu__tutorials.md
docs/tutorials/authentication/tu-au__authentication.md
docs/how-to/ht__how-to.md
docs/reference/re__reference.md
docs/explanation/ex__explanation.md
```

### Journals (Daily Notes)

The `journals/` directory uses a different pattern:

```
journals/YYYY-MM/YYYY-MM-DD.md
```

This is configured in `.obsidian/daily-notes.json` and doesn't use the prefix system.

### Images and Assets

Images follow the same prefix pattern as their related documentation:

```
docs/explanation/sharia-compliance/ex-shco__murabaha-flow.md
docs/explanation/sharia-compliance/ex-shco__murabaha-flow-diagram.png
```

## Maintenance and Scalability

### Adding New Directories

When creating a new subdirectory:

1. Apply the 2-letter rule to create the abbreviation
2. Add this abbreviation to the prefix of all files in that directory
3. Update any related index files

**Example**: Creating `docs/tutorials/testing/`:

- Directory: `docs/tutorials/testing/`
- Prefix: `tu-te__` (tu = tutorials, te = testing)
- Files: `tu-te__unit-testing.md`, `tu-te__integration-testing.md`

### Reorganizing Directories

When renaming or moving directories:

1. Rename all files in that directory to update their prefix
2. Update any wiki links that reference those files
3. Update related index files

### Scaling to Arbitrary Depth

This system scales to any nesting depth:

```
tu-au-oa-fl__authorization-code-flow.md
└─ tutorials → authentication → oauth → flows
   (tu+au+oa+fl)
```

## Quick Reference

| Category    | Prefix | Example                  |
| ----------- | ------ | ------------------------ |
| Tutorials   | `tu__` | `tu__getting-started.md` |
| How-To      | `ht__` | `ht__deploy-docker.md`   |
| Reference   | `re__` | `re__api-reference.md`   |
| Explanation | `ex__` | `ex__architecture.md`    |

## Related Documentation

- [[ex__diataxis-framework|Understanding the Diátaxis Framework]]
- [[ht__create-documentation|How to Create Documentation]]
- [[re__vault-structure|Vault Structure Reference]]

---

**Last Updated**: November 19, 2025
