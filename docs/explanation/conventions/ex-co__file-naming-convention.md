---
title: "Documentation File Naming Convention"
description: Systematic file naming for open-sharia-enterprise documentation
category: explanation
tags:
  - naming-convention
  - file-structure
  - organization
  - documentation
created: 2025-11-19
updated: 2025-11-22
---

# Documentation File Naming Convention

A systematic approach to naming files in the open-sharia-enterprise documentation that ensures clarity, organization, and discoverability while maintaining a logical hierarchy.

## Overview

The naming convention serves three critical purposes:

1. **Hierarchical Organization** - File names encode the folder path, making it possible to identify a file's location just by reading the filename
2. **Discoverability** - Consistent prefixes make it easy to find related files across the documentation
3. **Global Uniqueness** - Hierarchical prefixes ensure no two files have the same name, preventing ambiguity across the entire documentation vault

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

All documentation files (except index files) follow this pattern:

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

By encoding the folder hierarchy in the filename, we make all files globally unique and self-documenting. This prevents naming conflicts and makes it immediately clear where a file belongs in the documentation structure.

**Exception**: Index files use `README.md` for GitHub compatibility (see Special Cases below).

## How to Build a Prefix

The prefix encodes the folder path using abbreviations separated by single hyphens. Each level of nesting adds another abbreviation segment.

### Root Directory Prefixes

The four main Diátaxis categories have short prefixes:

- `tu` - Tutorials (`docs/tutorials/`)
- `ht` - How-To (`docs/how-to/`)
- `re` - Reference (`docs/reference/`)
- `ex` - Explanation (`docs/explanation/`)

**Note on Directory Naming:**

The directory names follow semantic conventions from the Diátaxis framework:

- `tutorials/` - **Plural** (collection of discrete tutorial documents)
- `how-to/` - **Singular category name** (the folder contains "how-to guides", matching the category name)
- `reference/` - **Singular/mass noun** (reference material as a collective whole, like "reference library")
- `explanation/` - **Singular/mass noun** (explanatory material as a collective whole)

This apparent inconsistency is intentional and follows standard documentation conventions. Only `tutorials/` is plural because tutorials are naturally countable discrete units, while the other categories represent types of content that are better expressed as mass nouns.

### Abbreviation Strategy

Use a systematic 2-letter encoding rule:

1. **2 letters per word** - Take first 2 letters (`authentication` → `au`, `api` → `ap`)
2. **Multi-word directories** - Concatenate without hyphens (`sharia-compliance` → `shco`)
3. **Single character words** - Add underscore suffix (`v` → `v_`)

Examples: `tu__getting-started.md` (tutorials), `tu-au__oauth2.md` (tutorials/authentication), `re-ap-en__transactions.md` (reference/api/endpoints)

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

**Required Format:** All dates must use **ISO 8601 format** (`YYYY-MM-DD`):

- `YYYY` = 4-digit year
- `MM` = 2-digit month (01-12)
- `DD` = 2-digit day (01-31)

**Examples:**

```
journals/2025-11/2025-11-19.md
ht-de__release-process-2025-11.md
```

**Frontmatter dates:** Frontmatter fields (`created`, `updated`) also use ISO 8601 format:

```yaml
---
created: 2025-11-19
updated: 2025-11-22
---
```

## Special Cases

### Index Files (README.md)

**GitHub Compatibility Exception:**

Each category and subcategory should have an index file named `README.md`. This is a special exception to the prefix naming convention to ensure GitHub automatically displays the index when browsing directories on the web.

```
docs/tutorials/README.md                          # Main category index
docs/tutorials/authentication/README.md          # Subcategory index (also uses README.md)
docs/how-to/README.md                            # Main category index
docs/reference/README.md                         # Main category index
docs/explanation/README.md                       # Main category index
docs/explanation/conventions/README.md           # Subcategory index
```

**Key Points:**

- Main category indices (`tutorials/`, `how-to/`, `reference/`, `explanation/`) use `README.md`
- Subcategory indices also use `README.md` for consistency
- `README.md` files are **exempt from the prefix requirement**
- This ensures GitHub web interface displays indices automatically
- Works seamlessly with Obsidian and other markdown viewers

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

- [Linking Convention](./ex-co__linking-convention.md) - How to link between documentation files
- [Diátaxis Framework](./ex-co__diataxis-framework.md) - Understanding the documentation organization framework
- [Conventions Index](./README.md) - Index of all documentation conventions

---

**Last Updated**: 2025-11-22
