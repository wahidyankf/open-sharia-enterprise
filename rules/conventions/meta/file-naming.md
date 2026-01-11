---
title: "Documentation File Naming Convention"
description: Systematic file naming for open-sharia-enterprise documentation
category: explanation
subcategory: conventions
tags:
  - naming-convention
  - file-structure
  - organization
  - documentation
created: 2025-11-19
updated: 2026-01-01
---

# Documentation File Naming Convention

A systematic approach to naming files in the open-sharia-enterprise documentation that ensures clarity, organization, and discoverability while maintaining a logical hierarchy.

## 📋 Overview

The naming convention serves three critical purposes:

1. **Hierarchical Organization** - File names encode the folder path, making it possible to identify a file's location just by reading the filename
2. **Discoverability** - Consistent prefixes make it easy to find related files across the documentation
3. **Global Uniqueness** - Hierarchical prefixes ensure no two files have the same name, preventing ambiguity across the entire documentation vault

## Principles Implemented/Respected

This convention implements the following core principles:

- **[Explicit Over Implicit](../../principles/software-engineering/ex-ru-pr-se__explicit-over-implicit.md)**: File names explicitly encode their location in the directory structure (`[prefix]__[name]`), making the organizational hierarchy visible without requiring users to navigate folders. No hidden conventions or magic - the full path is transparent in the filename itself.

- **[Simplicity Over Complexity](../../principles/general/ex-ru-pr-ge__simplicity-over-complexity.md)**: Uses straightforward abbreviation rules (2 or 4 characters) and a single separator pattern (`__`). Avoids complex encoding schemes or multiple separator types. The pattern is easy to learn, remember, and apply consistently.

## Purpose

This convention establishes a systematic file naming pattern that encodes directory hierarchy in filenames, ensuring global uniqueness and discoverability across the documentation. It prevents naming conflicts, makes file locations transparent, and enables efficient file management without relying on directory structure alone.

## 🎯 Scope

This naming convention applies to **all files in the `docs/` directory** (the Obsidian vault):

- `docs/tutorials/` - Learning-oriented guides
- `docs/how-to/` - Problem-solving guides
- `docs/reference/` - Technical reference documentation
- `docs/explanation/` - Conceptual documentation

**File types covered:**

- Markdown files (`.md`)
- Images (`.png`, `.jpg`, `.svg`, `.gif`, etc.)
- Diagrams (`.excalidraw`, `.mmd`, `.drawio`)
- Any other documentation assets

## 📝 The Universal Pattern

All documentation files (except index files) follow this pattern:

```
[hierarchical-prefix]__[content-identifier].[extension]
```

**Example**: `ex-ru-co-me__file-naming.md`

Breaking this down:

- `ex-ru-co-me` = hierarchical prefix (explanation → conventions → meta)
- `__` = double underscore separator
- `file-naming` = content identifier (the actual name)
- `.md` = file extension

**Why this pattern?**

By encoding the folder hierarchy in the filename, we make all files globally unique and self-documenting. This prevents naming conflicts and makes it immediately clear where a file belongs in the documentation structure.

**Exception**: Index files use `README.md` for GitHub compatibility (see Special Cases below).

## 🔧 How to Build a Prefix

The prefix encodes the folder path using abbreviations separated by single hyphens. Each level of nesting adds another abbreviation segment.

### Root Directory Prefixes

The four main Diátaxis categories use consistent 4-character prefixes:

- `tu` - Tutorials (`docs/tutorials/`) - Common/short root: 2 chars
- `hoto` - How-To (`docs/how-to/`) - Compound root: 4 chars (2 from each word)
- `re` - Reference (`docs/reference/`) - Common/short root: 2 chars
- `ex` - Explanation (`docs/explanation/`) - Common/short root: 2 chars

**Note on Directory Naming:**

The directory names follow semantic conventions from the Diátaxis framework:

- `tutorials/` - **Plural** (collection of discrete tutorial documents)
- `how-to/` - **Singular category name** (the folder contains "how-to guides", matching the category name)
- `reference/` - **Singular/mass noun** (reference material as a collective whole, like "reference library")
- `explanation/` - **Singular/mass noun** (explanatory material as a collective whole)

This apparent inconsistency is intentional and follows standard documentation conventions. Only `tutorials/` is plural because tutorials are naturally countable discrete units, while the other categories represent types of content that are better expressed as mass nouns.

### Subdirectory Encoding

**CRITICAL**: The 3-phase reorganization (completed 2026-01-01) introduced **subdirectory-based prefixes** for better organization. Files now encode their subdirectory in the prefix.

**Pattern**: `[root]-[subdirectory]__[content-name].md`

**Subdirectory codes** use 2-4 letter abbreviations based on directory characteristics:

**For Subdirectories:**

1. **Hyphenated compounds** - Concatenate first 2 letters of each word WITHOUT dash (`ayokoding-web` → `aywe`, `software-engineering` → `se`)
2. **Single words** - First 2 characters (`formatting` → `fo`, `content` → `co`, `meta` → `me`)

**Current Subdirectory Structure:**

### Conventions Subdirectories (`rules/conventions/`)

| Subdirectory  | Code | Prefix          | Example File                         |
| ------------- | ---- | --------------- | ------------------------------------ |
| `formatting/` | `fo` | `ex-ru-co-fo__` | `ex-ru-co-fo__indentation.md`        |
| `content/`    | `co` | `ex-ru-co-co__` | `ex-ru-co-co__quality.md`            |
| `meta/`       | `me` | `ex-ru-co-me__` | `ex-ru-co-me__file-naming.md`        |
| `tutorial/`   | `tu` | `ex-ru-co-tu__` | `ex-ru-co-tu__naming.md`             |
| `hugo/`       | `hu` | `ex-ru-co-hu__` | `ex-ru-co-hu__shared.md`             |
| `project/`    | `pr` | `ex-ru-co-pr__` | `ex-ru-co-pr__plans-organization.md` |

### Development Subdirectories (`rules/development/`)

| Subdirectory | Code | Prefix          | Example File                             |
| ------------ | ---- | --------------- | ---------------------------------------- |
| `workflow/`  | `wo` | `ex-ru-de-wo__` | `ex-ru-de-wo__commit-messages.md`        |
| `agents/`    | `ag` | `ex-ru-de-ag__` | `ex-ru-de-ag__ai-agents.md`              |
| `quality/`   | `qu` | `ex-ru-de-qu__` | `ex-ru-de-qu__code.md`                   |
| `pattern/`   | `pa` | `ex-ru-de-pa__` | `ex-ru-de-pa__functional-programming.md` |
| `hugo/`      | `hu` | `ex-ru-de-hu__` | `ex-ru-de-hu__development.md`            |
| `infra/`     | `in` | `ex-ru-de-in__` | `ex-ru-de-in__temporary-files.md`        |

### Workflows Subdirectories (`rules/workflows/`)

| Subdirectory     | Code   | Prefix            | Example File                             |
| ---------------- | ------ | ----------------- | ---------------------------------------- |
| `ayokoding-web/` | `aywe` | `ex-ru-wf-aywe__` | `ex-ru-wf-aywe__general-quality-gate.md` |
| `docs/`          | `do`   | `ex-ru-wf-do__`   | `ex-ru-wf-do__quality-gate.md`           |
| `meta/`          | `me`   | `ex-ru-wf-me__`   | `ex-ru-wf-me__workflow-pattern.md`       |
| `plan/`          | `pl`   | `ex-ru-wf-pl__`   | `ex-ru-wf-pl__execution.md`              |
| `wow/`           | `wo`   | `ex-ru-wf-wo__`   | `ex-ru-wf-wo__rules-quality-gate.md`     |

### Principles Subdirectories (`rules/principles/`)

| Subdirectory            | Code | Prefix          | Example File                                 |
| ----------------------- | ---- | --------------- | -------------------------------------------- |
| `general/`              | `ge` | `ex-ru-pr-ge__` | `ex-ru-pr-ge__simplicity-over-complexity.md` |
| `content/`              | `co` | `ex-ru-pr-co__` | `ex-ru-pr-co__accessibility-first.md`        |
| `software-engineering/` | `se` | `ex-ru-pr-se__` | `ex-ru-pr-se__explicit-over-implicit.md`     |

### Abbreviation Strategy

Use a systematic encoding rule based on directory characteristics:

**For Root Directories:**

1. **Common/short roots** - 2 characters (`tutorials` → `tu`, `explanation` → `ex`, `reference` → `re`)
2. **Compound roots** - 4 characters, 2 from each word (`how-to` → `hoto`)

**For Subdirectories:**

1. **Hyphenated compounds** - Concatenate first 2 letters of each word WITHOUT dash (`ayokoding-web` → `aywe`, `software-engineering` → `se`)
2. **Single words** - First 2 characters (`formatting` → `fo`, `content` → `co`, `meta` → `me`, `workflow` → `wo`, `agents` → `ag`, `quality` → `qu`, `pattern` → `pa`, `hugo` → `hu`, `infra` → `in`, `docs` → `do`, `plan` → `pl`, `wow` → `wo`, `general` → `ge`, `tutorial` → `tu`, `project` → `pr`)

**Key Rule**: For hyphenated directory names, take first 2 letters of EACH word and concatenate them WITHOUT inserting a dash. The dash in the directory name does NOT appear in the abbreviation.

**Real Examples:**

```
ex-ru-co-fo__indentation.md          (explanation/rules/conventions/formatting)
ex-ru-de-ag__ai-agents.md            (explanation/rules/development/agents)
ex-ru-wf-aywe__general-quality-gate.md  (explanation/rules/workflows/ayokoding-web)
ex-ru-pr-se__explicit-over-implicit.md  (explanation/rules/principles/software-engineering)
```

## The `__` Separator

The double underscore creates a critical visual boundary:

- **Left side** = Where the file lives (organizational structure)
- **Right side** = What the file actually is (content identifier)

**Why double underscore?**

- Creates obvious visual clarity when scanning filenames
- Distinguishes from single hyphens used in content names
- Makes parsing trivial: split on `__` to separate structure from content

## ✅ General Naming Rules

### Kebab-Case Format

All filenames use lowercase with hyphens as separators (no spaces, mixed case, or underscores):

```markdown
✅ Good:

- tu\_\_getting-started-with-authentication.md
- hoto\_\_configure-rate-limiting.md
- re\_\_transaction-endpoints.md
- ex-ru-co-me\_\_file-naming.md

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
hoto__release-process-2025-11.md
```

**Frontmatter dates:** Frontmatter fields (`created`, `updated`) also use ISO 8601 format:

```yaml
---
created: 2025-11-19
updated: 2026-01-01
---
```

## 🔍 Special Cases

### Index Files (README.md)

**GitHub Compatibility Exception:**

Each category and subcategory should have an index file named `README.md`. This is a special exception to the prefix naming convention to ensure GitHub automatically displays the index when browsing directories on the web.

```
docs/tutorials/README.md                          # Main category index
docs/how-to/README.md                            # Main category index
docs/reference/README.md                         # Main category index
docs/explanation/README.md                       # Main category index
rules/conventions/README.md           # Subcategory index (conventions)
rules/development/README.md           # Subcategory index (development)
rules/principles/README.md            # Subcategory index (principles)
rules/workflows/README.md             # Subcategory index (workflows)
```

**Key Points:**

- Main category indices (`tutorials/`, `how-to/`, `reference/`, `explanation/`) use `README.md`
- Subcategory indices also use `README.md` for consistency
- `README.md` files are **exempt from the prefix requirement**
- This ensures GitHub web interface displays indices automatically
- Works seamlessly with Obsidian and other markdown viewers

### Metadata Files (docs/metadata/)

The `docs/metadata/` directory stores operational metadata files about documentation (not documentation content itself):

**Location**: `docs/metadata/` at docs root

**Purpose**: Cache files, operational data, validation artifacts

**Naming**: No prefix (folder provides context)

**Examples**:

```
docs/metadata/external-links-status.yaml     # Link validation cache
docs/metadata/frontmatter-validation.json    # Frontmatter check results
docs/metadata/search-index.json              # Generated search index
```

**Rationale**: Similar to files in the `plans/` folder, the directory structure provides sufficient organizational context, making prefixes unnecessary and redundant.

**Key Points:**

- Metadata files are exempt from the prefix requirement
- These are operational files, not documentation content
- They are committed to git for sharing across the team
- They relate to the documentation but are not part of the Diátaxis framework

### Images and Assets

Images follow the same prefix pattern as their related documentation:

```
rules/conventions/formatting/ex-ru-co-fo__diagrams.md
rules/conventions/formatting/ex-ru-co-fo__diagrams-example.png
```

## 🔄 Maintenance and Scalability

### Adding New Directories

When creating a new subdirectory:

1. Apply the 2-letter rule to create the abbreviation
2. Add this abbreviation to the prefix of all files in that directory
3. Update any related index files

**Example**: Creating a hypothetical `rules/conventions/[new-category]/`:

- Directory pattern: `rules/conventions/[new-category]/`
- Subdirectory code: First 2 letters (e.g., `ne` for `new-category`)
- Prefix pattern: `ex-ru-co-ne__` (ex = explanation, co = conventions, ne = new-category)
- Files pattern: `ex-ru-co-ne__[content-name].md`

### Renaming Directories

When renaming a directory in `docs/`, you **must rename all files within that directory** to update their prefixes accordingly. This is because the file naming convention encodes the directory path in the filename prefix.

**Exception**: Files in `docs/metadata/` store operational files without prefixes and are exempt from this requirement.

**Process**:

1. Rename the directory
2. Rename all files in that directory to update their prefix
3. Update any markdown links that reference those files
4. Update related index files (`README.md`)

**Example**: Renaming `rules/conventions/meta/` to `rules/conventions/standards/`:

**Before**:

```
rules/conventions/meta/
├── README.md
├── ex-ru-co-me__file-naming.md
└── ex-ru-co-me__diataxis-framework.md
```

**After**:

```
rules/conventions/standards/
├── README.md
├── ex-ru-co-st__file-naming.md
└── ex-ru-co-st__diataxis-framework.md
```

**Changes required**:

- Directory: `meta/` → `standards/`
- Subdirectory code: `me` → `st` (first 2 letters)
- Prefix: `ex-ru-co-me__` → `ex-ru-co-st__`
- All files: `ex-ru-co-me__*.md` → `ex-ru-co-st__*.md`
- Links: Update all references from `./meta/ex-ru-co-me__*.md` to `./standards/ex-ru-co-st__*.md`
- Index: Update `rules/conventions/README.md` to reflect new directory name

### Reorganizing Directories

When moving files between directories:

1. Rename all moved files to update their prefix for the new location
2. Update any markdown links that reference those files
3. Update related index files in both source and destination directories

### Scaling to Arbitrary Depth

This system scales to any nesting depth:

```
ex-ru-pr-se__explicit-over-implicit.md
└─ explanation → principles → software-engineering
   (ex+pr+se)

ex-ru-wf-aywe__general-quality-gate.md
└─ explanation → workflows → ayokoding-web
   (ex+wf+aywe)
```

## 📖 Quick Reference

### Root Categories

| Category    | Prefix   | Example                  |
| ----------- | -------- | ------------------------ |
| Tutorials   | `tu__`   | `tu__getting-started.md` |
| How-To      | `hoto__` | `hoto__deploy-docker.md` |
| Reference   | `re__`   | `re__api-reference.md`   |
| Explanation | `ex__`   | _(see subdirectories)_   |

### Explanation Subdirectories (Complete Reference)

| Directory Path                                    | Prefix            | Example                                      |
| ------------------------------------------------- | ----------------- | -------------------------------------------- |
| explanation/rules/conventions/formatting          | `ex-ru-co-fo__`   | `ex-ru-co-fo__indentation.md`                |
| explanation/rules/conventions/content             | `ex-ru-co-co__`   | `ex-ru-co-co__quality.md`                    |
| explanation/rules/conventions/meta                | `ex-ru-co-me__`   | `ex-ru-co-me__file-naming.md`                |
| explanation/rules/conventions/tutorial            | `ex-ru-co-tu__`   | `ex-ru-co-tu__naming.md`                     |
| explanation/rules/conventions/hugo                | `ex-ru-co-hu__`   | `ex-ru-co-hu__shared.md`                     |
| explanation/rules/conventions/project             | `ex-ru-co-pr__`   | `ex-ru-co-pr__plans-organization.md`         |
| explanation/rules/development/workflow            | `ex-ru-de-wo__`   | `ex-ru-de-wo__commit-messages.md`            |
| explanation/rules/development/agents              | `ex-ru-de-ag__`   | `ex-ru-de-ag__ai-agents.md`                  |
| explanation/rules/development/quality             | `ex-ru-de-qu__`   | `ex-ru-de-qu__code.md`                       |
| explanation/rules/development/pattern             | `ex-ru-de-pa__`   | `ex-ru-de-pa__functional-programming.md`     |
| explanation/rules/development/hugo                | `ex-ru-de-hu__`   | `ex-ru-de-hu__development.md`                |
| explanation/rules/development/infra               | `ex-ru-de-in__`   | `ex-ru-de-in__temporary-files.md`            |
| explanation/rules/workflows/ayokoding-web         | `ex-ru-wf-aywe__` | `ex-ru-wf-aywe__general-quality-gate.md`     |
| explanation/rules/workflows/docs                  | `ex-ru-wf-do__`   | `ex-ru-wf-do__quality-gate.md`               |
| explanation/rules/workflows/meta                  | `ex-ru-wf-me__`   | `ex-ru-wf-me__workflow-pattern.md`           |
| explanation/rules/workflows/plan                  | `ex-ru-wf-pl__`   | `ex-ru-wf-pl__execution.md`                  |
| explanation/rules/workflows/wow                   | `ex-ru-wf-wo__`   | `ex-ru-wf-wo__rules-quality-gate.md`         |
| explanation/rules/principles/general              | `ex-ru-pr-ge__`   | `ex-ru-pr-ge__simplicity-over-complexity.md` |
| explanation/rules/principles/content              | `ex-ru-pr-co__`   | `ex-ru-pr-co__accessibility-first.md`        |
| explanation/rules/principles/software-engineering | `ex-ru-pr-se__`   | `ex-ru-pr-se__explicit-over-implicit.md`     |
| explanation/vision                                | `ex-vi__`         | `ex-vi__open-sharia-enterprise.md`           |

## 🔄 Migration History

### 3-Phase Reorganization (2026-01-01)

The repository underwent a comprehensive 3-phase reorganization to introduce subdirectory-based naming:

**Phase 1: Conventions Reorganization**

- Created 6 subdirectories in `rules/conventions/`
- Migrated 24 convention files to new subdirectory structure
- Updated all prefixes from `ex-co__` to `ex-ru-co-[subdirectory]__`

**Phase 2: Development Reorganization**

- Created 6 subdirectories in `rules/development/`
- Migrated 15 development files to new subdirectory structure
- Updated all prefixes from `ex-de__` to `ex-ru-de-[subdirectory]__`

**Phase 3: Workflows Reorganization**

- Created 5 subdirectories in `rules/workflows/`
- Migrated 7 workflow files to new subdirectory structure
- Updated all prefixes from `ex-wf__` to `ex-ru-wf-[subdirectory]__`

**Total Impact**: 46 files reorganized, 111+ links updated, zero broken links

**Benefits**:

- Better organization with clearer categorization
- Improved discoverability through logical grouping
- Maintained global uniqueness through subdirectory encoding
- All traceability sections preserved

## 🔗 Related Documentation

- [Linking Convention](../formatting/ex-ru-co-fo__linking.md) - How to link between documentation files
- [Diátaxis Framework](./ex-ru-co-me__diataxis-framework.md) - Understanding the documentation organization framework
- [Conventions Index](../README.md) - Index of all documentation conventions

---

**Last Updated**: 2026-01-01
