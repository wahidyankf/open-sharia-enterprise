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
updated: 2025-12-01
---

# Documentation File Naming Convention

A systematic approach to naming files in the open-sharia-enterprise documentation that ensures clarity, organization, and discoverability while maintaining a logical hierarchy.

## 📋 Overview

The naming convention serves three critical purposes:

1. **Hierarchical Organization** - File names encode the folder path, making it possible to identify a file's location just by reading the filename
2. **Discoverability** - Consistent prefixes make it easy to find related files across the documentation
3. **Global Uniqueness** - Hierarchical prefixes ensure no two files have the same name, preventing ambiguity across the entire documentation vault

## 🎯 Scope

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

## 📝 The Universal Pattern

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

## 🔧 How to Build a Prefix

The prefix encodes the folder path using abbreviations separated by single hyphens. Each level of nesting adds another abbreviation segment.

### Root Directory Prefixes

The four main Diátaxis categories use consistent 4-character prefixes:

- `tu` - Tutorials (`docs/tutorials/`) - Common/short root: 2 chars
- `hoto` - How-To (`docs/how-to/`) - Compound root: 4 chars (2 from each word)
- `refe` - Reference (`docs/reference/`) - Single word root: 4 chars
- `ex` - Explanation (`docs/explanation/`) - Common/short root: 2 chars

**Note on Directory Naming:**

The directory names follow semantic conventions from the Diátaxis framework:

- `tutorials/` - **Plural** (collection of discrete tutorial documents)
- `how-to/` - **Singular category name** (the folder contains "how-to guides", matching the category name)
- `reference/` - **Singular/mass noun** (reference material as a collective whole, like "reference library")
- `explanation/` - **Singular/mass noun** (explanatory material as a collective whole)

This apparent inconsistency is intentional and follows standard documentation conventions. Only `tutorials/` is plural because tutorials are naturally countable discrete units, while the other categories represent types of content that are better expressed as mass nouns.

### Abbreviation Strategy

Use a systematic encoding rule based on directory characteristics:

**For Root Directories:**

1. **Common/short roots** - 2 characters (`tutorials` → `tu`, `explanation` → `ex`)
2. **Compound roots** - 4 characters, 2 from each word (`how-to` → `hoto`)
3. **Single word roots** - 4 characters (`reference` → `refe`)

**For Subdirectories:**

1. **Hyphenated compounds** - Concatenate first 2 letters of each word WITHOUT dash (`ai-engineering` → `aien`, `business-and-finance` → `bufi`, `system-design` → `syde`, `information-security` → `inse`, `how-to` → `hoto`)
2. **Single words** - First 2 or 4 characters based on length (`conventions` → `co`, `development` → `de`, `toolings` → `to`)

**Key Rule**: For hyphenated directory names, take first 2 letters of EACH word and concatenate them WITHOUT inserting a dash. The dash in the directory name does NOT appear in the abbreviation.

Examples: `tu__getting-started.md` (tutorials), `tu-aien__chat-with-pdf.md` (tutorials/ai-engineering), `hoto__deploy-app.md` (how-to), `re__api-reference.md` (reference), `ex-co__file-naming-convention.md` (explanation/conventions), `ex-inse__infosec.md` (explanation/information-security)

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
- hoto-deve\_\_configure-rate-limiting.md
- refe-appe\_\_transaction-endpoints.md
- ex-conv\_\_file-naming-convention.md

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
hoto-de__release-process-2025-11.md
```

**Frontmatter dates:** Frontmatter fields (`created`, `updated`) also use ISO 8601 format:

```yaml
---
created: 2025-11-19
updated: 2025-11-22
---
```

## 🔍 Special Cases

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

**Daily entries:**

```
journals/YYYY-MM/YYYY-MM-DD.md
```

**Monthly summaries:**

```
journals/YYYY-MM/summary.md
```

These patterns are configured in `.obsidian/daily-notes.json` and don't use the prefix system.

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
docs/explanation/sharia-compliance/ex-shco__murabaha-flow.md
docs/explanation/sharia-compliance/ex-shco__murabaha-flow-diagram.png
```

## 🔄 Maintenance and Scalability

### Adding New Directories

When creating a new subdirectory:

1. Apply the 2-letter rule to create the abbreviation
2. Add this abbreviation to the prefix of all files in that directory
3. Update any related index files

**Example**: Creating `docs/how-to/deployment/`:

- Directory: `docs/how-to/deployment/`
- Prefix: `hoto-de__` (hoto = how-to, de = deployment)
- Files: `hoto-de__deploy-to-production.md`, `hoto-de__configure-ci-cd.md`

### Renaming Directories

When renaming a directory in `docs/`, you **must rename all files within that directory** to update their prefixes accordingly. This is because the file naming convention encodes the directory path in the filename prefix.

**Exception**: Files in `docs/journals/` follow a different naming pattern (`YYYY-MM/YYYY-MM-DD.md`) and are exempt from this requirement.

**Process**:

1. Rename the directory
2. Rename all files in that directory to update their prefix
3. Update any markdown links that reference those files
4. Update related index files (`README.md`)

**Example**: Renaming `docs/explanation/security/` to `docs/explanation/information-security/`:

**Before**:

```
docs/explanation/security/
├── README.md
├── ex-se__authentication.md
└── ex-se__authorization.md
```

**After**:

```
docs/explanation/information-security/
├── README.md
├── ex-inse__authentication.md
└── ex-inse__authorization.md
```

**Changes required**:

- Directory: `security/` → `information-security/`
- Prefix: `ex-se__` → `ex-inse__` (se = security, inse = in + se concatenated WITHOUT dash)
- All files: `ex-se__*.md` → `ex-inse__*.md`
- Links: Update all references from `./security/ex-se__*.md` to `./information-security/ex-inse__*.md`
- Index: Update `docs/explanation/README.md` to reflect new directory name

### Reorganizing Directories

When moving files between directories:

1. Rename all moved files to update their prefix for the new location
2. Update any markdown links that reference those files
3. Update related index files in both source and destination directories

### Scaling to Arbitrary Depth

This system scales to any nesting depth:

```
tu-au-oa-fl__authorization-code-flow.md
└─ tutorials → authentication → oauth → flows
   (tu+au+oa+fl)
```

## 📖 Quick Reference

| Category    | Prefix   | Example                  |
| ----------- | -------- | ------------------------ |
| Tutorials   | `tu__`   | `tu__getting-started.md` |
| How-To      | `hoto__` | `hoto__deploy-docker.md` |
| Reference   | `re__`   | `re__api-reference.md`   |
| Explanation | `ex__`   | `ex__architecture.md`    |

**Common Subdirectory Prefixes:**

| Directory Path                                              | Prefix                | Example                                        |
| ----------------------------------------------------------- | --------------------- | ---------------------------------------------- |
| explanation/conventions                                     | `ex-co__`             | `ex-co__file-naming-convention.md`             |
| explanation/development                                     | `ex-de__`             | `ex-de__ai-agents.md`                          |
| explanation/information-security                            | `ex-inse__`           | `ex-inse__infosec.md`                          |
| explanation/information-security/toolings                   | `ex-inse-to__`        | `ex-inse-to__gobuster.md`                      |
| tutorials/ai-engineering                                    | `tu-aien__`           | `tu-aien__chat-with-pdf.md`                    |
| tutorials/business-and-finance                              | `tu-bufi__`           | `tu-bufi__accounting.md`                       |
| tutorials/software-engineering/system-design                | `tu-soen-syde__`      | `tu-soen-syde__ai-personal-finance-advisor.md` |
| tutorials/software-engineering/programming-languages/golang | `tu-soen-prla-gola__` | `tu-soen-prla-gola__quick-start.md`            |

## 🔗 Related Documentation

- [Linking Convention](./ex-co__linking-convention.md) - How to link between documentation files
- [Diátaxis Framework](./ex-co__diataxis-framework.md) - Understanding the documentation organization framework
- [Conventions Index](./README.md) - Index of all documentation conventions

---

**Last Updated**: 2025-12-01
