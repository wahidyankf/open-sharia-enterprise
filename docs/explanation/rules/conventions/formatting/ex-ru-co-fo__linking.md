---
title: "Documentation Linking Convention"
description: Standards for linking between documentation files in open-sharia-enterprise
category: explanation
subcategory: conventions
tags:
  - linking
  - markdown
  - conventions
  - github-compatibility
created: 2025-11-22
updated: 2025-12-28
---

# Documentation Linking Convention

This document defines the standard syntax and practices for linking between documentation files in the open-sharia-enterprise project. Following these conventions ensures links work consistently across GitHub web, Obsidian, and other markdown viewers.

## Principles Implemented/Respected

This convention implements the following core principles:

- **[Explicit Over Implicit](../principles/software-engineering/ex-ru-pr-se__explicit-over-implicit.md)**: Uses explicit relative paths (`./path/to/file.md`) instead of implicit wiki-style links (`[[filename]]`). File extensions are always included, making it clear what type of file is being referenced. No magic linking behavior - every path is stated clearly.

- **[Accessibility First](../principles/content/ex-ru-pr-co__accessibility-first.md)**: Descriptive link text (not filenames) improves screen reader experience. Users hear meaningful context like "File Naming Convention" instead of cryptic identifiers like "ex-co\_\_file-naming-convention".

## Purpose

This convention establishes the standard linking format for all markdown files in the repository. It ensures links are GitHub-compatible, use relative paths with `.md` extensions, and follow consistent patterns across documentation and Hugo sites. This prevents broken links and maintains portability.

## Scope

### What This Convention Covers

- **Markdown link syntax** - `[Display Text](./path/to/file.md)` format
- **Relative vs. absolute paths** - When to use each
- **Extension requirements** - `.md` extension for docs/, no extension for Hugo sites
- **Cross-directory linking** - How to link between different documentation areas
- **External link formatting** - How to format links to external resources

### What This Convention Does NOT Cover

- **Link validation** - Covered by docs\_\_link-general-checker and ayokoding-web-link-checker agents
- **Link text quality** - Descriptive link text is covered in [Content Quality Principles](../content/ex-ru-co-co__quality.md)
- **Hugo site URLs** - Hugo-specific linking covered in Hugo content conventions
- **Anchor links** - Deep linking to specific sections (implementation detail)

## 🎯 Why GitHub-Compatible Links?

We use GitHub-compatible markdown link syntax instead of Obsidian wiki links to ensure:

1. **Universal Compatibility** - Links work on GitHub web interface, Obsidian, VS Code, and other markdown viewers
2. **Explicit Paths** - Relative paths make it clear where files are located
3. **Version Control** - Easier to track changes and validate links in CI/CD
4. **No Ambiguity** - Full paths prevent confusion when files have similar names

## 📝 Link Syntax Standard

### Required Format

Use standard markdown link syntax with relative paths:

```markdown
[Display Text](./path/to/file.md)
```

### Key Rules

1. **Always include the `.md` extension**
   - ✅ `[Initial Setup](./tutorials/tu__initial-setup.md)`
   - ❌ `[Initial Setup](./tutorials/tu__initial-setup)`

2. **Use relative paths from the current file's location**
   - Same directory: `./file.md`
   - Parent directory: `../file.md`
   - Subdirectory: `./subdirectory/file.md`
   - Multiple levels up: `../../path/to/file.md`
   - **Important**: The number of `../` depends on your file's nesting depth (see [Nested Directory Linking](#nested-directory-linking))

3. **Use descriptive link text instead of filename identifiers**
   - ✅ `[File Naming Convention](./conventions/meta/ex-ru-co-me__file-naming.md)`
   - ❌ `[ex-co__file-naming-convention](./conventions/meta/ex-ru-co-me__file-naming.md)`

4. **Avoid Obsidian-only wiki link syntax**
   - ❌ `[[filename]]`
   - ❌ `[[filename|Display Text]]`

## 🧪 Examples by Location

### Linking from Root README (`docs/README.md`)

```markdown
<!-- Link to category index files -->

[Tutorials](./tutorials/README.md)
[How-To Guides](./how-to/README.md)
[Reference](./reference/README.md)
[Explanation](./explanation/README.md)

<!-- Link to nested files -->

[File Naming Convention](./explanation/rules/conventions/meta/ex-ru-co-me__file-naming.md)
[Conventions Index](./explanation/rules/conventions/README.md)
```

### Linking from Category Index (`docs/tutorials/README.md`)

```markdown
<!-- Link to sibling files in same directory -->

[Initial Setup](./tu__initial-setup.md)
[First Deployment](./tu__first-deployment.md)

<!-- Link to parent directory -->

[Documentation Home](../README.md)

<!-- Link to other categories -->

[How-To Guides](../how-to/README.md)
[API Reference](../reference/README.md)
```

### Linking from Nested Files (`docs/explanation/rules/conventions/README.md`)

```markdown
<!-- Link to sibling files in same directory -->

[File Naming Convention](../meta/ex-ru-co-me__file-naming.md)
[Linking Convention](./ex-ru-co-fo__linking.md)

<!-- Link to parent directory -->

[Explanation Index](../README.md)

<!-- Link to root -->

[Documentation Home](../../README.md)

<!-- Link to other categories -->

[Tutorials](../../tutorials/README.md)
```

## Correct vs. Incorrect Examples

### ✅ Correct Examples

```markdown
<!-- Descriptive text with relative path and .md extension -->

[Understanding the Diátaxis Framework](../meta/ex-ru-co-me__diataxis-framework.md)
[Monorepo Structure](../../reference/re__monorepo-structure.md)
[AI Agents Convention](../development/agents/ex-ru-de-ag__ai-agents.md)

<!-- Links with context -->

See the [file naming convention](../meta/ex-ru-co-me__file-naming.md) for details.
For more information, refer to our [automation principle](../principles/software-engineering/ex-ru-pr-se__automation-over-manual.md).
```

### ❌ Incorrect Examples

```markdown
<!-- Obsidian wiki links (not compatible with GitHub web) -->

[[ex-co__diataxis-framework]]
[[ex-co__diataxis-framework|Diátaxis Framework]]

<!-- Missing .md extension -->

[Diátaxis Framework](./ex-co__diataxis-framework)

<!-- Absolute paths -->

[Conventions](/docs/explanation/rules/conventions/README.md)

<!-- Using filename as link text -->

[ex-co\_\_file-naming-convention.md](../meta/ex-ru-co-me__file-naming.md)

<!-- Wrong number of ../ for nesting depth -->
<!-- From docs/explanation/rules/conventions/formatting/ex-ru-co-fo__linking.md (2 levels deep) -->

[Documentation Home](./README.md) <!-- Should be ../../README.md -->
[Tutorials](../tutorials/README.md) <!-- Only 1 ../ instead of 2 -->

<!-- From docs/explanation/rules/conventions/README.md (2 levels deep) -->

[Documentation Home](../../../README.md) <!-- Too many ../ (3 instead of 2) -->
```

## 🌐 External Links

For links to external resources:

```markdown
<!-- Standard markdown links -->

[Diátaxis Framework](https://diataxis.fr/)
[Obsidian](https://obsidian.md/)
[GitHub](https://github.com/wahidyankf/open-sharia-enterprise)
```

## 📁 Nested Directory Linking

Understanding relative paths is crucial when linking from files at different nesting depths. The number of `../` you need depends on how deep your current file is nested.

### How to Calculate Relative Paths

1. **Count how many directories deep your current file is** from the `docs/` root
2. **Use that many `../` to reach the `docs/` root**
3. **Then navigate down** to your target file

### Nesting Depth Reference

| File Location                                                              | Depth from `docs/` | To reach `docs/` root |
| -------------------------------------------------------------------------- | ------------------ | --------------------- |
| `docs/README.md`                                                           | 0 (at root)        | `.` (current dir)     |
| `docs/tutorials/README.md`                                                 | 1 level deep       | `../`                 |
| `docs/explanation/rules/conventions/README.md`                             | 2 levels deep      | `../../`              |
| `docs/explanation/rules/conventions/formatting/ex-ru-co-fo__linking.md`    | 3 levels deep      | `../../../`           |
| `docs/explanation/rules/principles/software-engineering/ex-ru-pr-se__*.md` | 3 levels deep      | `../../../`           |

### Common Linking Patterns

#### From 1-Level Deep Files (`docs/explanation/README.md`)

```markdown
<!-- To sibling directories (same level) -->

[Conventions](./conventions/README.md)
[Development](./development/README.md)

<!-- To parent (docs/ root) -->

[Documentation Home](../README.md)

<!-- To other categories (up 1, down 1) -->

[Tutorials](../tutorials/README.md)
[How-To](../how-to/README.md)
```

#### From 2-Level Deep Files (`docs/explanation/rules/conventions/formatting/ex-ru-co-fo__linking.md`)

```markdown
<!-- To docs/ root (up 2 levels) -->

[Documentation Home](../../README.md)

<!-- To other categories (up 2, down 1) -->

[Tutorials](../../tutorials/README.md)
[How-To](../../how-to/README.md)

<!-- To sibling files (same directory) -->

[File Naming Convention](../meta/ex-ru-co-me__file-naming.md)
```

#### From 3-Level Deep Files (`docs/explanation/rules/principles/software-engineering/ex-ru-pr-se__explicit-over-implicit.md`)

```markdown
<!-- To docs/ root (up 3 levels) -->

[Documentation Home](../../../README.md)

<!-- To other categories (up 3, down 1 or 2) -->

[Tutorials](../../../tutorials/README.md)
[Conventions](../../conventions/README.md)

<!-- To parent categories (up 1, 2, or 3) -->

[Software Engineering Principles](../README.md) <!-- Parent directory -->
[All Principles](../../README.md) <!-- Grandparent directory -->
[Explanation Index](../../../README.md) <!-- Great-grandparent -->
```

### Verification Tip

To verify your relative path is correct:

1. **Start at your current file's location**
2. **Count each `../` as going up one directory level**
3. **Count each `/dirname/` as going down one level**
4. **Verify you end at the target file**

Example from `docs/explanation/rules/conventions/meta/ex-ru-co-me__file-naming.md` to `docs/tutorials/README.md`:

```
Start:  docs/explanation/rules/conventions/meta/ex-ru-co-me__file-naming.md
  ../   docs/explanation/rules/conventions/       (up 1)
  ../   docs/explanation/                   (up 2)
  ../   docs/                               (up 3 - wait, only need 2!)

Actually:
Start:  docs/explanation/rules/conventions/meta/ex-ru-co-me__file-naming.md
  ../   docs/explanation/                   (up 1)
  ../   docs/                               (up 2)
  tutorials/  docs/tutorials/               (down 1)
  README.md                                 (target file)

Final path: ../../tutorials/README.md
```

## Hugo Content Linking

**Important**: This convention applies to documentation in the `docs/` directory. Hugo content in `apps/ayokoding-web/` and `apps/ose-platform-web/` uses different linking rules:

- **Hugo internal links** use absolute paths starting with `/` (e.g., `/learn/ai/chat-with-pdf`)
- **Hugo links omit** the `.md` extension
- **Why different**: Hugo renders the same navigation content in different page contexts (sidebar, mobile menu, homepage), so relative paths would resolve incorrectly

See [Hugo Content Convention - Shared](../hugo/ex-ru-co-hu__shared.md) for complete Hugo linking standards applicable to all sites, and [Hugo Content Convention - ayokoding](../hugo/ex-ru-co-hu__ayokoding.md) or [Hugo Content Convention - OSE Platform](../hugo/ex-ru-co-hu__ose-platform.md) for site-specific linking patterns.

## Anchor Links (Same Page)

For linking to headings within the same document:

```markdown
[See Examples](#examples-by-location)
[Jump to Key Rules](#key-rules)
```

## Image Links

For embedding images:

```markdown
<!-- Same directory -->

![Diagram](./ex-co__diagram.png)

<!-- Subdirectory -->

![Architecture](./images/ex-ru-co-ar__architecture-diagram.png)
```

## ✅ Verification Checklist

Before committing documentation with links:

- [ ] All links use `[Text](./path/to/file.md)` syntax
- [ ] All internal links include `.md` extension
- [ ] All paths are relative (not absolute)
- [ ] Link text is descriptive (not filename-based)
- [ ] No Obsidian wiki links (`[[...]]`) used
- [ ] Manually verified links point to existing files
- [ ] Paths tested from the current file's location

## 🔍 Link Validation

When creating documentation, verify links by:

1. **Manual Testing**: Click links in your markdown viewer
2. **File Existence**: Use `ls` or `find` to verify target files exist
3. **Path Correctness**: Count `../` levels to ensure correct relative path
4. **Extension Check**: Confirm `.md` is present in all internal links

## 🔗 Related Documentation

- [File Naming Convention](../meta/ex-ru-co-me__file-naming.md) - How to name documentation files
- [Conventions Index](./README.md) - Overview of all documentation conventions

---

**Last Updated**: 2025-11-27

## 🔗 When to Link Rule References

When referencing repository rules (visions, principles, conventions, development practices, workflows), use a **two-tier formatting approach**:

### First Mention: MUST Use Markdown Link

The **first mention** of a rule in any document section MUST use a markdown link:

```markdown
[Rule Name](./path/to/rule.md)
```

**Rule categories requiring this treatment:**

- Vision documents (`docs/explanation/rules/vision/`)
- Core Principles (`docs/explanation/rules/principles/`)
- Conventions (`docs/explanation/rules/conventions/`)
- Development practices (`docs/explanation/rules/development/`)
- Workflows (`docs/explanation/rules/workflows/`)

### Subsequent Mentions: MUST Use Inline Code

**Subsequent mentions** of the same rule within the same section MUST use inline code formatting:

```markdown
`rule-name`
```

### Examples

#### ✅ Correct - Two-Tier Formatting

```markdown
## Implementation Details

This feature implements the [Linking Convention](./ex-ru-co-fo__linking.md) by using relative paths. The `Linking Convention` requires `.md` extensions, which helps maintain compatibility across viewers. When applying `Linking Convention` rules, verify all paths are relative.
```

**Analysis:**

- First mention: `[Linking Convention](./ex-ru-co-fo__linking.md)` ✅ (markdown link)
- Second mention: `` `Linking Convention` `` ✅ (inline code)
- Third mention: `` `Linking Convention` `` ✅ (inline code)

#### ✅ Correct - Multiple Rules

```markdown
## Standards Compliance

All documentation follows the [File Naming Convention](../meta/ex-ru-co-me__file-naming.md) and [Linking Convention](./ex-ru-co-fo__linking.md). The `File Naming Convention` defines prefix patterns, while the `Linking Convention` specifies link syntax. Both `File Naming Convention` and `Linking Convention` are validated by docs-checker.
```

**Analysis:**

- File Naming Convention: First mention (link) ✅, subsequent mentions (inline code) ✅
- Linking Convention: First mention (link) ✅, subsequent mentions (inline code) ✅

#### ❌ Incorrect - All Plain Text

```markdown
## Standards Compliance

All documentation follows the Linking Convention. The Linking Convention requires .md extensions. When applying Linking Convention rules, verify paths.
```

**Issue:** No links or inline code formatting - readers cannot navigate to convention document.

#### ❌ Incorrect - All Links

```markdown
## Standards Compliance

All documentation follows the [Linking Convention](./ex-ru-co-fo__linking.md). The [Linking Convention](./ex-ru-co-fo__linking.md) requires .md extensions. When applying [Linking Convention](./ex-ru-co-fo__linking.md) rules, verify paths.
```

**Issue:** Redundant links create visual clutter and maintenance burden.

#### ❌ Incorrect - All Inline Code

```markdown
## Standards Compliance

All documentation follows the `Linking Convention`. The `Linking Convention` requires .md extensions. When applying `Linking Convention` rules, verify paths.
```

**Issue:** First mention lacks navigable link - readers cannot discover the convention document.

### Exclusions

This two-tier formatting does NOT apply to:

- **Code blocks** - Already formatted as code
- **Quoted text** - Preserve original formatting
- **File path specifications** - Use literal paths
- **Meta-discussion about naming** - When discussing rule names as strings

**Example exclusions:**

````markdown
<!-- Code block - already formatted -->

```bash
# Apply linking-convention rules
validate_links
```
````

<!-- Quoted text - preserve original -->

> The author wrote "see linking convention for details"

<!-- File path - literal path -->

The rule is defined in `docs/explanation/rules/conventions/formatting/ex-ru-co-fo__linking.md`

<!-- Meta-discussion - discussing the name itself -->

We renamed "link-convention" to "linking-convention" for clarity

```

### Validation

The [docs\_\_checker agent](../../.claude/agents/docs\_\_checker.md) validates this two-tier formatting requirement:

- **First mention without link** → CRITICAL issue (breaks navigation)
- **Subsequent mention without inline code** → HIGH issue (convention violation)
- **All mentions improperly formatted** → CRITICAL issue (complete non-compliance)

```
