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
updated: 2025-11-27
---

# Documentation Linking Convention

This document defines the standard syntax and practices for linking between documentation files in the open-sharia-enterprise project. Following these conventions ensures links work consistently across GitHub web, Obsidian, and other markdown viewers.

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
   - ✅ `[File Naming Convention](./conventions/ex-co__file-naming-convention.md)`
   - ❌ `[ex-co__file-naming-convention](./conventions/ex-co__file-naming-convention.md)`

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

[File Naming Convention](./explanation/conventions/ex-co__file-naming-convention.md)
[Conventions Index](./explanation/conventions/README.md)
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

### Linking from Nested Files (`docs/explanation/conventions/README.md`)

```markdown
<!-- Link to sibling files in same directory -->

[File Naming Convention](./ex-co__file-naming-convention.md)
[Linking Convention](./ex-co__linking-convention.md)

<!-- Link to parent directory -->

[Explanation Index](../README.md)

<!-- Link to root -->

[Documentation Home](../../README.md)

<!-- Link to other categories -->

[Tutorials](../../tutorials/README.md)
```

### Linking from Journal Entries (`docs/journals/2025-11/2025-11-27.md`)

Journal files are nested 2 levels deep from `docs/`, so they require `../../` to reach the `docs/` root:

```markdown
<!-- Link to docs/ root (2 levels up) -->

[Documentation Home](../../README.md)

<!-- Link to explanation files (2 levels up, then down) -->

[File Naming Convention](../../explanation/conventions/ex-co__file-naming-convention.md)
[Information Security Overview](../../explanation/information-security/ex-inse__infosec.md)

<!-- Link to other categories (2 levels up, then down) -->

[Tutorials](../../tutorials/README.md)
[How-To Guides](../../how-to/README.md)

<!-- Link to sibling journal entries (same directory) -->

[Yesterday's Journal](./2025-11-26.md)

<!-- Link to other months (1 level up, then down) -->

[October Journal](../2025-10/2025-10-31.md)
```

## Correct vs. Incorrect Examples

### ✅ Correct Examples

```markdown
<!-- Descriptive text with relative path and .md extension -->

[Understanding the Diátaxis Framework](./ex-co__diataxis-framework.md)
[How to Configure API](../how-to/hoto__configure-api.md)
[Transaction Endpoints Reference](../../reference/api/re-ap__endpoints.md)

<!-- Links with context -->

See the [file naming convention](./ex-co__file-naming-convention.md) for details.
For more information, refer to our [authentication tutorial](../../tutorials/auth/tu-au__getting-started.md).
```

### ❌ Incorrect Examples

```markdown
<!-- Obsidian wiki links (not compatible with GitHub web) -->

[[ex-co__diataxis-framework]]
[[ex-co__diataxis-framework|Diátaxis Framework]]

<!-- Missing .md extension -->

[Diátaxis Framework](./ex-co__diataxis-framework)

<!-- Absolute paths -->

[Conventions](/docs/explanation/conventions/README.md)

<!-- Using filename as link text -->

[ex-co\_\_file-naming-convention.md](./ex-co__file-naming-convention.md)

<!-- Wrong number of ../ for nesting depth -->
<!-- From docs/journals/2025-11/2025-11-27.md (2 levels deep) -->

[File Naming Convention](../explanation/conventions/ex-co__file-naming-convention.md) <!-- Only 1 ../ instead of 2 -->
[Documentation Home](./README.md) <!-- Should be ../../README.md -->

<!-- From docs/explanation/conventions/README.md (2 levels deep) -->

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

| File Location                                            | Depth from `docs/` | To reach `docs/` root |
| -------------------------------------------------------- | ------------------ | --------------------- |
| `docs/README.md`                                         | 0 (at root)        | `.` (current dir)     |
| `docs/tutorials/README.md`                               | 1 level deep       | `../`                 |
| `docs/explanation/conventions/ex-co__linking.md`         | 2 levels deep      | `../../`              |
| `docs/journals/2025-11/2025-11-27.md`                    | 2 levels deep      | `../../`              |
| `docs/reference/api/endpoints/re-ap-en__transactions.md` | 3 levels deep      | `../../../`           |

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

#### From 2-Level Deep Files (`docs/journals/2025-11/2025-11-27.md`)

```markdown
<!-- To docs/ root (up 2 levels) -->

[Documentation Home](../../README.md)

<!-- To other categories (up 2, down 1) -->

[Tutorials](../../tutorials/README.md)
[Explanation](../../explanation/README.md)

<!-- To deeply nested files (up 2, down 2) -->

[File Naming Convention](../../explanation/conventions/ex-co__file-naming-convention.md)
```

#### From 3-Level Deep Files (`docs/reference/api/endpoints/re-ap-en__transactions.md`)

```markdown
<!-- To docs/ root (up 3 levels) -->

[Documentation Home](../../../README.md)

<!-- To other categories (up 3, down 1) -->

[Tutorials](../../../tutorials/README.md)

<!-- To parent categories (up 1, 2, or 3) -->

[Endpoints Index](../README.md) <!-- Parent directory -->
[API Index](../../README.md) <!-- Grandparent directory -->
[Reference Index](../../../reference/README.md) <!-- Great-grandparent -->
```

### Verification Tip

To verify your relative path is correct:

1. **Start at your current file's location**
2. **Count each `../` as going up one directory level**
3. **Count each `/dirname/` as going down one level**
4. **Verify you end at the target file**

Example from `docs/journals/2025-11/2025-11-27.md` to `docs/explanation/conventions/ex-co__linking-convention.md`:

```
Start:  docs/journals/2025-11/2025-11-27.md
  ../   docs/journals/                      (up 1)
  ../   docs/                               (up 2)
  explanation/  docs/explanation/           (down 1)
  conventions/  docs/explanation/conventions/  (down 2)
  ex-co__linking-convention.md              (target file)

Final path: ../../explanation/conventions/ex-co__linking-convention.md
```

## Hugo Content Linking

**Important**: This convention applies to documentation in the `docs/` directory. Hugo content in `apps/ayokoding-web/` and `apps/ose-platform-web/` uses different linking rules:

- **Hugo internal links** use absolute paths starting with `/` (e.g., `/learn/ai/chat-with-pdf`)
- **Hugo links omit** the `.md` extension
- **Why different**: Hugo renders the same navigation content in different page contexts (sidebar, mobile menu, homepage), so relative paths would resolve incorrectly

See [Hugo Content Convention - Shared](./ex-co__hugo-content-shared.md) for complete Hugo linking standards applicable to all sites, and [Hugo Content Convention - ayokoding](./ex-co__hugo-content-ayokoding.md) or [Hugo Content Convention - OSE Platform](./ex-co__hugo-content-ose-platform.md) for site-specific linking patterns.

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

![Architecture](./images/ex-co-ar__architecture-diagram.png)
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

- [File Naming Convention](./ex-co__file-naming-convention.md) - How to name documentation files
- [Conventions Index](./README.md) - Overview of all documentation conventions

---

**Last Updated**: 2025-11-27
