---
title: "Documentation Linking Convention"
description: Standards for linking between documentation files in open-sharia-fintech
category: explanation
tags:
  - linking
  - markdown
  - conventions
  - github-compatibility
created: 2025-11-22
updated: 2025-11-22
---

# Documentation Linking Convention

This document defines the standard syntax and practices for linking between documentation files in the open-sharia-fintech project. Following these conventions ensures links work consistently across GitHub web, Obsidian, and other markdown viewers.

## Why GitHub-Compatible Links?

We use GitHub-compatible markdown link syntax instead of Obsidian wiki links to ensure:

1. **Universal Compatibility** - Links work on GitHub web interface, Obsidian, VS Code, and other markdown viewers
2. **Explicit Paths** - Relative paths make it clear where files are located
3. **Version Control** - Easier to track changes and validate links in CI/CD
4. **No Ambiguity** - Full paths prevent confusion when files have similar names

## Link Syntax Standard

### Required Format

Use standard markdown link syntax with relative paths:

```markdown
[Display Text](./path/to/file.md)
```

### Key Rules

1. **Always include the `.md` extension**
   - ✅ `[Getting Started](./tutorials/tu__getting-started.md)`
   - ❌ `[Getting Started](./tutorials/tu__getting-started)`

2. **Use relative paths from the current file's location**
   - Same directory: `./file.md`
   - Parent directory: `../file.md`
   - Subdirectory: `./subdirectory/file.md`
   - Multiple levels up: `../../path/to/file.md`

3. **Use descriptive link text instead of filename identifiers**
   - ✅ `[File Naming Convention](./conventions/ex-co__file-naming-convention.md)`
   - ❌ `[ex-co__file-naming-convention](./conventions/ex-co__file-naming-convention.md)`

4. **Avoid Obsidian-only wiki link syntax**
   - ❌ `[[filename]]`
   - ❌ `[[filename|Display Text]]`

## Examples by Location

### Linking from Root README (`docs/README.md`)

```markdown
<!-- Link to category index files -->

[Tutorials](./tutorials/tu__tutorials.md)
[How-To Guides](./how-to/ht__how-to.md)
[Reference](./reference/re__reference.md)
[Explanation](./explanation/ex__explanation.md)

<!-- Link to nested files -->

[File Naming Convention](./explanation/conventions/ex-co__file-naming-convention.md)
```

### Linking from Category Index (`docs/tutorials/tu__tutorials.md`)

```markdown
<!-- Link to sibling files in same directory -->

[Getting Started](./tu__getting-started.md)
[First Deployment](./tu__first-deployment.md)

<!-- Link to parent directory -->

[Documentation Home](../README.md)

<!-- Link to other categories -->

[How-To Guides](../how-to/ht__how-to.md)
[API Reference](../reference/re__api-reference.md)
```

### Linking from Nested Files (`docs/explanation/conventions/ex-co__conventions.md`)

```markdown
<!-- Link to sibling files in same directory -->

[File Naming Convention](./ex-co__file-naming-convention.md)
[Linking Convention](./ex-co__linking-convention.md)

<!-- Link to parent directory -->

[Explanation Index](../ex__explanation.md)

<!-- Link to root -->

[Documentation Home](../../README.md)

<!-- Link to other categories -->

[Tutorials](../../tutorials/tu__tutorials.md)
```

## Correct vs. Incorrect Examples

### ✅ Correct Examples

```markdown
<!-- Descriptive text with relative path and .md extension -->

[Understanding the Diátaxis Framework](./ex-co__diataxis-framework.md)
[How to Configure API](../how-to/ht__configure-api.md)
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

[Conventions](/docs/explanation/conventions/ex-co__conventions.md)

<!-- Using filename as link text -->

[ex-co\_\_file-naming-convention.md](./ex-co__file-naming-convention.md)
```

## External Links

For links to external resources:

```markdown
<!-- Standard markdown links -->

[Diátaxis Framework](https://diataxis.fr/)
[Obsidian](https://obsidian.md/)
[GitHub](https://github.com/wahidyankf/open-sharia-fintech)
```

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

## Verification Checklist

Before committing documentation with links:

- [ ] All links use `[Text](./path/to/file.md)` syntax
- [ ] All internal links include `.md` extension
- [ ] All paths are relative (not absolute)
- [ ] Link text is descriptive (not filename-based)
- [ ] No Obsidian wiki links (`[[...]]`) used
- [ ] Manually verified links point to existing files
- [ ] Paths tested from the current file's location

## Link Validation

When creating documentation, verify links by:

1. **Manual Testing**: Click links in your markdown viewer
2. **File Existence**: Use `ls` or `find` to verify target files exist
3. **Path Correctness**: Count `../` levels to ensure correct relative path
4. **Extension Check**: Confirm `.md` is present in all internal links

## Related Documentation

- [File Naming Convention](./ex-co__file-naming-convention.md) - How to name documentation files
- [Conventions Index](./README.md) - Overview of all documentation conventions

---

**Last Updated**: November 22, 2025
