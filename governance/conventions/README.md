---
title: "Conventions"
description: Documentation conventions and standards for open-sharia-enterprise
category: explanation
tags:
  - index
  - conventions
  - standards
created: 2025-11-22
updated: 2025-12-28
---

# Conventions

Documentation conventions and standards for the open-sharia-enterprise project. These documents define how documentation should be organized, named, and linked. Contains 24 conventions covering all aspects of documentation writing and formatting.

**Governance**: All conventions in this directory serve the [Vision](../vision/open-sharia-enterprise.md) (Layer 0) and implement the [Core Principles](../principles/README.md) (Layer 1) as part of the six-layer architecture. Each convention MUST include a "Principles Implemented/Respected" section that explicitly traces back to foundational principles. See [Repository Governance Architecture](../repository-governance-architecture.md) for complete governance model and [Convention Writing Convention](./content/convention-writing.md) for structure requirements.

## 🎯 Scope

**This directory contains conventions for DOCUMENTATION:**

**✅ Belongs Here:**

- How to write and format markdown content
- Documentation organization and structure (Diátaxis)
- File naming, linking, and cross-referencing
- Visual elements in docs (diagrams, colors, emojis, math notation)
- Content quality and accessibility standards
- Documentation file formats (tutorials, plans)
- Hugo **content** writing (frontmatter, markdown, archetypes)
- Repository documentation standards (README, CONTRIBUTING)

**❌ Does NOT Belong Here (use [Development](../development/README.md) instead):**

- Software development methodologies (BDD, testing, agile)
- Build processes and tooling workflows
- Hugo **theme/layout development** (HTML templates, asset pipeline)
- Development infrastructure (temporary files, build artifacts)
- Git workflows and commit practices
- AI agent development standards
- Code quality and testing practices

## 🧪 The Layer Test for Conventions

**Question**: Does this document answer "**WHAT are the documentation rules?**"

✅ **Belongs in conventions/** if it defines:

- HOW to write markdown content (formatting, syntax, structure)
- WHAT files should be named or organized
- WHAT visual standards to follow in docs (colors, diagrams, emojis)
- WHAT content quality standards apply to documentation

❌ **Does NOT belong** if it defines:

- WHY we value something (that's a principle)
- HOW to develop software/themes (that's a development practice)
- HOW to solve a specific problem (that's a how-to guide)

**Examples**:

- "File naming must use `prefix__identifier.md` format" → ✅ Convention (documentation rule)
- "Use 2-space indentation for nested lists" → ✅ Convention (documentation formatting)
- "Hugo themes use Tailwind CSS" → ❌ Development (software practice)
- "Why we avoid time estimates in tutorials" → ❌ Principle (foundational value)

## 📋 Directory Structure

Conventions are organized into 6 semantic categories:

- **[formatting/](#-formatting-8-conventions)** - Markdown formatting, syntax, visual elements (8 conventions)
- **[content/](#-content-5-conventions)** - Content quality, validation, writing standards (5 conventions)
- **[tutorial/](#-tutorial-5-conventions)** - Tutorial creation, structure, naming (5 conventions)
- **[hugo/](#-hugo-3-conventions)** - Hugo site content conventions (3 conventions)
- **[meta/](#-meta-2-conventions)** - Documentation organization and frameworks (2 conventions)
- **[project/](#-project-1-convention)** - Project-level documentation organization (1 convention)

---

## 📐 Formatting (8 conventions)

Standards for markdown formatting, syntax, and visual elements.

- [Color Accessibility](./formatting/color-accessibility.md) - MASTER REFERENCE for all color decisions. Verified accessible color palette (Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161) supporting all color blindness types, WCAG AA standards, with complete implementation guidance for Mermaid diagrams and AI agent categorization
- [Diagrams and Schemas](./formatting/diagrams.md) - Standards for Mermaid diagrams (primary) and ASCII art (optional) with color-blind friendly colors for accessibility
- [Emoji Usage](./formatting/emoji.md) - Semantic emoji usage to enhance document scannability and engagement with accessible colored emojis
- [Indentation](./formatting/indentation.md) - Standard markdown indentation using 2 spaces per indentation level. YAML frontmatter uses 2 spaces. Code blocks use language-specific conventions
- [Linking Convention](./formatting/linking.md) - Standards for linking between documentation files using GitHub-compatible markdown. Defines two-tier formatting for rule references (first mention = markdown link, subsequent mentions = inline code)
- [Mathematical Notation](./formatting/mathematical-notation.md) - Standards for LaTeX notation for mathematical equations and formulas. Defines inline (`$...$`) vs display (`$$...$$`) delimiters, forbidden contexts (code blocks, Mermaid), Obsidian/GitHub dual compatibility
- [Nested Code Fences](./formatting/nested-code-fences.md) - Standards for properly nesting code fences when documenting markdown structure within markdown content. Defines fence depth rules (outer = 4 backticks, inner = 3 backticks), orphaned fence detection, and validation checklist
- [Timestamp Format](./formatting/timestamp.md) - Standard timestamp format using UTC+7 (Indonesian WIB Time)

## ✍️ Content (5 conventions)

Content quality standards, validation methodology, and writing guidelines.

- [Content Quality Principles](./content/quality.md) - Universal markdown content quality standards applicable to ALL repository markdown contexts (docs/, Hugo sites, plans/, root files). Covers writing style and tone (active voice, professional, concise), heading hierarchy (single H1, proper nesting), accessibility (alt text, semantic HTML, color contrast, screen readers), and formatting
- [Convention Writing](./content/convention-writing.md) - **Meta-convention** defining how to write and organize convention documents. Covers document structure, scope boundaries, quality checklist, when to create new vs update existing, length guidelines, and integration with agents. Essential reading for creating or updating conventions
- [Factual Validation](./content/factual-validation.md) - Universal methodology for validating factual correctness across all repository content using web verification (WebSearch + WebFetch). Defines core validation methodology (command syntax, features, versions, code examples, external refs, mathematical notation, diagram colors), web verification workflow, confidence classification (✅ Verified, ⚠️ Unverified, ❌ Error, 📅 Outdated)
- [OSS Documentation](./content/oss-documentation.md) - Standards for repository documentation files (README, CONTRIBUTING, ADRs, security) following open source best practices
- [README Quality](./content/readme-quality.md) - Quality standards for README.md files ensuring engagement, accessibility, and scannability. Defines problem-solution hooks, jargon elimination (plain language over corporate speak), acronym context requirements, benefits-focused language, navigation structure, and paragraph length limits (≤5 lines). **Agents**: readme**maker, readme**checker

## 🎓 Tutorial (5 conventions)

Tutorial creation, structure, naming, and content standards.

- [By Example Tutorial](./tutorial/by-example.md) - Standards for code-first by-example tutorials with 75-90 heavily annotated, self-contained, runnable examples achieving 95% coverage. Defines four-part example structure (brief explanation, Mermaid diagram when appropriate, heavily annotated code with `// =>` notation, key takeaway), self-containment rules across beginner/intermediate/advanced levels, educational comment standards, and coverage progression (0-40%, 40-75%, 75-95%). Validated by ayokoding-web-by-example-checker agent
- [Programming Language Content Standard](./tutorial/programming-language-content.md) - Universal content architecture for programming language education on ayokoding-web. Defines mandatory structure (5 tutorial levels, cookbook at position 3 in how-to/ for optimal engagement, how-to guides), coverage philosophy (0-5%, 5-30%, 0-60%, 60-85%, 85-95%), quality metrics, and completeness criteria. Benchmarked from Golang, Python, Java implementations. **See also**: [How to Add a Programming Language](../../how-to/hoto__add-programming-language.md)
- [Programming Language Tutorial Structure](./tutorial/programming-language-structure.md) - Dual-path tutorial organization for programming languages with by-concept (narrative-driven, comprehensive, 0-95% coverage through beginner/intermediate/advanced) and by-example (code-first, 75-90 annotated examples, 95% coverage for experienced developers). Defines directory structure, navigation pattern (learning paths first), foundational tutorials at root (Initial Setup, Quick Start), weight values, and optional by-example path. **Status**: Java/Elixir/Golang dual-path, Kotlin/Python/Rust single-path
- [Tutorial Convention](./tutorial/general.md) - Standards for creating learning-oriented tutorials with narrative flow, progressive scaffolding, and hands-on elements
- [Tutorial Naming](./tutorial/naming.md) - Standardized tutorial types and depth levels (Initial Setup, Quick Start, Beginner, Intermediate, Advanced, Cookbook) with "Full Set" concept for sequential learning

## 🌐 Hugo (3 conventions)

Hugo site-specific content conventions.

- [Hugo Content - ayokoding](./hugo/ayokoding.md) - Site-specific conventions for ayokoding-web (Hextra theme). Covers Hextra shortcodes, bilingual requirements, navigation patterns, weight field ordering, overview/ikhtisar file requirements, index file content separation, and optional Diátaxis structure. Use with shared convention
- [Hugo Content - OSE Platform](./hugo/ose-platform.md) - Site-specific conventions for ose-platform-web (PaperMod theme). Covers PaperMod features, English-only requirements, simple update patterns, cover image standards, and flat content structure. Use with shared convention
- [Hugo Content - Shared](./hugo/shared.md) - Common Hugo content conventions applying to all Hugo sites in this repository. Covers inherited conventions (Mathematical Notation, Color Accessibility, Diagrams, Emoji, Timestamp, Tutorial standards), adapted conventions (Indentation, Linking, File Naming, Frontmatter, Date Format), and Hugo-specific basics (Archetypes, Shortcodes, Taxonomy, Asset Organization). Foundation for all Hugo content work

## 🗂️ Meta (2 conventions)

Documentation organization frameworks and foundational concepts.

- [Diátaxis Framework](./meta/diataxis-framework.md) - Understanding the four-category documentation organization framework we use (Tutorials, How-To, Reference, Explanation)
- [File Naming Convention](./meta/file-naming.md) - Systematic approach to naming files with hierarchical prefixes encoding directory structure

## 📦 Project (1 convention)

Project-level documentation and planning organization.

- [Plans Organization](./project/plans-organization.md) - Standards for organizing project planning documents in plans/ folder including structure, naming, and workflow

## 📚 Related Documentation

- [Repository Governance Architecture](../repository-governance-architecture.md) - Complete six-layer architecture (Layer 2: Conventions)
- [Core Principles](../principles/README.md) - Layer 1: Foundational values that govern conventions
- [Development](../development/README.md) - Layer 3: Software practices (parallel governance with conventions)

---

**Last Updated**: 2026-01-01
