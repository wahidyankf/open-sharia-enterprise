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

Documentation conventions and standards for the open-sharia-enterprise project. These documents define how documentation should be organized, named, and linked. Contains 26 conventions covering all aspects of documentation writing and formatting.

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

Conventions are organized into 7 semantic categories:

- **[formatting/](#-formatting-8-conventions)** - Markdown formatting, syntax, visual elements (8 conventions)
- **[content/](#-content-5-conventions)** - Content quality, validation, writing standards (5 conventions)
- **[tutorial/](#-tutorial-6-conventions)** - Tutorial creation, structure, naming (6 conventions)
- **[hugo/](#-hugo-3-conventions)** - Hugo site content conventions (3 conventions)
- **[programming/](#-programming-1-convention)** - Software design and coding standards cross-references (1 convention)
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

## 🎓 Tutorial (6 conventions)

Tutorial creation, structure, naming, and content standards applying to **all tutorial content** (docs/, ayokoding-web, ose-platform-web, anywhere).

- [By Example Tutorial](./tutorial/by-example.md) - **Universal** standards for code-first by-example tutorials (Component 3 of Full Set Tutorial Package - PRIORITY) with 75-85 heavily annotated, self-contained, runnable examples achieving 95% coverage. Defines five-part example structure (brief explanation, optional Mermaid diagram, heavily annotated code with `// =>` notation, key takeaway), self-containment rules across beginner/intermediate/advanced levels, educational comment standards (1-2.25 ratio), and coverage progression (0-40%, 40-75%, 75-95%). Prioritized for fast learning ("move fast"). Applies to all programming language tutorials across the repository
- [By Concept Tutorial](./tutorial/by-concept.md) - **Universal** standards for narrative-driven by-concept tutorials (Component 4 of Full Set Tutorial Package) achieving 95% coverage through comprehensive concept explanations. Applies to all programming language tutorials across the repository
- [Programming Language Content Standard](./tutorial/programming-language-content.md) - **Universal** Full Set Tutorial Package architecture for programming language education. Defines 5 mandatory components with by-example prioritized first (Component 3: code-first 75-85 examples for fast learning), by-concept second (Component 4: narrative-driven for deep learning), plus foundational tutorials, cookbook in tutorials/, and supporting docs. Coverage philosophy (0-30% foundational, 95% learning tracks), quality metrics, and completeness criteria. Applies to all programming language tutorials (docs/, ayokoding-web, anywhere). **See also**: [How to Add a Programming Language](../../docs/how-to/hoto__add-programming-language.md)
- [Programming Language Tutorial Structure](./tutorial/programming-language-structure.md) - **Universal** directory structure for Full Set Tutorial Package with 5 mandatory components: foundational tutorials (initial-setup, quick-start), by-example track (Component 3 - PRIORITY: code-first with 75-85 examples, 95% coverage, "move fast"), by-concept track (Component 4: narrative-driven, 95% coverage, "learn deep"), and cookbook (Component 5: practical recipes in tutorials/cookbook/). Defines navigation pattern (by-example first), weight values, and creation order. All 5 components required for complete language content. Applies to all programming language tutorials across the repository
- [Tutorial Convention](./tutorial/general.md) - **Universal** standards for creating learning-oriented tutorials with narrative flow, progressive scaffolding, and hands-on elements. Covers all 7 tutorial types that combine into Full Set Tutorial Package. Applies to all tutorial content (docs/, ayokoding-web, ose-platform-web, anywhere)
- [Tutorial Naming](./tutorial/naming.md) - **Universal** Full Set Tutorial Package definition (5 mandatory components) and tutorial type standards (Initial Setup, Quick Start, Beginner, Intermediate, Advanced, Cookbook, By Example). Replaces old "Full Set" concept (5 sequential levels) with new architecture emphasizing component completeness. Applies to all tutorial content across the repository

## 🌐 Hugo (3 conventions)

Hugo site-specific content conventions.

- [Hugo Content - ayokoding](./hugo/ayokoding.md) - Site-specific conventions for ayokoding-web (Hextra theme). Covers Hextra shortcodes, bilingual requirements, navigation patterns, weight field ordering, overview/ikhtisar file requirements, index file content separation, and optional Diátaxis structure. Use with shared convention
- [Hugo Content - OSE Platform](./hugo/ose-platform.md) - Site-specific conventions for ose-platform-web (PaperMod theme). Covers PaperMod features, English-only requirements, simple update patterns, cover image standards, and flat content structure. Use with shared convention
- [Hugo Content - Shared](./hugo/shared.md) - Common Hugo content conventions applying to all Hugo sites in this repository. Covers inherited conventions (Mathematical Notation, Color Accessibility, Diagrams, Emoji, Timestamp, Tutorial standards), adapted conventions (Indentation, Linking, File Naming, Frontmatter, Date Format), and Hugo-specific basics (Archetypes, Shortcodes, Taxonomy, Asset Organization). Foundation for all Hugo content work

## 💻 Programming (1 convention)

Software design and coding standards cross-references.

- [Software Design Reference](./programming/software-design-reference.md) - Cross-reference to authoritative software design and coding standards documentation in docs/explanation/software/. Establishes separation between repository-wide documentation conventions (governance/conventions/) and language-specific technical guidance (docs/explanation/software/). Covers architecture patterns (C4, DDD, FSM), development practices (TDD, BDD), language-specific coding standards (Java, TypeScript, Go, Python, Elixir), and framework-specific standards (Spring Boot, Phoenix, React). Validated by repo-governance-checker agent

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

**Last Updated**: 2026-01-25
