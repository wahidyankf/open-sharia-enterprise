---
title: "Conventions"
description: Documentation conventions and standards for open-sharia-enterprise
category: explanation
tags:
  - index
  - conventions
  - standards
created: 2025-11-22
updated: 2025-12-07
---

# Conventions

Documentation conventions and standards for the open-sharia-enterprise project. These documents define how documentation should be organized, named, and linked. Contains 19 conventions covering all aspects of documentation writing and formatting.

**Governance**: All conventions in this directory implement and embody the [Core Principles](../principles/README.md). Each convention should trace back to one or more foundational principles (Accessibility First, Simplicity Over Complexity, Progressive Disclosure, etc.).

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

## 📋 Contents

- [Color Accessibility Convention](./ex-co__color-accessibility.md) - MASTER REFERENCE for all color-related decisions in the repository. Defines verified accessible color palette (Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161), supports all color blindness types (protanopia, deuteranopia, tritanopia), meets WCAG AA standards, provides complete implementation guidance for Mermaid diagrams and AI agent categorization, and includes testing methodology with color blindness simulators and contrast checkers. All color usage must reference this convention as the authoritative source
- [Content Quality Principles](./ex-co__content-quality.md) - Universal markdown content quality standards applicable to ALL repository markdown contexts (docs/, Hugo sites, plans/, root files). Covers writing style and tone (active voice, professional, concise), heading hierarchy (single H1, proper nesting), accessibility (alt text, semantic HTML, color contrast, screen readers), and formatting (code blocks, text formatting, lists, blockquotes, tables, line length, paragraphs)
- [Convention Writing Convention](./ex-co__convention-writing.md) - **Meta-convention** defining how to write and organize convention documents. Covers document structure, scope boundaries, quality checklist, when to create new vs update existing, length guidelines, and integration with agents. Essential reading for creating or updating conventions
- [Diagram and Schema Convention](./ex-co__diagrams.md) - Standards for using Mermaid diagrams (primary) and ASCII art (optional) with color-blind friendly colors for accessibility
- [Diátaxis Framework](./ex-co__diataxis-framework.md) - Understanding the four-category documentation organization framework we use
- [Documentation File Naming Convention](./ex-co__file-naming-convention.md) - Systematic approach to naming files with hierarchical prefixes
- [Documentation Linking Convention](./ex-co__linking-convention.md) - Standards for linking between documentation files using GitHub-compatible markdown
- [Emoji Usage Convention](./ex-co__emoji-usage.md) - Semantic emoji usage to enhance document scannability and engagement with accessible colored emojis
- [Hugo Content Convention - Shared](./ex-co__hugo-content-shared.md) - Common Hugo content conventions applying to all Hugo sites in this repository. Covers inherited conventions (Mathematical Notation, Color Accessibility, Diagrams, Emoji, Timestamp, Tutorial standards), adapted conventions (Indentation, Linking, File Naming, Frontmatter, Date Format), and Hugo-specific basics (Archetypes, Shortcodes, Taxonomy, Asset Organization). Foundation for all Hugo content work
- [Hugo Content Convention - ayokoding](./ex-co__hugo-content-ayokoding.md) - Site-specific conventions for ayokoding-web (Hextra theme). Covers Hextra shortcodes, bilingual requirements, navigation patterns, weight field ordering, overview/ikhtisar file requirements, index file content separation, and optional Diátaxis structure. Use with shared convention
- [Hugo Content Convention - OSE Platform](./ex-co__hugo-content-ose-platform.md) - Site-specific conventions for ose-platform-web (PaperMod theme). Covers PaperMod features, English-only requirements, simple update patterns, cover image standards, and flat content structure. Use with shared convention
- [Indentation Convention](./ex-co__indentation.md) - Standard markdown indentation for all files using 2 spaces per indentation level. YAML frontmatter uses 2 spaces. Code blocks use language-specific conventions
- [Mathematical Notation Convention](./ex-co__mathematical-notation.md) - Standards for using LaTeX notation for mathematical equations and formulas in GitHub-compatible markdown. Defines inline (`$...$`) vs display (`$$...$$`) delimiters, forbidden contexts (code blocks, Mermaid), and Obsidian/GitHub dual compatibility
- [OSS Documentation Convention](./ex-co__oss-documentation.md) - Standards for repository documentation files (README, CONTRIBUTING, ADRs, security) following open source best practices
- [Plans Organization Convention](./ex-co__plans-organization.md) - Standards for organizing project planning documents in plans/ folder including structure, naming, and workflow
- [README Quality Convention](./ex-co__readme-quality.md) - Quality standards for README.md files ensuring engagement, accessibility, and scannability. Defines problem-solution hooks, jargon elimination (plain language over corporate speak), acronym context requirements, benefits-focused language, navigation structure (summary + links), and paragraph length limits (≤5 lines). Includes transformation examples and quality checklist. **Agents**: readme-maker, readme-checker
- [Timestamp Format Convention](./ex-co__timestamp-format.md) ⏰ - Standard timestamp format using UTC+7 (Indonesian WIB Time)
- [Tutorial Convention](./ex-co__tutorials.md) - Standards for creating learning-oriented tutorials with narrative flow, progressive scaffolding, and hands-on elements
- [Tutorial Naming Convention](./ex-co__tutorial-naming.md) - Standardized tutorial types and depth levels (Initial Setup, Quick Start, Beginner, Intermediate, Advanced, Cookbook) with "Full Set" concept for sequential learning

---

**Last Updated**: 2025-12-12
