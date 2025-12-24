---
title: Development
description: Development conventions and standards for open-sharia-enterprise
category: explanation
subcategory: development
tags:
  - index
  - development
  - conventions
  - ai-agents
created: 2025-11-23
updated: 2025-12-24
---

# Development

Development conventions and standards for the open-sharia-enterprise project. These documents define how to create and manage development practices, tools, and workflows.

**Governance**: All development practices in this directory serve the [Vision](../vision/ex-vi__open-sharia-enterprise.md), implement the [Core Principles](../principles/README.md), and implement/enforce [Documentation Conventions](../conventions/README.md). Each practice MUST include TWO mandatory sections: "Principles Respected" (traces to foundational values) and "Conventions Implemented/Respected" (traces to documentation standards). Both sections require working links and explanations of HOW the practice implements/respects them. This creates the traceability chain: Vision → Principles → Development Practices. See [AI Agents Convention](./ex-de__ai-agents.md) and [Maker-Checker-Fixer Pattern](./ex-de__maker-checker-fixer-pattern.md) for examples.

## 🎯 Scope

**This directory contains conventions for SOFTWARE DEVELOPMENT:**

**✅ Belongs Here:**

- Software development methodologies (BDD, testing, agile practices)
- Build processes, tooling, and automation workflows
- Hugo **theme/layout development** (HTML templates, CSS/JS, asset pipeline)
- Development infrastructure (temporary files, build artifacts, reports)
- Git workflows and commit message standards
- AI agent development and configuration
- Code quality, testing, and deployment practices
- Acceptance criteria and testable requirements

**❌ Does NOT Belong Here (use [Conventions](../conventions/README.md) instead):**

- How to write and format documentation
- Markdown writing standards and style guides
- Documentation organization (Diátaxis framework)
- File naming and linking in docs
- Hugo **content** writing (frontmatter, markdown, archetypes)
- Visual documentation elements (diagrams, colors in docs)
- Documentation quality and accessibility

## 🧪 The Layer Test for Development

**Question**: Does this document answer "**HOW do we develop software?**"

✅ **Belongs in development/** if it defines:

- HOW to develop software systems (code, themes, layouts, build processes)
- WHAT development workflows to follow (git, commits, testing)
- HOW to automate development tasks (git hooks, CI/CD, AI agents)
- WHAT development tools and standards to use

❌ **Does NOT belong** if it defines:

- WHY we value something (that's a principle)
- HOW to write documentation (that's a convention)
- HOW to solve a specific user problem (that's a how-to guide)

**Examples**:

- "Use Trunk Based Development for git workflow" → ✅ Development (software practice)
- "Commit messages must follow Conventional Commits" → ✅ Development (development workflow)
- "Hugo themes use Hugo Pipes for asset processing" → ✅ Development (software development)
- "Markdown files use 2-space indentation" → ❌ Convention (documentation rule)
- "Why we automate repetitive tasks" → ❌ Principle (foundational value)

## 📂 Document Types

Development practices in this directory fall into several categories:

### Workflow Documentation

**Purpose:** Define step-by-step processes for development activities
**Examples:** Trunk Based Development, Commit Messages
**Structure:** Context → Process → Examples → Exceptions

### Standards Documentation

**Purpose:** Establish quality gates and requirements
**Examples:** Code Quality, Acceptance Criteria
**Structure:** Purpose → Requirements → Checklist → Examples

### Tool-Specific Documentation

**Purpose:** Define technology-specific best practices
**Examples:** Hugo Development, AI Agents
**Structure:** Overview → Conventions → Patterns → Anti-patterns

### Infrastructure Documentation

**Purpose:** Document system design decisions
**Examples:** Temporary Files
**Structure:** Problem → Solution → Organization → Usage

## 📋 Contents

- [Acceptance Criteria Convention](./ex-de__acceptance-criteria.md) - Writing testable acceptance criteria using Gherkin format for clarity and automation. Covers Gherkin syntax (Scenario, Given, When, Then), best practices, common patterns (CRUD, auth, errors), and integration with BDD test frameworks
- [AI Agents Convention](./ex-de__ai-agents.md) - Standards for creating and managing AI agents in the `.claude/agents/` directory
- [Code Quality Convention](./ex-de__code-quality.md) - Automated code quality tools and git hooks (Prettier, Husky, lint-staged) for consistent formatting and commit validation
- [Commit Message Convention](./ex-de__commit-messages.md) - Understanding Conventional Commits, commit granularity, and why we use them
- [Content Preservation Convention](./ex-de__content-preservation.md) - Principles and processes for preserving knowledge when condensing files and extracting duplications. Covers the fundamental MOVE NOT DELETE principle, offload decision tree, four offload options (create new, merge existing, extract common, add to development), verification checklist, and when NOT to offload
- [Fixer Confidence Levels Convention](./ex-de__fixer-confidence-levels.md) - Universal confidence level system for fixer agents to assess and apply validated fixes. Covers three confidence levels (HIGH/MEDIUM/FALSE_POSITIVE), when to apply fixes automatically, re-validation requirements, domain-specific vs universal criteria, and false positive feedback loop for improving checker accuracy
- [Hugo Development Convention](./ex-de__hugo-development.md) - Standards for developing Hugo sites (layouts, themes, assets, configuration) for ayokoding-web and ose-platform-web. Covers theme development, layout organization, asset pipeline (Hugo Pipes, CSS/JS processing, image optimization), configuration management, i18n/l10n, performance optimization, SEO best practices, accessibility (WCAG compliance), shortcode development, and build/deployment processes
- [Implementation Workflow Convention](./ex-de__implementation-workflow.md) - Three-stage development workflow: make it work (functionality first), make it right (refactor for quality), make it fast (optimize only if needed). Implements Simplicity Over Complexity, YAGNI, and Progressive Disclosure principles. Covers each stage's goals, what to do/avoid, anti-patterns (premature optimization, skipping refactoring), best practices (profile before optimizing, document decisions), and when to apply vs exceptions
- [Maker-Checker-Fixer Pattern Convention](./ex-de__maker-checker-fixer-pattern.md) - Three-stage quality workflow for content creation and validation. Covers agent roles (Maker creates/updates, Checker validates/audits, Fixer applies validated fixes), workflow stages with user review gates, confidence level integration, report pairing (audit-fix), benefits (safety, transparency, quality improvement loop), and when to use each agent type
- [Repository Validation Methodology Convention](./ex-de__repository-validation.md) - Standard validation methods and patterns for repository consistency checking. Covers frontmatter extraction (AWK pattern), validation checks (comments, fields, links, naming), best practices, and common pitfalls to avoid false positives
- [Temporary Files Convention](./ex-de__temporary-files.md) - Guidelines for AI agents creating temporary uncommitted files and folders
- [Trunk Based Development Convention](./ex-de__trunk-based-development.md) - Git workflow using Trunk Based Development for continuous integration

## 🔗 Related Documentation

- [Documentation Conventions](../conventions/README.md) - File naming, linking, and Diátaxis framework standards

---

**Last Updated**: 2025-12-15
