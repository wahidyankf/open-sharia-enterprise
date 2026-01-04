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
updated: 2026-01-04
---

# Development

Development conventions and standards for the open-sharia-enterprise project. These documents define how to create and manage development practices, tools, and workflows.

**Governance**: All development practices in this directory serve the [Vision](../vision/ex-vi__open-sharia-enterprise.md) (Layer 0), implement the [Core Principles](../principles/README.md) (Layer 1), and implement/enforce [Documentation Conventions](../conventions/README.md) (Layer 2) as part of the six-layer architecture. Each practice MUST include TWO mandatory sections: "Principles Respected" and "Conventions Implemented/Respected". See [Repository Governance Architecture](../ex-ru__repository-governance-architecture.md) for complete governance model and [AI Agents Convention](./agents/ex-ru-de-ag__ai-agents.md) for structure requirements.

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

### Workflow Documentation

- [Implementation Workflow Convention](./workflow/ex-ru-de-wo__implementation.md) - Three-stage development workflow: make it work (functionality first), make it right (refactor for quality), make it fast (optimize only if needed). Implements Simplicity Over Complexity, YAGNI, and Progressive Disclosure principles
- [Trunk Based Development Convention](./workflow/ex-ru-de-wo__trunk-based-development.md) - Git workflow using Trunk Based Development for continuous integration
- [Commit Message Convention](./workflow/ex-ru-de-wo__commit-messages.md) - Understanding Conventional Commits, commit granularity, and why we use them
- [Reproducible Environments Convention](./workflow/ex-ru-de-wo__reproducible-environments.md) - Practices for creating consistent, reproducible development and build environments. Covers runtime version management (Volta), dependency locking, environment configuration, and containerization

### Quality Standards Documentation

- [Code Quality Convention](./quality/ex-ru-de-qu__code.md) - Automated code quality tools and git hooks (Prettier, Husky, lint-staged) for consistent formatting and commit validation
- [Content Preservation Convention](./quality/ex-ru-de-qu__content-preservation.md) - Principles and processes for preserving knowledge when condensing files and extracting duplications. Covers the MOVE NOT DELETE principle and offload decision tree
- [Repository Validation Methodology Convention](./quality/ex-ru-de-qu__repository-validation.md) - Standard validation methods and patterns for repository consistency checking. Covers frontmatter extraction, validation checks, and best practices
- [Criticality Levels Convention](./quality/ex-ru-de-qu__criticality-levels.md) - Universal criticality level system for categorizing validation findings by importance and urgency (CRITICAL/HIGH/MEDIUM/LOW)
- [Fixer Confidence Levels Convention](./quality/ex-ru-de-qu__fixer-confidence-levels.md) - Universal confidence level system for fixer agents to assess and apply validated fixes (HIGH/MEDIUM/FALSE_POSITIVE)
- [Claude Folder Tool Usage Convention](./quality/ex-ru-de-qu__claude-folder-tool-usage.md) - Standards for AI agents writing to .claude/ folders using Bash tools instead of Write/Edit tools for autonomous operation

### Pattern Documentation

- [Maker-Checker-Fixer Pattern Convention](./pattern/ex-ru-de-pa__maker-checker-fixer.md) - Three-stage quality workflow for content creation and validation. Covers agent roles, workflow stages with user review gates, and confidence level integration
- [Functional Programming Practices](./pattern/ex-ru-de-pa__functional-programming.md) - Guidelines for applying functional programming principles in TypeScript/JavaScript. Covers immutability patterns, pure functions, and function composition

### Agent Standards Documentation

- [AI Agents Convention](./agents/ex-ru-de-ag__ai-agents.md) - Standards for creating and managing AI agents in the `.claude/agents/` directory. Covers agent naming, file structure, frontmatter requirements, tool access patterns, model selection, and size limits

### Infrastructure Documentation

- [Temporary Files Convention](./infra/ex-ru-de-in__temporary-files.md) - Guidelines for AI agents creating temporary uncommitted files and folders
- [Acceptance Criteria Convention](./infra/ex-ru-de-in__acceptance-criteria.md) - Writing testable acceptance criteria using Gherkin format for clarity and automation. Covers Gherkin syntax and common patterns

### Hugo Development Documentation

- [Hugo Development Convention](./hugo/ex-ru-de-hu__development.md) - Standards for developing Hugo sites (layouts, themes, assets, configuration) for ayokoding-web and ose-platform-web. Covers theme development, asset pipeline, i18n/l10n, performance optimization, and SEO best practices

## 🔗 Related Documentation

- [Repository Governance Architecture](../ex-ru__repository-governance-architecture.md) - Complete six-layer architecture (Layer 3: Development)
- [Core Principles](../principles/README.md) - Layer 1: Foundational values that govern development practices
- [Conventions](../conventions/README.md) - Layer 2: Documentation conventions (parallel governance with development)
- [Workflows](../workflows/README.md) - Layer 5: Multi-step processes orchestrating agents

---

**Last Updated**: 2025-12-24
