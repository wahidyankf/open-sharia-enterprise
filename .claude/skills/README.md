# Skills Directory

This directory contains **Claude Code Skills** - reusable knowledge packages that enable progressive disclosure of repository conventions and development practices to AI agents.

## What Are Skills?

Skills are model-invoked markdown-based knowledge packages that:

- **Auto-load based on description matching** - Claude decides when to use them
- **Enable progressive disclosure** - Name/description at startup, full content on-demand
- **Reduce duplication** - Shared knowledge packaged once, referenced by many
- **Support composition** - Multiple Skills work together seamlessly
- **Portable across platforms** - Open standard (agentskills.io) works beyond Claude

## Skills as Delivery Infrastructure

**IMPORTANT**: Skills are **delivery infrastructure**, not a governance layer.

```
Knowledge Flow:
  Startup: CLAUDE.md ──loaded──> Orchestrator (main conversation)
  Runtime: Orchestrator ──spawns──> Agents (isolated contexts)
           Skills ──delivers via skills: field──> Agents
           Direct refs ──explicit links──> Agents
```

Skills don't **govern** agents (like Conventions do). Skills **deliver** knowledge to agents. They're infrastructure, not architecture.

## Domain Prefixes

All Skills use domain prefixes matching agent naming convention: `[domain]__[skill-name]`

This creates consistency across the agent-skill ecosystem and improves discoverability:

- **docs\_\_** - Documentation creation and validation
- **readme\_\_** - README quality standards
- **plan\_\_** - Project planning standards
- **agent\_\_** - AI agent development
- **apps**ayokoding-web**** - Hugo Hextra content
- **apps**ose-platform-web**** - Hugo PaperMod content
- **wow\_\_** - Cross-cutting workflows (Ways of Working)

## Skill Structure

### Single-File Skill

```
skill-name/
└── SKILL.md
```

### Multi-File Skill

```
skill-name/
├── SKILL.md       # Main skill definition
├── examples.md    # Detailed examples
├── reference.md   # Quick reference
└── README.md      # Additional context
```

## SKILL.md Format

Every Skill must have a `SKILL.md` file with YAML frontmatter:

```yaml
---
name: domain__skill-name
description: Clear description for auto-loading (CRITICAL for model invocation)
allowed-tools: [Read, Grep] # Optional - tool access restrictions
model: sonnet # Optional - specific model requirement
---
# Skill Content

Markdown instructions, examples, best practices...
```

**CRITICAL**: The `name` field must exactly match the directory name.

## Available Skills (18)

This repository provides 18 Claude Code Skills organized by domain. Each Skill is a model-invoked knowledge package that auto-loads when agents need specific domain expertise.

### docs\_\_ Domain (6 Skills)

**docs**docs**applying-content-quality** - Universal markdown content quality standards for active voice, heading hierarchy, accessibility compliance (alt text, WCAG AA contrast, screen reader support), and professional formatting. Essential for all markdown content creation across docs/, Hugo sites, plans/, and repository files. Auto-loads when creating or editing markdown content.

**docs**docs**creating-accessible-diagrams** - WCAG-compliant Mermaid diagrams using verified accessible color palette. Use when creating diagrams, flowcharts, or any color-dependent visualizations requiring accessibility compliance for color blindness.

**docs**docs**creating-by-example-tutorials** - Comprehensive guide for creating by-example tutorials - code-first learning path with 75-90 heavily annotated examples achieving 95% language coverage. Covers five-part example structure, annotation density standards (1-2.25 comments per code line PER EXAMPLE), self-containment rules, and multiple code blocks for comparisons. Essential for creating by-example tutorials for programming languages on educational platforms.

**docs**docs**applying-diataxis-framework** - Diátaxis documentation framework for organizing content into four categories - tutorials (learning-oriented), how-to guides (problem-solving), reference (technical specifications), and explanation (conceptual understanding). Essential for creating and organizing documentation in docs/ directory.

**docs**docs**validating-factual-accuracy** - Universal methodology for verifying factual correctness in documentation using WebSearch and WebFetch tools. Covers command syntax verification, version checking, code example validation, API correctness, confidence classification system ([Verified], [Error], [Outdated], [Unverified]), source prioritization, and update frequency rules. Essential for maintaining factual accuracy in technical documentation and educational content.

**docs**docs**validating-links** - Link validation standards for markdown links including format requirements, path validation, broken link detection, and external link verification.

### readme\_\_ Domain (1 Skill)

**readme**readme**writing-readme-files** - README quality standards for engaging, accessible, scannable content including problem-solution hooks, plain language (no unexplained jargon), acronym context, paragraph limits (≤5 lines), benefits-focused language, and progressive disclosure. Essential for creating effective README files that welcome and guide users.

### plan\_\_ Domain (2 Skills)

**plan**plan**creating-project-plans** - Comprehensive project planning standards for plans/ directory including folder structure (ideas.md, backlog/, in-progress/, done/), naming convention (YYYY-MM-DD\_\_identifier/), file organization (README.md for small plans, multi-file for large), and Gherkin acceptance criteria. Essential for creating structured, executable project plans.

**plan**plan**writing-gherkin-criteria** - Guide for writing Gherkin acceptance criteria using Given-When-Then syntax for testable requirements. Covers scenario structure, background blocks, scenario outlines with examples tables, common patterns for authentication/CRUD/validation/error handling, and best practices for clear testable specifications. Essential for writing user stories and plan acceptance criteria.

### agent\_\_ Domain (1 Skill)

**agent**agent**developing-agents** - AI agent development standards including frontmatter structure, naming conventions, tool access patterns, model selection, and Bash-only file operations for .claude/ folders.

### apps**ayokoding-web** Domain (1 Skill)

**apps**ayokoding-web**developing-content** - Comprehensive guide for creating content on ayokoding-web Hugo site using Hextra theme. Covers bilingual content strategy (default English), level-based weight ordering system, by-example tutorial annotation standards (1-2.25 comments per code line), absolute path linking requirements, and ayokoding-web specific frontmatter patterns. Essential for content creation tasks on ayokoding-web.

### apps**ose-platform-web** Domain (1 Skill)

**apps**ose-platform-web**developing-content** - Guide for creating content on ose-platform-web Hugo site using PaperMod theme. Covers English-only landing page structure, update posts with date-prefixed filenames, PaperMod frontmatter (cover images, table of contents, author field), simple flat organization, and ose-platform-web specific conventions. Essential for ose-platform-web content creation tasks.

### wow\_\_ Domain (7 Cross-Cutting Skills)

**wow**wow**applying-maker-checker-fixer** - Three-stage content quality workflow pattern (Maker creates, Checker validates, Fixer remediates). Use when working with content quality workflows, validation processes, audit reports, or implementing maker/checker/fixer agent roles.

**wow**wow**assessing-criticality-confidence** - Universal classification system for checker and fixer agents using orthogonal criticality (CRITICAL/HIGH/MEDIUM/LOW importance) and confidence (HIGH/MEDIUM/FALSE_POSITIVE certainty) dimensions. Covers priority matrix (P0-P4), execution order, dual-label pattern for verification status, standardized report format, and domain-specific examples. Essential for implementing checker/fixer agents and processing audit reports.

**wow**wow**defining-workflows** - Workflow pattern standards for creating multi-agent orchestrations including YAML frontmatter (name, description, tags, status, agents, parameters), execution phases (sequential/parallel/conditional), agent coordination patterns, and Gherkin success criteria. Essential for defining reusable, validated workflow processes.

**wow**wow**generating-validation-reports** - Guidelines for generating validation/audit reports with UUID chains, progressive writing, and UTC+7 timestamps.

**wow**wow**multi-file-template** - Template for creating multi-file Skills with organized structure (SKILL.md, examples.md, reference.md, README.md).

**wow**wow**practicing-trunk-based-development** - Trunk Based Development workflow - all development on main branch with small frequent commits, minimal branching, and continuous integration. Covers when branches are justified (exceptional cases only), commit patterns, feature flag usage for incomplete work, environment branch rules (deployment only), and AI agent default behavior (assume main). Essential for understanding repository git workflow and preventing unnecessary branch proliferation.

**wow**wow**understanding-repository-architecture** - Six-layer governance hierarchy (Vision → Principles → Conventions → Development → Agents → Workflows). Use when understanding repository structure, tracing rules to foundational values, explaining architectural decisions, or navigating layer relationships.

---

**Skills Creation**: See [How to Create a Skill](../../docs/how-to/hoto__create-new-skill.md) for step-by-step guide to creating new Skills.

**Multi-File Template**: Use `.claude/skills/wow__wow__multi-file-template/` as starting point for complex Skills requiring multiple files (SKILL.md, examples.md, reference.md, README.md).
