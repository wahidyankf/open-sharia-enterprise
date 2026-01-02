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

## Skill Structure

### Single-File Skill

```
Knowledge Flow:
  Startup: CLAUDE.md ──loaded──> Orchestrator (main conversation)
  Runtime: Orchestrator ──spawns──> Agents (isolated contexts)
           Skills ──delivers via skills: field──> Agents
           Direct refs ──explicit links──> Agents
```

### Multi-File Skill

```
Knowledge Flow:
  Startup: CLAUDE.md ──loaded──> Orchestrator (main conversation)
  Runtime: Orchestrator ──spawns──> Agents (isolated contexts)
           Skills ──delivers via skills: field──> Agents
           Direct refs ──explicit links──> Agents
```

## SKILL.md Format

Every Skill must have a `SKILL.md` file with YAML frontmatter:

```yaml
---
name: skill-name
description: Clear description for auto-loading (CRITICAL for model invocation)
allowed-tools: [Read, Grep] # Optional - tool access restrictions
model: sonnet # Optional - specific model requirement
---
# Skill Content

Markdown instructions, examples, best practices...
```

Knowledge Flow:
Startup: CLAUDE.md ──loaded──> Orchestrator (main conversation)
Runtime: Orchestrator ──spawns──> Agents (isolated contexts)
Skills ──delivers via skills: field──> Agents
Direct refs ──explicit links──> Agents

````

**For agents not using Skills:**

```yaml
skills: []

## Available Skills (17)

This repository provides 17 Claude Code Skills organized into five categories. Each Skill is a model-invoked knowledge package that auto-loads when agents need specific domain expertise.

### Content Creation (5 Skills)

**applying-content-quality** - Universal markdown content quality standards for active voice, heading hierarchy, accessibility compliance (alt text, WCAG AA contrast, screen reader support), and professional formatting. Essential for all markdown content creation across docs/, Hugo sites, plans/, and repository files.

**creating-by-example-tutorials** - Comprehensive guide for creating by-example tutorials - code-first learning path with 75-90 heavily annotated examples achieving 95% language coverage. Covers five-part example structure, annotation density standards (1-2.25 comments per code line PER EXAMPLE), self-containment rules, and multiple code blocks for comparisons.

**developing-ayokoding-content** - Hugo Hextra theme patterns for ayokoding-web including bilingual content strategy (Indonesian/English), level-based weight system, 2-layer navigation depth, absolute path linking, and code annotation standards for programming tutorials.

**developing-ose-content** - Hugo PaperMod theme patterns for ose-platform-web including English-only landing page structure, date-based content organization, and deployment-specific conventions.

**writing-readme-files** - Engaging, accessible README creation standards including problem-solution hooks, plain language requirements, acronym context, paragraph limits (≤5 lines), benefits-focused language, and scannable structure for quick comprehension.

### Quality Assurance (4 Skills)

**applying-maker-checker-fixer** - Three-stage workflow pattern for content quality: Maker creates/updates, Checker validates and generates audit report, User reviews findings, Fixer applies validated fixes with confidence levels. Implements automated quality gates with human oversight.

**assessing-criticality-confidence** - Dual-dimension assessment system combining criticality levels (CRITICAL/HIGH/MEDIUM/LOW for importance/urgency) with confidence levels (HIGH/MEDIUM/FALSE_POSITIVE for fix certainty). Enables priority-based fix execution (P0-P4) and systematic false positive detection.

**validating-factual-accuracy** - Universal methodology for verifying factual correctness in documentation using WebSearch and WebFetch tools. Covers command syntax verification, version checking, code example validation, API correctness, confidence classification system ([Verified], [Error], [Outdated], [Unverified]), source prioritization, and update frequency rules.

**validating-links** - Comprehensive link validation methodology for both internal (relative paths, .md extensions) and external links (HTTP status checking, caching with 6-month expiry). Includes broken link detection, link format verification, and metadata cache management.

### Standards Application (3 Skills)

**applying-diataxis-framework** - Diátaxis documentation framework organizing content into four types: Tutorials (learning-oriented), How-To (problem-solving), Reference (technical information), Explanation (conceptual understanding). Ensures documentation serves all user needs systematically.

**creating-accessible-diagrams** - Color-blind friendly Mermaid diagram creation using verified accessible palette (Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161). Covers WCAG AA compliance, special character escaping, and accessibility best practices for technical diagrams.

**writing-gherkin-criteria** - Acceptance criteria authoring using Gherkin syntax (Given-When-Then) for behavior-driven development. Covers scenario structure, step patterns, test data management, and integration with project planning documents.

### Process Execution (3 Skills)

**creating-project-plans** - Project planning structure and organization following the plans/ directory convention: ideas.md for quick captures, backlog/ for future work, in-progress/ for active projects, done/ for archives. Covers folder naming (YYYY-MM-DD__project-identifier), required documents (README.md, requirements.md, tech-docs.md, delivery.md), and plan lifecycle management.

**defining-workflows** - Multi-step workflow definition using standardized YAML frontmatter and markdown structure. Covers workflow types (sequential, parallel, conditional), input/output specifications, termination criteria, and integration with AI agents for automated orchestration.

**practicing-trunk-based-development** - Trunk-based development workflow patterns: single main branch, small frequent commits, feature toggles over feature branches, environment branches for deployment only. Implements continuous integration with minimal merge conflicts.

### Technical Knowledge (2 Skills)

**developing-agents** - AI agent development standards including frontmatter structure (name, description, tools, model, color, skills), naming conventions (kebab-case, scope prefixes), tool access patterns, model selection criteria, and Bash-only file operations for .claude/ folders.

**understanding-repository-architecture** - Six-layer governance hierarchy: Vision (WHY we exist) → Principles (WHY we value) → Conventions (WHAT docs rules) → Development (HOW we develop) → AI Agents (WHO enforces) → Workflows (WHEN orchestration). Covers traceability requirements, delivery infrastructure (CLAUDE.md, Skills, Direct References), and change propagation patterns.

---

**Skills Creation**: See [How to Create a Skill](../../docs/how-to/hoto__create-new-skill.md) for step-by-step guide to creating new Skills.

**Multi-File Template**: Use `.claude/skills/multi-file-template/` as starting point for complex Skills requiring multiple files (SKILL.md, examples.md, reference.md, README.md).
````
