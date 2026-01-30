---
title: "Plans Organization Convention"
description: Standards for organizing project planning documents in plans/ folder
category: explanation
subcategory: conventions
tags:
  - conventions
  - plans
  - project-planning
  - organization
created: 2025-12-05
updated: 2025-12-05
---

# Plans Organization Convention

<!--
  MAINTENANCE NOTE: Master reference for plans organization
  This convention is referenced by:
  1. plans/README.md (brief landing page with link to this convention)
  2. AGENTS.md (summary with link to this convention)
  3. .opencode/agent/plan__maker.md (reference to this convention)
  When updating, ensure all references remain accurate.
-->

This document defines the standards for organizing project planning documents in the `plans/` folder. Plans are temporary, ephemeral documents used for project planning and tracking, distinct from permanent documentation in `docs/`.

## Principles Implemented/Respected

This convention implements the following core principles:

- **[Simplicity Over Complexity](../../principles/general/simplicity-over-complexity.md)**: Flat structure with three clear states (backlog, in-progress, done). No complex nested hierarchies or status tracking systems.

- \*\*[Explicit Over Implicit](../../principles/software-engineering/[identifier]`) making chronological order explicit. File location indicates status - no hidden metadata or databases.

## Purpose

This convention establishes the organizational structure for project planning documents in the `plans/` directory. It defines how to organize ideas, backlog, in-progress work, and completed projects using date-based folder naming and standardized lifecycle stages.

## Scope

### What This Convention Covers

- **Plans directory structure** - ideas.md, backlog/, in-progress/, done/ organization
- **Folder naming pattern** - `YYYY-MM-DD__[project-identifier]/` format
- **File organization** - What files belong in each folder
- **Lifecycle stages** - How plans move from ideas → backlog → in-progress → done
- **Project identifiers** - How to name projects consistently

### What This Convention Does NOT Cover

- **Plan content format** - How to write plans (covered by plan\_\_checker agent)
- **Project management methodology** - This is file organization, not PM process
- **Task tracking** - Covered by plan\_\_executor agent
- **Deployment scheduling** - Covered in deployment conventions

## Overview

The `plans/` folder serves as the workspace for project planning activities:

- **Purpose**: Temporary project planning and tracking
- **Location**: Root-level `plans/` folder (not inside `docs/`)
- **Lifecycle**: Plans move between subfolders as work progresses
- **Format**: Structured markdown documents following specific naming and organization conventions

**Key Distinction**: Plans are temporary working documents that eventually move to `done/` and may be archived, while `docs/` contains permanent documentation that evolves over time.

## ️ Folder Structure

The `plans/` folder is organized into four main components:

```
plans/
├── ideas.md         # Quick 1-3 liner ideas not yet formalized into plans
├── backlog/         # Planned projects for future implementation
├── in-progress/     # Active plans currently being worked on
└── done/            # Completed and archived plans
```

### Subfolder Purposes

**backlog/** - Planning Queue

- Contains plans that are ready for implementation but not yet started
- Plans are fully structured with requirements, tech docs, and delivery sections
- Each subfolder has a `README.md` listing all plans in backlog

**in-progress/** - Active Work

- Contains plans currently being executed
- Plans being actively worked on by the team
- Limited to a small number of concurrent plans (prevents context switching)
- Each subfolder has a `README.md` listing all active plans

**done/** - Completed Work

- Contains completed and archived plans
- Plans are moved here when implementation is finished
- Serves as historical record of project evolution
- Each subfolder has a `README.md` listing all completed plans

## Ideas File

**Location**: `plans/ideas.md` (root level of plans/ folder)

**Purpose**: Capture quick ideas and todos that haven't been formalized into full plan documents yet.

### Characteristics

- **Lightweight**: Simple markdown file with bullet points or numbered lists
- **Quick Capture**: Each idea should be 1-3 lines maximum
- **No Structure**: No formal plan structure required
- **Brainstorming**: Ideas that need more thought before becoming formal plans

### Format

```markdown
# Ideas

Quick ideas and todos that haven't been formalized into plans yet.

- Add OAuth2 authentication system with Google and GitHub providers
- Implement real-time notification system using WebSockets
- Create admin dashboard for user management and analytics
- Optimize database queries for better performance
```

### Difference from backlog/

- **ideas.md**: 1-3 liner quick captures without detailed structure
- **backlog/**: Full plan folders with structured requirements, tech-docs, and delivery files

### Promoting an Idea to a Plan

When an idea is ready for formal planning:

1. Create a new plan folder in `backlog/` with `YYYY-MM-DD__[project-identifier]/` format
2. Create the standard plan files (README.md or multi-file structure)
3. Remove or check off the idea from `ideas.md`
4. The idea now has a structured plan with requirements, technical docs, and delivery timeline

## Plan Folder Naming

**CRITICAL**: Every plan folder MUST follow this naming pattern:

```
YYYY-MM-DD__[project-identifier]/
```

### Naming Rules

- **Date Format**: ISO 8601 format (`YYYY-MM-DD`)
- **Date Meaning**:
  - In `backlog/` and `in-progress/`: Plan creation date
  - In `done/`: Updated to completion date when moved
- **Separator**: Double underscore `__` separates date from identifier
- **Identifier**: Kebab-case (lowercase with hyphens)
- **No Spaces**: Use hyphens instead of spaces
- **No Special Characters**: Only alphanumeric and hyphens in identifier

### Examples

**Good**:

- `2025-11-24__init-monorepo/`
- `2025-12-01__auth-system/`
- `2026-01-15__mobile-app-redesign/`
- `2025-12-05__payment-integration/`

**Bad**:

- `2025-11-24_init-monorepo/` (single underscore)
- `init-monorepo/` (missing date)
- `2025-11-24__Init Monorepo/` (capital letters, spaces)
- `2025-11-24__init_monorepo/` (underscores in identifier)

## Plan Contents

Plans can use either **single-file** or **multi-file** structure depending on size and complexity.

### Structure Decision

**Single-File Structure** (≤ 1000 lines total):

- Use when combined content of requirements + tech-docs + delivery ≤ 1000 lines
- All content in a single `README.md` file
- Simpler, easier to read and navigate for small plans
- Recommended for most plans

**Multi-File Structure** (> 1000 lines total):

- Use when combined content exceeds 1000 lines
- Separate files: `README.md`, `requirements.md`, `tech-docs.md`, `delivery.md`
- Better organization for complex, large-scale plans
- Each file focuses on a specific aspect of the plan

**Decision Rule**: If you estimate the plan will be ≤ 1000 lines total, use single-file. Otherwise use multi-file.

### Single-File Structure

```
2025-12-01__feature-name/
└── README.md                # All-in-one plan document
```

**README.md sections**:

1. **Overview** - Project description, goals, and context
2. **Requirements** - Detailed requirements and objectives
3. **Technical Documentation** - Architecture, design, implementation approach
4. **Delivery** - Milestones, deliverables, success criteria

### Multi-File Structure

```
2025-12-01__feature-name/
├── README.md                # Plan overview and navigation
├── requirements.md          # Detailed requirements and objectives
├── tech-docs.md            # Technical documentation and architecture
└── delivery.md             # Timeline and milestones
```

**File purposes**:

- **README.md**: High-level overview, links to other files, quick reference
- **requirements.md**: User stories, acceptance criteria (Gherkin format), business requirements
- **tech-docs.md**: Architecture diagrams, API design, data models, technical decisions
- **delivery.md**: Milestones, deliverables, success metrics, validation checklist

**Acceptance Criteria**: All user stories in requirements.md must include testable acceptance criteria using Gherkin format. See [Acceptance Criteria Convention](../../development/infra/acceptance-criteria.md) for complete details.

### Important Note on File Naming

**Files inside plan folders do NOT use naming prefixes** (no `pl-re__`, `pl-td__`, etc.).

The folder structure provides sufficient context, so prefixes are unnecessary and would add noise. This differs from files in `docs/` which use prefixes for organization.

## Key Differences from Documentation

Plans differ from `docs/` in several important ways:

| Aspect           | Plans (`plans/`)                      | Documentation (`docs/`)              |
| ---------------- | ------------------------------------- | ------------------------------------ |
| **Location**     | Root-level `plans/` folder            | Root-level `docs/` folder            |
| **Purpose**      | Temporary project planning            | Permanent documentation              |
| **File Naming**  | No prefixes inside folders            | Prefixes encode directory path       |
| **Lifecycle**    | Move between in-progress/backlog/done | Evolve and update in place           |
| **Audience**     | Project team, stakeholders            | All users, contributors, maintainers |
| **Longevity**    | Temporary (archived in done/)         | Permanent (evolves over time)        |
| **Organization** | By project and status                 | By Diátaxis category                 |

## Working with Plans

### Creating Plans

1. **Start with an idea**: Capture quick idea in `ideas.md` (1-3 lines)
2. **Formalize when ready**: Create plan folder in `backlog/` when idea is mature
3. **Follow naming convention**: Use `YYYY-MM-DD__[project-identifier]/` format
4. **Choose structure**: Single-file (≤1000 lines) or multi-file (>1000 lines)
5. **Create content**: Write overview, requirements, tech docs, and delivery sections
6. **Update index**: Add plan to `backlog/README.md`

### Starting Work

1. **Move folder**: Move plan folder from `backlog/` to `in-progress/`
2. **Update index**: Update both `backlog/README.md` and `in-progress/README.md`
3. **Git commit**: Commit the move with appropriate message
4. **Begin execution**: Start implementing according to delivery checklist

### Completing Work

1. **Verify completion**: Ensure all deliverables and acceptance criteria met
2. **Update date**: Optionally update folder name date to completion date
3. **Move folder**: Move plan folder from `in-progress/` to `done/`
4. **Update index**: Update both `in-progress/README.md` and `done/README.md`
5. **Git commit**: Commit the move with completion message
6. **Archive**: Plan is now archived for historical reference

### Plan Index Files

Each subfolder (`backlog/`, `in-progress/`, `done/`) has a `README.md` that:

- Lists all plans in that category
- Provides brief description of each plan
- Links to each plan folder
- Updated whenever plans are added, moved, or removed

## Diagrams in Plans

Files in `plans/` folder should use **Mermaid diagrams** as the primary format (same as all markdown files in the repository).

**Diagram Standards**:

- **Primary Format**: Mermaid diagrams for all flowcharts, architecture diagrams, sequences
- **ASCII Art**: Optional, only for simple directory trees or rare edge cases
- **Orientation**: Prefer vertical (top-down or bottom-top) for mobile-friendly viewing
- **Colors**: Use color-blind friendly palette from [Color Accessibility Convention](../formatting/color-accessibility.md)

**Why Mermaid**:

- Renders properly in GitHub and most markdown viewers
- Version-controllable (text-based)
- Easy to update and maintain
- Supports multiple diagram types (flowchart, sequence, class, ER, etc.)

For complete diagram standards, see [Diagram and Schema Convention](../formatting/diagrams.md).

## Related Documentation

**Decision Guides**:

- [How to Organize Your Work](../../../docs/how-to/hoto__organize-work.md) - Decision guide for choosing between plans/ and docs/

**Related Conventions**:

- [Acceptance Criteria Convention](../../development/infra/acceptance-criteria.md) - Writing testable acceptance criteria using Gherkin format
- [Diátaxis Framework](./diataxis-framework.md) - Organization of `docs/` directory
- [File Naming Convention](./file-naming.md) - Naming files within `docs/` (not applicable to plans/)
- [Diagram and Schema Convention](../formatting/diagrams.md) - Standards for Mermaid diagrams

**Development Guides**:

- [AI Agents Convention](../../development/agents/ai-agents.md) - Standards for AI agents (including plan**maker and plan**executor)

## Best Practices

### Keep Plans Focused

- One plan per project or major feature
- Break large initiatives into multiple plans
- Each plan should have clear, achievable scope

### Update Plans as You Go

- Plans are living documents during execution
- Update technical docs when making design decisions
- Check off deliverables as completed
- Add notes about challenges or learnings

### Use Ideas File Liberally

- Capture ideas quickly without overthinking
- Don't worry about perfect wording
- Review ideas periodically and promote mature ones to plans
- Archive or delete ideas that are no longer relevant

### Maintain Indices

- Always update subfolder README.md when moving plans
- Keep descriptions current and accurate
- Remove completed plans from in-progress index promptly

### Archive Completed Plans

- Don't delete completed plans - move them to `done/`
- Completed plans serve as historical record
- Review past plans to learn from successes and challenges
- Use completed plans as templates for similar future work

## Examples

### Example: Small Plan (Single-File)

```
2025-12-05__add-user-search/
└── README.md                # ~400 lines total
```

**README.md structure**:

```markdown
# Add User Search Feature

## Overview

Brief description and goals...

## Requirements

User stories and acceptance criteria...

## Technical Documentation

API design, database changes...

## Delivery

Milestones and deliverables...
```

### Example: Large Plan (Multi-File)

```
2025-12-05__migrate-to-microservices/
├── README.md                # ~100 lines (overview + navigation)
├── requirements.md          # ~300 lines (detailed requirements)
├── tech-docs.md            # ~800 lines (architecture + API specs)
└── delivery.md             # ~200 lines (phased rollout plan)
```

### Example: Ideas File

```markdown
# Ideas

Quick ideas and todos that haven't been formalized into plans yet.

## Authentication & Security

- Add OAuth2 support for Google and GitHub
- Implement API rate limiting
- Add 2FA support for admin accounts

## Performance

- Optimize database queries with proper indexing
- Add Redis caching layer
- Implement CDN for static assets

## User Experience

- Add dark mode toggle
- Implement keyboard shortcuts
- Add progressive web app support
```

---

**Last Updated**: 2025-12-05
