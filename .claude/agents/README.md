# AI Agents

This directory contains specialized AI agents for repository maintenance and documentation tasks. Each agent has specific responsibilities and tools to ensure consistency, quality, and adherence to project conventions.

## Available Agents

### 🟦 `agent-creator.md`

Expert at creating new AI agents following all repository conventions.

- **Primary Use:** Adding a new agent to .claude/agents/ directory
- **Specialization:** Agent scaffolding, automatic README updates, convention compliance, validation integration
- **Tools:** Read, Write, Edit, Glob, Grep
- **When to Use:**
  - Creating a new AI agent with proper structure and frontmatter
  - Auto-generating agent files following all naming and format conventions
  - Automatically assigning color based on agent role
  - Automatically updating agents README with new agent listing
  - Running validation via repo-rules-checker

### 🟦 `docs-writer.md`

Expert documentation writer specializing in Obsidian-optimized markdown and Diátaxis framework.

- **Primary Use:** Creating, editing, or organizing project documentation
- **Specialization:** Markdown optimization, Diátaxis framework, convention compliance, emoji usage
- **Tools:** Read, Write, Edit, Glob, Grep
- **When to Use:**
  - Creating new documentation files with proper emoji usage
  - Editing existing documentation for clarity or structure
  - Organizing documentation according to Diátaxis framework
  - Ensuring documentation follows file naming, linking, and emoji conventions

### 🟩 `docs-link-checker.md`

Expert at validating both external and internal links in documentation files to ensure they are not broken.

- **Primary Use:** Checking for dead links, verifying URL accessibility, validating internal references, or auditing documentation link health
- **Specialization:** External URL validation, internal link verification, web accessibility testing, broken link detection and repair
- **Tools:** Read, Glob, Grep, WebFetch, WebSearch
- **When to Use:**
  - Auditing all external and internal links in documentation
  - Verifying external URLs are accessible (not 404, 403, or broken)
  - Checking internal markdown links point to existing files
  - Finding and replacing broken links with working alternatives
  - Periodic link health checks (monthly or before releases)
  - After major documentation updates to ensure link integrity
  - After file renames or directory restructuring

### 🟨 `docs-renamer.md`

Expert at renaming/moving files and directories in docs/ directory while maintaining conventions.

- **Primary Use:** Reorganizing documentation structure, renaming directories, or moving files between locations
- **Specialization:** File/directory renaming, prefix recalculation, link updates, index maintenance, git operations
- **Tools:** Read, Edit, Glob, Grep, Bash
- **When to Use:**
  - Renaming directories in docs/ (updates all file prefixes and links)
  - Moving files between directories (recalculates prefixes)
  - Reorganizing documentation structure with multiple renames/moves
  - Fixing incorrect file prefixes that don't match directory location
  - After rename: automatically updates all internal links and indices
  - Uses git mv to preserve file history

### 🟩 `repo-rules-checker.md`

Expert at validating consistency between agents, CLAUDE.md, conventions, and documentation.

- **Primary Use:** Checking for inconsistencies, contradictions, or verifying compliance
- **Specialization:** Cross-file validation, duplication detection, convention enforcement, emoji consistency
- **Tools:** Read, Glob, Grep
- **When to Use:**
  - After making changes to conventions or CLAUDE.md
  - Periodic audits of repository consistency
  - Validating that all files follow documented standards (including emoji usage)
  - Detecting contradictions or outdated references
  - Identifying duplicate content that could be consolidated

### 🟨 `repo-rules-updater.md`

Expert at propagating rule and convention changes across CLAUDE.md, convention docs, agents, and indices.

- **Primary Use:** Adding/modifying rules, conventions, or standards affecting multiple files
- **Specialization:** Systematic propagation, cascade updates, consistency maintenance, emoji adoption
- **Tools:** Read, Edit, Glob, Grep
- **When to Use:**
  - Adding new conventions or standards (including emoji vocabulary)
  - Modifying existing rules that affect multiple files
  - Ensuring changes cascade to all relevant locations
  - Updating cross-references after structural changes
  - Maintaining consistency across agent definitions

### 🟦 `plan-writer.md`

Expert at creating structured project planning documents in the plans/ folder.

- **Primary Use:** Starting new projects, defining requirements, or organizing project deliverables
- **Specialization:** Project planning, requirements documentation, technical architecture, delivery planning, ASCII art diagrams
- **Tools:** Read, Write, Edit, Glob, Grep
- **When to Use:**
  - Creating comprehensive planning documents for new projects
  - Defining project scope, requirements, and objectives
  - Documenting technical approach and architecture decisions
  - Creating project roadmaps with milestones and timelines
  - Organizing project deliverables into structured plans

### 🟪 `plan-implementor.md`

Expert at systematically implementing project plans by following delivery checklists.

- **Primary Use:** Executing plans created by the plan-writer agent
- **Specialization:** Sequential implementation, per-phase validation, progress tracking, checklist management
- **Tools:** Read, Write, Edit, Glob, Grep, Bash
- **When to Use:**
  - Implementing a plan from plans/in-progress/
  - Following delivery checklists step-by-step
  - Running per-phase validation and acceptance criteria (self-validation)
  - Updating delivery.md with implementation progress and notes
  - Completing all phases of a multi-phase plan
  - Stopping at final validation handoff (does NOT perform final validation)

### 🟪 `plan-implementation-checker.md`

Expert at validating plan implementations against requirements, performing comprehensive quality checks, and providing detailed validation reports.

- **Primary Use:** Independent final validation of completed plan implementations
- **Specialization:** Requirements verification, code quality assessment, integration testing, comprehensive validation reporting
- **Tools:** Read, Glob, Grep, Bash
- **When to Use:**
  - After plan-implementor completes all implementation tasks
  - Validating implementation meets all requirements from requirements.md
  - Verifying technical documentation alignment (tech-docs.md)
  - Running comprehensive code quality checks (tests, lints, builds)
  - Performing end-to-end integration testing
  - Providing independent quality gate with fresh eyes
  - Generating detailed validation reports with specific findings
  - Iterating with plan-implementor to fix issues until validation passes

### 🟦 `journal-writer.md`

Expert journal writer specializing in Logseq-style outliner format for daily research notes and monthly project summaries.

- **Primary Use:** Capturing research insights or creating monthly progress reports
- **Specialization:** Bullet-based quick capture, knowledge graph building with markdown links, research organization, emoji usage in journals
- **Tools:** Read, Write, Edit, Glob, Grep, Bash
- **When to Use:**
  - Creating daily research notes using Logseq-style outliner format (bullet-based)
  - Generating monthly project summaries (recommended second Sunday of month)
  - Building knowledge graphs through markdown linking
  - Suggesting link opportunities across journal entries
  - Reorganizing and merging concepts for better knowledge retrieval
  - Tracking research themes and project progress over time
- **Format:** No headings - starts directly with bullets, uses emojis semantically (see [Journals Format Convention](../docs/explanation/conventions/ex-co__journals-format.md))

## 🔄 Agent Workflow

The agents work together in complementary workflows:

### 📋 Project Planning and Implementation Workflow

```
1. Plan Project
   └─> Use plan-writer to create structured plan in plans/backlog/
        └─> Creates requirements.md, tech-docs.md, delivery.md

2. Start Implementation
   └─> Move plan from backlog/ to in-progress/
   └─> Use plan-implementor with plan path
        └─> Executes delivery checklist step-by-step
        └─> Updates delivery.md with progress and notes
        └─> Performs per-phase validation (self-validation)
        └─> Marks status as "Ready for Final Validation"

3. Final Validation (Independent Quality Gate)
   └─> Use plan-implementation-checker with plan path
        └─> Validates all requirements are met
        └─> Runs comprehensive quality checks
        └─> Performs integration testing
        └─> Generates detailed validation report
        └─> If issues found: Returns to plan-implementor for fixes
        └─> If validation passes: Marks plan as complete

4. Complete and Archive
   └─> Move plan from in-progress/ to done/
   └─> Plan ready for review or deployment
```

### 🔧 Repository Maintenance Workflow

```
1. Make Changes
   └─> Use repo-rules-updater to propagate across files
        └─> Ensures consistency in CLAUDE.md, conventions, agents, indices

2. Validate Changes
   └─> Use repo-rules-checker to verify consistency
        └─> Detects inconsistencies, contradictions, duplications

3. Fix Issues (if any)
   └─> Use repo-rules-updater to fix detected issues
        └─> Return to step 2 for re-validation

4. Write/Update Documentation
   └─> Use docs-writer for documentation tasks
        └─> Ensures proper formatting and convention compliance

5. Rename/Move Files (if needed)
   └─> Use docs-renamer to reorganize documentation
        └─> Renames files/directories with git mv
        └─> Recalculates file prefixes based on new location
        └─> Updates all internal links automatically
        └─> Updates index files (README.md)

6. Verify All Links
   └─> Use docs-link-checker to audit link health
        └─> Validates all external URLs are accessible
        └─> Validates all internal markdown links exist
        └─> Fixes broken links with working alternatives
```

## ✅ Best Practices

- **When starting a new project:** Use `plan-writer` to create structured plans in plans/backlog/
- **When implementing a plan:** Use `plan-implementor` with the plan path to execute systematically
- **After plan-implementor completes:** Use `plan-implementation-checker` for independent final validation
- **Quality assurance workflow:** plan-implementor → plan-implementation-checker → (fix issues if needed) → repeat until validation passes
- **After adding new conventions:** Use `repo-rules-updater` → `repo-rules-checker`
- **Before major releases:** Run `repo-rules-checker` for full audit and `docs-link-checker` to verify all links
- **When creating documentation:** Use `docs-writer` for proper structure
- **When modifying CLAUDE.md:** Use `repo-rules-updater` to cascade changes
- **During plan implementation:** Let `plan-implementor` update delivery.md - it maintains detailed notes
- **When renaming/moving files in docs/:** Use `docs-renamer` to handle prefixes, links, and indices automatically
- **After using docs-renamer:** Always run `docs-link-checker` to verify all links are valid
- **Monthly or before releases:** Run `docs-link-checker` to ensure all links are valid
- **After major documentation updates:** Use `docs-link-checker` to verify link integrity

## 📚 Resources

- [AI Agents Convention](../docs/explanation/development/ex-de__ai-agents.md) - Complete agent specification and standards
- [CLAUDE.md](../CLAUDE.md) - Project guidance for all agents
- [Documentation Conventions](../docs/explanation/conventions/README.md) - File naming, linking, and Diátaxis framework
- [Plans Organization](../plans/README.md) - Planning document structure and conventions (use ASCII art for diagrams)

## 🆕 Adding New Agents

When creating new agents:

1. Follow the [AI Agents Convention](../docs/explanation/development/ex-de__ai-agents.md)
2. Add the agent to this README index
3. Use `repo-rules-updater` to propagate references to CLAUDE.md and other files
4. Use `repo-rules-checker` to validate the new agent follows all conventions
5. Update CLAUDE.md if the agent should be mentioned in project guidance

---

**Note:** This README follows the naming exception for README.md files documented in the [File Naming Convention](../docs/explanation/conventions/ex-co__file-naming-convention.md).
