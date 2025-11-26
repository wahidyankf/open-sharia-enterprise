# AI Agents

This directory contains specialized AI agents for repository maintenance and documentation tasks. Each agent has specific responsibilities and tools to ensure consistency, quality, and adherence to project conventions.

## 🤖 Available Agents

### 📝 `doc-writer.md`

Expert documentation writer specializing in Obsidian-optimized markdown and Diátaxis framework.

- **Primary Use:** Creating, editing, or organizing project documentation
- **Specialization:** Markdown optimization, Diátaxis framework, convention compliance, emoji usage
- **Tools:** Read, Write, Edit, Glob, Grep
- **When to Use:**
  - Creating new documentation files with proper emoji usage
  - Editing existing documentation for clarity or structure
  - Organizing documentation according to Diátaxis framework
  - Ensuring documentation follows file naming, linking, and emoji conventions

### ✅ `repo-rules-checker.md`

Validates consistency between agents, CLAUDE.md, conventions, and documentation.

- **Primary Use:** Checking for inconsistencies, contradictions, or verifying compliance
- **Specialization:** Cross-file validation, duplication detection, convention enforcement, emoji consistency
- **Tools:** Read, Glob, Grep
- **When to Use:**
  - After making changes to conventions or CLAUDE.md
  - Periodic audits of repository consistency
  - Validating that all files follow documented standards (including emoji usage)
  - Detecting contradictions or outdated references
  - Identifying duplicate content that could be consolidated

### 🔄 `repo-rules-updater.md`

Propagates rule and convention changes across CLAUDE.md, convention docs, agents, and indices.

- **Primary Use:** Adding/modifying rules, conventions, or standards affecting multiple files
- **Specialization:** Systematic propagation, cascade updates, consistency maintenance, emoji adoption
- **Tools:** Read, Edit, Glob, Grep
- **When to Use:**
  - Adding new conventions or standards (including emoji vocabulary)
  - Modifying existing rules that affect multiple files
  - Ensuring changes cascade to all relevant locations
  - Updating cross-references after structural changes
  - Maintaining consistency across agent definitions

### 📋 `plan-writer.md`

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

### 🚀 `plan-implementor.md`

Expert at systematically implementing project plans by following delivery checklists.

- **Primary Use:** Executing plans created by the plan-writer agent
- **Specialization:** Sequential implementation, validation, progress tracking, checklist management
- **Tools:** Read, Write, Edit, Glob, Grep, Bash
- **When to Use:**
  - Implementing a plan from plans/in-progress/
  - Following delivery checklists step-by-step
  - Running validation checklists and acceptance criteria
  - Updating delivery.md with implementation progress and notes
  - Completing all phases of a multi-phase plan
  - Ensuring all requirements are met before marking plan complete

### 📓 `journal-writer.md`

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
        └─> Validates each phase before proceeding

3. Complete Implementation
   └─> plan-implementor completes final validation
   └─> All acceptance criteria verified
   └─> Plan marked as complete
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
   └─> Use doc-writer for documentation tasks
        └─> Ensures proper formatting and convention compliance
```

## ✅ Best Practices

- **When starting a new project:** Use `plan-writer` to create structured plans in plans/backlog/
- **When implementing a plan:** Use `plan-implementor` with the plan path to execute systematically
- **After adding new conventions:** Use `repo-rules-updater` → `repo-rules-checker`
- **Before major releases:** Run `repo-rules-checker` for full audit
- **When creating documentation:** Use `doc-writer` for proper structure
- **When modifying CLAUDE.md:** Use `repo-rules-updater` to cascade changes
- **During plan implementation:** Let `plan-implementor` update delivery.md - it maintains detailed notes

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
