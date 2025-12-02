# AI Agents

This directory contains specialized AI agents for repository maintenance and documentation tasks. Each agent has specific responsibilities and tools to ensure consistency, quality, and adherence to project conventions.

## Available Agents

### 🟦 `agent-maker.md`

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

### 🟩 `docs-checker.md`

Expert at validating factual correctness and content consistency of documentation using web verification. Checks technical accuracy, detects contradictions, validates examples and commands, and identifies outdated information.

- **Primary Use:** Verifying technical claims, checking command syntax, detecting contradictions, or auditing documentation accuracy
- **Specialization:** Factual accuracy verification, command syntax validation, code example checking, contradiction detection, freshness assessment, web-based verification
- **Tools:** Read, Glob, Grep, WebFetch, WebSearch
- **When to Use:**
  - Validating technical documentation before release
  - Checking documentation after dependency updates
  - Reviewing community contributions for accuracy
  - Auditing documentation for outdated information
  - Verifying technical claims in tutorials or guides
  - Ensuring code examples use current APIs
  - Detecting contradictions across documentation
  - Checking command syntax and flags are correct

### 🟦 `docs-maker.md`

Expert documentation writer specializing in Obsidian-optimized markdown and Diátaxis framework.

- **Primary Use:** Creating, editing, or organizing project documentation (how-to guides, reference, explanations)
- **Specialization:** Markdown optimization, Diátaxis framework, convention compliance, emoji usage
- **Tools:** Read, Write, Edit, Glob, Grep
- **When to Use:**
  - Creating how-to guides, reference documentation, or explanations
  - Editing existing documentation for clarity or structure
  - Organizing documentation according to Diátaxis framework
  - Ensuring documentation follows file naming, linking, and emoji conventions
- **Works with:** `docs-checker` for accuracy validation, `docs-link-checker` for link validation
- **Note:** For tutorials, use `docs-tutorial-maker` instead

### 🟦 `docs-tutorial-maker.md`

Expert tutorial writer specializing in learning-oriented content with narrative flow, progressive scaffolding, visual aids, and hands-on elements.

- **Primary Use:** Creating engaging, learning-oriented tutorials with narrative storytelling
- **Specialization:** Learning-oriented content, narrative writing, progressive scaffolding, diagram creation (architecture/sequence/flowcharts), hands-on examples, pedagogical structure
- **Tools:** Read, Write, Edit, Glob, Grep, WebFetch, WebSearch
- **When to Use:**
  - Creating new tutorials in docs/tutorials/
  - Building complete learning journeys (concept → implementation → practice)
  - Writing narrative-driven content (not list-heavy reference material)
  - Adding comprehensive diagrams (architecture, sequence, flowcharts)
  - Creating step-by-step hands-on learning experiences
  - Teaching complex technical concepts progressively
  - Following Diátaxis tutorial principles (learning-oriented, not task-oriented)
- **Works with:** `docs-tutorial-checker` for quality validation

### 🟩 `docs-link-checker.md`

Validates both external and internal links in documentation files to ensure they are not broken. Maintains a cache of verified external links in `docs/metadata/external-links-status.yaml` (the ONLY cache file) with automatic pruning and mandatory lastFullScan updates on every run. **HARD REQUIREMENT**: Cache file usage is mandatory regardless of how the agent is invoked (spawned by other agents, automated processes, or direct invocation). Outputs results in conversation only (no separate report files).

- **Primary Use:** Checking for dead links, verifying URL accessibility, validating internal references, or auditing documentation link health
- **Specialization:** External URL validation with caching, internal link verification, automatic cache pruning, mandatory lastFullScan updates, web accessibility testing, broken link detection and repair
- **Tools:** Read, Glob, Grep, WebFetch, WebSearch, Write, Edit
- **Cache File:** `docs/metadata/external-links-status.yaml` (REQUIRED - the ONLY cache file, updated on EVERY run, regardless of invocation context)
- **Output:** Conversation response only (no separate report files created)
- **When to Use:**
  - Auditing all external and internal links in documentation
  - Verifying external URLs are accessible (not 404, 403, or broken)
  - Checking internal markdown links point to existing files
  - Finding and replacing broken links with working alternatives
  - Periodic link health checks (monthly or before releases)
  - After major documentation updates to ensure link integrity
  - After file renames or directory restructuring
  - Automatic cache maintenance (prunes orphaned links, updates locations, updates lastFullScan)
- **IMPORTANT:** Cache requirement applies universally to ALL invocations - whether spawned by other agents, processes, or direct user invocation

### 🟩 `docs-tutorial-checker.md`

Validates tutorial quality focusing on pedagogical structure, narrative flow, visual completeness, and hands-on elements.

- **Primary Use:** Validating tutorials for learning effectiveness and completeness
- **Specialization:** Pedagogical assessment, narrative flow analysis, diagram completeness checking, hands-on element validation, tutorial structure verification (Diátaxis compliance), content balance assessment
- **Tools:** Read, Glob, Grep, WebFetch, WebSearch
- **When to Use:**
  - Validating new tutorials before publication
  - Reviewing existing tutorials for quality and effectiveness
  - Ensuring tutorials have sufficient diagrams and visual aids
  - Checking narrative flow and storytelling quality
  - Verifying tutorials aren't list-heavy (need narrative explanations)
  - Assessing hands-on elements (code examples, checkpoints, exercises)
  - Ensuring progressive scaffolding (simple → complex)
  - Validating tutorial completeness (intro, objectives, prerequisites, next steps)
- **Works with:** `docs-tutorial-maker` for content creation, `docs-checker` for accuracy, `docs-link-checker` for links
- **Note:** Complements (doesn't duplicate) docs-checker (accuracy) and docs-link-checker (links)

### 🟨 `docs-file-manager.md`

Expert at managing files and directories in docs/ directory. Handles renaming, moving, and deleting operations while maintaining conventions.

- **Primary Use:** Reorganizing documentation structure, renaming directories, moving files, or deleting unused files/directories
- **Specialization:** File/directory management (rename, move, delete), prefix recalculation, link updates, index maintenance, safe deletion verification, git operations
- **Tools:** Read, Edit, Glob, Grep, Bash
- **When to Use:**
  - Renaming directories in docs/ (updates all file prefixes and links)
  - Moving files between directories (recalculates prefixes)
  - Deleting files or directories (verifies no broken links, updates references)
  - Reorganizing documentation structure with multiple operations
  - Fixing incorrect file prefixes that don't match directory location
  - After operations: automatically updates all internal links and indices
  - Uses git mv and git rm to preserve file history

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

### 🟦 `plan-maker.md`

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
- **Works with:** `plan-checker` for pre-implementation validation

### 🟪 `plan-executor.md`

Expert at systematically implementing project plans by following delivery checklists.

- **Primary Use:** Executing plans created by the plan-maker agent
- **Specialization:** Sequential implementation, per-phase validation, progress tracking, checklist management
- **Tools:** Read, Write, Edit, Glob, Grep, Bash
- **When to Use:**
  - Implementing a plan from plans/in-progress/
  - Following delivery checklists step-by-step
  - Running per-phase validation and acceptance criteria (self-validation)
  - Updating delivery.md with implementation progress and notes
  - Completing all phases of a multi-phase plan
  - Stopping at final validation handoff (does NOT perform final validation)
- **Works with:** `plan-execution-checker` for final validation

### 🟪 `plan-execution-checker.md`

Expert at validating plan implementations against requirements, performing comprehensive quality checks, and providing detailed validation reports.

- **Primary Use:** Independent final validation of completed plan implementations
- **Specialization:** Requirements verification, code quality assessment, integration testing, comprehensive validation reporting
- **Tools:** Read, Glob, Grep, Bash
- **When to Use:**
  - After plan-executor completes all implementation tasks
  - Validating implementation meets all requirements from requirements.md
  - Verifying technical documentation alignment (tech-docs.md)
  - Running comprehensive code quality checks (tests, lints, builds)
  - Performing end-to-end integration testing
  - Providing independent quality gate with fresh eyes
  - Generating detailed validation reports with specific findings
  - Iterating with plan-executor to fix issues until validation passes
- **Works with:** `plan-executor` for implementation

### 🟩 `plan-checker.md`

Expert at validating plans are ready for implementation by verifying completeness, checking codebase alignment, and validating technical accuracy using web verification.

- **Primary Use:** Pre-implementation validation of project plans
- **Specialization:** Plan completeness verification, codebase alignment checking, external verification via web, technical accuracy validation
- **Tools:** Read, Glob, Grep, WebSearch, WebFetch
- **When to Use:**
  - After plan-maker creates a plan, before implementation begins
  - Validating plan structure and completeness (requirements, tech-docs, delivery)
  - Verifying codebase assumptions are accurate (check package.json, directory structure)
  - Checking technology choices are current and maintained (WebSearch verification)
  - Validating documentation URLs are accessible (WebFetch)
  - Ensuring requirements have testable acceptance criteria
  - Identifying contradictions or missing information in plan
  - Preventing implementation blockers by catching plan issues early
- **Works with:** `plan-maker` for plan creation

### 🟦 `journal-maker.md`

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
1. Plan Creation
   └─> Use plan-maker to create structured plan in plans/backlog/
        └─> Creates requirements.md, tech-docs.md, delivery.md

2. Plan Validation (Quality Gate for Plans)
   └─> Use plan-checker with plan path
        └─> Validates plan structure and completeness
        └─> Verifies codebase assumptions (checks package.json, directories)
        └─> Validates technology choices via WebSearch
        └─> Checks documentation URLs via WebFetch
        └─> Identifies contradictions or missing information
        └─> If issues found: Returns to plan-maker for fixes
        └─> If validation passes: Plan ready for implementation

3. Implementation
   └─> Move plan from backlog/ to in-progress/
   └─> Use plan-executor with plan path
        └─> Executes delivery checklist step-by-step
        └─> Updates delivery.md with progress and notes
        └─> Performs per-phase validation (self-validation)
        └─> Marks status as "Ready for Final Validation"

4. Implementation Validation (Quality Gate for Code)
   └─> Use plan-execution-checker with plan path
        └─> Validates all requirements are met
        └─> Runs comprehensive quality checks
        └─> Performs integration testing
        └─> Generates detailed validation report
        └─> If issues found: Returns to plan-executor for fixes
        └─> If validation passes: Marks plan as complete

5. Complete and Archive
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
   └─> For tutorials: Use docs-tutorial-maker
        └─> Creates learning-oriented content with narrative flow
        └─> Adds comprehensive diagrams (architecture, sequences, flowcharts)
        └─> Includes hands-on elements and progressive scaffolding
   └─> For other docs: Use docs-maker
        └─> Creates how-to guides, reference, or explanations
        └─> Ensures proper formatting and convention compliance

5. Validate Tutorial Quality (for tutorials only)
   └─> Use docs-tutorial-checker to validate tutorial effectiveness
        └─> Checks pedagogical structure and narrative flow
        └─> Validates diagram completeness
        └─> Assesses hands-on elements and learning progression
        └─> Identifies list-heavy sections needing narrative

6. Rename/Move Files (if needed)
   └─> Use docs-file-manager to reorganize documentation
        └─> Renames files/directories with git mv
        └─> Recalculates file prefixes based on new location
        └─> Updates all internal links automatically
        └─> Updates index files (README.md)

7. Verify All Links
   └─> Use docs-link-checker to audit link health
        └─> Validates all external URLs are accessible
        └─> Validates all internal markdown links exist
        └─> Fixes broken links with working alternatives

8. Validate Documentation Accuracy
   └─> Use docs-checker to verify factual correctness
        └─> Validates technical claims against authoritative sources
        └─> Checks command syntax and code examples
        └─> Detects contradictions within and across documents
        └─> Identifies outdated information
```

## ✅ Best Practices

- **When starting a new project:** Use `plan-maker` to create structured plans in plans/backlog/
- **After creating a plan:** Use `plan-checker` to validate plan before implementation (prevents wasted effort)
- **When implementing a plan:** Use `plan-executor` with the plan path to execute systematically
- **After plan-executor completes:** Use `plan-execution-checker` for independent final validation
- **Full planning workflow:** plan-maker → plan-checker → (fix if needed) → plan-executor → plan-execution-checker
- **Quality assurance workflow:** Maker-checker at both stages (planning and implementation)
- **After adding new conventions:** Use `repo-rules-updater` → `repo-rules-checker`
- **Before major releases:** Run `repo-rules-checker` for full audit and `docs-link-checker` to verify all links
- **When creating tutorials:** Use `docs-tutorial-maker` for learning-oriented content with narrative flow and diagrams
- **When creating other documentation:** Use `docs-maker` for how-to guides, reference, or explanations
- **After creating tutorials:** Use `docs-tutorial-checker` to validate pedagogical quality and completeness
- **When modifying CLAUDE.md:** Use `repo-rules-updater` to cascade changes
- **During plan implementation:** Let `plan-executor` update delivery.md - it maintains detailed notes
- **When managing files in docs/:** Use `docs-file-manager` to handle prefixes, links, and indices automatically (rename, move, or delete)
- **After using docs-file-manager:** Always run `docs-link-checker` to verify all links are valid
- **Monthly or before releases:** Run `docs-link-checker` to ensure all links are valid, then `docs-checker` to verify technical accuracy
- **After major documentation updates:** Use `docs-link-checker` to verify link integrity, then `docs-checker` to validate content accuracy
- **After dependency updates:** Run `docs-checker` to ensure documentation matches new versions
- **Before releasing technical docs:** Use `docs-checker` to validate all technical claims and code examples
- **When reviewing contributions:** Use `docs-checker` to verify factual accuracy of new documentation

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
