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

### 🟪 `ayokoding-deployer.md`

Expert at deploying ayokoding-web to production. Synchronizes prod-ayokoding-web branch with main and pushes to origin to trigger automatic deployment to ayokoding.com via Vercel.

- **Primary Use:** Deploying ayokoding-web to production environment
- **Specialization:** Git branch synchronization, production deployment, Vercel integration, safety checks, rollback procedures
- **Tools:** Bash
- **When to Use:**
  - Deploying latest main branch to ayokoding.com production
  - After changes to apps/ayokoding-web/ are tested on main
  - Triggering Vercel production build and deployment
  - Need safe, controlled deployment with pre-flight checks
  - Syncing prod-ayokoding-web branch with origin/main
- **IMPORTANT:** All work must be done on main branch first - this agent only synchronizes branches, never commits directly to prod-ayokoding-web

### 🟪 `ose-platform-web-deployer.md`

Expert at deploying ose-platform-web to production. Synchronizes prod-ose-platform-web branch with main and pushes to origin to trigger automatic deployment to oseplatform.com via Vercel.

- **Primary Use:** Deploying ose-platform-web to production environment
- **Specialization:** Git branch synchronization, production deployment, Vercel integration, safety checks, rollback procedures
- **Tools:** Bash
- **When to Use:**
  - Deploying latest main branch to oseplatform.com production
  - After changes to apps/ose-platform-web/ are tested on main
  - Triggering Vercel production build and deployment
  - Need safe, controlled deployment with pre-flight checks
  - Syncing prod-ose-platform-web branch with origin/main
- **IMPORTANT:** All work must be done on main branch first - this agent only synchronizes branches, never commits directly to prod-ose-platform-web

### 🟦 `ayokoding-content-maker.md`

Expert at creating Hugo content for ayokoding-web (Hextra theme) following Hugo Content Convention and Content Quality Principles.

- **Primary Use:** Creating new Hugo content for ayokoding-web educational platform
- **Specialization:** Hextra theme content, bilingual content (Indonesian/English), learning content, personal essays, video content, archetype usage (5 types), Tutorial Convention compliance, Content Quality Principles
- **Tools:** Read, Write, Edit, Glob, Grep, Bash
- **When to Use:**
  - Creating learning content (tutorials, guides, courses) for ayokoding-web
  - Writing personal essays (celoteh/rants)
  - Adding video content (konten-video/video-content)
  - Creating section index pages (\_index.md)
  - Producing bilingual content (Indonesian and English versions)
- **Works with:** `ayokoding-content-checker` for validation before publication
- **References:** Hugo Content Convention, Content Quality Principles, Tutorial Convention, Tutorial Naming Convention

### 🟩 `ayokoding-content-checker.md`

Expert at validating Hugo content for ayokoding-web (Hextra theme) against Hugo Content Convention and Content Quality Principles.

- **Primary Use:** Validating ayokoding-web content quality before publication
- **Specialization:** Frontmatter validation (YAML, date format, required fields), content structure (heading hierarchy, links, images), Hextra shortcode validation, Mermaid diagram accessibility, tutorial-specific validation, content quality assessment
- **Tools:** Read, Grep, Glob, Bash
- **When to Use:**
  - Validating new content before publication
  - Checking frontmatter correctness (YAML format, date format YYYY-MM-DDTHH:MM:SS+07:00)
  - Verifying content structure (heading hierarchy, link format, image alt text)
  - Ensuring convention compliance (Hugo conventions, content quality standards)
  - Quality assurance before merging or deploying content
- **Works with:** `ayokoding-content-maker` for content creation
- **References:** Hugo Content Convention, Content Quality Principles, Tutorial Convention

### 🟨 `ayokoding-content-fixer.md`

Applies validated fixes from ayokoding-content-checker audit reports. Re-validates findings before applying changes to prevent false positives.

- **Primary Use:** Applying validated fixes from ayokoding-content-checker audit reports after user review
- **Specialization:** Fix validation, confidence assessment (HIGH/MEDIUM/FALSE_POSITIVE), automated fix application with safety checks for Hugo content
- **Tools:** Read, Edit, Glob, Grep, Write, Bash
- **When to Use:**
  - After reviewing ayokoding-content-checker audit report
  - Applying validated fixes automatically with re-validation
  - Detecting false positives in checker findings
  - Generating fix audit trail for transparency
- **Workflow:** ayokoding-content-checker (detect) → User review → ayokoding-content-fixer (apply validated fixes)
- **Safety:** Re-executes all checks before applying fixes (applies only HIGH confidence fixes automatically)
- **Output:** Generates `ayokoding-content__{timestamp}__fix.md` report in `generated-reports/`

### 🟩 `ayokoding-link-checker.md`

Validates internal and external links in ayokoding-web Hugo content, enforcing Hugo-specific linking conventions (absolute paths without .md extension). Detects common linking mistakes and maintains external link cache.

- **Primary Use:** Checking for dead links, verifying URL accessibility, validating Hugo link format compliance, or auditing link health in ayokoding-web
- **Specialization:** Hugo link format validation (absolute paths, no .md extension, no language prefix), external URL validation with caching, internal link verification, automatic cache pruning, mandatory lastFullScan updates, broken link detection and repair
- **Tools:** Read, Glob, Grep, WebFetch, WebSearch, Write, Edit
- **Cache File:** `apps/ayokoding-web/ayokoding-links-status.yml` (REQUIRED - site-specific cache, updated on EVERY run, regardless of invocation context)
- **Output:** Conversation response only (no separate report files created)
- **When to Use:**
  - Auditing all external and internal links in ayokoding-web content
  - Verifying Hugo link format compliance (no relative paths, .md extensions, language prefixes)
  - Checking external URLs are accessible (not 404, 403, or broken)
  - Validating internal links point to existing content files
  - Finding and replacing broken links with working alternatives
  - Periodic link health checks (monthly or before releases)
  - After major content updates to ensure link integrity
  - After file renames or directory restructuring
  - Automatic cache maintenance (prunes orphaned links, updates locations, updates lastFullScan)
- **IMPORTANT:** Cache requirement applies universally to ALL invocations - whether spawned by other agents, processes, or direct user invocation
- **Works with:** `ayokoding-content-maker` for content creation, `ayokoding-content-checker` for content quality
- **References:** Hugo Content Convention, Linking Convention (adapted for Hugo)

### 🟦 `ose-platform-web-content-maker.md`

Expert at creating Hugo content for ose-platform-web (PaperMod theme) following Hugo Content Convention and Content Quality Principles.

- **Primary Use:** Creating new Hugo content for ose-platform-web project landing page
- **Specialization:** PaperMod theme content (v7.0+), English-only content, update posts, about page, professional enterprise tone, archetype usage (default.md), release announcements
- **Tools:** Read, Write, Edit, Glob, Grep, Bash
- **When to Use:**
  - Creating platform update posts for ose-platform-web
  - Writing about page content or site information
  - Publishing project announcements or release notes
  - Adding progress updates or milestone reports
  - Creating English-only professional content for enterprise audience
- **Works with:** `ose-platform-web-content-checker` for validation before publication
- **References:** Hugo Content Convention, Content Quality Principles

### 🟩 `ose-platform-web-content-checker.md`

Expert at validating Hugo content for ose-platform-web (PaperMod theme) against Hugo Content Convention and Content Quality Principles.

- **Primary Use:** Validating ose-platform-web content quality before publication
- **Specialization:** Frontmatter validation (YAML, PaperMod fields, cover image, summary), content structure (heading hierarchy, links, images), Mermaid diagram accessibility, English language quality, professional tone verification
- **Tools:** Read, Grep, Glob, Bash
- **When to Use:**
  - Validating new content before publication
  - Checking frontmatter correctness (YAML format, date format, cover image alt text)
  - Verifying content structure (heading hierarchy, link format, image alt text)
  - Ensuring convention compliance (Hugo conventions, content quality standards)
  - Quality assurance before merging or deploying content
- **Works with:** `ose-platform-web-content-maker` for content creation
- **References:** Hugo Content Convention, Content Quality Principles

### 🟨 `ose-platform-web-content-fixer.md`

Applies validated fixes from ose-platform-web-content-checker audit reports. Re-validates findings before applying changes to prevent false positives.

- **Primary Use:** Applying validated fixes from ose-platform-web-content-checker audit reports after user review
- **Specialization:** Fix validation, confidence assessment (HIGH/MEDIUM/FALSE_POSITIVE), automated fix application with safety checks for Hugo content
- **Tools:** Read, Edit, Glob, Grep, Write, Bash
- **When to Use:**
  - After reviewing ose-platform-web-content-checker audit report
  - Applying validated fixes automatically with re-validation
  - Detecting false positives in checker findings
  - Generating fix audit trail for transparency
- **Workflow:** ose-platform-web-content-checker (detect) → User review → ose-platform-web-content-fixer (apply validated fixes)
- **Safety:** Re-executes all checks before applying fixes (applies only HIGH confidence fixes automatically)
- **Output:** Generates `ose-platform-web-content__{timestamp}__fix.md` report in `generated-reports/`

### 🟦 `hugo-developer.md`

Expert at developing Hugo sites (layouts, themes, assets, configuration) for ayokoding-web and ose-platform-web following Hugo Development Convention.

- **Primary Use:** Developing Hugo site infrastructure (layouts, themes, assets, config, build)
- **Specialization:** Theme customization (Hextra, PaperMod), layout development, shortcode creation, asset pipeline (Hugo Pipes), configuration, i18n, performance, SEO, accessibility, build processes
- **Tools:** Read, Write, Edit, Glob, Grep, Bash
- **When to Use:**
  - Customizing themes (Hextra/PaperMod)
  - Creating layouts, partials, templates
  - Developing custom shortcodes
  - Processing assets with Hugo Pipes
  - Configuring hugo.yaml
  - Implementing i18n translations
  - Optimizing performance and SEO
  - Managing build scripts
- **Out of Scope:** Creating content (use content-maker agents), validating content (use content-checker agents)
- **Works with:** Content-maker agents for content, hugo-developer for infrastructure
- **References:** Hugo Development Convention, Color Accessibility Convention

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
- **Works with:** `docs-fixer` for applying validated fixes, `docs-maker` for content creation

### 🟨 `docs-fixer.md`

Applies validated fixes from docs-checker audit reports. Re-validates factual accuracy findings before applying changes. Use after reviewing docs-checker output.

- **Primary Use:** Applying validated fixes from docs-checker audit reports after user review
- **Specialization:** Factual accuracy fix application, web-based re-validation (WebSearch/WebFetch), confidence assessment (HIGH/MEDIUM/FALSE_POSITIVE), objective vs subjective distinction, false positive detection
- **Tools:** Read, Edit, Glob, Grep, Write, Bash
- **When to Use:**
  - After reviewing docs-checker audit report and deciding to apply validated fixes
  - Fixing objective factual errors (command syntax, version numbers, broken links) automatically
  - Flagging subjective improvements (narrative quality, terminology choices) for manual review
  - Detecting and reporting false positives to improve docs-checker accuracy
  - Generating comprehensive fix reports with audit trail
  - Re-validating documentation findings using WebSearch and WebFetch before applying changes
- **Workflow:** docs-checker (detect) → User review → docs-fixer (apply validated fixes)
- **Safety:** Re-executes validation checks using web tools before applying fixes (applies only HIGH confidence objective fixes automatically)
- **Output:** Generates `docs__{timestamp}__fix.md` report in `generated-reports/`
- **Note:** Many documentation "issues" are subjective (editorial improvements, style preferences) - this agent applies only objective factual errors (verifiable against authoritative sources) and flags subjective improvements for human judgment
- **Works with:** `docs-checker` for audit report generation, `docs-maker` for content creation

### 🟦 `docs-maker.md`

Expert documentation writer specializing in Obsidian-optimized markdown and Diátaxis framework. Follows CLAUDE.md content philosophy (navigation document, not knowledge dump).

- **Primary Use:** Creating, editing, or organizing project documentation (how-to guides, reference, explanations)
- **Specialization:** Markdown optimization, Diátaxis framework, convention compliance, emoji usage, CLAUDE.md brevity (max 3-5 lines + link)
- **Tools:** Read, Write, Edit, Glob, Grep
- **When to Use:**
  - Creating how-to guides, reference documentation, or explanations
  - Editing existing documentation for clarity or structure
  - Organizing documentation according to Diátaxis framework
  - Ensuring documentation follows file naming, linking, and emoji conventions
  - Adding convention summaries to CLAUDE.md (brief only, link to details)
- **Works with:** `docs-checker` for accuracy validation, `docs-link-checker` for link validation
- **Note:** For tutorials, use `docs-tutorial-maker` instead

### 🟦 `docs-tutorial-maker.md`

Expert tutorial writer specializing in learning-oriented content with narrative flow, progressive scaffolding, visual aids, and hands-on elements. Follows Tutorial Naming Convention with six standardized types forming "Full Set" (5 sequential levels: Initial Setup, Quick Start, Beginner, Intermediate, Advanced) plus Cookbook (parallel track).

- **Primary Use:** Creating engaging, learning-oriented tutorials with narrative storytelling
- **Specialization:** Learning-oriented content, narrative writing, progressive scaffolding, diagram creation (architecture/sequence/flowcharts), hands-on examples, pedagogical structure, tutorial type selection (Initial Setup through Advanced)
- **Tools:** Read, Write, Edit, Glob, Grep, WebFetch, WebSearch
- **When to Use:**
  - Creating new tutorials in docs/tutorials/
  - Choosing appropriate tutorial type based on audience and depth (0-5% to 85-95% coverage)
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

Validates tutorial quality focusing on pedagogical structure, narrative flow, visual completeness, hands-on elements, and tutorial type compliance (Initial Setup through Advanced).

- **Primary Use:** Validating tutorials for learning effectiveness and completeness
- **Specialization:** Pedagogical assessment, narrative flow analysis, diagram completeness checking, hands-on element validation, tutorial structure verification (Diátaxis compliance), content balance assessment, tutorial type compliance validation (naming, coverage, time estimates)
- **Tools:** Read, Glob, Grep, WebFetch, WebSearch
- **When to Use:**
  - Validating new tutorials before publication
  - Reviewing existing tutorials for quality and effectiveness
  - Verifying tutorial type compliance (title, coverage, time estimate, depth)
  - Ensuring tutorials have sufficient diagrams and visual aids
  - Checking narrative flow and storytelling quality
  - Verifying tutorials aren't list-heavy (need narrative explanations)
  - Assessing hands-on elements (code examples, checkpoints, exercises)
  - Ensuring progressive scaffolding (simple → complex)
  - Validating tutorial completeness (intro, objectives, prerequisites, next steps)
- **Works with:** `docs-tutorial-maker` for content creation, `docs-checker` for accuracy, `docs-link-checker` for links
- **Note:** Complements (doesn't duplicate) docs-checker (accuracy) and docs-link-checker (links)

### 🟨 `docs-tutorial-fixer.md`

Applies validated fixes from docs-tutorial-checker audit reports. Re-validates pedagogical findings before applying changes to prevent false positives.

- **Primary Use:** Applying validated fixes from tutorial quality audits
- **Specialization:** Tutorial quality fix application, pedagogical finding re-validation, confidence level assessment (HIGH/MEDIUM/FALSE_POSITIVE), objective vs subjective issue distinction, fix report generation, false positive detection
- **Tools:** Read, Edit, Glob, Grep, Write, Bash
- **When to Use:**
  - Applying fixes from docs-tutorial-checker audit reports automatically
  - After reviewing checker findings and deciding to apply validated fixes
  - Fixing objective issues (missing sections, LaTeX errors, naming violations) automatically
  - Flagging subjective issues (narrative quality, diagram placement) for manual review
  - Detecting and reporting false positives to improve checker accuracy
  - Generating comprehensive fix reports with audit trail
  - Re-validating tutorial quality findings before applying changes
- **Works with:** `docs-tutorial-checker` for audit report generation, `docs-tutorial-maker` for content creation
- **Note:** Only applies HIGH confidence fixes (objective issues), flags MEDIUM confidence (subjective quality) for manual review

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

### 🟩 `readme-checker.md`

Validates README.md for engagement, accessibility, and quality standards. Checks for jargon, scannability, proper structure, and consistency with documentation.

- **Primary Use:** Reviewing README changes or auditing README quality before merge/release
- **Specialization:** Engagement validation (problem-solution hooks, clear motivation), accessibility checking (jargon detection, plain language, acronym context), scannability assessment (paragraph length ≤5 lines, visual hierarchy), navigation focus (summary + links, not comprehensive), language quality (active voice, benefits-focused)
- **Tools:** Read, Glob, Grep, Write, Bash
- **When to Use:**
  - Validating README updates before committing
  - Auditing README quality after major changes
  - Checking for jargon or corporate speak
  - Verifying paragraph length and scannability
  - Ensuring acronyms have context (not just expansion)
  - Confirming problem-solution narrative exists
  - Detecting duplicate content from detailed docs
  - Assessing overall engagement and accessibility
- **Works with:** `readme-maker` for content creation/updates, `readme-fixer` for applying validated fixes
- **References:** README Quality Convention

### 🟨 `readme-fixer.md`

Applies validated fixes from readme-checker audit reports. Re-validates README quality findings before applying changes. Conservative approach applies only objective fixes automatically.

- **Primary Use:** Applying validated fixes from readme-checker audit reports after user review
- **Specialization:** Objective vs subjective fix classification, confidence assessment (HIGH/MEDIUM/FALSE_POSITIVE), automated fix application for measurable issues (paragraph length, acronym context, broken links), manual review flagging for subjective improvements (tone, engagement, benefits framing)
- **Tools:** Read, Edit, Glob, Grep, Write, Bash
- **When to Use:**
  - After reviewing readme-checker audit report
  - Applying validated objective fixes automatically (paragraph breaks, acronym expansion, format corrections)
  - Detecting false positives in checker findings
  - Flagging subjective improvements for manual review
  - Generating fix audit trail for transparency
- **Workflow:** readme-checker (detect) → User review → readme-fixer (apply validated fixes)
- **Safety:** Re-executes all checks before applying fixes (applies only HIGH confidence objective fixes automatically)
- **Output:** Generates `readme__{timestamp}__fix.md` report in `generated-reports/`
- **Note:** Many README quality issues are subjective - this agent applies only objective fixes (measurable violations) and flags subjective improvements (tone, engagement) for human judgment
- **Works with:** `readme-checker` for audit report generation, `readme-maker` for content creation

### 🟦 `readme-maker.md`

Creates and updates README.md content while maintaining engagement, accessibility, and quality standards. Rewrites jargony sections, adds context to acronyms, breaks up dense paragraphs.

- **Primary Use:** Adding or updating README sections with quality standards
- **Specialization:** Engaging content creation (problem-solution hooks, clear narrative), jargon elimination (plain language transformation), acronym contextualization (English-first naming), benefits-focused writing (user perspective), scannability optimization (short paragraphs, visual hierarchy), navigation structure (summary + links)
- **Tools:** Read, Write, Edit, Glob, Grep
- **When to Use:**
  - Adding new sections to README (features, getting started, etc.)
  - Rewriting jargony or corporate sections
  - Breaking up dense paragraphs (>5 lines)
  - Adding context to unexplained acronyms
  - Converting feature lists to user benefits
  - Creating problem-solution hooks for motivation
  - Ensuring README stays navigation-focused (not comprehensive)
  - Maintaining consistent, welcoming tone throughout
- **Works with:** `readme-checker` for quality validation
- **References:** README Quality Convention

### 🟩 `repo-rules-checker.md`

Expert at validating consistency between agents, CLAUDE.md, conventions, and documentation. Generates detailed audit reports in `generated-reports/repo-rules-audit-{timestamp}.md` for historical tracking.

- **Primary Use:** Checking for inconsistencies, contradictions, or verifying compliance
- **Specialization:** Cross-file validation, duplication detection, convention enforcement, emoji consistency, persistent audit reporting
- **Tools:** Read, Glob, Grep, Write
- **When to Use:**
  - After making changes to conventions or CLAUDE.md
  - Periodic audits of repository consistency
  - Validating that all files follow documented standards (including emoji usage)
  - Detecting contradictions or outdated references
  - Identifying duplicate content that could be consolidated
  - Historical tracking of repository consistency over time
- **Important:** READ-ONLY agent - does not apply fixes. Use `repo-rules-fixer` to apply validated fixes after reviewing audit report.

### 🟨 `repo-rules-fixer.md`

Applies validated fixes from repo-rules-checker audit reports. Re-validates findings before applying changes to prevent false positives.

- **Primary Use:** Applying validated fixes from repo-rules-checker audit reports after user review
- **Specialization:** Fix validation, confidence assessment (HIGH/MEDIUM/FALSE_POSITIVE), automated fix application with safety checks
- **Tools:** Read, Edit, Glob, Grep, Write
- **When to Use:**
  - After reviewing repo-rules-checker audit report
  - Applying validated fixes automatically with re-validation
  - Detecting false positives in checker findings
  - Generating fix audit trail for transparency
- **Workflow:** repo-rules-checker (detect) → User review → repo-rules-fixer (apply validated fixes)
- **Safety:** Re-executes all checks before applying fixes (applies only HIGH confidence fixes automatically)
- **Output:** Generates `repo-rules-audit-{timestamp}-fix.md` report in `generated-reports/`

### 🟨 `repo-rules-maker.md`

Expert at making rule and convention changes effective across CLAUDE.md, convention docs, agents, and indices. Responsible for maintaining CLAUDE.md size limits.

- **Primary Use:** Adding/modifying rules, conventions, or standards affecting multiple files
- **Specialization:** Systematic propagation, cascade updates, consistency maintenance, CLAUDE.md size management (40k hard limit, 30k target)
- **Tools:** Read, Edit, Glob, Grep
- **When to Use:**
  - Adding new conventions or standards (including emoji vocabulary)
  - Modifying existing rules that affect multiple files
  - Ensuring changes cascade to all relevant locations
  - Updating cross-references after structural changes
  - Maintaining consistency across agent definitions
  - Checking CLAUDE.md size and suggesting condensation when needed

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
- **Tools:** Read, Glob, Grep, WebSearch, WebFetch, Write, Bash
- **When to Use:**
  - After plan-maker creates a plan, before implementation begins
  - Validating plan structure and completeness (requirements, tech-docs, delivery)
  - Verifying codebase assumptions are accurate (check package.json, directory structure)
  - Checking technology choices are current and maintained (WebSearch verification)
  - Validating documentation URLs are accessible (WebFetch)
  - Ensuring requirements have testable acceptance criteria
  - Identifying contradictions or missing information in plan
  - Preventing implementation blockers by catching plan issues early
- **Output:** Generates `plan__{timestamp}__validation.md` report in `generated-reports/`
- **Works with:** `plan-fixer` for applying validated fixes, `plan-maker` for plan creation

### 🟨 `plan-fixer.md`

Applies validated fixes from plan-checker audit reports. Re-validates plan completeness and accuracy findings before applying changes. Distinguishes structural/format issues from strategic decisions.

- **Primary Use:** Applying validated fixes from plan-checker validation reports after user review
- **Specialization:** Structural vs strategic issue classification, confidence assessment (HIGH/MEDIUM/FALSE_POSITIVE), automated fix application for objective issues (missing sections, broken links, format errors), manual review flagging for strategic decisions (scope, architecture, technology choices, timelines)
- **Tools:** Read, Edit, Glob, Grep, Write, Bash
- **When to Use:**
  - After reviewing plan-checker validation report
  - Applying validated structural/format fixes automatically (missing sections, broken links, format violations)
  - Detecting false positives in checker findings
  - Flagging strategic/architectural decisions for manual review
  - Generating fix audit trail for transparency
- **Workflow:** plan-checker (validate) → User review → plan-fixer (apply validated structural fixes)
- **Safety:** Re-executes all checks before applying fixes (applies only HIGH confidence objective fixes automatically)
- **Output:** Generates `plan__{timestamp}__fix.md` report in `generated-reports/`
- **Note:** Plans contain strategic decisions - this agent applies only structural/format fixes (objective) and flags strategic choices (scope, architecture, timelines) for human judgment
- **Works with:** `plan-checker` for validation report generation, `plan-maker` for plan creation

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

### 📝 Content Quality Workflows (Maker-Checker-Fixer Pattern)

The repository uses a three-stage workflow for content creation and quality assurance:

**Pattern: Maker → Checker → (User Review) → Fixer**

```
1. Creation Stage (Maker Agents)
   └─> Create content following conventions
        └─> Makers: docs-maker, docs-tutorial-maker, readme-maker,
                    ayokoding-content-maker, ose-platform-web-content-maker

2. Detection Stage (Checker Agents)
   └─> Validate content quality and convention compliance
        └─> Generate audit report in generated-reports/
        └─> Checkers: docs-checker, docs-tutorial-checker, readme-checker,
                      ayokoding-content-checker, ose-platform-web-content-checker,
                      repo-rules-checker

3. User Review Stage
   └─> Review audit report findings
        └─> Identify which fixes should be applied
        └─> Verify checker findings are accurate

4. Fix Application Stage (Fixer Agents)
   └─> Re-validate findings, apply HIGH confidence fixes automatically
        └─> Skip MEDIUM confidence (manual review needed)
        └─> Skip FALSE_POSITIVE (report to improve checker)
        └─> Generate fix report in generated-reports/
        └─> Fixers: docs-fixer, docs-tutorial-fixer, readme-fixer,
                    ayokoding-content-fixer, ose-platform-web-content-fixer,
                    repo-rules-fixer, plan-fixer

5. Verification Stage (Re-run Checker)
   └─> Re-run checker to verify fixes resolved issues
        └─> Ensure no new issues introduced
```

**Why This Pattern:**

- **Safety:** Human review before automated fixes (prevents over-automation)
- **Confidence Levels:** Fixers distinguish objective fixes (HIGH - apply) from subjective improvements (MEDIUM - manual review)
- **False Positive Detection:** Fixers validate findings, report checker errors with improvement suggestions
- **Audit Trail:** Both audit and fix reports saved in `generated-reports/` for transparency
- **Quality Improvement Loop:** False positive reports improve checker accuracy over time

**See Also:** [Fixer Confidence Levels Convention](../docs/explanation/development/ex-de__fixer-confidence-levels.md) for universal confidence assessment criteria.

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
   └─> Use repo-rules-maker to make changes effective across files
        └─> Ensures consistency in CLAUDE.md, conventions, agents, indices

2. Validate Changes
   └─> Use repo-rules-checker to verify consistency
        └─> Detects inconsistencies, contradictions, duplications
        └─> Generates audit report in generated-reports/

3. Review Audit Report
   └─> Check findings and validate recommendations
        └─> Identify which fixes should be applied

4. Apply Validated Fixes
   └─> Use repo-rules-fixer to apply fixes automatically
        └─> Re-validates findings, applies HIGH confidence fixes
        └─> Skips false positives, reports MEDIUM confidence items
        └─> Generates fix report for audit trail

5. Verify Fixes (if fixes were applied)
   └─> Use repo-rules-checker to re-validate
        └─> Ensure fixes resolved issues without introducing new ones

6. Write/Update Documentation
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
        └─> Generates validation report in generated-reports/

9. Apply Documentation Fixes (if issues found)
   └─> Use docs-fixer to apply validated fixes
        └─> Re-validates findings using WebSearch and WebFetch
        └─> Applies HIGH confidence objective fixes automatically
        └─> Flags MEDIUM confidence subjective improvements for manual review
        └─> Reports FALSE_POSITIVE to improve checker
        └─> Generates fix report for audit trail

10. Verify Documentation Fixes (if fixes applied)
   └─> Re-run docs-checker to verify fixes resolved issues
        └─> Ensure no new factual errors introduced
```

## ✅ Best Practices

- **When starting a new project:** Use `plan-maker` to create structured plans in plans/backlog/
- **After creating a plan:** Use `plan-checker` to validate plan before implementation (prevents wasted effort)
- **When implementing a plan:** Use `plan-executor` with the plan path to execute systematically
- **After plan-executor completes:** Use `plan-execution-checker` for independent final validation
- **Full planning workflow:** plan-maker → plan-checker → (fix if needed) → plan-executor → plan-execution-checker
- **Quality assurance workflow:** Maker-checker at both stages (planning and implementation)
- **After adding new conventions:** Use `repo-rules-maker` → `repo-rules-checker` → `repo-rules-fixer` (if issues found)
- **CLAUDE.md maintenance:** Keep under 30k characters (target), never exceed 40k (hard limit). Brief summaries only, link to detailed docs. Use `repo-rules-maker` to check size when adding rules
- **Agent file size limits:** Three tiers - Simple (<800 lines), Standard (<1,200 lines), Complex (<1,800 lines). Link to convention docs instead of duplicating content. See [AI Agents Convention](../docs/explanation/development/ex-de__ai-agents.md) for complete size guidelines
- **Before major releases:** Run `repo-rules-checker` for full audit and `docs-link-checker` to verify all links
- **When creating tutorials:** Use `docs-tutorial-maker` for learning-oriented content with narrative flow and diagrams
- **When creating other documentation:** Use `docs-maker` for how-to guides, reference, or explanations
- **After creating tutorials:** Use `docs-tutorial-checker` to validate pedagogical quality and completeness
- **When modifying CLAUDE.md:** Use `repo-rules-maker` to cascade changes
- **During plan implementation:** Let `plan-executor` update delivery.md - it maintains detailed notes
- **When managing files in docs/:** Use `docs-file-manager` to handle prefixes, links, and indices automatically (rename, move, or delete)
- **After using docs-file-manager:** Always run `docs-link-checker` to verify all links are valid
- **Monthly or before releases:** Run `docs-link-checker` to ensure all links are valid, then `docs-checker` to verify technical accuracy
- **After major documentation updates:** Use `docs-link-checker` to verify link integrity, then `docs-checker` to validate content accuracy
- **After dependency updates:** Run `docs-checker` to ensure documentation matches new versions
- **Before releasing technical docs:** Use `docs-checker` to validate all technical claims and code examples
- **When reviewing contributions:** Use `docs-checker` to verify factual accuracy of new documentation
- **Documentation accuracy workflow:** docs-checker → (review validation report) → docs-fixer (apply objective fixes) → re-run docs-checker
- **Documentation validation with automated fixes:** Use `docs-checker` to generate validation report, then `docs-fixer` to apply validated objective fixes (command syntax, version numbers, broken links) while flagging subjective improvements (narrative, terminology) for manual review
- **When creating/updating README:** Use `readme-maker` for content, then `readme-checker` for validation
- **README quality workflow:** readme-maker → readme-checker → (review audit) → readme-fixer (apply objective fixes) → commit
- **README validation with automated fixes:** Use `readme-checker` to generate audit report, then `readme-fixer` to apply validated objective fixes (paragraph breaks, acronym context, format corrections) while flagging subjective improvements for manual review

## 📚 Resources

- [AI Agents Convention](../docs/explanation/development/ex-de__ai-agents.md) - Complete agent specification and standards
- [CLAUDE.md](../CLAUDE.md) - Project guidance for all agents
- [Documentation Conventions](../docs/explanation/conventions/README.md) - File naming, linking, and Diátaxis framework
- [Plans Organization](../plans/README.md) - Planning document structure and conventions

## 🆕 Adding New Agents

When creating new agents:

1. Use `agent-maker` to automate creation with proper structure, size verification, and README updates
2. Follow the [AI Agents Convention](../docs/explanation/development/ex-de__ai-agents.md) for all standards
3. Verify agent size within tier limits (Simple: <800, Standard: <1,200, Complex: <1,800 lines)
4. Use `repo-rules-maker` to propagate references to CLAUDE.md and other files
5. Use `repo-rules-checker` to validate the new agent follows all conventions
6. Use `repo-rules-fixer` to apply any validated fixes from the audit report
7. Update CLAUDE.md if the agent should be mentioned in project guidance

---

**Note:** This README follows the naming exception for README.md files documented in the [File Naming Convention](../docs/explanation/conventions/ex-co__file-naming-convention.md).
