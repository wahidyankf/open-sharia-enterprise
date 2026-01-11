# AI Agents

This directory contains specialized AI agents for repository maintenance and documentation tasks. Each agent has specific responsibilities and tools to ensure consistency, quality, and adherence to project conventions.

**Hierarchy Position**: AI agents are the **implementation layer** (Layer 4 in the six-layer hierarchy) that enforces conventions (Layer 2) and development practices (Layer 3). Each agent implements and validates specific rules, which ultimately trace back through [Core Principles](../rules/principles/README.md) (Layer 1) to the [Vision](../rules/vision/open-sharia-enterprise.md) (Layer 0) of democratizing Shariah-compliant enterprise.

**Traceability**: Every agent should document which conventions/practices it enforces. When creating new agents, reference the specific rules being implemented. This creates a clear chain: Vision → Principle → Convention/Practice → Agent.

**Token Budget**: When invoking agents and workflows, don't think about token budget constraints. We have "unlimited" token budget through reliable compaction mechanism. Focus on execution quality, not token efficiency. See [AI Agents Convention - Token Budget Philosophy](../rules/development/agents/ex-ru-de-ag-ai-agents.md#token-budget-philosophy) for complete details.

## Available Agents

### 🟦 `agent-maker.md`

Expert at creating new AI agents following all repository conventions.

- **Primary Use:** Adding a new agent to .claude/agents/ directory
- **Specialization:** Agent scaffolding, automatic README updates, convention compliance, validation integration
- **Tools:** Read, Glob, Grep, Write, Bash
- **When to Use:**
  - Creating a new AI agent with proper structure and frontmatter
  - Auto-generating agent files following all naming and format conventions
  - Automatically assigning color based on agent role
  - Automatically updating agents README with new agent listing
  - Running validation via wow\_\_rules-checker

### 🟦 `ayokoding-web-by-example-maker.md`

Expert at creating by-example tutorials with 75-90 annotated code examples achieving 95% coverage for ayokoding-web. Uses five-part format (explanation, diagram, annotated code, takeaway) with self-contained examples.

- **Primary Use:** Creating by-example tutorial content for experienced developers switching languages or frameworks
- **Specialization:** Code-first learning materials, 75-90 annotated examples per language/framework, 95% coverage target, five-part format (explanation → diagram → annotated code → takeaway), self-contained runnable examples, educational comment standards (`// =>` notation), color-blind friendly diagrams (30-50% frequency)
- **Tools:** Read, Write, Edit, Glob, Grep, WebFetch, WebSearch, Bash
- **Model:** sonnet (advanced educational content design, pattern recognition for coverage, multi-step workflow orchestration)
- **When to Use:**
  - Creating by-example tutorials for programming languages (Go, Elixir, TypeScript, Python, etc.)
  - Creating by-example tutorials for frameworks (Phoenix, React, Next.js, etc.)
  - Targeting experienced developers who prefer code-first learning
  - Generating 75-90 annotated examples with educational value
  - Aiming for comprehensive 95% coverage of language/framework features
- **Works with:** `ayokoding-web-by-example-checker` for validation, `ayokoding-web-by-example-fixer` for fixes
- **References:** By-Example Tutorial Convention (master reference), Hugo Content Convention, Content Quality Principles, Tutorial Convention, Color Accessibility Convention

### 🟦 `ayokoding-web-general-maker.md`

Expert at creating general Hugo content for ayokoding-web (Hextra theme) following Hugo Content Convention and Content Quality Principles.

- **Primary Use:** Creating new general Hugo content for ayokoding-web educational platform (not by-example tutorials)
- **Specialization:** Hextra theme content, bilingual content (Indonesian/English), learning content, personal essays, video content, archetype usage (5 types), Tutorial Convention compliance, Content Quality Principles
- **Tools:** Read, Write, Edit, Glob, Grep, Bash
- **Model:** sonnet (advanced educational content design)
- **When to Use:**
  - Creating general learning content (tutorials, guides, courses) for ayokoding-web
  - Writing personal essays (celoteh/rants)
  - Adding video content (konten-video/video-content)
  - Creating section index pages (\_index.md)
  - Producing bilingual content (Indonesian and English versions)
- **Works with:** `ayokoding-web-general-checker` for validation before publication
- **References:** Hugo Content Convention, Content Quality Principles, Tutorial Convention, Tutorial Naming Convention

### 🟩 `ayokoding-web-general-checker.md`

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
- **Works with:** `ayokoding-web-general-maker` for content creation
- **References:** Hugo Content Convention, Content Quality Principles, Tutorial Convention

### 🟩 `ayokoding-web-facts-checker.md`

Expert at validating factual correctness of ayokoding-web educational content using web verification. Checks technical accuracy, code examples, tutorial sequences, and bilingual consistency.

- **Primary Use:** Verifying factual accuracy of educational tutorials and learning content
- **Specialization:** Code example validation, version number verification, bilingual consistency checking, tutorial sequence logic, educational accuracy using WebSearch/WebFetch
- **Tools:** Read, Glob, Grep, Write, Bash, WebFetch, WebSearch
- **When to Use:**
  - Validating tutorial code examples are correct and current
  - Checking framework/library versions are accurate
  - Verifying bilingual content (Indonesian/English) is factually consistent
  - Ensuring learning sequences are logical and achievable
  - Auditing educational content for technical accuracy
  - After framework/library version updates
- **Works with:** `ayokoding-web-facts-fixer` for applying validated fixes
- **References:** Factual Validation Convention, Hugo Content Convention, Tutorial Convention

### 🟪 `ayokoding-web-general-fixer.md`

Applies validated fixes from ayokoding-web-general-checker audit reports. Re-validates findings before applying changes to prevent false positives.

- **Primary Use:** Applying validated fixes from ayokoding-web-general-checker audit reports after user review
- **Specialization:** Fix validation, confidence assessment (HIGH/MEDIUM/FALSE_POSITIVE), automated fix application with safety checks for Hugo content
- **Tools:** Read, Edit, Glob, Grep, Write, Bash
- **When to Use:**
  - After reviewing ayokoding-web-general-checker audit report
  - Applying validated fixes automatically with re-validation
  - Detecting false positives in checker findings
  - Generating fix audit trail for transparency
- **Workflow:** ayokoding-web-general-checker (detect) → User review → ayokoding-web-general-fixer (apply validated fixes)
- **Safety:** Re-executes all checks before applying fixes (applies only HIGH confidence fixes automatically)
- **Output:** Generates `ayokoding-web-{timestamp}-fix.md` report in `generated-reports/`

### 🟦 `ayokoding-web-by-example-maker.md`

Expert at creating by-example tutorials with 75-90 annotated examples achieving 95% coverage for ayokoding-web.

- **Primary Use:** Creating code-first learning resources with comprehensive annotated examples
- **Specialization:** Five-part format (explanation, diagram, annotated code with `// =>` notation, key takeaway), self-contained examples, color-blind friendly diagrams, bilingual support
- **Tools:** Read, Write, Edit, Glob, Grep, WebFetch, WebSearch, Bash
- **When to Use:**
  - Creating new by-example tutorials for programming languages/frameworks
  - Generating 75-90 progressive examples (beginner, intermediate, advanced)
  - Building code-first learning resources with extensive annotations
  - Achieving 95% coverage through practical examples
- **Works with:** `ayokoding-web-by-example-checker` for validation
- **References:** By-Example Tutorial Convention, Hugo Content Convention, Content Quality Principles

### 🟩 `ayokoding-web-by-example-checker.md`

Validates by-example tutorial quality focusing on 95% coverage, self-containment, educational annotations, and diagram presence.

- **Primary Use:** Validating by-example tutorials meet quality standards
- **Specialization:** Coverage verification (95% target), example count validation (75-90 range), self-containment checks, annotation quality (`// =>` notation), diagram presence frequency (30-50%)
- **Tools:** Read, Glob, Grep, Write, Bash
- **When to Use:**
  - After creating/updating by-example tutorials
  - Validating 95% coverage achievement
  - Checking self-containment (imports present, runnable)
  - Verifying annotation quality and diagram presence
  - Quality assurance before publication
- **Works with:** `ayokoding-web-by-example-maker` for content creation, `ayokoding-web-by-example-fixer` for fixing issues
- **References:** By-Example Tutorial Convention, Content Quality Principles
- **Output:** Generates `ayokoding-web-by-example-{uuid-chain}-{timestamp}-audit.md` report in `generated-reports/`

### 🟪 `ayokoding-web-by-example-fixer.md`

Applies validated fixes from ayokoding-web-by-example-checker audit reports. Re-validates findings before applying changes to prevent false positives.

- **Primary Use:** Applying validated fixes from by-example tutorial audits after user review
- **Specialization:** Fix validation for by-example tutorials, confidence assessment (HIGH/MEDIUM/FALSE_POSITIVE), automated fix application with safety checks
- **Tools:** Read, Edit, Glob, Grep, Write, Bash
- **When to Use:**
  - After reviewing ayokoding-web-by-example-checker audit report
  - Applying validated fixes automatically with re-validation
  - Detecting false positives in checker findings
  - Generating fix audit trail for transparency
- **Workflow:** ayokoding-web-by-example-checker (detect) → User review → ayokoding-web-by-example-fixer (apply validated fixes)
- **Safety:** Re-executes all checks before applying fixes (applies only HIGH confidence fixes automatically)
- **Output:** Generates `ayokoding-web-by-example-{uuid-chain}-{timestamp}-fix.md` report in `generated-reports/`

### 🟪 `ayokoding-web-deployer.md`

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

### 🟪 `ayokoding-web-facts-fixer.md`

Applies validated fixes from ayokoding-web-facts-checker audit reports. Re-validates factual findings before applying changes.

- **Primary Use:** Applying validated factual accuracy fixes from ayokoding-web-facts-checker reports after user review
- **Specialization:** Factual accuracy fix application, confidence assessment (HIGH/MEDIUM/FALSE_POSITIVE), objective vs subjective distinction, code/version corrections
- **Tools:** Read, Edit, Glob, Grep, Write, Bash
- **When to Use:**
  - After reviewing ayokoding-web-facts-checker validation report
  - Fixing objective factual errors (code syntax, API usage, version numbers) automatically
  - Flagging subjective improvements (difficulty levels, pedagogical choices) for manual review
  - Detecting and reporting false positives to improve checker accuracy
  - Generating comprehensive fix reports with audit trail
- **Workflow:** ayokoding-web-facts-checker (validate) → User review → ayokoding-web-facts-fixer (apply validated factual fixes)
- **Safety:** Re-validates findings before applying fixes (applies only HIGH confidence objective fixes automatically)
- **Output:** Generates `ayokoding-facts-{timestamp}-fix.md` report in `generated-reports/`

### 🟪 `ayokoding-web-link-checker.md`

**HYBRID AGENT** (Validator + State Manager): Validates internal and external links in ayokoding-web Hugo content while maintaining operational cache file. Enforces Hugo-specific linking conventions (absolute paths without .md extension). Detects common linking mistakes and maintains external link cache.

- **Primary Use:** Checking for dead links, verifying URL accessibility, validating Hugo link format compliance, or auditing link health in ayokoding-web
- **Specialization:** Hugo link format validation (absolute paths, no .md extension, no language prefix), external URL validation with caching, internal link verification, automatic cache pruning, mandatory lastFullScan updates, broken link detection and repair
- **Tools:** Read, Glob, Grep, WebFetch, WebSearch, Write, Edit, Bash
- **Color:** purple (hybrid: validation + cache state management)
- **Why hybrid:** Validation (checker behavior) + persistent cache file management (state management) - Write tool used ONLY for designated cache file, NOT general content modification
- **Cache File:** `apps/ayokoding-web/ayokoding-links-status.yaml` (REQUIRED - site-specific cache, updated on EVERY run, regardless of invocation context)
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
- **Works with:** `ayokoding-web-general-maker` for content creation, `ayokoding-web-general-checker` for content quality
- **References:** Hugo Content Convention, Linking Convention (adapted for Hugo)
- **See Also:** [AI Agents Convention - Hybrid Agents Exception](../rules/development/agents/ex-ru-de-ag-ai-agents.md#hybrid-agents-exception) for complete rationale

### 🟡 `apps-ayokoding-web-link-fixer.md`

Fixes broken links and link format violations in ayokoding-web Hugo site content based on validation audit findings from apps**ayokoding-web**link-checker.

- **Primary Use:** Applying validated link fixes from link-checker audit reports after user review
- **Specialization:** Broken internal link fixes (update paths), Hugo format violations (add language prefix, remove .md), external link updates (with user confirmation), confidence-based prioritization (P0-P4)
- **Tools:** Read, Write, Edit, Grep, Glob, Bash
- **When to Use:**
  - After reviewing link-checker audit findings
  - Fixing broken internal links with clear correct paths
  - Correcting Hugo link format violations (missing /en/ or /id/, .md extensions)
  - Updating/removing broken external links (requires user confirmation)
  - Applying P0-P4 prioritized fixes based on criticality + confidence
- **Workflow:** apps**ayokoding-web**link-checker (detect) → User review → apps**ayokoding-web**link-fixer (apply validated fixes)
- **Safety:** Re-validates findings, uses confidence levels (HIGH/MEDIUM/FALSE_POSITIVE), asks for confirmation on external link changes
- **References:** Linking Convention, Hugo Content Convention, Fixer Confidence Levels, Maker-Checker-Fixer Pattern

### 🟩 `ayokoding-web-navigation-maker.md`

Automatically regenerate 2-layer navigation listings in ayokoding-web \_index.md files from file structure. **⚠️ Now automated via pre-commit hook** - runs automatically when committing ayokoding-web content changes (see [Pre-commit Automation](../apps/ayokoding-cli/README.md#pre-commit-automation)).

- **Primary Use:** Batch updates or manual corrections outside commit workflow (most updates now automated via pre-commit hook)
- **Specialization:** File structure scanning (2 layers deep), weight-based sorting, frontmatter preservation, title extraction, markdown navigation generation, mechanical content regeneration
- **Tools:** Read, Write, Glob, Grep, Bash
- **Model:** Haiku (efficient mechanical task)
- **When to Use:**
  - After adding new content files to ayokoding-web (tutorials, guides, pages)
  - After changing file weights to regenerate navigation order
  - After restructuring content (moving files, renaming directories)
  - Bulk navigation updates needed across multiple \_index.md files
  - Semi-automatic workflow (suggested after content changes detected)
- **Exclusions:** Root \_index.md files (en/\_index.md, id/\_index.md) use custom Hugo shortcodes
- **Content Replacement:** Completely replaces everything after frontmatter with generated navigation list
- **Works with:** `ayokoding-web-general-maker` creates files → `ayokoding-web-navigation-maker` generates navigation → `ayokoding-web-structure-checker` validates → `ayokoding-web-structure-fixer` fixes issues
- **References:** Hugo Content Convention - ayokoding (weight system, navigation depth)

### 🟪 `ayokoding-web-title-maker.md`

Automatically update title fields in ayokoding-web markdown files based on filenames and configuration. **⚠️ Now automated via pre-commit hook** - runs automatically when committing ayokoding-web content changes (see [Pre-commit Automation](../apps/ayokoding-cli/README.md#pre-commit-automation)).

- **Primary Use:** Batch updates or manual corrections outside commit workflow (most updates now automated via pre-commit hook)
- **Specialization:** Title generation from filenames (Title Case), custom overrides (special cases), lowercase article/preposition handling, bilingual support (English/Indonesian), idempotent updates (skip if already correct)
- **Tools:** Bash
- **Model:** Haiku (efficient mechanical task)
- **When to Use:**
  - After adding new content files to ayokoding-web
  - After renaming files to regenerate titles from new filenames
  - Bulk title standardization needed across all content
  - Before navigation regeneration to ensure titles are consistent
  - Semi-automatic workflow (suggested after content changes detected)
- **Do NOT use for:** Creating new files, validating structure, fixing weights, writing custom content, manual title editing
- **Idempotency:** Only updates files where title differs from expected (faster execution, cleaner git diffs)
- **Works with:** `ayokoding-web-general-maker` creates files → `ayokoding-web-title-maker` updates titles → `ayokoding-web-navigation-maker` generates navigation → `ayokoding-web-structure-checker` validates
- **Configuration:** Uses `config/title-overrides-en.yaml` and `config/title-overrides-id.yaml` for special cases
- **References:** Hugo Content Convention - ayokoding, Content Quality Principles

### 🟨 `ayokoding-web-structure-maker.md`

Expert at proactively modifying ayokoding-web content structure by adjusting weights to reorder content, insert new items at specific positions, and maintain weight conventions. Automatically regenerates navigation listings after structural changes using ayokoding-web-navigation-maker CLI.

- **Primary Use:** Intentionally restructuring ayokoding-web content by adjusting weights
- **When to Use:**
  - Reordering content intentionally (e.g., move rust before golang)
  - Inserting new items at specific positions with proper weight spacing
  - Restructuring sections (change organization of existing content)
  - Preparing for new content (reserve weight slots for upcoming additions)
- **Key Operations:**
  - Reorder existing items by adjusting weights
  - Insert new items at specific positions with cascading updates
  - Create gaps for future content insertions
  - Move items to different parent folders with level recalculation
  - Invoke ayokoding-web-navigation-maker CLI automatically after changes
- **Works with:** `ayokoding-web-general-maker` creates files → `ayokoding-web-structure-maker` adjusts weights → `ayokoding-web-navigation-maker` regenerates navigation → `ayokoding-web-structure-checker` validates → `ayokoding-web-structure-fixer` fixes issues
- **Workflow:** Proactive structural changes (this agent) → Navigation Regeneration (CLI) → Validation (checker) → Reactive fixes (fixer if needed)
- **References:** Hugo Content Convention - ayokoding (weight system, level-based ordering)

### 🟩 `ayokoding-web-structure-checker.md`

Expert at validating ALL ayokoding-web content files including navigation architecture, weight conventions across all markdown files, overview completeness, and pedagogical progression.

- **Primary Use:** Validating ayokoding-web navigation architecture and structural compliance for ALL markdown files
- **Specialization:** Navigation depth validation (3 layers), weight ordering for ALL content files (level-based system with per-parent resets), overview/ikhtisar presence checking, tutorial progression validation, pedagogical progression assessment, structural integrity verification
- **Tools:** Read, Glob, Grep, Write, Bash
- **Expanded Scope:** Now validates ALL markdown files with weight fields, including:
  - Navigation files (`_index.md`)
  - Overview/intro files (`overview.md`, `ikhtisar.md`)
  - Recipe collections (`cookbook.md`)
  - Tutorial files (`initial-setup.md`, `quick-start.md`, `beginner.md`, `intermediate.md`, `advanced.md`)
  - How-to guides (all files in `how-to/` directories)
  - Reference files (all files in `reference/` directories)
  - Explanation files (all files in `explanation/` directories)
  - Topic content files (standalone content with weights)
  - Static pages (`about-ayokoding.md`, `terms-and-conditions.md`, etc.)
- **When to Use:**
  - Validating navigation architecture across ayokoding-web content
  - Checking weight ordering follows level-based system across ALL content files
  - Verifying overview/ikhtisar presence in learning content folders
  - Auditing navigation depth (2 layers deep requirement)
  - Validating tutorial pedagogical progression (initial-setup → quick-start → beginner → intermediate → advanced)
  - Ensuring structural compliance with Hugo Content Convention - ayokoding
- **Output:** Generates `ayokoding-structure-{timestamp}-audit.md` report in `generated-reports/`
- **Works with:** `ayokoding-web-general-checker` for content quality, `ayokoding-web-facts-checker` for factual accuracy, `ayokoding-web-link-checker` for link validation
- **References:** Hugo Content Convention - ayokoding (weight system), Programming Language Content Standard (structure requirements)

### 🟪 `ayokoding-web-structure-fixer.md`

Applies validated fixes from ayokoding-web-structure-checker audit reports for ALL content files. Re-validates structural findings before applying weight corrections and ordering fixes across all markdown files. CANNOT regenerate navigation listings (use ayokoding-web-navigation-maker).

- **Primary Use:** Applying validated structural fixes from ayokoding-web-structure-checker reports after user review
- **Specialization:** Structural fix application for ALL content files (weight values across all file types, file presence, tutorial progression), confidence assessment (HIGH/MEDIUM/FALSE_POSITIVE), objective fix automation, content creation and navigation regeneration boundary awareness
- **Tools:** Read, Edit, Glob, Grep, Write, Bash
- **Expanded Scope:** Now fixes weight issues in ALL markdown files with frontmatter, including:
  - Tutorial files (initial-setup.md, quick-start.md, beginner.md, intermediate.md, advanced.md)
  - How-to guides (all files in how-to/ directories, including cookbook.md)
  - Reference files (cheat-sheet.md, glossary.md, resources.md, etc.)
  - Explanation files (best-practices.md, anti-patterns.md, etc.)
  - Topic content files (standalone content with weight fields)
  - Static pages (about-ayokoding.md, terms-and-conditions.md, etc.)
- **When to Use:**
  - After reviewing ayokoding-web-structure-checker structural audit report
  - Fixing objective structural issues (weight values across ALL files, file presence) automatically
  - Correcting tutorial progression violations (wrong pedagogical order)
  - Flagging content creation tasks (overview writing) for ayokoding-web-general-maker
  - Flagging navigation regeneration tasks for ayokoding-web-navigation-maker
  - Detecting and reporting false positives to improve checker accuracy
  - Generating comprehensive fix reports with audit trail
- **Workflow:** ayokoding-web-structure-checker (detect) → User review → ayokoding-web-structure-fixer (apply validated structural fixes)
- **Safety:** Re-validates findings before applying fixes (applies only HIGH confidence structural fixes automatically)
- **Output:** Generates `ayokoding-structure-{timestamp}-fix.md` report in `generated-reports/`
- **Limitations:** CANNOT write overview content (requires ayokoding-web-general-maker), CANNOT regenerate navigation listings (requires ayokoding-web-navigation-maker)

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

### 🟪 `ose-platform-web-content-fixer.md`

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
- **Output:** Generates `ose-platform-web-content-{timestamp}-fix.md` report in `generated-reports/`

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

### 🟦 `swe-hugo-developer.md`

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
- **Works with:** Content-maker agents for content, swe**hugo**developer for infrastructure
- **References:** Hugo Development Convention, Color Accessibility Convention

### 🟦 `social-linkedin-post-maker.md`

Expert content creator specializing in weekly LinkedIn update posts. Use when generating factual, no-hype weekly summaries of Open Sharia Enterprise development progress.

- **Primary Use:** Creating weekly LinkedIn update posts summarizing development progress (2,800-3,000 characters)
- **Specialization:** Git history analysis, commit counting, before/after comparisons, factual verification, conversational narrative-driven voice, themed reflection subsections (ON NAMING, ON TOOLING PHILOSOPHY, etc.), continuity with previous posts, length optimization (2,800-3,000 character target)
- **Tools:** Read, Write, Bash, Glob, Grep
- **Model:** Haiku (cost-effective for straightforward content creation)
- **When to Use:**
  - Generating weekly progress updates for LinkedIn
  - Analyzing git commits to extract major themes
  - Creating factual, verifiable before/after comparisons
  - Writing honest, no-hype summaries with conversational personal voice
  - Maintaining consistent narrative across posts with themed reflections
  - Connecting current week's progress to previous updates
- **Output:** Saves posts to `generated-socials/` with naming pattern `YYYY-MM-DD-linkedin-ose-update-phase-X-week-Y.md`
- **Content Structure:** CAPS headers with emojis (WHERE WE STARTED, WHERE WE ARE NOW, WHY THIS MATTERS with themed subsections, NEXT WEEK, LINKS), conversational tone showing thinking process and decision-making
- **References:** Content Quality Principles, File Naming Convention

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

### 🟪 `docs-fixer.md`

Applies validated fixes from docs**checker audit reports. Re-validates factual accuracy findings before applying changes. Use after reviewing docs**checker output.

- **Primary Use:** Applying validated fixes from docs\_\_checker audit reports after user review
- **Specialization:** Factual accuracy fix application, web-based re-validation (WebSearch/WebFetch), confidence assessment (HIGH/MEDIUM/FALSE_POSITIVE), objective vs subjective distinction, false positive detection
- **Tools:** Read, Edit, Glob, Grep, Write, Bash
- **When to Use:**
  - After reviewing docs\_\_checker audit report and deciding to apply validated fixes
  - Fixing objective factual errors (command syntax, version numbers, broken links) automatically
  - Flagging subjective improvements (narrative quality, terminology choices) for manual review
  - Detecting and reporting false positives to improve docs\_\_checker accuracy
  - Generating comprehensive fix reports with audit trail
  - Re-validating documentation findings using WebSearch and WebFetch before applying changes
- **Workflow:** docs**checker (detect) → User review → docs**fixer (apply validated fixes)
- **Safety:** Re-executes validation checks using web tools before applying fixes (applies only HIGH confidence objective fixes automatically)
- **Output:** Generates `docs-{timestamp}-fix.md` report in `generated-reports/`
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
- **Works with:** `docs-checker` for accuracy validation, `docs-link-general-checker` for link validation
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

### 🟪 `docs-link-general-checker.md`

**HYBRID AGENT** (Validator + State Manager): Validates both external and internal links in documentation files while maintaining operational cache file. Maintains a cache of verified external links in `docs/metadata/external-links-status.yaml` (the ONLY cache file) with automatic pruning and mandatory lastFullScan updates on every run. **HARD REQUIREMENT**: Cache file usage is mandatory regardless of how the agent is invoked (spawned by other agents, automated processes, or direct invocation). Outputs results in conversation only (no separate report files).

- **Primary Use:** Checking for dead links, verifying URL accessibility, validating internal references, or auditing documentation link health
- **Specialization:** External URL validation with caching, internal link verification, automatic cache pruning, mandatory lastFullScan updates, web accessibility testing, broken link detection and repair
- **Tools:** Read, Glob, Grep, WebFetch, WebSearch, Write, Edit, Bash
- **Color:** purple (hybrid: validation + cache state management)
- **Why hybrid:** Validation (checker behavior) + persistent cache file management (state management) - Write tool used ONLY for designated cache file, NOT general content modification
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
- **See Also:** [AI Agents Convention - Hybrid Agents Exception](../rules/development/agents/ex-ru-de-ag-ai-agents.md#hybrid-agents-exception) for complete rationale

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
- **Works with:** `docs-tutorial-maker` for content creation, `docs-checker` for accuracy, `docs-link-general-checker` for links
- **Note:** Complements (doesn't duplicate) docs\_\_checker (accuracy) and docs-link-general-checker (links)

### 🟪 `docs-tutorial-fixer.md`

Applies validated fixes from docs\_\_tutorial-checker audit reports. Re-validates pedagogical findings before applying changes to prevent false positives.

- **Primary Use:** Applying validated fixes from tutorial quality audits
- **Specialization:** Tutorial quality fix application, pedagogical finding re-validation, confidence level assessment (HIGH/MEDIUM/FALSE_POSITIVE), objective vs subjective issue distinction, fix report generation, false positive detection
- **Tools:** Read, Edit, Glob, Grep, Write, Bash
- **When to Use:**
  - Applying fixes from docs\_\_tutorial-checker audit reports automatically
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

### 🟪 `readme-fixer.md`

Applies validated fixes from readme\_\_checker audit reports. Re-validates README quality findings before applying changes. Conservative approach applies only objective fixes automatically.

- **Primary Use:** Applying validated fixes from readme\_\_checker audit reports after user review
- **Specialization:** Objective vs subjective fix classification, confidence assessment (HIGH/MEDIUM/FALSE_POSITIVE), automated fix application for measurable issues (paragraph length, acronym context, broken links), manual review flagging for subjective improvements (tone, engagement, benefits framing)
- **Tools:** Read, Edit, Glob, Grep, Write, Bash
- **When to Use:**
  - After reviewing readme\_\_checker audit report
  - Applying validated objective fixes automatically (paragraph breaks, acronym expansion, format corrections)
  - Detecting false positives in checker findings
  - Flagging subjective improvements for manual review
  - Generating fix audit trail for transparency
- **Workflow:** readme**checker (detect) → User review → readme**fixer (apply validated fixes)
- **Safety:** Re-executes all checks before applying fixes (applies only HIGH confidence objective fixes automatically)
- **Output:** Generates `readme-{timestamp}-fix.md` report in `generated-reports/`
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

### 🟦 `wow-workflow-maker.md`

Expert at creating and updating workflow definition files in rules/workflows/ following Workflow Pattern Convention.

- **Primary Use:** Defining multi-agent orchestration processes with clear termination criteria
- **Specialization:** Workflow structure design, agent orchestration, execution modes (Sequential/Parallel/Conditional), state management, error handling, principle alignment, agent reference validation
- **Tools:** Read, Write, Edit, Glob, Grep
- **Model:** Sonnet (complex reasoning for workflow orchestration patterns)
- **When to Use:**
  - Creating new workflow files with structured Markdown + YAML frontmatter
  - Updating existing workflow definitions
  - Documenting multi-step processes that compose multiple agents
  - Defining execution modes, termination criteria, and human checkpoints
  - Validating agent references exist in .claude/agents/
  - Tracing workflows back to foundational principles
  - Implementing common patterns (Maker-Checker-Fixer, Parallel Validation, Conditional Deployment)
- **Works with:** All agents (workflows orchestrate agents)
- **References:** Workflow Pattern Convention, Maker-Checker-Fixer Pattern, AI Agents Convention

### 🟩 `wow-workflow-checker.md`

Expert at validating workflow definition files in rules/workflows/ against Workflow Pattern Convention and quality standards.

- **Primary Use:** Validating workflow files for structure, semantics, and completeness
- **Specialization:** YAML frontmatter schema validation, agent reference verification, state reference checking, termination criteria validation, principle traceability, execution mode consistency, dependency cycle detection
- **Tools:** Read, Glob, Grep, Write, Bash
- **When to Use:**
  - Validating new workflow files before committing
  - Checking workflow structure follows Workflow Pattern Convention
  - Verifying all agent references exist in .claude/agents/
  - Validating state references ({input.x}, {stepN.outputs.y}) are correct
  - Ensuring termination criteria are well-defined
  - Checking execution modes (Sequential/Parallel/Conditional) are consistent
  - Detecting circular dependencies between steps
  - Verifying workflows trace back to principles
  - Periodic workflow quality audits
- **Output:** Generates `workflow-{timestamp}-audit.md` report in `generated-reports/`
- **Works with:** `wow-workflow-maker` for workflow creation, `wow-workflow-fixer` for applying validated fixes
- **References:** Workflow Pattern Convention, Repository Validation Methodology, Temporary Files Convention

### 🟨 `wow-workflow-fixer.md`

Applies validated fixes from wow\_\_workflow-checker audit reports. Re-validates workflow definition findings before applying changes.

- **Primary Use:** Applying validated fixes from wow\_\_workflow-checker audit reports after user review
- **Specialization:** Workflow structure fix application, confidence assessment (HIGH/MEDIUM/FALSE_POSITIVE), objective vs subjective distinction, frontmatter/agent reference/state reference corrections
- **Tools:** Read, Edit, Glob, Grep, Write, Bash
- **When to Use:**
  - After reviewing wow\_\_workflow-checker audit report
  - Fixing objective structural issues (missing fields, invalid syntax, broken references) automatically
  - Flagging subjective improvements (execution mode choices, agent selection) for manual review
  - Detecting and reporting false positives to improve checker accuracy
  - Generating comprehensive fix reports with audit trail
- **Workflow:** wow**workflow-checker (validate) → User review → wow**workflow-fixer (apply validated structural fixes)
- **Safety:** Re-validates findings before applying fixes (applies only HIGH confidence objective fixes automatically)
- **Output:** Generates `workflow-{timestamp}-fix.md` report in `generated-reports/`
- **Note:** Many workflow "issues" are design decisions - this agent applies only objective structural errors (verifiable violations) and flags subjective improvements for human judgment
- **Works with:** `wow-workflow-checker` for audit report generation, `wow-workflow-maker` for workflow creation

### 🟩 `wow-rules-checker.md`

Expert at validating repository-wide consistency, CLAUDE.md maintenance (size limits, duplication), and rules governance across all governance layers. Detects contradictions, inaccuracies, inconsistencies, traceability violations, and layer coherence issues. Generates detailed audit reports in generated-reports/.

- **Primary Use:** Checking for inconsistencies, contradictions, or verifying compliance
- **Specialization:** Cross-file validation, duplication detection, convention enforcement, emoji consistency, CLAUDE.md size monitoring, rules governance validation (contradictions, inaccuracies, inconsistencies, traceability violations, layer coherence across vision/, principles/, conventions/, development/, workflows/), persistent audit reporting
- **Tools:** Read, Glob, Grep, Write
- **When to Use:**
  - After making changes to conventions or CLAUDE.md
  - Validating rules governance (contradictions, inaccuracies, inconsistencies, traceability violations, layer coherence)
  - Periodic audits of repository consistency
  - Validating that all files follow documented standards (including emoji usage)
  - Detecting contradictions or outdated references
  - Identifying duplicate content that could be consolidated
  - Historical tracking of repository consistency over time
- **Important:** READ-ONLY agent - does not apply fixes. Use `wow-rules-fixer` to apply validated fixes after reviewing audit report.

### 🟪 `wow-rules-fixer.md`

Applies validated fixes from wow\_\_rules-checker audit reports including CLAUDE.md issues and rules governance problems (contradictions, inaccuracies, inconsistencies, traceability violations, layer coherence). Re-validates findings before applying changes. Uses bash tools for .claude/ files, Edit tool for docs/explanation/ files.

- **Specialization:** Fix validation, confidence assessment (HIGH/MEDIUM/FALSE_POSITIVE), automated fix application with safety checks using bash for .claude/ files and Edit for docs/ files, rules governance fixes (contradictions, inaccuracies, inconsistencies, traceability violations, layer coherence)
- **Tools:** Read, Glob, Grep, Write, Bash
- **When to Use:**
  - After reviewing wow\_\_rules-checker audit report
  - Applying validated fixes automatically with re-validation
  - Detecting false positives in checker findings
  - Generating fix audit trail for transparency
- **Workflow:** wow**rules-checker (detect) → User review → wow**rules-fixer (apply validated fixes)
- **Safety:** Re-executes all checks before applying fixes (applies only HIGH confidence fixes automatically)
- **Output:** Generates `wow-rules-audit-{timestamp}-fix.md` report in `generated-reports/`

### 🟨 `wow-rules-maker.md`

Expert at making rule and convention changes effective across CLAUDE.md, convention docs, agents, and indices. Responsible for maintaining CLAUDE.md size limits. Uses bash commands for file creation and editing.

- **Primary Use:** Adding/modifying rules, conventions, or standards affecting multiple files
- **Specialization:** Systematic propagation, cascade updates, consistency maintenance, CLAUDE.md size management (40k hard limit, 30k target), bash-based file operations
- **Tools:** Read, Glob, Grep, Write, Bash
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

- **Primary Use:** Executing plans created by the plan\_\_maker agent
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
- **Tools:** Read, Glob, Grep, Write, Bash
- **When to Use:**
  - After plan\_\_executor completes all implementation tasks
  - Validating implementation meets all requirements from requirements.md
  - Verifying technical documentation alignment (tech-docs.md)
  - Running comprehensive code quality checks (tests, lints, builds)
  - Performing end-to-end integration testing
  - Providing independent quality gate with fresh eyes
  - Generating detailed validation reports with specific findings
  - Iterating with plan\_\_executor to fix issues until validation passes
- **Works with:** `plan-executor` for implementation

### 🟩 `plan-checker.md`

Expert at validating plans are ready for implementation by verifying completeness, checking codebase alignment, and validating technical accuracy using web verification.

- **Primary Use:** Pre-implementation validation of project plans
- **Specialization:** Plan completeness verification, codebase alignment checking, external verification via web, technical accuracy validation
- **Tools:** Read, Glob, Grep, WebSearch, WebFetch, Write, Bash
- **When to Use:**
  - After plan\_\_maker creates a plan, before implementation begins
  - Validating plan structure and completeness (requirements, tech-docs, delivery)
  - Verifying codebase assumptions are accurate (check package.json, directory structure)
  - Checking technology choices are current and maintained (WebSearch verification)
  - Validating documentation URLs are accessible (WebFetch)
  - Ensuring requirements have testable acceptance criteria
  - Identifying contradictions or missing information in plan
  - Preventing implementation blockers by catching plan issues early
- **Output:** Generates `plan-{timestamp}-validation.md` report in `generated-reports/`
- **Works with:** `plan-fixer` for applying validated fixes, `plan-maker` for plan creation

### 🟪 `plan-fixer.md`

Applies validated fixes from plan\_\_checker audit reports. Re-validates plan completeness and accuracy findings before applying changes. Distinguishes structural/format issues from strategic decisions.

- **Primary Use:** Applying validated fixes from plan\_\_checker validation reports after user review
- **Specialization:** Structural vs strategic issue classification, confidence assessment (HIGH/MEDIUM/FALSE_POSITIVE), automated fix application for objective issues (missing sections, broken links, format errors), manual review flagging for strategic decisions (scope, architecture, technology choices, timelines)
- **Tools:** Read, Edit, Glob, Grep, Write, Bash
- **When to Use:**
  - After reviewing plan\_\_checker validation report
  - Applying validated structural/format fixes automatically (missing sections, broken links, format violations)
  - Detecting false positives in checker findings
  - Flagging strategic/architectural decisions for manual review
  - Generating fix audit trail for transparency
- **Workflow:** plan**checker (validate) → User review → plan**fixer (apply validated structural fixes)
- **Safety:** Re-executes all checks before applying fixes (applies only HIGH confidence objective fixes automatically)
- **Output:** Generates `plan-{timestamp}-fix.md` report in `generated-reports/`
- **Note:** Plans contain strategic decisions - this agent applies only structural/format fixes (objective) and flags strategic choices (scope, architecture, timelines) for human judgment
- **Works with:** `plan-checker` for validation report generation, `plan-maker` for plan creation

## 🔗 Hierarchy Examples: Principles to Agents

Understanding which agents enforce which conventions helps trace decisions from principles to implementation.

### Example 1: Color Accessibility

**Vision**: [Democratize Shariah-compliant enterprise](../rules/vision/open-sharia-enterprise.md) - accessible to everyone

**Principle**: [Accessibility First](../rules/principles/content/ex-ru-pr-co-accessibility-first.md)

**Convention**: [Color Accessibility Convention](../rules/conventions/formatting/ex-ru-co-fo-color-accessibility.md)

**Enforcing Agents**:

- **docs\_\_maker**: Validates Mermaid diagram colors against accessible palette
- **docs\_\_tutorial-maker**: Ensures tutorial diagrams use color-blind friendly colors
- **swe**hugo**developer**: Implements accessible color palette in theme development
- **ayokoding-web-general-maker**, **ose-platform-web-content-maker**: Apply accessible colors in Hugo content

### Example 2: Explicit Configuration

**Vision**: [Transparent, verifiable Islamic finance](../rules/vision/open-sharia-enterprise.md)

**Principle**: [Explicit Over Implicit](../rules/principles/software-engineering/ex-ru-pr-se-explicit-over-implicit.md)

**Practice**: [AI Agents Convention](../rules/development/agents/ex-ru-de-ag-ai-agents.md)

**Enforcing Agents**:

- **agent\_\_maker**: Validates new agents have explicit `tools`, `model`, and `color` frontmatter fields
- **wow\_\_rules-checker**: Audits all agents for missing or incomplete frontmatter
- **wow\_\_rules-fixer**: Adds missing frontmatter fields when validated by user

### Example 3: Automation

**Vision**: [Scale Islamic enterprise knowledge globally](../rules/vision/open-sharia-enterprise.md)

**Principle**: [Automation Over Manual](../rules/principles/software-engineering/ex-ru-pr-se-automation-over-manual.md)

**Practice**: [Code Quality Convention](../rules/development/quality/code.md), [Maker-Checker-Fixer Pattern](../rules/development/pattern/ex-ru-de-pa-maker-checker-fixer.md)

**Enforcing Agents** (Checker family):

- **docs\_\_checker**: Automated factual accuracy validation
- **docs\_\_tutorial-checker**: Automated pedagogical quality validation
- **wow\_\_rules-checker**: Automated consistency validation
- **ayokoding-web-general-checker**, **ose-platform-web-content-checker**: Automated Hugo content validation
- **readme\_\_checker**: Automated README quality validation

**Enforcing Agents** (Fixer family):

- **docs\_\_fixer**, **docs\_\_tutorial-fixer**, **wow\_\_rules-fixer**, **ayokoding-web-general-fixer**, **ose-platform-web-content-fixer**, **readme\_\_fixer**, **plan\_\_fixer**: Automated fix application with confidence levels

## 🔄 Agent Workflow

The agents work together in complementary workflows:

### 📝 Content Quality Workflows (Maker-Checker-Fixer Pattern)

The repository uses a three-stage workflow for content creation and quality assurance:

**Pattern: Maker → Checker → (User Review) → Fixer**

```
1. Creation Stage (Maker Agents)
   └─> Create content following conventions
        └─> Makers: docs-maker, docs-tutorial-maker, readme-maker,
                    ayokoding-web-general-maker, ose-platform-web-content-maker

2. Detection Stage (Checker Agents)
   └─> Validate content quality and convention compliance
        └─> Generate audit report in generated-reports/
        └─> Checkers: docs-checker, docs-tutorial-checker, readme-checker,
                      ayokoding-web-general-checker, ose-platform-web-content-checker,
                      wow-rules-checker

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
                    ayokoding-web-general-fixer, ose-platform-web-content-fixer,
                    wow-rules-fixer, plan-fixer

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

**See Also:** [Fixer Confidence Levels Convention](../rules/development/quality/ex-ru-de-qu-fixer-confidence-levels.md) for universal confidence assessment criteria.

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
   └─> Use wow-rules-maker to make changes effective across files
        └─> Ensures consistency in CLAUDE.md, conventions, agents, indices

2. Validate Changes
   └─> Use wow-rules-checker to verify consistency
        └─> Detects inconsistencies, contradictions, duplications
        └─> Generates audit report in generated-reports/

3. Review Audit Report
   └─> Check findings and validate recommendations
        └─> Identify which fixes should be applied

4. Apply Validated Fixes
   └─> Use wow-rules-fixer to apply fixes automatically
        └─> Re-validates findings, applies HIGH confidence fixes
        └─> Skips false positives, reports MEDIUM confidence items
        └─> Generates fix report for audit trail

5. Verify Fixes (if fixes were applied)
   └─> Use wow-rules-checker to re-validate
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
   └─> Use docs-link-general-checker to audit link health
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
- **After plan\_\_executor completes:** Use `plan-execution-checker` for independent final validation
- **Full planning workflow:** plan**maker → plan**checker → (fix if needed) → plan**executor → plan**execution-checker
- **Quality assurance workflow:** Maker-checker at both stages (planning and implementation)
- **After adding new conventions:** Use `wow-rules-maker` → `wow-rules-checker` → `wow-rules-fixer` (if issues found)
- **CLAUDE.md maintenance:** Keep under 30k characters (target), never exceed 40k (hard limit). Brief summaries only, link to detailed docs. Use `wow-rules-maker` to check size when adding rules
- **Agent file size limits:** Three tiers - Simple (<800 lines), Standard (<1,200 lines), Complex (<1,800 lines). Link to convention docs instead of duplicating content. See [AI Agents Convention](../rules/development/agents/ex-ru-de-ag-ai-agents.md) for complete size guidelines
- **Before major releases:** Run `wow-rules-checker` for full audit and `docs-link-general-checker` to verify all links
- **When creating tutorials:** Use `docs-tutorial-maker` for learning-oriented content with narrative flow and diagrams
- **When creating other documentation:** Use `docs-maker` for how-to guides, reference, or explanations
- **After creating tutorials:** Use `docs-tutorial-checker` to validate pedagogical quality and completeness
- **When modifying CLAUDE.md:** Use `wow-rules-maker` to cascade changes
- **During plan implementation:** Let `plan-executor` update delivery.md - it maintains detailed notes
- **When managing files in docs/:** Use `docs-file-manager` to handle prefixes, links, and indices automatically (rename, move, or delete)
- **After using docs\_\_file-manager:** Always run `docs-link-general-checker` to verify all links are valid
- **Monthly or before releases:** Run `docs-link-general-checker` to ensure all links are valid, then `docs-checker` to verify technical accuracy
- **After major documentation updates:** Use `docs-link-general-checker` to verify link integrity, then `docs-checker` to validate content accuracy
- **After dependency updates:** Run `docs-checker` to ensure documentation matches new versions
- **Before releasing technical docs:** Use `docs-checker` to validate all technical claims and code examples
- **When reviewing contributions:** Use `docs-checker` to verify factual accuracy of new documentation
- **Documentation accuracy workflow:** docs**checker → (review validation report) → docs**fixer (apply objective fixes) → re-run docs\_\_checker
- **Documentation validation with automated fixes:** Use `docs-checker` to generate validation report, then `docs-fixer` to apply validated objective fixes (command syntax, version numbers, broken links) while flagging subjective improvements (narrative, terminology) for manual review
- **When creating/updating README:** Use `readme-maker` for content, then `readme-checker` for validation
- **README quality workflow:** readme**maker → readme**checker → (review audit) → readme\_\_fixer (apply objective fixes) → commit
- **README validation with automated fixes:** Use `readme-checker` to generate audit report, then `readme-fixer` to apply validated objective fixes (paragraph breaks, acronym context, format corrections) while flagging subjective improvements for manual review

## 📚 Resources

- [AI Agents Convention](../rules/development/agents/ex-ru-de-ag-ai-agents.md) - Complete agent specification and standards
- [CLAUDE.md](../CLAUDE.md) - Project guidance for all agents
- [Documentation Conventions](../rules/conventions/README.md) - File naming, linking, and Diátaxis framework
- [Plans Organization](../plans/README.md) - Planning document structure and conventions

## 🔄 Agent Lifecycle

Agents follow a structured lifecycle from creation to deprecation:

### Creation

1. Use `agent-maker` to scaffold new agent with proper structure
2. Validate with `wow-rules-checker` to ensure convention compliance
3. Update CLAUDE.md if agent affects project guidance (use `wow-rules-maker`)
4. Test agent behavior matches specification

### Updates

1. Edit agent file with required changes
2. Update `updated` field in frontmatter (YYYY-MM-DD format)
3. Validate changes with `wow-rules-checker`
4. Apply fixes if needed using `wow-rules-fixer`
5. Update CLAUDE.md and convention docs if agent behavior changed

### Maintenance

- Review agent periodically (quarterly or when conventions change)
- Update references to conventions/practices if they move or change
- Verify tool permissions still match agent responsibilities
- Check agent size within tier limits (Simple: <800, Standard: <1,200, Complex: <1,800 lines)

### Deprecation

1. Add deprecation notice at top of agent file
2. Document replacement agent or workflow (if applicable)
3. Update CLAUDE.md to remove or replace deprecated agent references
4. Keep deprecated agent for 6 months before deletion (migration period)
5. Update all references in workflows, conventions, and documentation

## 🆕 Adding New Agents

When creating new agents:

1. Use `agent-maker` to automate creation with proper structure, size verification, and README updates
2. Follow the [AI Agents Convention](../rules/development/agents/ex-ru-de-ag-ai-agents.md) for all standards
3. Verify agent size within tier limits (Simple: <800, Standard: <1,200, Complex: <1,800 lines)
4. Use `wow-rules-maker` to propagate references to CLAUDE.md and other files
5. Use `wow-rules-checker` to validate the new agent follows all conventions
6. Use `wow-rules-fixer` to apply any validated fixes from the audit report
7. Update CLAUDE.md if the agent should be mentioned in project guidance

---

**Note:** This README follows the naming exception for README.md files documented in the [File Naming Convention](../rules/conventions/meta/ex-ru-co-me-file-naming.md).
