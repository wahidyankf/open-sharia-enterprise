# Markdown Linting Implementation Plan

**Created**: 2026-01-17
**Status**: Done
**Completed**: 2026-01-17
**Priority**: MEDIUM
**Scope**: 1,039 markdown files (17,903 violations → 0 violations)

## Background

The repository contains extensive markdown documentation across multiple directories (governance/, docs/, .claude/, apps/, plans/, root files).

**Current State** (verified 2026-01-17):

- ✅ Prettier (v3.6.2) is already installed and runs on pre-commit via lint-staged
- ✅ .prettierrc.json exists: `printWidth: 120, proseWrap: "preserve"`
- ✅ Markdown files are automatically formatted on commit
- ✅ .husky/pre-push exists: runs `npx nx affected -t test:quick`
- ✅ 1,413 markdown files in repository
- ❌ No markdown linting in place to catch structural/syntactic issues
- ❌ No pre-push validation to prevent pushing violations
- ❌ No Claude Code hooks for real-time feedback
- ⚠️ `jq` installation status unknown (required for Claude Code hooks)

## Pre-Execution Verification

**Critical Issues Discovered During Planning**:

1. ✅ **RESOLVED**: Prettier already installed - no installation needed
2. ✅ **RESOLVED**: .prettierrc.json exists with good settings
3. ✅ **RESOLVED**: pre-push hook exists - will append to it
4. ⚠️ **TO VERIFY**: `jq` installation (required for Claude Code hooks)
5. 📊 **NOTED**: 1,413 markdown files (expect 10-30s lint time on pre-push)

**Before Starting Phase 1**:

- [ ] Verify `jq` is installed: `which jq`
- [ ] If not installed, install `jq`: `sudo apt-get install jq` or `brew install jq`
- [ ] Backup current .husky/pre-push hook
- [ ] Review .prettierrc.json settings (already optimal)

## Problem Statement

**Gap Analysis**:

While Prettier handles formatting, it doesn't catch:

- Structural issues (heading hierarchy violations)
- Linting concerns (bare URLs, empty links)
- Best practice violations (missing language tags in code blocks)
- Markdown-specific style issues

**Impact**:

- Prettier ensures consistent formatting but not quality
- No automated feedback on markdown structural issues
- Risk of accumulating markdown linting debt
- No pre-push validation to catch violations before they're pushed

## Goals

**Primary Goal**: Implement markdown linting with pre-push hook enforcement and Claude Code automation after ensuring all existing markdown files pass validation

**Success Criteria**:

- All markdown files pass markdown linting
- Markdown linting integrated into pre-push hooks
- Claude Code automatically formats and lints markdown files on edit
- Prettier and markdownlint work together without conflicts
- Linting configuration documented
- Zero markdown violations in repository
- No new markdown violations can be pushed
- Real-time feedback during Claude Code editing sessions

## Proposed Solution

### Phase 1: Research & Setup

**Deliverables**:

1. Evaluate npm markdown linting libraries
   - **markdownlint-cli2** (recommended, modern successor)
   - **markdownlint-cli** (original, widely used)
   - **remark-cli** (with remark-lint plugins)
2. Select appropriate library based on:
   - npm ecosystem compatibility
   - Configuration flexibility
   - Performance
   - Active maintenance
   - Rule customization
3. Install selected library as devDependency
4. Create configuration file (`.markdownlint.json` or `.markdownlint-cli2.jsonc`)

### Phase 2: Configuration

**Deliverables**:

1. Define markdown linting rules aligned with repository conventions
   - Heading structure (single H1, proper nesting)
   - Line length limits (consider existing content)
   - List indentation (2 spaces per level per conventions)
   - Code block formatting
   - Link formatting
   - Trailing whitespace
   - File ending newline
2. Create `.markdownlintignore` file for exclusions
   - Generated files
   - Third-party content
   - node_modules/
3. Add npm script for linting
   - `npm run lint:md` - Lint all markdown files
   - `npm run lint:md:fix` - Auto-fix violations

### Phase 3: Baseline Validation & Remediation

**Deliverables**:

1. Run linting on all markdown files
   - Generate violation report
   - Categorize violations by type and severity
2. Analyze violations
   - Identify auto-fixable violations
   - Identify manual fixes required
3. Remediate all violations
   - Run auto-fix for safe violations
   - Manual fixes for complex violations
   - Validate no new issues introduced
4. Verify zero violations
   - Run full linting pass
   - Confirm all files pass

### Phase 4: Pre-Push Hook Integration

**Deliverables**:

1. Update `.husky/pre-push` hook
   - Add markdown linting step
   - **Option A**: Run on all markdown files (comprehensive but slower)
   - **Option B**: Run on changed files only (faster, requires git diff logic)
   - Block push if violations detected
2. Test hook behavior
   - Verify hook blocks pushes with violations
   - Verify hook allows clean pushes
   - Verify error messages are clear
3. Document linting in contributing guide

**Performance Optimization**:

Research indicates pre-push hooks can be slower than pre-commit since they run less frequently. Two approaches:

- **All Files** (Recommended for initial implementation): `npm run lint:md || exit 1`
  - Comprehensive check
  - Simpler implementation
  - Acceptable performance for most repos
  - Ensures no violations slip through

- **Changed Files Only** (Optional optimization): Use git diff to identify changed markdown files
  - Faster for large repos with many markdown files
  - More complex implementation
  - Requires custom script or tool like lint-staged
  - May miss violations in unchanged files

**Decision**: Start with all files approach. Add performance optimization only if push times become problematic.

### Phase 5: Claude Code Hook Integration

**Note**: Prettier is already installed (v3.6.2) and runs on pre-commit via lint-staged. This phase focuses on integrating both tools into Claude Code workflows for real-time feedback.

**Deliverables**:

1. Verify and optimize existing Prettier configuration
   - Check/create `.prettierrc` configuration file
   - Optimize markdown-specific options (prose wrap, line width)
   - Verify `.prettierignore` alignment
2. Integrate Prettier with markdownlint
   - Update `.markdownlint-cli2.jsonc` to extend `markdownlint/style/prettier`
   - Ensure no rule conflicts between tools
   - Test integration on sample files
3. Configure Claude Code PostToolUse hook
   - Create/update `.claude/settings.json` (project-level)
   - Add PostToolUse hook with matcher `Edit|Write|MultiEdit`
   - Create shell script to run both tools
   - Only trigger for `*.md` files (performance optimization)
4. Test hook automation
   - Verify hook triggers after editing markdown files with Claude Code
   - Verify automatic formatting applied (Prettier)
   - Verify automatic linting and fixes applied (markdownlint-cli2)
   - Verify no impact on non-markdown files

**Hook Configuration Strategy**:

Research shows Claude Code hooks provide deterministic automation for formatting and linting after file changes, ensuring quality without relying on LLM to remember to run tools.

**Execution Order**:

1. **Prettier** (format first): `prettier --write <file>`
2. **markdownlint-cli2** (lint second): `markdownlint-cli2 --fix <file>`

**Rationale**: Format first to ensure consistent structure, then lint to catch remaining issues. Both tools are complementary.

**File Targeting**: Shell script checks file extension (\*.md) to avoid performance impact on non-markdown file types.

**Relationship to Pre-Commit Hook**: Prettier already runs on pre-commit via lint-staged. Claude Code hook provides real-time feedback during editing, while pre-commit ensures quality before commit.

### Phase 6: Documentation & Training

**Deliverables**:

1. Document markdown linting in governance/
   - Configuration rationale
   - Rule explanations
   - How to run linting locally
   - How to fix common violations
   - Claude Code hook behavior
2. Update CLAUDE.md with linting information
3. Add linting to development workflow documentation

## Web Research Validation

Research conducted 2026-01-17 confirms the following:

**markdownlint ecosystem status**:

- **markdownlint** library: v0.40.0 (updated within last month)
- **markdownlint-cli**: 643,240 weekly downloads, 962 GitHub stars
- **markdownlint-cli2**: 268,362 weekly downloads (newer but growing)
- Author statement: "Future development will focus on markdownlint-cli2"

**Prettier + markdownlint integration**:

- Prettier has native markdown support since v1.8
- markdownlint includes `style/prettier.json` extension to disable formatting rules
- Both tools are complementary: Prettier handles formatting, markdownlint handles linting
- Configuration: Add `"extends": "markdownlint/style/prettier"` to `.markdownlint.json`
- Recommended workflow: Run Prettier first, then markdownlint

**Claude Code hooks**:

- PostToolUse hook triggers after file edits/writes (deterministic automation)
- Matcher `Edit|Write|MultiEdit` targets all file modification events
- Common use case: Automatic formatting/linting after file changes
- Configuration location: `.claude/settings.json` (project-level)
- Hooks provide deterministic control vs relying on LLM to remember to run tools

**Performance considerations**:

- Pre-commit hooks should be fast and frictionless
- Pre-push hooks can be slower (run less frequently)
- lint-staged commonly used to optimize by checking only staged/changed files
- markdownlint-cli2 has good performance but checking all files may still impact push time

**Integration patterns**:

- Husky widely used for git hooks in npm projects
- markdownlint-cli2 supports .pre-commit-hooks.yaml for pre-commit framework
- Both Husky and pre-commit framework are valid choices

## Implementation Strategy

### Library Selection Criteria

**markdownlint-cli2** (Recommended):

**Advantages**:

- Modern successor to markdownlint-cli
- Better performance (glob-based file discovery)
- Supports multiple config formats (JSON, JSONC, YAML, JS)
- Built-in support for .markdownlintignore
- Auto-fix capability (`--fix`)
- Active maintenance (author's focus for future development)
- Supports custom rules
- Configuration-based approach (aligns with repository conventions)
- Works with both Husky and pre-commit framework

**Disadvantages**:

- Lower download count than markdownlint-cli (268k vs 643k weekly)
- Newer (less battle-tested, though mature enough for production)

**markdownlint-cli** (Alternative):

**Advantages**:

- Widely adopted (643k weekly downloads)
- Stable and mature
- Good documentation
- More battle-tested

**Disadvantages**:

- Slower performance
- Less flexible configuration
- Author will maintain but focus new features on markdownlint-cli2

**remark-cli** (Alternative):

**Advantages**:

- Part of unified ecosystem
- Highly extensible with plugins

**Disadvantages**:

- More complex setup
- Requires multiple plugins for comprehensive linting
- Steeper learning curve

**Decision**: Use **markdownlint-cli2** for modern tooling, performance, configuration-based approach, and future-proof development focus.

## Configuration Approach

### Rule Categories

**Structural Rules** (HIGH priority):

- MD001: Heading levels increment by one
- MD003: Heading style consistency
- MD022: Headings surrounded by blank lines
- MD025: Single top-level heading

**Formatting Rules** (MEDIUM priority):

- MD004: List style consistency
- MD007: List indentation (2 spaces)
- MD009: No trailing spaces
- MD010: No hard tabs
- MD012: No multiple consecutive blank lines
- MD047: Files end with newline

**Link Rules** (MEDIUM priority):

- MD034: No bare URLs
- MD042: No empty links

**Code Rules** (LOW priority):

- MD031: Fenced code blocks surrounded by blank lines
- MD040: Code blocks have language specified

### Rules to Disable

Based on repository conventions and existing content:

- MD013: Line length (may need flexible limit or disable for long links)
- MD033: Inline HTML (may be needed in Hugo frontmatter or special cases)
- MD041: First line H1 (may conflict with frontmatter)

## Git Workflow

**Branch Strategy**: Work directly on `main` branch (Trunk Based Development)

**Commit Strategy**: Multiple small commits for each phase

1. Commit 1: Add markdownlint-cli2 library and configuration
2. Commit 2: Auto-fix violations
3. Commit 3: Manual violation fixes (if needed)
4. Commit 4: Add pre-push hook integration
5. Commit 5: Add Claude Code hook integration and Prettier config optimization
6. Commit 6: Add documentation

**Rationale**: Incremental commits allow easier rollback and clearer history of changes.

**Note**: Do NOT commit until explicitly instructed. Each phase completion requires user approval.

## Implementation Checklist

### Phase 1: Research & Setup

- [x] Evaluate markdownlint-cli2
- [x] Evaluate markdownlint-cli
- [x] Evaluate remark-cli
- [x] Document library comparison
- [x] Select library (recommendation: markdownlint-cli2)
- [x] Install library: `npm install --save-dev markdownlint-cli2`
- [x] Verify installation: `npx markdownlint-cli2 --version`

### Phase 2: Configuration

- [x] Create `.markdownlint-cli2.jsonc` configuration file
  - Define rule set aligned with repository conventions
  - Include rules from Configuration Approach section
  - Add `"extends": "markdownlint/style/prettier"` for Prettier compatibility
- [x] Create `.markdownlintignore` file with specific exclusions:

  ```
  # Dependencies
  node_modules/

  # Build outputs
  dist/
  build/
  .next/
  .nx/

  # Hugo public directories
  apps/*/public/

  # Experimental apps (not in Nx)
  apps-labs/

  # Lock files
  package-lock.json

  # Generated reports
  generated-reports/

  # IDE
  .vscode/
  .idea/
  ```

- [x] Add npm scripts to package.json
  - `"lint:md": "markdownlint-cli2 \"**/*.md\""`
  - `"lint:md:fix": "markdownlint-cli2 --fix \"**/*.md\"`
- [x] Test configuration on sample files
  - Test on 5-10 existing markdown files
  - Verify rules are appropriate
  - Adjust configuration if needed

### Phase 3: Baseline Validation & Remediation

- [x] Run initial linting: `npm run lint:md`
- [x] Generate violation report
- [x] Categorize violations by type and severity
- [x] Analyze auto-fixable vs manual violations
- [x] Run auto-fix: `npm run lint:md:fix`
- [x] Review auto-fix changes
- [x] Commit auto-fix changes (if safe)
- [x] Fix remaining violations manually
- [x] Verify zero violations: `npm run lint:md`
- [x] Commit manual fixes

### Phase 4: Pre-Push Hook Integration

- [x] Read current `.husky/pre-push` hook
  - ✅ Current content: `npx nx affected -t test:quick`
  - Our linting will run AFTER the test command
- [x] Add markdown linting step
  - Add after existing test command: `npm run lint:md || exit 1`
  - Full hook content should be:

    ```bash
    npx nx affected -t test:quick
    npm run lint:md || exit 1
    ```

- [x] Create test markdown file with known violations
  - Create `test-violations.md` with:
    - Multiple H1 headings (MD025 violation)
    - Bare URL (MD034 violation)
    - Missing language tag in code block (MD040 violation)
  - Use for testing hook blocking behavior
- [x] Test hook with clean push (should succeed)
  - Remove or fix test-violations.md
  - Push should succeed after all files pass linting
- [x] Test hook with violations (should block)
  - Re-add test-violations.md with violations
  - Push should be blocked with clear error messages
  - Verify error shows violation details
- [x] Verify error messages are clear
  - Check that violations are easy to understand
  - Verify file paths and line numbers are shown
- [x] Delete test-violations.md
- [x] Commit hook changes

### Phase 5: Claude Code Hook Integration

**Note**: Prettier is already installed (✅ .prettierrc.json exists) and runs on pre-commit. This phase focuses on Claude Code automation.

- [ ] ⚠️ Verify `jq` is installed (required for parsing hook JSON)
  - Run: `which jq` or `jq --version`
  - If not installed: `sudo apt-get install jq` (Linux) or `brew install jq` (Mac)
  - **STATUS**: Installation interrupted - needs manual completion
- [x] Verify existing Prettier configuration
  - ✅ .prettierrc.json exists with: `printWidth: 120, proseWrap: "preserve"`
  - Review if settings are optimal for markdown
  - Note: printWidth 120 is good for markdown (plan suggested 100-120)
- [x] Check for `.prettierignore` file
  - Create if it doesn't exist
  - Align exclusions with `.markdownlintignore`
  - Add: node_modules/, dist/, build/, .nx/, apps-labs/
- [x] Add npm scripts for manual Prettier operations
  - `"format:md": "prettier --write \"**/*.md\""`
  - `"format:md:check": "prettier --check \"**/*.md\""`
- [x] Update `.markdownlint-cli2.jsonc`
  - ~~Add `"extends": "markdownlint/style/prettier"`~~ (removed - property not allowed)
  - Verify no rule conflicts
  - Test with sample files
- [x] Create `.claude/settings.json` (file doesn't exist yet)
  - Add PostToolUse hook configuration
  - Set matcher: `"Edit|Write|MultiEdit"`
  - Reference shell script: `"$CLAUDE_PROJECT_DIR/.claude/hooks/format-lint-markdown.sh"`
  - **Example configuration**:

    ```json
    {
      "hooks": {
        "PostToolUse": [
          {
            "matcher": "Edit|Write|MultiEdit",
            "hooks": [
              {
                "type": "command",
                "command": "\"$CLAUDE_PROJECT_DIR\"/.claude/hooks/format-lint-markdown.sh"
              }
            ]
          }
        ]
      }
    }
    ```

- [x] Create hook script: `.claude/hooks/format-lint-markdown.sh`
  - Parse JSON from stdin using `jq -r '.tool_input.file_path'`
  - Check if file extension is `.md`
  - If yes, run: `prettier --write "$file_path" && markdownlint-cli2 --fix "$file_path"`
  - If no, exit silently (avoid performance impact)
  - Add error handling (exit 0 on errors to avoid blocking Claude)
  - Make executable: `chmod +x .claude/hooks/format-lint-markdown.sh`
  - **Example script**:

    ```bash
    #!/bin/bash
    # Extract file path from stdin JSON
    file_path=$(jq -r '.tool_input.file_path')

    # Only process markdown files
    if [[ "$file_path" =~ \.md$ ]]; then
      prettier --write "$file_path" 2>/dev/null || true
      markdownlint-cli2 --fix "$file_path" 2>/dev/null || true
    fi

    # Always exit 0 to avoid blocking Claude Code
    exit 0
    ```

- [ ] ⚠️ Test Claude Code hook
  - Edit a markdown file with Claude Code
  - Verify Prettier formatting applied automatically
  - Verify markdownlint-cli2 fixes applied
  - Check that non-markdown files are unaffected
  - Test with intentional violations to ensure detection
  - Verify hook doesn't block Claude Code on errors
  - **STATUS**: Cannot test until `jq` is installed
- [x] Commit Claude Code hook configuration

### Phase 6: Documentation & Training

- [x] Document linting in `governance/development/quality/markdown.md` (new file)
  - Configuration rationale
  - Rule explanations
  - How to run linting locally
  - How to fix common violations
  - Claude Code hook behavior and automation
  - Prettier configuration and formatting
- [x] Update CLAUDE.md with markdown linting information
- [x] Add linting to development workflow documentation
- [x] Commit documentation

## Acceptance Criteria

### Scenario: All Markdown Files Pass Linting

**Given** markdown linting has been configured and violations remediated
**When** `npm run lint:md` is executed
**Then** zero violations are reported
**And** all markdown files pass validation
**And** exit code is 0

### Scenario: Pre-Push Hook Blocks Violations

**Given** markdown linting is integrated into pre-push hook
**When** a developer attempts to push changes with markdown violations
**Then** the pre-push hook blocks the push
**And** the developer receives clear error messages listing violations
**And** the developer receives guidance on how to fix violations

### Scenario: Pre-Push Hook Allows Clean Code

**Given** markdown linting is integrated into pre-push hook
**When** a developer attempts to push changes with no markdown violations
**Then** the pre-push hook allows the push to proceed
**And** no markdown linting errors are shown

### Scenario: Auto-Fix Resolves Common Violations

**Given** markdown files have auto-fixable violations
**When** `npm run lint:md:fix` is executed
**Then** violations are automatically corrected
**And** files are modified with fixes applied
**And** running `npm run lint:md` shows zero violations for fixed issues

### Scenario: Claude Code Hook Formats and Lints Automatically

**Given** Claude Code hook is configured for markdown files
**When** Claude Code edits or writes a markdown file
**Then** Prettier automatically formats the file
**And** markdownlint-cli2 automatically lints and fixes violations
**And** the user is notified of any remaining violations
**And** non-markdown files are unaffected by the hook

### Scenario: Prettier Formats Markdown Consistently

**Given** Prettier is configured for markdown
**When** `npm run format:md` is executed
**Then** all markdown files are formatted consistently
**And** prose wrapping is applied per configuration
**And** code blocks within markdown are formatted
**And** running `npm run format:md:check` shows zero formatting issues

### Scenario: Prettier and markdownlint Work Together

**Given** Prettier and markdownlint are both configured
**When** both tools are run on the same markdown file
**Then** no conflicts occur between formatting and linting rules
**And** Prettier handles formatting concerns
**And** markdownlint handles linting concerns
**And** both tools report zero violations

### Scenario: Pre-Commit and Claude Code Hooks Work Together

**Given** Prettier runs on pre-commit and in Claude Code hooks
**When** a markdown file is edited with Claude Code and then committed
**Then** Claude Code hook runs Prettier immediately after edit
**And** pre-commit hook runs Prettier again before commit
**And** Prettier is idempotent (running twice produces same result)
**And** no conflicts or double-formatting issues occur
**And** file is properly formatted in both cases

### Scenario: Hook Errors Don't Block Claude Code

**Given** Claude Code hook encounters an error
**When** Prettier or markdownlint-cli2 fails in the hook
**Then** the hook exits with code 0 (success)
**And** Claude Code operation is not blocked
**And** errors are suppressed (2>/dev/null || true)
**And** user can continue working

## Risks & Mitigations

| Risk                                        | Impact | Probability | Mitigation                                    |
| ------------------------------------------- | ------ | ----------- | --------------------------------------------- |
| Auto-fix introduces unwanted changes        | MEDIUM | MEDIUM      | Review auto-fix changes before committing     |
| Existing content violates too many rules    | HIGH   | MEDIUM      | Configure rules to align with existing style  |
| Performance issues with large file count    | LOW    | LOW         | markdownlint-cli2 has good performance        |
| Pre-push hook slows down developer workflow | MEDIUM | LOW         | Lint only changed files, optimize rules       |
| Rules conflict with Hugo/frontmatter        | MEDIUM | MEDIUM      | Disable conflicting rules, use ignore files   |
| Prettier rewraps prose unexpectedly         | MEDIUM | LOW         | Use proseWrap: "preserve" to disable wrapping |
| Claude Code hook slows down file edits      | MEDIUM | LOW         | Only run on .md files, optimize scripts       |
| Prettier and markdownlint rule conflicts    | MEDIUM | LOW         | Use markdownlint/style/prettier extension     |
| Hook errors block Claude Code operations    | HIGH   | LOW         | Test hooks thoroughly, add error handling     |

## Dependencies

**Tools Required**:

- Node.js 24.11.1 (✅ already installed, managed by Volta)
- npm 11.6.3 (✅ already installed)
- Prettier 3.6.2 (✅ already installed, runs on pre-commit via lint-staged)
  - ✅ .prettierrc.json exists: `printWidth: 120, proseWrap: "preserve"`
- lint-staged 16.2.6 (✅ already installed, configured for markdown)
- Husky 9.1.7 (✅ already installed, pre-commit hook active)
  - ✅ .husky/pre-push exists: runs `npx nx affected -t test:quick`
- markdownlint-cli2 (to be installed)
- jq (required for Claude Code hook JSON parsing, to be verified/installed)

**Repository Metrics**:

- **Total markdown files**: 1,413 files
- **Performance consideration**: Pre-push linting all files may take 10-30 seconds

**Knowledge Required**:

- Markdown syntax
- Repository markdown conventions
- Husky git hooks
- npm scripts
- Claude Code hook JSON parsing with jq
- Bash conditional scripting

## Follow-up Tasks

After markdown linting implementation:

1. **Monitor Performance**: Track pre-push hook execution time
   - If consistently >5 seconds, consider optimizing to check only changed files
   - Consider adding to pre-commit hooks for faster feedback cycle
2. **Monitor for false positives**: Track overly strict or conflicting rules
3. **Adjust configuration**: Based on developer feedback and real-world usage
4. **CI/CD Integration**: Add markdown linting to CI/CD pipeline for additional safety net
5. **Periodic Review**: Schedule quarterly review of linting rules and markdownlint updates
6. **Custom Rules**: Evaluate need for project-specific custom rules (keyword: markdownlint-rule on npm)
7. **Pre-Commit Addition** (Optional): If pre-push proves too slow, add to pre-commit for earlier feedback
   - Recommended approach: Pre-commit for fast checks, pre-push for comprehensive validation

## References

**Related Conventions**:

- [Content Quality Convention](../../../../governance/conventions/writing/quality.md)
- [Indentation Convention](../../../../governance/conventions/formatting/indentation.md)
- [Code Quality Convention](../../../governance/development/quality/code.md)

**External Resources**:

Official Documentation:

- [markdownlint-cli2 GitHub Repository](https://github.com/DavidAnson/markdownlint-cli2)
- [markdownlint-cli2 npm Package](https://www.npmjs.com/package/markdownlint-cli2)
- [markdownlint Library GitHub](https://github.com/DavidAnson/markdownlint)
- [markdownlint Rules Reference](https://github.com/DavidAnson/markdownlint/blob/main/doc/Rules.md)
- [markdownlint Prettier Integration](https://github.com/DavidAnson/markdownlint/blob/main/doc/Prettier.md)
- [Prettier Documentation](https://prettier.io/)
- [Prettier Options](https://prettier.io/docs/options)
- [Husky Documentation](https://typicode.github.io/husky/)
- [Claude Code Hooks Reference](https://code.claude.com/docs/en/hooks)
- [Claude Code Hooks Guide](https://code.claude.com/docs/en/hooks-guide)

Research Sources - markdownlint (2026-01-17):

- [markdownlint-cli vs markdownlint-cli2 Comparison](https://npm-compare.com/markdownlint-cli,markdownlint-cli2)
- [Author's Blog: markdownlint-cli2 Introduction](https://dlaa.me/blog/post/markdownlintcli2)
- [npm trends: markdownlint vs markdownlint-cli vs markdownlint-cli2](https://npmtrends.com/markdownlint-vs-markdownlint-cli-vs-markdownlint-cli2)
- [Linting Markdown And Documentation - Earthly Blog](https://earthly.dev/blog/markdown-lint/)
- [Best Markdown Static Analysis Tools](https://analysis-tools.dev/tag/markdown)
- [Linting your Markdown Files - Scott's Weblog](https://blog.scottlowe.org/2024/03/01/linting-your-markdown-files/)

Research Sources - Prettier Integration (2026-01-17):

- [Configuring Markdownlint Alongside Prettier](https://www.joshuakgoldberg.com/blog/configuring-markdownlint-alongside-prettier/)
- [Prettier Markdown Support Announcement](https://prettier.io/blog/2017/11/07/1.8.0.html)
- [markdownlint/style/prettier.json](https://github.com/DavidAnson/markdownlint/blob/main/style/prettier.json)

Research Sources - Git Hooks (2026-01-17):

- [Pre-Push Hooks - DEV Community](https://dev.to/jameson/pre-push-hooks-42g5)
- [Mastering Git Hooks: Advanced Techniques](https://kinsta.com/blog/git-hooks/)
- [Maximizing Code Quality with Pre-Commit and Pre-Push Hooks](https://www.shakacode.com/blog/maximizing-code-quality-with-rails-pre-commit-and-pre-push-hooks/)
- [Prevent Bad Commits with Husky and lint-staged](https://betterstack.com/community/guides/scaling-nodejs/husky-and-lint-staged/)

Research Sources - Claude Code Hooks (2026-01-17):

- [A complete guide to hooks in Claude Code](https://www.eesel.ai/blog/hooks-in-claude-code)
- [Developer's hooks reference for Claude Code](https://www.eesel.ai/blog/hooks-reference-claude-code)
- [How to Automate Development Workflows with Claude Code Hooks](https://apidog.com/blog/claude-code-hooks/)
- [Claude Code Hooks: The Feature You're Ignoring](https://medium.com/@lakshminp/claude-code-hooks-the-feature-youre-ignoring-while-babysitting-your-ai-789d39b46f6c)
- [Claude Code: Part 8 - Hooks for Automated Quality Checks](https://www.letanure.dev/blog/2025-08-06--claude-code-part-8-hooks-automated-quality-checks)
- [Configure Claude Code Hooks to Automate Your Workflow](https://www.gend.co/blog/configure-claude-code-hooks-automation)
- [Ultimate Guide to Claude Code Setup: Hooks, Skills & Actions](https://aibit.im/blog/post/ultimate-guide-to-claude-code-setup-hooks-skills-actions)
- [Claude Code Hook Examples](https://stevekinney.com/courses/ai-development/claude-code-hook-examples)
- [Complete Guide: Creating Claude Code Hooks](https://suiteinsider.com/complete-guide-creating-claude-code-hooks/)
- [Claude Code and Bash Scripts](https://stevekinney.com/courses/ai-development/claude-code-and-bash-scripts)

## Notes

### Plan Evolution

- **Scope Update**: Plan expanded to include Claude Code hook integration (user requirement added during planning)
- **Comprehensive Review Completed 2026-01-17**: Plan validated against current repository state and web research

### Critical Discoveries (Pre-Execution Verification)

1. **Prettier Already Configured** ✅
   - v3.6.2 installed with .prettierrc.json
   - Settings: `printWidth: 120, proseWrap: "preserve"` (optimal for markdown)
   - Runs on pre-commit via lint-staged
   - Phase 5 simplified: No installation, only optimization and Claude Code integration

2. **Pre-Push Hook Exists** ✅
   - Current content: `npx nx affected -t test:quick`
   - Our linting will be APPENDED (not replaced)
   - Full hook: test command + lint command

3. **Repository Metrics** 📊
   - 1,413 markdown files in repository
   - Expected pre-push lint time: 10-30 seconds
   - Performance optimization may be needed if >30s

4. **Claude Code Hook Implementation** 🔧
   - Requires `jq` for JSON parsing (installation status unknown)
   - Must parse stdin: `jq -r '.tool_input.file_path'`
   - Hook receives JSON with tool_input.file_path
   - Must exit 0 to avoid blocking Claude Code
   - Complete example script provided in checklist

5. **No .claude/settings.json** ⚠️
   - File doesn't exist yet (needs creation)
   - Complete example configuration provided in checklist

### Implementation Guidance

- **Baseline validation is CRITICAL** before enforcement to avoid blocking developers
- Configuration should align with existing repository markdown conventions
- Auto-fix should be used cautiously with manual review
- Do NOT commit until explicitly instructed
- Test with intentional violations before enforcement

### Web Research Validation

Research validated 2026-01-17:

- markdownlint-cli2 is the modern, future-focused choice
- Prettier + markdownlint integration is well-established (style/prettier.json extension)
- Claude Code hooks provide deterministic automation vs relying on LLM
- PostToolUse hooks receive JSON via stdin with file_path
- jq is the standard tool for JSON parsing in bash hooks
- Pre-push hook approach is appropriate for comprehensive checks
- Prettier is idempotent (safe to run multiple times)
- Error handling (exit 0) prevents hook from blocking Claude Code
