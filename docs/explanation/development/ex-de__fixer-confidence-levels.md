---
title: "Fixer Confidence Levels Convention"
description: Universal confidence level system for fixer agents to assess and apply validated fixes
category: explanation
subcategory: development
tags:
  - fixer-agents
  - confidence-levels
  - validation
  - automation
  - quality-assurance
created: 2025-12-14
updated: 2025-12-15
---

# Fixer Confidence Levels Convention

This document defines the universal confidence level system used by all fixer agents (repo-rules-fixer, ayokoding-content-fixer, docs-tutorial-fixer, ose-platform-web-content-fixer, readme-fixer, docs-fixer, plan-fixer) to assess findings from checker agents and determine which fixes can be applied automatically versus which require manual review.

## Principles Respected

This practice respects the following core principles:

- **[Explicit Over Implicit](../principles/software-engineering/ex-pr-se__explicit-over-implicit.md)**: Three confidence levels (HIGH, MEDIUM, FALSE_POSITIVE) with explicit, documented criteria. Fix decisions are stated clearly in audit reports - no hidden judgment calls or magic automation.

- **[Automation Over Manual](../principles/software-engineering/ex-pr-se__automation-over-manual.md)**: Objective, verifiable issues (HIGH confidence) fixed automatically. Machines handle mechanical corrections. Humans focus on subjective improvements and ambiguous cases flagged as MEDIUM confidence.

## 📋 Overview

### What This Convention Defines

This convention establishes:

- **Three confidence levels** - HIGH, MEDIUM, FALSE_POSITIVE
- **When to apply fixes automatically** - HIGH confidence only
- **When to skip fixes** - MEDIUM (manual review) and FALSE_POSITIVE (report to user)
- **Universal criteria** - Applicable across all fixer agents regardless of domain
- **Domain-specific examples** - How each fixer agent applies the system

### Why Confidence Levels Matter

Without a standardized confidence assessment system, automated fixers can:

- **Over-apply fixes** - Modify content that shouldn't be changed (subjective quality improvements, ambiguous cases)
- **Create false changes** - Apply fixes based on incorrect checker findings (false positives)
- **Break functionality** - Make changes in contexts where checker's fix suggestion was wrong
- **Erode trust** - Users lose confidence when automated fixes produce unexpected results

With confidence levels:

- ✅ **Safety** - Only high-confidence, objective fixes applied automatically
- ✅ **Transparency** - Users know why fixes were applied or skipped
- ✅ **Efficiency** - Obvious objective issues fixed without manual intervention
- ✅ **Quality** - Subjective improvements flagged for human judgment
- ✅ **Feedback loop** - False positives reported to improve checker accuracy

## 🎯 Purpose

Confidence levels serve multiple critical purposes:

### 1. Automated Fixing Safety

**Problem:** Checkers sometimes flag legitimate content as violations or suggest inappropriate fixes.

**Solution:** Re-validate findings and apply fixes only when confidence is HIGH.

**Benefit:** Prevents automated tools from making inappropriate changes.

### 2. Human Judgment Recognition

**Problem:** Many quality issues are subjective and context-dependent (narrative flow, tone, engagement, word choice).

**Solution:** Flag subjective improvements as MEDIUM confidence requiring manual review.

**Benefit:** Respects the human element in quality assessment while automating objective fixes.

### 3. Checker Quality Improvement

**Problem:** Checkers can have detection logic flaws that produce false positives.

**Solution:** Identify false positives through re-validation and report them with suggested improvements.

**Benefit:** Creates feedback loop that continuously improves checker accuracy.

### 4. Audit Trail and Transparency

**Problem:** Users need to understand what was fixed, what was skipped, and why.

**Solution:** Document all confidence assessments in fix reports with detailed reasoning.

**Benefit:** Builds trust and provides clear path for manual review.

## 📊 Scope

### Agents Using This System

All fixer agents implement this confidence level system:

- **repo-rules-fixer** - Repository-wide structural consistency fixes
- **ayokoding-content-fixer** - ayokoding-web Hugo content fixes
- **docs-tutorial-fixer** - Tutorial quality fixes
- **ose-platform-web-content-fixer** - ose-platform-web Hugo content fixes
- **readme-fixer** - README quality fixes
- **docs-fixer** - Documentation factual accuracy fixes
- **plan-fixer** - Plan structural and format fixes

### Universal Application

The three confidence levels (HIGH, MEDIUM, FALSE_POSITIVE) are universal. Each agent:

1. **Reads audit reports** from corresponding checker agent
2. **Re-validates findings** using same patterns as checker
3. **Assesses confidence** using criteria defined in this convention
4. **Applies HIGH confidence fixes** automatically
5. **Skips MEDIUM and FALSE_POSITIVE** with explanations
6. **Generates fix reports** documenting all decisions

## 🔍 The Three Confidence Levels

### HIGH_CONFIDENCE → Apply Fix Automatically

**Criteria:**

- Re-validation clearly confirms the issue exists
- Issue is OBJECTIVE and verifiable (not subjective judgment)
- No ambiguity in detection
- Fix is straightforward and safe
- Low risk of unintended consequences
- Context is clear and fix applies universally

**Decision:** Apply fix automatically without user confirmation.

**Examples Across Domains:**

**repo-rules-fixer:**

- Missing `subcategory` field verified by re-reading frontmatter
- Broken internal link verified by checking file doesn't exist at target path
- Wrong field value verified by comparing actual vs expected value
- File naming convention violation verified by prefix analysis

**ayokoding-content-fixer:**

- Missing `draft: false` field verified by re-reading frontmatter
- Wrong date format verified by regex pattern match (missing UTC+7 timezone)
- Weight field error verified for \_index.md (should be 1, found 10)
- Relative link in navigation content verified (should use absolute with language prefix)

**docs-tutorial-fixer:**

- Missing required section verified by section heading search (Introduction, Prerequisites)
- Incorrect LaTeX delimiter verified by pattern match (single `$` on own line for display math)
- Wrong tutorial type naming verified against convention patterns
- Time estimate violation verified by keyword search (forbidden in tutorials)

**ose-platform-web-content-fixer:**

- Missing required frontmatter field verified (title, date, draft)
- Wrong date format verified by regex (missing timezone)
- Missing cover.alt verified when cover.image exists
- Multiple H1 headings verified by counting (should be only 1)

**readme-fixer:**

- Paragraph exceeding 5 lines verified by objective line count
- Acronym without context verified by context search (missing expansion/explanation)
- Broken internal link verified by file existence check
- Format errors verified by structural analysis (heading hierarchy violations)

**docs-fixer:**

- Broken command syntax verified by WebFetch of official documentation
- Incorrect version number verified by checking package registry (npm, PyPI)
- Wrong API method verified by WebFetch of current API docs
- LaTeX delimiter error verified by pattern match (single `$` on own line for display math)
- Diagram color accessibility violation verified against accessible palette

**plan-fixer:**

- Missing required section verified by heading search (Introduction, Requirements, Technical Documentation)
- Broken internal link to codebase file verified by file existence check
- Format violation verified (frontmatter YAML, acceptance criteria format)
- Naming convention violation verified (folder name doesn't match `YYYY-MM-DD__identifier`)
- File structure mismatch verified (single-file vs multi-file convention)

**Common Pattern:** HIGH confidence issues are **objective, measurable, and verifiable** - they either exist or they don't.

### MEDIUM_CONFIDENCE → Skip (Manual Review Needed)

**Criteria:**

- Re-validation is unclear or ambiguous
- Issue is SUBJECTIVE (requires human judgment)
- Multiple valid interpretations possible
- Context-dependent decision
- Requires domain expertise or creative judgment
- Fix could harm quality in certain contexts

**Decision:** Skip fix, flag for manual review with explanation.

**Examples Across Domains:**

**repo-rules-fixer:**

- Content duplication between CLAUDE.md and convention file (context differs, may be intentional)
- Link target unclear (file missing, but can't determine correct target automatically)
- Field value could be valid in specific context (non-standard but potentially intentional)

**ayokoding-content-fixer:**

- Description length borderline (145 chars vs 150-160 optimal - functional but could improve)
- Line length slightly over 100 characters (breaking might harm readability)
- Alt text could be more descriptive but not completely missing
- Content structure acceptable but could be improved

**docs-tutorial-fixer:**

- Narrative flow issues (too list-heavy, needs better storytelling)
- Diagram placement suggestions (section would benefit from visual aid)
- Writing style critiques (too dry, needs more engaging voice)
- Content balance assessments (theory vs practice ratio)
- Example quality assessments (examples work but could be better)

**ose-platform-web-content-fixer:**

- Summary length is short but functional (85 chars vs 150-160 optimal)
- Image alt text vague but not missing ("screenshot" - need image context to improve)
- Line length exceeds limit but breaking would harm readability
- Broken link with unclear correct target (file missing, multiple possibilities)

**readme-fixer:**

- Engagement quality ("opening paragraph not engaging enough" - subjective tone judgment)
- Tone improvements ("sounds too corporate" - style preference)
- Benefits framing ("not benefits-focused enough" - messaging choice)
- Word choice preferences ("utilize" vs "use" when both are clear)
- Section length borderline (25 lines - depends on README philosophy)

**docs-fixer:**

- Contradiction that may be context-dependent (HTTP for local, HTTPS for production)
- Outdated information where "outdated" is subjective or requires judgment
- Content duplication where duplication may be intentional for clarity
- Narrative flow issues or writing style critiques (subjective quality)
- Terminology inconsistency where both terms are technically correct

**plan-fixer:**

- Scope decisions ("plan scope too broad" - requires business judgment)
- Technology choices ("should use PostgreSQL instead of MongoDB" - architectural expertise)
- Approach critiques ("microservices approach not suitable" - domain knowledge)
- Timeline assessments ("timeline unrealistic" - team capacity knowledge)
- Implementation strategies ("should use different design pattern" - technical judgment)

**Common Pattern:** MEDIUM confidence issues involve **human judgment, subjective quality assessment, or context-dependent decisions**.

### FALSE_POSITIVE → Skip (Report to User)

**Criteria:**

- Re-validation clearly disproves the issue
- Checker's detection logic was flawed
- Finding was based on incorrect analysis
- Content is actually compliant but checker missed it
- Checker applied wrong rules to specific context

**Decision:** Skip fix, report to user with detailed analysis and checker improvement suggestion.

**Examples Across Domains:**

**repo-rules-fixer:**

- Checker flagged markdown headings as YAML comments (searched entire file instead of just frontmatter)
- Checker reported missing field that actually exists (case sensitivity issue)
- Checker misinterpreted file content (wrong pattern match)

**ayokoding-content-fixer:**

- Checker flagged overview.md in English folder but file is correct (checker confused /en/ with /id/)
- Checker flagged missing ikhtisar.md in blogging content (learning-only rule applied to wrong directory)
- Checker misidentified language path when validating filenames

**docs-tutorial-fixer:**

- Checker reported missing Introduction section but section exists (titled "Introduction to Topic")
- Checker reported missing diagram but diagram exists (different Mermaid syntax or placement)
- Checker misinterpreted tutorial type (tutorial follows convention correctly)

**ose-platform-web-content-fixer:**

- Checker flagged Hugo shortcode link as broken (doesn't recognize `{{< ref >}}` syntax)
- Checker applied post validation rules to static page (about.md doesn't need date field)
- Checker counted code block as prose paragraph (wrong content type detection)

**readme-fixer:**

- Checker flagged valid acronym expansion as missing (expansion exists nearby)
- Checker counted lines incorrectly (markdown formatting issues)
- Checker misinterpreted valid plain language as jargon (context-appropriate technical term)
- Checker flagged code block as long paragraph (wrong content detection)

**docs-fixer:**

- Checker flagged correct LaTeX as incorrect (misunderstood syntax)
- Checker reported missing field that actually exists in frontmatter
- Checker flagged valid command as broken (used wrong verification source)
- Checker misinterpreted accessible diagram colors as inaccessible
- Checker reported contradiction but statements apply to different contexts

**plan-fixer:**

- Checker reported missing section that actually exists (different heading variation)
- Checker flagged technology as "deprecated" but it's still maintained (outdated info)
- Checker reported broken link that actually works (path resolution issue)
- Checker misidentified file structure (valid edge case)

**Common Pattern:** FALSE_POSITIVE issues reveal **checker logic flaws** that need correction.

## 🔄 Why Re-Validation is Mandatory

### Never Trust Checker Findings Blindly

**CRITICAL PRINCIPLE:** Fixer agents MUST re-validate all findings before applying fixes.

**Why:**

1. **Checkers can be wrong** - Detection logic may have bugs or edge cases
2. **Context changes** - File may have been modified between checker run and fixer run
3. **Ambiguity exists** - What looks like violation may be valid in specific context
4. **Confidence assessment requires verification** - Can't assess confidence without re-checking

**Process:**

```
Checker Report → Read Finding → Re-execute Validation → Assess Confidence → Apply/Skip/Report
```

**Re-validation methods:**

- Extract frontmatter using same AWK pattern as checker
- Check file existence for broken links
- Count objective metrics (paragraph lines, H1 headings)
- Verify patterns match (date format, naming convention)
- Analyze context (content type, directory, file purpose)

**See:** [Repository Validation Methodology Convention](./ex-de__repository-validation.md) for standard re-validation patterns.

## 📏 Confidence Assessment Process

### How Fixers Determine Confidence Level

For each finding in the checker's audit report:

#### Step 1: Classify Issue Type

**Question:** Is this issue objective or subjective?

**Objective issues** (measurable, verifiable):

- Missing fields in frontmatter
- Wrong field values
- Broken links
- Format violations (LaTeX delimiters, heading hierarchy)
- Naming convention violations
- Objective length violations (paragraphs >5 lines, descriptions missing optimal range)

**Subjective issues** (judgment-based, context-dependent):

- Narrative flow quality
- Tone and voice preferences
- Engagement assessments
- Writing style critiques
- Content balance judgments
- Diagram placement suggestions
- Word choice preferences (when both options are clear)

#### Step 2: Re-validate the Finding

**Question:** Does the issue actually exist when re-checked?

**Re-validation confirms issue:**

- Field is actually missing
- Link target actually doesn't exist
- Format actually violates pattern
- Continue to Step 3

**Re-validation disproves issue:**

- Field exists (checker missed it)
- Link target exists (checker had wrong logic)
- Format is actually valid (checker applied wrong rule)
- **Confidence: FALSE_POSITIVE** → Skip and report

#### Step 3: Assess Fix Safety

**Question:** Can fix be applied safely and unambiguously?

**Safe and unambiguous:**

- Add missing field with standard value
- Fix date format to standard pattern
- Convert single `$` to `$$` for LaTeX
- Split long paragraph at sentence boundary
- **Confidence: HIGH** → Apply fix

**Unsafe or ambiguous:**

- Broken link but correct target unclear
- Subjective quality improvement
- Context-dependent decision needed
- **Confidence: MEDIUM** → Skip and flag for manual review

#### Step 4: Document Decision

**Always document:**

- What was re-validated
- Confidence level assigned
- Reasoning for confidence assessment
- Action taken (fixed / skipped / reported)

## 🌍 Domain-Specific vs Universal Criteria

### What's Universal

These criteria apply across ALL fixer agents:

**HIGH Confidence Universal Criteria:**

- Issue is objective and verifiable
- Re-validation confirms issue exists
- Fix is straightforward and safe
- No context-dependent judgment required

**MEDIUM Confidence Universal Criteria:**

- Issue is subjective or context-dependent
- Multiple valid interpretations exist
- Requires human judgment or creativity
- Fix could harm quality in certain contexts

**FALSE_POSITIVE Universal Criteria:**

- Re-validation clearly disproves issue
- Checker's detection logic was flawed
- Content is actually compliant

### What Varies by Domain

Each fixer agent has domain-specific validation checks:

**repo-rules-fixer:**

- Frontmatter field validation for agent files
- File naming convention compliance
- Structural consistency across repository

**ayokoding-content-fixer:**

- Hugo frontmatter for Hextra theme
- Bilingual content validation (en/id)
- Learning content specific rules (overview/ikhtisar, weight ordering)
- Navigation link format (absolute paths with language prefix)

**docs-tutorial-fixer:**

- Tutorial-specific structure (Introduction, Prerequisites, Learning Objectives)
- LaTeX notation compliance
- Tutorial naming patterns by type
- No time estimates rule

**ose-platform-web-content-fixer:**

- Hugo frontmatter for PaperMod theme
- English-only content validation
- Cover image alt text requirements
- Heading hierarchy (single H1 rule)

**readme-fixer:**

- README-specific quality standards
- Paragraph length limits (≤5 lines)
- Acronym context requirements
- Plain language preferences (with technical section exceptions)

**Key Point:** While validation checks differ, the confidence level criteria remain universal.

## 🔗 Integration with Fixer Agents

### How Each Fixer Uses This System

All fixer agents follow the same workflow:

#### 1. Report Discovery

- Auto-detect latest audit report in `generated-reports/`
- Allow manual override if user specifies specific report
- Verify report exists and is readable

#### 2. Findings Parsing

- Extract findings from audit report sections
- Identify file path, issue type, line numbers
- Group by issue category

#### 3. Re-validation Loop

For each finding:

```python
def process_finding(finding):
    # Re-execute validation check
    validation_result = revalidate_finding(finding)

    # Assess confidence
    if validation_result.is_objective and validation_result.confirmed:
        confidence = "HIGH"
        apply_fix(finding)
    elif validation_result.is_subjective or validation_result.ambiguous:
        confidence = "MEDIUM"
        flag_for_manual_review(finding)
    elif validation_result.disproved:
        confidence = "FALSE_POSITIVE"
        report_to_user(finding, improvement_suggestion)

    # Document decision
    log_to_fix_report(finding, confidence, validation_result)
```

#### 4. Fix Application

- Apply ALL HIGH confidence fixes automatically
- Skip MEDIUM and FALSE_POSITIVE findings
- NO confirmation prompts (user already reviewed checker report)

#### 5. Fix Report Generation

Create comprehensive report in `generated-reports/`:

- Validation summary (HIGH/MEDIUM/FALSE_POSITIVE counts)
- Fixes applied section (what changed)
- False positives detected (detailed analysis)
- Needs manual review (subjective items)
- Recommendations for checker improvement

**File naming:** Replace `__audit` suffix with `__fix` (same timestamp)

### Consistency Across Agents

All fixer agents MUST:

- Use the same three confidence levels
- Apply the same universal criteria
- Generate fix reports in the same format
- Report false positives with improvement suggestions
- Document all confidence assessments
- Never skip re-validation

## ↩️ False Positive Feedback Loop

### How False Positives Improve Checker Accuracy

When a fixer detects a false positive:

#### 1. Detailed Analysis

Fixer performs root cause analysis:

- **What checker flagged:** Description of the finding
- **Re-validation result:** What fixer discovered when re-checking
- **Why it's false positive:** Explanation of checker's logic flaw
- **Example:** Concrete example from the file

#### 2. Improvement Suggestion

Fixer provides actionable recommendation:

- **Current issue:** Specific problem in checker's detection logic
- **Fix:** Corrected validation pattern or logic
- **Code example:** Updated bash/grep/awk command or logic
- **Impact:** How many false positives this would eliminate

#### 3. Reporting to User

Fixer includes in fix report:

- **False Positives Detected** section
- One entry per false positive with full analysis
- **Recommendations for [checker-name]** section
- Numbered list of suggested improvements

#### 4. Checker Evolution

User or maintainer reviews false positive reports and:

- Updates checker agent with corrected logic
- Re-runs checker on repository
- Verifies false positives are eliminated
- Re-runs fixer to confirm clean results

### Example Feedback Loop

**Initial State:**

```
repo-rules-checker flags:
  - VIOLATION: 15 agent files have YAML comments in frontmatter
```

**Fixer Re-validation:**

```
repo-rules-fixer re-validates:
  - Extracts frontmatter from each file
  - Searches isolated frontmatter for # symbols
  - Result: 0 actual violations found (all # symbols in markdown body)
  - Confidence: FALSE_POSITIVE for all 15 findings
```

**Fixer Report:**

````markdown
## False Positives Detected (15)

❌ All agent files - Frontmatter comment detection

- **Checker finding:** Agent frontmatter contains YAML comment (# symbol)
- **Re-validation:** Extracted frontmatter, no # found (only in markdown body)
- **Conclusion:** FALSE POSITIVE
- **Reason:** Checker searched entire file instead of just frontmatter section
- **Recommendation:** Update checker to use:
  ```bash
  awk 'BEGIN{p=0} /^---$/{if(p==0){p=1;next}else{exit}} p==1' file.md | grep "#"
  ```
````

- **Impact:** Eliminates all 15 false positives in this run

```

**Checker Update:**

- Maintainer updates repo-rules-checker with corrected AWK pattern
- Re-runs checker: 0 violations found
- False positives eliminated

**Continuous Improvement:**

- Each fixer run identifies new edge cases
- Recommendations accumulate in fix reports
- Checker accuracy improves over time
- Trust in automation increases

## 📚 References

### Fixer Agents Using This Convention

- [repo-rules-fixer.md](https://github.com/wahidyankf/open-sharia-enterprise/blob/main/.claude/agents/repo-rules-fixer.md) - Repository structural consistency fixer
- [ayokoding-content-fixer.md](https://github.com/wahidyankf/open-sharia-enterprise/blob/main/.claude/agents/ayokoding-content-fixer.md) - ayokoding-web Hugo content fixer
- [docs-tutorial-fixer.md](https://github.com/wahidyankf/open-sharia-enterprise/blob/main/.claude/agents/docs-tutorial-fixer.md) - Tutorial quality fixer
- [ose-platform-web-content-fixer.md](https://github.com/wahidyankf/open-sharia-enterprise/blob/main/.claude/agents/ose-platform-web-content-fixer.md) - ose-platform-web Hugo content fixer
- [readme-fixer.md](https://github.com/wahidyankf/open-sharia-enterprise/blob/main/.claude/agents/readme-fixer.md) - README quality fixer

### Related Conventions

**Validation Methodology:**
- [Repository Validation Methodology Convention](./ex-de__repository-validation.md) - Standard validation patterns (frontmatter extraction, field checks, link validation)

**AI Agents:**
- [AI Agents Convention](./ex-de__ai-agents.md) - Standards for all AI agents including fixers

**Content Standards:**
- [Tutorial Convention](../conventions/ex-co__tutorials.md) - Tutorial-specific validation criteria (used by docs-tutorial-fixer)
- [Content Quality Principles](../conventions/ex-co__content-quality.md) - Universal content quality standards
- [README Quality Convention](../conventions/ex-co__readme-quality.md) - README-specific standards (used by readme-fixer)
- [Hugo Content Convention - Shared](../conventions/ex-co__hugo-content-shared.md) - Shared Hugo content standards
- [Hugo Content Convention - ayokoding](../conventions/ex-co__hugo-content-ayokoding.md) - ayokoding-web specific standards
- [Hugo Content Convention - OSE Platform](../conventions/ex-co__hugo-content-ose-platform.md) - ose-platform-web specific standards

**Infrastructure:**
- [Temporary Files Convention](./ex-de__temporary-files.md) - Where to store fix reports (`generated-reports/`)

## 🔄 Maintenance

### When to Update This Convention

Update this convention when:

1. **New fixer agent created** - Add to scope section
2. **New confidence criteria discovered** - Add to universal criteria
3. **Common patterns emerge** - Document in domain-specific vs universal section
4. **False positive patterns repeat** - Document in feedback loop section
5. **Validation methodology changes** - Update re-validation process

### Propagating Changes

When this convention is updated:

1. **Review all fixer agents** - Ensure they follow updated criteria
2. **Update agent prompts** - Reflect new confidence assessment guidance
3. **Test edge cases** - Verify new criteria work across domains
4. **Document examples** - Add concrete examples of new patterns
5. **Announce changes** - Notify maintainers of fixer agents

### Version History

- **2025-12-14** - Initial convention established based on 5 fixer agents (repo-rules, ayokoding-content, docs-tutorial, ose-platform-web-content, readme)

---

This convention is the single source of truth for confidence level assessment across all fixer agents. All fixers should reference and implement these criteria consistently to ensure safe, effective automated fixing with proper human oversight for subjective quality improvements.
```
