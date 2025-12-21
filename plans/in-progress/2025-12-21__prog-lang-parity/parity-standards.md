# Parity Standards

## Purpose

This document defines explicit, measurable parity standards that all 6 programming languages must meet. These standards are based on:

1. Programming Language Content Standard convention (baseline requirements)
2. Highest standards identified from existing content (aspirational targets)
3. Elixir reference implementation (structural compliance)

All standards are **objective and measurable** to enable automated validation.

## Section 1: Structural Parity Standard

### Required Directory Structure

All languages MUST have this exact structure:

```
apps/ayokoding-web/content/en/learn/swe/prog-lang/[language]/
├── _index.md                     (weight: varies by language order)
├── overview.md                   (weight: 1000000)
├── tutorials/
│   ├── _index.md                 (weight: 100002)
│   ├── overview.md               (weight: 1000000)
│   ├── initial-setup.md          (weight: 1000001)
│   ├── quick-start.md            (weight: 1000002)
│   ├── beginner.md               (weight: 1000003)
│   ├── intermediate.md           (weight: 1000004)
│   └── advanced.md               (weight: 1000005)
├── how-to/
│   ├── _index.md                 (weight: 100003)
│   ├── overview.md               (weight: 1000000)
│   ├── cookbook.md               (weight: 1000001) ← CRITICAL
│   ├── [guide-1].md              (weight: 1000002)
│   ├── [guide-2].md              (weight: 1000003)
│   └── ... (minimum 12 guides excluding cookbook)
├── explanation/
│   ├── _index.md                 (weight: 100004)
│   ├── overview.md               (weight: 1000000)
│   ├── best-practices.md         (weight: 1000001 or higher)
│   └── anti-patterns.md          (weight: 1000002 or higher)
└── reference/
    ├── _index.md                 (weight: 100005)
    ├── overview.md               (weight: 1000000)
    ├── cheat-sheet.md            (weight: 1000001 or higher)
    ├── glossary.md               (weight: 1000002 or higher)
    └── resources.md              (weight: 1000003 or higher)
```

### Weight Progression Formulas

#### Category Folders (\_index.md)

**Formula:** `100000 + (2 + category_position)`

Where category_position:

- tutorials: 0 → weight 100002
- how-to: 1 → weight 100003
- explanation: 2 → weight 100004
- reference: 3 → weight 100005

**Rationale:** Uses powers of 10 for level-based weight system (as documented in Hugo Content Convention - ayokoding).

#### Overview Files (overview.md)

**Formula:** All overview.md files in category folders have weight `1000000`

**Rationale:** Consistent position 1 in all categories, before any content.

#### Content Files

**Formula:** Sequential from `1000001`

First content file (initial-setup in tutorials, cookbook in how-to): `1000001`
Subsequent files: `1000002`, `1000003`, `1000004`, ...

**Rationale:** Sequential weights ensure predictable ordering, starting from 1000001 as first content item.

### Critical Requirement: Cookbook Position

**MANDATORY:** Cookbook MUST be at weight `1000001` in how-to folder.

This positions it at **position 3** in navigation:

1. overview.md (weight 1000000)
2. [hidden due to overview page structure]
3. cookbook.md (weight 1000001) ← Appears here

**Rationale:**

- Pedagogical requirement from Programming Language Content Standard
- "Hook → Engage → Teach" model (overview hooks, cookbook engages, guides teach)
- Learner motivation: practical recipes early in learning journey
- Elixir serves as reference implementation

**Validation:** Run ayokoding-structure-checker, verify cookbook appears at position 3 in navigation.

### Reference Language for Structure

**Reference Implementation:** Elixir

**Justification:**

- ONLY language currently meeting all structural requirements
- Cookbook at correct weight (1000001)
- Category folders at correct weights (100002-100005)
- Tutorial weights sequential from 1000001
- Most recently added (Dec 2024), follows current standards most closely

**Alternative References:**

- Python, Golang, Java: Good content but structural violations
- Use these for content patterns, not structure

### File Naming Requirements

**Pattern:** Lowercase with hyphens, `.md` extension

**Examples:**

- ✅ `initial-setup.md`
- ✅ `quick-start.md`
- ✅ `best-practices.md`
- ✅ `cheat-sheet.md`
- ❌ `InitialSetup.md` (wrong case)
- ❌ `initial_setup.md` (underscores not hyphens)
- ❌ `quick start.md` (spaces not allowed)

### Structural Validation Criteria

All languages MUST pass these checks:

1. **Category folder weights:**
   - tutorials/\_index.md: 100002
   - how-to/\_index.md: 100003
   - explanation/\_index.md: 100004
   - reference/\_index.md: 100005

2. **Overview weights:** All overview.md files: 1000000

3. **Cookbook weight:** how-to/cookbook.md: 1000001

4. **Tutorial weights:** Sequential 1000001-1000005

5. **All required files present:**
   - 5 tutorials (initial-setup, quick-start, beginner, intermediate, advanced)
   - 1 cookbook + 12+ how-to guides
   - 2 explanations (best-practices, anti-patterns)
   - 3+ references (cheat-sheet, glossary, resources minimum)

6. **File naming:** All files follow lowercase-with-hyphens pattern

7. **ayokoding-structure-checker:** Zero violations

## Section 2: Content Parity Standard

### Minimum Line Counts by File Type

Based on Programming Language Content Standard:

#### Tutorials

| Tutorial         | Minimum Lines | Target Lines | Rationale                                   |
| ---------------- | ------------- | ------------ | ------------------------------------------- |
| initial-setup.md | 300           | 400+         | Installation, verification, troubleshooting |
| quick-start.md   | 600           | 700+         | 10-12 touchpoints covering 5-30% knowledge  |
| beginner.md      | 1200          | 1800+        | Comprehensive 0-60% coverage                |
| intermediate.md  | 1000          | 1500+        | 60-85% knowledge coverage                   |
| advanced.md      | 1000          | 1500+        | 85-95% knowledge coverage                   |
| **TOTAL**        | **4100**      | **5900+**    | Comprehensive learning path                 |

**Highest Standards (Aspirational):**

- Elixir tutorial total: 8087 lines (97% above minimum)
- Rust tutorial total: 6656 lines (62% above minimum)

#### Cookbook

| Metric           | Minimum | Target | Highest Standard    |
| ---------------- | ------- | ------ | ------------------- |
| Line count       | 4000    | 5000+  | Elixir: 5625 lines  |
| Recipe count     | 30      | 40+    | Manual count needed |
| Code examples    | 80      | 100+   | Golang: 192 blocks  |
| Cross-references | 30      | 50+    | Elixir: 52 links    |

**Recipe Structure:** Each recipe MUST follow Problem-Solution-How-Use pattern.

#### How-To Guides (excluding cookbook)

| Metric          | Minimum | Target |
| --------------- | ------- | ------ |
| Guide count     | 12      | 20+    |
| Lines per guide | 200     | 400+   |
| Total lines     | 3000    | 6000+  |

**Current State:** All 6 languages have 23-24 guides (exceeds minimum).

#### Explanations

| File              | Minimum Lines | Target Lines | Highest Standard   |
| ----------------- | ------------- | ------------ | ------------------ |
| best-practices.md | 500           | 700+         | Elixir: 1075 lines |
| anti-patterns.md  | 500           | 700+         | Elixir: 1054 lines |
| **TOTAL**         | **1000**      | **1400+**    | Elixir: 2129 lines |

#### Reference Materials

| File           | Minimum Lines | Target Lines | Highest Standard    |
| -------------- | ------------- | ------------ | ------------------- |
| cheat-sheet.md | 200           | 800+         | Golang: 1404 lines  |
| glossary.md    | 200           | 1000+        | Java: 1873 lines    |
| resources.md   | 200           | 600+         | Java: 879 lines     |
| **TOTAL**      | **600**       | **2400+**    | Combined excellence |

### Content Completeness Requirements

1. **All required files present:** No missing files from required structure
2. **All files meet minimums:** Every file meets minimum line count
3. **Cookbook completeness:** 30+ recipes with Problem-Solution-How-Use pattern
4. **How-to guide sufficiency:** 12+ guides covering common tasks
5. **Reference materials:** cheat-sheet, glossary, and resources with comprehensive coverage

### Highest Content Examples

Based on analysis:

- **Tutorial Excellence:** Elixir (all 5 tutorials longest and most comprehensive)
- **Cookbook Excellence:** Elixir (5625 lines, 52 cross-references, correct position)
- **Best Practices Excellence:** Elixir (1075 lines, 98 code examples)
- **Anti-Patterns Excellence:** Elixir (1054 lines, 76 code examples)
- **Cheat Sheet Excellence:** Golang (1404 lines, 64 code examples)
- **Glossary Excellence:** Java (1873 lines, 128 code examples)
- **Resources Excellence:** Java (879 lines, comprehensive links)

### Content Validation Criteria

All languages MUST meet:

1. **Tutorial totals:** 4100+ lines (target 5900+)
2. **Cookbook:** 4000+ lines with 30+ recipes
3. **How-to guides:** 12+ guides, 200+ lines each, 3000+ total
4. **Best practices:** 500+ lines (target 700+)
5. **Anti-patterns:** 500+ lines (target 700+)
6. **Reference:** 600+ total lines (target 2400+)
7. **ayokoding-content-checker:** Zero violations for line count requirements

## Section 3: Quality Parity Standard

### Pedagogical Pattern Requirements

All tutorials (30 files total across 6 languages) MUST have:

#### 1. Front Hooks (First Paragraph)

**Requirement:** First paragraph after frontmatter MUST engage reader with:

- Questions: "Want to", "Ever wondered", "What if", "Have you"
- Scenarios: "Imagine", "Picture this", "Consider"
- Challenges: "Struggling with", "Tired of"

**Example (to be created):**

```markdown
Want to build web applications that handle millions of concurrent connections without breaking a sweat? Elixir's lightweight processes and OTP platform make this possible, and you're about to discover how.
```

**Current State:** 0% compliance (0/30 tutorials have front hooks)

**Validation:** Manual review of first paragraph in all tutorials

#### 2. Learning Path Diagrams

**Requirement:** All tutorials MUST have Mermaid learning path diagram showing:

- Concept progression (what you'll learn in sequence)
- Knowledge building (how concepts connect)
- Prerequisites → Core Concepts → Advanced Topics flow

**Example (reference: Elixir tutorials):**

- Use vertical orientation (mobile-friendly)
- Use approved color palette ONLY
- Show 5-10 main concepts
- Show relationships between concepts

**Current State:**

- Elixir: 100% compliance (5/5 tutorials)
- Java, Kotlin, Rust: 80% compliance (4/5 tutorials - missing initial-setup)
- Python, Golang: 0% compliance (0/5 tutorials)

**Validation:** Check for "mermaid" code blocks with learning path content

#### 3. Prerequisites Sections

**Requirement:** All tutorials MUST have "## Prerequisites" section listing:

- Required prior knowledge
- Required installed software
- Optional helpful background

**Example:**

```markdown
## Prerequisites

- Basic command line familiarity
- [Language] installed (see [Initial Setup](/path/to/initial-setup))
- Text editor or IDE configured
- (Optional) Experience with [related concept]
```

**Current State:**

- Java, Kotlin, Rust, Elixir: 100% compliance (5/5 tutorials each)
- Python, Golang: 0% compliance (0/5 tutorials each)

**Reference Implementation:** Java, Kotlin, Rust, Elixir

**Validation:** Check for "## Prerequisites" heading in all tutorials

### Code Quality Requirements

All code examples MUST be:

1. **Runnable:** Can be executed without errors (given prerequisites met)
2. **Commented:** Key points explained with inline comments
3. **Complete:** Not fragments - full working examples
4. **Tested:** Examples verified in actual environment

**Validation:** ayokoding-facts-checker for syntax, manual testing for execution

### Diagram Requirements

#### Count Requirements

**Minimum per tutorial:**

- Initial Setup: 1 diagram (installation flow or verification)
- Quick Start: 1 diagram (learning path)
- Beginner: 1 diagram minimum (learning path), 3+ recommended
- Intermediate: 1 diagram minimum (learning path), 2+ recommended
- Advanced: 1 diagram minimum (learning path), 2+ recommended

**Excellence Reference:** Golang beginner tutorial (6 diagrams)

#### Color Palette Requirements

**MANDATORY:** All diagrams MUST use ONLY approved color-blind friendly palette:

- Blue: #0173B2
- Orange: #DE8F05
- Teal: #029E73
- Purple: #CC78BC
- Brown: #CA9161

**FORBIDDEN:** Never use:

- Red: #ff0000, #FF0000, 'red'
- Green: #00ff00, #00FF00, 'green'
- Yellow: #ffff00, #FFFF00, 'yellow'
- Any variations of red/green/yellow

**Rationale:** WCAG accessibility compliance, color-blind friendly (from Color Accessibility Convention)

**Current Violations:**

- Rust: 9 violations
- Elixir: 8 violations
- Kotlin: 2 violations

**Validation:** Grep for prohibited colors, manual review of rendered diagrams

### Cross-Reference Requirements

**Minimum per tutorial:** 10 cross-references to:

- Other tutorials (learning path connections)
- Cookbook recipes (practical applications)
- How-to guides (detailed instructions)
- Explanation content (deeper understanding)
- Reference materials (quick lookup)

**Current State:** All languages below minimum (0-7 links per tutorial)

**Excellence Reference:** Elixir cookbook (52 cross-references)

**Validation:** Count `](/` patterns in markdown, verify links with ayokoding-link-checker

### Content Quality Principles

From Content Quality Principles convention:

1. **Active Voice:** Use active voice for clarity
2. **Single H1:** Only one H1 (title) per file
3. **Proper Heading Nesting:** H2 → H3 → H4 (no skipping levels)
4. **Alt Text:** All images have descriptive alt text
5. **WCAG AA Colors:** All diagrams meet contrast requirements
6. **Semantic Formatting:** Bold for emphasis, italics for technical terms, code blocks for code
7. **No Time Estimates:** Never include time estimates (e.g., "this will take 30 minutes")

**Validation:** ayokoding-content-checker

### Quality Validation Criteria

All languages MUST pass:

1. **Front hooks:** 100% compliance (30/30 tutorials)
2. **Learning paths:** 100% compliance (30/30 tutorials have diagrams)
3. **Prerequisites:** 100% compliance (30/30 tutorials have sections)
4. **Cross-references:** 10+ per tutorial (300+ total across all tutorials)
5. **Color palette:** Zero violations (no red/green/yellow)
6. **Diagrams:** Minimum counts met, all using approved colors
7. **Code examples:** All runnable, commented, complete
8. **Content quality:** Zero time estimates, active voice, proper heading hierarchy
9. **ayokoding-content-checker:** Zero violations
10. **ayokoding-facts-checker:** Zero factual errors

## Section 4: Measurable Success Criteria

### Structural Success

✅ All 6 languages have:

- Cookbook at weight 1000001
- Category folders at weights 100002-100005
- Tutorial weights 1000001-1000005
- Zero ayokoding-structure-checker violations

### Content Success

✅ All 6 languages have:

- Tutorial total 4100+ lines (target 5900+)
- Cookbook 4000+ lines with 30+ recipes
- How-to guides 12+, 3000+ total lines
- Best practices 500+ lines (target 700+)
- Anti-patterns 500+ lines (target 700+)
- Reference 600+ total lines (target 2400+)
- Zero ayokoding-content-checker violations for line counts

### Quality Success

✅ All 6 languages have:

- 30/30 tutorials with front hooks
- 30/30 tutorials with learning path diagrams
- 30/30 tutorials with prerequisites sections
- 300+ cross-references total (10+ per tutorial)
- Zero color violations
- All diagrams using approved palette
- All code examples runnable
- Zero time estimates
- Zero ayokoding-content-checker violations
- Zero ayokoding-facts-checker errors

### Overall Success

✅ All automated checkers pass:

- ayokoding-structure-checker: 6/6 languages zero violations
- ayokoding-content-checker: 6/6 languages zero violations
- ayokoding-facts-checker: 6/6 languages zero errors
- ayokoding-link-checker: Zero broken links site-wide

✅ Manual QA sampling confirms:

- Pedagogical effectiveness (hooks engage, flow makes sense)
- Code examples execute correctly
- Cross-references are accurate
- Diagrams render correctly and clearly

## Reference Implementations Summary

**Structural:** Elixir (ONLY fully compliant language)

**Content Excellence:**

- Tutorials: Elixir (all 5 tutorials highest standard)
- Cookbook: Elixir (5625 lines, correct position, 52 links)
- Best Practices: Elixir (1075 lines, 98 code examples)
- Anti-Patterns: Elixir (1054 lines, 76 code examples)
- Cheat Sheet: Golang (1404 lines)
- Glossary: Java (1873 lines)
- Resources: Java (879 lines)

**Quality Excellence:**

- Learning Paths: Elixir (100% compliance)
- Prerequisites: Java, Kotlin, Rust, Elixir (100% compliance)
- Diagrams: Golang beginner (6 diagrams)
- Cross-References: Elixir cookbook (52 links)
- Color Compliance: Python, Golang, Java (zero violations)

## Usage

This parity-standards.md document serves as:

1. **Remediation Checklist:** What to fix in each language
2. **Validation Reference:** What to check after fixes
3. **Future Language Template:** Standards for new language additions
4. **Quality Benchmark:** Aspirational targets beyond minimums
