---
title: "Programming Language Tutorial Structure Convention"
description: "Dual-path tutorial organization pattern for programming language education with by-concept and by-example learning tracks"
category: explanation
subcategory: conventions
tags:
  - programming-languages
  - tutorials
  - ayokoding-web
  - education
  - structure
created: 2025-12-27
updated: 2025-12-30
---

# Programming Language Tutorial Structure Convention

**Defines the dual-path tutorial directory organization for programming language content on ayokoding-web.**

This convention standardizes how programming language tutorials are organized with two complementary learning paths: narrative-driven by-concept tutorials for deep understanding and code-first by-example tutorials for rapid language pickup.

## Principles Implemented/Respected

This convention implements the following core principles:

- **[Progressive Disclosure](../../principles/content/progressive-disclosure.md)**: Dual-path structure allows learners to choose their entry point based on experience level. By-concept path for gradual learning, by-example path for rapid exploration.
- **[Accessibility First](../../principles/content/accessibility-first.md)**: Multiple learning paths serve diverse learning styles - narrative-driven for methodical learners, code-first for experienced developers.
- **[Explicit Over Implicit](../../principles/software-engineering/explicit-over-implicit.md)**: Clear directory structure makes learning path choices obvious. Foundational tutorials at root signal prerequisite status.

## Purpose

This convention ensures:

- **Consistent Structure**: All programming languages follow same dual-path organization
- **Learner Choice**: Multiple entry points serve different experience levels and learning styles
- **Clear Navigation**: Directory structure signals learning path differences
- **Optional Scalability**: By-example path is optional when demand doesn't justify creation
- **Pedagogical Clarity**: Foundational content (Initial Setup, Quick Start) remains accessible at root level

## Scope

**Applies to:**

- All programming language content under `apps/ayokoding-web/content/[lang]/learn/software-engineering/programming-language/[language]/tutorials/`
- Languages: Java, Elixir, Golang, Kotlin, Python, Rust (and future additions)

**Enforced by:**

- `ayokoding-web-general-checker` (validates by-concept structure)
- `ayokoding-web-by-example-checker` (validates by-example structure)
- `ayokoding-web-structure-checker` (validates directory organization)

## Directory Structure Pattern

### Dual-Path Languages (Java, Elixir, Golang)

Languages with both learning paths:

```
[language]/tutorials/                          # Level 6 folder
├── _index.md                                  # Navigation hub (weight: 100002)
├── by-concept/                                # Narrative-driven path (Level 7 folder)
│   ├── _index.md                              # Navigation hub (weight: 1000000)
│   ├── overview.md                            # Path introduction (weight: 10000000)
│   ├── beginner.md                            # 0-60% coverage (weight: 10000001)
│   ├── intermediate.md                        # 60-85% coverage (weight: 10000002)
│   └── advanced.md                            # 85-95% coverage (weight: 10000003)
├── by-example/                                # Code-first path (Level 7 folder, OPTIONAL)
│   ├── _index.md                              # Navigation hub (weight: 1000001)
│   ├── overview.md                            # Path introduction (weight: 10000000)
│   ├── beginner.md                            # Examples 1-15 (weight: 10000001)
│   ├── intermediate.md                        # Examples 16-35 (weight: 10000002)
│   └── advanced.md                            # Examples 36-60 (weight: 10000003)
├── initial-setup.md                           # Foundational (0-5%, weight: 1000002)
└── quick-start.md                             # Foundational (5-30%, weight: 1000003)
```

### Single-Path Languages (Kotlin, Python, Rust)

Languages with only by-concept path (by-example not yet created):

```
[language]/tutorials/                          # Level 6 folder
├── _index.md                                  # Navigation hub (weight: 100002)
├── by-concept/                                # Narrative-driven path (Level 7 folder)
│   ├── _index.md                              # Navigation hub (weight: 1000000)
│   ├── overview.md                            # Path introduction (weight: 10000000)
│   ├── beginner.md                            # 0-60% coverage (weight: 10000001)
│   ├── intermediate.md                        # 60-85% coverage (weight: 10000002)
│   └── advanced.md                            # 85-95% coverage (weight: 10000003)
├── initial-setup.md                           # Foundational (0-5%, weight: 1000002)
└── quick-start.md                             # Foundational (5-30%, weight: 1000003)
```

## The Two Tutorial Paths

### By Concept Path (Mandatory)

**Purpose**: Deep understanding through comprehensive narrative-driven tutorials

**Characteristics:**

- **Comprehensive explanations** with rationale and context
- **Progressive examples** building on previous concepts
- **Diagrams and visualizations** for complex concepts
- **0-95% coverage** through three levels (beginner 0-60%, intermediate 60-85%, advanced 85-95%)
- **Methodical learning** for deep foundation

**Target Audience:**

- Complete beginners to programming
- Developers wanting deep language understanding
- Learners who prefer narrative explanations
- Building production-ready skills

**File Structure:**

```
by-concept/
├── _index.md        # Navigation hub
├── overview.md      # Explains narrative-driven approach
├── beginner.md      # Fundamentals with detailed explanations
├── intermediate.md  # Production patterns with context
└── advanced.md      # Expert mastery with internals
```

**Content Requirements:**

See [Programming Language Content Standard](./programming-language-content.md) for complete pedagogical requirements including:

- Front hooks ("Want to..." opening paragraphs)
- Learning path visualizations (Mermaid diagrams)
- Prerequisites sections
- Progressive disclosure patterns
- Runnable code examples
- Hands-on exercises
- Cross-references

### By Example Path (Optional)

**Purpose**: Rapid language pickup through heavily annotated code examples

**Characteristics:**

- **Code-first approach** with minimal prose
- **75-90 heavily annotated examples** achieving 95% coverage
- **Self-contained examples** runnable without dependencies
- **Educational comments** showing outputs, states, intermediate values
- **Mermaid diagrams** when appropriate for concept relationships
- **Five-part structure** per example: brief explanation, optional diagram, heavily commented code, key takeaway

**Target Audience:**

- Experienced developers (seasonal programmers, software engineers)
- Already know at least one programming language well
- Want quick language pickup without extensive narrative
- Prefer learning through working code
- Need 90% coverage efficiently

**File Structure:**

```
by-example/
├── _index.md        # Navigation hub
├── overview.md      # Explains code-first approach
├── beginner.md      # Examples 1-15 (basics: syntax, control flow, functions)
├── intermediate.md  # Examples 16-35 (data structures, OOP/functional, modules)
└── advanced.md      # Examples 36-60 (concurrency, metaprogramming, internals)
```

**Content Requirements:**

See [By Example Tutorial Convention](./by-example.md) for complete by-example standards including:

- Five-part example structure
- Self-containment rules
- Educational comment standards (`// =>` notation)
- Coverage progression (0-40%, 40-75%, 75-95%)
- Mermaid diagram usage

**NOT a replacement for:**

- Beginner tutorial (which provides deep explanations for complete beginners)
- Quick Start (which is 5-30% coverage touchpoints)
- Cookbook (which is problem-solving oriented, not learning-oriented)

## Foundational Tutorials at Root

**CRITICAL**: Initial Setup and Quick Start remain at tutorials root level (NOT nested in by-concept or by-example).

**Rationale:**

- **Prerequisites for both paths**: Both by-concept and by-example assume working environment
- **Common entry point**: All learners need installation and basic verification
- **Accessibility**: Root placement signals "start here" before choosing learning path
- **Clarity**: Avoids duplication across paths

**Files:**

```
tutorials/
├── initial-setup.md   # 0-5% coverage: Installation, verification, Hello World
└── quick-start.md     # 5-30% coverage: Core concepts touchpoints
```

**Coverage:**

- **Initial Setup (0-5%)**: Get language working on learner's system
  - Installation instructions (platform-specific)
  - Version verification
  - First "Hello, World!" program
  - Basic tool setup (compiler/interpreter, package manager)
- **Quick Start (5-30%)**: Learn enough to explore independently
  - 8-12 core concepts in order of importance
  - Mermaid learning path diagram
  - Runnable code for each touchpoint
  - Links to by-concept Beginner for depth

## Navigation Pattern

### Navigation Ordering

**CRITICAL**: By Concept and By Example sections appear FIRST in tutorials navigation (before Initial Setup and Quick Start).

**Rationale:**

- **Learning path choice comes first**: Learners choose narrative-driven vs code-first BEFORE starting
- **Prerequisites come second**: After choosing path, learners complete foundational setup
- **Pedagogical progression**: Choice → Setup → Learn

**Navigation Example (Java with dual-path):**

```markdown
<!-- File: tutorials/_index.md -->

- [By Concept](/en/learn/software-engineering/programming-language/java/tutorials/by-concept)
- [By Example](/en/learn/software-engineering/programming-language/java/tutorials/by-example)
- [Initial Setup](/en/learn/software-engineering/programming-language/java/tutorials/initial-setup)
- [Quick Start](/en/learn/software-engineering/programming-language/java/tutorials/quick-start)
```

**Navigation Example (Kotlin with single-path):**

```markdown
<!-- File: tutorials/_index.md -->

- [By Concept](/en/learn/software-engineering/programming-language/kotlin/tutorials/by-concept)
- [Initial Setup](/en/learn/software-engineering/programming-language/kotlin/tutorials/initial-setup)
- [Quick Start](/en/learn/software-engineering/programming-language/kotlin/tutorials/quick-start)
```

### Weight Values

Uses ayokoding-web's level-based weight system with powers of 10 ranges:

**Path Calculation:**

```
/en/ (1) → /learn/ (2) → /software-engineering/ (3) → /programming-language/ (4) → /[language]/ (5) → /tutorials/ (6)
```

**tutorials/ is level 6 folder**:

- `tutorials/_index.md`: `weight: 100002` (level 6 - represents the folder)
- Content INSIDE tutorials/ uses level 7 (1000000, 1000001, 1000002...)

**by-concept/ is level 7 folder** (child of tutorials/):

- `by-concept/_index.md`: `weight: 1000000` (level 7 - first child, represents folder)
- Content INSIDE by-concept/ uses level 8 (10000000, 10000001...)

**by-example/ is level 7 folder** (child of tutorials/):

- `by-example/_index.md`: `weight: 1000001` (level 7 - second child, represents folder)
- Content INSIDE by-example/ uses level 8 (10000000, 10000001...)

**Foundational files are level 7 content** (children of tutorials/):

- `initial-setup.md`: `weight: 1000002` (level 7 - third child)
- `quick-start.md`: `weight: 1000003` (level 7 - fourth child)

**Complete Weight Example (Java dual-path):**

```
tutorials/
├── _index.md                # weight: 100002 (level 6 - represents folder)
├── by-concept/
│   ├── _index.md            # weight: 1000000 (level 7 - first child, represents folder)
│   ├── overview.md          # weight: 10000000 (level 8 - content inside by-concept/)
│   ├── beginner.md          # weight: 10000001 (level 8)
│   ├── intermediate.md      # weight: 10000002 (level 8)
│   └── advanced.md          # weight: 10000003 (level 8)
├── by-example/
│   ├── _index.md            # weight: 1000001 (level 7 - second child, represents folder)
│   ├── overview.md          # weight: 10000000 (level 8 - RESET, different parent)
│   ├── beginner.md          # weight: 10000001 (level 8)
│   ├── intermediate.md      # weight: 10000002 (level 8)
│   └── advanced.md          # weight: 10000003 (level 8)
├── initial-setup.md         # weight: 1000002 (level 7 - third child)
└── quick-start.md           # weight: 1000003 (level 7 - fourth child)
```

**Key Rules:**

1. **Folder's `_index.md`** represents the folder itself at level N → uses level N weight
2. **Content INSIDE folder** is one level deeper → uses level N+1 base weight
3. **Weights RESET per parent**: by-concept/ and by-example/ both start at 10000000 for overview.md (different parents, independent sequences)

See [Hugo Content Convention - ayokoding](../hugo/ayokoding.md) for complete level-based weight system details.

## Optional By-Example Path

**CRITICAL**: The by-example path is OPTIONAL. Not all languages require it.

**When to Create By-Example:**

PASS: **Create when:**

- Demand exists from experienced developers wanting rapid pickup
- Language has sufficient community interest
- Resources available for creating 75-90 annotated examples
- By-concept path is complete (don't create by-example before by-concept)

FAIL: **Skip when:**

- Language is new to platform (focus on by-concept first)
- Limited resources (prioritize by-concept completion)
- Low demand from target audience
- By-concept tutorials already serve experienced developers well

**Current Status (as of 2025-12-27):**

- **With by-example**: Java, Elixir, Golang
- **Without by-example**: Kotlin, Python, Rust

**Migration Path:**

Languages start with by-concept only:

```
1. Create by-concept tutorials (beginner, intermediate, advanced)
2. Monitor demand from experienced developers
3. If demand justifies, create by-example path
4. Update tutorials/_index.md to show both paths
```

## Hugo Requirements

### Frontmatter

All tutorial files follow standard Hugo frontmatter:

```yaml
---
title: "Tutorial Title"
date: 2025-12-27T10:00:00+07:00
draft: false
description: "Brief description for SEO"
weight: [level-based weight]
tags: ["language-name", "tutorial-type", "skill-level"]
---
```

**Rules:**

- **No categories field**: Causes raw text leak in Hextra theme
- **No author field**: Uses site-level config (params.author in hugo.yaml)
- **Date format**: UTC+7 with ISO 8601 format
- **Weight field**: MANDATORY - uses level-based system
- **Tags**: JSON array format `["tag1", "tag2"]` (NOT dash-based YAML)

### Internal Links

**CRITICAL**: All internal links MUST use absolute paths with language prefix.

**Format:**

```markdown
[Display Text](/[language]/learn/software-engineering/programming-language/[language]/tutorials/[path])
```

**Examples:**

```markdown
- [By Concept](/en/learn/software-engineering/programming-language/java/tutorials/by-concept)
- [By Example](/en/learn/software-engineering/programming-language/java/tutorials/by-example)
- [Initial Setup](/en/learn/software-engineering/programming-language/java/tutorials/initial-setup)
- [Beginner Tutorial](/en/learn/software-engineering/programming-language/java/tutorials/by-concept/beginner)
```

**Why absolute paths?**

- Hugo resolves links based on current page context
- Relative paths break when content rendered in different locations
- Absolute paths work from ANY page context
- Language prefix ensures correct bilingual routing

See [Hugo Content Convention - ayokoding](../hugo/ayokoding.md#internal-link-requirements) for complete details.

### Overview Files

Both by-concept/ and by-example/ MUST have overview.md files:

**by-concept/overview.md:**

- Explains narrative-driven learning approach
- Describes comprehensive coverage philosophy
- Links to Programming Language Content Standard
- Sets expectations for deep explanations

**by-example/overview.md:**

- Explains code-first learning approach
- Describes 75-90 annotated examples
- Links to By Example Tutorial Convention
- Sets expectations for experienced developers
- Clarifies NOT a replacement for by-concept

### Index Files

All directories MUST have `_index.md` navigation hubs:

```
tutorials/_index.md         # Lists by-concept/, by-example/, initial-setup, quick-start
by-concept/_index.md        # Lists overview, beginner, intermediate, advanced
by-example/_index.md        # Lists overview, beginner, intermediate, advanced
```

**Navigation Pattern**: 2-layer depth with complete coverage (show all immediate children).

## Examples

### Example 1: Java (Dual-Path Language)

**Complete Structure:**

```
java/tutorials/
├── _index.md                # "Tutorials" (weight: 100002)
├── by-concept/
│   ├── _index.md            # "By Concept" (weight: 1000000)
│   ├── overview.md          # "Overview" (weight: 10000000)
│   ├── beginner.md          # "Beginner Tutorial" (weight: 10000001)
│   ├── intermediate.md      # "Intermediate Tutorial" (weight: 10000002)
│   └── advanced.md          # "Advanced Tutorial" (weight: 10000003)
├── by-example/
│   ├── _index.md            # "By Example" (weight: 1000001)
│   ├── overview.md          # "Overview" (weight: 10000000)
│   ├── beginner.md          # "Beginner Examples" (weight: 10000001)
│   ├── intermediate.md      # "Intermediate Examples" (weight: 10000002)
│   └── advanced.md          # "Advanced Examples" (weight: 10000003)
├── initial-setup.md         # "Initial Setup" (weight: 1000002)
└── quick-start.md           # "Quick Start" (weight: 1000003)
```

**Navigation (`tutorials/_index.md`):**

```markdown
---
title: Tutorials
weight: 100002
---

- [By Concept](/en/learn/software-engineering/programming-language/java/tutorials/by-concept)
- [By Example](/en/learn/software-engineering/programming-language/java/tutorials/by-example)
- [Initial Setup](/en/learn/software-engineering/programming-language/java/tutorials/initial-setup)
- [Quick Start](/en/learn/software-engineering/programming-language/java/tutorials/quick-start)
```

### Example 2: Kotlin (Single-Path Language)

**Complete Structure:**

```
kotlin/tutorials/
├── _index.md                # "Tutorials" (weight: 100002)
├── by-concept/
│   ├── _index.md            # "By Concept" (weight: 1000000)
│   ├── overview.md          # "Overview" (weight: 10000000)
│   ├── beginner.md          # "Beginner Tutorial" (weight: 10000001)
│   ├── intermediate.md      # "Intermediate Tutorial" (weight: 10000002)
│   └── advanced.md          # "Advanced Tutorial" (weight: 10000003)
├── initial-setup.md         # "Initial Setup" (weight: 1000001)
└── quick-start.md           # "Quick Start" (weight: 1000002)
```

**Navigation (`tutorials/_index.md`):**

```markdown
---
title: Tutorials
weight: 100002
---

- [By Concept](/en/learn/software-engineering/programming-language/kotlin/tutorials/by-concept)
- [Initial Setup](/en/learn/software-engineering/programming-language/kotlin/tutorials/initial-setup)
- [Quick Start](/en/learn/software-engineering/programming-language/kotlin/tutorials/quick-start)
```

**Note**: When Kotlin gains by-example path, `initial-setup.md` weight changes from 1000001 to 1000002, `quick-start.md` from 1000002 to 1000003.

## Validation

### Automated Validation

**ayokoding-web-general-checker** validates:

- PASS: By-concept directory structure exists
- PASS: All mandatory files present (\_index.md, overview.md, beginner/intermediate/advanced.md)
- PASS: Weight values follow level-based system
- PASS: Internal links use absolute paths
- PASS: Frontmatter completeness
- PASS: No H1 headings in content

**ayokoding-web-by-example-checker** validates:

- PASS: By-example directory structure (when exists)
- PASS: 75-90 examples across three files
- PASS: Five-part example structure
- PASS: Self-containment rules
- PASS: Educational comment standards
- PASS: Coverage progression

**ayokoding-web-structure-checker** validates:

- PASS: Navigation ordering (by-concept/by-example first, then setup/quick-start)
- PASS: 2-layer navigation depth with complete coverage
- PASS: Index files exist and are properly structured
- PASS: Weight value consistency

### Manual Verification Checklist

Before publishing new language tutorials:

- [ ] By-concept directory with all required files
- [ ] Overview files explain learning approach
- [ ] Beginner/intermediate/advanced tutorials exist
- [ ] Initial Setup and Quick Start at root level
- [ ] Navigation lists paths in correct order (by-concept/by-example first)
- [ ] Weight values follow level-based system
- [ ] All links use absolute paths with language prefix
- [ ] Frontmatter complete and correct
- [ ] No categories field in frontmatter
- [ ] Tags use JSON array format
- [ ] If by-example exists: 75-90 examples across three files
- [ ] If by-example exists: Five-part structure per example
- [ ] Cross-references to Programming Language Content Standard

## Common Mistakes

FAIL: **Mistake 1: Nesting foundational tutorials in by-concept/**

```
# WRONG!
by-concept/
├── initial-setup.md   # Should be at tutorials/ root
└── quick-start.md     # Should be at tutorials/ root
```

PASS: **Correct: Foundational at root**

```
# RIGHT!
tutorials/
├── by-concept/
├── initial-setup.md   # At root - prerequisite for both paths
└── quick-start.md     # At root - prerequisite for both paths
```

---

FAIL: **Mistake 2: Wrong navigation order**

```markdown
# WRONG! Setup/quick-start before learning paths

- [Initial Setup](/en/.../initial-setup)
- [Quick Start](/en/.../quick-start)
- [By Concept](/en/.../by-concept)
- [By Example](/en/.../by-example)
```

PASS: **Correct: Learning paths first**

```markdown
# RIGHT! Learning path choice comes first

- [By Concept](/en/.../by-concept)
- [By Example](/en/.../by-example)
- [Initial Setup](/en/.../initial-setup)
- [Quick Start](/en/.../quick-start)
```

---

FAIL: **Mistake 3: Using relative paths**

```markdown
# WRONG! Relative paths break from different page contexts

- [Beginner](by-concept/beginner)
- [Examples](./by-example/beginner)
```

PASS: **Correct: Absolute paths**

```markdown
# RIGHT! Absolute paths work from any context

- [Beginner](/en/learn/software-engineering/programming-language/java/tutorials/by-concept/beginner)
- [Examples](/en/learn/software-engineering/programming-language/java/tutorials/by-example/beginner)
```

---

FAIL: **Mistake 4: Creating by-example before by-concept**

```
# WRONG! By-example created first
java/tutorials/
├── by-example/          # Created first
└── initial-setup.md     # Missing by-concept/
```

PASS: **Correct: By-concept foundation first**

```
# RIGHT! By-concept foundation, then optional by-example
java/tutorials/
├── by-concept/          # Created first (mandatory)
├── by-example/          # Created later (optional)
├── initial-setup.md
└── quick-start.md
```

---

FAIL: **Mistake 5: Missing overview.md files**

```
# WRONG! No overview explaining learning approach
by-concept/
├── _index.md
├── beginner.md          # Missing overview.md
├── intermediate.md
└── advanced.md
```

PASS: **Correct: Overview explains path**

```
# RIGHT! Overview sets expectations
by-concept/
├── _index.md
├── overview.md          # Explains narrative-driven approach
├── beginner.md
├── intermediate.md
└── advanced.md
```

## Migration Guide

### Adding By-Example to Existing Language

If a language currently has only by-concept, follow these steps to add by-example:

**Step 1: Create directory structure**

```bash
cd apps/ayokoding-web/content/en/learn/software-engineering/programming-language/[language]/tutorials/
mkdir -p by-example
```

**Step 2: Create files with weights**

```bash
# Navigation hub
touch by-example/_index.md         # weight: 1000001

# Content files
touch by-example/overview.md       # weight: 10000000
touch by-example/beginner.md       # weight: 10000001
touch by-example/intermediate.md   # weight: 10000002
touch by-example/advanced.md       # weight: 10000003
```

**Step 3: Update tutorials/\_index.md**

Add by-example link BEFORE initial-setup/quick-start:

```markdown
- [By Concept](/en/.../by-concept)
- [By Example](/en/.../by-example) # NEW!
- [Initial Setup](/en/.../initial-setup)
- [Quick Start](/en/.../quick-start)
```

**Step 4: Update foundational file weights**

```bash
# initial-setup.md: change weight from 1000001 to 1000002
# quick-start.md: change weight from 1000002 to 1000003
```

**Step 5: Write content**

Follow [By Example Tutorial Convention](./by-example.md) to create 75-90 annotated examples.

**Step 6: Validate**

Run `ayokoding-web-by-example-checker` to verify structure and content quality.

## Related Conventions

- **[Programming Language Content Standard](./programming-language-content.md)** - Universal content architecture for programming languages (5 tutorial levels, coverage philosophy, quality metrics, pedagogical patterns)
- **[By Example Tutorial Convention](./by-example.md)** - Complete standards for creating code-first by-example tutorials (five-part structure, self-containment, educational comments, coverage progression)
- **[Hugo Content Convention - ayokoding](../hugo/ayokoding.md)** - Hextra theme specifics (level-based weights, absolute paths, navigation depth, frontmatter requirements)
- **[Tutorial Naming Convention](./naming.md)** - Tutorial type definitions (Initial Setup, Quick Start, Beginner, Intermediate, Advanced coverage percentages)
- **[Content Quality Principles](../content/quality.md)** - Universal markdown quality standards (active voice, heading hierarchy, accessibility)
- **[Diátaxis Framework](../meta/diataxis-framework.md)** - Documentation categorization (tutorials vs how-to vs reference vs explanation)

## Version History

- **v1.0** (2025-12-27): Initial convention based on Java/Elixir/Golang dual-path implementations and Kotlin/Python/Rust single-path implementations

## Tutorial Folder Arrangement Standard

**CRITICAL**: All topics with by-example tutorials MUST follow this specific arrangement order:

1. **overview** (weight: 100000)
2. **initial-setup** (weight: 100001)
3. **quick-start** (weight: 100002)
4. **by-example** (weight: 100003)
5. **by-concept** (weight: 100004) - OPTIONAL (only for topics that originally had it)

**Key Rules**:

- **Manual arrangement**: Tutorial structure is arranged MANUALLY by content creators, NOT automatically by agents
- **NO automatic "by pedagogical" ordering**: Agents should NOT enforce automatic ordering patterns beyond this standard arrangement
- **by-concept is optional**: Not all topics require by-concept path (some may have only by-example)
- **Consistent weight values**: Use the exact weight values shown above for predictable navigation ordering

**Rationale**:

- **Learner-first ordering**: Overview → Setup → Quick touchpoints → Example-driven learning → Narrative-driven learning
- **Progressive complexity**: Foundational setup before learning paths, code examples before deep explanations
- **Flexibility**: by-concept optional allows topics to provide by-example-only when appropriate
- **Explicit control**: Content creators manually arrange structure based on pedagogical goals

**Examples Across Content Types**:

**Programming Languages** (Java, Golang, Elixir):

```
tutorials/
├── _index.md                # weight: 100002
├── overview.md              # weight: 100000
├── initial-setup.md         # weight: 100001
├── quick-start.md           # weight: 100002
├── by-example/              # weight: 100003
└── by-concept/              # weight: 100004
```

**Infrastructure Tools** (Ansible, Terraform):

```
tutorials/
├── _index.md                # weight: 100002
├── overview.md              # weight: 100000
├── initial-setup.md         # weight: 100001
├── quick-start.md           # weight: 100002
├── by-example/              # weight: 100003
└── by-concept/              # weight: 100004 (OPTIONAL)
```

**Data Tools** (PostgreSQL, Redis):

```
tutorials/
├── _index.md                # weight: 100002
├── overview.md              # weight: 100000
├── initial-setup.md         # weight: 100001
├── quick-start.md           # weight: 100002
├── by-example/              # weight: 100003
└── by-concept/              # weight: 100004 (OPTIONAL)
```

**Platforms** (AWS, Google Cloud):

```
tutorials/
├── _index.md                # weight: 100002
├── overview.md              # weight: 100000
├── initial-setup.md         # weight: 100001
├── quick-start.md           # weight: 100002
├── by-example/              # weight: 100003
└── by-concept/              # weight: 100004 (OPTIONAL)
```

**Agent Responsibilities**:

- **Validation**: Agents SHOULD validate that tutorial folders follow this standard arrangement
- **NO automatic reordering**: Agents MUST NOT automatically reorder tutorials into "pedagogical" patterns
- **Respect manual arrangement**: Content creators control the structure intentionally
