---
title: "Emoji Usage Convention"
description: Standards for semantic emoji usage to enhance document scannability and engagement with accessible colored emojis
category: explanation
subcategory: conventions
tags:
  - emoji
  - accessibility
  - scannability
  - conventions
  - markdown
created: 2025-12-04
updated: 2025-12-04
---

# Emoji Usage Convention

## 📋 Overview

This document defines conventions for emoji usage in markdown documentation across the Open Sharia Enterprise repository. Emojis serve as **semantic visual markers** that enhance document scannability and engagement while maintaining professionalism.

## Principles Implemented/Respected

This convention implements the following core principles:

- **[Accessibility First](../../principles/content/accessibility-first.md)**: Uses color-blind friendly emoji colors (blue, orange, teal, purple, brown). Emojis supplement text headings, never replace them. Semantic meaning is always conveyed through text first, emoji second.

- **[Explicit Over Implicit](../../principles/software-engineering/explicit-over-implicit.md)**: Defines a standardized emoji vocabulary where each emoji has explicit, documented meaning. Same emoji = same meaning across all docs. No guessing or implicit conventions.

## Scope

### What This Convention Covers

- **Where emojis are allowed** - docs/, READMEs, plans/, .claude/agents/README.md
- **Where emojis are forbidden** - CLAUDE.md, agent prompts, config files, source code
- **Semantic emoji usage** - Using emojis for meaning, not decoration
- **Emoji consistency** - Standard emojis for common concepts
- **Accessibility considerations** - How emojis affect screen readers

### What This Convention Does NOT Cover

- **Emoji rendering** - Platform-specific emoji display (implementation detail)
- **Custom emojis** - Creating custom emoji sets
- **Emoji in commit messages** - Git commit formatting covered separately
- **Emoji alternatives** - When emojis aren't available (fallback text)

## 🎯 Purpose

Emojis in documentation should:

1. **Enhance scannability** - Help readers quickly locate content types
2. **Add semantic meaning** - Reinforce the purpose of sections
3. **Improve engagement** - Make long documentation more visually interesting
4. **Maintain consistency** - Same emoji = same meaning across all docs

Emojis should **NOT**:

- Be purely decorative without semantic value
- Replace clear text headings
- Appear in code, commands, or technical specifications
- Be overused (causing visual noise)

## 📚 Emoji Vocabulary

### Document Type Markers

Use at the start of section headings to indicate content category:

| Emoji | Meaning                      | Usage                                        |
| ----- | ---------------------------- | -------------------------------------------- |
| 📋    | **Overview/Summary**         | High-level summaries, document overviews     |
| 🎯    | **Purpose/Goals/Objectives** | Why something exists, objectives             |
| 💡    | **Key Concepts/Ideas**       | Important conceptual information             |
| 📚    | **Documentation/Resources**  | References to other docs, learning resources |
| 📖    | **Reference**                | Technical specifications, API docs           |
| 🛠️    | **How-To/Tools**             | Practical guides, tool usage                 |
| 🧪    | **Examples/Demos**           | Code examples, demonstrations                |

### Status and Signal Markers

Use to indicate state, warnings, or validation:

| Emoji | Meaning                                            | Usage                                                                         |
| ----- | -------------------------------------------------- | ----------------------------------------------------------------------------- |
| ✅    | **Correct/Working/Approved/Advantages/Pros**       | Best practices, correct examples, completed items, advantages, benefits, pros |
| ❌    | **Incorrect/Broken/Deprecated/Disadvantages/Cons** | Anti-patterns, wrong examples, errors, disadvantages, limitations, cons       |
| ⚠️    | **Warning/Caution**                                | Security concerns, breaking changes, important notes                          |
| 🚧    | **Work in Progress**                               | Incomplete features, under construction                                       |
| 🆕    | **New/Recently Added**                             | New features, recent changes                                                  |
| 🔜    | **Coming Soon/Planned**                            | Future features, roadmap items                                                |
| 🗑️    | **Deprecated/To Remove**                           | Outdated features, planned removals                                           |

### Action and Process Markers

Use to indicate steps, actions, or processes:

| Emoji | Meaning                       | Usage                                   |
| ----- | ----------------------------- | --------------------------------------- |
| 🚀    | **Quick Start/Initial Setup** | Onboarding, initial setup               |
| 🔧    | **Configuration/Setup**       | Configuration steps, setup instructions |
| ⚙️    | **Settings/Options**          | Configuration options, parameters       |
| 🔍    | **Deep Dive/Investigation**   | Detailed explanations, debugging        |
| 📊    | **Metrics/Analysis**          | Data, statistics, measurements          |
| 🔄    | **Process/Workflow**          | Multi-step processes, workflows         |
| 🔗    | **Links/Connections**         | Related content, cross-references       |
| 📝    | **Notes/Documentation**       | Additional information, footnotes       |

### Domain-Specific Markers

Use for specific technical domains:

| Emoji | Meaning                   | Usage                                   |
| ----- | ------------------------- | --------------------------------------- |
| 🔒    | **Security**              | Security considerations, authentication |
| 🧪    | **Testing**               | Test cases, testing strategies          |
| 🏗️    | **Architecture**          | System design, architectural decisions  |
| 🌐    | **API/Network**           | API documentation, network concepts     |
| 💾    | **Data/Storage**          | Database, data structures               |
| 🎨    | **UI/Frontend**           | User interface, styling                 |
| ⚡    | **Performance**           | Optimization, speed improvements        |
| 📦    | **Dependencies/Packages** | External libraries, modules             |

### Domain-Specific: Enterprise and Financial Services

Use for enterprise and financial services content:

| Emoji | Meaning                    | Usage                                      |
| ----- | -------------------------- | ------------------------------------------ |
| 💰    | **Finance/Money**          | Financial concepts, transactions           |
| 🏦    | **Banking**                | Banking operations, accounts               |
| 💳    | **Payments**               | Payment processing, cards                  |
| 📈    | **Analytics/Growth**       | Financial analytics, metrics               |
| ⚖️    | **Compliance/Legal**       | Regulatory compliance, legal requirements  |
| 🕌    | **Sharia/Islamic Finance** | Sharia-compliant features, Islamic banking |

### Domain-Specific: AI Agents

Use for AI agent categorization in `.claude/agents/README.md`:

| Emoji | Meaning                              | Usage                                                   |
| ----- | ------------------------------------ | ------------------------------------------------------- |
| 🟦    | **Writer/Creator Agents (Blue)**     | Agents that create or write content (docs, plans, etc.) |
| 🟩    | **Checker/Validator Agents (Green)** | Agents that validate or check consistency               |
| 🟨    | **Updater/Modifier Agents (Yellow)** | Agents that update or modify existing content           |
| 🟪    | **Implementor Agents (Purple)**      | Agents that execute or implement plans                  |

**Note:** These colored square emojis are ONLY used in `.claude/agents/README.md` to visually categorize agents by role. They match the `color` field in agent frontmatter. See [AI Agents Convention](../development/agents/ai-agents.md) for complete details on agent color categorization.

**Color Accessibility:** All four colors (blue, green, yellow, purple) are from the verified accessible palette and work for all types of color blindness (protanopia, deuteranopia, tritanopia). These emojis are SUPPLEMENTARY to text labels - agents are primarily identified by their name, role suffix, and description, not by color alone. See [Color Accessibility Convention](./color-accessibility.md) for complete details.

## 🎨 Color Accessibility for Colored Emojis

**Master Reference**: All colored emoji usage MUST follow the [Color Accessibility Convention](./color-accessibility.md) - the authoritative source for accessible color palette, WCAG standards, and testing methodology. This section provides emoji-specific guidance.

**Where colored emojis (like 🟦🟩🟨🟪) are used for visual categorization, ensure:**

1. **Colors are SUPPLEMENTARY to semantic information**
   - Primary identification relies on non-color factors (shape, text label, context)
   - Color enhances visual scannability but is never the sole identifier

2. **All colors used are from the verified accessible palette**
   - Blue (#0173B2), Orange (#DE8F05), Teal (#029E73), Purple (#CC78BC), Brown (#CA9161)
   - See [Color Accessibility Convention](./color-accessibility.md) for complete palette details, WCAG compliance verification, and testing tools

3. **Users with color blindness can still identify items by shape/text alone**
   - Square emoji shape (🟦) is distinct from other emoji shapes
   - Text labels ("Writer", "Checker", "Updater") provide semantic meaning
   - Context (placement next to agent names) provides additional cues

4. **Never rely on color alone for categorization**
   - Always combine color with text labels
   - Always combine color with shape differentiation
   - Always provide context through surrounding text

**Example of accessible colored emoji usage:**

✅ **Good - Color + Text + Shape:**

```markdown
### 🟦 `docs__maker.md`

Expert documentation writer specializing in Obsidian-optimized markdown.
```

**Why this works:**

- Color: Blue square (accessible color from verified palette)
- Shape: Square emoji (distinguishable shape)
- Text: "docs\_\_maker.md" (primary identifier)
- Description: "Expert documentation writer..." (semantic meaning)

❌ **Bad - Color only:**

```markdown
### 🟦

Agent for documentation
```

**Why this fails:**

- No text label to identify specific agent
- Relies solely on color and shape
- No semantic context provided

For complete color accessibility guidelines including WCAG standards, testing tools, and research sources, see [Color Accessibility Convention](./color-accessibility.md).

## ✅ Usage Rules

### Rule 1: Semantic Consistency

**Each emoji must have a single, consistent meaning across all documents.**

✅ **Correct:**

```markdown
## 🔒 Security Considerations

## 🔒 Authentication

## 🔒 Authorization
```

❌ **Incorrect:**

```markdown
## 🔒 Security Considerations

## 🔐 Authentication <!-- Don't use different security emojis -->

## 🛡️ Authorization <!-- Stick to one emoji per concept -->
```

### Rule 2: Restraint and Balance

**Use 1-2 emojis per section. Avoid emoji overload.**

✅ **Correct:**

```markdown
## 🎯 Purpose

This section explains the core objectives...

## ✅ Best Practices

1. Configure for your stack
2. Tune rules
3. Set thresholds
```

❌ **Incorrect:**

```markdown
## 🎯 Purpose 🚀 💡

This section explains 📝 the core objectives... ⭐

## ✅ Best Practices 🔧 ⚙️ 🛠️

1. 🔧 Configure 🎨 for your stack 💻
2. ⚙️ Tune 🎯 rules 📏
3. 📊 Set 🔢 thresholds ⚡
```

### Rule 3: Heading-Level Placement

**Place emojis at the start of headings (H2, H3, H4), not inline in body text.**

✅ **Correct:**

```markdown
## 🔧 Configuration

Configure the application by editing...
```

❌ **Incorrect:**

```markdown
## Configuration

Configure 🔧 the application by editing... 🎯
```

**Exception:** Status indicators (✅ ❌ ⚠️) can be used inline for examples or lists.

### Rule 4: No Emojis in Technical Content

**Never use emojis in code blocks, commands, file paths, or technical specifications.**

✅ **Correct:**

```markdown
## 🚀 Quick Start

Install dependencies:
\`\`\`bash
npm install
npm run dev
\`\`\`
```

❌ **Incorrect:**

```markdown
## 🚀 Quick Start

Install dependencies:
\`\`\`bash
npm install 📦
npm run dev 🚀
\`\`\`
```

### Rule 5: Accessibility Consideration

**Use emojis that enhance, not replace, text meaning. Screen readers will read emoji alt text.**

✅ **Correct:**

```markdown
## ⚠️ Security Warning

This feature has security implications...
```

❌ **Incorrect:**

```markdown
## ⚠️

This feature has security implications... <!-- Heading must have text -->
```

### Rule 6: No Emojis in Frontmatter or Metadata

**Keep YAML frontmatter, file names, and metadata emoji-free.**

✅ **Correct:**

```yaml
---
title: Security Best Practices
category: explanation
---
```

❌ **Incorrect:**

```yaml
---
title: 🔒 Security Best Practices
category: explanation
---
```

### Rule 7: Scope - Where to Use Emojis

**Emojis enhance scannability and engagement in human-readable files.**

**✅ USE emojis in these files:**

1. **All documentation** - `docs/**/*.md`
   - Explanations, tutorials, how-tos, reference
   - Conventions, development docs

2. **All README files** - `**/README.md`
   - Root README.md
   - Index files in any directory (human-oriented overviews)
   - Including `.claude/agents/README.md` (agent index for humans)

3. **Planning documents** - `plans/**/*.md`
   - Project plans, requirements, technical docs
   - Human-readable working documents

4. **Agent configuration files** - CLAUDE.md, .claude/agents/_.md, .opencode/agent/_.md
   - CLAUDE.md - Human-readable navigation document (~30,000 lines) for developers
   - .claude/agents/\*.md - Agent definitions read by developers to understand agent behavior
   - .opencode/agent/\*.md - OpenCode agent definitions (same purpose as Claude Code agents)
   - Emojis enhance scannability for:
     - Criticality level definitions (🔴 CRITICAL, 🟠 HIGH, 🟡 MEDIUM, 🟢 LOW)
     - Section headers (🎯 Purpose, 💡 Key Concepts, 📖 Reference)
     - Status indicators in examples (✅ Correct, ❌ Incorrect, ⚠️ Warning)

**❌ DO NOT use emojis in these files:**

1. **Configuration files**
   - `*.json`, `*.yaml`, `*.toml`
   - `package.json`, `tsconfig.json`, etc.
   - `.gitignore`, `.gitattributes`
   - `.github/workflows/*.yml`

**Rationale:**

**Enhanced scannability:**

- CLAUDE.md is a human-readable navigation document (~30,000 lines) that benefits from emoji-enhanced scannability
- Agent files are human-readable specifications - developers read them to understand behavior, patterns, workflows
- Emojis provide semantic visual markers that help developers quickly locate sections (criticality, purpose, references)

**Consistency with referenced content:**

- Agent files reference Skills and conventions that use emojis (e.g., criticality definitions with 🔴🟠🟡🟢)
- Agent definitions should be visually consistent with their referenced content
- When agents display emoji-based definitions in their own documentation, it maintains semantic consistency

**Why agent files now get emojis:**

- Agent files are specifications for both humans (developers) AND AI (execution)
- Developers read agent files to understand behavior, patterns, and workflows
- Emojis enhance scannability without changing agent execution logic
- Similar to how docs/\*_/_.md use emojis for human scannability

✅ **Clear rule:**

```
Emojis for humans: docs/, plans/, README.md files
Emojis for agents: CLAUDE.md, .claude/agents/*.md, .opencode/agent/*.md
No emojis for machines: config files (*.json, *.yaml, *.toml)
```

## 📖 Document Type Specific Guidelines

### Tutorials (`docs/tutorials/`)

**Goal:** Guide learners step-by-step

**Recommended emojis:**

- 🚀 Quick Start sections
- 📝 Prerequisites
- 🔧 Setup steps
- ✅ Verification steps
- 🎯 Learning objectives
- 💡 Key concepts

**Example:**

```markdown
# Initial Setup for SAST

## 🎯 Learning Objectives

By the end of this tutorial, you will:

- Understand what SAST is
- Configure SonarQube
- Run your first scan

## 📝 Prerequisites

- Node.js 18+
- npm 9+

## 🚀 Quick Start

### 1. 🔧 Install SonarQube

...
```

### How-To Guides (`docs/how-to/`)

**Goal:** Solve specific problems

**Recommended emojis:**

- 🎯 Problem statement
- 🔧 Solution steps
- ✅ Success criteria
- ⚠️ Common pitfalls
- 💡 Tips and tricks

**Example:**

```markdown
# How to Integrate SAST in CI/CD

## 🎯 Problem

You need to automatically scan code for security vulnerabilities...

## 🔧 Solution

### Step 1: Configure SonarQube

...

## ⚠️ Common Pitfalls

- Don't run SAST on every commit...
```

### Reference (`docs/reference/`)

**Goal:** Provide technical specifications

**Recommended emojis:**

- 📖 Main reference sections
- ⚙️ Configuration options
- 🌐 API endpoints
- 📊 Parameters and return values
- 🔗 Related references

**Example:**

```markdown
# SAST Tools Reference

## 📖 SonarQube

### ⚙️ Configuration Options

| Option           | Type   | Description |
| ---------------- | ------ | ----------- |
| `sonar.host.url` | string | Server URL  |

### 🌐 API Endpoints

...
```

### Explanation (`docs/explanation/`)

**Goal:** Explain concepts and decisions

**Recommended emojis:**

- 💡 Key concepts
- 🎯 Purpose and rationale
- 🏗️ Architecture
- 🔍 Deep dives
- ✅ Advantages
- ❌ Disadvantages
- 📊 Comparisons

**Example:**

```markdown
# SAST Explanation

## 💡 Core Concept

SAST analyzes code without executing it...

## 🎯 Why Use SAST

...

## ✅ Advantages

- Early detection
- Complete coverage

## ❌ Limitations

- False positives
- No runtime context
```

### Plans (`plans/`)

**Goal:** Project planning and tracking

**Recommended emojis:**

- 🎯 Objectives
- 📋 Requirements
- 🏗️ Architecture
- 🔄 Workflow
- ✅ Completed milestones
- 🚧 In-progress work
- 🔜 Upcoming tasks
- ⚠️ Risks and blockers

**Example:**

```markdown
# Project: Authentication System

## 🎯 Objectives

Implement secure user authentication...

## 📋 Requirements

### ✅ Completed

- User registration

### 🚧 In Progress

- Password reset

### 🔜 Planned

- OAuth integration

## ⚠️ Risks

- Third-party OAuth provider rate limits
```

### CLAUDE.md and README.md (Root Files)

**Goal:** Repository overview and AI guidance

**Recommended emojis:**

- 📋 Overview sections
- 🎯 Project goals
- 🚀 Quick start
- 🔧 Setup instructions
- 📚 Documentation links
- ⚠️ Important notices
- 🔗 External links

**Example:**

```markdown
# Open Sharia Enterprise

## 📋 Overview

An enterprise platform...

## 🚀 Quick Start

\`\`\`bash
npm install
npm run dev
\`\`\`

## 📚 Documentation

- [Conventions](../)
- [Development](../../development/)

## ⚠️ Important

Do not commit changes unless explicitly instructed.
```

## 🔍 Migration Strategy

### Updating Existing Documents

When adding emojis to existing documentation:

1. **Start with headings** - Add emojis to H2/H3 headings first
2. **Follow the vocabulary** - Use only emojis from the defined vocabulary
3. **Maintain consistency** - Same emoji for same concept across documents
4. **Don't overdo it** - If a section doesn't benefit from an emoji, skip it
5. **Test scannability** - Review the document to ensure emojis improve (not hinder) navigation

### Phased Rollout

**Phase 1: Core Documentation** (Immediate)

- Update convention documents in `rules/conventions/`
- Update README.md files (root and `.claude/agents/README.md`)
- **Skip** CLAUDE.md and agent prompt files (AI instructions)

**Phase 2: Explanation Docs** (Next)

- Update explanation documents in `docs/explanation/`
- Focus on frequently read documents first

**Phase 3: Reference and How-To** (Later)

- Update reference documentation
- Update how-to guides
- Update tutorials

**Phase 4: Plans and Historical Content** (As Needed)

- Update active plans in `plans/in-progress/`
- Update archived plans as they are revisited
- Not urgent for completed/archived content

## ✅ Validation Checklist

When reviewing emoji usage, verify:

- [ ] Emojis are from the defined vocabulary
- [ ] Same emoji = same meaning throughout document
- [ ] 1-2 emojis per section maximum
- [ ] Emojis only in headings (except status indicators)
- [ ] No emojis in code blocks, commands, or file paths
- [ ] No emojis in frontmatter or metadata
- [ ] No emojis in CLAUDE.md (AI instructions)
- [ ] No emojis in agent prompt files `.claude/agents/*.md` (except README.md)
- [ ] Emojis ARE used in README.md files (human-oriented indices)
- [ ] Emojis ARE used in docs/ and plans/ (human documentation)
- [ ] Headings still make sense without emoji (accessibility)
- [ ] Emojis enhance scannability and engagement

## 🔗 Related Conventions

- [File Naming Convention](../meta/file-naming.md)
- [Linking Convention](./linking.md)
- [Diátaxis Framework](../meta/diataxis-framework.md)
- [AI Agents Convention](../development/agents/ai-agents.md) - For agent color categorization using colored square emojis
- [Color Accessibility Convention](./color-accessibility.md) - For accessible color palette and WCAG standards

## 📝 Notes

### Why These Specific Emojis?

The emoji vocabulary was chosen based on:

1. **Universal recognition** - Emojis with clear, consistent meanings
2. **Professional context** - Appropriate for technical/enterprise documentation
3. **Accessibility** - Screen reader friendly with clear alt text
4. **Render consistency** - Display consistently across platforms (GitHub, Obsidian, VS Code)

### Cultural Considerations

While emojis generally have universal meanings, we've avoided:

- Hand gestures (can have different cultural meanings)
- Flags (potentially political)
- Food/animals (may not render consistently)
- Faces (except for status like ✅ ❌ ⚠️)

### When in Doubt

If unsure whether to use an emoji:

1. Ask: "Does this emoji add semantic meaning or just decoration?"
2. If decoration → skip it
3. If semantic → check if it's in the vocabulary
4. If not in vocabulary → consider if it should be added (propose via PR/issue)

---

**Last Updated:** 2025-12-04
**Status:** 🆕 New Convention
