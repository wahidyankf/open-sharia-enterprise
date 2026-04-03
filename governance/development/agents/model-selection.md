---
title: "AI Agent Model Selection Convention"
description: Standards for selecting the appropriate model tier (opus, sonnet, haiku) for AI agents based on task complexity
category: explanation
subcategory: development
tags:
  - ai-agents
  - model-selection
  - standards
  - development
created: 2026-04-03
updated: 2026-04-03
---

# AI Agent Model Selection Convention

This document defines the standards for selecting the appropriate model tier when creating or updating AI agents. The governing principle is **match model capability to task complexity** -- use the most capable model only when the task demands it, and use lighter models for structured or mechanical work.

## Principles Implemented/Respected

This practice implements the following core principles:

- **[Simplicity Over Complexity](../../principles/general/simplicity-over-complexity.md)**: Select the simplest model that can accomplish the task. Avoid using opus-tier reasoning for tasks that follow fixed patterns or templates. Simpler models reduce latency and resource consumption without sacrificing quality on structured work.

- **[Explicit Over Implicit](../../principles/software-engineering/explicit-over-implicit.md)**: Every agent MUST declare its model tier in frontmatter and include a `Model Selection Justification` comment explaining why that tier was chosen. No implicit defaults -- the reasoning is transparent and auditable.

- **[Deliberate Problem-Solving](../../principles/general/deliberate-problem-solving.md)**: Model selection requires deliberate analysis of what cognitive capabilities the task demands. Agents should not default to the highest tier "just in case" -- each selection reflects a considered judgment about the task's actual requirements.

## Conventions Implemented/Respected

This practice respects the following conventions:

- **[Content Quality Principles](../../conventions/writing/quality.md)**: Agent frontmatter and model justification comments follow active voice and clarity standards.

## Purpose

Model selection directly affects agent quality, latency, and resource efficiency. Selecting too powerful a model wastes resources on simple tasks; selecting too weak a model produces poor results on complex work. This convention establishes clear criteria for matching model tiers to task types, ensuring consistent and justified model assignments across all agents.

## Scope

### What This Convention Covers

- Model tier definitions and their cognitive capabilities
- Decision criteria for selecting each tier
- Task-to-tier mapping with concrete examples
- Justification requirements for model selection

### What This Convention Does NOT Cover

- Tool permission selection (see [AI Agents Convention](./ai-agents.md))
- Agent color categorization (see [AI Agents Convention](./ai-agents.md))
- Agent naming and file structure (see [AI Agents Convention](./ai-agents.md))
- Workflow orchestration (see [Agent Workflow Orchestration](./agent-workflow-orchestration.md))

## Model Tiers

### Opus (Inherit / No Model Specified)

**When to use**: Tasks requiring creative reasoning, architectural decisions, code generation, multi-step judgment calls, or nuanced content creation.

**Cognitive profile**: Deep analytical reasoning, novel problem-solving, multi-step planning, creative synthesis across domains, nuanced judgment under ambiguity.

**Task characteristics**:

- Open-ended problems without a single correct answer
- Architectural decisions requiring trade-off analysis
- Code generation across multiple languages and paradigms
- Content creation requiring domain expertise and originality
- Multi-step planning with conditional branching
- Tasks where the agent must invent approaches, not follow templates

**Agent examples**:

- **SWE developers** (all language-specific agents) -- generate and refactor production code across diverse language ecosystems, requiring deep understanding of idioms, patterns, and trade-offs
- **plan-maker** -- creates project plans requiring scope analysis, dependency mapping, and strategic sequencing
- **plan-executor** -- executes complex plans with conditional logic, error recovery, and adaptive decision-making
- **Content makers** (docs-maker, docs-tutorial-maker, apps-\*-maker, swe-ui-maker) -- produce original content requiring domain knowledge, pedagogical judgment, and audience awareness
- **agent-maker** -- designs new agents requiring understanding of the governance architecture, tool permissions, and agent ecosystem
- **repo-governance-maker** -- creates governance documents requiring architectural reasoning about layer relationships and traceability
- **repo-workflow-maker** -- designs multi-step workflows requiring understanding of agent capabilities and orchestration patterns
- **readme-maker** -- produces project documentation requiring codebase comprehension and audience-appropriate framing
- **specs-maker** -- creates technical specifications requiring domain modeling and API design judgment

**Frontmatter**: Omit the `model` field (inherits the default, which is opus-tier).

```yaml
---
name: swe-typescript-developer
description: Expert TypeScript/Node.js developer...
tools: [Read, Write, Edit, Glob, Grep, Bash]
# model omitted -- inherits opus (creative reasoning, code generation)
color: purple
---
```

### Sonnet

**When to use**: Rule-based validation, applying validated fixes from audit reports, template-driven output, and structured pattern-following tasks.

**Cognitive profile**: Strong pattern recognition, reliable rule application, structured output generation, systematic validation against defined criteria.

**Task characteristics**:

- Validating content against a defined checklist or ruleset
- Applying fixes identified by a prior audit (checker output drives fixer input)
- Generating output from templates with variable substitution
- Following a documented procedure step-by-step
- Tasks where correctness means conforming to explicit rules, not inventing solutions

**Agent examples**:

- **All checkers** -- validate content against conventions using defined rulesets and produce structured audit reports (docs-checker, readme-checker, specs-checker, repo-governance-checker, plan-checker, plan-execution-checker, swe-code-checker, swe-ui-checker, ci-checker, apps-\*-checker)
- **All fixers** -- apply corrections from checker audit reports following documented fix procedures (docs-fixer, docs-tutorial-fixer, docs-software-engineering-separation-fixer, readme-fixer, specs-fixer, repo-governance-fixer, repo-workflow-fixer, plan-fixer, swe-ui-fixer, ci-fixer, apps-\*-fixer)
- **docs-file-manager** -- manages file operations (move, rename, delete) following documented structure rules
- **social-linkedin-post-maker** -- generates social media posts following a defined template and tone guidelines

**Frontmatter**: Specify `model: sonnet` explicitly.

```yaml
---
name: docs-checker
description: Expert documentation validator...
tools: [Read, Glob, Grep, Write, Bash]
model: sonnet
color: green
---
```

### Haiku

**When to use**: Purely mechanical tasks with no reasoning required -- simple automation, URL validation, deployment scripts, and straightforward command execution.

**Cognitive profile**: Fast execution of simple, well-defined operations. No analytical reasoning needed. Input-output mapping is deterministic or near-deterministic.

**Task characteristics**:

- Running predefined shell commands in sequence
- Validating URLs against HTTP status codes
- Executing deployment scripts with known parameters
- Simple file existence or format checks
- Tasks where the entire procedure is a fixed script with no branching logic

**Agent examples**:

- **Deployers** (apps-ayokoding-web-deployer, apps-oseplatform-web-deployer, apps-organiclever-fe-deployer) -- execute git branch operations and deployment commands following a fixed procedure
- **Link checkers** (docs-link-general-checker, apps-ayokoding-web-link-checker) -- validate URLs by checking HTTP status codes and managing cache files

**Frontmatter**: Specify `model: haiku` explicitly.

```yaml
---
name: apps-ayokoding-web-deployer
description: Expert deployment orchestrator...
tools: [Bash, Read, Glob, Grep]
model: haiku
color: purple
---
```

## Model Selection Decision Tree

```
Start: Choosing Agent Model
    |
    +-- Does the task require creative reasoning, code generation,
    |   architectural decisions, or nuanced content creation?
    |   |
    |   +-- Yes --> Opus (omit model field)
    |   |
    |   +-- No --> Does the task require applying rules, validating
    |              against checklists, or following structured procedures?
    |              |
    |              +-- Yes --> Sonnet (model: sonnet)
    |              |
    |              +-- No --> Is the task purely mechanical with
    |                         no reasoning required?
    |                         |
    |                         +-- Yes --> Haiku (model: haiku)
    |                         |
    |                         +-- No --> Default to Sonnet
    |                                    (safer than haiku for
    |                                     ambiguous cases)
```

## Justification Requirement

Every agent MUST include a **Model Selection Justification** block in its markdown body explaining why the chosen tier is appropriate. This block appears near the top of the agent file, after the frontmatter metadata section.

**Format**:

```markdown
**Model Selection Justification**: This agent uses `model: sonnet` because it requires:

- [Capability 1] to [accomplish task aspect]
- [Capability 2] to [accomplish task aspect]
```

**Examples**:

For a checker agent:

> **Model Selection Justification**: This agent uses `model: sonnet` because it requires:
>
> - Systematic rule application to validate content against defined checklists
> - Structured report generation following the audit report template
> - Pattern recognition to identify convention violations across files

For a developer agent:

> **Model Selection Justification**: This agent uses `model: sonnet` (inherited opus) because it requires:
>
> - Advanced reasoning to generate idiomatic code across language paradigms
> - Multi-step problem decomposition for complex refactoring tasks
> - Creative synthesis to design APIs and data models

For a deployer agent:

> **Model Selection Justification**: This agent uses `model: haiku` because it requires:
>
> - Execution of predefined git and deployment commands
> - No analytical reasoning beyond following a fixed procedure

## Tier Comparison Summary

| Dimension              | Opus (inherit)              | Sonnet                      | Haiku                       |
| ---------------------- | --------------------------- | --------------------------- | --------------------------- |
| **Reasoning depth**    | Deep, multi-step            | Moderate, rule-based        | Minimal, mechanical         |
| **Creativity**         | High (novel solutions)      | Low (follows templates)     | None (fixed procedures)     |
| **Task ambiguity**     | Handles open-ended problems | Handles structured problems | Requires deterministic flow |
| **Output originality** | Creates new content/code    | Transforms per rules        | Executes predefined steps   |
| **Error recovery**     | Adapts to unexpected states | Follows fallback rules      | Fails or retries            |
| **Typical agents**     | Makers, developers          | Checkers, fixers            | Deployers, link checkers    |

## Common Mistakes

| Mistake                                   | Problem                                                          | Correction                                                |
| ----------------------------------------- | ---------------------------------------------------------------- | --------------------------------------------------------- |
| Using opus for validation tasks           | Wastes resources; opus may over-interpret instead of checking    | Use sonnet for checkers and fixers                        |
| Using haiku for content creation          | Haiku lacks reasoning depth for original content                 | Use opus (inherit) for makers and developers              |
| Using sonnet for deployment scripts       | Sonnet is overqualified for deterministic command sequences      | Use haiku for deployers and link checkers                 |
| Omitting model justification              | Future maintainers cannot assess whether the tier is appropriate | Always include Model Selection Justification block        |
| Defaulting to opus "just in case"         | Violates Simplicity Over Complexity principle                    | Analyze task requirements; use the simplest adequate tier |
| Using haiku for tasks with error handling | Haiku cannot reason about unexpected states                      | Use sonnet or opus depending on error complexity          |

## Special Considerations

### Borderline Cases

Some agents straddle tier boundaries. When uncertain:

1. **Analyze the core loop** -- what does the agent do repeatedly? If the core loop is rule application, use sonnet even if setup requires some reasoning.
2. **Consider the failure mode** -- if the agent picks a wrong approach, how bad is the outcome? Higher-stakes failures justify a higher tier.
3. **Start lower, promote if needed** -- begin with sonnet; promote to opus only if quality issues emerge in practice.

### Link Checkers as Haiku

Link checker agents (docs-link-general-checker, apps-ayokoding-web-link-checker) use haiku despite being categorized as checkers (green). This is because their validation is purely mechanical (HTTP status code checking), not rule-based reasoning. The checker color reflects their role in the maker-checker-fixer workflow, while the model reflects their cognitive requirements.

### Social Media Maker as Sonnet

The social-linkedin-post-maker uses sonnet despite being a "maker" agent. This is because LinkedIn post generation follows a rigid template and tone guide, making it a structured pattern-following task rather than creative content creation.

## Tools and Automation

The following agents enforce or assist with model selection:

- **agent-maker** -- applies these guidelines when creating new agents
- **repo-governance-checker** -- validates that all agents have model justification blocks and appropriate tier assignments
- **repo-governance-fixer** -- corrects model selection issues identified by the checker

## References

**Related Development Practices:**

- [AI Agents Convention](./ai-agents.md) -- Complete agent standards including frontmatter, naming, and tool permissions
- [Best Practices](./best-practices.md) -- Recommended agent development patterns
- [Anti-Patterns](./anti-patterns.md) -- Common agent development mistakes

**Related Principles:**

- [Simplicity Over Complexity](../../principles/general/simplicity-over-complexity.md) -- Use the simplest model that works
- [Explicit Over Implicit](../../principles/software-engineering/explicit-over-implicit.md) -- Justify model selection transparently
- [Deliberate Problem-Solving](../../principles/general/deliberate-problem-solving.md) -- Analyze task requirements before selecting

**Related Conventions:**

- [Content Quality Principles](../../conventions/writing/quality.md) -- Quality standards for justification text

**Agents:**

- `agent-maker` -- Creates agents following these model selection standards
- `repo-governance-checker` -- Validates model selection compliance
- `repo-governance-fixer` -- Fixes model selection issues

---

**Last Updated**: 2026-04-03
