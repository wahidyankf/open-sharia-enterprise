# Repository Architecture - Detailed Reference

This file provides detailed explanations of layer characteristics, governance relationships, and architectural decision criteria.

## Layer Characteristics Matrix

| Aspect           | L0: Vision | L1: Principles | L2: Conventions | L3: Development | L4: Agents     | L5: Workflows     |
| ---------------- | ---------- | -------------- | --------------- | --------------- | -------------- | ----------------- |
| **Question**     | WHY exist? | WHY value?     | WHAT doc rules? | HOW develop?    | WHO enforces?  | WHEN orchestrate? |
| **Stability**    | Immutable  | Rarely changes | Occasionally    | More frequently | Often          | As needed         |
| **Scope**        | Project    | Repository     | Documentation   | Software        | Tasks          | Multi-step        |
| **Format**       | Narrative  | Documents      | Documents       | Documents       | Agent files    | Workflow files    |
| **Changes**      | Never      | 1-2 per year   | 4-8 per year    | 10-15 per year  | 20-40 per year | 5-10 per year     |
| **Traceability** | Standalone | → Vision       | → Principles    | → Prin + Conv   | → L2/L3        | → L4              |

## Governance Relationships

### What is Governance?

A layer **governs** another layer when:

1. **Enforces rules** - Lower layer MUST comply
2. **Creates obligations** - Lower layer has requirements
3. **Provides constraints** - Lower layer has boundaries

**Test**: Can the lower layer violate the upper layer's rules?

- If **NO** → Governance relationship exists
- If **YES** → Not governance (support, delivery, or optional)

### Governance Matrix

| From → To                 | Governance?   | Evidence                                          |
| ------------------------- | ------------- | ------------------------------------------------- |
| Vision → Principles       | Inspires      | Principles support vision (not strict governance) |
| Principles → Conventions  | **Yes**       | Conventions MUST implement principles             |
| Principles → Development  | **Yes**       | Practices MUST align with principles              |
| Conventions → Development | **Yes**       | Development MUST respect documentation standards  |
| Conventions → Agents      | **Yes**       | Agents MUST enforce convention rules              |
| Development → Agents      | **Yes**       | Agents MUST follow development practices          |
| Agents → Workflows        | Orchestration | Workflows compose agents (not governance)         |

### Non-Governance Examples

**CLAUDE.md → Agents**: Not governance

- CLAUDE.md provides context summaries
- Agents can function without CLAUDE.md
- CLAUDE.md is support, not requirement

**Skills → Agents**: Not governance

- Skills provide knowledge packages
- Agents can function without Skills
- Skills are delivery, not requirement

**Workflows → Agents**: Not governance

- Workflows orchestrate agents
- Agents can run independently
- Workflows are composition, not requirement

## Layer Deep Dive

### Layer 0: Vision - Detailed Characteristics

**Purpose**: Define WHY the project exists and WHAT change we seek in the world.

**Key Questions**:

- Why does this project exist?
- What problem does it solve?
- What change do we want to see?
- Who benefits from this project?

**Content Structure**:

- Problem statement
- Solution approach
- Success metrics
- Who benefits (audience)

**Change Criteria**:

Vision changes only when:

- Fundamental mission shifts
- Project pivots to different domain
- Organizational purpose changes

**Typical frequency**: Never (project lifetime)

**Example**:

> **Problem**: Islamic finance locked in closed, expensive systems  
> **Solution**: Open-source halal enterprise anyone can use  
> **Success**: Global access to Shariah-compliant tools  
> **Benefit**: Anyone building Islamic enterprise

### Layer 1: Principles - Detailed Characteristics

**Purpose**: Define foundational values that serve the vision and govern all standards.

**Key Questions**:

- Why do we value this approach?
- How does this support the vision?
- What trade-offs does this principle make?

**Required Sections**:

1. **Vision Supported** - How principle serves Layer 0
2. **What This Means** - Concrete applications
3. **Why This Matters** - Benefits and rationale
4. **What This Is NOT** - Common misconceptions

**Change Criteria**:

Principle changes when:

- Fundamental value shift justified
- New principle better serves vision
- Principle proven ineffective over time

**Typical frequency**: 1-2 changes per year

**Validation**:

- Does it support the vision?
- Is it universally applicable?
- Does it conflict with existing principles?
- Can it be implemented in practice?

### Layer 2: Conventions - Detailed Characteristics

**Purpose**: Define WHAT documentation rules we follow.

**Key Questions**:

- What are the documentation standards?
- Which principles does this implement?
- How do agents enforce this?

**Required Sections**:

1. **Principles Implemented/Respected** - Traceability to Layer 1
2. **Scope** - What this covers, what it doesn't
3. **Standards** - Concrete rules and examples
4. **Validation** - How to verify compliance

**Change Criteria**:

Convention changes when:

- New documentation needs emerge
- Better approach discovered
- Principle implementation needs refinement
- Tool/platform capabilities change

**Typical frequency**: 4-8 changes per year

**Validation**:

- Which principle(s) does it implement?
- Is it enforceable by agents?
- Are examples clear?
- Does it conflict with existing conventions?

### Layer 3: Development - Detailed Characteristics

**Purpose**: Define HOW we develop, test, and deploy software.

**Key Questions**:

- How do we develop software?
- Which principles and conventions does this implement?
- What automation supports this?

**Required Sections**:

1. **Principles Implemented/Respected** - Traceability to Layer 1
2. **Conventions Implemented/Respected** - Traceability to Layer 2
3. **Practice Details** - Concrete implementation guidance
4. **Automation** - Git hooks, CI/CD, agents that enforce this

**Change Criteria**:

Practice changes when:

- New tools or frameworks adopted
- Better development patterns discovered
- Principle/convention implementation needs refinement
- Team size or structure changes

**Typical frequency**: 10-15 changes per year

**Validation**:

- Which principle(s) does it implement?
- Which convention(s) does it implement/respect?
- Is automation possible?
- Are examples clear?
- Does it conflict with existing practices?

### Layer 4: Agents - Detailed Characteristics

**Purpose**: WHO enforces conventions and development practices.

**Key Questions**:

- What specific task does this agent perform?
- Which conventions/practices does it enforce?
- What tools does it need?

**Required Elements**:

1. **Frontmatter** - name, description, tools, model, color
2. **Governing Layers** - Which L2/L3 rules it enforces
3. **Atomic Responsibility** - One clear task
4. **Tool Permissions** - Minimal necessary access

**Change Criteria**:

Agent changes when:

- New conventions/practices to enforce
- Better implementation approach discovered
- Tool capabilities expand
- Workflow requirements change

**Typical frequency**: 20-40 changes per year

**Validation**:

- Is responsibility atomic?
- Which L2/L3 rules does it enforce?
- Are tool permissions minimal?
- Does it conflict with existing agents?

### Layer 5: Workflows - Detailed Characteristics

**Purpose**: WHEN to run which agents in what order.

**Key Questions**:

- What agents are needed?
- In what order do they run?
- When does the workflow terminate?
- Where do humans review?

**Required Elements**:

1. **Agent Sequence** - Sequential/parallel/conditional
2. **State Management** - How state flows between steps
3. **Approval Checkpoints** - Where humans review
4. **Termination Criteria** - Clear exit conditions

**Change Criteria**:

Workflow changes when:

- New agent composition patterns emerge
- Better orchestration approach discovered
- Quality requirements change
- Efficiency improvements needed

**Typical frequency**: 5-10 changes per year

**Validation**:

- Are all steps necessary?
- Is termination criteria clear?
- Are approval checkpoints appropriate?
- Does state flow correctly?

## Traceability Requirements

### Layer 0 → Layer 1

**Requirement**: Each principle MUST include "Vision Supported" section.

**Validation**:

```
Principle: Accessibility First
    ↓
Vision Supported:
- Accessible tools enable global participation in Islamic finance
- Universal access serves democratization goal
```

### Layer 1 → Layer 2

**Requirement**: Each convention MUST include "Principles Implemented/Respected" section.

**Validation**:

```
Convention: Color Accessibility
    ↓
Principles Implemented:
- Accessibility First: WCAG compliance for universal access
- Simplicity Over Complexity: Single verified palette for all contexts
```

### Layer 1 → Layer 3

**Requirement**: Each practice MUST include "Principles Implemented/Respected" section.

**Validation**:

```
Practice: Trunk Based Development
    ↓
Principles Implemented:
- Automation Over Manual: Small frequent commits reduce manual merging
- Simplicity Over Complexity: Single branch simpler than complex branching
```

### Layer 2 → Layer 3

**Requirement**: Development practices MUST respect relevant conventions.

**Validation**:

```
Practice: AI Agents Convention
    ↓
Conventions Respected:
- File Naming Convention: Agent files follow [name].md pattern
- Color Accessibility: Agent categorization uses accessible palette
```

### Layer 3 → Layer 3

**Requirement**: Development practices MUST include "Conventions Implemented/Respected" section.

**Validation**:

```
Practice: Code Quality Convention
    ↓
Conventions Respected:
- Content Quality Principles: Pre-commit hooks enforce formatting
```

### Layer 2/3 → Layer 4

**Requirement**: Agents MUST explicitly state which conventions/practices they enforce.

**Validation**:

```
Agent: docs__checker
    ↓
Enforces:
- Color Accessibility Convention: Validates diagram colors
- Content Quality Principles: Checks heading hierarchy, alt text
- Linking Convention: Verifies relative paths, .md extensions
```

### Layer 4 → Layer 5

**Requirement**: Workflows MUST specify which agents they orchestrate.

**Validation**:

```
Workflow: Maker-Checker-Fixer (docs)
    ↓
Orchestrates:
1. docs__maker (creates content)
2. docs__checker (validates)
3. docs__fixer (applies validated fixes)
```

## Architectural Decision Criteria

### When to Add a New Layer?

**CRITICAL**: Don't add new layers without extreme justification.

**Questions**:

1. Does it answer a fundamentally different question (WHY/WHAT/HOW/WHO/WHEN)?
2. Does it have a governance relationship with another layer?
3. Can it be modeled as infrastructure instead?

**Example - Why Skills are NOT Layer 4.5**:

- ❌ Skills don't answer different question (same as CLAUDE.md - "how to deliver knowledge")
- ❌ Skills don't govern agents (Skills SERVE agents)
- ✅ Skills are delivery infrastructure (like CLAUDE.md)

### When to Add Infrastructure vs Layer?

**Infrastructure** when:

- Delivers or transports something
- No governance relationship with consumers
- Multiple alternatives can coexist

**Examples**:

- CLAUDE.md (delivers summaries)
- Skills (deliver knowledge packages)
- Git hooks (deliver automation)

**Layer** when:

- Enforces rules on layer below
- Creates obligations and constraints
- Single authoritative source

**Examples**:

- Conventions (enforce doc rules)
- Development (enforce practices)
- Agents (enforce conventions/practices)

## Complete Traceability Template

Use this template to trace any rule back to vision:

```
Layer 0: Vision
    ↓ inspires
Layer 1: Principle
    ↓ governs
Layer 2: Convention (if documentation)
    ↓ governs
Layer 3: Development (practice)
    ↓ governs
Layer 4: Agent (enforcer)
    ↓ orchestrated by
Layer 5: Workflow (multi-step)
```

**Example - Complete Trace**:

```
L0: Vision - Democratize Islamic enterprise → accessible to everyone
    ↓ inspires
L1: Principle - Accessibility First
    ↓ governs
L2: Convention - Color Accessibility (verified palette, WCAG AA)
    ↓ governs
L3: Development - AI Agents Convention (agent colors from palette)
    ↓ governs
L4: Agents - docs__checker (validates colors), docs__fixer (corrects)
    ↓ orchestrated by
L5: Workflow - Maker-Checker-Fixer (docs)
```

## Architectural Maintenance

### Adding New Conventions

1. **Identify principle** - Which principle does this implement?
2. **Check existing conventions** - Does this conflict or overlap?
3. **Define scope** - What does it cover? What doesn't it?
4. **Add traceability** - Principles Implemented/Respected section
5. **Create examples** - Clear, concrete illustrations
6. **Update index** - Add to docs/explanation/conventions/README.md
7. **Consider agents** - Which agents need to enforce this?

### Adding New Development Practices

1. **Identify principle AND convention** - What do you implement/respect?
2. **Check existing practices** - Conflicts or overlaps?
3. **Define scope** - What does it cover?
4. **Add traceability** - BOTH Principles and Conventions sections
5. **Consider automation** - Git hooks? CI/CD? Agents?
6. **Create examples** - Clear, concrete illustrations
7. **Update index** - Add to docs/explanation/development/README.md

### Adding New Agents

1. **Identify governing layers** - Which L2/L3 rules does this enforce?
2. **Check existing agents** - Conflicts or overlaps?
3. **Define atomic responsibility** - One clear task
4. **Choose tools** - Minimal necessary permissions
5. **Add frontmatter** - name, description, tools, model, color
6. **Document in prompt** - Which conventions/practices enforced
7. **Update index** - Add to .claude/agents/README.md

### Adding New Workflows

1. **Identify agent sequence** - Which agents, in what order?
2. **Check existing workflows** - Similar patterns?
3. **Define termination** - Clear exit conditions
4. **Add approval checkpoints** - Where does user review?
5. **Document state flow** - How state passes between steps
6. **Create workflow file** - docs/explanation/workflows/
7. **Update index** - Add to docs/explanation/workflows/README.md

---

**Note**: This reference provides detailed layer characteristics and governance relationships. The main SKILL.md provides quick overview and traceability examples.
