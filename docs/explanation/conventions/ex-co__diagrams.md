---
title: "Diagram and Schema Convention"
description: Standards for using Mermaid diagrams and ASCII art in open-sharia-enterprise markdown files. Includes color-blind accessibility requirements
category: explanation
subcategory: conventions
tags:
  - diagrams
  - mermaid
  - ascii-art
  - visualization
  - conventions
  - accessibility
  - color-blindness
created: 2025-11-24
updated: 2025-12-31
---

# Diagram and Schema Convention

This document defines when and how to use different diagram formats in the open-sharia-enterprise project. Understanding the appropriate format for each context ensures diagrams render consistently across all platforms where our documentation is viewed.

## Principles Implemented/Respected

This convention implements the following core principles:

- **[Accessibility First](../principles/content/ex-pr-co__accessibility-first.md)**: Requires color-blind friendly palettes, vertical orientation for mobile users, and text-based source that screen readers can parse. Mermaid diagrams provide semantic structure accessible to assistive technology.

- **[Simplicity Over Complexity](../principles/general/ex-pr-ge__simplicity-over-complexity.md)**: Mermaid as the primary format for all markdown files provides a single, universal approach instead of juggling multiple diagram tools. Simple, text-based syntax that's easy to learn and version control.

## Purpose

This convention establishes Mermaid diagrams as the primary visualization format for all markdown files in the repository. It ensures diagrams are accessible, maintainable, and render consistently across GitHub, Obsidian, VS Code, and mobile platforms. This replaces fragmented diagram approaches with a single, universal standard that works everywhere.

## Scope

### What This Convention Covers

- **Mermaid diagram syntax** - Flowcharts, sequence diagrams, class diagrams, state diagrams, and all supported Mermaid types
- **Color accessibility requirements** - Mandatory color-blind friendly palette for all diagrams
- **Mobile-friendly orientation** - Vertical diagram orientation for mobile viewing
- **Mermaid comment syntax** - Correct use of `%%` comments (not `%%{ }%%`)
- **ASCII art guidelines** - When and how to use ASCII as optional fallback
- **Diagram placement** - Where to use diagrams in different markdown contexts

### What This Convention Does NOT Cover

- **Hugo theme diagram rendering** - Covered in [Hugo Development Convention](../development/ex-de__hugo-development.md)
- **Diagram content strategy** - What diagrams to create (covered in specific domain conventions)
- **Vector graphics or images** - This convention is only for text-based diagrams (Mermaid and ASCII)
- **Interactive diagram features** - Platform-specific interactivity (zoom, pan) is implementation detail
- **Diagram export formats** - Exporting Mermaid to PNG, SVG, PDF (tool-specific, not repository standard)

## 🎯 The Core Principle

**Mermaid diagrams are the primary and preferred format for all markdown files** in this repository, both inside and outside the `docs/` directory.

- **All markdown files**: Use Mermaid diagrams as the primary format
- **ASCII art**: Optional fallback for edge cases where Mermaid isn't supported (rarely needed)

## 💡 Why Mermaid First?

Mermaid diagram support has become ubiquitous across modern development tools:

### Wide Platform Support

- **GitHub**: Native Mermaid rendering in markdown files (since May 2021)
- **Text Editors**: VS Code, IntelliJ IDEA, Sublime Text (via plugins/extensions)
- **Obsidian**: Native rendering without plugins
- **Documentation Platforms**: GitLab, Notion, Confluence all support Mermaid
- **Mobile Apps**: GitHub mobile, Obsidian mobile render Mermaid correctly

### Advantages Over ASCII Art

1. **Professional Appearance**: Clean, crisp diagrams with proper styling
2. **Maintainability**: Text-based source is easier to edit than ASCII positioning
3. **Expressiveness**: Supports complex relationships (sequence diagrams, entity relationships, state machines)
4. **Interactive**: Many platforms allow zooming and inspection
5. **Accessible**: Screen readers can parse the source text structure

### When ASCII Art Is Still Useful

ASCII art is now **optional** and only recommended for rare edge cases:

- Terminal-only environments without rich markdown support
- Extremely limited bandwidth scenarios where rendering is disabled
- Simple directory tree structures (where ASCII is clearer than Mermaid)

**In practice**: Most users will view markdown files through GitHub, Obsidian, or modern text editors, all of which support Mermaid.

## 🎨 Mermaid Diagrams: Primary Format for All Markdown Files

### When to Use

Use Mermaid diagrams for **all markdown files** in the repository:

```
open-sharia-enterprise/
├── README.md              ← Use Mermaid
├── CLAUDE.md             ← Use Mermaid
├── CONTRIBUTING.md       ← Use Mermaid
├── docs/                 ← Use Mermaid
│   ├── tutorials/
│   ├── how-to/
│   ├── reference/
│   └── explanation/
├── plans/                ← Use Mermaid
│   ├── in-progress/
│   ├── backlog/
│   └── done/
└── .github/              ← Use Mermaid
    └── *.md
```

### Why Mermaid?

1. **Universal Support** - GitHub, Obsidian, VS Code, and most platforms render Mermaid natively
2. **Rich Visuals** - Professional-looking diagrams with colors, shapes, and styling
3. **Interactive** - Diagrams can be zoomed and inspected
4. **Maintainable** - Text-based source is easy to version control and edit
5. **Powerful** - Supports flowcharts, sequence diagrams, class diagrams, entity relationships, state diagrams, and more
6. **Mobile-Friendly** - Renders beautifully on mobile devices (when using vertical orientation)

### Mermaid Syntax

Mermaid diagrams are defined in code blocks with the `mermaid` language identifier:

````markdown
%% Color palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161, Gray #808080
%% All colors are color-blind friendly and meet WCAG AA contrast standards

```mermaid
graph TD
		A[Start] --> B{Decision}
		B -->|Yes| C[Action 1]
		B -->|No| D[Action 2]
		C --> E[End]
		D --> E
```
````

### Common Mermaid Diagram Types

#### Flowchart

Perfect for processes, workflows, and decision trees:

````markdown
```mermaid
flowchart LR
		A[User Request] --> B{Authenticated?}
		B -->|Yes| C[Process Request]
		B -->|No| D[Return 401]
		C --> E[Return Response]
```
````

```mermaid
flowchart LR
    A[User Request] --> B{Authenticated?}
    B -->|Yes| C[Process Request]
    B -->|No| D[Return 401]
    C --> E[Return Response]
```

#### Sequence Diagram

Shows interactions between components over time:

````markdown
```mermaid
sequenceDiagram
		participant Client
		participant API
		participant Database

		Client->>API: POST /transactions
		API->>Database: Save transaction
		Database-->>API: Confirmation
		API-->>Client: 201 Created
```
````

```mermaid
sequenceDiagram
    participant Client
    participant API
    participant Database

    Client->>API: POST /transactions
    API->>Database: Save transaction
    Database-->>API: Confirmation
    API-->>Client: 201 Created
```

#### Class Diagram

Represents object-oriented structures and relationships:

````markdown
```mermaid
classDiagram
		class Transaction {
				+String id
				+BigDecimal amount
				+Date timestamp
				+validate()
				+execute()
		}

		class Account {
				+String id
				+BigDecimal balance
				+debit()
				+credit()
		}

		Transaction --> Account : involves
```
````

```mermaid
classDiagram
    class Transaction {
        +String id
        +BigDecimal amount
        +Date timestamp
        +validate()
        +execute()
    }

    class Account {
        +String id
        +BigDecimal balance
        +debit()
        +credit()
    }

    Transaction --> Account : involves
```

#### Entity Relationship Diagram

Shows database schema relationships:

````markdown
```mermaid
erDiagram
		CUSTOMER ||--o{ ACCOUNT : owns
		ACCOUNT ||--o{ TRANSACTION : contains
		TRANSACTION }o--|| TRANSACTION_TYPE : has

		CUSTOMER {
				string id PK
				string name
				string email
		}

		ACCOUNT {
				string id PK
				string customer_id FK
				decimal balance
		}
```
````

```mermaid
erDiagram
    CUSTOMER ||--o{ ACCOUNT : owns
    ACCOUNT ||--o{ TRANSACTION : contains
    TRANSACTION }o--|| TRANSACTION_TYPE : has

    CUSTOMER {
        string id PK
        string name
        string email
    }

    ACCOUNT {
        string id PK
        string customer_id FK
        decimal balance
    }
```

#### State Diagram

Illustrates state transitions in systems:

````markdown
```mermaid
stateDiagram-v2
		[*] --> Pending
		Pending --> Processing : start
		Processing --> Completed : success
		Processing --> Failed : error
		Failed --> Pending : retry
		Completed --> [*]
```
````

```mermaid
stateDiagram-v2
    [*] --> Pending
    Pending --> Processing : start
    Processing --> Completed : success
    Processing --> Failed : error
    Failed --> Pending : retry
    Completed --> [*]
```

#### Git Graph

Shows branch and merge history:

````markdown
```mermaid
gitGraph
		commit
		branch develop
		checkout develop
		commit
		checkout main
		merge develop
		commit
```
````

```mermaid
gitGraph
    commit
    branch develop
    checkout develop
    commit
    checkout main
    merge develop
    commit
```

### Diagram Orientation

**Mobile-First Orientation**: Diagrams should be styled vertically (top to bottom or bottom to top) for optimal mobile viewing:

- **Preferred**: `graph TD` (top-down) or `graph BT` (bottom-top)
- **Avoid when possible**: `graph LR` (left-right) or `graph RL` (right-left)
- **Exception**: Use horizontal orientation when vertical layout would significantly harm clarity or readability

**Rationale**: Mobile devices have vertical screens. Vertical diagrams are easier to view without horizontal scrolling.

**Example**:

```mermaid
graph TD
	A[Start] --> B[Process]
	B --> C[End]
```

### Mermaid Best Practices

1. **Keep it Simple** - Complex diagrams become hard to maintain
2. **Use Descriptive Labels** - Clear node names improve readability
3. **Add Comments** - Explain complex logic with inline comments
4. **Test Rendering** - Preview in Obsidian before committing
5. **Version Control Friendly** - Use consistent formatting for easier diffs
6. **Prefer Vertical Orientation** - Use top-down or bottom-top layouts for mobile-friendly viewing
7. **Use Color-Blind Friendly Colors** - REQUIRED: Use accessible hex codes in `classDef` from verified palette (see Color Accessibility below)
8. **Document Color Scheme** - RECOMMENDED: Add ONE color palette comment at the start listing colors used (aids verification, but somewhat redundant if `classDef` already has correct hex codes). No duplicate comments
9. **Correct Comment Syntax** - Use `%%` for comments, NOT `%%{ }%%` (see Comment Syntax below)

### Mermaid Comment Syntax

**CRITICAL**: Mermaid comments MUST use `%%` syntax, NOT `%%{ }%%` syntax.

**Correct Syntax** (✅):

```mermaid
%% This is a comment
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73
graph TD
    A[Start] --> B[End]
```

**Incorrect Syntax** (❌):

```mermaid
%% WRONG EXAMPLE - DO NOT USE
%% The %%{ }%% syntax below is INVALID and will cause errors
%% %%{ This is a comment }%%
%% %%{ Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73 }%%
graph TD
    A[Start] --> B[End]
```

**Why**: The `%%{ }%%` syntax causes "Syntax error in text" in Mermaid rendering. The correct syntax is simply `%%` followed by the comment text.

**Common Mistake**: Adding curly braces around comments is invalid Mermaid syntax. Always use plain `%%` comments.

**Example (Color Palette Comment)**:

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
%% All colors are color-blind friendly and meet WCAG AA contrast standards
graph TD
    A[Start] --> B[Process] --> C[End]
```

**Exception - Mermaid Initialization Directives**:

The `%%{init:...}%%` syntax is VALID when used for Mermaid initialization directives (theme configuration, variables). This is DIFFERENT from comments:

- **Valid Init Directive**: `%%{init: {'theme': 'base', 'themeVariables': {...}}}%%` - For theme customization
- **Invalid Comment**: `%%{ Color Palette: ... }%%` - WRONG syntax for comments
- **Valid Comment**: `%% Color Palette: ...` - Correct syntax for comments

**Key Distinction**: `%%{...}%%` is ONLY valid when containing `init:` directive for Mermaid configuration. Never use it for general comments, color palette notes, or documentation.

**When to Use Init Directives**: Rarely needed. Most diagrams use default theming. Use only when you need to customize Mermaid's theme variables or configuration. See [Hugo Development Convention](../development/ex-de__hugo-development.md) for examples of valid init directive usage.

### Color Accessibility for Color Blindness

**CRITICAL REQUIREMENT**: All Mermaid diagrams MUST use color-blind friendly colors that work in both light and dark modes.

**Master Reference**: See [Color Accessibility Convention](./ex-co__color-accessibility.md) for the complete authoritative guide to color usage, including verified accessible palette, WCAG standards, testing methodology, and implementation details. This section provides a summary for diagram-specific context.

#### Why This Matters

Approximately 8% of males and 0.5% of females have some form of color blindness. Accessible diagrams benefit everyone with clearer, more professional appearance and ensure compliance with accessibility standards.

#### Color Blindness Types to Support

1. **Protanopia (red-blind)**: Cannot distinguish red/green, sees reds and greens as brownish-yellow
2. **Deuteranopia (green-blind)**: Cannot distinguish red/green, sees reds and greens as brownish-yellow
3. **Tritanopia (blue-yellow blind)**: Cannot distinguish blue/yellow, sees blues as pink and yellows as light pink

#### Accessible Color Palette

Use ONLY these proven accessible colors for Mermaid diagram elements:

**Recommended Colors (safe for all color blindness types):**

- **Blue**: `#0173B2` - Safe for all types, works in light and dark mode
- **Orange**: `#DE8F05` - Safe for all types, works in light and dark mode
- **Teal**: `#029E73` - Safe for all types, works in light and dark mode
- **Purple**: `#CC78BC` - Safe for all types, works in light and dark mode
- **Brown**: `#CA9161` - Safe for all types, works in light and dark mode
- **Black**: `#000000` - Safe for borders and text on light backgrounds
- **White**: `#FFFFFF` - Safe for text on dark backgrounds
- **Gray**: `#808080` - Safe for secondary elements

**DO NOT USE:**

- ❌ Red (`#FF0000`, `#E74C3C`, `#DC143C`) - Invisible to protanopia/deuteranopia
- ❌ Green (`#00FF00`, `#27AE60`, `#2ECC71`) - Invisible to protanopia/deuteranopia
- ❌ Yellow (`#FFFF00`, `#F1C40F`) - Invisible to tritanopia
- ❌ Light red/pink (`#FF69B4`, `#FFC0CB`) - Problematic for tritanopia
- ❌ Bright magenta (`#FF00FF`) - Problematic for all types

#### Dark and Light Mode Compliance

All colors must provide sufficient contrast in BOTH rendering modes:

**Light mode background**: White (`#FFFFFF`)
**Dark mode background**: Dark gray/black (`#1E1E2E`)

**Contrast Requirements (WCAG AA):**

- Minimum contrast ratio: **4.5:1** for normal text
- Large text (18pt+ or 14pt+ bold): **3:1**
- Element borders must be distinguishable by shape + color, not color alone

#### Shape Differentiation (Required)

**Never rely on color alone.** Always use multiple visual cues:

- Different node shapes (rectangle, circle, diamond, hexagon)
- Different line styles (solid, dashed, dotted)
- Clear text labels
- Icons or symbols where appropriate

#### Implementation Example

**Good Example (accessible):**

````markdown
<!-- Uses accessible colors: blue (#0173B2), orange (#DE8F05), teal (#029E73) -->

```mermaid
graph TD
		A["User Request<br/>(Blue)"]:::blue
		B["Processing<br/>(Orange)"]:::orange
		C["Response<br/>(Teal)"]:::teal

		A --> B
		B --> C

		classDef blue fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
		classDef orange fill:#DE8F05,stroke:#000000,color:#FFFFFF,stroke-width:2px
		classDef teal fill:#029E73,stroke:#000000,color:#FFFFFF,stroke-width:2px
```
````

**Bad Example (not accessible):**

````markdown
<!-- Uses inaccessible colors: red and green -->

```mermaid
graph TD
		A["Success"]:::green
		B["Error"]:::red

		classDef green fill:#029E73,stroke:#000000  ❌ Invisible to protanopia/deuteranopia
		classDef red fill:#DE8F05,stroke:#000000    ❌ Invisible to protanopia/deuteranopia
```
````

#### Testing Requirements

All diagrams SHOULD be tested with color blindness simulators before publishing:

- **Simulators**: [Coblis Color Blindness Simulator](https://www.color-blindness.com/coblis-color-blindness-simulator/)
- **Contrast Checker**: [WebAIM Contrast Checker](https://webaim.org/resources/contrastchecker/)

**Testing Process:**

1. Create diagram with accessible color palette
2. Test in at least one color blindness simulator (protanopia, deuteranopia, or tritanopia)
3. Verify contrast ratios meet WCAG AA standards
4. Confirm shape differentiation is sufficient

#### Documentation Requirements

**IMPORTANT DISTINCTION:**

- **REQUIRED FOR ACCESSIBILITY**: Using accessible hex codes in `classDef` from the verified palette - this is what makes diagrams accessible
- **RECOMMENDED FOR DOCUMENTATION**: Adding a color palette comment listing which colors are used - this aids verification and signals intent, but is somewhat redundant

For each diagram using colors:

1. **Use accessible hex codes in `classDef`** (REQUIRED)
   - Example: `classDef blue fill:#0173B2,stroke:#000000,color:#FFFFFF`
   - This is the functional accessibility requirement
2. **Add ONE color palette comment** (RECOMMENDED)
   - Example: `<!-- Uses colors #0173B2 (blue), #DE8F05 (orange) for accessibility -->`
   - This is a documentation/transparency practice
   - **CRITICAL**: Each diagram should have exactly ONE color palette comment (no duplicates)
   - Multiple identical comments add unnecessary clutter and create maintenance burden
   - Comment is helpful for quick verification but is redundant with the hex codes in `classDef`
3. **Include labels** that don't rely solely on color
4. **Test verification** noted in diagram documentation (if applicable)

#### Key Implementation Points

When creating Mermaid diagrams:

- Use hex color codes (not CSS color names like "red", "green")
- Always include black borders (`#000000`) for shape definition
- Use white text (`#FFFFFF`) for dark-filled backgrounds
- Use black text (`#000000`) for light-filled backgrounds
- Define colors in `classDef` sections, not inline
- Ensure contrast ratios meet WCAG AA (4.5:1 for normal text)

### Mermaid Resources

- [Official Mermaid Documentation](https://mermaid.js.org/)
- [Mermaid Live Editor](https://mermaid.live/) - Test diagrams online
- [Obsidian Mermaid Docs](https://help.obsidian.md/Editing+and+formatting/Advanced+formatting+syntax#Diagram)
- [Coblis Color Blindness Simulator](https://www.color-blindness.com/coblis-color-blindness-simulator/) - Test diagrams for accessibility
- [WebAIM Contrast Checker](https://webaim.org/resources/contrastchecker/) - Verify WCAG compliance

## 📝 ASCII Art: Optional Fallback

### When to Use

ASCII art is now **optional** and should only be used when:

- **Directory tree structures**: Simple file/folder hierarchies (ASCII is often clearer than Mermaid for this specific use case)
- **Terminal-only contexts**: Rare situations where rich markdown rendering is completely unavailable
- **Personal preference**: When you find ASCII art clearer for a specific simple diagram

**Default recommendation**: Use Mermaid for all diagrams unless you have a specific reason to use ASCII art.

### Why ASCII Art Is Now Optional

With widespread Mermaid support across GitHub, Obsidian, VS Code, and other platforms, the original rationale for requiring ASCII art in files outside `docs/` no longer applies:

1. **GitHub Support**: GitHub has supported Mermaid natively since May 2021
2. **Editor Support**: Modern text editors (VS Code, IntelliJ, Sublime) all support Mermaid previews
3. **Mobile Support**: GitHub mobile and Obsidian mobile render Mermaid correctly
4. **Better Maintainability**: Mermaid is easier to update than manually positioned ASCII art

**Previous approach**: We required ASCII art for files outside `docs/` (README.md, CLAUDE.md, plans/) to ensure universal compatibility.

**Current approach**: Use Mermaid everywhere. ASCII art is a fallback option, not a requirement.

### ASCII Art Use Cases

#### Directory Structure

Perfect for showing file and folder hierarchies:

```
open-sharia-enterprise/
├── .claude/                   # Claude Code configuration
│   └── agents/               # Specialized AI agents
├── docs/                      # Documentation (Diátaxis framework)
│   ├── tutorials/            # Learning-oriented guides
│   ├── how-to/               # Problem-oriented guides
│   ├── reference/            # Technical reference
│   └── explanation/          # Conceptual documentation
├── src/                       # Source code
├── package.json              # Node.js manifest
└── README.md                 # Project README
```

#### Simple Diagrams

Basic flowcharts and relationships:

```
┌─────────────┐
│   Request   │
└──────┬──────┘
       │
       ▼
┌─────────────┐     ┌─────────────┐
│ Validation  │────▶│   Process   │
└─────────────┘     └──────┬──────┘
                           │
                           ▼
                    ┌─────────────┐
                    │  Response   │
                    └─────────────┘
```

#### Process Flow

Sequential steps with connectors:

```
User Action
    │
    ├──▶ Authentication Check
    │        │
    │        ├─ Success ──▶ Process Request ──▶ Return Result
    │        │
    │        └─ Failure ──▶ Return 401
    │
    └──▶ Log Event
```

#### Component Relationships

System architecture overview:

```
┌──────────────────────────────────────┐
│           Frontend (React)           │
└────────────┬─────────────────────────┘
             │
             ▼
┌──────────────────────────────────────┐
│         API Gateway (Express)        │
└─────┬──────────────┬─────────────────┘
      │              │
      ▼              ▼
┌─────────┐    ┌─────────────┐
│ Auth    │    │  Business   │
│ Service │    │  Logic      │
└─────────┘    └──────┬──────┘
                      │
                      ▼
               ┌─────────────┐
               │  Database   │
               └─────────────┘
```

#### Tables and Matrices

Structured data representation:

```
┌──────────────┬────────────┬──────────────┐
│   Category   │   Prefix   │   Example    │
├──────────────┼────────────┼──────────────┤
│  Tutorials   │    tu__    │  tu__start.md│
│  How-To      │   hoto__   │ hoto__api.md │
│  Reference   │    re__    │  re__spec.md │
│  Explanation │    ex__    │  ex__arch.md │
└──────────────┴────────────┴──────────────┘
```

### ASCII Art Best Practices

1. **Use Box-Drawing Characters** - `┌─┐│└┘├┤┬┴┼` for clean borders
2. **Consistent Spacing** - Align elements for better readability
3. **Test in Monospace** - Verify rendering in fixed-width fonts
4. **Keep it Simple** - Complex ASCII art is hard to maintain
5. **Comment Structure** - Add text labels for clarity

### ASCII Art Character Sets

Common characters for drawing:

```
Box Drawing:
┌ ┬ ┐   ╔ ╦ ╗
├ ┼ ┤   ╠ ╬ ╣
└ ┴ ┘   ╚ ╩ ╝
─ │     ═ ║

Arrows:
→ ← ↑ ↓ ↔ ↕
▶ ◀ ▲ ▼

Connectors:
┬ ┴ ├ ┤ ┼
╭ ╮ ╰ ╯
```

### ASCII Art Tools

- Manual creation in text editor with monospace font
- Online generators (limited utility)
- Terminal tools like `figlet` for text banners

## 🔍 Decision Matrix

Use this quick reference to choose the right format:

| File Location     | Primary Format | Alternative       | Notes                                           |
| ----------------- | -------------- | ----------------- | ----------------------------------------------- |
| `docs/**/*.md`    | **Mermaid**    | ASCII (optional)  | Native Obsidian rendering, rich visuals         |
| `README.md`       | **Mermaid**    | ASCII (optional)  | GitHub renders Mermaid natively                 |
| `CLAUDE.md`       | **Mermaid**    | ASCII (optional)  | Modern text editors support Mermaid             |
| `plans/**/*.md`   | **Mermaid**    | ASCII (optional)  | GitHub and editors render Mermaid               |
| `.github/**/*.md` | **Mermaid**    | ASCII (optional)  | GitHub Actions and web UI support Mermaid       |
| `CONTRIBUTING.md` | **Mermaid**    | ASCII (optional)  | Contributors use GitHub web or modern editors   |
| Directory trees   | **ASCII**      | Mermaid (complex) | ASCII is clearer for simple file/folder listing |

## 🧪 Examples in Context

### Example 1: API Flow in Documentation

**File**: `docs/explanation/architecture/ex-ar__request-flow.md`

**Use Mermaid**:

````markdown
## Request Processing Flow

```mermaid
sequenceDiagram
		participant Client
		participant Gateway
		participant Auth
		participant Business
		participant Database

		Client->>Gateway: HTTP Request
		Gateway->>Auth: Validate Token
		Auth-->>Gateway: Token Valid
		Gateway->>Business: Process Request
		Business->>Database: Query Data
		Database-->>Business: Result
		Business-->>Gateway: Response
		Gateway-->>Client: HTTP Response
```
````

### Example 2: Project Structure in README

**File**: `README.md`

**Recommended: Use Mermaid for Complex Diagrams**:

````markdown
## Project Architecture

```mermaid
graph TD
    A[Client Request] --> B[API Gateway]
    B --> C{Auth Check}
    C -->|Valid| D[Business Logic]
    C -->|Invalid| E[Return 401]
    D --> F[Database]
    F --> D
    D --> G[Response]
```
````

**Alternative: Use ASCII for Simple Directory Trees**:

```markdown
## Project Structure

open-sharia-enterprise/
├── .claude/ # Claude Code configuration
├── docs/ # Documentation
│ ├── tutorials/ # Step-by-step guides
│ ├── how-to/ # Problem solutions
│ └── reference/ # Technical specs
├── src/ # Source code
└── package.json # Dependencies
```

### Example 3: State Machine in Tutorial

**File**: `docs/tutorials/transactions/tu-tr__transaction-lifecycle.md`

**Use Mermaid**:

````markdown
## Transaction States

```mermaid
stateDiagram-v2
		[*] --> Draft
		Draft --> Submitted : submit()
		Submitted --> UnderReview : auto
		UnderReview --> Approved : approve()
		UnderReview --> Rejected : reject()
		Approved --> Completed : process()
		Rejected --> [*]
		Completed --> [*]
```
````

### Example 4: Component Architecture in CLAUDE.md

**File**: `CLAUDE.md`

**Recommended: Use Mermaid**:

````markdown
## Agent Architecture

```mermaid
graph TD
    A[Claude Code - Main Agent] --> B[docs__maker.md]
    A --> C[wow__rules-checker.md]
    A --> D[wow__rules-maker.md]
    A --> E[plan__maker.md]

    B --> F[Documentation]
    C --> G[Validation]
    D --> H[Propagation]
    E --> I[Planning]
```
````

**Alternative: Use ASCII for Simple Hierarchies**:

```markdown
## Agent Architecture

Claude Code (Main Agent)
├── docs**maker.md (Documentation)
├── wow**rules-checker.md (Validation)
├── wow**rules-maker.md (Propagation)
└── plan**maker.md (Planning)
```

## Mixing Formats

**Prefer consistency within a single file**. Choose Mermaid as your primary format and use it throughout the file unless you have a specific reason to use ASCII art.

❌ **Avoid mixing unnecessarily**:

````markdown
## System Flow

```mermaid
graph TD
    A --> B
```

## Directory Structure

```
A
└── B
```

## Another Flow

A --> B (plain text - no format!)
````

✅ **Good - consistent Mermaid**:

````markdown
## System Flow

```mermaid
graph TD
    A[Component A] --> B[Component B]
```

## State Transitions

```mermaid
stateDiagram-v2
    [*] --> Active
    Active --> Inactive
```
````

✅ **Acceptable - intentional format choice**:

````markdown
## Architecture Diagram

```mermaid
graph TD
    A[API] --> B[Database]
```

## Project Structure (simple tree)

```
project/
├── src/
└── docs/
```
````

**Rationale**: Mermaid is preferred, but ASCII directory trees are acceptable when they're clearer for simple file/folder listings.

## Migration Strategy

### Upgrading ASCII to Mermaid (Recommended)

Since Mermaid is now the primary format, consider upgrading existing ASCII art diagrams to Mermaid for better maintainability and visual quality:

**When to upgrade**:

- Complex flowcharts or architecture diagrams currently in ASCII
- Diagrams that are hard to update due to ASCII positioning
- When adding new content to a file with ASCII diagrams (good time to upgrade all diagrams)

**When to keep ASCII**:

- Simple directory tree structures (ASCII is clearer)
- If the ASCII diagram is simple and works perfectly well

**Upgrade process**:

1. Identify the diagram type (flowchart, sequence, state machine, etc.)
2. Use appropriate Mermaid syntax
3. Test rendering on GitHub preview or Obsidian
4. Verify all relationships and labels are preserved
5. Keep vertical orientation (top-down or bottom-top) for mobile-friendliness

**Example upgrade**:

**Before (ASCII)**:

```
┌───────┐
│ Start │
└───┬───┘
    │
    ▼
┌─────────┐
│ Process │
└────┬────┘
     │
     ▼
┌─────┐
│ End │
└─────┘
```

**After (Mermaid - vertical orientation)**:

````markdown
```mermaid
graph TD
    A[Start] --> B[Process]
    B --> C[End]
```
````

### No Need to Convert Mermaid to ASCII

With widespread Mermaid support, there's no reason to convert Mermaid diagrams to ASCII art. If you encounter a situation where Mermaid doesn't render, consider:

1. Using a different viewing platform (GitHub web, VS Code, Obsidian)
2. Updating your editor/viewer to support Mermaid
3. Only in extreme edge cases: create an ASCII fallback

## Verification Checklist

Before committing documentation with diagrams:

- [ ] Primary format is Mermaid (unless specific reason for ASCII)
- [ ] Mermaid diagrams use vertical orientation (TD or BT) for mobile-friendliness
- [ ] Mermaid diagrams use color-blind friendly colors (only accessible palette)
- [ ] Colors work in both light and dark mode
- [ ] Shape differentiation used (not relying on color alone)
- [ ] Contrast ratios meet WCAG AA standards (4.5:1 for text)
- [ ] Color scheme documented in comment above diagram
- [ ] **Each diagram has exactly ONE color palette comment** (no duplicates)
- [ ] **Mermaid comments use `%%` syntax, NOT `%%{ }%%`** (correct comment syntax)
- [ ] **Parentheses and brackets escaped in node text** (use HTML entities: `#40;` `#41;` `#91;` `#93;`)
- [ ] **No style commands in sequence diagrams** (use `box` syntax or switch to flowchart)
- [ ] Mermaid diagrams tested in GitHub preview or Obsidian
- [ ] ASCII art (if used) verified in monospace font
- [ ] Format choice is intentional (not mixing Mermaid and ASCII unnecessarily)
- [ ] All labels and text are clear and readable
- [ ] Complex diagrams simplified where possible
- [ ] Diagram serves the documentation purpose
- [ ] Vertical orientation preferred (horizontal only if clarity requires it)

## Common Mermaid Syntax Errors

This section documents critical Mermaid syntax rules discovered through debugging production diagrams. These errors cause "syntax error in text" or rendering failures.

### Error 1: Parentheses in Node Text

**CRITICAL**: Parentheses inside square bracket node definitions cause syntax errors.

**Problem Examples (❌ BROKEN):**

```mermaid
graph TD
    A[O(1) lookup]                  %% ERROR: Parentheses cause syntax error
    B[function(args)]               %% ERROR: Parentheses cause syntax error
    C[Fast Lookup<br/>O(log n)]     %% ERROR: Parentheses cause syntax error
```

**Solution (✅ WORKING):**

Escape parentheses using HTML entity codes:

- `(` → `#40;`
- `)` → `#41;`

```mermaid
graph TD
    A[O#40;1#41; lookup]                     %% CORRECT: Escaped parentheses
    B[function#40;args#41;]                  %% CORRECT: Escaped parentheses
    C[Fast Lookup<br/>O#40;log n#41;]        %% CORRECT: Escaped parentheses
```

**Also applies to square brackets in text:**

- `[` → `#91;`
- `]` → `#93;`

```mermaid
graph TD
    A[Array: #91;0, 1, 2, 3#93;]             %% CORRECT: Escaped square brackets
```

**Rationale**: Mermaid's parser interprets unescaped parentheses and square brackets as syntax elements, not literal characters.

**Real-World Examples Fixed:**

- Python beginner Example 12 (dictionaries): `O(1) lookup` → `O#40;1#41; lookup`
- Python intermediate Example 43 (deque): `O(1) operations` → `O#40;1#41; operations`
- SQL beginner (index lookup): `O(log n)` → `O#40;log n#41;`
- PostgreSQL intermediate (B-tree): `O(log n) search` → `O#40;log n#41; search`

### Error 2: Style Commands in Sequence Diagrams

**CRITICAL**: The `style` command only works in `graph`/`flowchart` diagrams, NOT in `sequenceDiagram`.

**Problem Example (❌ BROKEN):**

```mermaid
sequenceDiagram
    participant User
    participant System

    User->>System: Request
    System-->>User: Response

    style User fill:#0173B2           %% ERROR: style not supported in sequence diagrams
    style System fill:#DE8F05         %% ERROR: style not supported in sequence diagrams
```

**Solution (✅ WORKING):**

For sequence diagrams, use `box` syntax for grouping and coloring instead:

```mermaid
sequenceDiagram
    box Blue User Side
        participant User
    end
    box Orange System Side
        participant System
    end

    User->>System: Request
    System-->>User: Response
```

**Alternative: Use graph/flowchart for styled diagrams:**

```mermaid
flowchart LR
    User[User]:::blue
    System[System]:::orange

    User --> System

    classDef blue fill:#0173B2,stroke:#000000,color:#FFFFFF
    classDef orange fill:#DE8F05,stroke:#000000,color:#FFFFFF
```

**Rationale**: Mermaid diagram types have different syntax capabilities. `style` commands are only valid in graph-based diagrams (graph, flowchart), not in interaction diagrams (sequenceDiagram, classDiagram, stateDiagram).

**Real-World Example Fixed:**

- Python intermediate Example 33 (context manager): Removed `style` commands from sequence diagram

### Quick Reference: Character Escaping

**Characters requiring HTML entity codes in Mermaid node text:**

| Character       | HTML Entity | Example Usage                           |
| --------------- | ----------- | --------------------------------------- |
| `(`             | `#40;`      | `O#40;1#41;` for "O(1)"                 |
| `)`             | `#41;`      | `O#40;1#41;` for "O(1)"                 |
| `[`             | `#91;`      | `#91;0, 1#93;` for "[0, 1]"             |
| `]`             | `#93;`      | `#91;0, 1#93;` for "[0, 1]"             |
| `{`             | `#123;`     | `#123;key: value#125;` for "{key: ...}" |
| `}`             | `#125;`     | `#123;key: value#125;` for "{key: ...}" |
| `<` (less than) | `&lt;`      | `List&lt;T&gt;` for "List<T>"           |
| `>` (more than) | `&gt;`      | `List&lt;T&gt;` for "List<T>"           |

**When to escape:**

- Only when these characters appear **inside square bracket node definitions** `[text here]`
- NOT needed in edge labels, regular text, or comments
- NOT needed in code blocks or quoted strings

**Example: Complex node text with multiple escapes:**

```mermaid
graph TD
    A[HashMap&lt;K, V&gt;<br/>O#40;1#41; lookup<br/>Values: #91;1, 2, 3#93;]
```

Renders as: "HashMap<K, V> / O(1) lookup / Values: [1, 2, 3]"

## 🔗 Related Documentation

- [Color Accessibility Convention](./ex-co__color-accessibility.md) - Master reference for accessible color palette, WCAG standards, and testing tools (comprehensive guide for all color usage)
- [File Naming Convention](./ex-co__file-naming-convention.md) - How to name documentation files
- [Linking Convention](./ex-co__linking-convention.md) - How to link between files
- [Diátaxis Framework](./ex-co__diataxis-framework.md) - Documentation organization principles
- [Conventions Index](./README.md) - Overview of all conventions

## 🌐 External Resources

- [Mermaid Official Documentation](https://mermaid.js.org/)
- [Mermaid Live Editor](https://mermaid.live/)
- [ASCII Art Generator](https://www.asciiart.eu/)
- [Box Drawing Unicode Characters](https://en.wikipedia.org/wiki/Box-drawing_characters)

---

**Last Updated**: 2025-12-31
