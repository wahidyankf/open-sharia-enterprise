# Mermaid Diagram Examples - Accessible Colors

This file provides complete working examples of Mermaid diagrams using the verified accessible color palette.

## Example 1: Basic Accessible Flowchart

This demonstrates the standard pattern for creating accessible Mermaid diagrams:

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Gray #808080
%% All colors are color-blind friendly and meet WCAG AA contrast standards

graph TD
    A["Receive Request<br/>Primary Flow"]:::blue
    B{"Validate Request<br/>Decision Point"}:::orange
    C["Process Request<br/>Success Path"]:::teal
    D["Return Response<br/>Complete"]:::teal
    E["Return Error<br/>Alternative"]:::gray

    A --> B
    B -->|Valid| C
    B -->|Invalid| E
    C --> D

    classDef blue fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef orange fill:#DE8F05,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef teal fill:#029E73,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef gray fill:#808080,stroke:#000000,color:#FFFFFF,stroke-width:2px
```

**Why this works:**

- ✅ Uses only verified palette colors (Blue, Orange, Teal, Gray)
- ✅ Black borders provide shape definition
- ✅ White text provides contrast on dark fills
- ✅ Descriptive text labels on each node
- ✅ Diamond shape for decision (not just color)
- ✅ Palette documented in comment
- ✅ Safe for all color blindness types

## Example 2: Multi-Color Architecture Diagram

This shows all palette colors in action for complex diagrams:

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161, Gray #808080
%% All colors are color-blind friendly and meet WCAG AA contrast standards

graph TD
    L0["Layer 0: Vision<br/>WHY WE EXIST"]:::brown
    L1["Layer 1: Principles<br/>WHY - Values"]:::blue
    L2["Layer 2: Conventions<br/>WHAT - Rules"]:::orange
    L3["Layer 3: Development<br/>HOW - Practices"]:::teal
    L4["Layer 4: AI Agents<br/>WHO - Executors"]:::purple
    L5["Layer 5: Workflows<br/>WHEN - Orchestration"]:::brown

    L0 -->|inspires| L1
    L1 -->|governs| L2
    L1 -->|governs| L3
    L2 -->|governs| L3
    L2 -->|governs| L4
    L3 -->|governs| L4
    L4 -->|orchestrated by| L5

    classDef brown fill:#CA9161,stroke:#000000,color:#FFFFFF,stroke-width:3px
    classDef blue fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef orange fill:#DE8F05,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef teal fill:#029E73,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef purple fill:#CC78BC,stroke:#000000,color:#FFFFFF,stroke-width:2px
```

**Features:**

- Uses all 5 main palette colors
- Different stroke widths for emphasis (2px vs 3px)
- Clear text labels explaining layer purpose
- Edge labels showing relationships
- Vertical orientation for mobile viewing

## Example 3: Sequence Diagram with Accessible Colors

Sequence diagrams can use colors for participants:

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73
%% All colors are color-blind friendly and meet WCAG AA contrast standards

sequenceDiagram
    participant User
    participant API
    participant Database

    User->>API: POST /transactions
    API->>Database: Save transaction
    Database-->>API: Confirmation
    API-->>User: 201 Created
```

**Note**: Sequence diagrams typically don't need explicit color styling as participants are distinguished by position and labels. Color can be added for emphasis if needed.

## Example 4: State Diagram with Accessible Colors

State diagrams showing system states:

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73
%% All colors are color-blind friendly and meet WCAG AA contrast standards

stateDiagram-v2
    [*] --> Idle
    Idle --> Processing
    Processing --> Success
    Processing --> Error
    Success --> [*]
    Error --> Idle
```

**Important**: State diagrams use `state --> state: label` syntax. Do NOT use colons in label text (reserved separator).

## Common Mistakes and Corrections

### Mistake 1: Using Red-Green Combination

❌ **WRONG** - Red and green invisible to ~8% of males:

```mermaid
%% DO NOT USE - Red/Green are NOT accessible
graph TD
    A[Success]:::green
    B[Error]:::red

    classDef green fill:#27AE60,stroke:#000000,color:#FFFFFF
    classDef red fill:#E74C3C,stroke:#000000,color:#FFFFFF
```

✅ **CORRECT** - Use Teal and Orange instead:

```mermaid
%% Color Palette: Teal #029E73, Orange #DE8F05
%% All colors are color-blind friendly

graph TD
    A["Success Path<br/>Complete"]:::teal
    B["Error State<br/>Requires Attention"]:::orange

    classDef teal fill:#029E73,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef orange fill:#DE8F05,stroke:#000000,color:#FFFFFF,stroke-width:2px
```

### Mistake 2: Relying on Color Alone

❌ **WRONG** - No text labels, color-only identification:

```mermaid
graph TD
    A:::blue
    B:::orange
    C:::teal

    classDef blue fill:#0173B2,stroke:#000000,color:#FFFFFF
    classDef orange fill:#DE8F05,stroke:#000000,color:#FFFFFF
    classDef teal fill:#029E73,stroke:#000000,color:#FFFFFF
```

✅ **CORRECT** - Add descriptive labels:

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73

graph TD
    A["User Input<br/>Primary"]:::blue
    B["Validation<br/>Decision"]:::orange
    C["Success<br/>Complete"]:::teal

    A --> B
    B --> C

    classDef blue fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef orange fill:#DE8F05,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef teal fill:#029E73,stroke:#000000,color:#FFFFFF,stroke-width:2px
```

### Mistake 3: Missing Black Borders

❌ **WRONG** - No borders, shapes blend together:

```mermaid
graph TD
    A[Step 1]:::blue
    B[Step 2]:::blue

    classDef blue fill:#0173B2,color:#FFFFFF
```

✅ **CORRECT** - Add black borders for definition:

```mermaid
%% Color Palette: Blue #0173B2

graph TD
    A["Step 1<br/>Process"]:::blue
    B["Step 2<br/>Complete"]:::blue

    classDef blue fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
```

### Mistake 4: Using CSS Color Names

❌ **WRONG** - Color names are inconsistent:

```mermaid
graph TD
    A[Item]:::wrong

    classDef wrong fill:red,stroke:black,color:white
```

✅ **CORRECT** - Use hex codes:

```mermaid
%% Color Palette: Orange #DE8F05

graph TD
    A["Item<br/>Accessible"]:::correct

    classDef correct fill:#DE8F05,stroke:#000000,color:#FFFFFF,stroke-width:2px
```

### Mistake 5: Incorrect Comment Syntax

❌ **WRONG** - Using `%%{ }%%` syntax (causes syntax errors):

```mermaid
%%{ This breaks rendering }%%
graph TD
    A[Item]
```

✅ **CORRECT** - Use double-percent `%%` syntax:

```mermaid
%% This is the correct comment syntax
%% Color Palette: Blue #0173B2

graph TD
    A["Item<br/>Correct"]:::blue

    classDef blue fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
```

## Special Character Escaping Examples

### Example: Function Signatures

When showing code with parentheses, brackets, or braces:

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05

graph TD
    A["Function Call<br/>getData#40;params#41;"]:::blue
    B["Array Access<br/>items#91;index#93;"]:::orange
    C["Object Literal<br/>config#123;key: value#125;"]:::blue

    A --> B
    B --> C

    classDef blue fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef orange fill:#DE8F05,stroke:#000000,color:#FFFFFF,stroke-width:2px
```

### Example: Avoiding Nested Escaping

❌ **WRONG** - Nested escaping breaks rendering:

```
Node text: config#123;\"name\"#125;
```

✅ **CORRECT** - Use descriptive text without quotes:

```
Node text: config#123;name: value#125;
```

Or better yet, use plain descriptive language:

```
Node text: Configuration Object
```

## Complete Template for Quick Start

Copy this template for new diagrams:

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161, Gray #808080
%% All colors are color-blind friendly and meet WCAG AA contrast standards

graph TD
    A["Node 1<br/>Description"]:::blue
    B["Node 2<br/>Description"]:::orange
    C["Node 3<br/>Description"]:::teal

    A --> B
    B --> C

    classDef blue fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef orange fill:#DE8F05,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef teal fill:#029E73,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef purple fill:#CC78BC,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef brown fill:#CA9161,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef gray fill:#808080,stroke:#000000,color:#FFFFFF,stroke-width:2px
```

---

**Usage**: Copy the template above, modify node labels and connections for your diagram. All colors are pre-configured for accessibility.
