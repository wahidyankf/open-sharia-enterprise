---
title: "C4 Diagram Templates"
description: Starter templates for creating C4 architecture diagrams with WCAG-compliant color palette
category: explanation
subcategory: architecture
tags:
  - c4-model
  - templates
  - starter-kit
  - diagrams
created: 2026-01-20
updated: 2026-01-20
---

# C4 Diagram Templates

**Stop reinventing diagrams, start with templates.**

Ready-to-use templates for creating C4 architecture diagrams in your project. All templates include WCAG-compliant color palettes, proper structure, and clear placeholders. Copy, customize, and focus on your system's unique characteristics instead of diagram formatting.

## Quick Start

**New to C4 or just want to start quickly?**

### For New Systems (Recommended)

1. **Start with Context + Container**:
   - Copy [Blank Context Diagram](./ex-so-ar-c4armo-te__blank-context-diagram.md) to your project
   - Copy [Blank Container Diagram](./ex-so-ar-c4armo-te__blank-container-diagram.md) to your project
   - Fill in your system details
   - Most systems only need these two diagrams

2. **Add Component Diagrams as Needed**:
   - Only for complex containers requiring detailed documentation
   - Copy [Blank Component Diagram](./ex-so-ar-c4armo-te__blank-component-diagram.md)
   - Document one complex container at a time

3. **Add Supplementary Diagrams**:
   - **Dynamic**: For workflow documentation ([Blank Dynamic Diagram](./ex-so-ar-c4armo-te__blank-dynamic-diagram.md))
   - **Deployment**: For production infrastructure ([Blank Deployment Diagram](./ex-so-ar-c4armo-te__blank-deployment-diagram.md))

### For Comprehensive Documentation

Use [Starter Full Documentation](./ex-so-ar-c4armo-te__starter-full-documentation.md) which includes:

- System Context Diagram
- Container Diagram
- Component Diagram (for one complex container)
- All templates pre-linked and ready to customize

## Available Templates

### Individual Diagram Templates

1. **[Blank Context Diagram](./ex-so-ar-c4armo-te__blank-context-diagram.md)** - System-level view showing external actors and systems
2. **[Blank Container Diagram](./ex-so-ar-c4armo-te__blank-container-diagram.md)** - High-level technical building blocks (apps, databases, services)
3. **[Blank Component Diagram](./ex-so-ar-c4armo-te__blank-component-diagram.md)** - Internal structure of containers (classes, modules, services)
4. **[Blank Dynamic Diagram](./ex-so-ar-c4armo-te__blank-dynamic-diagram.md)** - Sequence diagram showing interactions over time
5. **[Blank Deployment Diagram](./ex-so-ar-c4armo-te__blank-deployment-diagram.md)** - Infrastructure mapping (servers, containers, cloud resources)

### Reference Materials

1. **[Color Palette Reference](./ex-so-ar-c4armo-te__color-palette.md)** - WCAG AA-compliant color palette with hex codes, contrast ratios, and usage guidelines
2. **[Starter Full Documentation](./ex-so-ar-c4armo-te__starter-full-documentation.md)** - Complete C4 documentation set for new systems (Context + Container + Component)

## Color Palette Summary

All templates use the **WCAG AA-compliant** color palette defined in this repository:

| Color  | Hex Code  | Usage                                 | Contrast with White | Contrast with Black |
| ------ | --------- | ------------------------------------- | ------------------- | ------------------- |
| Blue   | `#0173B2` | Primary containers, applications      | 5.93:1 ✓            | 3.55:1              |
| Teal   | `#029E73` | Secondary containers, data stores     | 4.70:1 ✓            | 4.48:1              |
| Orange | `#DE8F05` | Databases, persistence layers         | 3.00:1              | 7.01:1 ✓            |
| Purple | `#CC78BC` | External systems, third-party APIs    | 2.94:1              | 7.15:1 ✓            |
| Brown  | `#CA9161` | Legacy systems, deprecated components | 2.99:1              | 7.03:1 ✓            |
| Black  | `#000000` | Borders, text                         | 1:1                 | 21:1 ✓              |
| White  | `#FFFFFF` | Backgrounds, text on dark fills       | 21:1 ✓              | 1:1                 |
| Gray   | `#808080` | Infrastructure, supporting services   | 3.95:1              | 5.31:1 ✓            |

**Mermaid Style Format**:

```mermaid
style NodeName fill:#0173B2,stroke:#000000,color:#ffffff
```

**See**: [Color Palette Reference](./ex-so-ar-c4armo-te__color-palette.md) for complete guidelines and verification checklist.

## Template Usage Guidelines

### Frontmatter

All diagram files should include frontmatter:

```markdown
---
title: "Your Diagram Title"
description: Brief description of what this diagram shows
category: explanation
subcategory: architecture
tags:
  - c4-model
  - your-system-name
  - context-diagram # or container-diagram, component-diagram, etc.
created: YYYY-MM-DD
updated: YYYY-MM-DD
---
```

### Placeholders

Templates use the following placeholder conventions:

- `[Your System Name]`: Replace with actual system name
- `[Container Name]`: Replace with actual container (app, database, service)
- `[Component Name]`: Replace with actual component (class, module, service)
- `[Technology Stack]`: Replace with technology (e.g., "Node.js/Express", "PostgreSQL")
- `[Description]`: Replace with brief description

### Mermaid Conventions

- **Orientation**: Use `graph TD` (top-down) for mobile-friendly layouts
- **Comments**: Use `%%` for single-line comments (not `%%{ }%%`)
- **Escape Special Characters**: Use HTML entities (`#40;` for `(`, `#91;` for `[`)
- **Node IDs**: Use descriptive IDs (e.g., `UserService`, `OrderDB`)
- **Labels**: Use multi-line labels with `<br/>` for readability
- **Relationships**: Always label relationships with protocol/method (e.g., "HTTP GET /api/users", "SQL SELECT")

### Best Practices

1. **Start Simple**: Don't create all diagrams upfront. Start with Context + Container.
2. **Use Consistent Colors**: Follow the color palette for consistency across diagrams.
3. **Label Everything**: Relationships should describe HOW components communicate.
4. **Update Regularly**: Review diagrams quarterly or when architecture changes.
5. **Link Diagrams**: Cross-reference related diagrams in your documentation.

## Related Documentation

- **[C4 Model Overview](../README.md)** - Introduction to C4 architecture model
- **[Level 1: System Context](../ex-so-ar-c4armo__01-level-1-system-context.md)** - Context diagram guide
- **[Level 2: Container](../ex-so-ar-c4armo__02-level-2-container.md)** - Container diagram guide
- **[Level 3: Component](../ex-so-ar-c4armo__03-level-3-component.md)** - Component diagram guide
- **[Supplementary Diagrams](../ex-so-ar-c4armo__07-supplementary-diagrams.md)** - Dynamic and Deployment diagrams
- **[Best Practices](../ex-so-ar-c4armo__09-best-practices.md)** - When to use C4, common mistakes, tooling
- **[Repository Diagram Convention](../../../../../../governance/conventions/formatting/diagrams.md)** - Accessibility requirements

## Feedback and Improvements

Found an issue with these templates? Have suggestions for improvement? Please open an issue in the repository.
