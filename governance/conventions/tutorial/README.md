# Tutorial Conventions

Tutorial creation, structure, naming, and content standards for educational content.

## Purpose

These conventions define **HOW to create and structure tutorials**, covering tutorial types, depth levels, naming standards, programming language content architecture, and by-example tutorial formats. These standards ensure consistent, high-quality learning experiences **across all tutorial content in the repository**.

## Scope

**✅ Applies To:**

- **All tutorial content** in the repository:
  - `docs/tutorials/` - Business, AI, Software Engineering tutorials
  - `apps/ayokoding-web/content/` - Programming language tutorials
  - `apps/ose-platform-web/content/` - Platform tutorials
  - Any other location with tutorial content
- Tutorial structure and organization
- Tutorial naming and depth levels
- Programming language education architecture (Full Set Tutorial Package)
- By-example and by-concept tutorial standards
- Tutorial quality requirements

**❌ Does NOT Cover:**

- General content quality (that's content/)
- Markdown formatting (that's formatting/)
- Hugo-specific implementation details (that's hugo/)

**Note**: These are **content conventions** defining WHAT to write and HOW to structure tutorials. Hugo-specific implementation details (frontmatter, weights, navigation) are covered in [Hugo conventions](../hugo/)

## Conventions

- [By Example Tutorial](./by-example.md) - Standards for code-first by-example tutorials (Component 3 of Full Set - PRIORITY) with 75-85 heavily annotated examples
- [By Concept Tutorial](./by-concept.md) - Standards for narrative-driven by-concept tutorials (Component 4 of Full Set)
- [General Tutorial Convention](./general.md) - Standards for creating learning-oriented tutorials with narrative flow and progressive scaffolding
- [Programming Language Content Standard](./programming-language-content.md) - Full Set Tutorial Package architecture for programming language education
- [Programming Language Tutorial Structure](./programming-language-structure.md) - Complete directory structure for Full Set (5 mandatory components)
- [Tutorial Naming](./naming.md) - Full Set Tutorial Package definition and tutorial type standards

## Full Set Tutorial Package

A **Full Set Tutorial Package** represents complete language content with **all 5 mandatory components**:

1. **Component 1-2: Foundational** - initial-setup.md + quick-start.md (0-30% coverage)
2. **Component 3: By-Example Track** - by-example/ folder with 75-85 code examples (0-95% coverage through beginner/intermediate/advanced) - **PRIORITY for fast learning**
3. **Component 4: By-Concept Track** - by-concept/ folder with narrative-driven tutorials (0-95% coverage through beginner/intermediate/advanced)
4. **Component 5: Cookbook** - cookbook/ folder with practical recipes (complements both tracks)
5. **Supporting Docs** - best-practices, anti-patterns, additional how-to guides

**Key Changes**:

- **By-example prioritized**: Component 3 (was Component 4) - appears first for "move fast" learning
- **By-concept second**: Component 4 (was Component 3) - for "learn deep" understanding
- **Cookbook location**: Moved from `how-to/cookbook.md` to `tutorials/cookbook/` folder
- **By-example status**: Now mandatory (was optional)
- **Full Set definition**: Changed from "5 sequential levels" to "5 mandatory components"
- **Old terminology**: "Full Set" (5 levels) renamed to "Sequential Learning Path" (within by-concept/)

A language is **NOT complete** until all 5 components exist and pass validation. Languages can be production-ready with a subset of components.

## Related Documentation

- [Conventions Index](../README.md) - All documentation conventions
- [Progressive Disclosure Principle](../../principles/content/progressive-disclosure.md) - Why layered complexity matters
- [No Time Estimates Principle](../../principles/content/no-time-estimates.md) - Why we use coverage percentages
- [Repository Architecture](../../repository-governance-architecture.md) - Six-layer governance model

## Principles Implemented/Respected

This set of conventions implements/respects the following core principles:

- **[No Time Estimates](../../principles/content/no-time-estimates.md)**: Tutorial conventions use coverage percentages and depth levels rather than time estimates, focusing on learning outcomes rather than duration.

- **[Progressive Disclosure](../../principles/content/progressive-disclosure.md)**: Tutorial naming conventions and depth levels (Initial Setup, Quick Start, Beginner, Intermediate, Advanced, Cookbook) enable progressive learning based on user's current knowledge level.

- **[Simplicity Over Complexity](../../principles/general/simplicity-over-complexity.md)**: By-example tutorial approach provides code-first learning with simple, annotated examples rather than complex theoretical explanations.

---

**Last Updated**: 2026-01-01
