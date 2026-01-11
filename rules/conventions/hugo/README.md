# Hugo Content Conventions

Hugo site-specific content conventions for ayokoding-web and ose-platform-web.

## Purpose

These conventions define **WHAT Hugo content rules apply** to our Hugo-based websites, covering theme-specific requirements, content structure, navigation patterns, and bilingual support. All Hugo content must follow these standards.

## Scope

**✅ Belongs Here:**

- Hugo content writing standards
- Theme-specific conventions (Hextra, PaperMod)
- Frontmatter requirements for Hugo
- Navigation and weight management
- Shared Hugo content patterns

**❌ Does NOT Belong:**

- Hugo theme/layout development (that's development/hugo/)
- General markdown formatting (that's formatting/)
- Tutorial content structure (that's tutorial/)
- Non-Hugo documentation standards

## Conventions

- [Hugo Content - ayokoding](./ayokoding.md) - Site-specific conventions for ayokoding-web (Hextra theme, bilingual, weight-based ordering)
- [Hugo Content - OSE Platform](./ose-platform.md) - Site-specific conventions for ose-platform-web (PaperMod theme, English-only)
- [Hugo Content - Shared](./shared.md) - Common Hugo content conventions applying to all Hugo sites in this repository

## Related Documentation

- [Conventions Index](../README.md) - All documentation conventions
- [Hugo Development Convention](../../development/hugo/development.md) - Hugo theme/layout development standards
- [Tutorial Conventions](../tutorial/README.md) - Tutorial structure and naming
- [Repository Architecture](../..repository-governance-architecture.md) - Six-layer governance model

## Principles Implemented/Respected

This set of conventions implements/respects the following core principles:

- **[Accessibility First](../../principles/content/accessibility-first.md)**: Hugo content conventions enforce accessible HTML structure, proper ARIA labels, and semantic markup to ensure web content is accessible to all users.

- **[Explicit Over Implicit](../../principles/software-engineering/explicit-over-implicit.md)**: Frontmatter requirements and weight-based ordering make content structure explicit rather than implicit, ensuring predictable site navigation and organization.

- **[Progressive Disclosure](../../principles/content/progressive-disclosure.md)**: Bilingual content strategy and structured frontmatter enable layering content complexity for different audience levels.

---

**Last Updated**: 2026-01-01
