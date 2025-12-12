---
title: "Indentation Convention"
description: TAB indentation standard for files in docs/ directory (Obsidian vault) with YAML frontmatter exception
category: explanation
subcategory: conventions
tags:
  - indentation
  - formatting
  - markdown
  - obsidian
  - logseq
created: 2025-12-12
updated: 2025-12-12
---

# Indentation Convention

This convention establishes indentation standards for markdown files in the `docs/` directory (Obsidian vault) to ensure compatibility with Logseq and Obsidian tools.

## 🎯 Scope

**Applies to**: Files in `docs/` directory ONLY (Obsidian vault)

**Does NOT apply to**: Files outside `docs/` (root README.md, CLAUDE.md, `plans/`, `.github/`, etc.)

```
open-sharia-enterprise/
├── docs/              ← TAB indentation (Obsidian vault)
│   ├── tutorials/
│   ├── how-to/
│   ├── reference/
│   ├── explanation/
│   └── journals/
├── README.md          ← Standard markdown (spaces OK)
├── CLAUDE.md          ← Standard markdown (spaces OK)
├── plans/             ← Standard markdown (spaces OK)
└── .github/           ← Standard markdown (spaces OK)
```

## 💡 Core Principle

**Files in `docs/` directory use TAB indentation for nested bullet items** (NOT spaces).

**Why?**

- **Obsidian vault scope**: The `docs/` directory is the Obsidian vault requiring Logseq/Obsidian compatibility
- **Logseq compatibility**: Logseq requires TAB indentation for proper outliner functionality
- **Obsidian compatibility**: Obsidian renders both tabs and spaces, but tabs align with Logseq
- **Consistency within vault**: Using tabs ensures the same file works seamlessly in both Logseq and Obsidian
- **Visual clarity**: Tab width is adjustable per user preference in most editors
- **Not project-wide**: Files outside `docs/` can use standard markdown conventions (spaces are fine)

## 📝 Basic Rules

### Markdown Bullet Indentation

Use TAB character for each nesting level in bullet items:

```markdown
✅ CORRECT - TABs for nesting:

- Main point
  - Nested detail (indented with TAB)
  - Another detail (indented with TAB)
    - Deeper elaboration (indented with 2 TABs)

❌ INCORRECT - Spaces for nesting:

- Main point
  - Nested detail (spaces instead of TAB)
  - Another detail (spaces instead of TAB)
    - Deeper elaboration (spaces instead of TAB)
```

**Important**: The TAB character (`\t`) is a single character, not multiple spaces. Configure your editor to insert actual tabs, not spaces when you press the Tab key.

## 🚨 CRITICAL Exception: YAML Frontmatter

**YAML frontmatter is the ONLY exception to TAB indentation within `docs/` directory files.**

All YAML frontmatter blocks MUST use **2 spaces per indentation level** (NOT tabs):

```yaml
✅ CORRECT - Frontmatter uses 2 spaces:
---
title: "Document Title"
description: Brief description
category: explanation
tags:
  - primary-topic    # 2 spaces before dash
  - secondary-topic  # 2 spaces before dash
created: 2025-12-12
updated: 2025-12-12
---

❌ INCORRECT - Frontmatter uses tabs:
---
title: "Document Title"
description: Brief description
category: explanation
tags:
	- primary-topic    # TAB before dash - WRONG!
	- secondary-topic  # TAB before dash - WRONG!
created: 2025-12-12
updated: 2025-12-12
---
```

**Why spaces in frontmatter?**

- **Obsidian requirement**: Obsidian's frontmatter parser expects spaces, not tabs
- **YAML spec**: While YAML allows both, Obsidian tooling is stricter
- **Critical for ALL nested frontmatter fields**: This applies to `tags`, any list fields, and any nested objects
- **Consistency**: All frontmatter across `docs/` must use same indentation

**After frontmatter, use TABs**: All content bullets after the frontmatter block MUST continue using TAB indentation.

## 🔧 Code Block Exception

**Code blocks are exempt from the markdown TAB indentation rule.**

Code blocks within documentation use language-appropriate indentation standards, not the TAB indentation required for markdown bullets:

- **JavaScript/TypeScript**: 2 spaces (aligns with project Prettier configuration)
- **Python**: 4 spaces (PEP 8 standard)
- **YAML**: 2 spaces (YAML specification)
- **JSON**: 2 spaces (project standard)
- **Go**: Tabs (Go language standard)
- **Bash/Shell**: 2 or 4 spaces (common practice)

**Example**:

````markdown
- Research on authentication patterns #auth
  - Key findings about OAuth 2.0
    - Implementation in JavaScript:

```javascript
function authenticate(user) {
  if (user.isValid) {
    return generateToken(user); // 2 spaces (JavaScript standard)
  }
  return null;
}
```

    - Implementation in Python:

```python
def authenticate(user):
    if user.is_valid:
        return generate_token(user)  # 4 spaces (Python standard)
    return None
```
````

**Rationale**: Code blocks represent actual source code and must follow their language's conventions, not the markdown formatting rules. This ensures code examples are syntactically correct and can be copied directly into editors or files.

## 🧪 Complete Example

Here's a complete example showing proper indentation in a `docs/` file:

````markdown
---
title: "Authentication Guide"
description: How to implement authentication
category: how-to
tags:
  - auth # 2 spaces (frontmatter uses spaces)
  - oauth # 2 spaces (frontmatter uses spaces)
created: 2025-12-12
updated: 2025-12-12
---

# Authentication Guide

- Overview of authentication #auth
  - OAuth 2.0 is the recommended approach
    - Authorization code flow for web apps
    - Client credentials flow for service-to-service
  - Key security considerations
    - Token storage strategy
    - Refresh token rotation

- Implementation steps
  - Install dependencies:

```bash
npm install oauth2-provider
```
````

    - Configure the provider:

```javascript
const oauth = new OAuth2Provider({
  clientId: process.env.CLIENT_ID, // 2 spaces (JS standard)
  clientSecret: process.env.CLIENT_SECRET,
  redirectUri: "https://example.com/callback",
});
```

    - Test the integration
    	- Use Postman for manual testing
    	- Write automated tests for token flow

#authentication #oauth #implementation

```

## 📋 Indentation Checklist

Before committing files in `docs/`:

- [ ] **Markdown bullets** use TAB indentation for nesting
- [ ] **YAML frontmatter** uses 2 spaces per indentation level (NOT tabs)
- [ ] **Code blocks** use language-appropriate indentation
- [ ] **No mixed indentation** - consistent throughout file
- [ ] **Editor configured** to insert actual tab characters (not spaces converted to tabs)

## 🔗 Related Conventions

**Universal Application**:

- [Content Quality Principles](./ex-co__content-quality.md) - Quality standards for all markdown

**Context-Specific**:

- [Journals Format Convention](./ex-co__journals-format.md) - Logseq-style outliner format for journals (uses TAB indentation)
- [Hugo Content Convention](./ex-co__hugo-content.md) - Adapted for Hugo (frontmatter spaces, content standard markdown)
- [File Naming Convention](./ex-co__file-naming-convention.md) - File naming standards

## 🌐 External Resources

- [Logseq Documentation](https://docs.logseq.com/) - Outliner tool requiring TAB indentation
- [Obsidian Documentation](https://help.obsidian.md/) - Knowledge base tool with frontmatter parsing
- [YAML Specification](https://yaml.org/spec/) - YAML format specification

---

**Last Updated**: 2025-12-12
```
