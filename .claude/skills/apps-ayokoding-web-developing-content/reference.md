# Hugo ayokoding-web Development Reference

This document provides detailed reference information for ayokoding-web Hugo development patterns.

## Weight System Deep Dive

### Powers of 10 Pattern

**Concept**: Each nesting level multiplies weight by 10

**Level 1** (Main Sections): 1-9

```yaml
# apps/ayokoding-web/content/en/learn/_index.md
weight: 2

# apps/ayokoding-web/content/en/rants/_index.md
weight: 3

# apps/ayokoding-web/content/en/video-content/_index.md
weight: 4
```

**Level 2** (Subsections): 10-90

```yaml
# apps/ayokoding-web/content/en/learn/nodejs/_index.md
weight: 10

# apps/ayokoding-web/content/en/learn/typescript/_index.md
weight: 20

# apps/ayokoding-web/content/en/learn/python/_index.md
weight: 30
```

**Level 3** (Tutorials): 100-900

```yaml
# apps/ayokoding-web/content/en/learn/nodejs/00-initial-setup.md
weight: 100

# apps/ayokoding-web/content/en/learn/nodejs/01-quick-start.md
weight: 101

# apps/ayokoding-web/content/en/learn/nodejs/02-beginner-tutorial.md
weight: 102
```

### Weight Allocation Strategy

**Reserve ranges for future additions**:

```
10, 20, 30, 40, 50...  # Not 10, 11, 12, 13, 14
```

**Why**: Enables insertion without renumbering

**Example**: Adding Java between TypeScript (20) and Python (30):

```yaml
# New content
# apps/ayokoding-web/content/en/learn/java/_index.md
weight: 25 # Fits between 20 and 30
```

## Bilingual Content Patterns

### Directory Mirroring

**Structure**: Parallel directories for id/ and en/

```
content/
├── en/
│   └── learn/
│       └── nodejs/
│           ├── _index.md              (weight: 10)
│           ├── 00-initial-setup.md    (weight: 100)
│           └── 01-quick-start.md      (weight: 101)
└── id/
    └── belajar/
        └── nodejs/
            ├── _index.md              (weight: 10)
            ├── 00-initial-setup.md    (weight: 100)
            └── 01-quick-start.md      (weight: 101)
```

**Note**: Weights should match for equivalent content

### Cross-Language Links

**Pattern 1**: Top-level language switcher (in \_index.md)

```markdown
<!-- en/learn/nodejs/_index.md -->

[🇮🇩 Ikhtisar (Indonesian)](/id/belajar/nodejs)

<!-- id/belajar/nodejs/_index.md -->

[🇬🇧 Overview (English)](/en/learn/nodejs)
```

**Pattern 2**: Tutorial-level links

```markdown
<!-- en/learn/nodejs/00-initial-setup.md -->

**Bahasa Indonesia**: [Panduan Initial Setup](/id/belajar/nodejs/00-initial-setup)

<!-- id/belajar/nodejs/00-initial-setup.md -->

**English**: [Initial Setup Guide](/en/learn/nodejs/00-initial-setup)
```

## Frontmatter Field Reference

### Required Fields

| Field    | Type     | Description                     | Example                    |
| -------- | -------- | ------------------------------- | -------------------------- |
| `title`  | string   | Page title (becomes H1)         | "Quick Start - TypeScript" |
| `date`   | datetime | Publication date (UTC+7)        | 2025-12-22T10:00:00+07:00  |
| `draft`  | boolean  | Draft status                    | false                      |
| `weight` | integer  | Navigation order (powers of 10) | 101                        |

### Optional Fields (Common)

| Field           | Type    | Description             | Example                    |
| --------------- | ------- | ----------------------- | -------------------------- |
| `type`          | string  | Content type            | "docs"                     |
| `tags`          | array   | Topic tags              | ["typescript", "tutorial"] |
| `sidebar`       | object  | Sidebar configuration   | `{ exclude: false }`       |
| `toc`           | boolean | Show table of contents  | true                       |
| `excludeSearch` | boolean | Exclude from FlexSearch | false                      |

### Optional Fields (Advanced)

| Field     | Type    | Description                  | Example             |
| --------- | ------- | ---------------------------- | ------------------- |
| `editURL` | string  | Custom edit link             | "https://..."       |
| `noindex` | boolean | Block search engine indexing | false               |
| `params`  | object  | Custom parameters            | `{ images: [...] }` |

## Code Annotation Patterns

### Pattern 1: Simple Variable Declaration (1:1 ratio)

```java
int count = 0;                    // => count is 0 (type: int)
```

### Pattern 2: Method Call with Side Effects (1:2 ratio)

```java
String result = service.process(data);  // => Calls service.process with data
                                        // => result is "processed" (type: String)
```

### Pattern 3: Complex Operation (1:2.25 ratio)

```java
List<User> users = repository
    .findByStatus("active")       // => Query repository for active users
    .stream()                     // => Convert to stream for processing
    .filter(u -> u.age > 18)      // => Keep only users over 18
    .collect(Collectors.toList());// => Collect to List
                                  // => users contains filtered active adult users (type: List<User>)
```

### Measurement Example

**Code block**:

```java
int x = 10;                       // => x is 10
String y = "hello";               // => y is "hello"
System.out.println(y);            // => Output: hello
```

**Calculation**:

- Lines of code: 3
- Lines of comments: 3
- Ratio: 3/3 = 1.0 comments per code line ✅

## Hextra Shortcode Details

### Callout Types

```markdown
{{< callout type="info" >}}
Informational message
{{< /callout >}}

{{< callout type="warning" >}}
Warning message
{{< /callout >}}

{{< callout type="error" >}}
Error message
{{< /callout >}}

{{< callout type="tip" >}}
Helpful tip
{{< /callout >}}
```

### Cards Layout

```markdown
{{< cards >}}
{{< card link="/en/learn/nodejs" title="Node.js" icon="nodejs-icon" >}}
{{< card link="/en/learn/typescript" title="TypeScript" icon="ts-icon" >}}
{{< card link="/en/learn/python" title="Python" icon="python-icon" >}}
{{< /cards >}}
```

### Steps Sequence (REQUIRES {{% %}} delimiters)

```markdown
{{% steps %}}

### Step 1: Install Node.js

Download from [nodejs.org](https://nodejs.org)

### Step 2: Verify Installation

Run `node --version` to check installation.

### Step 3: Create Project

Initialize with `npm init`

{{% /steps %}}
```

### File Tree

```markdown
{{< filetree >}}

- content/
  - en/
    - learn/
      - nodejs/
        - \_index.md
        - 00-initial-setup.md
  - id/ - belajar/ - nodejs/ - \_index.md
    {{< /filetree >}}
```

### Details (Collapsible)

```markdown
{{< details title="Advanced Configuration" >}}

This section contains advanced configuration options...

{{< /details >}}
```

## Navigation Coverage Verification

### Check for Orphaned Content

**Orphaned page**: Page not reachable through navigation

**Detection**:

1. List all pages with `find content/ -name "*.md"`
2. Check each page has weight in frontmatter
3. Verify parent \_index.md exists
4. Confirm weight makes page visible in navigation

**Example orphaned page**:

```yaml
# apps/ayokoding-web/content/en/learn/nodejs/advanced-topics.md
# NO weight field → orphaned
title: "Advanced Topics"
date: 2025-12-22T10:00:00+07:00
draft: false
```

**Fix**:

```yaml
# Add weight to make it discoverable
weight: 110
```

### Verify 2-Layer Depth

**Invalid structure** (3 layers):

```
en/
└── learn/              # Layer 1
    └── nodejs/         # Layer 2
        └── modules/    # Layer 3 (TOO DEEP)
            └── fs.md
```

**Valid structure** (2 layers):

```
en/
└── learn/              # Layer 1
    └── nodejs/         # Layer 2
        └── 05-filesystem-module.md  # Tutorial covering fs module
```

## Link Format Verification

### Valid Absolute Paths

```markdown
[Node.js Quick Start](/en/learn/nodejs/01-quick-start)
[TypeScript Tutorial](/en/learn/typescript/02-beginner-tutorial)
[About Us](/en/about)
```

### Invalid Relative Paths

```markdown
<!-- WRONG -->

[Quick Start](./01-quick-start)
[Quick Start](01-quick-start)
[Quick Start](../nodejs/01-quick-start)
```

### External Links (No Language Prefix)

```markdown
<!-- CORRECT for external links -->

[Hugo Documentation](https://gohugo.io)
[Hextra Theme](https://imfing.github.io/hextra/)
```

## Timezone Format

**REQUIRED**: All dates must use UTC+7 timezone

**Valid formats**:

```yaml
date: 2025-12-22T10:00:00+07:00
date: 2025-12-22T14:30:00+07:00
```

**Invalid formats**:

```yaml
date: 2025-12-22T10:00:00Z        # UTC, not UTC+7
date: 2025-12-22T10:00:00         # No timezone
date: 2025-12-22                  # Date only
```

## Tutorial Type Organization

### Standard Tutorial Sequence

Using [Tutorial Naming Convention](../../../docs/explanation/rules/conventions/tutorial/ex-ru-co-tu__naming.md):

```
nodejs/
├── _index.md                    (weight: 10)
├── 00-initial-setup.md          (weight: 100)  # 0-5% coverage
├── 01-quick-start.md            (weight: 101)  # 5-30% coverage
├── 02-beginner-tutorial.md      (weight: 102)  # 0-60% coverage
├── 03-cookbook.md               (weight: 103)  # Practical recipes
├── 04-by-example.md             (weight: 104)  # 95% via examples
├── 05-intermediate-guide.md     (weight: 105)  # 60-85% coverage
└── 06-advanced-guide.md         (weight: 106)  # 85-95% coverage
```

### By-Concept Organization

Alternative for detailed topic breakdowns:

```
nodejs/
├── _index.md                    (weight: 10)
├── 00-initial-setup.md          (weight: 100)
├── 01-quick-start.md            (weight: 101)
├── 02-beginner-tutorial/        (weight: 102)
│   ├── _index.md
│   ├── 01-variables.md          (weight: 200)
│   ├── 02-functions.md          (weight: 201)
│   └── 03-modules.md            (weight: 202)
└── 03-cookbook.md               (weight: 103)
```

**Note**: Nested tutorials count as Layer 3 (discouraged). Prefer flat structure.

---

**Reference**: This document provides detailed implementation patterns. See [SKILL.md](./SKILL.md) for core guidance and [examples.md](./examples.md) for working code examples.
