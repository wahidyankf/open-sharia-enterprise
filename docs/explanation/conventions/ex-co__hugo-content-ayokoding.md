---
title: "Hugo Content Convention - ayokoding-web"
description: Hugo content conventions specific to ayokoding-web (Hextra theme, bilingual educational platform)
category: explanation
subcategory: conventions
tags:
  - hugo
  - content
  - ayokoding-web
  - hextra
  - bilingual
  - educational
created: 2025-12-13
updated: 2025-12-18
---

# Hugo Content Convention - ayokoding-web

This document defines Hugo content conventions specific to **ayokoding-web** - a bilingual educational platform using the Hextra theme.

## Principles Implemented

This convention implements the following core principles:

- **[Progressive Disclosure](../principles/content/ex-pr-co__progressive-disclosure.md)**: Weight-based navigation ordering allows complexity to be introduced gradually. Main sections (weight 1-10) for beginners, advanced topics later (weight 20+). Overview/ikhtisar pages provide entry points appropriate for each skill level.

- **[Accessibility First](../principles/content/ex-pr-co__accessibility-first.md)**: Bilingual content (Indonesian/English) serves diverse audiences. Consistent structure aids screen readers. Color-blind friendly palettes in visual content.

- **[Explicit Over Implicit](../principles/software-engineering/ex-pr-se__explicit-over-implicit.md)**: Weight values explicitly control navigation order - no magic sorting algorithms. Frontmatter fields clearly state language, type, and navigation position.

## Prerequisites

**IMPORTANT**: This document assumes familiarity with [Shared Hugo Content Conventions](./ex-co__hugo-content-shared.md).

Read the shared conventions first, as they cover:

- Inherited conventions (8 standards from docs/)
- Adapted conventions (5 Hugo-specific modifications)
- Hugo-specific conventions (6 basic concepts)

This document covers **ayokoding-web specific patterns only**.

---

## ayokoding-web Overview

**Site**: [ayokoding.com](https://ayokoding.com)
**Theme**: Hextra
**Purpose**: Bilingual educational platform for Indonesian developers
**Languages**: Indonesian (id) and English (en)
**Repository Path**: `apps/ayokoding-web/`

**Content Types**:

- Learning content (tutorials, guides, courses)
- Personal essays/rants (celoteh)
- Video content (konten-video)

---

## Content Structure

```
apps/ayokoding-web/content/
├── id/                                    # Indonesian content
│   ├── _index.md
│   ├── belajar/                           # Learning (Indonesian)
│   │   ├── _index.md
│   │   ├── nodejs/
│   │   └── typescript/
│   ├── celoteh/                           # Personal essays (Indonesian)
│   └── konten-video/                      # Video content (Indonesian)
└── en/                                    # English content
    ├── _index.md
    ├── learn/                             # Learning (English)
    ├── rants/                             # Personal essays (English)
    └── video-content/                     # Video content (English)
```

---

## Hextra Theme

**Official Documentation**: [Hextra Docs](https://imfing.github.io/hextra/docs/)
**GitHub**: [imfing/hextra](https://github.com/imfing/hextra)
**Last Updated**: 2025-09-06 | **GitHub Stars**: 1,789

**Description**: Modern, fast, batteries-included Hugo theme built with Tailwind CSS for documentation, blogs, and static websites.

### Hextra Key Features

- **Search**: Offline full-text search via FlexSearch (no additional configuration)
- **Design**: Tailwind CSS with responsive layout and dark mode support
- **Performance**: Lightning-fast static-site generator, no JavaScript/Node.js required
- **Content**: Markdown, syntax highlighting, LaTeX math formulae, diagrams, shortcodes
- **Multilingual**: Built-in multi-language support (English, Farsi, Japanese, Simplified Chinese)
- **Navigation**: Auto-generated table of contents, breadcrumbs, pagination, sidebar
- **SEO**: Out-of-the-box SEO tags, Open Graph, Twitter Cards

### Hextra-Specific Frontmatter Fields

```yaml
---
title: "Page Title"
sidebar:
  exclude: true # Exclude from left sidebar
toc: false # Disable table of contents
editURL: "https://..." # Custom edit link
excludeSearch: true # Exclude from FlexSearch index
noindex: true # Block Google Search indexing
params:
  images: ["image-url"] # Open Graph images
  audio: "audio-file.mp3" # Open Graph audio
  videos: ["video-url"] # Open Graph videos
---
```

### Hextra Shortcodes

**Available Shortcodes**:

- `{{< callout >}}` - Highlighted callout boxes
- `{{< cards >}}` - Card grid layout
- `{{< details >}}` - Collapsible content
- `{{< filetree >}}` - File/directory tree visualization
- `{{< icon >}}` - Icon insertion
- `{{< steps >}}` - Numbered step sequence
- `{{< tabs >}}` - Tabbed content

**CRITICAL: Hextra shortcodes require `{{% %}}` delimiters** (see [Shortcode Delimiter Rules](./ex-co__hugo-content-shared.md#2-shortcodes) in shared conventions).

**Example Usage**:

```markdown
{{< callout type="info" >}}
This is an informational callout using Hextra's callout shortcode.
{{< /callout >}}

{{% steps %}}

### Step 1: Install Dependencies

Run `npm install` to install required packages.

### Step 2: Configure Settings

Edit the configuration file...

{{% /steps %}}

{{< cards >}}
{{< card link="/learn/nodejs" title="Node.js" >}}
{{< card link="/learn/typescript" title="TypeScript" >}}
{{< /cards >}}
```

---

## ayokoding-web Site Patterns

### Bilingual Content Organization

**Languages**:

- **Indonesian (id)**: Primary language, default
- **English (en)**: Secondary language

**Directory Structure**:

- Indonesian content: `content/id/`
- English content: `content/en/`

**Language Codes**:

- Indonesian: `id-ID`
- English: `en-US`

**Example (parallel content)**:

```
content/
├── id/belajar/nodejs/getting-started.md     # Indonesian
└── en/learn/nodejs/getting-started.md       # English
```

### Author Field Rules

**CRITICAL RULE**: Most ayokoding-web content does NOT include `author:` field in frontmatter.

**Default Behavior**:

- Site-level author configuration in `hugo.yaml` provides default author: `"Wahidyan Kresna Fridayoka"`
- This applies globally to ALL content unless overridden

**When to Use Author Field**:

✅ **ALLOWED in these directories**:

- `apps/ayokoding-web/content/en/rants/` - English rants may have different authors
- `apps/ayokoding-web/content/id/celoteh/` - Indonesian rants may have different authors

❌ **FORBIDDEN in these directories**:

- `apps/ayokoding-web/content/en/learn/` - Uses site-level author
- `apps/ayokoding-web/content/id/belajar/` - Uses site-level author
- `apps/ayokoding-web/content/en/video-content/` - Uses site-level author
- `apps/ayokoding-web/content/id/konten-video/` - Uses site-level author

**Rationale**: Site-level author handles most content; rants/celoteh may have guest contributors.

**Example (learning content - NO author field)**:

```yaml
---
title: "Getting Started with TypeScript"
date: 2025-12-07T10:00:00+07:00
draft: false
description: "Learn TypeScript fundamentals"
weight: 10
tags: ["typescript", "javascript", "tutorial"]
categories: ["learn"]
# Note: No author field - uses site-level config
---
```

**Example (rant/celoteh content - author field ALLOWED)**:

```yaml
---
title: "Why I Switched to Neovim"
date: 2025-12-07T11:30:00+07:00
draft: false
description: "Personal reflections on developer tooling"
tags: ["tools", "productivity", "opinion"]
categories: ["celoteh"]
author: "Guest Contributor Name" # Optional - allowed in rants/celoteh only
---
```

### Frontmatter Field Formatting Rules

**CRITICAL RULES**: ayokoding-web frontmatter has specific formatting requirements to prevent rendering errors.

#### Categories Field - DO NOT USE

**FORBIDDEN**: The `categories:` field MUST NOT be used in ayokoding-web frontmatter.

**Rationale**: Hextra theme renders categories field as raw text on the page. Content categorization is handled through:

- Directory structure (`/learn/`, `/rants/`, `/celoteh/`)
- Tags field for topic categorization
- Navigation weight for ordering

❌ **Bad (causes raw text leak on page)**:

```yaml
---
title: "Getting Started with TypeScript"
date: 2025-12-07T10:00:00+07:00
tags: ["typescript", "tutorial"]
categories: ["learn"] # WRONG! Displays as raw text on page
---
```

✅ **Good (no categories field)**:

```yaml
---
title: "Getting Started with TypeScript"
date: 2025-12-07T10:00:00+07:00
tags: ["typescript", "tutorial"]
# Note: No categories field - categorization through directory structure
---
```

#### Tags Field - JSON Array Format (Prettier-Enforced)

**REQUIRED FORMAT**: Tags MUST use JSON array format (either single-line or Prettier's multi-line format). Dash-based YAML array format is forbidden.

**IMPORTANT**: Prettier automatically reformats tags during pre-commit hooks. Both formats below are acceptable:

✅ **Good (true single-line JSON array)**:

```yaml
---
title: "Understanding Async/Await in JavaScript"
date: 2025-12-07T10:00:00+07:00
tags: ["javascript", "async", "promises", "tutorial"]
---
```

✅ **Good (Prettier's multi-line JSON array)** - Prettier auto-formats to this:

```yaml
---
title: "Understanding Async/Await in JavaScript"
date: 2025-12-07T10:00:00+07:00
tags: ["javascript", "async", "promises", "tutorial"]
---
```

❌ **Bad (dash-based YAML array format)**:

```yaml
---
title: "Understanding Async/Await in JavaScript"
date: 2025-12-07T10:00:00+07:00
tags:
  - javascript
  - async
  - promises
  - tutorial
---
```

**Rationale**:

- Prettier (automated code formatter) enforces multi-line JSON array format during pre-commit hooks
- Both single-line and Prettier's multi-line JSON array formats are valid JSON arrays in YAML
- Consistent with repository-wide Prettier configuration
- Attempting to override Prettier's formatting would add configuration complexity
- Dash-based YAML arrays are forbidden for consistency with JSON array format

### Weight Field Ordering

**CRITICAL RULE**: All content files use the `weight` field to control navigation ordering (lower weight = first in list). ayokoding-web uses a **level-based weight system** with powers of 10 ranges that reset for each parent folder.

**Scope**: Applies to ALL content in `apps/ayokoding-web/content/` (both `/en/` and `/id/`).

**Key Principle**: Hugo compares weights only among siblings within the same parent folder. Weights reset for children of different parents, allowing independent numbering in each folder.

---

#### Quick Reference Table

| Folder Level | Path Example                      | Folder's \_index.md       | Content Inside (overview.md, files) |
| ------------ | --------------------------------- | ------------------------- | ----------------------------------- |
| 1            | `/en/`, `/id/`                    | 1, 2 (level 1)            | 10, 11, 12... (level 2)             |
| 2            | `/en/learn/`, `/id/belajar/`      | 12, 13, 14... (level 2)   | 100, 101, 102... (level 3)          |
| 3            | `/en/learn/swe/`                  | 102, 103... (level 3)     | 1000, 1001, 1002... (level 4)       |
| 4            | `/en/learn/swe/prog-lang/`        | 1002, 1003... (level 4)   | 10000, 10001... (level 5)           |
| 5            | `/en/learn/swe/prog-lang/golang/` | 10002, 10003... (level 5) | 100000, 100001... (level 6)         |

**Critical Rule**: `_index.md` represents the folder itself at level N. Content **inside** the folder is one level deeper (level N+1).

---

#### The Weight System

**Powers of 10 Ranges**:

- Level 1: **0-9** (language roots)
- Level 2: **10-99** (children of language roots)
- Level 3: **100-999** (children of level 2 folders)
- Level 4: **1000-9999** (children of level 3 folders)
- Level 5: **10000-99999** (children of level 4 folders)
- And so on (×10 for each level)...

**Reset Rule**: Weights reset to the base range for children of each parent folder.

**Example**:

```
/en/ → 1 (level 1)
  children start at 10 (level 2 base)

/id/ → 2 (level 1)
  children start at 10 (level 2 base, RESET - different parent)

/en/learn/ → 12 (level 2)
  children start at 100 (level 3 base)

/en/rants/ → 13 (level 2)
  children start at 100 (level 3 base, RESET - different parent)
```

---

#### Level Calculation

**How to determine the level**:

1. Count the directory depth from the language root
2. Language root (`/en/`, `/id/`) = Level 1
3. Direct children of language root = Level 2
4. Children of level 2 = Level 3
5. And so on...

**Example**:

```
/en/                           → Level 1 (language root)
/en/learn/                     → Level 2 (child of level 1)
/en/learn/swe/                 → Level 3 (child of level 2)
/en/learn/swe/prog-lang/       → Level 4 (child of level 3)
/en/learn/swe/prog-lang/golang/→ Level 5 (child of level 4)
```

---

#### Standard File Weights

**CRITICAL RULE**: `_index.md` represents the folder itself at level N. Content **inside** the folder is one level deeper (level N+1).

Within each folder, files follow this standard sequence:

**Level 1 Folders** (`/en/`, `/id/`):

- Folder's `_index.md`: `weight: 1, 2` (level 1)
- Content inside folder: Uses level 2 (10, 11, 12...)

**Level 2 Folders** (e.g., `/en/learn/`):

1. **Folder's `_index.md`** → Level 2 weight
   - Examples: 12, 13, 14... (positions folder among level 1 children)
2. **`overview.md`/`ikhtisar.md`** → Level 3 base (content is one level deeper)
   - Weight: 100 (level 3 base)
3. **Other content files** → Level 3 base + position
   - Examples: 101, 102, 103... (level 3 range)
4. **Child folders' `_index.md`** → Level 3 weight
   - Examples: 104, 105, 106... (level 3 range)

**Level 3 Folders** (e.g., `/en/learn/swe/`):

1. **Folder's `_index.md`** → Level 3 weight
   - Examples: 102, 103, 104... (positions folder among level 2 children)
2. **`overview.md`/`ikhtisar.md`** → Level 4 base (content is one level deeper)
   - Weight: 1000 (level 4 base)
3. **Other content files** → Level 4 base + position
   - Examples: 1001, 1002, 1003... (level 4 range)
4. **Child folders' `_index.md`** → Level 4 weight
   - Examples: 1004, 1005, 1006... (level 4 range)

**Example (Level 3 folder: `/en/learn/swe/`)**:

```yaml
# /en/learn/swe/_index.md
weight: 102 # Level 3 weight - represents the level 3 folder

# /en/learn/swe/overview.md
weight: 1000 # Level 4 base - content inside level 3 folder is one level deeper

# /en/learn/swe/prog-lang/ (folder)
# /en/learn/swe/prog-lang/_index.md
weight: 1002 # Level 4 weight - represents the level 4 folder

# /en/learn/swe/infosec/ (folder)
# /en/learn/swe/infosec/_index.md
weight: 1003 # Level 4 weight - represents the level 4 folder
```

---

#### Reset Rules (Per-Parent Independence)

**CRITICAL**: Weights reset to the base range for children of EACH parent folder.

**Example - Language Root Children Reset**:

```
/en/ (level 1 folder)
├── _index.md           → weight: 1    (level 1 - represents the folder)
├── learn/ (level 2 folder)
│   └── _index.md       → weight: 12   (level 2 - represents the folder)
├── rants/ (level 2 folder)
│   └── _index.md       → weight: 13   (level 2 - represents the folder)
└── about-ayokoding.md  → weight: 14   (level 2 - content inside /en/)

/id/ (level 1 folder)
├── _index.md           → weight: 2    (level 1 - represents the folder)
├── belajar/ (level 2 folder)
│   └── _index.md       → weight: 12   (level 2 - RESET, same weight as /en/learn/)
├── celoteh/ (level 2 folder)
│   └── _index.md       → weight: 13   (level 2 - RESET, same weight as /en/rants/)
└── konten-video/ (level 2 folder)
    └── _index.md       → weight: 14   (level 2 - RESET)
```

**Example - Sibling Section Children Reset**:

```
/en/learn/ (level 2 folder)
├── _index.md       → weight: 12   (level 2 - represents the folder)
├── overview.md     → weight: 100  (level 3 - content inside level 2 folder)
├── swe/ (level 3 folder)
│   └── _index.md   → weight: 102  (level 3 - represents the folder)
├── ai/ (level 3 folder)
│   └── _index.md   → weight: 103  (level 3 - represents the folder)
└── business/ (level 3 folder)
    └── _index.md   → weight: 104  (level 3 - represents the folder)

/en/rants/ (level 2 folder - different parent!)
├── _index.md       → weight: 13   (level 2 - represents the folder)
├── 2024/ (level 3 folder)
│   └── _index.md   → weight: 102  (level 3 - RESET, same weight as /en/learn/swe/)
└── 2023/ (level 3 folder)
    └── _index.md   → weight: 103  (level 3 - RESET, same weight as /en/learn/ai/)
```

**Why Reset Per Parent?**

- ✅ **Hugo compares siblings only**: Weights only matter within the same parent
- ✅ **Independent folders**: No coordination needed across different parents
- ✅ **Simple numbering**: Each folder starts fresh at its level's base
- ✅ **Easy maintenance**: Moving folders doesn't affect unrelated content

---

#### Complete Example: Multi-Level Structure

```
/en/ (level 1 folder)
├── _index.md                           → weight: 1      (level 1 - represents the folder)
├── learn/ (level 2 folder)
│   ├── _index.md                       → weight: 12     (level 2 - represents the folder)
│   ├── overview.md                     → weight: 100    (level 3 - content inside level 2 folder)
│   ├── swe/ (level 3 folder)
│   │   ├── _index.md                   → weight: 102    (level 3 - represents the folder)
│   │   ├── overview.md                 → weight: 1000   (level 4 - content inside level 3 folder)
│   │   ├── prog-lang/ (level 4 folder)
│   │   │   ├── _index.md               → weight: 1002   (level 4 - represents the folder)
│   │   │   ├── overview.md             → weight: 10000  (level 5 - content inside level 4 folder)
│   │   │   ├── golang/ (level 5 folder)
│   │   │   │   ├── _index.md           → weight: 10002  (level 5 - represents the folder)
│   │   │   │   └── overview.md         → weight: 100000 (level 6 - content inside level 5 folder)
│   │   │   ├── java/ (level 5 folder)
│   │   │   │   └── _index.md           → weight: 10003  (level 5 - represents the folder)
│   │   │   └── python/ (level 5 folder)
│   │   │       └── _index.md           → weight: 10004  (level 5 - represents the folder)
│   │   └── infosec/ (level 4 folder - sibling of prog-lang/)
│   │       ├── _index.md               → weight: 1003   (level 4 - sibling continues sequence)
│   │       └── overview.md             → weight: 10000  (level 5 - RESET, content inside different parent)
│   ├── ai/ (level 3 folder - sibling of swe/)
│   │   ├── _index.md                   → weight: 103    (level 3 - sibling continues sequence)
│   │   └── overview.md                 → weight: 1000   (level 4 - RESET, content inside different parent)
│   └── business/ (level 3 folder - sibling of swe/ and ai/)
│       └── _index.md                   → weight: 104    (level 3 - sibling continues sequence)
├── rants/ (level 2 folder)
│   ├── _index.md                       → weight: 13     (level 2 - represents the folder)
│   └── 2024/ (level 3 folder)
│       └── _index.md                   → weight: 102    (level 3 - RESET, represents the folder)
└── about-ayokoding.md                  → weight: 14     (level 2 - content inside /en/)

/id/ (level 1 folder)
├── _index.md                           → weight: 2      (level 1 - represents the folder)
├── belajar/ (level 2 folder)
│   ├── _index.md                       → weight: 12     (level 2 - RESET, represents the folder)
│   ├── ikhtisar.md                     → weight: 100    (level 3 - RESET, content inside level 2 folder)
│   └── swe/ (level 3 folder)
│       └── _index.md                   → weight: 102    (level 3 - RESET, represents the folder)
├── celoteh/ (level 2 folder)
│   └── _index.md                       → weight: 13     (level 2 - RESET, represents the folder)
└── konten-video/ (level 2 folder)
    └── _index.md                       → weight: 14     (level 2 - RESET)
```

---

#### Common Mistakes

**Understanding These Mistakes:**

These examples show actual errors content creators make when learning the level-based weight system. Each mistake illustrates a misunderstanding of how Hugo's navigation hierarchy works with our weight conventions.

❌ **Mistake 1: Content using same level as folder instead of one level deeper**

**Common Error:** Using the folder's own weight range for content files inside it

**Why this happens:** Forgetting that `_index.md` represents the folder itself, so regular content must be one level deeper

**How to avoid:** Always ask: "Is this file `_index.md` (folder itself, uses folder's level) or content (inside folder, uses next level deeper)?"

```yaml
# WRONG! Content using same level as folder
# File: /en/learn/overview.md (inside level 2 folder)
---
title: Overview
weight: 10 # WRONG! Should use level 3, not level 2
---
# This puts overview.md in the same weight range as /en/learn/_index.md,
# which breaks navigation hierarchy
```

✅ **Correct: Content is one level deeper than folder**

```yaml
# File: /en/learn/overview.md (inside level 2 folder)
---
title: Overview
weight: 100 # Correct! Level 3 base (one level deeper than folder)
---
# /en/learn/ is level 2 folder → /en/learn/_index.md uses level 2 (weight: 12)
# Content INSIDE /en/learn/ is level 3 → overview.md uses level 3 (weight: 100)
```

---

❌ **Mistake 2: Not resetting weights for different parents**

**Common Error:** Continuing sequential weights across folders with different parents

**Why this happens:** Thinking weights are globally sequential instead of per-parent

**How to avoid:** Remember Hugo only compares siblings (same parent). Different parents = independent weight sequences.

```yaml
# WRONG! Continuing numbers across different parents
/en/learn/swe/_index.md → weight: 102 (level 3, parent: learn/)
/en/learn/ai/_index.md → weight: 103 (level 3, parent: learn/)
# But then:
/en/rants/2024/_index.md → weight: 104 # WRONG! Should reset to 102
# Different parent (/en/rants/ vs /en/learn/) = should start from base again

# Same mistake at deeper levels:
/en/learn/swe/prog-lang/_index.md → weight: 1002 (level 4, parent: swe/)
/en/learn/ai/ml-basics/_index.md → weight: 1003 # WRONG! Should be 1002 (different parent: ai/)
# ai/ and swe/ are different parents, so their children reset independently
```

✅ **Correct: Reset to base for each parent**

```yaml
/en/learn/swe/_index.md → weight: 102 (level 3, parent: learn/)
/en/learn/ai/_index.md → weight: 103 (level 3, parent: learn/)
/en/rants/2024/_index.md → weight: 102 # RESET (level 3, different parent: rants/)
# rants/ and learn/ are siblings under /en/, so their children reset independently

/en/learn/swe/prog-lang/_index.md → weight: 1002 (level 4, parent: swe/)
/en/learn/ai/ml-basics/_index.md → weight: 1002 # RESET (level 4, different parent: ai/)
# swe/ and ai/ are siblings under learn/, so their children reset independently
```

---

❌ **Mistake 3: Using wrong level for \_index.md**

**Common Error:** Using the content base weight for `_index.md` instead of folder's sequential weight

**Why this happens:** Confusing "level 3 base" (100, for content) with "level 3 weight" (102, for 3rd folder sibling)

**How to avoid:** Remember `_index.md` IS the folder itself, representing it among siblings. Content files LIVE INSIDE the folder.

```yaml
# WRONG! _index.md using wrong level
# File: /en/learn/swe/_index.md (swe/ is level 3 folder, 3rd child of learn/)
---
title: Software Engineering
weight: 100 # WRONG! This is level 3 BASE for content, not folder weight
---
# Using 100 makes it appear BEFORE overview.md (100) in the parent folder
```

✅ **Correct: \_index.md represents the folder at its level**

```yaml
# File: /en/learn/swe/_index.md (swe/ is level 3 folder, 3rd child of learn/)
---
title: Software Engineering
weight: 102 # Correct! Level 3 weight represents level 3 folder (3rd sibling)
---
# Path: /en/learn/ has children: overview.md (100), ikhtisar.md (101), swe/ (102)
# swe/_index.md uses 102 to represent the folder among its siblings
```

---

❌ **Mistake 4: Missing weight field**

**Common Error:** Omitting the `weight` field entirely

**Why this happens:** Assuming Hugo will auto-assign weights or use alphabetical order

**How to avoid:** EVERY content file in ayokoding-web MUST have an explicit `weight` field. Hugo's default alphabetical sorting breaks our hierarchical navigation.

```yaml
# WRONG! Missing weight field entirely
---
title: Initial Setup
# No weight field
---
# Without weight, Hugo sorts alphabetically by filename
# This breaks the intended tutorial progression order
```

✅ **Correct: Always include weight**

```yaml
---
title: Initial Setup
weight: 1000001 # Level 7 base + 1 (content inside level 6 folder)
---
# Explicit weight ensures this appears in the correct position
# in the tutorial sequence: overview (1000000), initial-setup (1000001), quick-start (1000002)
```

---

#### Benefits of This System

- ✅ **Powers of 10**: Natural progression (10, 100, 1000) is intuitive and memorable
- ✅ **Scalable capacity**: More items at deeper levels (90, 900, 9000) where complexity grows
- ✅ **Per-folder independence**: Each parent's children reset to base - no global coordination needed
- ✅ **Hugo-native**: Leverages Hugo's sibling-only weight comparison for simpler numbering
- ✅ **Level visibility**: Number magnitude immediately shows level (102 vs 10002)
- ✅ **No conflicts**: Different parents use same ranges without collision
- ✅ **Easy calculation**: Base for level + sequence position
- ✅ **Room to grow**: Massive capacity at deep levels (90,000 items at level 5)
- ✅ **Maintainable**: Moving folders only requires updating the folder's level, not global renumbering
- ✅ **Compact numbers**: Lower weights at shallow levels (12 vs 102 in old system)

---

#### Maximum Weight Values (Technical Limits)

Hugo uses Go's `int` type for the weight field. The maximum value depends on system architecture:

**64-bit systems (most modern systems and GitHub Actions):**

- Maximum: **9,223,372,036,854,775,807** (~9.2 quintillion)

**32-bit systems (legacy systems):**

- Maximum: **2,147,483,647** (~2.1 billion)

**Safety Analysis for Our Weight System:**

Our level-based system with powers of 10 ranges is extremely safe:

```
Maximum practical weights by level:
- Level 1: 9 (languages)
- Level 2: 99 (90 items)
- Level 3: 999 (900 items)
- Level 4: 9,999 (9,000 items)
- Level 5: 99,999 (90,000 items)
- Level 6: 999,999 (900,000 items)
- Level 7: 9,999,999 (9,000,000 items)
```

Even on 32-bit systems (max: 2.1 billion), we could theoretically have:

- **Level 10 with maximum items**: 9,999,999,999 (~10 billion) - exceeds 32-bit limit
- **Practical limit on 32-bit**: Level 9 (999,999,999) is well within bounds
- **On 64-bit systems**: Effectively unlimited (can reach level 18+)

**Real-world usage**: Most sites never exceed level 5 (max weight: 99,999), which is safe on all systems.

**Conclusion**: Our weight system has zero risk of exceeding Hugo's limits in any practical scenario.

**Platform Verification:**

- ✅ **GitHub Actions CI**: All runners are 64-bit (x64 or ARM64)
- ✅ **Modern development machines**: Typically 64-bit
- ✅ **Hugo production builds**: Almost always 64-bit

**References:**

- [Hugo Weight Method](https://gohugo.io/methods/menu-entry/weight/)
- [Hugo weighted.go source code](https://github.com/gohugoio/hugo/blob/master/resources/page/weighted.go)
- [Maximum value of int in Go](https://yourbasic.org/golang/max-min-int-uint/)
- [GitHub Actions ARM64 runners](https://github.blog/news-insights/product-news/arm64-on-github-actions-powering-faster-more-efficient-build-systems/)
- [Self-hosted runners architecture support](https://docs.github.com/en/actions/hosting-your-own-runners/managing-self-hosted-runners/supported-architectures-and-operating-systems-for-self-hosted-runners)

### Index File Requirements

#### Navigation Depth (3 Layers)

**CRITICAL RULE**: `_index.md` files MUST display navigation links **3 layers deep**.

**Layer Definition**:

- **Layer 1**: Parent section/category (current level)
- **Layer 2**: Children (immediate subsections)
- **Layer 3**: Grandchildren (subsections of children)

**Rationale**: Provides comprehensive navigation hierarchy, helps users discover deeply nested content, improves UX.

**Applies to**: All `_index.md` files in `apps/ayokoding-web/content/en/learn/` and `apps/ayokoding-web/content/id/belajar/`

✅ **Good (3 layers deep with overview links first)**:

```markdown
<!-- File: content/en/learn/swe/prog-lang/_index.md -->
<!-- Shows: 3 layers deep + overview link as first item -->

- [Overview](/learn/swe/prog-lang/overview) # ← Overview FIRST when exists
- [Golang](/learn/swe/prog-lang/golang)
  - [Overview](/learn/swe/prog-lang/golang/overview) # ← Overview FIRST
  - [Initial Setup](/learn/swe/prog-lang/golang/initial-setup)
  - [Quick Start](/learn/swe/prog-lang/golang/quick-start)
  - [Beginner](/learn/swe/prog-lang/golang/beginner)
  - [Intermediate](/learn/swe/prog-lang/golang/intermediate)
  - [Advanced](/learn/swe/prog-lang/golang/advanced)
  - [Cookbook](/learn/swe/prog-lang/golang/cookbook)
- [Java](/learn/swe/prog-lang/java)
  - [Overview](/learn/swe/prog-lang/java/overview) # ← Overview FIRST
  - [Initial Setup](/learn/swe/prog-lang/java/initial-setup)
  - [Quick Start](/learn/swe/prog-lang/java/quick-start)
  - [Beginner](/learn/swe/prog-lang/java/beginner)
```

❌ **Bad (only 2 layers - missing grandchildren)**:

```markdown
<!-- WRONG! Stops at children, doesn't show grandchildren -->

- [Software Engineering](/learn/swe)
  - [Programming Languages](/learn/swe/prog-lang)
  - [System Design](/learn/swe/system-design)
- [AI Engineering](/learn/ai)
  - [Chat with PDF](/learn/ai/chat-with-pdf)
```

#### Index File Title Rule

**CRITICAL RULE**: The `title` field in `_index.md` frontmatter should be **DESCRIPTIVE and READABLE** for human display.

**Rationale**: Titles are for user interface display - they should be clear and meaningful, not constrained by URL-friendly folder names.

**Key Principle**: Folder names use lowercase-with-hyphens for URL compatibility; titles use proper capitalization and spacing for readability.

✅ **Good (descriptive, readable titles)**:

```yaml
# File: content/en/learn/swe/prog-lang/_index.md
---
title: Programming Languages # Clear, descriptive, properly capitalized
---
# File: content/en/learn/ai/_index.md
---
title: AI Engineering # Human-readable, proper acronym capitalization
---
# File: content/en/learn/human/tools/cliftonstrengths/_index.md
---
title: CliftonStrengths # Proper product name capitalization
---
# File: content/en/learn/business/_index.md
---
title: Business and Finance # Descriptive with context
---
```

❌ **Bad (forced folder name matching)**:

```yaml
# File: content/en/learn/swe/prog-lang/_index.md
---
title: Prog-lang # WRONG! Unreadable, keeps folder naming pattern
---
# File: content/en/learn/ai/_index.md
---
title: Ai # WRONG! Incorrect capitalization (should be "AI")
---
# File: content/en/learn/business/_index.md
---
title: Business # WRONG! Too generic (missing context)
---
```

**Capitalization Guidelines**:

- Use proper title case for readability
- Acronyms use standard capitalization (AI, SWE, PDF, not Ai, Swe, Pdf)
- Product names use official capitalization (CliftonStrengths, JavaScript, TypeScript)
- Add context when helpful (e.g., "Business and Finance" instead of just "Business")

#### Content Separation

**CRITICAL RULE**: `_index.md` files should contain ONLY navigation lists (3 layers deep) - NO introduction or overview content.

**Introduction/Overview Content**: Goes in separate `overview.md` (English) or `ikhtisar.md` (Indonesian) file.

**Rationale**: Clear separation of concerns (navigation vs content), consistent UX pattern, easier maintenance.

**Applies to**: All `_index.md` files in `apps/ayokoding-web/content/en/learn/` and `apps/ayokoding-web/content/id/belajar/`

### Overview/Ikhtisar File Requirements

**CRITICAL REQUIREMENT**: EVERY content folder MUST have an intro content file.

**Language-Specific File Names**:

- **English folders** (`/en/learn/` and ALL subfolders): MUST have `overview.md`
- **Indonesian folders** (`/id/belajar/` and ALL subfolders): MUST have `ikhtisar.md` (NOT `overview.md`)

**Applies to ALL folder types**:

- Topic folders (e.g., `/en/learn/swe/prog-lang/golang/`)
- Category folders (e.g., `/en/learn/swe/`, `/en/learn/ai/`)
- Diátaxis subdirectories (e.g., `/en/learn/swe/prog-lang/golang/tutorials/`)
- Any folder containing `_index.md` navigation file

**Rationale**: Every section needs context and introduction separate from navigation; ensures consistent user experience.

**Why "ikhtisar"?** Indonesian word for "overview" - maintains bilingual file naming consistency.

#### Overview/Ikhtisar Link Requirement

**CRITICAL RULE**: All `_index.md` files that have a corresponding `overview.md` or `ikhtisar.md` MUST include a link to that overview/ikhtisar page as the FIRST item in their navigation list.

**Rationale**: Ensures overview pages are consistently visible and accessible.

#### Title Format for Overview/Ikhtisar Files

**CRITICAL RULE**: Titles must be simple and generic.

- **`overview.md` files**: Title MUST be "Overview" (not descriptive)
- **`ikhtisar.md` files**: Title MUST be "Ikhtisar" (not descriptive)

**Rationale**: Context is provided by directory structure. Simple titles make navigation cleaner and more consistent.

✅ **Good (simple, generic titles)**:

```yaml
# File: content/en/learn/swe/prog-lang/overview.md
# Path: /en/ (1) → /learn/ (2) → /swe/ (3) → /prog-lang/ (4)
# prog-lang is level 4 folder, content inside uses level 5
---
title: "Overview" # Simple, generic - context from path
weight: 10000 # Level 5 base - content inside level 4 folder
---
# File: content/id/belajar/swe/prog-lang/ikhtisar.md
# Path: /id/ (1) → /belajar/ (2) → /swe/ (3) → /prog-lang/ (4)
# prog-lang is level 4 folder, content inside uses level 5
---
title: "Ikhtisar" # Simple, generic - context from path
weight: 10000 # Level 5 base - content inside level 4 folder
---
```

❌ **Bad (descriptive titles)**:

```yaml
# File: content/en/learn/swe/prog-lang/overview.md
---
title: "Programming Languages Overview" # WRONG! Too descriptive
---
# File: content/id/belajar/swe/prog-lang/ikhtisar.md
---
title: "Ikhtisar Penyimpanan Data Dalam Memori" # WRONG! Too descriptive
---
```

#### Example Structure

**English Structure** (golang/ is level 5 folder, content inside uses level 6):

```
content/en/learn/swe/prog-lang/golang/
├── _index.md        # Folder (weight: 10002, level 5 - represents golang/ folder)
├── overview.md      # Content (weight: 100000, level 6 - content inside level 5 folder)
├── initial-setup.md # Content (weight: 100001, level 6)
├── quick-start.md   # Content (weight: 100002, level 6)
├── beginner.md      # Content (weight: 100003, level 6)
├── intermediate.md  # Content (weight: 100004, level 6)
├── advanced.md      # Content (weight: 100005, level 6)
└── cookbook.md      # Content (weight: 100006, level 6)
```

**Indonesian Structure** (golang/ is level 5 folder, content inside uses level 6):

```
content/id/belajar/swe/prog-lang/golang/
├── _index.md        # Folder (weight: 10002, level 5 - represents golang/ folder)
├── ikhtisar.md      # Content (weight: 100000, level 6 - content inside level 5 folder)
├── initial-setup.md # Content (weight: 100001, level 6)
├── quick-start.md   # Content (weight: 100002, level 6)
├── beginner.md      # Content (weight: 100003, level 6)
├── intermediate.md  # Content (weight: 100004, level 6)
├── advanced.md      # Content (weight: 100005, level 6)
└── cookbook.md      # Content (weight: 100006, level 6)
```

**Example `_index.md` (navigation only with overview link first)**:

```markdown
---
title: Golang
weight: 10002 # Level 5 - represents the level 5 folder
---

- [Overview](/learn/swe/prog-lang/golang/overview) # MUST be first
- [Initial Setup](/learn/swe/prog-lang/golang/initial-setup)
- [Quick Start](/learn/swe/prog-lang/golang/quick-start)
- [Beginner Guide](/learn/swe/prog-lang/golang/beginner)
- [Intermediate Guide](/learn/swe/prog-lang/golang/intermediate)
- [Advanced Guide](/learn/swe/prog-lang/golang/advanced)
- [Cookbook](/learn/swe/prog-lang/golang/cookbook)
```

**Example `overview.md` (English intro content)**:

```markdown
---
title: "Overview"
date: 2025-12-09T10:00:00+07:00
draft: false
description: "Introduction to our comprehensive Golang learning resources"
weight: 100000 # Level 6 base - content inside level 5 folder
tags: ["golang", "programming", "overview"]
# Note: No categories field - causes raw text leak in Hextra theme
---

Welcome to our Golang learning path! This comprehensive curriculum takes you from...
```

**Example `ikhtisar.md` (Indonesian intro content)**:

```markdown
---
title: "Ikhtisar"
date: 2025-12-09T10:00:00+07:00
draft: false
description: "Pengenalan ke sumber pembelajaran Golang komprehensif kami"
weight: 100000 # Level 6 base - content inside level 5 folder
tags: ["golang", "programming", "ikhtisar"]
# Note: No categories field - causes raw text leak in Hextra theme
---

Selamat datang di jalur pembelajaran Golang kami! Kurikulum komprehensif ini membawa Anda dari...
```

### Optional Topic-Level Diátaxis Structure

**Status**: OPTIONAL pattern (not required, not validated by ayokoding-content-checker)

Topics in ayokoding-web MAY optionally organize content using Diátaxis framework at the topic level.

**Diátaxis Directories** (optional):

- `tutorials/` - Learning-oriented, step-by-step guides
- `how-to/` - Problem-solving, practical recipes
- `reference/` - Information-oriented, technical reference
- `explanation/` - Understanding-oriented, conceptual guides

**When to Use**:

✅ **Use when**:

- Topic has diverse content types (tutorials + recipes + reference + concepts)
- Content is growing and needs better organization
- Users have different goals (learning vs. looking up vs. solving problems)

❌ **Keep flat when**:

- Topic has few pages (< 10 files)
- All content is similar type (e.g., all tutorials)
- Simple structure meets user needs

**Example (topic using Diátaxis structure)**:

```
content/en/learn/swe/prog-lang/golang/
├── _index.md                           # Navigation hub (3 layers deep)
├── overview.md                         # Topic introduction
├── tutorials/
│   ├── _index.md                       # Tutorials section index
│   ├── overview.md                     # Tutorials overview
│   ├── initial-setup.md                # Tutorial: 0-5% coverage
│   ├── quick-start.md                  # Tutorial: 5-30% coverage
│   ├── beginner.md                     # Tutorial: 0-60% coverage
│   ├── intermediate.md                 # Tutorial: 60-85% coverage
│   └── advanced.md                     # Tutorial: 85-95% coverage
├── how-to/
│   ├── _index.md                       # How-to section index
│   ├── overview.md                     # How-to overview
│   └── cookbook.md                     # Practical recipes
├── reference/
│   ├── _index.md                       # Reference section
│   └── overview.md                     # Reference overview
└── explanation/
    ├── _index.md                       # Explanation section
    └── overview.md                     # Explanation overview
```

**Note**: This pattern is OPTIONAL. Each topic decides independently based on content volume and diversity.

**Programming Language Content**: For programming languages specifically (e.g., Golang, Python, Java, Kotlin, TypeScript, Rust), this Diátaxis structure is MANDATORY and follows the [Programming Language Content Standard](./ex-co__programming-language-content.md). See that convention for complete requirements including:

- Universal directory structure (5 tutorial levels, cookbook, how-to guides, best practices, anti-patterns)
- Coverage philosophy (0-5%, 5-30%, 0-60%, 60-85%, 85-95%, cookbook)
- Quality metrics and pedagogical patterns
- Step-by-step implementation guide: [How to Add a Programming Language](../../how-to/hoto__add-programming-language.md)

### Blogging Content Structure (Rants/Celoteh)

**Scope**: This section applies ONLY to blogging content in:

- `/en/rants/` - English personal essays and opinion pieces
- `/id/celoteh/` - Indonesian personal essays and opinion pieces

**CRITICAL DIFFERENCES from Learning Content**:

Blogging content uses a **year/month organization** structure that differs significantly from the topic-based learning content structure.

#### Cross-Reference Pattern (Bilingual Blogging Content)

**CRITICAL REQUIREMENT**: Blogging content that exists in both languages MUST include cross-reference links at the top of each article.

**Cross-Reference Placement**: Immediately after frontmatter, before main content (first element readers see).

**English Articles (Original)**:

```markdown
**Similar article:** [Indonesian Article Title](/id/celoteh/2023/07/article-slug)
```

- Uses bold formatting for "**Similar article:**"
- Links to corresponding Indonesian translation
- Uses absolute path with language prefix

**Indonesian Articles (Translated)**:

```markdown
> _Artikel ini adalah hasil terjemahan dengan bantuan mesin. Karenanya akan ada pergeseran nuansa dari artikel aslinya. Untuk mendapatkan pesan dan nuansa asli dari artikel ini, silakan kunjungi artikel yang asli di: [English Article Title](/en/rants/2023/07/article-slug)_
```

- Uses blockquote (>) for machine translation disclaimer
- Explains nuance shifts due to machine translation
- Links to original English article
- Uses absolute path with language prefix

**Key Rules**:

1. Cross-references appear at **top of article** (after frontmatter, before main content)
2. Links use **absolute paths** with language prefix (`/en/rants/...` or `/id/celoteh/...`)
3. Links do NOT include `.md` extension (Hugo convention)
4. **Both directions exist**: EN → ID and ID → EN
5. **Applies ONLY to blogging content** (`/rants/` and `/celoteh/`), NOT learning content

**Example - English Article** (`/en/rants/2023/07/why-neovim.md`):

```markdown
---
title: "Why I Switched to Neovim"
date: 2023-07-15T10:30:00+07:00
draft: false
description: "Personal reflections on moving from Vim to Neovim"
weight: 10000 # Level 5 base - content inside level 4 folder (07/)
tags: ["tools", "vim", "neovim"]
# Note: No categories field - causes raw text leak in Hextra theme
---

**Similar article:** [Kenapa Saya Pindah ke Neovim](/id/celoteh/2023/07/kenapa-neovim)

After 5 years of using Vim, I finally made the switch to Neovim...
```

**Example - Indonesian Article** (`/id/celoteh/2023/07/kenapa-neovim.md`):

```markdown
---
title: "Kenapa Saya Pindah ke Neovim"
date: 2023-07-15T10:30:00+07:00
draft: false
description: "Refleksi personal tentang perpindahan dari Vim ke Neovim"
weight: 10000 # Level 5 base - content inside level 4 folder (07/)
tags: ["tools", "vim", "neovim"]
# Note: No categories field - causes raw text leak in Hextra theme
---

> _Artikel ini adalah hasil terjemahan dengan bantuan mesin. Karenanya akan ada pergeseran nuansa dari artikel aslinya. Untuk mendapatkan pesan dan nuansa asli dari artikel ini, silakan kunjungi artikel yang asli di: [Why I Switched to Neovim](/en/rants/2023/07/why-neovim)_

Setelah 5 tahun menggunakan Vim, saya akhirnya pindah ke Neovim...
```

**Rationale**: Cross-references help bilingual readers find content in their preferred language and set expectations about translation quality.

**Directory Organization**:

```
content/en/rants/
├── _index.md                           # Main rants section index
├── 2023/
│   ├── _index.md                       # Year index (3-layer tree: year → months → articles)
│   ├── 07/
│   │   ├── _index.md                   # Month index (lists articles in that month)
│   │   ├── article-1.md                # Individual blog post
│   │   └── article-2.md                # Individual blog post
│   └── 06/
│       ├── _index.md                   # Month index
│       └── article-3.md                # Individual blog post
└── 2024/
    ├── _index.md                       # Year index
    └── 01/
        └── article-4.md
```

**Key Differences from Learning Content**:

| Feature                     | Learning Content (/learn/, /belajar/)                              | Blogging Content (/rants/, /celoteh/)                                          |
| --------------------------- | ------------------------------------------------------------------ | ------------------------------------------------------------------------------ |
| **Organization**            | Topic-based hierarchy (e.g., swe/prog-lang/golang/)                | Time-based hierarchy (year/month/)                                             |
| **Overview/Ikhtisar**       | **REQUIRED** - Every folder needs overview.md or ikhtisar.md       | **NOT REQUIRED** - No overview files needed                                    |
| **Index File Structure**    | Navigation lists (3 layers deep)                                   | Year index: 3-layer tree; Month index: flat article list                       |
| **Directory Depth Purpose** | Represents topic nesting                                           | Represents time period (year/month)                                            |
| **Content Separation**      | `_index.md` navigation only, intro in overview/ikhtisar            | `_index.md` can include intro text (no separate overview required)             |
| **Weight System**           | Level-based (\_index uses parent level, content uses current base) | Same level-based system (\_index uses parent level, articles use current base) |
| **Naming Convention**       | Topic slugs (getting-started.md, advanced-patterns.md)             | Descriptive article slugs (why-i-switched-to-neovim.md)                        |
| **Author Field**            | FORBIDDEN (uses site-level config)                                 | ALLOWED (guest contributors possible)                                          |
| **Validation Strictness**   | Strict structure enforcement                                       | Flexible structure (no overview requirement)                                   |
| **Diátaxis Organization**   | Optional (tutorials/, how-to/, reference/, explanation/)           | Not applicable (chronological organization)                                    |

**Year Index File (`_index.md`) Structure**:

Year index files display a **3-layer tree structure** showing:

1. **Layer 1**: Year (current level)
2. **Layer 2**: Months within that year (children)
3. **Layer 3**: Articles within each month (grandchildren)

**Example Year Index** (`/en/rants/2023/_index.md`):

```markdown
---
title: "2023 Rants"
weight: 102 # Level 3 - positions 2023/ among rants/ children
date: 2023-01-01T00:00:00+07:00
draft: false
---

- [July 2023](/en/rants/2023/07)
  - [Why I Switched to Neovim](/en/rants/2023/07/why-i-switched-to-neovim)
  - [The Case for Trunk Based Development](/en/rants/2023/07/trunk-based-development)
- [June 2023](/en/rants/2023/06)
  - [Building Apps That Last](/en/rants/2023/06/building-apps-that-last)
  - [Simplicity in Software Engineering](/en/rants/2023/06/simplicity-in-swe)
```

**Month Index File (`_index.md`) Structure**:

Month index files display a **flat list of articles** within that month.

**Example Month Index** (`/en/rants/2023/07/_index.md`):

```markdown
---
title: "July 2023"
weight: 1002 # Level 4 - represents the level 4 folder (07/)
date: 2023-07-01T07:20:00+07:00
draft: false
---

- [Why I Switched to Neovim](/en/rants/2023/07/why-i-switched-to-neovim)
- [The Case for Trunk Based Development](/en/rants/2023/07/trunk-based-development)
```

**Weight System for Blogging Content**:

Uses the **same level-based system** as learning content:

- Folder at level N → `_index.md` uses level N weight
- Content inside folder → uses level N+1 weight (one level deeper)
- Weights reset for children of each parent folder
- Hugo compares siblings only (no need to coordinate across parents)

**Example Weights** (for `/en/rants/2023/07/`):

- Level calculation: `/en/` (1) → `/rants/` (2) → `/2023/` (3) → `/07/` (4)
- `_index.md` for month (07/): `weight: 1002` (level 4 - represents the level 4 folder)
- First article: `weight: 10000` (level 5 base - content inside level 4 folder)
- Second article: `weight: 10001` (level 5 base + 1)

**Important Notes**:

1. **No Overview/Ikhtisar Required**: Unlike learning content, blogging content does NOT need separate overview.md or ikhtisar.md files
2. **Index Can Include Intro**: Month/year `_index.md` files MAY include introductory text (not restricted to navigation only)
3. **Chronological Ordering**: Articles are typically ordered by date within month (newest first), though weight field provides manual control
4. **Flexible Structure**: Blogging content has more structural flexibility than learning content (fewer validation rules)

**Article Frontmatter Example** (`/en/rants/2023/07/why-i-switched-to-neovim.md`):

```yaml
---
title: "Why I Switched to Neovim"
date: 2023-07-15T10:30:00+07:00
draft: false
description: "Personal reflections on moving from Vim to Neovim and the productivity benefits"
weight: 10000 # Level 5 base - content inside level 4 folder (07/)
tags: ["tools", "vim", "neovim", "productivity"]
# Note: No categories field - causes raw text leak in Hextra theme
author: "Wahidyan Kresna Fridayoka" # Optional - allowed in rants/celoteh
---
After 5 years of using Vim, I finally made the switch to Neovim...
```

**Validation Differences**:

For blogging content (`/en/rants/`, `/id/celoteh/`):

- ✅ Weight field ordering: Same level-based system
- ✅ Author field: ALLOWED (guest contributors possible)
- ✅ Year/month directory structure: REQUIRED
- ✅ Index files: REQUIRED (year and month levels)
- ❌ Overview/Ikhtisar files: NOT REQUIRED (major difference)
- ❌ Content separation rule: NOT ENFORCED (index can have intro)
- ❌ 3-layer navigation: Different pattern (year → months → articles tree)
- ❌ Diátaxis organization: NOT APPLICABLE (chronological only)

**Creating Blogging Content**:

```bash
# Create article (English)
hugo new content/en/rants/2024/01/my-new-post.md --kind celoteh

# Create article (Indonesian)
hugo new content/id/celoteh/2024/01/postingan-baru.md --kind celoteh

# Create month index (if not exists)
hugo new content/en/rants/2024/01/_index.md --kind _index

# Create year index (if not exists)
hugo new content/en/rants/2024/_index.md --kind _index
```

---

## Archetypes

**Location**: `apps/ayokoding-web/archetypes/`

**Available Archetypes** (5 total):

### 1. learn.md - Educational/Tutorial Content

```yaml
---
title: '{{ replace .File.ContentBaseName "-" " " | title }}'
date: { { .Date } }
draft: true
description: ""
weight: 10
tags: []
# Note: No categories field - causes raw text leak in Hextra theme
# Note: No author field - uses site-level config (params.author in hugo.yaml)
---
```

### 2. celoteh.md - Personal Essays/Rants

```yaml
---
title: '{{ replace .File.ContentBaseName "-" " " | title }}'
date: { { .Date } }
draft: true
description: ""
tags: []
# Note: No categories field - causes raw text leak in Hextra theme
author: "Author Name" # Optional - allowed in rants/celoteh directories
---
```

### 3. konten-video.md - Video Content

```yaml
---
title: '{{ replace .File.ContentBaseName "-" " " | title }}'
date: { { .Date } }
draft: true
description: ""
tags: []
videoUrl: ""
# Note: No categories field - causes raw text leak in Hextra theme
# Note: No author field - uses site-level config (params.author in hugo.yaml)
---
```

### 4. \_index.md - Section Index Pages

Template for navigation hub files.

### 5. default.md - Default Template

Fallback template for general content.

---

## Taxonomy

**CRITICAL**: ayokoding-web does NOT use the `categories` field in frontmatter (causes raw text leak).

**Content Categorization** (via directory structure):

- `/en/learn/` and `/id/belajar/` - Educational content
- `/en/rants/` - Personal essays (English)
- `/id/celoteh/` - Personal essays (Indonesian)
- `/en/video-content/` and `/id/konten-video/` - Video content

**Tags** (flexible):

- Granular topics (e.g., "nodejs", "api", "tutorial", "beginner")
- Multiple tags allowed per content
- Use lowercase, hyphenated format
- MUST use single-line JSON array format: `["tag1", "tag2"]`

**Example**:

```yaml
---
title: "Understanding Async/Await in JavaScript"
tags: ["javascript", "async", "promises", "tutorial"]
# Note: No categories field - categorization through directory structure
---
```

---

## Configuration Highlights

**File**: `apps/ayokoding-web/hugo.yaml`

### Language Configuration

```yaml
defaultContentLanguage: id
defaultContentLanguageInSubdir: true

languages:
  id:
    languageName: "Bahasa Indonesia"
    languageCode: "id-ID"
    weight: 1
  en:
    languageName: "English"
    languageCode: "en-US"
    weight: 2
```

### Module Mounts

```yaml
module:
  imports:
    - path: github.com/imfing/hextra
  mounts:
    - source: content/en
      target: content
      lang: en
    - source: content/id
      target: content
      lang: id
```

### LaTeX Math Rendering

```yaml
markup:
  goldmark:
    extensions:
      passthrough:
        delimiters:
          block: [["$$", "$$"]]
          inline: [["$", "$"]]
        enable: true

params:
  math:
    enable: true
    engine: katex
```

### FlexSearch Configuration

```yaml
params:
  search:
    enable: true
    type: flexsearch
    flexsearch:
      index: content # Index full content
      tokenize: full # Full tokenization
```

### SEO & Author Configuration

```yaml
params:
  # Site-level author (used when content doesn't have author field)
  author: "Wahidyan Kresna Fridayoka"

  # Default Open Graph image
  images:
    - /logo.png

  # Social media profiles
  social:
    threads: "https://www.threads.net/@wahidyankf"
    github: "https://github.com/wahidyankf/open-sharia-enterprise"
    youtube: "https://www.youtube.com/@AyoKoding"
```

**Important**: The `params.author` field serves as the default author for ALL content. Individual content files should NOT include `author:` in frontmatter unless they have guest contributors (rants/celoteh only).

### Sitemap & Analytics

```yaml
sitemap:
  changefreq: weekly
  filename: sitemap.xml
  priority: 0.5

services:
  googleAnalytics:
    ID: G-1NHDR7S3GV

privacy:
  googleAnalytics:
    anonymizeIP: true
    respectDoNotTrack: true
```

---

## Content Creation Workflow

### Creating New Content

```bash
# Create learning content (Indonesian)
hugo new content/id/belajar/nodejs/getting-started.md --kind learn

# Create learning content (English)
hugo new content/en/learn/nodejs/getting-started.md --kind learn

# Create personal essay (Indonesian)
hugo new content/id/celoteh/my-thoughts.md --kind celoteh

# Create section index
hugo new content/id/belajar/rust/_index.md --kind _index
```

### Content Validation Checklist

Before publishing, verify:

- [ ] Frontmatter uses YAML format with 2-space indentation
- [ ] Date format is `YYYY-MM-DDTHH:MM:SS+07:00`
- [ ] Description length is 150-160 characters (if present)
- [ ] Internal links use absolute paths without `.md` extension
- [ ] All images have descriptive alt text
- [ ] Mermaid diagrams use accessible color palette
- [ ] Mermaid comments use `%% Comment %%` format (NO curly braces)
- [ ] Mermaid text with forward slashes is quoted (e.g., `["Path: /admin"]`)
- [ ] Hextra shortcodes use `{{% %}}` delimiters for Markdown content
- [ ] Tags use single-line JSON array format: `["tag1", "tag2"]`
- [ ] NO `categories` field in frontmatter (causes raw text leak)
- [ ] Draft status is set correctly (`draft: true/false`)
- [ ] `weight` field follows level-based ordering rules (\_index.md represents folder at level N, content inside uses level N+1)
- [ ] NO `author` field in learning content (uses site-level config)
- [ ] `author` field ONLY in rants/celoteh directories if needed
- [ ] Every folder has `overview.md` (English) or `ikhtisar.md` (Indonesian)
- [ ] `_index.md` files show 3-layer navigation with overview links first
- [ ] Content follows archetype patterns
- [ ] Assets are organized in appropriate `static/` subdirectories

---

## Asset Organization

**Location**: `apps/ayokoding-web/static/`

**Structure**:

```
static/
└── images/
    ├── learn/
    │   └── typescript/
    ├── celoteh/
    └── video-content/
```

**Image References**:

```markdown
![TypeScript Logo](/images/learn/typescript/logo.png)

{{< figure src="/images/learn/typescript/generics.png" alt="TypeScript Generics Diagram" >}}
```

---

## References

**Required Reading**:

- [Shared Hugo Content Conventions](./ex-co__hugo-content-shared.md) - Read this first!

**Theme Documentation**:

- [Hextra Official Documentation](https://imfing.github.io/hextra/docs/)
- [Hextra GitHub Repository](https://github.com/imfing/hextra)

**Related AI Agents**:

- [ayokoding-content-maker](../../.claude/agents/ayokoding-content-maker.md) - Creates ayokoding-web content
- [ayokoding-content-checker](../../.claude/agents/ayokoding-content-checker.md) - Validates ayokoding-web content
- [ayokoding-link-checker](../../.claude/agents/ayokoding-link-checker.md) - Validates links in ayokoding-web
- [ayokoding-deployer](../../.claude/agents/ayokoding-deployer.md) - Deploys ayokoding-web to production
