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
updated: 2025-12-14
---

# Hugo Content Convention - ayokoding-web

This document defines Hugo content conventions specific to **ayokoding-web** - a bilingual educational platform using the Hextra theme.

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

**Example Usage**:

```markdown
{{< callout type="info" >}}
This is an informational callout using Hextra's callout shortcode.
{{< /callout >}}

{{< steps >}}

### Step 1: Install Dependencies

Run `npm install` to install required packages.

### Step 2: Configure Settings

Edit the configuration file...

{{< /steps >}}

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

### Weight Field Ordering

**CRITICAL RULE**: All content files use the `weight` field to control navigation ordering (lower weight = first in list). ayokoding-web uses a **depth-based weight system** to ensure clear separation between directory levels.

**Weight Formula**: `weight = (depth × 100) + sequence`

Where:

- `depth` = directory nesting level (0 = content root, 1 = first level, 2 = second level, etc.)
- `sequence` = position within that depth level (1 for \_index.md, 2 for overview.md, 3+ for content)

**CRITICAL: Weights Reset for Each Folder at Same Depth**

Each folder at the same depth uses the SAME weight pattern:

- `/en/learn/swe/prog-lang/golang/` → 401, 402, 403...
- `/en/learn/swe/prog-lang/java/` → 401, 402, 403... (RESET, not 404, 405...)
- `/en/learn/swe/prog-lang/python/` → 401, 402, 403... (RESET, not 407, 408...)

**Why**: Folders are self-contained and independent. No coordination needed across siblings. This makes adding/removing/reorganizing folders easier.

**Depth Level Ranges**:

- Depth 0: 1-99 (root level: `/en/`, `/id/`)
- Depth 1: 101-199 (e.g., `/en/learn/`, `/id/belajar/`)
- Depth 2: 201-299 (e.g., `/en/learn/swe/`)
- Depth 3: 301-399 (e.g., `/en/learn/swe/prog-lang/`)
- Depth 4: 401-499 (e.g., `/en/learn/swe/prog-lang/golang/`)
- And so on...

**Standard Sequence Within Each Depth**:

1. `_index.md` → `depth × 100 + 1`
2. `overview.md`/`ikhtisar.md` → `depth × 100 + 2`
3. Content files → `depth × 100 + 3, 4, 5...`

**Rationale**: This system ensures subdirectories at deeper levels (e.g., depth 4 = 401+) always appear after content at shallower levels (e.g., depth 3 = 301-399), preventing weight conflicts and ensuring predictable navigation order.

**Scope**: Applies to ALL content in `apps/ayokoding-web/content/` (both `/en/` and `/id/`).

✅ **Good (correct depth-based weight ordering)**:

```yaml
# /en/learn/swe/prog-lang/golang/_index.md (depth 4)
---
title: Golang
weight: 401 # depth 4: (4 × 100) + 1 = 401
---
# /en/learn/swe/prog-lang/golang/overview.md (depth 4)
---
title: Overview
weight: 402 # depth 4: (4 × 100) + 2 = 402
---
# /en/learn/swe/prog-lang/golang/initial-setup.md (depth 4)
---
title: Initial Setup
weight: 403 # depth 4: (4 × 100) + 3 = 403
---
# /en/learn/swe/prog-lang/golang/quick-start.md (depth 4)
---
title: Quick Start
weight: 404 # depth 4: (4 × 100) + 4 = 404
---
# /en/learn/swe/prog-lang/golang/tutorials/_index.md (depth 5 - subdirectory)
---
title: Tutorials
weight: 501 # depth 5: (5 × 100) + 1 = 501
---
```

**Example: Multi-level Structure**

```
/en/learn/ (depth 1, base = 100)
├── _index.md (weight: 101)
├── overview.md (weight: 102)
├── swe/ (depth 2, base = 200)
│   ├── _index.md (weight: 201)
│   ├── overview.md (weight: 202)
│   └── prog-lang/ (depth 3, base = 300)
│       ├── _index.md (weight: 301)
│       ├── overview.md (weight: 302)
│       ├── golang/ (depth 4, base = 400)
│       │   ├── _index.md (weight: 401)
│       │   ├── overview.md (weight: 402)
│       │   └── tutorials/ (depth 5, base = 500)
│       │       └── _index.md (weight: 501)
│       └── java/ (depth 4, base = 400)
│           ├── _index.md (weight: 401)  # RESET to base + 1 (self-contained)
│           └── overview.md (weight: 402) # RESET to base + 2 (independent)
```

❌ **Bad (incorrect weight ordering)**:

```yaml
# WRONG! Not using depth-based system
---
title: Golang
weight: 1 # Should be 401 at depth 4
---
# WRONG! Conflicting weights at different depths
/en/learn/overview.md → weight: 2
/en/learn/swe/overview.md → weight: 2 # Conflict!

# WRONG! Missing weight field
---
title: Initial Setup
# Missing weight field entirely
---
```

**Benefits of This System**:

- ✅ **No conflicts**: Each depth has its own 100-number range
- ✅ **Automatic separation**: Subdirectories (deeper levels) always appear after shallower content
- ✅ **Predictable**: Easy to calculate weights (depth × 100 + position)
- ✅ **Scalable**: Supports up to 99 items per depth level
- ✅ **Readable**: Depth 4 = 400s, Depth 5 = 500s (easy to understand at a glance)
- ✅ **Self-contained**: Each folder at the same depth resets to the same weight pattern (401, 402, 403...) - no coordination needed across siblings
- ✅ **Independent**: Adding, removing, or reordering sibling folders doesn't affect each other's weights
- ✅ **Maintainable**: Moving folders between locations is simpler - just update depth calculation, not sequential numbering

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
# File: content/en/learn/swe/prog-lang/overview.md (depth 3)
---
title: "Overview" # Simple, generic - context from path
weight: 302 # depth 3: (3 × 100) + 2
---
# File: content/id/belajar/swe/prog-lang/ikhtisar.md (depth 3)
---
title: "Ikhtisar" # Simple, generic - context from path
weight: 302 # depth 3: (3 × 100) + 2
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

**English Structure** (depth 4 example):

```
content/en/learn/swe/prog-lang/golang/
├── _index.md        # Navigation only (weight: 401)
├── overview.md      # English intro content (weight: 402)
├── initial-setup.md # (weight: 403)
├── quick-start.md   # (weight: 404)
├── beginner.md      # (weight: 405)
├── intermediate.md  # (weight: 406)
├── advanced.md      # (weight: 407)
└── cookbook.md      # (weight: 408)
```

**Indonesian Structure** (depth 4 example):

```
content/id/belajar/swe/prog-lang/golang/
├── _index.md        # Navigation only (weight: 401)
├── ikhtisar.md      # Indonesian intro content (weight: 402)
├── initial-setup.md # (weight: 403)
├── quick-start.md   # (weight: 404)
├── beginner.md      # (weight: 405)
├── intermediate.md  # (weight: 406)
├── advanced.md      # (weight: 407)
└── cookbook.md      # (weight: 408)
```

**Example `_index.md` (navigation only with overview link first)**:

```markdown
---
title: Golang
weight: 401 # depth 4: /en/learn/swe/prog-lang/golang/
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
weight: 402 # depth 4: (4 × 100) + 2
tags: ["golang", "programming", "overview"]
categories: ["learn"]
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
weight: 402 # depth 4: (4 × 100) + 2
tags: ["golang", "programming", "ikhtisar"]
categories: ["learn"]
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
categories: ["learn"]
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
categories: ["celoteh"]
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
categories: ["video"]
videoUrl: ""
# Note: No author field - uses site-level config (params.author in hugo.yaml)
---
```

### 4. \_index.md - Section Index Pages

Template for navigation hub files.

### 5. default.md - Default Template

Fallback template for general content.

---

## Taxonomy

**Categories** (fixed set):

- `["learn"]` - Educational content
- `["celoteh"]` - Personal essays (Indonesian)
- `["rants"]` - Personal essays (English)
- `["video"]` - Video content

**Tags** (flexible):

- Granular topics (e.g., "nodejs", "api", "tutorial", "beginner")
- Multiple tags allowed per content
- Use lowercase, hyphenated format

**Example**:

```yaml
---
title: "Understanding Async/Await in JavaScript"
tags: ["javascript", "async", "promises", "tutorial"]
categories: ["learn"]
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
- [ ] Tags and categories are properly formatted arrays
- [ ] Draft status is set correctly (`draft: true/false`)
- [ ] `weight` field follows depth-based ordering rules (formula: depth × 100 + sequence)
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
