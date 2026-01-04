---
title: "Hugo Content Convention - ose-platform-web"
description: Hugo content conventions specific to ose-platform-web (PaperMod theme, project landing page)
category: explanation
subcategory: conventions
tags:
  - hugo
  - content
  - ose-platform-web
  - papermod
  - landing-page
created: 2025-12-13
updated: 2025-12-13
---

# Hugo Content Convention - ose-platform-web

This document defines Hugo content conventions specific to **ose-platform-web** - an English-only project landing page using the PaperMod theme.

## Principles Implemented/Respected

This convention implements the following core principles:

- **[Simplicity Over Complexity](../principles/general/ex-ru-pr-ge__simplicity-over-complexity.md)**: Flat content structure (updates/ and about.md) - no deep hierarchies or complex organization. English-only, single theme, minimal configuration. Updates sorted by date automatically - no manual weight management.

- **[Explicit Over Implicit](../principles/software-engineering/ex-ru-pr-se__explicit-over-implicit.md)**: Update filenames encode the date (YYYY-MM-DD prefix) making chronological order explicit and sortable.

## Purpose

This convention defines content creation standards specific to the ose-platform-web Hugo site, which uses the PaperMod theme and serves as an English-only landing page. It establishes frontmatter requirements and content patterns unique to ose-platform-web, complementing the shared Hugo conventions.

## Scope

### What This Convention Covers

- **OSE platform frontmatter** - Required YAML fields specific to PaperMod theme
- **Landing page structure** - English-only single-page design
- **PaperMod theme specifics** - Cover image, author info, table of contents, and other PaperMod features
- **Navigation patterns** - Simple navigation structure for landing page
- **Content organization** - How to structure landing page sections

### What This Convention Does NOT Cover

- **Shared Hugo conventions** - Covered in [Hugo Content Shared Convention](./ex-ru-co-hu__shared.md)
- **Bilingual content** - This site is English-only, see ayokoding-web for bilingual patterns
- **Hugo theme development** - Covered in [Hugo Development Convention](../development/hugo/ex-ru-de-hu__development.md)
- **Deployment** - Covered by ose-platform-web-deployer agent

## Prerequisites

**IMPORTANT**: This document assumes familiarity with [Shared Hugo Content Conventions](./ex-ru-co-hu__shared.md).

Read the shared conventions first, as they cover:

- Inherited conventions (8 standards from docs/)
- Adapted conventions (5 Hugo-specific modifications)
- Hugo-specific conventions (6 basic concepts)

This document covers **ose-platform-web specific patterns only**.

---

## ose-platform-web Overview

**Site**: [oseplatform.com](https://oseplatform.com)
**Theme**: PaperMod v7.0+ (compatible with v8.0)
**Purpose**: English-only project landing page with progress updates
**Languages**: English only
**Repository Path**: `apps/ose-platform-web/`

**Content Types**:

- Platform updates (progress, releases, announcements)
- About page (project information)

---

## Content Structure

```
apps/ose-platform-web/content/
├── updates/                               # Platform updates
│   ├── _index.md
│   ├── 2025-12-07-initial-release.md
│   └── 2025-11-20-announcement.md
└── about.md                               # About page
```

**Simple flat structure** - no deep hierarchies, no multi-language subdirectories.

---

## PaperMod Theme

**Official Documentation**: [PaperMod Site](https://adityatelange.github.io/hugo-PaperMod/)
**GitHub**: [adityatelange/hugo-PaperMod](https://github.com/adityatelange/hugo-PaperMod)
**Last Updated**: 2025-10-26 | **GitHub Stars**: 12,755
**Theme Version Compatibility**: Targets v7.0+, compatible with v8.0

**Description**: Fast, clean, responsive Hugo theme based on hugo-paper, focused on simplicity with useful features.

### PaperMod Key Features

- **Design**: Clean, simple, fast, and responsive
- **Theme Support**: Light/dark mode with localStorage preference
- **Navigation**: Smooth scrolling, breadcrumbs, archive, search
- **Social**: Share buttons for multiple platforms
- **SEO**: Built-in SEO optimization
- **Accessibility**: Reduced-motion support, semantic HTML
- **Analytics**: Google Analytics, Bing, Yandex site verification

### PaperMod-Specific Frontmatter Fields

```yaml
---
title: "Post Title"
date: 2025-12-07T14:30:00+07:00
draft: false
description: "Post description" # Subtitle/description
summary: "Brief summary" # For list pages
showtoc: true # Enable table of contents
tocopen: false # ToC collapsed by default
hidemeta: false # Show post metadata
comments: true # Show comments section
searchHidden: false # Include in search
hideSummary: false # Show in lists
author: "Author Name" # Single or multiple authors
weight: 1 # Page order/pin position
canonicalURL: "https://..." # Canonical link
robotsNoIndex: false # Exclude from indexing
cover:
  image: "/images/cover.png" # Cover image
  caption: "Image caption" # Cover caption
  alt: "Image description" # Alt text
  relative: false # Use relative paths
  responsiveImages: true # Generate responsive variants
  hidden: false # Hide on current page
tags: ["tag1", "tag2"]
categories: ["updates"]
---
```

### PaperMod Shortcodes

PaperMod is minimal and relies primarily on **Hugo's built-in shortcodes**:

```markdown
{{< figure src="/images/dashboard.png" caption="OSE Platform Dashboard" >}}

Check out our [getting started guide]({{< ref "/updates/getting-started" >}})
```

**Available Hugo Built-in Shortcodes**:

- `{{< ref >}}` - Internal content reference
- `{{< relref >}}` - Relative content reference
- `{{< figure >}}` - Image with caption
- `{{< highlight >}}` - Syntax highlighting
- `{{< youtube >}}` - Embed YouTube video
- `{{< tweet >}}` - Embed tweet

---

## ose-platform-web Site Patterns

### English-Only Content

**No Multi-Language Structure**:

- All content in English
- No language subdirectories
- Simple, flat content organization

### Simple Content Organization

**Content Types**:

1. **Updates** (`content/updates/`)
   - Platform progress reports
   - Feature releases
   - Announcements
   - Date-prefixed filenames: `YYYY-MM-DD-title.md`

2. **About Page** (`content/about.md`)
   - Project information
   - Team details
   - Contact information

**File Naming Pattern**:

```
content/
├── updates/
│   ├── _index.md
│   ├── 2025-12-07-beta-release.md         # Date-prefixed
│   └── 2025-11-20-announcement.md          # Date-prefixed
└── about.md                                 # Simple slug
```

### Frontmatter Patterns

**Updates Content**:

```yaml
---
title: "OSE Platform Beta Release"
date: 2025-12-07T14:30:00+07:00
draft: false
tags: ["release", "beta", "announcement"]
categories: ["updates"]
summary: "Introducing the beta version of Open Sharia Enterprise Platform"
showtoc: true
cover:
  image: "/images/beta-release.png"
  alt: "OSE Platform Dashboard Screenshot"
---
```

**About Page**:

```yaml
---
title: "About OSE Platform"
url: "/about/"
summary: "Learn about Open Sharia Enterprise Platform"
showtoc: false
---
```

### Author Field Usage

**Flexible Author Field**:

- `author:` field is allowed and used per-post
- Can be single author or multiple authors
- No site-level default author restriction (unlike ayokoding-web)

**Example**:

```yaml
---
title: "Platform Architecture Overview"
author: "OSE Platform Team"
# or
author: ["John Doe", "Jane Smith"]
---
```

---

## Archetypes

**Location**: `apps/ose-platform-web/archetypes/`

**Available Archetypes** (1 total):

### default.md - All Content Types

```yaml
---
title: '{{ replace .File.ContentBaseName "-" " " | title }}'
date: { { .Date } }
draft: false
tags: []
categories: []
summary: ""
---
```

**Simple archetype** - applies to all content (updates, about pages, etc.)

---

## Taxonomy

**Categories**:

- `["updates"]` - Platform updates (primary category)
- Flexible - can add new categories as needed

**Tags**:

- Flexible topics (e.g., "release", "feature", "announcement", "architecture")
- Multiple tags allowed per content
- Use lowercase format

**Series** (optional):

- `series: ["platform-architecture"]` - For multi-part series
- Groups related posts together

**Example**:

```yaml
---
title: "Platform Architecture Overview - Part 1"
tags: ["architecture", "system-design"]
categories: ["updates"]
series: ["platform-architecture"]
---
```

---

## Configuration Highlights

**File**: `apps/ose-platform-web/hugo.yaml`

### Basic Configuration

```yaml
baseURL: "https://oseplatform.com/"
title: "OSE Platform"
languageCode: "en-us"
defaultContentLanguage: "en"

theme: "PaperMod"
```

### PaperMod Parameters

```yaml
params:
  env: production
  author: "OSE Platform Team"

  # Content display
  ShowReadingTime: true
  ShowShareButtons: true
  ShowCodeCopyButtons: true
  ShowPostNavLinks: true
  ShowBreadCrumbs: true

  # Theme
  defaultTheme: auto # light/dark/auto

  # Social sharing
  ShareButtons:
    - twitter
    - linkedin
    - reddit
```

### Home Page Configuration

```yaml
params:
  homeInfoParams:
    Title: "Welcome to OSE Platform"
    Content: "Open Sharia Enterprise Platform documentation and updates"

  socialIcons:
    - name: github
      url: "https://github.com/wahidyankf/open-sharia-enterprise"
    - name: twitter
      url: "https://twitter.com/oseplatform"
```

### Cover Image Defaults

```yaml
params:
  cover:
    responsiveImages: true
    hidden: false
    linkFullImages: true
```

---

## Content Creation Workflow

### Creating New Content

```bash
# Create update post
hugo new content/updates/2025-12-07-feature-release.md

# Create about page
hugo new content/about.md
```

### Content Validation Checklist

Before publishing, verify:

- [ ] Frontmatter uses YAML format with 2-space indentation
- [ ] Date format is `YYYY-MM-DDTHH:MM:SS+07:00`
- [ ] Description length is 150-160 characters (if present)
- [ ] Internal links use absolute paths without `.md` extension
- [ ] All images have descriptive alt text
- [ ] Mermaid diagrams use accessible color palette (if used)
- [ ] Tags and categories are properly formatted arrays
- [ ] Draft status is set correctly (`draft: true/false`)
- [ ] Update posts use date-prefixed filenames
- [ ] Cover images have alt text
- [ ] Summary field is provided for list pages
- [ ] Assets are organized in appropriate `static/` subdirectories

---

## Asset Organization

**Location**: `apps/ose-platform-web/static/`

**Structure**:

```
static/
├── images/
│   ├── updates/
│   └── about/
└── casts/                    # Asciinema recordings
```

**Image References**:

```markdown
![OSE Platform Dashboard](/images/updates/dashboard.png)

{{< figure src="/images/updates/architecture.png" alt="System Architecture" caption="OSE Platform Architecture" >}}
```

**Asciinema Casts**:

```markdown
<!-- Reference terminal recordings -->

![Terminal Demo](/casts/demo.cast)
```

---

## Comparison with ayokoding-web

| Aspect                 | ose-platform-web                       | ayokoding-web                                     |
| ---------------------- | -------------------------------------- | ------------------------------------------------- |
| **Theme**              | PaperMod                               | Hextra                                            |
| **Languages**          | English only                           | Bilingual (Indonesian/English)                    |
| **Content Types**      | Updates, about page                    | Learning, essays, video content                   |
| **Content Structure**  | Flat (updates/, about.md)              | Deep hierarchy (learn/archived/crash-courses/...) |
| **Archetypes**         | 1 type (default)                       | 5 types (learn, celoteh, konten-video, etc.)      |
| **Primary Purpose**    | Project landing & progress updates     | Educational platform for developers               |
| **Target Audience**    | Enterprise users (international)       | Indonesian developers (bilingual)                 |
| **Tutorial Content**   | No (not applicable)                    | Yes (id/belajar/, en/learn/)                      |
| **Navigation**         | Breadcrumbs, archive, smooth scrolling | Auto-sidebar, 3-layer nav, breadcrumbs            |
| **Author Field**       | Per-post (flexible)                    | Site-level default (rants/celoteh exceptions)     |
| **Complexity**         | Simple, minimal                        | Feature-rich, complex structure                   |
| **Weight Ordering**    | Optional                               | Strict rules (1=index, 2=overview, 3+=content)    |
| **Overview Files**     | Not required                           | Required in every folder                          |
| **Diátaxis Structure** | Not applicable                         | Optional at topic level                           |

---

## References

**Required Reading**:

- [Shared Hugo Content Conventions](./ex-ru-co-hu__shared.md) - Read this first!

**Theme Documentation**:

- [PaperMod Official Documentation](https://adityatelange.github.io/hugo-PaperMod/)
- [PaperMod GitHub Repository](https://github.com/adityatelange/hugo-PaperMod)

**Related AI Agents**:

- [ose-platform-web-content-maker](https://github.com/wahidyankf/open-sharia-enterprise/blob/main/.claude/agents/ose-platform-web-content-maker.md) - Creates ose-platform-web content
- [ose-platform-web-content-checker](https://github.com/wahidyankf/open-sharia-enterprise/blob/main/.claude/agents/ose-platform-web-content-checker.md) - Validates ose-platform-web content
- [ose-platform-web-deployer](https://github.com/wahidyankf/open-sharia-enterprise/blob/main/.claude/agents/ose-platform-web-deployer.md) - Deploys ose-platform-web to production
