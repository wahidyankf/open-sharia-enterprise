# Hugo Content Convention and Agents

**Status**: ✅ Complete

## Overview

Establish standardized Hugo content conventions, create universal Content Quality Principles convention, and create 4 specialized AI agents for managing content in two Hugo-based websites (ayokoding-web and ose-platform-web).

**Deliverables**:

- 1 Hugo Content Convention (governance/conventions/ex-co\_\_hugo-content.md)
- 1 Content Quality Principles Convention (governance/conventions/ex-co\_\_content-quality.md)
- 4 AI Agents (ayokoding-web-general-maker, ayokoding-web-general-checker, ose-platform-web-content-maker, ose-platform-web-content-checker)

**Git Workflow**: Commit to `main`

**Delivery Type**: Single PR

---

## Requirements

### Objectives

#### Primary Objectives

1. **Create comprehensive Hugo content convention**
   - Document 7 inherited conventions from existing docs/ standards (Mathematical Notation, Color Accessibility, Diagrams, Emoji Usage, Timestamp Format, Tutorial Convention, Tutorial Naming Convention)
   - Define Hugo-specific adaptations for web content
   - Establish Hugo-unique conventions (archetypes, shortcodes, taxonomy)
   - Document theme-specific differences (Hextra vs PaperMod) with validated web research
   - Document site-specific differences between ayokoding-web and ose-platform-web
   - Success criteria: Convention doc covers all aspects of Hugo content creation and maintenance

2. **Create Content Quality Principles convention**
   - Document universal markdown content quality standards
   - Apply to ALL markdown content (docs/, Hugo sites, plans/, repository docs)
   - Cover writing style, heading hierarchy, accessibility, code formatting, lists, emphasis, blockquotes, tables
   - Success criteria: Convention provides clear quality standards applicable across all markdown contexts

3. **Create 4 specialized Hugo content agents**
   - ayokoding-web-general-maker (Green - creator agent)
   - ayokoding-web-general-checker (Yellow - validator agent)
   - ose-platform-web-content-maker (Green - creator agent)
   - ose-platform-web-content-checker (Yellow - validator agent)
   - Success criteria: All agents follow AI Agents Convention and reference Hugo content convention

4. **Update repository indices and CLAUDE.md**
   - Add Hugo content convention to conventions index
   - Add Content Quality Principles convention to conventions index
   - Add 4 new agents to agents index
   - Update CLAUDE.md with Hugo content guidance
   - Success criteria: All indices and CLAUDE.md accurately reflect new conventions and agents

#### Secondary Objectives

1. **Establish validation patterns for Hugo content**
   - Define what checkers validate (frontmatter, structure, links, SEO)
   - Document validation workflow
   - Success criteria: Clear validation guidelines for content quality

### User Stories

#### Story 1: Content Creator Needs Convention Guidance

**As a** content creator for ayokoding-web or ose-platform-web
**I want** clear Hugo content conventions
**So that** I can create consistent, high-quality content that follows repository standards

**Acceptance Criteria** (Gherkin):

```gherkin
Scenario: Content creator references Hugo convention
  Given I need to create new Hugo content
  When I read the Hugo content convention
  Then I see inherited conventions (Mathematical Notation, Color Accessibility, Diagrams, Emoji, Timestamp, Tutorial Convention, Tutorial Naming)
  And I see adapted conventions (Indentation, Linking, File Naming, Frontmatter)
  And I see Hugo-specific conventions (Archetypes, Shortcodes, Taxonomy, Asset Organization)
  And I see theme-specific differences (Hextra vs PaperMod)
  And I see site-specific differences between ayokoding-web and ose-platform-web
  And I can follow clear examples for each convention

Scenario: Content creator uses site-specific archetype
  Given I'm creating content for ayokoding-web
  When I reference the Hugo convention for archetypes
  Then I see available archetypes (learn, celoteh, konten-video, default, _index)
  And I see which frontmatter fields are required for each archetype
  And I see example content structure for each archetype type
```

#### Story 2: AI Agent Creates Hugo Content

**As an** AI agent (ayokoding-web-general-maker or ose-platform-web-content-maker)
**I want** to reference the Hugo content convention
**So that** I can create properly structured, convention-compliant content

**Acceptance Criteria** (Gherkin):

```gherkin
Scenario: Agent creates new Hugo content
  Given I'm ayokoding-web-general-maker agent
  When I create new content in apps/ayokoding-web/content/
  Then I use correct frontmatter format (YAML with 2-space indentation)
  And I use correct timestamp format (YYYY-MM-DDTHH:MM:SS+07:00)
  And I use correct file naming convention for Hugo
  And I use correct linking format for Hugo (without .md extension)
  And I use Mermaid diagrams with accessible colors
  And I follow ayokoding-web-specific archetype patterns
  And I organize assets in static/images/ with proper structure

Scenario: Agent references site-specific conventions
  Given I'm ose-platform-web-content-maker agent
  When I create content for ose-platform-web
  Then I use PaperMod theme-specific frontmatter fields
  And I follow ose-platform-web content structure (updates/, about.md)
  And I use ose-platform-web-specific taxonomy (categories, tags, series)
```

#### Story 3: AI Agent Validates Hugo Content

**As an** AI agent (ayokoding-web-general-checker or ose-platform-web-content-checker)
**I want** to validate Hugo content against conventions
**So that** content quality and consistency are maintained

**Acceptance Criteria** (Gherkin):

```gherkin
Scenario: Agent validates frontmatter correctness
  Given I'm ayokoding-web-general-checker agent
  When I validate content in apps/ayokoding-web/content/
  Then I verify frontmatter uses YAML format with 2-space indentation
  And I verify required fields are present (title, date, draft)
  And I verify date format is YYYY-MM-DDTHH:MM:SS+07:00
  And I verify description length is 150-160 characters (if present)
  And I verify tags and categories arrays are properly formatted

Scenario: Agent validates content structure
  Given I'm validating Hugo content
  When I check content files
  Then I verify file naming follows Hugo conventions
  And I verify internal links use correct Hugo format (no .md extension)
  And I verify images reference correct paths in static/ directory
  And I verify Mermaid diagrams use accessible color palette
  And I verify shortcodes are used correctly

Scenario: Agent validates site-specific requirements
  Given I'm ose-platform-web-content-checker agent
  When I validate ose-platform-web content
  Then I verify content follows PaperMod theme requirements
  And I verify taxonomy usage matches site configuration
  And I verify SEO fields are properly populated
```

#### Story 4: Documentation Writer Uses Content Quality Principles

**As a** documentation writer creating any markdown content
**I want** clear content quality principles
**So that** my content is consistent, accessible, and professional across all repository contexts

**Acceptance Criteria** (Gherkin):

```gherkin
Scenario: Writer applies content quality principles
  Given I'm writing markdown content in any repository location (docs/, Hugo, plans/)
  When I reference the Content Quality Principles convention
  Then I see writing style and tone guidelines
  And I see heading hierarchy standards (single H1, proper H2-H6 nesting)
  And I see accessibility requirements (alt text, semantic HTML, ARIA labels)
  And I see code block formatting standards
  And I see text formatting guidance (bold, italic, emphasis)
  And I see list and bullet point conventions
  And I see blockquote and callout usage
  And I see table formatting standards
  And I see line length and readability guidelines
  And I see paragraph structure recommendations

Scenario: Writer creates accessible content
  Given I'm adding images to documentation
  When I reference accessibility standards
  Then I provide descriptive alt text for all images
  And I use semantic HTML elements appropriately
  And I ensure color is not the only means of conveying information
  And I maintain proper heading hierarchy for screen readers
  And I use sufficient color contrast in diagrams
```

### Functional Requirements

#### Hugo Content Convention Document

**REQ-001**: Create comprehensive Hugo content convention

- **Priority**: High
- **User Stories**: Story 1, Story 2, Story 3
- **Acceptance Criteria**: See Story 1 Gherkin scenarios

**REQ-002**: Document 7 inherited conventions with links

- **Priority**: High
- **User Stories**: Story 1, Story 2
- **Details**: Link to Mathematical Notation, Color Accessibility, Diagrams, Emoji Usage, Timestamp Format, Tutorial Convention, Tutorial Naming Convention conventions. Explain how each applies to Hugo content (e.g., Tutorial conventions apply to ayokoding-web's learning content)

**REQ-003**: Document adapted conventions for Hugo

- **Priority**: High
- **User Stories**: Story 1, Story 2, Story 3
- **Details**: Cover Indentation (spaces in frontmatter, standard markdown in content), Linking (Hugo ref/relref), File Naming, Frontmatter format

**REQ-004**: Document Hugo-specific conventions

- **Priority**: High
- **User Stories**: Story 1, Story 2
- **Details**: Cover Archetypes, Shortcodes, Taxonomy, Asset Organization, Content Types, URL structure

**REQ-005**: Document theme-specific differences (Hextra vs PaperMod)

- **Priority**: High
- **User Stories**: Story 1, Story 2, Story 3
- **Details**: Document validated theme characteristics from web research (Hextra: multilingual, FlexSearch, Tailwind CSS, shortcodes; PaperMod: clean blog theme, simple frontmatter, share buttons, analytics)

**REQ-006**: Document site-specific differences

- **Priority**: High
- **User Stories**: Story 1, Story 2, Story 3
- **Details**: Clearly differentiate ayokoding-web (Hextra theme, bilingual Indonesian/English, learning content) from ose-platform-web (PaperMod theme, English-only, updates/about)

#### Content Quality Principles Convention

**REQ-007**: Create comprehensive Content Quality Principles convention

- **Priority**: High
- **User Stories**: Story 4
- **Details**: Universal markdown content quality standards applicable to all repository markdown content

**REQ-008**: Document writing style and tone guidelines

- **Priority**: High
- **User Stories**: Story 4
- **Details**: Clear voice (active vs passive), professional tone, clarity, conciseness

**REQ-009**: Document heading hierarchy standards

- **Priority**: High
- **User Stories**: Story 4
- **Details**: Single H1 per document, proper H2-H6 nesting, descriptive headings, semantic structure

**REQ-010**: Document accessibility standards

- **Priority**: High
- **User Stories**: Story 4
- **Details**: Alt text for images, semantic HTML, ARIA labels where appropriate, color contrast, screen reader considerations

**REQ-011**: Document formatting conventions

- **Priority**: Medium
- **User Stories**: Story 4
- **Details**: Code blocks, text formatting (bold/italic/emphasis), lists, blockquotes, tables, line length, paragraph structure

#### AI Agent Creation

**REQ-012**: Create ayokoding-web-general-maker agent

- **Priority**: High
- **User Stories**: Story 2
- **Details**: Green (creator) agent for ayokoding-web content creation, references Hugo convention

**REQ-013**: Create ayokoding-web-general-checker agent

- **Priority**: High
- **User Stories**: Story 3
- **Details**: Yellow (validator) agent for ayokoding-web content validation

**REQ-014**: Create ose-platform-web-content-maker agent

- **Priority**: High
- **User Stories**: Story 2
- **Details**: Green (creator) agent for ose-platform-web content creation

**REQ-015**: Create ose-platform-web-content-checker agent

- **Priority**: High
- **User Stories**: Story 3
- **Details**: Yellow (validator) agent for ose-platform-web content validation

**REQ-016**: All agents follow AI Agents Convention

- **Priority**: High
- **User Stories**: Story 2, Story 3
- **Details**: Include name, description, tools, model, color fields, proper section structure

#### Documentation Updates

**REQ-017**: Update conventions index (governance/conventions/README.md)

- **Priority**: High
- **User Stories**: Story 1, Story 4
- **Details**: Add Hugo content convention and Content Quality Principles convention to index with proper descriptions

**REQ-018**: Update agents index (.claude/agents/README.md)

- **Priority**: High
- **User Stories**: Story 2, Story 3
- **Details**: Add 4 new agents with descriptions and workflow guidance

**REQ-019**: Update CLAUDE.md

- **Priority**: High
- **User Stories**: Story 1, Story 2, Story 3, Story 4
- **Details**: Add section on Hugo content conventions, Content Quality Principles, and reference to agents

### Non-Functional Requirements

**REQ-NFR-001**: Convention document readability

- Must be easy to scan and reference during content creation
- Clear section headings and examples
- Proper use of Mermaid diagrams for visual clarity

**REQ-NFR-002**: Agent response quality

- Agents must produce content that passes Hugo build validation
- Agents must reference convention accurately
- Agents must handle site-specific differences correctly

**REQ-NFR-003**: Documentation maintainability

- Convention should be easy to update as Hugo sites evolve
- Clear separation between inherited, adapted, and Hugo-specific conventions
- Site-specific differences clearly documented

### Constraints

- Must work with existing Hugo themes (Hextra for ayokoding-web, PaperMod for ose-platform-web)
- Must integrate with existing repository conventions
- Must not contradict existing docs/ conventions
- Agents must follow AI Agents Convention color categorization

### Out of Scope

- Creating new Hugo themes or modifying existing themes
- Changing existing content in Hugo sites (convention applies to new content only)
- Creating additional archetypes beyond what already exists
- Modifying Hugo configuration files (hugo.yaml)
- Creating deployment automation for Hugo sites

---

## Technical Documentation

### Architecture Overview

This project creates a documentation and agent layer on top of two existing Hugo sites. The convention serves as the single source of truth for content creation, referenced by both human creators and AI agents.

#### 1. Hugo Content Convention Structure

**Inherited Conventions (7 items)**:

- Mathematical Notation
- Color Accessibility
- Diagrams
- Emoji Usage
- Timestamp Format
- Tutorial Convention (applies to ayokoding-web learning content)
- Tutorial Naming (applies to ayokoding-web learning content)

**Adapted Conventions (4 items)**:

- Indentation (spaces in frontmatter and content)
- Linking (Hugo ref/relref shortcodes, no .md extension)
- File Naming (Hugo-specific patterns: date-based or simple slugs)
- Frontmatter (YAML format, Hugo-specific fields)

**Hugo-Specific Conventions (4 items)**:

- Archetypes (content templates)
- Shortcodes (reusable content snippets)
- Taxonomy (tags, categories, series)
- Asset Organization (static/ directory structure)

**Theme-Specific Differences (2 themes)**:

- Hextra Theme (ayokoding-web): Documentation-style, multilingual, FlexSearch, Tailwind CSS
- PaperMod Theme (ose-platform-web): Clean blog, simple frontmatter, share buttons, analytics

**Site-Specific Differences (2 sites)**:

- ayokoding-web: Bilingual (Indonesian/English), learning content, 5 archetypes, deep hierarchy
- ose-platform-web: English-only, updates/about, 1 archetype, flat structure

#### 2. Content Quality Principles (Universal Standards)

Applies to ALL markdown content (docs/, Hugo sites, plans/, repository files):

- Writing Style (active voice, professional tone, clarity)
- Heading Hierarchy (single H1, proper nesting)
- Accessibility (alt text, semantic HTML, ARIA labels, color contrast)
- Formatting (code blocks, lists, emphasis, blockquotes, tables)

#### 3. AI Agents (4 total)

**Content Makers (Green - Creators)**:

- ayokoding-web-general-maker: Creates content for ayokoding-web (Hextra theme)
- ose-platform-web-content-maker: Creates content for ose-platform-web (PaperMod theme)

**Content Checkers (Yellow - Validators)**:

- ayokoding-web-general-checker: Validates ayokoding-web content
- ose-platform-web-content-checker: Validates ose-platform-web content

### Technology Stack

- **Hugo**: Static site generator (ayokoding-web uses Hextra theme, ose-platform-web uses PaperMod theme v7.0)
- **Markdown**: Content format with YAML frontmatter
- **Mermaid**: Diagram generation (with accessible color palette)
- **AI Agents**: Claude Code agents for content creation and validation
- **Theme Compatibility**: Convention targets PaperMod v7.0+ and is compatible with v8.0

### Design Decisions

#### Decision 1: Single Convention for Both Sites

- **Context**: We have two Hugo sites with different themes and purposes
- **Decision**: Create one Hugo content convention that covers both sites with a "Site-Specific Differences" section
- **Rationale**:
  - Most conventions apply to both sites (timestamps, diagrams, linking format)
  - Easier to maintain one convention than two separate docs
  - Site-specific section makes differences explicit
  - Reduces duplication and keeps knowledge centralized
- **Consequences**:
  - Convention document needs clear organization to avoid confusion
  - Site-specific sections must be prominent and easy to find
  - Agents must correctly identify which site they're working with

#### Decision 2: Separate Maker and Checker Agents Per Site

- **Context**: Need agents for both content creation and validation for two sites
- **Decision**: Create 4 agents total - 2 makers (green) and 2 checkers (yellow), one pair per site
- **Rationale**:
  - Different sites have different archetypes, frontmatter patterns, and content structures
  - Site-specific agents can be more focused and accurate
  - Follows AI Agents Convention color categorization (green for creators, yellow for validators)
  - Allows for site-specific validation rules
- **Consequences**:
  - More agents to maintain (4 instead of 2)
  - Each agent must have clear scope boundaries
  - Agents can be more specialized and accurate for their target site

#### Decision 3: Frontmatter Indentation Exception

- **Context**: docs/ directory uses TABs for indentation, but YAML frontmatter requires spaces
- **Decision**: Document that YAML frontmatter in Hugo content MUST use 2 spaces (same as docs/ convention)
- **Rationale**:
  - YAML specification and parsers expect spaces
  - Obsidian frontmatter parser requires spaces
  - Hugo frontmatter parser expects spaces
  - This is already established in docs/ convention
- **Consequences**:
  - Clear exception documented in Hugo convention
  - Agents must handle this correctly (spaces in frontmatter, standard markdown in content)

#### Decision 4: Hugo-Specific Linking Format

- **Context**: docs/ uses `.md` extensions in links, Hugo uses ref/relref or no extension
- **Decision**: Document Hugo linking convention as different from docs/ linking
- **Rationale**:
  - Hugo generates URLs without .md extensions
  - Using .md in Hugo links breaks in production builds
  - Hugo's ref/relref provides better link validation
- **Consequences**:
  - Adaptation section must clearly explain the difference
  - Agents must use correct linking format for Hugo content
  - Content creators need to understand the difference between docs/ and Hugo linking

### Research Findings

#### Hextra Theme (ayokoding-web)

**Official Documentation**: [Hextra Docs](https://imfing.github.io/hextra/docs/)
**GitHub**: [imfing/hextra](https://github.com/imfing/hextra)
**Last Updated**: 2025-09-06 | **GitHub Stars**: 1789

**Description**: Modern, fast, batteries-included Hugo theme built with Tailwind CSS for documentation, blogs, and static websites.

**Key Features** (validated from official docs):

- **Search**: Offline full-text search via FlexSearch (no additional configuration)
- **Design**: Tailwind CSS with responsive layout and dark mode support
- **Performance**: Lightning-fast static-site generator, no JavaScript/Node.js required
- **Content**: Markdown, syntax highlighting, LaTeX math formulae, diagrams, shortcodes
- **Multilingual**: Built-in multi-language support (English, Farsi, Japanese, Simplified Chinese)
- **Navigation**: Auto-generated table of contents, breadcrumbs, pagination, sidebar
- **SEO**: Out-of-the-box SEO tags, Open Graph, Twitter Cards

**Frontmatter Fields** (Hextra-specific):

- `title`: Page title (required)
- `sidebar.exclude`: Boolean to exclude from left sidebar
- `toc`: Boolean to disable table of contents
- `editURL`: Custom edit link for specific page
- `excludeSearch`: Boolean to exclude from FlexSearch index
- `noindex`: Boolean to block Google Search indexing
- `params.images`: Array of image URLs for Open Graph
- `params.audio`: Audio file path for Open Graph
- `params.videos`: Array of video URLs for Open Graph

**Content Structure**:

- Main sidebar auto-generates from content directory structure
- Blog posts organized by year/month: `content/posts/2025/02/post-name.md`
- Navigation via `prev`/`next` frontmatter fields (note: reversed - `prev` points to newer post)

**Configuration Options** (hugo.yaml):

- Navigation menu (`menu.main`): Links, search, icons, theme toggle, language switcher
- Sidebar menu (`menu.sidebar`): Extra links beyond auto-generated content
- Logo & navbar customization
- Theme settings (default: light/dark/system, displayToggle)
- Search configuration (FlexSearch indexing and tokenization)
- Analytics (Google Analytics, Umami, Matomo)
- Page width, blog tags display, date format, external link decoration

**Available Shortcodes**:

- Callout
- Cards
- Details
- FileTree
- Icon
- Steps
- Tabs

#### PaperMod Theme (ose-platform-web)

**Official Documentation**: [PaperMod Site](https://adityatelange.github.io/hugo-PaperMod/)
**GitHub**: [adityatelange/hugo-PaperMod](https://github.com/adityatelange/hugo-PaperMod)
**GitHub Wiki**: [PaperMod Wiki](https://github.com/adityatelange/hugo-PaperMod/wiki)
**Last Updated**: 2025-10-26 | **GitHub Stars**: 12,755

**Description**: Fast, clean, responsive Hugo theme based on hugo-paper, focused on simplicity with useful features.

**Key Features** (validated from official docs):

- **Design**: Clean, simple, fast, and responsive
- **Theme Support**: Light/dark mode with localStorage preference
- **Navigation**: Smooth scrolling, breadcrumbs, archive, search
- **Social**: Share buttons for multiple platforms
- **SEO**: Built-in SEO optimization
- **Accessibility**: Reduced-motion support, semantic HTML
- **Analytics**: Google Analytics, Bing, Yandex site verification
- **Multilingual**: Multi-language support (French, Farsi noted)

**Site Variables** (Params in hugo.yaml):

- `env`: Environment (e.g., 'production')
- `title`, `description`, `author`, `images`, `keywords`
- `DateFormat`: Date display format
- `ShowReadingTime`, `ShowShareButtons`, `ShowCodeCopyButtons`
- `ShowPostNavLinks`, `ShowBreadCrumbs`, `ShowWordCount`
- `defaultTheme`: light/dark/auto
- `disableThemeToggle`, `disableScrollToTop`, `disableAnchoredHeadings`
- `hideMeta`, `hideSummary`, `showtoc`, `tocopen`, `hideFooter`
- `ShareButtons`: Platform customization
- `label`: Header branding (text, icon, size)
- `profileMode`: Home page profile layout
- `assets`: Favicon, Highlight.js, fingerprinting
- `cover`: Image display settings
- `schema`: Publisher type and sameAs URLs
- `fuseOpts`: Search configuration
- `socialIcons`: Social media links
- `editPost`: Content editing links
- `analytics`: Google, Bing, Yandex verification tags

**Page Variables** (Frontmatter):

- `title`: Page title (required)
- `date`: Publication date (required)
- `draft`: Boolean draft status
- `description`: Subtitle/description
- `showtoc`: Enable table of contents
- `tocopen`: Open ToC by default
- `hidemeta`: Hide post metadata
- `comments`: Comment visibility
- `searchHidden`: Exclude from search
- `hideSummary`: Hide in lists
- `hideFooter`: Hide footer
- `author`: Single/multiple authors
- `weight`: Page order/pin position
- `canonicalURL`: Canonical link
- `robotsNoIndex`: Exclude from indexing
- `cover.image`, `cover.caption`, `cover.alt`: Cover image settings
- `cover.relative`: Use relative paths
- `cover.responsiveImages`: Generate responsive variants
- `cover.hidden`: Hide on current page
- `ShareButtons`: Per-page share platform customization

**Content Organization**:

- Simple flat structure for updates and about pages
- Standard Hugo content organization
- Clean URL structure

#### ayokoding-web Site Structure (Using Hextra)

- **Multilingual**: Indonesian (id) and English (en) with language subfolders
- **Content Types**:
  - Educational content (learn/ and belajar/)
  - Personal essays (rants/ and celoteh/)
  - Video content (video-content/ and konten-video/)
  - About pages
- **Archetypes**: learn.md, celoteh.md, konten-video.md, \_index.md, default.md
- **Frontmatter Pattern** (learn.md):
  ```yaml
  title: "Title"
  date: YYYY-MM-DDTHH:MM:SS+07:00
  draft: true
  description: ""
  weight: 10
  tags: []
  categories: ["learn"]
  author: "Author Name"
  ```
- **Frontmatter Pattern** (celoteh.md):
  ```yaml
  title: "Title"
  date: YYYY-MM-DDTHH:MM:SS+07:00
  draft: true
  description: ""
  tags: []
  categories: ["celoteh"]
  author: "Author Name"
  ```
- **Asset Organization**: static/images/ with subdirectories
- **Taxonomy**: tags, categories
- **SEO Fields**: description, images, keywords (optional)

#### ose-platform-web Structure

- **Theme**: PaperMod (clean blog/landing page theme)
- **Monolingual**: English only
- **Content Types**:
  - Updates (blog posts about project progress)
  - About page
- **Archetypes**: default.md only (uses TOML format)
- **Frontmatter Pattern** (updates):
  ```yaml
  title: "Title"
  date: YYYY-MM-DDTHH:MM:SS+07:00
  draft: false
  tags: ["tag1", "tag2"]
  categories: ["updates"]
  summary: "Brief description"
  ```
- **Frontmatter Pattern** (about):
  ```yaml
  title: "Title"
  url: "/about/"
  summary: "Description"
  ```
- **Asset Organization**: static/images/, static/casts/ (for asciinema)
- **Taxonomy**: categories, tags, series
- **SEO Fields**: summary, keywords (optional)

#### Key Differences Between Sites

| Aspect               | ayokoding-web                                            | ose-platform-web                           |
| -------------------- | -------------------------------------------------------- | ------------------------------------------ |
| **Theme**            | Hextra                                                   | PaperMod                                   |
| Theme Style          | Documentation/educational (Tailwind CSS)                 | Clean blog/landing page                    |
| Theme Stars          | 1,789 GitHub stars                                       | 12,755 GitHub stars                        |
| Theme Update         | 2025-09-06                                               | 2025-10-26                                 |
| **Features**         | FlexSearch, multilingual, LaTeX math, shortcodes         | Share buttons, analytics, simple SEO       |
| **Languages**        | Bilingual (Indonesian/English)                           | English only                               |
| **Archetypes**       | 5 types (learn, celoteh, konten-video, \_index, default) | 1 type (default)                           |
| Archetype Format     | YAML                                                     | TOML (default)                             |
| **Content Types**    | Learning, essays, video content                          | Updates, about page                        |
| Content Structure    | Deep hierarchy (learn/archived/crash-courses/...)        | Flat (updates/, about.md)                  |
| **Primary Purpose**  | Educational platform for developers                      | Project landing & progress updates         |
| **Target Audience**  | Indonesian developers (bilingual)                        | Enterprise users (international)           |
| **Frontmatter**      | More fields (weight, categories, author, sidebar)        | Simpler (tags, categories, summary, cover) |
| **Navigation**       | Auto-sidebar, prev/next, breadcrumbs                     | Breadcrumbs, archive, smooth scrolling     |
| **Search**           | FlexSearch (offline)                                     | Fuse.js search                             |
| **Tutorial Content** | Yes (id/belajar/, en/learn/)                             | No (not applicable)                        |

### Implementation Approach

1. **Research Phase** (Completed)
   - Analyze both Hugo sites (configuration, content, archetypes)
   - Document frontmatter patterns
   - Identify commonalities and differences
   - Review existing conventions that apply

2. **Convention Creation**
   - Create governance/conventions/ex-co\_\_hugo-content.md
   - Structure: Inherited Conventions, Adapted Conventions, Hugo-Specific Conventions, Site-Specific Differences
   - Include Mermaid diagrams for visual clarity
   - Add examples for each pattern
   - Use accessible color palette in diagrams

3. **Agent Creation**
   - Create apps-ayokoding-web-general-maker.md (green)
   - Create apps-ayokoding-web-general-checker.md (yellow)
   - Create ose-platform-web-content-maker.md (green)
   - Create ose-platform-web-content-checker.md (yellow)
   - Each agent references Hugo content convention
   - Follow AI Agents Convention structure

4. **Index Updates**
   - Update governance/conventions/README.md
   - Update .claude/agents/README.md
   - Update CLAUDE.md with Hugo content section

5. **Validation**
   - Review convention for completeness
   - Verify agents follow AI Agents Convention
   - Check all links and references
   - Ensure no contradictions with existing conventions

### Testing Strategy

- **Convention Validation**: Manual review against existing Hugo content to ensure accuracy
- **Agent Testing**: Test agents by creating sample content and validating it
- **Link Validation**: Verify all internal links in convention document work correctly
- **Cross-Reference Testing**: Ensure agents correctly reference convention sections
- **Site-Specific Testing**: Verify agents handle site-specific differences correctly

---

## Delivery Plan

### Implementation Steps

- [x] Step 1: Create Hugo content convention document structure
  - **Implementation Notes**: Created ex-co\_\_hugo-content.md with complete frontmatter (title, description, category, tags, dates). Organized into 5 main sections: Overview, Inherited Conventions (7), Adapted Conventions (5), Hugo-Specific Conventions (6), Theme-Specific Differences (2), Site-Specific Differences (2). Added comprehensive Mermaid diagram showing convention hierarchy with accessible color palette.
  - **Date**: 2025-12-07
  - **Status**: Completed
  - **Files Changed**: governance/conventions/ex-co\_\_hugo-content.md (new)

- [x] Step 2: Document Inherited Conventions section (7 total)
  - **Implementation Notes**: Documented all 7 inherited conventions with links: Mathematical Notation (LaTeX in learning content), Color Accessibility (Mermaid diagrams), Diagrams (Mermaid preferred), Emoji Usage (semantic use), Timestamp Format (ISO 8601 UTC+7), Tutorial Convention (ayokoding-web learning), Tutorial Naming (ayokoding-web types). Each includes application to Hugo with specific examples. Emphasized Tutorial conventions apply to ayokoding-web only.
  - **Date**: 2025-12-07
  - **Status**: Completed

- [x] Step 3: Document Adapted Conventions section
  - **Implementation Notes**: Documented 5 adapted conventions: (1) Indentation - YAML frontmatter uses 2 spaces (NOT tabs), content uses standard markdown; (2) Linking - Hugo ref/relref or paths WITHOUT .md extension (different from docs/); (3) File Naming - simple slugs or date-prefixed, no prefix encoding; (4) Frontmatter - YAML format with required fields (title, date, draft); (5) Date Format - ISO 8601 with UTC+7 (YYYY-MM-DDTHH:MM:SS+07:00). Included examples showing correct vs incorrect usage.
  - **Date**: 2025-12-07
  - **Status**: Completed

- [x] Step 4: Document Hugo-Specific Conventions section
  - **Implementation Notes**: Documented 6 Hugo-specific conventions: (1) Archetypes - content templates with site-specific examples (ayokoding: 5 types, ose-platform: 1 type); (2) Shortcodes - Hugo built-ins, Hextra shortcodes (callout, cards, steps, tabs), PaperMod reliance on built-ins; (3) Taxonomy - tags, categories, series with site-specific usage; (4) Asset Organization - static/ directory structure with recommended subdirectories; (5) Content Types - \_index.md vs regular files; (6) URL Structure - slug generation and custom URLs. Included code examples for each.
  - **Date**: 2025-12-07
  - **Status**: Completed

- [x] Step 5: Document Theme-Specific and Site-Specific Differences sections
  - **Implementation Notes**: Created comprehensive theme comparison (Hextra vs PaperMod) with validated research from official docs: Hextra (1,789 stars, updated 2025-09-06, FlexSearch, Tailwind, multilingual, rich shortcodes) vs PaperMod (12,755 stars, updated 2025-10-26, v7.0+ compatibility, share buttons, simple SEO). Documented ayokoding-web (bilingual, 5 archetypes, deep hierarchy, learning content) vs ose-platform-web (English-only, 1 archetype, flat structure, updates/about). Added comparison tables and content structure diagrams. Included theme version compatibility note (PaperMod v7.0+, compatible with v8.0).
  - **Date**: 2025-12-07
  - **Status**: Completed

- [x] Step 6: Create ayokoding-web-general-maker agent
  - **Implementation Notes**: Created apps-ayokoding-web-general-maker.md with complete frontmatter (green color for creator). Documented: Core Responsibility (create Hugo content for ayokoding-web), When to Use (learning, essays, video content), Site Characteristics (Hextra theme, bilingual, 5 archetypes), Hugo convention compliance (7 inherited, 5 adapted, 4 Hugo-specific), Content Quality compliance, Creation Workflow (6 steps), and 2 complete examples (beginner tutorial, personal essay). References Hugo Content Convention and Content Quality Principles. Includes extensive Hextra shortcode usage examples.
  - **Date**: 2025-12-07
  - **Status**: Completed
  - **Files Changed**: .claude/agents/apps-ayokoding-web-general-maker.md (new)

- [x] Step 7: Create ayokoding-web-general-checker agent
  - **Implementation Notes**: Created apps-ayokoding-web-general-checker.md with complete frontmatter (yellow color for validator). Documented: Core Responsibility (validate ayokoding-web content), Validation Checklist (frontmatter, structure, links, images, Mermaid, code blocks, Hextra shortcodes, taxonomy, content quality, tutorial-specific), Validation Process (6 steps), Report format with status indicators (✅ Pass, ⚠️ Warning, ❌ Fail), and 2 validation scenarios (valid content, content with errors). Emphasizes read-only validation with actionable feedback. References Hugo and Content Quality conventions.
  - **Date**: 2025-12-07
  - **Status**: Completed
  - **Files Changed**: .claude/agents/apps-ayokoding-web-general-checker.md (new)

- [x] Step 8: Create ose-platform-web-content-maker agent
  - **Implementation Notes**: Created ose-platform-web-content-maker.md with complete frontmatter (green color for creator). Documented: Core Responsibility (create ose-platform-web content), When to Use (updates, about page, announcements), Site Characteristics (PaperMod v7.0+ theme, English-only, 1 archetype, flat structure), Hugo convention compliance (5 inherited - no tutorials, 5 adapted, 3 Hugo-specific), Content Quality compliance, Creation Workflow (6 steps), and 2 complete examples (feature release update, project milestone). References Hugo and Content Quality conventions. Includes PaperMod-specific frontmatter and shortcode patterns.
  - **Date**: 2025-12-07
  - **Status**: Completed
  - **Files Changed**: .claude/agents/ose-platform-web-content-maker.md (new)

- [x] Step 9: Create ose-platform-web-content-checker agent
  - **Implementation Notes**: Created ose-platform-web-content-checker.md with complete frontmatter (yellow color for validator). Documented: Core Responsibility (validate ose-platform-web content), Validation Checklist (frontmatter with PaperMod fields, cover image validation, structure, links, images, Mermaid, code blocks, taxonomy, content quality, English language), Validation Process (6 steps), Report format with detailed error/warning examples, and 2 validation scenarios (valid update, update with errors). Emphasizes PaperMod-specific validation (cover image alt text, summary field). References Hugo and Content Quality conventions.
  - **Date**: 2025-12-07
  - **Status**: Completed
  - **Files Changed**: .claude/agents/ose-platform-web-content-checker.md (new)

- [x] Step 10: Update conventions index
  - **Implementation Notes**: Updated governance/conventions/README.md with both new conventions in alphabetical order: (1) Content Quality Principles - added after Color Accessibility, described as "Universal markdown content quality standards applicable to ALL repository markdown contexts (docs/, Hugo sites, plans/, root files). Covers writing style, heading hierarchy, accessibility, formatting"; (2) Hugo Content Convention - added after Emoji Usage, described as "Comprehensive Hugo content standards for ayokoding-web (Hextra theme) and ose-platform-web (PaperMod theme). Covers 7 inherited, 5 adapted, 6 Hugo-specific conventions, and theme/site-specific differences". Updated "Last Updated" to 2025-12-07.
  - **Date**: 2025-12-07
  - **Status**: Completed
  - **Files Changed**: governance/conventions/README.md (modified)

- [x] Step 11: Update agents index
  - **Implementation Notes**: Updated .claude/agents/README.md with 4 new Hugo content agents inserted after deployer agents, before docs-checker. Added: (1) ayokoding-web-general-maker (🟦 green) with description, specialization (Hextra, bilingual, 5 archetypes, Tutorial Convention), tools, when to use, works with ayokoding-web-general-checker; (2) ayokoding-web-general-checker (🟨 yellow) with validation specialization, tools, when to use; (3) ose-platform-web-content-maker (🟦 green) with PaperMod v7.0+, English-only, enterprise tone; (4) ose-platform-web-content-checker (🟨 yellow) with PaperMod validation, cover image checking. All include references to Hugo Content Convention and Content Quality Principles.
  - **Date**: 2025-12-07
  - **Status**: Completed
  - **Files Changed**: .claude/agents/README.md (modified)

- [x] Step 12: Update CLAUDE.md
  - **Implementation Notes**: Added two new sections to Documentation Standards: (1) "Hugo Content Convention" section with overview of 2 sites (ayokoding-web, ose-platform-web), 7 inherited conventions, 5 adapted, 6 Hugo-specific, site comparison table, and list of 4 specialized agents; (2) "Content Quality Principles" section emphasizing universal application to ALL markdown, with 4 key principles (Writing Style, Heading Hierarchy, Accessibility, Formatting) and quality checklist. Added both conventions to Key Resources list (alphabetically). Added 4 agents to Available Agents list. All updates maintain existing CLAUDE.md structure and formatting.
  - **Date**: 2025-12-07
  - **Status**: Completed
  - **Files Changed**: CLAUDE.md (modified)

- [x] Step 13: Create Content Quality Principles convention document structure
  - **Implementation Notes**: Created ex-co\_\_content-quality.md with complete frontmatter (title, description, category, tags, dates). Organized into 6 main sections: Scope (applies to ALL markdown), Writing Style & Tone, Heading Hierarchy, Accessibility Standards, Formatting Conventions, Quality Checklist. Added comprehensive Mermaid diagram showing quality principles hierarchy with accessible color palette. Emphasized universal applicability to docs/, Hugo sites, plans/, and root files.
  - **Date**: 2025-12-07
  - **Status**: Completed
  - **Files Changed**: governance/conventions/ex-co\_\_content-quality.md (new)

- [x] Step 14: Document Writing Style and Tone section
  - **Implementation Notes**: Documented 4 key principles: (1) Active Voice - prefer active over passive with examples showing good/bad/acceptable usage; (2) Professional Tone - approachable yet professional with examples of too-casual vs too-formal; (3) Clarity & Conciseness - one idea per sentence, short paragraphs, remove filler words, concrete examples; (4) Audience Awareness - writing for beginners vs intermediate/advanced with examples. Included extensive good/bad examples throughout.
  - **Date**: 2025-12-07
  - **Status**: Completed

- [x] Step 15: Document Heading Hierarchy section
  - **Implementation Notes**: Documented 4 critical rules: (1) Single H1 Rule - exactly ONE H1 per document (the title); (2) Proper Heading Nesting - semantic hierarchy H1→H2→H3→H4 without skipping levels; (3) Descriptive Headings - specific, not vague; (4) Semantic Structure - headings for structure not styling. Included correct vs incorrect examples for each rule with explanations of why proper hierarchy matters for screen readers and SEO.
  - **Date**: 2025-12-07
  - **Status**: Completed

- [x] Step 16: Document Accessibility Standards section
  - **Implementation Notes**: Documented 5 accessibility requirements: (1) Alt Text for Images - ALL images must have descriptive alt text with guidelines (describe content, explain purpose, keep concise); (2) Semantic HTML - use semantic elements appropriately; (3) ARIA Labels - when to use ARIA (complex components, custom widgets, additional context); (4) Color Contrast - reference Color Accessibility convention, WCAG AA requirements (4.5:1 normal text, 3:1 large text), use accessible palette; (5) Screen Reader Considerations - logical reading order, descriptive links, table headers, list structure, heading hierarchy. Included extensive examples.
  - **Date**: 2025-12-07
  - **Status**: Completed

- [x] Step 17: Document Formatting Conventions section
  - **Implementation Notes**: Documented 6 formatting areas: (1) Code Block Formatting - language-specific indentation (JS/TS: 2 spaces, Python: 4 spaces, YAML: 2 spaces), always specify language; (2) Text Formatting - purposeful use of bold (key terms, UI elements), italic (emphasis, foreign terms), inline code (variables, paths, commands), strikethrough (deprecated features); (3) List Formatting - unordered vs ordered, proper nesting, checklist format; (4) Blockquotes & Callouts - quotations and callout types (Note, Warning, Success, Tip, Important); (5) Table Formatting - basic tables, alignment syntax, guidelines; (6) Line Length & Paragraph Structure - 80-100 chars for prose, 3-5 sentences per paragraph, topic sentence first. Included extensive examples showing good vs bad usage.
  - **Date**: 2025-12-07
  - **Status**: Completed

- [x] Step 18: Update conventions index with Content Quality Principles
  - **Implementation Notes**: Combined with Step 10. Updated governance/conventions/README.md with Content Quality Principles convention in alphabetical order (after Color Accessibility, before Diagram). Description emphasizes universal application: "Universal markdown content quality standards applicable to ALL repository markdown contexts (docs/, Hugo sites, plans/, root files). Covers writing style and tone (active voice, professional, concise), heading hierarchy (single H1, proper nesting), accessibility (alt text, semantic HTML, color contrast, screen readers), and formatting (code blocks, text formatting, lists, blockquotes, tables, line length, paragraphs)".
  - **Date**: 2025-12-07
  - **Status**: Completed (combined with Step 10)
  - **Files Changed**: governance/conventions/README.md (modified)

- [x] Step 19: Update CLAUDE.md with Content Quality Principles
  - **Implementation Notes**: Combined with Step 12. Added "Content Quality Principles" section to CLAUDE.md emphasizing universal application to ALL markdown content. Section includes scope (docs/, Hugo, plans/, root files), 4 key principles (Writing Style & Tone, Heading Hierarchy, Accessibility, Formatting), and quality checklist. Added to Key Resources list with description "Universal markdown content quality standards for ALL repository markdown (docs/, Hugo sites, plans/, root files)". Maintains consistency with other convention descriptions.
  - **Date**: 2025-12-07
  - **Status**: Completed (combined with Step 12)
  - **Files Changed**: CLAUDE.md (modified)

### Validation Checklist

- [x] All files created in correct locations (2 conventions + 4 agents)
- [x] Hugo content convention covers all required topics (7 inherited, adapted, Hugo-specific, theme-specific, site-specific)
- [x] Hugo content convention includes theme research validation (Hextra, PaperMod)
- [x] Hugo content convention includes proper Mermaid diagrams with accessible colors
- [x] Hugo content convention has no broken internal links
- [x] Content Quality Principles convention covers all required topics (writing style, heading hierarchy, accessibility, formatting)
- [x] Content Quality Principles convention applies universally to all markdown contexts
- [x] Content Quality Principles convention has clear examples
- [x] All 4 agents follow AI Agents Convention structure
- [x] All 4 agents have correct frontmatter (name, description, tools, model, color)
- [x] All 4 agents reference Hugo content convention correctly
- [x] Conventions index updated with both new conventions and properly formatted
- [x] Agents index updated with all 4 new agents
- [x] CLAUDE.md updated with Hugo content section and Content Quality Principles
- [x] No linting errors or warnings
- [x] All Mermaid diagrams use accessible color palette
- [x] All links use correct format (relative paths with .md extension)
- [x] All timestamps use UTC+7 format
- [x] Frontmatter in all files uses 2-space indentation
- [x] Web research findings validated and cited in plan

### Acceptance Criteria

- [x] Hugo content convention document exists at governance/conventions/ex-co\_\_hugo-content.md
- [x] Hugo convention documents 7 inherited conventions (not 5)
- [x] Hugo convention clearly separates inherited, adapted, Hugo-specific, theme-specific, and site-specific sections
- [x] Hugo convention includes validated theme research (Hextra, PaperMod)
- [x] Hugo convention includes Mermaid diagrams for content structure
- [x] Hugo convention includes examples for all patterns
- [x] Content Quality Principles convention document exists at governance/conventions/ex-co\_\_content-quality.md
- [x] Content Quality Principles convention applies to ALL markdown content (docs/, Hugo, plans/, root files)
- [x] Content Quality Principles convention covers writing style, heading hierarchy, accessibility, and formatting
- [x] All 4 agents exist in .claude/agents/ directory
- [x] All agents have correct color categorization (green for makers, yellow for checkers)
- [x] All agents reference Hugo content convention
- [x] Conventions index includes both new conventions
- [x] Agents index includes all 4 new agents
- [x] CLAUDE.md includes Hugo content guidance and Content Quality Principles
- [x] All acceptance criteria from user stories pass (Stories 1-4)
- [x] No conflicts with existing conventions
- [x] Documentation is clear, scannable, and practical

### Web Research Sources

All theme information validated from official sources:

**Hextra Theme**:

- [Hextra Official Documentation](https://imfing.github.io/hextra/docs/)
- [Hextra Getting Started](https://imfing.github.io/hextra/docs/getting-started/)
- [Hextra Configuration Guide](https://imfing.github.io/hextra/docs/guide/configuration/)
- [Hextra GitHub Repository](https://github.com/imfing/hextra)
- [Structure Hugo Blog Posts with Hextra Theme](https://www.juliusunscripted.com/posts/structure-hugo-blog-posts-with-hextra-theme/)

**PaperMod Theme**:

- [PaperMod Official Demo Site](https://adityatelange.github.io/hugo-PaperMod/)
- [PaperMod Variables Reference](https://adityatelange.github.io/hugo-PaperMod/posts/papermod/papermod-variables/)
- [PaperMod GitHub Wiki](https://github.com/adityatelange/hugo-PaperMod/wiki)
- [PaperMod GitHub Repository](https://github.com/adityatelange/hugo-PaperMod)
- [PaperMod Features Documentation](https://github.com/adityatelange/hugo-PaperMod/wiki/Features)

### Completion Status

**Overall Status**: ✅ COMPLETE - Validated and Ready for Archival

**Last Updated**: 2025-12-07

**Completion Date**: 2025-12-07

**Final Validation**: ✅ PASS (plan-execution-checker)

- All 19 requirements met (100%)
- All 20 validation checklist items passed
- All 18 acceptance criteria satisfied
- 0 critical issues, 0 warnings

**Plan Updates**:

- Added 7 inherited conventions (was 5)
- Added Content Quality Principles convention as new deliverable
- Added validated theme research (Hextra and PaperMod)
- Added theme-specific differences section
- Added User Story 4 for Content Quality Principles
- Added requirements REQ-007 through REQ-011 for Content Quality Principles
- Renumbered agent requirements to REQ-012 through REQ-016
- Updated documentation requirements to REQ-017 through REQ-019
- Added implementation steps 13-19 for Content Quality Principles
- Updated Mermaid diagram with 7 inherited conventions and Content Quality Principles
- Enhanced site comparison table with theme details

**Implementation Summary**:

- **Total Implementation Steps Completed**: 19/19
- **Files Created**:
  - 2 convention documents (ex-co**hugo-content.md, ex-co**content-quality.md)
  - 4 AI agent files (apps-ayokoding-web-general-maker.md, apps-ayokoding-web-general-checker.md, ose-platform-web-content-maker.md, ose-platform-web-content-checker.md)
- **Files Updated**:
  - governance/conventions/README.md (added 2 conventions)
  - .claude/agents/README.md (added 4 agents)
  - CLAUDE.md (added Hugo Content and Content Quality sections, updated resources and agents lists)
- **Self-Validation Status**: All deliverables created, all indices updated, all conventions followed

**Next Steps**:

- Final validation by plan-execution-checker agent
- Address any issues found during final validation
- Mark plan as complete after validation passes
