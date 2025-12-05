# ayokoding-web

Bilingual educational platform for software engineering content, focused on helping the Indonesian tech community learn and grow. Built with Hugo and the Hextra documentation theme.

## Overview

AyoKoding ("Let's Code" in Indonesian) is a platform dedicated to sharing software engineering knowledge in both Indonesian and English. It follows a "learning in public" philosophy, encouraging developers to share their learning journey with the community.

## Features

- **Bilingual Content**: Full support for Indonesian (ID) and English (EN)
- **Documentation-style Layout**: Clean, readable format using Hextra theme
- **YouTube Integration**: Connected with @AyoKoding YouTube channel
- **SEO Optimized**: Static site generation for fast loading and better SEO
- **Responsive Design**: Works across all devices
- **Dark/Light Theme**: User-preferred theme support

## Tech Stack

- **Static Site Generator**: Hugo (latest)
- **Theme**: Hextra documentation theme
- **Content Format**: Markdown with YAML frontmatter
- **Deployment**: Vercel
- **Languages**: Indonesian (primary), English

## Getting Started

### Prerequisites

- Node.js 22.20.0 (managed by Volta)
- npm 11.1.0
- Hugo extended version (latest)

### Installation

```bash
# Install Hugo (macOS)
brew install hugo

# Navigate to project
cd apps-standalone/ayokoding-web

# Install npm dependencies
npm install
```

### Development

```bash
# Start development server
npm run dev
# or
hugo server -D

# Server runs at http://localhost:1313
```

### Building

```bash
# Build for production
npm run build
# or
hugo --minify

# Output will be in public/ directory
```

## Content Structure

```
content/
├── _index.md              # Homepage
├── en/                    # English content
│   ├── _index.md
│   ├── about-ayokoding.md
│   └── learn/            # Learning materials
│       ├── _index.md
│       └── swe/          # Software engineering
└── id/                   # Indonesian content
    ├── _index.md
    ├── tentang-ayokoding.md
    └── belajar/          # Learning materials
```

### Creating Content

#### English Content

```bash
hugo new content/en/learn/topic-name.md
```

#### Indonesian Content

```bash
hugo new content/id/belajar/nama-topik.md
```

### Content Frontmatter

```yaml
---
title: "Your Article Title"
date: 2024-01-01
draft: false
description: "Brief description for SEO"
tags: ["javascript", "tutorial"]
categories: ["learn"]
weight: 10 # For ordering
---
```

## Configuration

Main configuration in `hugo.yaml`:

```yaml
baseURL: "https://ayokoding.com/"
title: "Ayo Koding"
theme: "hextra"

languages:
  en:
    languageName: "English"
    weight: 2
  id:
    languageName: "Indonesia"
    weight: 1

defaultContentLanguage: "id"
defaultContentLanguageInSubdir: false
```

## Theme Customization

### Override Theme Files

Place custom files in:

- `layouts/` - Override theme templates
- `assets/` - Custom CSS/JS
- `static/` - Static assets

### Custom Styles

```css
/* assets/custom.scss */
.your-custom-class {
  /* Custom styles */
}
```

## Deployment

Configuration in `vercel.json`:

```json
{
  "build": {
    "env": {
      "HUGO_VERSION": "0.xxx.x"
    }
  },
  "buildCommand": "hugo --minify",
  "outputDirectory": "public"
}
```

## Content Guidelines

### Writing Style

- Clear and concise explanations
- Practical examples
- Step-by-step tutorials
- Include code snippets

### Language Considerations

- Maintain consistency between ID/EN versions
- Use appropriate technical terms
- Consider cultural context

## Scripts

```bash
npm run dev      # Start development server
npm run build    # Build for production
./build.sh       # Custom build script
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Write content in Markdown
4. Test locally with Hugo server
5. Submit a pull request

## YouTube Integration

The platform is integrated with the @AyoKoding YouTube channel. Video content can be embedded using Hugo shortcodes.

## Related Documentation

- [Hugo Documentation](https://gohugo.io/documentation/)
- [Hextra Theme Documentation](https://imfing.github.io/hextra/)
- [Standalone Projects Guide](/docs/reference/standalone-projects.md)
- [Common Commands Reference](/docs/reference/commands.md)

## License

MIT License - see the [LICENSE](LICENSE) file for details.
