# OSE Platform Web

Official website for the Open Sharia Enterprise platform - a Sharia-compliant enterprise solutions platform.

Landing page with weekly/monthly project updates.

## 🌐 Website

- **Production**: https://oseplatform.com (under construction)
- **Repository**: Part of the [open-sharia-enterprise](https://github.com/wahidyankf/open-sharia-enterprise) monorepo

## 🛠️ Tech Stack

- **Hugo**: v0.152.2 (Extended)
- **Go**: 1.25
- **Theme**: [PaperMod](https://github.com/adityatelange/hugo-PaperMod) - Fast, clean, responsive theme for landing pages and blogs
- **Build System**: Nx monorepo

## 🚀 Development

### Prerequisites

- Hugo Extended 0.152.2 or later
- Go 1.25 or later
- Node.js (for Nx commands)

### Local Development

Start the development server:

```bash
# From repository root
nx dev ose-platform-web

# Or from this directory
hugo server --buildDrafts --buildFuture
```

The site will be available at `http://localhost:1313`

### Building

Build the site for production:

```bash
# From repository root
nx build ose-platform-web

# Or from this directory
./build.sh
```

The built site will be in the `public/` directory.

### Cleaning

Remove generated files:

```bash
# From repository root
nx clean ose-platform-web

# Or from this directory
rm -rf public resources
```

## 📂 Project Structure

```
ose-platform-web/
├── archetypes/          # Content templates
├── assets/              # Source assets (SCSS, JS, images)
├── content/             # Markdown content
├── data/                # Data files (YAML, JSON, TOML)
├── layouts/             # HTML templates
├── static/              # Static files (images, fonts, etc.)
├── hugo.yaml            # Hugo configuration
├── go.mod               # Go module definition
├── go.sum               # Go module checksums
├── project.json         # Nx project configuration
├── build.sh             # Production build script
└── README.md            # This file
```

## 🎨 Theme

This site uses the [PaperMod](https://github.com/adityatelange/hugo-PaperMod) theme - a fast, clean, and responsive Hugo theme perfect for landing pages and blogs.

### Theme Features

- ✨ Clean and minimalist design
- 🌙 Dark mode support (auto/light/dark)
- 📱 Fully responsive and mobile-friendly
- ⚡ Extremely fast page loads
- 🎨 Syntax highlighting with code copy
- 📊 Reading time and word count
- 🔗 Social icons and sharing
- 📡 RSS feed support

## 📝 Content Management

Content is written in Markdown and organized in the `content/` directory.

### Site Structure

- **Landing Page** - Homepage with project overview (configured in `hugo.yaml` homeInfoParams)
- **About** - Project mission and details (`content/about.md`)
- **Updates** - Weekly/monthly blog posts (`content/updates/`)

### Creating New Updates

```bash
# Create a new update post
hugo new content/updates/YYYY-MM-DD-post-title.md

# Edit the frontmatter:
# - title: Post title
# - date: Publication date (YYYY-MM-DDTHH:MM:SS+07:00)
# - draft: false (set to false to publish)
# - tags: ["tag1", "tag2"]
# - categories: ["updates"]
# - summary: Brief description
```

## 🚢 Deployment

This site is deployed automatically via CI/CD when changes are pushed to the designated production branch.

## 📜 License

This project is part of the Open Sharia Enterprise platform and is licensed under the MIT License.

## 🔗 Links

- [Main Repository](https://github.com/wahidyankf/open-sharia-enterprise)
- [Hugo Documentation](https://gohugo.io/documentation/)
- [PaperMod Theme Documentation](https://github.com/adityatelange/hugo-PaperMod/wiki)
