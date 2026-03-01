# OSE Platform Web

Official website for the **Open Sharia Enterprise** platform - an open-source Sharia-compliant enterprise solutions platform built in the open.

**Why This Matters**: Islamic finance is a multi-trillion dollar industry, but most Sharia-compliant enterprise solutions are proprietary and expensive. We're building an open-source alternative with Sharia-compliance at its core - not bolted on as an afterthought.

**What This Site Does**: Showcases the platform and shares our journey. Weekly and monthly updates keep the community informed as we build this long-term project with radical transparency.

**Why Open Source**: Transparency builds trust in Sharia-compliant systems. By building in the open, we make trustworthy enterprise technology accessible to organizations of all sizes - not just those who can afford expensive proprietary solutions.

## 🌐 Website

- **Production**: <https://oseplatform.com> (under construction)
- **Main Project**: [Open Sharia Enterprise on GitHub](https://github.com/wahidyankf/open-sharia-enterprise) - The full platform repository
- **This Site**: Part of the OSE monorepo under `apps/oseplatform-web/`

## 🛠️ Tech Stack

- **Hugo**: v0.156.0 (Extended)
- **Go**: 1.26
- **Theme**: [PaperMod](https://github.com/adityatelange/hugo-PaperMod) - Fast, clean, responsive theme for landing pages and blogs
- **Build System**: Nx monorepo

## 🚀 Development

### Prerequisites

- Hugo Extended 0.156.0 or later
- Go 1.26 or later
- Node.js (for Nx commands)

### Local Development

Start the development server:

```bash
# From repository root
nx dev oseplatform-web

# Or from this directory
hugo server --buildDrafts --buildFuture
```

The site will be available at `http://localhost:3000`

### Building

Build the site for production:

```bash
# From repository root
nx build oseplatform-web

# Or from this directory
./build.sh
```

The built site will be in the `public/` directory.

### Cleaning

Remove generated files:

```bash
# From repository root
nx clean oseplatform-web

# Or from this directory
rm -rf public resources
```

## 📂 Project Structure

```
oseplatform-web/
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

**Production Branch**: `prod-oseplatform-web`

### Automated Deployment (Primary)

Deployment is automated via the `deploy-oseplatform-web.yml` GitHub Actions workflow:

- **Schedule**: Runs at **6 AM and 6 PM WIB** (UTC+7) every day
- **Change detection**: Compares `HEAD` on `main` against `prod-oseplatform-web`, scoped to `apps/oseplatform-web/`. Skips build and deploy if nothing changed
- **Build**: Runs `nx build oseplatform-web` (Hugo extended build with PaperMod theme)
- **Deploy**: Force-pushes `main` to `prod-oseplatform-web`; Vercel detects the push and builds automatically

**Manual trigger**: The workflow can also be triggered on-demand from the GitHub Actions UI. Set `force_deploy=true` to deploy even if no changes are detected.

### Emergency / On-Demand Deployment

For immediate deployments outside the scheduled window, use the `apps-oseplatform-web-deployer` AI agent or run directly:

```bash
git push --force origin HEAD:prod-oseplatform-web
```

## 📜 License

This project is part of the Open Sharia Enterprise platform and is licensed under the MIT License.

## 🔗 Links

- [Main Repository](https://github.com/wahidyankf/open-sharia-enterprise)
- [Hugo Documentation](https://gohugo.io/documentation/)
- [PaperMod Theme Documentation](https://github.com/adityatelange/hugo-PaperMod/wiki)
