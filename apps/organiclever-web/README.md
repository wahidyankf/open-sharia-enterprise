# organiclever-web

Landing and promotional website for [OrganicLever](https://www.organiclever.com/) — an individual productivity tracker with Sharia-compliant features.

**URL**: https://www.organiclever.com/

## Purpose

This app serves as the public-facing landing page for OrganicLever. It introduces the product, communicates its value proposition, and provides entry points for download and sign-up.

**Distinct role from `organiclever-app`**:

| App                | Purpose                                        | URL                          |
| ------------------ | ---------------------------------------------- | ---------------------------- |
| `organiclever-web` | Landing and promotional website (this app)     | www.organiclever.com         |
| `organiclever-app` | Main Flutter application (web + Android + iOS) | app.organiclever.com, stores |

## Tech Stack

| Layer      | Technology               |
| ---------- | ------------------------ |
| Framework  | Next.js 14 (App Router)  |
| UI Runtime | React 18                 |
| Styling    | TailwindCSS              |
| Components | Radix UI / shadcn-ui     |
| Auth       | Cookie-based sessions    |
| Data       | JSON files (`src/data/`) |
| Deployment | Vercel (auto-detected)   |
| Port (dev) | 3000                     |

## Development Commands

```bash
# Start development server (http://localhost:3000)
nx dev organiclever-web

# Build for production (local verification)
nx build organiclever-web

# Run E2E tests (app must be running first)
nx run organiclever-web-e2e:test:e2e

# Type checking
npx tsc --noEmit --project apps/organiclever-web/tsconfig.json
```

## Project Structure

```
apps/organiclever-web/
├── src/
│   ├── app/                    # Next.js App Router pages
│   │   ├── dashboard/          # Dashboard route
│   │   ├── login/              # Login route
│   │   ├── api/                # API route handlers
│   │   ├── contexts/           # App-level context providers
│   │   ├── fonts/              # Font assets
│   │   ├── layout.tsx          # Root layout
│   │   └── page.tsx            # Root page (landing)
│   ├── components/             # Reusable React components
│   │   └── ui/                 # shadcn-ui component library
│   ├── contexts/               # Shared React contexts
│   ├── data/                   # JSON data files
│   └── lib/                    # Utility functions and helpers
├── public/                     # Static assets
├── components.json             # shadcn-ui configuration
├── next.config.mjs             # Next.js configuration
├── tailwind.config.ts          # TailwindCSS configuration
├── tsconfig.json               # TypeScript configuration
├── vercel.json                 # Vercel deployment configuration
└── project.json                # Nx project configuration
```

## Deployment

**Branch**: `prod-organiclever-web` → [https://www.organiclever.com/](https://www.organiclever.com/)

Vercel monitors `prod-organiclever-web` and auto-builds on every push. Never commit directly to this branch — it is deployment-only. To deploy:

```bash
# From main branch with clean working tree
git push origin main:prod-organiclever-web --force
```

Use the `apps-organiclever-web-deployer` agent for guided deployment.

## E2E Tests

E2E tests live in [`apps/organiclever-web-e2e/`](../organiclever-web-e2e/). See that directory's README for details.

## Related

- **Skill**: `apps-organiclever-web-developing-content` — full development reference
- **Agent**: `apps-organiclever-web-deployer` — deploys to production
- **Agent**: `swe-e2e-test-developer` — E2E testing with Playwright
