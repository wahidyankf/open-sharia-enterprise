# Delivery Checklist: OSE Platform Web - Next.js Rewrite

Execute phases in order. Each phase produces a working, committable state.

---

## Phase 0: Preparation

### Verify Prerequisites

- [ ] Confirm CI passes on main (`nx affected -t typecheck lint test:quick`)
- [ ] Confirm no uncommitted changes in working tree
- [ ] Verify Node.js 24 and npm 11 available (via Volta)

### Visual Reference Capture (Playwright)

- [ ] Start Hugo dev server: `nx dev oseplatform-web` (port 3000)
- [ ] Create `scripts/capture-hugo-reference.ts` (Playwright script, see tech-docs.md)
- [ ] Capture screenshots at 3 viewports (mobile 375px, tablet 768px, desktop 1440px) for:
  - [ ] Landing page (`/`)
  - [ ] About page (`/about/`)
  - [ ] Updates listing (`/updates/`)
  - [ ] Update detail (`/updates/2026-02-08-phase-0-end-of-phase-0/`)
- [ ] Save 12 screenshots (4 pages × 3 viewports) to `local-temp/oseplatform-web-hugo-reference/`
- [ ] Stop Hugo dev server

---

## Phase 1: Project Scaffolding

### Archive Hugo Files

- [ ] Create `archived/oseplatform-web-hugo/` directory
- [ ] Move `hugo.yaml` to `archived/oseplatform-web-hugo/`
- [ ] Move `go.mod` and `go.sum` to `archived/oseplatform-web-hugo/`
- [ ] Move `build.sh` to `archived/oseplatform-web-hugo/`
- [ ] Move `archetypes/` to `archived/oseplatform-web-hugo/`
- [ ] Move `layouts/` to `archived/oseplatform-web-hugo/`
- [ ] Copy current `vercel.json` to `archived/oseplatform-web-hugo/vercel.json`
- [ ] Copy current `project.json` to `archived/oseplatform-web-hugo/project.json`
- [ ] Keep `content/` directory in place (shared with new app)
- [ ] Keep `README.md` in place (will be rewritten later)

### Initialize Next.js

- [ ] Create `next.config.ts` with standalone output, file tracing for content/ and generated/
- [ ] Create `tsconfig.json` with strict mode, path aliases (`@/*`), bundler resolution (copy from ayokoding-web)
- [ ] Create `package.json` with all dependencies (see tech-docs.md)
- [ ] Create `postcss.config.mjs` (Tailwind v4 PostCSS plugin)
- [ ] Create `src/app/globals.css` with Tailwind directives, shared token import (`@open-sharia-enterprise/ts-ui-tokens`), brand colors, dark mode overrides, code block styling
- [ ] Create `.gitignore` additions (`.next/`, `generated/`, `coverage/`)
- [ ] Create `public/` directory with `favicon.ico` and `favicon.png`
- [ ] Run `npm install` from monorepo root
- [ ] Create `src/app/layout.tsx` (root layout: import globals.css, metadata, `min-h-screen antialiased`)
- [ ] Create `src/app/page.tsx` (placeholder landing page)
- [ ] Verify `nx dev oseplatform-web` starts on port 3100

### Initialize Tooling

- [ ] Create `vitest.config.ts` with three test projects (unit, unit-fe, integration), coverage exclusions (see tech-docs.md)
- [ ] Create `oxlint.json` with plugins: typescript, react, nextjs, import, unicorn, jsx-a11y, vitest (copy from ayokoding-web)
- [ ] Create `components.json` manually (new-york style, rsc: true, neutral base color — see tech-docs.md for full content)
  - Note: Do NOT run `npx shadcn@latest init` separately — it would overwrite the manually crafted `components.json`. The `init` command is only needed if generating `components.json` interactively. Use the manual file from tech-docs.md directly.
- [ ] Install shadcn/ui components: badge, card, command, dropdown-menu, scroll-area, separator, sheet, tabs, tooltip
- [ ] Create `src/lib/utils.ts` with `cn()` utility (clsx + tailwind-merge)
- [ ] Create `src/test/setup.ts` for frontend tests (import `@testing-library/jest-dom`)

### Initialize Playwright (Visual Regression)

- [ ] Install Playwright: add `@playwright/test` to devDependencies
- [ ] Create `playwright.config.ts` with 3 viewports (mobile, tablet, desktop) -- see tech-docs.md
- [ ] Create `test/visual/` directory for visual regression tests

### Update Nx Configuration

- [ ] Replace `project.json` with Next.js targets (see tech-docs.md)
- [ ] Verify tags: `type:app, platform:nextjs, lang:ts, domain:oseplatform`
- [ ] Verify `implicitDependencies: ["oseplatform-cli"]`
- [ ] Verify `nx dev oseplatform-web` works
- [ ] Verify `nx build oseplatform-web` produces output

**Commit**: `refactor(oseplatform-web): scaffold Next.js 16 app, archive Hugo files`

---

## Phase 2: Gherkin Specs

### Create Spec Directory

- [ ] Create `specs/apps/oseplatform-web/gherkin/` directory structure (be/ and fe/ subdirs)
- [ ] Create `specs/apps/oseplatform-web/README.md` with spec overview

### Write Backend Feature Files (`specs/apps/oseplatform-web/be/gherkin/`)

- [ ] Create `content-retrieval/content-retrieval.feature` (4 scenarios: getBySlug, listUpdates, draft filtering, 404 handling)
- [ ] Create `search/search.feature` (3 scenarios: query matching, empty results, result limiting)
- [ ] Create `rss-feed/rss-feed.feature` (2 scenarios: feed structure, entry content)
- [ ] Create `health/health.feature` (1 scenario: health endpoint response)
- [ ] Create `seo/seo.feature` (2 scenarios: sitemap generation, robots.txt)

### Write Frontend Feature Files (`specs/apps/oseplatform-web/fe/gherkin/`)

- [ ] Create `landing-page.feature` (2 scenarios: hero content, social icons)
- [ ] Create `navigation.feature` (3 scenarios: header links, breadcrumbs, prev/next)
- [ ] Create `theme.feature` (2 scenarios: default light mode, toggle persistence)
- [ ] Create `responsive.feature` (2 scenarios: mobile nav, desktop layout)

**Commit**: `test(oseplatform-web): add Gherkin specs for Next.js rewrite`

---

## Phase 3: Content Layer

### Types and Schemas

- [ ] Create `src/server/content/types.ts` (ContentMeta, ContentPage, Heading, PageLink, SearchResult)
- [ ] Create `src/lib/schemas/content.ts` (frontmatterSchema with Zod -- include Hugo-compatible fields: showtoc, url, categories, summary)
- [ ] Create `src/lib/schemas/search.ts` (searchQuerySchema without locale, searchResultSchema without locale)

### Repository Pattern

- [ ] Create `src/server/content/repository.ts` (ContentRepository interface: readAllContent, readFileContent)
- [ ] Create `src/server/content/repository-fs.ts` (FileSystem implementation)
  - [ ] Implement `readAllContent()`: glob markdown files, parse frontmatter, derive slugs
  - [ ] Implement `readFileContent(filePath)`: read single file with gray-matter
  - [ ] Handle `SHOW_DRAFTS` env var for draft filtering
  - [ ] Validate frontmatter with Zod schema
  - [ ] Derive slug from file path (e.g., `content/updates/2026-02-08-*.md` -> `updates/2026-02-08-*`)
- [ ] Create `src/server/content/repository-memory.ts` (in-memory implementation for tests)

### Content Parsing

- [ ] Create `src/server/content/reader.ts` (file I/O utilities, slug derivation, stripMarkdown for search)
- [ ] Create `src/server/content/parser.ts` (unified markdown pipeline)
  - [ ] Configure remark-parse, remark-gfm
  - [ ] Configure remark-rehype with `allowDangerousHtml: true`
  - [ ] Configure rehype-raw, rehype-pretty-code (with shiki), rehype-slug, rehype-autolink-headings
  - [ ] Configure rehype-stringify
  - [ ] Extract headings (H2-H4) during parsing via hast tree walk
  - [ ] Note: No remark-math/rehype-katex needed (no math in oseplatform content)
  - [ ] Note: No shortcode transformation needed (only mermaid, handled by markdown-renderer)

### Content Service

- [ ] Create `src/server/content/service.ts` (ContentService class)
  - [ ] Implement `getIndex()` with in-memory caching
  - [ ] Implement `getBySlug(slug)` with markdown parsing and heading extraction
  - [ ] Implement `listUpdates()` sorted by date descending
  - [ ] Implement `search(query, limit)` with FlexSearch (lazy-loaded from generated data)
  - [ ] Implement `isSearchIndexReady()` for search availability

### Search Data Generation

- [ ] Create `src/scripts/generate-search-data.ts`
  - [ ] Read all non-draft, non-section content files
  - [ ] Strip markdown to plain text (first 2000 chars)
  - [ ] Write `generated/search-data.json` with id, title, content, slug fields
- [ ] Add `generated/` to `.gitignore`

### Verify Content Layer

- [ ] Run a manual test: import ContentService, call `getIndex()`, verify all pages found
- [ ] Verify `getBySlug("about")` returns parsed HTML
- [ ] Verify `listUpdates()` returns 4 posts in correct order

**Commit**: `feat(oseplatform-web): implement content layer with markdown pipeline`

---

## Phase 4: tRPC API

### Initialize tRPC

- [ ] Create `src/server/trpc/init.ts` (TRPCContext with ContentService, initTRPC with superjson, singleton defaultContentService)
- [ ] Create `src/server/trpc/procedures/content.ts` (getBySlug, listUpdates -- no locale param)
- [ ] Create `src/server/trpc/procedures/search.ts` (query with Zod validation, no locale param)
- [ ] Create `src/server/trpc/procedures/meta.ts` (health endpoint returning `{ status: "ok" }`)
- [ ] Create `src/server/trpc/router.ts` (combine content, search, meta routers, export AppRouter type)

### Wire Up Endpoints

- [ ] Create `src/app/api/trpc/[trpc]/route.ts` (fetchRequestHandler for GET/POST)
- [ ] Create `src/lib/trpc/server.ts` (server caller with `import "server-only"` guard)
- [ ] Create `src/lib/trpc/client.ts` (vanilla `createTRPCClient` with httpBatchLink + superjson)
- [ ] Create `src/lib/trpc/provider.tsx` (QueryClientProvider wrapper only -- no tRPC links)

### Verify API

- [ ] Start dev server, hit `/api/trpc/meta.health` -- expect `{"result":{"data":{"status":"ok"}}}`
- [ ] Hit `/api/trpc/content.getBySlug?input={"slug":"about"}` -- expect about page content
- [ ] Hit `/api/trpc/content.listUpdates` -- expect 4 updates

**Commit**: `feat(oseplatform-web): add tRPC API with content and search procedures`

---

## Phase 5: Layout and Landing Page

### Root Layout

- [ ] Update `src/app/layout.tsx`:
  - [ ] Import `globals.css`
  - [ ] Add ThemeProvider (next-themes, default: light, `suppressHydrationWarning` on html)
  - [ ] Add TRPCProvider
  - [ ] Add SearchProvider
  - [ ] Add global metadata (title template: `%s | OSE Platform`, description, Open Graph, metadataBase)
  - [ ] Body: `className="min-h-screen antialiased"`

### Header Component

- [ ] Create `src/components/layout/header.tsx`
  - [ ] Logo/site title linking to `/`
  - [ ] Nav links: Updates, About, Documentation (external), GitHub (external)
  - [ ] Search button (opens search dialog, Cmd+K hint on desktop)
  - [ ] Theme toggle
  - [ ] Responsive: collapse to hamburger on mobile
  - [ ] Use `Button` from `@open-sharia-enterprise/ts-ui` (shared lib)
- [ ] Create `src/components/layout/mobile-nav.tsx` (Sheet-based mobile menu)
- [ ] Create `src/components/layout/theme-toggle.tsx` (DropdownMenu with light/dark/system, uses ts-ui Button)

### Footer Component

- [ ] Create `src/components/layout/footer.tsx`
  - [ ] MIT License link
  - [ ] AyoKoding attribution
  - [ ] Consistent with current Hugo footer

### Landing Page

- [ ] Create `src/components/landing/hero.tsx`
  - [ ] Platform title and mission description
  - [ ] CTA buttons (Learn More -> /about, GitHub -> external)
  - [ ] "Why Open Source Matters" section with bullet points
- [ ] Create `src/components/landing/social-icons.tsx` (GitHub, RSS icons)
- [ ] Update `src/app/page.tsx` to compose Header + Hero + SocialIcons + Footer
- [ ] Verify visual parity with Hugo landing page screenshots

**Commit**: `feat(oseplatform-web): implement layout, header, footer, and landing page`

---

## Phase 6: Content Pages

### Markdown Renderer

- [ ] Create `src/components/content/markdown-renderer.tsx`
  - [ ] Parse HTML string with html-react-parser
  - [ ] Handle mermaid: detect `<figure data-rehype-pretty-code-figure>` with `data-language="mermaid"` -> `<MermaidDiagram>`
  - [ ] Fallback: detect `<code class="language-mermaid">` in `<pre>` -> `<MermaidDiagram>`
  - [ ] Convert internal `<a>` hrefs to Next.js `<Link>` (for `/about/`, `/updates/` paths)
  - [ ] Apply prose typography classes (`prose prose-neutral dark:prose-invert`)
- [ ] Create `src/components/content/mermaid.tsx` (client component, dynamic import of mermaid, render to SVG)

### Navigation Components

- [ ] Create `src/components/layout/breadcrumb.tsx` (ChevronRight separators, last item non-linked)
- [ ] Create `src/components/layout/toc.tsx` (client component with IntersectionObserver for active heading tracking)
- [ ] Create `src/components/layout/prev-next.tsx` (prev/next update navigation with ChevronLeft/Right)

### About Page

- [ ] Create `src/app/about/page.tsx`
  - [ ] Call `serverCaller.content.getBySlug({ slug: "about" })`
  - [ ] Render with MarkdownRenderer, TOC (if showtoc), breadcrumbs
  - [ ] Add `generateMetadata()` for SEO

### Updates Listing

- [ ] Create `src/components/content/update-card.tsx` (card with title, date, summary, tags as badges)
- [ ] Create `src/app/updates/page.tsx`
  - [ ] Call `serverCaller.content.listUpdates()`
  - [ ] Render update cards sorted by date
  - [ ] Add `generateMetadata()` for SEO

### Update Detail

- [ ] Create `src/app/updates/[slug]/page.tsx`
  - [ ] `generateStaticParams()` from listUpdates
  - [ ] `dynamicParams = false`
  - [ ] Render markdown with TOC, reading time, tags as badges, formatted date (Geist Mono)
  - [ ] Add prev/next navigation between updates
  - [ ] Add `generateMetadata()` for SEO

### Verify Content Pages

- [ ] Navigate to `/about/` -- verify content matches Hugo version
- [ ] Navigate to `/updates/` -- verify all 4 posts listed
- [ ] Navigate to each update detail page -- verify content renders
- [ ] Verify Mermaid diagram on about page renders correctly
- [ ] Verify code blocks have syntax highlighting with light/dark theme support

### Verify Responsive Design (All Content Pages)

- [ ] Test at mobile viewport (375px): single column, hamburger nav, stacked update cards
- [ ] Test at tablet viewport (768px): two-column update cards, proper spacing
- [ ] Test at desktop viewport (1440px): full header nav, max-width content, TOC sidebar
- [ ] Verify touch targets are >= 44px on mobile
- [ ] Verify text is readable without horizontal scrolling at all viewports

**Commit**: `feat(oseplatform-web): implement about, updates, and content rendering`

---

## Phase 7: Search, RSS, and SEO

### Search

- [ ] Create `src/lib/hooks/use-search.ts` (SearchContext with open/setOpen via React Context)
- [ ] Create `src/components/search/search-provider.tsx` (context provider + SearchDialog wrapper)
- [ ] Create `src/components/search/search-dialog.tsx` (CommandDialog from cmdk)
  - [ ] Uses vanilla `trpcClient` to query search endpoint (not React Query hooks)
  - [ ] Debounced search (200ms)
  - [ ] Displays results with title and excerpt
  - [ ] Keyboard shortcut: Cmd/Ctrl+K
  - [ ] Navigates to selected result via router.push
- [ ] Wire search button in header to open dialog

### RSS Feed

- [ ] Create `src/app/feed.xml/route.ts`
  - [ ] Generate RSS 2.0 XML from listUpdates()
  - [ ] Include title, link, date, summary for each entry
  - [ ] Set `Content-Type: application/xml`

### SEO

- [ ] Create `src/app/sitemap.ts` (generate sitemap with all public URLs)
- [ ] Create `src/app/robots.ts` (allow all crawlers, reference sitemap)
- [ ] Verify meta tags on all pages (title, description, Open Graph)

### Verify

- [ ] Open search dialog with Cmd/Ctrl+K, search for "phase" -- expect results
- [ ] Navigate to `/feed.xml` -- verify valid RSS XML
- [ ] Navigate to `/sitemap.xml` -- verify all URLs present
- [ ] Navigate to `/robots.txt` -- verify sitemap reference

**Commit**: `feat(oseplatform-web): add search, RSS feed, sitemap, and robots.txt`

---

## Phase 8: Unit Tests

### Backend Steps

- [ ] Create `test/unit/be-steps/helpers/mock-content.ts` (test fixture content)
- [ ] Create `test/unit/be-steps/helpers/test-caller.ts` (tRPC test caller with in-memory repo)
- [ ] Create `test/unit/be-steps/helpers/test-service.ts` (ContentService with in-memory repo)
- [ ] Create `test/unit/be-steps/content-retrieval.steps.ts`
  - [ ] Use in-memory repository with test fixtures
  - [ ] Test getBySlug, listUpdates, draft filtering, 404
- [ ] Create `test/unit/be-steps/search.steps.ts`
  - [ ] Test query matching, empty results, result limiting
- [ ] Create `test/unit/be-steps/rss-feed.steps.ts`
  - [ ] Test feed structure and entry content
- [ ] Create `test/unit/be-steps/health.steps.ts`
  - [ ] Test health endpoint response
- [ ] Create `test/unit/be-steps/seo.steps.ts`
  - [ ] Test sitemap and robots generation

### Frontend Steps

- [ ] Create `test/unit/fe-steps/helpers/test-setup.ts` (component rendering helpers)
- [ ] Create `test/unit/fe-steps/landing-page.steps.tsx`
  - [ ] Test hero content rendering, social icons (jsdom environment)
- [ ] Create `test/unit/fe-steps/navigation.steps.tsx`
  - [ ] Test header links, breadcrumbs, prev/next (jsdom environment)
- [ ] Create `test/unit/fe-steps/theme.steps.tsx`
  - [ ] Test default light mode, toggle behavior (jsdom environment)
- [ ] Create `test/unit/fe-steps/responsive.steps.tsx`
  - [ ] Test mobile nav visibility, desktop layout (jsdom environment)

### Run Tests

- [ ] Run `npx vitest run --project unit --project unit-fe` -- all tests pass (both backend and frontend scenarios)
- [ ] Verify all 21 Gherkin scenarios covered

**Commit**: `test(oseplatform-web): implement unit tests with Gherkin step definitions`

---

## Phase 9: Coverage Gate

- [ ] Run `vitest run --project unit --project unit-fe --coverage`
- [ ] Check coverage report -- identify gaps below 80%
- [ ] Add missing tests for uncovered code paths
- [ ] Run `rhino-cli test-coverage validate apps/oseplatform-web/coverage/lcov.info 80`
- [ ] Verify `nx run oseplatform-web:test:quick` passes (tests + coverage + links)

**Commit**: `test(oseplatform-web): achieve 80% line coverage threshold`

---

## Phase 10: Integration Tests

- [ ] Create `test/integration/be-steps/helpers/test-caller.ts` (tRPC caller with real filesystem repo)
- [ ] Create `test/integration/be-steps/helpers/test-service.ts` (ContentService with real filesystem)
- [ ] Create `test/integration/be-steps/content-retrieval.steps.ts`
  - [ ] Use real filesystem with actual content/ directory
  - [ ] Test getBySlug, listUpdates against real markdown files
- [ ] Create `test/integration/be-steps/search.steps.ts`
  - [ ] Test search against real content with FlexSearch index
- [ ] Run `nx run oseplatform-web:test:integration` -- all tests pass

**Commit**: `test(oseplatform-web): implement integration tests with real filesystem`

---

## Phase 11: Docker, Vercel, and Visual Tests

### Docker

- [ ] Create `Dockerfile` (multi-stage: deps -> build -> runner, see tech-docs.md)
  - [ ] Inject `ts-ui` and `ts-ui-tokens` into `node_modules` via direct COPY commands in builder stage (see tech-docs.md)
  - [ ] Copy content/ and generated/ into runner stage
  - [ ] Run as non-root `nextjs` user
- [ ] Verify `docker build` succeeds
- [ ] Verify `docker run` serves site on port 3100

### Vercel Configuration

- [ ] Replace `vercel.json` with Next.js configuration (see tech-docs.md)
  - [ ] Set `installCommand` for monorepo (`npm install --prefix=../.. --ignore-scripts`)
  - [ ] Set `buildCommand` with search data generation
  - [ ] Set `ignoreCommand` for `prod-oseplatform-web` branch
  - [ ] Preserve security headers from Hugo config
- [ ] Verify `nx build oseplatform-web` produces standalone output

### Playwright Visual Tests

- [ ] Create `test/visual/pages.spec.ts` (see tech-docs.md)
  - [ ] Test all 4 pages (landing, about, updates, update-detail) at 3 viewports
  - [ ] Assert no console errors on any page
  - [ ] Use `toHaveScreenshot` for visual regression baselines
- [ ] Generate initial screenshot baselines: `npx playwright test --update-snapshots`
- [ ] Run `npx playwright test` -- all visual tests pass

**Commit**: `ci(oseplatform-web): add Docker, Vercel config, and Playwright visual tests`

---

## Phase 12: Reference Updates

### Update README

- [ ] Rewrite `apps/oseplatform-web/README.md` for Next.js
  - [ ] Update tech stack section (Next.js 16, TypeScript, tRPC, Tailwind v4, shadcn/ui)
  - [ ] Update development commands (port 3100)
  - [ ] Update project structure
  - [ ] Update deployment instructions

### Update CLAUDE.md

- [ ] Update oseplatform-web entry: "Hugo static site (PaperMod theme)" -> "Next.js 16 content platform (TypeScript, tRPC)"
- [ ] Update dev port reference (3000 -> 3100)
- [ ] Update Nx targets list for oseplatform-web
- [ ] Update Hugo Sites section: remove oseplatform-web subsection
- [ ] Add oseplatform-web to the coverage enforcement section (80% line coverage)
- [ ] Update tags reference (platform:hugo -> platform:nextjs, add lang:ts)

### Update Agents (`.claude/agents/`)

- [ ] Update `apps-oseplatform-web-deployer.md`:
  - [ ] Change description: "Hugo static site" -> "Next.js 16 content platform"
  - [ ] Update deployment workflow reference: `test-and-deploy-oseplatform-web.yml`
  - [ ] Note that deploy now requires all tests to pass (unit, integration, e2e)
- [ ] Update `apps-oseplatform-web-content-maker.md`:
  - [ ] Change skill reference context: "PaperMod theme" -> "Next.js 16 with tRPC"
  - [ ] Keep content format guidance (markdown unchanged)
  - [ ] Remove Hugo-specific mentions
- [ ] Update `apps-oseplatform-web-content-checker.md`:
  - [ ] Update validation context: remove PaperMod theme compliance
  - [ ] Add awareness of tRPC content procedures
  - [ ] Keep markdown quality checks (framework-agnostic)
- [ ] Update `apps-oseplatform-web-content-fixer.md` (if exists):
  - [ ] Mirror content-checker changes

### Update Skills (`.claude/skills/`)

- [ ] Update `apps-oseplatform-web-developing-content/SKILL.md`:
  - [ ] Replace "Hugo site (PaperMod theme)" with "Next.js 16 content platform"
  - [ ] Remove Hugo-specific frontmatter fields (cover, bookCollapseSection, bookFlatSection, cascade)
  - [ ] Keep: title, date, draft, tags, categories, summary, showtoc, description
  - [ ] Update content structure section (remove `static/` asset references)
  - [ ] Update comparison table: oseplatform-web now uses "Next.js 16" not "PaperMod"
  - [ ] Update deployment workflow section
  - [ ] Remove cover image requirements section
  - [ ] Update internal link format guidance (still absolute paths, no `.md`)

### Sync to OpenCode

- [ ] Run `npm run sync:claude-to-opencode` to propagate agent/skill changes

### Verify oseplatform-cli Compatibility

- [ ] Run `nx run oseplatform-web:links:check` -- must pass with no broken links
- [ ] If failures: investigate `hugo-commons/links` resolution logic
- [ ] Verify `oseplatform-cli` project.json still lists correct implicit dependencies

### Update Governance Docs

- [ ] Grep for `platform:hugo` references to oseplatform-web -- update to `platform:nextjs`
- [ ] Grep for Hugo-specific oseplatform-web references in governance/ -- update
- [ ] Check if `swe-hugo-developer` agent is still needed (verify no other Hugo apps exist; if not, archive it)
- [ ] Check if `libs/hugo-commons` is still needed (used by oseplatform-cli -- yes, keep)

### Remove Hugo Remnants

- [ ] Verify `archived/oseplatform-web-hugo/` contains all Hugo-specific files
- [ ] Verify `oseplatform-web` no longer has `go.mod`, `hugo.yaml`, `build.sh`, `archetypes/`, `layouts/`
- [ ] Grep sweep: search for stale "Hugo" references specific to oseplatform-web (exclude `plans/`, `archived/`)

**Commit**: `docs(oseplatform-web): update agents, skills, and references for Next.js migration`

---

## Phase 13: CI/CD Workflow

### Create Workflow

- [ ] Create `.github/workflows/test-and-deploy-oseplatform-web.yml` (see tech-docs.md)
  - [ ] 5 jobs: unit, integration, e2e, detect-changes, deploy (mirrors ayokoding-web)
  - [ ] Schedule: 2x daily (06:00 WIB, 18:00 WIB)
  - [ ] `unit` job: build Go CLIs, typecheck, lint, test:quick, upload Codecov
  - [ ] `integration` job: run integration tests
  - [ ] `e2e` job: build Next.js, install Playwright, run visual regression, upload report
  - [ ] `detect-changes` job: check apps/oseplatform-web/, libs/ts-ui/, libs/ts-ui-tokens/
  - [ ] `deploy` job: requires all 3 test jobs + changes, force-push to prod-oseplatform-web
  - [ ] `workflow_dispatch` with `force_deploy` input option
- [ ] Delete old Hugo workflow (`test-and-deploy-oseplatform-web.yml` -- replace in-place)

### Verify Deployment

- [ ] Run full quality gate: `nx run oseplatform-web:typecheck && nx run oseplatform-web:lint && nx run oseplatform-web:test:quick && nx run oseplatform-web:build`
- [ ] Verify build output in `.next/` directory
- [ ] Verify standalone output includes content files

**Commit**: `ci(oseplatform-web): replace Hugo workflow with Next.js test-and-deploy`

---

## Phase 14: Final Validation

### Playwright Visual Comparison (Against Hugo Reference)

- [ ] Start Next.js server on port 3100
- [ ] Run Playwright tests comparing all 4 pages at 3 viewports (12 screenshot comparisons)
- [ ] Review any visual differences -- expected changes (theme update) vs regressions
- [ ] Verify Mermaid diagrams render correctly at all viewports
- [ ] Verify dark mode renders correctly on all pages at all viewports
- [ ] Verify no console errors on any page

### URL Parity

- [ ] Navigate to `/` -- landing page loads
- [ ] Navigate to `/about/` -- about page loads
- [ ] Navigate to `/updates/` -- updates listing loads
- [ ] Navigate to each of 4 update detail pages -- content loads
- [ ] Navigate to `/feed.xml` -- valid RSS
- [ ] Navigate to `/sitemap.xml` -- all URLs present
- [ ] Navigate to `/robots.txt` -- valid robots file
- [ ] Navigate to `/api/trpc/meta.health` -- health check responds

### Quality Gate

- [ ] `nx run oseplatform-web:typecheck` passes
- [ ] `nx run oseplatform-web:lint` passes
- [ ] `nx run oseplatform-web:test:quick` passes (tests + coverage >= 80% + links)
- [ ] `nx run oseplatform-web:test:integration` passes
- [ ] `nx run oseplatform-web:build` succeeds

### Cross-Repository Verification

- [ ] `nx affected -t typecheck lint test:quick` passes for all affected projects
- [ ] No stale references to Hugo for oseplatform-web (grep for `platform:hugo.*oseplatform`)
- [ ] `oseplatform-cli links check` passes

### Cleanup Reference Capture Script

- [ ] Archive or remove `scripts/capture-hugo-reference.ts` (it served its one-time purpose in Phase 0)
  - Option A: Delete the file (screenshots remain in `local-temp/` as the reference)
  - Option B: Move to `local-temp/oseplatform-web-hugo-reference/capture-hugo-reference.ts` for historical context
  - Add a `# One-time Hugo reference capture script - archived` comment if keeping

---

## Post-Completion Notes

After all phases are complete and verified:

1. Move plan folder to `plans/done/` with completion date
2. Deploy to production via deployer agent or manual push to `prod-oseplatform-web`
3. Verify live site at oseplatform.com
4. **Vercel Dashboard Configuration** (manual, see tech-docs.md for details):
   - [ ] Open Vercel Dashboard -> Project Settings -> General
   - [ ] Verify Framework Preset shows "Next.js" (auto-detected from `vercel.json`)
   - [ ] Remove `HUGO_VERSION` environment variable
   - [ ] Remove any Hugo-related build environment variables
   - [ ] Verify deployment logs show `next build` (not Hugo)
   - [ ] Verify security headers present on live site responses
   - [ ] Verify static assets have cache headers
