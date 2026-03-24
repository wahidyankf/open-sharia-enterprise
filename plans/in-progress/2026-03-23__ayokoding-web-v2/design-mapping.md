# Design Mapping: Hextra → shadcn/ui + Tailwind CSS

## Layout Grid

| Breakpoint | Viewport | Sidebar           | Content    | TOC         |
| ---------- | -------- | ----------------- | ---------- | ----------- |
| Desktop    | ≥1280px  | 250px fixed left  | max-w-3xl  | 200px right |
| Laptop     | ≥1024px  | 250px fixed left  | flex-1     | hidden      |
| Tablet     | ≥768px   | collapsed to icon | flex-1     | hidden      |
| Mobile     | <768px   | hamburger drawer  | full-width | hidden      |

## Color Tokens (Light/Dark)

| Token              | Light Mode | Dark Mode |
| ------------------ | ---------- | --------- |
| Background         | white      | #111827   |
| Foreground         | #111827    | #f9fafb   |
| Primary            | #2563eb    | #3b82f6   |
| Primary foreground | white      | white     |
| Muted              | #f3f4f6    | #1f2937   |
| Muted foreground   | #6b7280    | #9ca3af   |
| Border             | #e5e7eb    | #374151   |
| Sidebar bg         | #f9fafb    | #111827   |
| Sidebar border     | #e5e7eb    | #1f2937   |
| Code bg            | #f3f4f6    | #1e293b   |

## Typography

| Element    | Font              | Size     | Weight | Line Height |
| ---------- | ----------------- | -------- | ------ | ----------- |
| Body       | Inter / system-ui | 16px     | 400    | 1.75        |
| H1         | Inter / system-ui | 2.25rem  | 800    | 1.2         |
| H2         | Inter / system-ui | 1.5rem   | 700    | 1.3         |
| H3         | Inter / system-ui | 1.25rem  | 600    | 1.4         |
| H4         | Inter / system-ui | 1.125rem | 600    | 1.5         |
| Code       | monospace         | 0.875rem | 400    | 1.7         |
| Sidebar    | Inter / system-ui | 0.875rem | 400    | 1.5         |
| TOC        | Inter / system-ui | 0.8rem   | 400    | 1.5         |
| Breadcrumb | Inter / system-ui | 0.875rem | 400    | 1.5         |

## Component Mapping

| Hextra Element     | shadcn/ui + Tailwind Equivalent       |
| ------------------ | ------------------------------------- |
| Header bar         | Custom header with flex layout        |
| Sidebar tree       | Custom collapsible tree (Button)      |
| Mobile drawer      | Sheet (slide from left)               |
| Search dialog      | Command (cmdk)                        |
| Theme toggle       | DropdownMenu with Button              |
| Language switcher  | DropdownMenu with Button              |
| Breadcrumb         | Breadcrumb (custom)                   |
| TOC (On this page) | Custom sticky list                    |
| Callout admonition | Alert (warning/info/default variants) |
| Tabs shortcode     | Tabs (shadcn)                         |
| Code blocks        | Custom with shiki (server-rendered)   |
| Prev/Next nav      | Custom flex layout with Button        |
| Footer             | Custom footer                         |

## Responsive Rules

### Code blocks

- Desktop: full width within content area
- Mobile: horizontal scroll (`overflow-x-auto`)

### Tables

- Horizontal scroll wrapper on overflow

### Images

- `max-width: 100%`, centered via `mx-auto`
- Hover scale effect (`transform: scale(1.02)`)

### Search dialog

- Desktop: centered modal (max-w-lg)
- Mobile: full-screen overlay

### Sidebar

- Desktop: persistent 250px left column
- Mobile: Sheet overlay (slide from left)

### TOC

- Desktop ≥1280px: right column, sticky
- Below 1280px: hidden

### Breadcrumb

- Mobile: truncated with ellipsis

### Prev/Next nav

- Desktop: side-by-side
- Mobile: stacked vertically

## Hugo Custom CSS Replicated

- Images and figures: `mx-auto max-w-full`
- Figure text alignment: `text-center`
- Hover effect: `hover:scale-[1.02] transition-transform duration-300`

## i18n Keys (9 keys)

| Key               | EN                  | ID                    |
| ----------------- | ------------------- | --------------------- |
| readMore          | Read More           | Baca Selengkapnya     |
| lastUpdated       | Last updated        | Terakhir diperbarui   |
| publishedOn       | Published on        | Dipublikasikan pada   |
| author            | Author              | Penulis               |
| tags              | Tags                | Tag                   |
| categories        | Categories          | Kategori              |
| share             | Share               | Bagikan               |
| relatedContent    | Related Content     | Konten Terkait        |
| openSourceProject | Open Source Project | Proyek Sumber Terbuka |
