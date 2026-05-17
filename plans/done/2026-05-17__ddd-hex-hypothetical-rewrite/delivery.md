---
title: "Delivery checklist — Rewrite DDD + Hexagonal in Practice with hypothetical domain"
date: 2026-05-17
---

# Delivery Checklist (TDD-shaped)

## Phase 1 — Plan + scaffolding

- [x] Lock hypothetical domain spec (Conference Talk Submission Platform) — `tech-docs.md`
- [x] Create plan folder with README/brd/prd/tech-docs/delivery
- [x] Catalog every existing guide → wiring seam → hypothetical mapping (in `tech-docs.md`)

## Phase 2 — Top-level + per-track overviews

- [ ] Rewrite `ddd-hexagonal-in-practice/_index.md` (drop F# stack / Java stack specifics from description; keep two-track nav)
- [ ] Rewrite `fp-in-the-field/_index.md` + `overview.md` (drop ose-app-be tags + dogfooding section; add hypothetical-domain framing block)
- [ ] Rewrite `oop-in-the-field/_index.md` + `overview.md` (drop organiclever-be tags + dogfooding section; add hypothetical-domain framing block)

## Phase 3 — FP track rewrite

- [ ] Rewrite `fp-in-the-field/beginner.md` (Guides 1-6)
- [ ] Rewrite `fp-in-the-field/intermediate.md` (Guides 7-14)
- [ ] Rewrite `fp-in-the-field/advanced.md` (Guides 15-22)
- [ ] Rewrite `fp-in-the-field/production.md` (Guides 23-27)

## Phase 4 — OOP track rewrite

- [ ] Rewrite `oop-in-the-field/beginner.md` (Guides 1-7)
- [ ] Rewrite `oop-in-the-field/intermediate.md` (Guides 8-15)
- [ ] Rewrite `oop-in-the-field/advanced.md` (Guides 16-22)
- [ ] Rewrite `oop-in-the-field/production.md` (Guides 23-27)

## Phase 5 — Quality gate

- [ ] Strip all relative links to `apps/ose-app-be` and `apps/organiclever-be` from the 12 files
- [ ] Run `apps-ayokoding-web-in-the-field-checker` against both tracks
- [ ] Run `apps-ayokoding-web-in-the-field-fixer` with the checker report (cap 3 iterations)
- [ ] Run `apps-ayokoding-web-link-checker` on rewritten content
- [ ] Run `npm run lint:md:fix` + `npm run format:md`

## Phase 6 — Build + tests

- [ ] `nx build ayokoding-web` — content layer parses
- [ ] `nx run ayokoding-web:test:quick` — unit + coverage + link validation
- [ ] `nx run ayokoding-web-fe-e2e:test:e2e` — smoke routes for rewritten pages

## Phase 7 — Plan close-out

- [ ] Move plan to `plans/done/2026-05-17__ddd-hex-hypothetical-rewrite/`
- [ ] Add outcome + lessons section to README.md
- [ ] Await user explicit commit + push approval; then split commits by domain
