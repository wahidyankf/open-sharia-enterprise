# Ubiquitous Language — seo

**Bounded context**: `seo`
**Maintainer**: oseplatform-web team
**Last reviewed**: 2026-05-10
**Audience:** Engineers, Technical Product/Project Managers

## One-line summary

Search-engine surfaces — sitemap, robots, per-route metadata. Application computes sitemap
entries and metadata; presentation exposes the `defaultMetadata` constant. Customer of `content`.

## Term index

| Term             | Code identifier(s)         | Used in features  |
| ---------------- | -------------------------- | ----------------- |
| `Meta tag`       | `defaultMetadata`          | `seo/seo.feature` |
| `Sitemap entry`  | `buildSitemap`             | `seo/seo.feature` |
| `Canonical URL`  | `buildSitemap`, `SITE_URL` | `seo/seo.feature` |
| `OpenGraph card` | `defaultMetadata`          | `seo/seo.feature` |

## Terms in detail

### Term: `Meta tag`

A single HTML `<head>` tag — `<title>`, `<meta name="description">`, `<meta property="og:*">`.

**Code identifier(s)**: `defaultMetadata` (`src/contexts/seo/presentation/metadata.ts`).

**Used in features**: `seo/seo.feature`

**Forbidden synonyms in this context**: "metadata" alone; "header tag".

**Related**: `OpenGraph card`, `Canonical URL`

---

### Term: `Sitemap entry`

One row in `/sitemap.xml` — `url`, `lastModified`, `changeFrequency`, `priority`.

**Code identifier(s)**: `buildSitemap` (`src/contexts/seo/application/sitemap-builder.ts`).

**Used in features**: `seo/seo.feature`

**Forbidden synonyms in this context**: "site map"; "page list".

**Related**: `Canonical URL`

---

### Term: `Canonical URL`

Fully qualified absolute URL of a page. The constant `SITE_URL` is the base.

**Code identifier(s)**: `buildSitemap`, `SITE_URL` (`src/contexts/seo/application/sitemap-builder.ts`).

**Used in features**: `seo/seo.feature`

**Forbidden synonyms in this context**: "permalink"; "URL" alone.

**Related**: `Sitemap entry`, `Meta tag`

---

### Term: `OpenGraph card`

Bundle of OpenGraph `<meta property="og:*">` tags for social-media link previews.

**Code identifier(s)**: `defaultMetadata` (`src/contexts/seo/presentation/metadata.ts`).

**Used in features**: `seo/seo.feature`

**Forbidden synonyms in this context**: "og card"; "link preview".

**Related**: `Meta tag`
