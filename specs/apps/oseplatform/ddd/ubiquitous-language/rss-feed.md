# Ubiquitous Language — rss-feed

**Bounded context**: `rss-feed`
**Maintainer**: oseplatform-web team
**Last reviewed**: 2026-05-10
**Audience:** Engineers, Technical Product/Project Managers

## One-line summary

RSS 2.0 feed generation route handler at `/feed.xml`. Reads update articles and serializes
to XML. No UI. Customer of `content`.

## Term index

| Term               | Code identifier(s) | Used in features            |
| ------------------ | ------------------ | --------------------------- |
| `Feed`             | `buildFeed`        | `rss-feed/rss-feed.feature` |
| `Feed entry`       | `buildFeed`        | `rss-feed/rss-feed.feature` |
| `Channel metadata` | `buildFeed`        | `rss-feed/rss-feed.feature` |

## Terms in detail

### Term: `Feed`

Complete RSS 2.0 XML document served at `/feed.xml`. Marked `force-static`.

**Code identifier(s)**: `buildFeed` (`src/contexts/rss-feed/application/feed-builder.ts`).

**Used in features**: `rss-feed/rss-feed.feature`

**Forbidden synonyms in this context**: "subscription"; "RSS" alone.

**Related**: `Feed entry`, `Channel metadata`

---

### Term: `Feed entry`

One `<item>` element inside the `<channel>`. Carries title, link, guid, pubDate, description.

**Code identifier(s)**: `buildFeed` (per-update item construction;
`src/contexts/rss-feed/application/feed-builder.ts`).

**Used in features**: `rss-feed/rss-feed.feature`

**Forbidden synonyms in this context**: "post"; "article".

**Related**: `Feed`, `Channel metadata`

---

### Term: `Channel metadata`

The `<channel>` header fields — `title`, `link`, `description`, `language`, `atom:link self`.

**Code identifier(s)**: `buildFeed` (channel construction;
`src/contexts/rss-feed/application/feed-builder.ts`).

**Used in features**: `rss-feed/rss-feed.feature`

**Forbidden synonyms in this context**: "meta"; "channel info".

**Related**: `Feed`, `Feed entry`
