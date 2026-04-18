# Transforms

Transforms apply to `propagate` and `bidirectional` content before the sync-maker surfaces it as a finding. The classifier row's Transform column selects exactly one.

## `identity`

No modification. Propagate or adopt the file byte-for-byte.

Applies to files whose content is inherently generic (no product-app references): `LICENSE`-neutral config files, generic skill definitions, generic convention docs, the shared `Brewfile`, `.husky` hook scripts, etc.

### Pseudocode

```python
def identity(source_bytes: bytes) -> bytes:
    return source_bytes
```

## `strip-product-sections`

Remove content that mentions `ose-public`-specific product apps while preserving generic content.

### Product-app markers

The transform flags a heading or body paragraph as product-specific when any of the following tokens appear (case-insensitive, word-boundary):

- Product names: `OrganicLever`, `AyoKoding`, `OSE Platform`, `Open Sharia Enterprise` (when used as a product name rather than the repo name).
- Product paths: `apps/organiclever-*`, `apps/ayokoding-*`, `apps/oseplatform-*`, `specs/apps/organiclever/`, `specs/apps/ayokoding/`, `specs/apps/oseplatform/`.
- Product domain strings: `organiclever.com`, `ayokoding.com`, `oseplatform.com`.

### Rewrite rules

- **H2/H3 sections**: If the heading line or any paragraph within the section (stopping at the next equal-or-higher heading) contains a product-app marker, remove the entire section including its heading.
- **Table rows**: If a row's cells contain a product-app marker, remove the row. Preserve the header and separator rows.
- **List items**: If a bullet's body contains a product-app marker, remove the bullet. Preserve surrounding bullets.
- **Inline mentions** (marker in otherwise-generic sentence): leave in place; flag the file as a **transform-gap** instead — the agent declines to auto-transform and surfaces the file for maintainer review.

### Non-goals

- The transform does NOT rewrite inline text to remove product markers; it either removes structural elements whole or abstains.
- The transform does NOT translate product references into generic equivalents; the primer has its own voice.
- The transform does NOT touch the file's frontmatter other than through the H2/H3 rules above.

### Pseudocode

```python
PRODUCT_MARKERS = [
    r"\bOrganicLever\b",
    r"\bAyoKoding\b",
    r"\bOSE Platform\b",
    r"\bOpen Sharia Enterprise\b",
    r"apps/(organiclever|ayokoding|oseplatform)-",
    r"specs/apps/(organiclever|ayokoding|oseplatform)/",
    r"(organiclever|ayokoding|oseplatform)\.com",
]

def strip_product_sections(source: str) -> str:
    sections = split_by_heading(source, levels=[2, 3])
    kept = [s for s in sections if not any_marker(s.text, PRODUCT_MARKERS)]
    return reassemble(kept)
```

## Transform-gap handling

When `strip-product-sections` encounters inline markers that cannot be handled by the structural rules, the agent:

1. Adds the file to the **Transform-gap** list in its report.
2. Excludes the file from the findings list.
3. Does NOT propose a change for that file in any mode.

Transform-gap is not an error — it is the safe default when automation is unsure. The maintainer reviews the list and either hand-syncs the file or extends the transform vocabulary via a convention amendment.

## Invariants

- Every `propagate`/`bidirectional` file has exactly one transform applied.
- Transforms are deterministic: same input bytes + same marker list produce same output bytes.
- Transforms never touch `neither` paths (those are dropped before the transform stage).
- New transforms require a convention amendment; the skill never invents a transform on the fly.
