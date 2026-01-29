# Anti-Patterns in Hugo Development

> **Companion Document**: For positive guidance on what to do, see [Best Practices](./best-practices.md)

## Overview

## Principles Respected

This companion document respects:

- **[Simplicity Over Complexity](../../principles/general/simplicity-over-complexity.md)**: Provides practical examples of simple vs complex approaches
- **[Explicit Over Implicit](../../principles/software-engineering/explicit-over-implicit.md)**: Makes patterns and anti-patterns explicit through clear examples

## Conventions Implemented/Respected

This companion document supports the conventions in this directory by providing practical examples and guidance.

Understanding common mistakes in Hugo site development helps teams build more maintainable, performant, and reliable sites. These anti-patterns cause complexity, performance issues, and maintenance burden.

## Purpose

## Principles Respected

This companion document respects:

- **[Simplicity Over Complexity](../../principles/general/simplicity-over-complexity.md)**: Provides practical examples of simple vs complex approaches
- **[Explicit Over Implicit](../../principles/software-engineering/explicit-over-implicit.md)**: Makes patterns and anti-patterns explicit through clear examples

## Conventions Implemented/Respected

This companion document supports the conventions in this directory by providing practical examples and guidance.

This document provides:

- Common anti-patterns in Hugo development
- Examples of problematic implementations
- Solutions and corrections for each anti-pattern
- Performance and maintenance considerations

## Common Anti-Patterns

### Anti-Pattern 1: Editing Theme Files Directly

**Problem**: Modifying theme files directly breaks update path.

**Bad Example:**

```bash
# Editing theme file directly
vim themes/hextra/layouts/_default/baseof.html
# Commit changes
git add themes/hextra/
```

**Solution:**

```bash
# Override in project layouts directory
cp themes/hextra/layouts/_default/baseof.html layouts/_default/baseof.html
# Edit project copy
vim layouts/_default/baseof.html
```

**Rationale:**

- Theme updates overwrite direct edits
- Loses customizations when updating theme
- Creates merge conflicts
- Project `layouts/` takes precedence in Hugo

### Anti-Pattern 2: Using static/ for Processed Assets

**Problem**: Files in `static/` bypass Hugo Pipes processing.

**Bad Example:**

```
static/
├── css/main.css      # NO minification
├── js/app.js         # NO bundling
└── images/large.jpg  # NO optimization
```

**Solution:**

```
assets/
├── css/main.css      # PostCSS, minification
├── js/app.js         # Bundling, minification
└── images/large.jpg  # Resizing, WebP conversion

static/
├── fonts/            # Fonts (no processing needed)
├── favicon.ico       # Favicon (no processing needed)
└── robots.txt        # Static files only
```

**Rationale:**

- Hugo Pipes only processes `assets/` directory
- Missing minification hurts performance
- No cache busting without fingerprinting
- Large unoptimized images slow page loads

### Anti-Pattern 3: Not Cleaning Build Directory

**Problem**: Old files remain in `public/` from previous builds.

**Bad Example:**

```bash
# Building without cleanup
hugo
# Old deleted content still in public/
```

**Solution:**

```bash
# Clean build with garbage collection
hugo --gc --minify

# Or manually clean first
rm -rf public/
hugo --gc --minify
```

**Rationale:**

- Deploys deleted content
- Deploys old drafts
- Inconsistent build artifacts
- Wastes bandwidth and storage

### Anti-Pattern 4: Hardcoded URLs

**Problem**: Using hardcoded URLs breaks in different environments.

**Bad Example:**

```html
<!-- Hardcoded URL -->
<a href="https://mysite.com/blog/post-1/">Read more</a>
```

**Solution:**

```html
<!-- Use Hugo's URL functions -->
{{ $page := .Site.GetPage "/blog/post-1" }}
<a href="{{ $page.Permalink }}">Read more</a>

<!-- Or use relref -->
<a href="{{ ref . "/blog/post-1" }}">Read more</a>
```

**Rationale:**

- Breaks on localhost
- Breaks on staging environments
- Can't change domain easily
- Hugo generates correct URLs automatically

### Anti-Pattern 5: Skipping Image Optimization

**Problem**: Using original large images without processing.

**Bad Example:**

```html
<!-- Original 5MB image -->
<img src="/images/photo.jpg" alt="Photo" />
```

**Solution:**

```html
<!-- Resized and optimized -->
{{ $image := resources.Get "images/photo.jpg" }} {{ $resized := $image.Resize "800x webp q85" }}
<img src="{{ $resized.RelPermalink }}" alt="Photo" loading="lazy" />
```

**Rationale:**

- Huge file sizes slow page loads
- Wastes bandwidth (especially mobile)
- Poor Lighthouse performance scores
- Hugo can process images faster than other SSGs

### Anti-Pattern 6: No Fingerprinting in Production

**Problem**: Assets without fingerprinting cause cache issues.

**Bad Example:**

```html
<!-- No cache busting -->
<link rel="stylesheet" href="/css/main.css" />
```

**Solution:**

```html
{{ $css := resources.Get "css/main.css" | resources.PostCSS }} {{ if hugo.IsProduction }} {{ $css = $css | minify |
fingerprint }} {{ end }} <link rel="stylesheet" href="{{ $css.RelPermalink }}" />
```

**Rationale:**

- Users see cached old CSS after updates
- No automatic cache invalidation
- Can't use long cache times safely
- Fingerprinting solves this automatically

### Anti-Pattern 7: Same Build for All Environments

**Problem**: Using same build settings for dev and production.

**Bad Example:**

```bash
# Same build everywhere
hugo
```

**Solution:**

```bash
# Development
hugo server --environment development

# Production
hugo --gc --minify --environment production
```

**Rationale:**

- Development needs drafts and fast builds
- Production needs optimization and no drafts
- Can accidentally deploy drafts
- Different environments have different needs

### Anti-Pattern 8: Ignoring Build Warnings

**Problem**: Warnings indicate real issues that cause production problems.

**Bad Example:**

```bash
hugo
# WARN: template not found
# WARN: missing parameter
# ... ignoring warnings ...
```

**Solution:**

```bash
# Check warnings explicitly
hugo --printPathWarnings --printUnusedTemplates

# Fail build on warnings in CI
hugo --logLevel warn
```

**Rationale:**

- Warnings become errors eventually
- Indicates broken templates or links
- Better to fix during development
- Prevents production surprises

### Anti-Pattern 9: No Lazy Loading for Images

**Problem**: All images load immediately, slowing page load.

**Bad Example:**

```html
<!-- All images load immediately -->
<img src="{{ $image.RelPermalink }}" alt="Photo" />
```

**Solution:**

```html
<!-- Lazy load images below fold -->
<img src="{{ $image.RelPermalink }}" alt="Photo" loading="lazy" />
```

**Rationale:**

- Slows initial page load
- Wastes bandwidth for off-screen images
- Poor Largest Contentful Paint (LCP)
- Browser-native lazy loading is free

### Anti-Pattern 10: Creating Single-Use Shortcodes

**Problem**: Creating shortcodes for one-off layouts.

**Bad Example:**

```html
<!-- layouts/shortcodes/specific-blog-post-header.html -->
<!-- Only used once in one blog post -->
<div class="special-header-only-for-post-123">
  <!-- Complex one-off layout -->
</div>
```

**Solution:**

```markdown
<!-- Use HTML directly in markdown for one-offs -->
<div class="special-header">
  <!-- One-off layout directly in content -->
</div>
```

**Rationale:**

- Adds complexity for no reusability
- Harder to find one-off shortcodes
- Shortcodes are for reusable components
- Direct HTML is simpler for one-offs

## Summary of Anti-Patterns

| Anti-Pattern              | Problem                        | Solution                           |
| ------------------------- | ------------------------------ | ---------------------------------- |
| **Editing Theme Files**   | Breaks updates                 | Override in project layouts/       |
| **static/ for Assets**    | No processing                  | Use assets/ for processed files    |
| **Not Cleaning public/**  | Deploys old content            | Use --gc flag                      |
| **Hardcoded URLs**        | Breaks environments            | Use .Permalink or ref              |
| **No Image Optimization** | Slow page loads                | Use Hugo image processing          |
| **No Fingerprinting**     | Cache issues                   | Fingerprint assets in production   |
| **Same Build Everywhere** | Wrong settings per environment | Environment-specific configuration |
| **Ignoring Warnings**     | Future errors                  | Fix warnings immediately           |
| **No Lazy Loading**       | Slow initial load              | Use loading="lazy"                 |
| **Single-Use Shortcodes** | Unnecessary complexity         | Use HTML directly for one-offs     |

## Related Documentation

- [Hugo Development Convention](./development.md) - Complete Hugo development standards
- [Best Practices](./best-practices.md) - Recommended patterns
- [Accessibility First Principle](../../principles/content/accessibility-first.md) - Why accessibility matters
- [Simplicity Over Complexity Principle](../../principles/general/simplicity-over-complexity.md) - Why simple is better

## Conclusion

Avoiding these anti-patterns ensures:

- Maintainable theme customizations
- Optimized asset pipeline
- Clean build artifacts
- Environment-appropriate builds
- Fast page load times
- Cache-friendly deployments
- Clear, focused codebase
- Production-ready sites

When developing Hugo sites, ask: **Am I adding performance or complexity?** If complexity, refactor to follow Hugo development best practices.

## Principles Applied

- **Accessibility First**: Image optimization, lazy loading
- **Explicit Over Implicit**: Clear URLs, validated parameters
- **Simplicity Over Complexity**: Direct HTML for one-offs, minimal shortcodes
- **Automation Over Manual**: Asset processing, fingerprinting, optimization
