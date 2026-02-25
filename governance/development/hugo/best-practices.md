# Best Practices for Hugo Development

> **Companion Document**: For common mistakes to avoid, see [Anti-Patterns](./anti-patterns.md)

## Overview

This document outlines best practices for developing Hugo sites in the `apps/ayokoding-web/` and `apps/oseplatform-web/` projects. Following these practices ensures maintainable, performant, and accessible Hugo implementations.

## Purpose

Provide actionable guidance for:

- Theme development and customization
- Asset pipeline optimization
- Performance and accessibility
- Build configuration
- Testing and deployment

## Best Practices

### Practice 1: Always Use Hugo Extended Version

**Principle**: Use hugo-extended for SCSS/SASS support.

**Good Example:**

```bash
# Verify extended version
hugo version
# Output: hugo v0.156.0+extended linux/amd64
```

**Bad Example:**

```bash
# Using standard version (no SCSS support)
hugo version
# Output: hugo v0.156.0 linux/amd64  # Missing "+extended"
```

**Rationale:**

- Hugo extended includes SCSS/SASS processing
- Required for modern theme development
- Both Hextra and PaperMod may need SCSS compilation
- Prevents build errors with theme customizations

### Practice 2: Override Theme Files, Never Edit Directly

**Principle**: Copy theme files to project `layouts/` instead of editing theme directly.

**Good Example:**

```bash
# Copy theme file to project for customization
cp themes/hextra/layouts/_default/baseof.html layouts/_default/baseof.html
# Now edit layouts/_default/baseof.html safely
```

**Bad Example:**

```bash
# Editing theme file directly
vim themes/hextra/layouts/_default/baseof.html  # BREAKS UPDATES
```

**Rationale:**

- Preserves theme update path
- Clear separation of customizations
- Hugo checks project `layouts/` first
- Easier to track changes in git

### Practice 3: Process Assets with Hugo Pipes

**Principle**: Use `assets/` directory for processed files, `static/` for unprocessed files.

**Good Example:**

```
assets/               # Files processed by Hugo Pipes
├── css/main.css      # PostCSS processing, minification
├── js/app.js         # Bundling, minification
└── images/hero.jpg   # Resizing, WebP conversion

static/               # Served as-is (no processing)
├── fonts/            # Font files
├── favicon.ico       # Favicon
└── robots.txt        # Static text file
```

**Bad Example:**

```
static/
├── css/main.css      # NO processing (no minification!)
├── js/app.js         # NO bundling
└── images/large.jpg  # NO optimization (huge file!)
```

**Rationale:**

- Hugo Pipes only processes `assets/` directory
- Automatic minification, fingerprinting, optimization
- Better performance and caching
- Efficient build pipeline

### Practice 4: Commit resources/\_gen to Git

**Principle**: Commit processed image cache to speed up builds.

**Good Example:**

```bash
# Include resources cache in git
git add resources/_gen
git commit -m "chore: add Hugo resources cache"
```

**Bad Example:**

```gitignore
# .gitignore - DO NOT DO THIS
/resources/_gen/images  # SLOWS BUILDS
```

**Rationale:**

- Pre-processed images speed up CI/CD builds
- Can reduce build time from 6 minutes to 1 minute
- Especially important for large sites
- One-time processing, shared across environments

### Practice 5: Use Fingerprinting in Production

**Principle**: Enable asset fingerprinting for cache busting.

**Good Example:**

```html
{{ $js := resources.Get "js/main.js" | js.Build }} {{ if hugo.IsProduction }} {{ $js = $js | minify | fingerprint }} {{
end }}
<!-- Generates: /js/main.a1b2c3d4.min.js -->
<script src="{{ $js.RelPermalink }}"></script>
```

**Bad Example:**

```html
<!-- Hardcoded path, no cache busting -->
<link rel="stylesheet" href="/css/main.css" />
```

**Rationale:**

- Unique filenames based on content hash
- Long cache times without stale content
- Automatic cache invalidation on changes
- Standard best practice for production

### Practice 6: Always Resize and Optimize Images

**Principle**: Process all images with Hugo's image pipeline.

**Good Example:**

```html
{{ $image := resources.Get "images/photo.jpg" }} {{ $resized := $image.Resize "800x webp q85" }}
<img src="{{ $resized.RelPermalink }}" alt="Photo" loading="lazy" />
```

**Bad Example:**

```html
<!-- Using original large image -->
<img src="/images/photo.jpg" alt="Photo" />
```

**Rationale:**

- Reduces image size by 60-80%
- Faster page load times
- WebP format support
- Lazy loading improves performance

### Practice 7: Validate Shortcode Parameters

**Principle**: Check required parameters and provide clear errors.

**Good Example:**

```html
<!-- layouts/shortcodes/image.html -->
{{ $src := .Get "src" }} {{ if not $src }} {{ errorf "image shortcode missing required 'src' parameter in %s" .Position
}} {{ end }} {{ $alt := .Get "alt" | default "Image" }}
<img src="{{ $src }}" alt="{{ $alt }}" />
```

**Bad Example:**

```html
<!-- No validation -->
<img src="{{ .Get "src" }}" alt="{{ .Get "alt" }}" />
<!-- Fails silently if parameters missing -->
```

**Rationale:**

- Clear error messages during development
- Prevents silent failures
- Better debugging experience
- Documents required parameters

### Practice 8: Use Environment-Specific Configuration

**Principle**: Different settings for development vs production.

**Good Example:**

```bash
# Development (drafts, verbose)
hugo server --environment development

# Production (no drafts, minified)
hugo --gc --minify --environment production
```

**Bad Example:**

```bash
# Same build for all environments
hugo
```

**Rationale:**

- Development needs drafts and fast builds
- Production needs optimization and no drafts
- Clear separation of concerns
- Prevents deployment mistakes

### Practice 9: Clean Build Directory Before Production

**Principle**: Use `--gc` flag to clean old files.

**Good Example:**

```bash
# Clean build with garbage collection
hugo --gc --minify
```

**Bad Example:**

```bash
# No cleanup (old files remain)
hugo
```

**Rationale:**

- Removes deleted content from output
- Prevents deploying old drafts
- Consistent build artifacts
- Smaller deployment size

### Practice 10: Test Builds Before Deployment

**Principle**: Run full production build and verify locally.

**Good Example:**

```bash
# Test production build
hugo --gc --minify --environment production
cd public/
python3 -m http.server 8000
# Visit http://localhost:8000 and verify
```

**Bad Example:**

```bash
# Deploy without testing
git push origin prod-ayokoding-web  # HOPE IT WORKS
```

**Rationale:**

- Catches build errors before deployment
- Verifies content renders correctly
- Tests production-specific settings
- Reduces deployment failures

## Related Documentation

- [Hugo Development Convention](./development.md) - Complete Hugo development standards
- [Anti-Patterns](./anti-patterns.md) - Common mistakes to avoid
- [Accessibility First Principle](../../principles/content/accessibility-first.md) - Why accessibility matters
- [Color Accessibility Convention](../../conventions/formatting/color-accessibility.md) - Accessible color palette

## Summary

Following these best practices ensures:

1. Use Hugo extended version for SCSS support
2. Override theme files without editing directly
3. Process assets with Hugo Pipes
4. Commit resources cache for faster builds
5. Fingerprint assets in production
6. Resize and optimize all images
7. Validate shortcode parameters
8. Use environment-specific configuration
9. Clean build directory before production
10. Test builds before deployment

Hugo sites built following these practices are maintainable, performant, accessible, and production-ready.

## Principles Implemented/Respected

- **Accessibility First**: Image processing, semantic HTML, WCAG compliance
- **Explicit Over Implicit**: Clear configuration, validated parameters
- **Reproducibility First**: Environment-specific builds, resource caching
- **Automation Over Manual**: Asset processing, fingerprinting, optimization

## Conventions Implemented/Respected

- **[Color Accessibility Convention](../../conventions/formatting/color-accessibility.md)**: WCAG AA color compliance in Hugo themes
- **[Content Quality Principles](../../conventions/writing/quality.md)**: Active voice, clear headings in documentation
- **[File Naming Convention](../../conventions/structure/file-naming.md)**: Hugo content files follow naming conventions
