---
title: "Overview"
date: 2026-02-03T00:00:00+07:00
draft: false
description: "Overview of Java LTS releases from the last 5 years (2021-2025)"
weight: 1000000
tags: ["java", "lts", "strategy", "overview"]
---

## LTS Release Strategy

Oracle's Long-Term Support (LTS) strategy provides predictable, stable Java versions for enterprise applications. This section covers the most recent LTS releases from the **last 5 years** (2021-2025).

## Covered LTS Releases

This section highlights three major LTS releases:

- **[Java 17 LTS](/en/learn/software-engineering/programming-languages/java/release-highlights/java-17)** (September 2021) - Sealed classes, pattern matching preview, enhanced PRNG
- **[Java 21 LTS](/en/learn/software-engineering/programming-languages/java/release-highlights/java-21)** (September 2023) - Virtual threads, sequenced collections, pattern matching finalized
- **[Java 25 LTS](/en/learn/software-engineering/programming-languages/java/release-highlights/java-25)** (September 2025) - Stream gatherers, compact headers, performance optimizations

## LTS Cadence

Oracle delivers LTS releases every **2 years** (reduced from 3 years in 2023):

```mermaid
%%{init: {'theme':'base', 'themeVariables': { 'primaryColor':'#0173B2','primaryTextColor':'#fff','primaryBorderColor':'#0173B2','lineColor':'#DE8F05','secondaryColor':'#029E73','tertiaryColor':'#CC78BC','fontSize':'16px'}}}%%
timeline
    title Java LTS Release Timeline
    2018 : Java 11 LTS : First post-Java 8 LTS
    2021 : Java 17 LTS : 3-year gap
    2023 : Java 21 LTS : 2-year cadence begins
    2025 : Java 25 LTS : 2-year cadence
    2027 : Java 27 LTS (expected) : 2-year cadence
```

## Why LTS Matters

**Long-Term Support** provides:

- **8+ years** of security updates and bug fixes
- **Stability** for production applications
- **Predictable** upgrade cycles
- **Enterprise-ready** releases with extensive testing
- **Ecosystem support** from frameworks, libraries, and tools

## Feature Evolution

Key features evolve across LTS releases:

| Feature                   | Java 17 (2021) | Java 21 (2023) | Java 25 (2025) |
| ------------------------- | -------------- | -------------- | -------------- |
| **Sealed Classes**        | ✅ Finalized   | ✅ Available   | ✅ Available   |
| **Pattern Matching**      | 🔬 Preview     | ✅ Finalized   | ✅ Enhanced    |
| **Virtual Threads**       | ❌ None        | ✅ Finalized   | ✅ Optimized   |
| **Sequenced Collections** | ❌ None        | ✅ Finalized   | ✅ Available   |
| **Stream Gatherers**      | ❌ None        | ❌ None        | ✅ Finalized   |
| **Compact Headers**       | ❌ None        | ❌ None        | ✅ Finalized   |

Legend:

- ✅ **Finalized** - Production-ready
- 🔬 **Preview** - Feature complete, requires `--enable-preview`
- ❌ **None** - Not available

## Choosing Your LTS Version

**For new projects in 2026:**

- **Start with Java 21** - Virtual threads essential for modern concurrency
- **Consider Java 25** - If cutting-edge performance and latest features needed

**For existing applications:**

- **Java 8/11 → Java 17** - Solid foundation, mature ecosystem
- **Java 17 → Java 21** - Virtual threads unlock massive concurrency
- **Java 21 → Java 25** - Performance optimization (20-40% gains)

## Support Timeline

| Version | Release Date | Premier Support Until | Extended Support Until |
| ------- | ------------ | --------------------- | ---------------------- |
| Java 17 | Sep 2021     | Sep 2026              | Sep 2029               |
| Java 21 | Sep 2023     | Sep 2028              | Sep 2031               |
| Java 25 | Sep 2025     | Sep 2030              | Sep 2033               |

**Note:** Oracle provides 8 years of support (5 years premier + 3 years extended).

## Indonesian Translation Status

Indonesian (`/id/`) translations for release highlights are **pending**. English versions provide complete coverage of all three LTS releases.

## Next Steps

Explore each LTS release:

1. **[Java 17 Highlights](/en/learn/software-engineering/programming-languages/java/release-highlights/java-17)** - Foundation features (sealed classes, pattern matching preview)
2. **[Java 21 Highlights](/en/learn/software-engineering/programming-languages/java/release-highlights/java-21)** - Concurrency revolution (virtual threads)
3. **[Java 25 Highlights](/en/learn/software-engineering/programming-languages/java/release-highlights/java-25)** - Performance optimization (compact headers, stream gatherers)
