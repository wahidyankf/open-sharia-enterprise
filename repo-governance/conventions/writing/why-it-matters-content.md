---
title: "Why It Matters Content Convention"
description: Rule prohibiting corporate case studies and fabricated platform scenarios in Why It Matters sections of ayokoding-web tutorials; requires theoretical explanations only
category: explanation
subcategory: conventions
tags:
  - ayokoding-web
  - tutorial-content
  - factual-accuracy
  - why-it-matters
  - hallucination-prevention
created: 2026-05-09
---

# Why It Matters Content Convention

This convention defines the content rules for `**Why It Matters**:` sections in
ayokoding-web tutorials. These sections must use theoretical explanations only.
Corporate case studies, anecdotal company events, and fabricated platform scenarios
are prohibited regardless of how plausible they appear.

## Principles Implemented/Respected

This convention implements the following core principles:

- **[Deliberate Problem-Solving](../../principles/general/deliberate-problem-solving.md)**:
  AI-generated corporate anecdotes invite readers to accept unverifiable claims as fact.
  Deliberate content creation requires surfacing uncertainty rather than papering over it
  with invented evidence. Theoretical explanations make their epistemic status transparent.

- **[Explicit Over Implicit](../../principles/software-engineering/explicit-over-implicit.md)**:
  The boundary between verifiable fact and theoretical reasoning must be explicit in every
  tutorial section. This convention makes that boundary a hard structural rule rather than
  a judgment call left to individual authors or AI agents.

- **[Root Cause Orientation](../../principles/general/root-cause-orientation.md)**:
  Fabricated corporate case studies are the root cause of accuracy debt in educational
  content. Patching individual hallucinated claims after the fact is the wrong fix;
  prohibiting the pattern at authoring time eliminates the root cause.

- **[Simplicity Over Complexity](../../principles/general/simplicity-over-complexity.md)**:
  Theoretical explanations convey the same pedagogical value as corporate case studies
  without the verification complexity. Simpler content with lower accuracy risk is
  preferable to elaborate anecdotes that require ongoing fact-checking.

## Purpose

`**Why It Matters**:` sections exist to help readers understand the practical importance
of each concept before they invest time learning it. This pedagogical value is fully
achievable through theoretical reasoning about system properties, trade-offs, and
consequences — no corporate anecdote is required.

The problem this convention solves: AI agents writing tutorial content frequently invent
specific corporate case studies (e.g., "When LinkedIn migrated from Oracle to MySQL...",
"Netflix adopted this pattern because...", "A ride-sharing platform integrated with...")
that appear credible but are hallucinated. These claims:

- Cannot be verified without a citable primary source
- Create accuracy debt that is expensive to audit and fix
- Erode reader trust when discovered to be fabricated
- Require ongoing re-verification as content is updated

This convention eliminates the problem at the source by prohibiting the pattern entirely.

## Scope

### What This Convention Covers

- All `**Why It Matters**:` sections in ayokoding-web tutorial files
- Applies to both by-example tutorials (`apps/ayokoding-web/content/en/learn/**/by-example/`)
- Applies to in-the-field guides (`apps/ayokoding-web/content/en/learn/**/in-the-field/`)
- Applies to all future tutorial formats that include a Why It Matters section
- Applies equally to English and Indonesian content

### What This Convention Does NOT Cover

- The overall structure of tutorial files — see the relevant tutorial format conventions
- Factual validation of technical code examples — see [Factual Validation Convention](./factual-validation.md)
- Other tutorial sections (Introduction, Code Example, Explanation, etc.)
- Content in `docs/`, `plans/`, or convention documents themselves

## Standards

### Standard 1: Theoretical Explanations Only

Every `**Why It Matters**:` section MUST rely solely on:

- Statements about the concept or pattern as a general category
- Capability claims: what the pattern enables, prevents, or guarantees
- Trade-off discussions: what is gained and what is sacrificed
- References to verifiable technical facts — library names, algorithm complexity,
  language specifications, or industry-standard definitions with citable sources

### Standard 2: Prohibited Content Patterns

The following patterns are **never** permitted inside a `**Why It Matters**:` section:

**Prohibited: Named-company anecdotes**

Any sentence that names a specific company (Netflix, Amazon, LinkedIn, Uber, Google,
Stripe, Shopify, Coinbase, PayPal, Facebook, Twitter, Apple, Microsoft, etc.) in
connection with an internal engineering decision, migration, or metric.

**Prohibited: "When [Company] did X" structure**

Sentences following the pattern: _"When [Company] implemented / migrated / adopted /
refactored X, they achieved / reduced / improved Y."_

**Prohibited: Fabricated platform scenarios**

Sentences that replace a company name with a generic category to preserve the anecdotal
structure: _"A ride-sharing platform...", "A large e-commerce company...", "A fintech
startup..."_ These patterns fabricate a scenario to simulate evidence without providing
any.

**Prohibited: Unsourced numeric claims**

Specific percentages, counts, cost savings, or performance ratios attributed to a
real or implied organization without a citable primary source (engineering blog post,
academic paper, official announcement, or conference talk with a URL).

### Standard 3: Suspension Test

Before writing any sentence containing a company name or a specific metric, ask:
**"Can I link to the primary source right now?"**

- If yes: include the inline link and write the claim. The claim is now verifiable.
- If no: rewrite using the underlying principle without the company name and metric.

This test applies to both human authors and AI content agents.

### Standard 4: Permitted Reference Patterns

The following reference patterns are permitted because they are verifiable:

| Pattern                             | Example                                                                                                           |
| ----------------------------------- | ----------------------------------------------------------------------------------------------------------------- |
| Citable historical fact with source | "Tony Hoare called null references his 'billion-dollar mistake' (QCon 2009)"                                      |
| Named tool with documented behavior | "Netflix's Hystrix popularized circuit breakers; it is now in maintenance mode"                                   |
| Well-known community-verified event | "Twitter's Finagle directly influenced the creation of Linkerd (now a CNCF project)"                              |
| Academic or specification reference | "Dijkstra's seminal 1968 letter, 'Go To Statement Considered Harmful'"                                            |
| Mathematical property statement     | "N states × M variants without parameterization = N×M total states"                                               |
| General pattern consequence         | "Without explicit state machine enforcement, nothing prevents an order from being marked 'shipped' before 'paid'" |

## Examples

### Before and After: Removing a Fabricated Corporate Case Study

**FAIL: Prohibited (fabricated corporate case study)**

```markdown
**Why It Matters**: When Shopify refactored order processing from anemic to rich
domain models, they reduced order-related bugs by 73%. Business rules previously
scattered across 15 service classes were consolidated into domain objects, making
the codebase dramatically easier to maintain.
```

Problems with this example:

- Names a real company (Shopify) without a citable source
- Provides specific metrics (73%, 15 service classes) that appear precise but are invented
- No engineering blog post, paper, or conference talk supports this claim

**PASS: Required (theoretical explanation)**

```markdown
**Why It Matters**: Anemic domain models scatter business rules across service classes
rather than encapsulating them in domain objects. This creates three compounding problems:
business rules become invisible to domain experts, they duplicate silently across services
as the system grows, and they are difficult to test in isolation. Martin Fowler identified
the Anemic Domain Model as an anti-pattern in 2003, noting that it violates object-oriented
principles by separating data from the behavior that operates on it. When domain objects
own their invariants, the same logic that makes an order invalid in a unit test is the
same logic that runs in production — there is no gap for bugs to enter.
```

This rewrite conveys the same pedagogical value — readers understand why the pattern
matters — without fabricating evidence.

---

### Before and After: Replacing a Generic Platform Scenario

**FAIL: Prohibited (fabricated platform scenario)**

```markdown
**Why It Matters**: A ride-sharing platform integrated this pattern and saw their
service recovery time drop from 45 seconds to under 3 seconds during peak load.
The pattern prevents cascading failures from taking down the entire system.
```

Problems:

- "A ride-sharing platform" is a fabricated stand-in for a company name
- "45 seconds to under 3 seconds" is a specific metric with no source
- The final sentence is the actual valuable insight; the anecdote adds nothing

**PASS: Required (theoretical explanation)**

```markdown
**Why It Matters**: Without circuit breaker isolation, a slow or failing downstream
service holds connections open until they time out. Under load, new requests queue
behind the blocked ones, exhausting the thread pool and bringing the calling service
down as well. The pattern prevents this cascade by failing fast — returning an
immediate error rather than waiting — which keeps the rest of the system operational.
Recovery happens through periodic probing rather than waiting for the operator to
intervene.
```

---

### Example: Verifiable Fact Used Correctly

**PASS: Permitted (citable event with named source)**

```markdown
**Why It Matters**: Unit mismatches between subsystems can have catastrophic
consequences even in mission-critical engineering. NASA's Mars Climate Orbiter
($327M total mission cost) was lost in 1999 because one engineering team used
pound-force seconds while another used newton-seconds — a mismatch that went
undetected until the spacecraft entered the wrong orbit. Strong typing that
encodes units at the type level makes this class of error a compile-time
failure rather than a runtime disaster.
```

This is permitted because the NASA event is documented in the official accident
investigation report, and the fact is citable.

## Tools and Automation

- **`apps-ayokoding-web-by-example-checker`** — Validates by-example tutorial content,
  including scanning `**Why It Matters**:` sections for prohibited patterns
- **`apps-ayokoding-web-in-the-field-checker`** — Validates in-the-field tutorial content
  using the same Why It Matters rules
- **`apps-ayokoding-web-by-example-fixer`** — Applies fixes to by-example tutorial content,
  rewriting prohibited Why It Matters patterns as theoretical explanations
- **`apps-ayokoding-web-in-the-field-fixer`** — Applies fixes to in-the-field tutorial content

## References

**Related Conventions:**

- [Factual Validation Convention](./factual-validation.md) — Universal methodology for
  verifying factual correctness; the Fabricated Corporate Case Study Rule section in that
  document provides the foundational detection and fix patterns that this convention
  specializes for Why It Matters sections
- [Content Quality Principles](./quality.md) — Universal markdown quality standards
  (active voice, heading hierarchy, accessibility) that apply alongside this convention

**Related Principles:**

- [Deliberate Problem-Solving](../../principles/general/deliberate-problem-solving.md)
- [Root Cause Orientation](../../principles/general/root-cause-orientation.md)
- [Explicit Over Implicit](../../principles/software-engineering/explicit-over-implicit.md)
- [Simplicity Over Complexity](../../principles/general/simplicity-over-complexity.md)

**Agents:**

- `apps-ayokoding-web-by-example-maker` — Creates by-example tutorials; must follow this convention
- `apps-ayokoding-web-in-the-field-maker` — Creates in-the-field guides; must follow this convention
- `apps-ayokoding-web-by-example-checker` — Validates Why It Matters sections in by-example tutorials
- `apps-ayokoding-web-in-the-field-checker` — Validates Why It Matters sections in in-the-field guides
- `apps-ayokoding-web-by-example-fixer` — Fixes prohibited patterns in by-example tutorials
- `apps-ayokoding-web-in-the-field-fixer` — Fixes prohibited patterns in in-the-field guides
