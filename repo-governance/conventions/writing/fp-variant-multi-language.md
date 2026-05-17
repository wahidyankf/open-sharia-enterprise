---
title: "FP-Variant Multi-Language Convention"
description: Bidirectional idiomatic-language rule requiring F# AND Clojure tabs in FP-variant by-example tutorials in ayokoding-web, with each language kept idiomatically native rather than mechanically translated from the other
category: explanation
subcategory: conventions
tags:
  - fp
  - clojure
  - fsharp
  - by-example
  - ayokoding-web
  - tutorial
created: 2026-05-17
---

# FP-Variant Multi-Language Convention

FP-variant by-example tutorials in ayokoding-web teach functional programming concepts through architecture examples. Without a normative language rule, authors may present only one language, mechanically translate idioms across languages, or present non-idiomatic code that misleads learners. This convention establishes the bidirectional idiomatic-language rule: every FP-variant by-example page presents code in F# AND Clojure using Hugo tabs, and each language stays idiomatic to its own community and runtime rather than being forced into the shape of the other.

## Principles Implemented/Respected

This convention implements the following core principles:

- **[Explicit Over Implicit](../../principles/software-engineering/explicit-over-implicit.md)**: The two-language requirement and the Hugo tabs format are stated explicitly — not left to author preference. The list of idiomatic patterns for each language, the annotation density requirement, and the cross-paradigm annotation rule are all stated as numbered standards rather than vague guidance.

- **[Simplicity Over Complexity](../../principles/general/simplicity-over-complexity.md)**: When a paradigm-specific concept has no direct counterpart in the other language, the rule requires the closest native equivalent plus a short annotation — not an elaborate simulation. The simplest truthful representation takes precedence over architectural symmetry for its own sake.

- **[Accessibility First](../../principles/content/accessibility-first.md)**: Non-idiomatic code in either language forces learners to decode foreign patterns on top of the architecture concept being taught. Idiomatic code in each language lowers the cognitive barrier for readers coming from that community.

- **[Documentation First](../../principles/content/documentation-first.md)**: Cross-paradigm annotations are mandatory, not optional. When a concept is F#-native or Clojure-native and is expressed via the closest equivalent in the other language, the annotation documenting the trade-off is a required part of the example — it is documentation embedded in the code, not an afterthought.

## Purpose

This convention exists to:

- Mandate two-language coverage (F# and Clojure) in every FP-variant by-example page so learners see both typed-FP and dynamic-FP perspectives on the same architecture concept.
- Prevent mechanical cross-language translation that produces non-idiomatic code and misleads learners about each language's natural design style.
- Establish a closed-form rule for handling concepts that exist natively in only one language (closest native equivalent plus annotation, not forced translation).
- Maintain the annotation density standard required by the by-example tutorial convention, applied per-language within each tab.

## Scope

### What This Convention Covers

- All FP-variant by-example tutorial files in `apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/*/in-fp-by-example/` — specifically `beginner.md`, `intermediate.md`, and `advanced.md` level pages.
- Overview pages (`overview.md`) under those paths, for any code snippets they contain.
- Both English and Indonesian variants of those files when they exist.

### What This Convention Does NOT Cover

- **OOP-variant by-example tutorials** (`in-oop-by-example/`) — those have separate language scope rules.
- **In-the-field guides** (`in-the-field/`) — real-world framework and library choices override theoretical multi-language presentation.
- **By-concept tutorials** (`by-concept/`) — narrative explanations follow their own structure convention.
- **Other languages** (Java, TypeScript, Go, Python, etc.) — governed by their respective tutorial conventions. This convention is specific to the FP variant's F# + Clojure requirement.
- **docs/explanation/ documents** — repository explanation docs follow the Diátaxis explanation standard, not this tutorial-content convention.
- **Clojure or F# language tutorials** themselves — the programming-language tutorial structure convention governs standalone language tutorials.

## Standards

### S1: Hugo Tabs Format — F# First, Clojure Second

Every code block in an FP-variant by-example page MUST use Hugo tabs with F# as the first tab and Clojure as the second tab.

The outer shortcode structure is:

```text
{{< tabs items="F#,Clojure" >}}

{{< tab >}}
[F# code block here]
{{< /tab >}}

{{< tab >}}
[Clojure code block here]
{{< /tab >}}

{{< /tabs >}}
```

Each `[language code block here]` placeholder is a fenced code block with the appropriate language identifier (`fsharp` or `clojure`).

The tab label strings are exactly `F#` and `Clojure` (case-sensitive, in that order). Do not reorder the tabs, do not use alternate labels such as `fsharp` or `clj`.

Standalone code blocks that appear outside example sections (for example, in an intro paragraph) are exempt from the tabs requirement when they are not demonstrating the example concept itself.

### S2: F# Idiomatic Patterns

F# code in these tutorials MUST remain idiomatic to the F# community and runtime. The following patterns are expected to appear at the appropriate complexity level and MUST NOT be replaced by Clojure-influenced equivalents:

- **Discriminated unions (DUs)** — model domain variants as `type Result<'T, 'E> = Ok of 'T | Error of 'E`, sum types for state machines, and tagged union domain concepts.
- **Record types** — immutable named-field types with `{ FieldName: Type }` syntax and `with` update syntax.
- **Smart constructors** — private constructors plus a public `create` function returning `Result` to enforce invariants at the boundary.
- **`Result<'T, 'E>` and computation expressions** — `result { ... }` or `asyncResult { ... }` computation expressions for railway-oriented programming; `Result.bind`, `Result.map` combinators.
- **`Async<'T>` and async computation expressions** — `async { ... }` blocks for asynchronous workflows; `Async.RunSynchronously`, `Async.StartAsTask`.
- **Units of measure** — `[<Measure>]` type attributes and `float<kg>`, `decimal<USD>` annotations where numeric domain types have physical or monetary units.
- **`|>` pipelines and partial application** — left-to-right composition with the pipe operator; curried function application for dependency injection and workflow construction.
- **Module-level functions** — top-level `let` bindings rather than method-on-class patterns; modules as namespaces.
- **Pattern matching** — `match` expressions for case analysis, `function` keyword for single-argument match, active patterns for reusable decomposition.
- **Interfaces and object expressions** — when required by platform APIs, expressed as `{ new IInterface with member _.Method() = ... }` rather than class definitions.

F# code that introduces class hierarchies, mutable state, or imperative loops solely to mirror a Clojure pattern is non-compliant with this standard.

### S3: Clojure Idiomatic Patterns

Clojure code in these tutorials MUST remain idiomatic to the Clojure community and runtime. The following patterns are expected to appear at the appropriate complexity level and MUST NOT be replaced by F#-influenced equivalents:

- **Maps with namespaced keywords** — domain entities as plain maps using `::entity/field` namespaced keywords; `assoc`, `dissoc`, `update`, `merge` for structural transformation.
- **Specs or malli schemas for validation** — `clojure.spec.alpha` `s/def` + `s/valid?` + `s/explain`, or `malli` schema definitions, to enforce domain invariants.
- **Sequences and transducers** — `map`, `filter`, `reduce`, `into`, `transduce` over lazy sequences; transducer composition with `comp` for efficient pipeline processing.
- **Multimethods** — `defmulti` + `defmethod` for open dispatch on arbitrary criteria; the idiomatic Clojure approach to polymorphism that does not require a type hierarchy.
- **Threading macros** — `->` (thread-first) and `->>` (thread-last) for left-to-right data transformation pipelines; `as->` for non-uniform threading.
- **Atoms, refs, and agents** — `atom` for uncoordinated synchronous state; `ref` + `dosync` for coordinated transactional state; `agent` for asynchronous independent state.
- **Protocols** — `defprotocol` + `extend-protocol` or `reify` for polymorphic dispatch over existing types; the idiomatic Clojure alternative to interfaces.
- **REPL-friendly data orientation** — functions that accept and return plain data (maps, vectors, sets, lists); no hidden class coupling; results that print legibly in a REPL session.
- **Namespace organisation** — `ns` declarations with `:require` aliases; `defn` at the top level; namespace-qualified symbols for public APIs.
- **`core.async` channels** — `go` blocks, `chan`, `<!`, `>!` for asynchronous workflows when concurrency is part of the example concept.

Clojure code that simulates discriminated unions as tagged maps (`{:tag :ok :value x}`) is acceptable only when the example specifically teaches that pattern; it MUST include an annotation noting this is the Clojure data-orientation approach (see S5).

### S4: Bidirectional Idiomatic Rule

Neither language MUST force non-idiomatic patterns from the other. This rule applies in both directions:

- **F# MUST NOT** adopt Clojure-style dynamic dispatch, untyped maps, or runtime-tag dispatch solely to mirror the Clojure tab's structure.
- **Clojure MUST NOT** adopt F#-style record simulations (defrecord used purely as a value-semantics clone) or rigid type-hierarchy patterns that conflict with Clojure's data-orientation philosophy.

The idiomatic divergence between tabs is intentional and educational. Authors MUST NOT normalise or suppress the difference. Where tabs diverge structurally (for example, F# uses a computation expression and Clojure uses a threading macro), both structures MUST be retained as written; the cross-paradigm annotation in S5 explains the divergence.

### S5: Cross-Paradigm Concept Handling

When a paradigm-specific concept exists natively in only one language, use the closest native equivalent in the other language and add an annotation explaining the trade-off. Do NOT mechanically translate the construct.

**Direction rules:**

- If the concept is F#-native (discriminated unions, computation expressions, units of measure), the Clojure tab uses the closest Clojure native equivalent (multimethods, tagged maps, malli spec, threading macros) with a comment annotation.
- If the concept is Clojure-native (multimethods, REPL session state, spec hierarchies), the F# tab uses the closest F# native equivalent (DUs + pattern matching, interfaces, discriminated union hierarchies) with a comment annotation.

**Annotation format for cross-paradigm divergence:**

In F# tabs, use `// [Clojure: <equivalent> — <one-sentence trade-off note>]`:

```fsharp
// [Clojure: tagged map {:tag :ok :value x} — data-first; no compile-time exhaustiveness check]
type Outcome<'T, 'E> = Ok of 'T | Err of 'E
```

In Clojure tabs, use `; [F#: <equivalent> — <one-sentence trade-off note>]`:

```clojure
; [F#: discriminated union — compiler-enforced exhaustiveness; Clojure uses open dispatch via multimethods]
(defmulti handle-outcome :status)
```

The annotation MUST appear on the line immediately preceding the construct it annotates, or as an inline comment on the same line when the construct is a single line.

### S6: Annotation Density

Every code block in both tabs MUST meet the annotation density standard defined in the [By-Example Tutorial Convention](../tutorials/by-example.md): **1.0–2.25 comment lines per code line, measured per individual example** (not tutorial-wide). This ratio applies independently to the F# tab and the Clojure tab.

Lines that are blank, closing braces, or closing brackets do not count as code lines. Comment lines beginning with `//` (F#) or `;` (Clojure) count as comment lines. Inline comments on a code line count as 0.5 comment lines toward the ratio.

When a cross-paradigm annotation (S5) is added, it counts toward the comment lines of the tab it appears in.

## Examples

### PASS: Idiomatic F# and Clojure side-by-side — discriminated union vs. multimethod

This example teaches domain-state dispatch. F# uses a discriminated union with pattern matching (its native mechanism). Clojure uses a multimethod (its native mechanism). Each is explained with a cross-paradigm annotation.

````markdown
{{< tabs items="F#,Clojure" >}}

{{< tab >}}

```fsharp
// Domain state for a purchase requisition lifecycle
// [Clojure: multimethod dispatch on :status key — open extension without exhaustiveness]
type RequisitionStatus =
    // => Five variants cover the full procurement lifecycle
    | Draft                          // => Initial state after creation
    | PendingApproval                // => Submitted, awaiting manager sign-off
    | Approved of approvedBy: string // => Carries approver identity on transition
    | Rejected of reason: string     // => Carries rejection reason for audit trail
    | Closed                         // => Terminal state; no further transitions

// Pattern match is exhaustive — compiler rejects missing cases
let describeStatus status =
    match status with
    | Draft           -> "awaiting submission"        // => Human-readable label
    | PendingApproval -> "pending manager approval"
    | Approved name   -> $"approved by {name}"        // => Interpolates approver name
    | Rejected reason -> $"rejected: {reason}"
    | Closed          -> "closed"
```

{{< /tab >}}

{{< tab >}}

```clojure
;; Domain state for a purchase requisition lifecycle
;; [F#: discriminated union — compiler-enforced exhaustiveness; missing cases are compile errors]
(defmulti describe-status
  ;; Dispatch on the :status key of the requisition map
  :status)
;; => defmulti defines the dispatch function; defmethod adds each variant

(defmethod describe-status :draft [_]
  "awaiting submission")
;; => _ ignores the full map; only the dispatch value matters here

(defmethod describe-status :pending-approval [_]
  "pending manager approval")

(defmethod describe-status :approved [{:keys [approved-by]}]
  ;; Destructure the map to extract the approver identity
  (str "approved by " approved-by))
;; => approved-by comes from the map, not a tagged payload position

(defmethod describe-status :rejected [{:keys [reason]}]
  (str "rejected: " reason))

(defmethod describe-status :closed [_]
  "closed")
;; => Clojure multimethods are open: a new :status variant requires only a new defmethod
;; => F# DUs are closed: adding a variant requires updating all match sites
```

{{< /tab >}}

{{< /tabs >}}
````

**Why this is compliant:**

- F# uses DU + pattern matching — native, exhaustive, compile-time safe.
- Clojure uses defmulti/defmethod — native open dispatch, REPL-friendly, data-oriented.
- Each tab has a cross-paradigm annotation noting the key trade-off.
- Both tabs meet the 1.0–2.25 comment-to-code ratio independently.

### FAIL: Clojure tab mechanically simulates F# DU

````markdown
{{< tab >}}

```clojure
;; BAD: simulating F# discriminated union with a defrecord hierarchy
(defrecord Draft [])
(defrecord PendingApproval [])
(defrecord Approved [approved-by])
(defrecord Rejected [reason])
(defrecord Closed [])
```

{{< /tab >}}
````

**Why this fails:** `defrecord` used purely to clone an F# DU shape is non-idiomatic Clojure. Idiomatic Clojure represents domain states as maps with a `:status` keyword and uses multimethods or cond for dispatch. The code above forces F#'s closed-type model onto Clojure, which misleads learners about Clojure design style.

### FAIL: F# tab suppresses DU to mirror Clojure map approach

````markdown
{{< tab >}}

```fsharp
// BAD: simulating Clojure tagged maps in F#
let draft = Map.ofList [("tag", box "draft")]
let approved = Map.ofList [("tag", box "approved"); ("approvedBy", box "Alice")]
```

{{< /tab >}}
````

**Why this fails:** Using `Map` to simulate tagged data in F# loses type safety, exhaustiveness checking, and all the benefits of the type system. This forces Clojure's data-orientation philosophy onto F# in a way that is both non-idiomatic and educationally misleading.

## Rationale

**Why two languages?**

The FP-variant by-example tutorials teach architecture concepts — DDD, hexagonal architecture, FSM, and similar patterns — through a functional programming lens. F# and Clojure represent two distinct traditions within functional programming: typed-FP with rich compile-time guarantees (F#) and dynamic-FP with REPL-driven data orientation (Clojure). Showing the same architecture concept in both languages reveals how design decisions shift across these traditions, producing deeper understanding than a single-language presentation can achieve.

**Why the bidirectional idiomatic constraint?**

Mechanical translation produces code that no experienced practitioner in either community would write. A Clojure developer reading F#-flavoured Clojure code learns incorrect idioms. An F# developer reading Clojure-flavoured F# code learns patterns that fight the type system. In both cases the learner exits the tutorial with habits that the respective community would flag in code review. The bidirectional constraint protects the educational integrity of both language tracks.

**Why closest native equivalent plus annotation rather than forced translation?**

Some concepts are paradigm-specific, not merely language-specific. F# discriminated unions exist because the compiler can prove exhaustiveness at compile time — there is no meaningful Clojure equivalent that preserves that property. Pretending otherwise by constructing an elaborate simulation teaches the wrong lesson. The closest native equivalent plus an annotation teaches two true things simultaneously: how Clojure actually solves this class of problem, and what is lost or gained relative to the F# approach. That is richer and more honest than either forced symmetry or silent omission.

## Validation

The following checks determine whether an FP-variant by-example page complies with this convention:

1. **Tabs format**: Does every code example use `{{< tabs items="F#,Clojure" >}}` with F# as the first tab?
2. **F# idiomatic patterns**: Does the F# tab use DUs, records, smart constructors, `Result`, computation expressions, `|>` pipelines, and pattern matching at the appropriate level? Does it avoid untyped maps or runtime dispatch as primary patterns?
3. **Clojure idiomatic patterns**: Does the Clojure tab use maps with namespaced keywords, multimethods or protocols, threading macros, and spec/malli at the appropriate level? Does it avoid forced record or class simulations as primary patterns?
4. **Bidirectional rule**: Is idiomatic divergence between tabs retained rather than normalised?
5. **Cross-paradigm annotations**: When a concept is language-specific, does the other tab include a `// [Clojure: ...]` or `; [F#: ...]` annotation?
6. **Annotation density**: Does each tab's code block meet the 1.0–2.25 comment-to-code ratio?

`apps-ayokoding-web-by-example-checker` enforces checks 1, 5, and 6. Checks 2, 3, and 4 require AI semantic judgement and are part of the checker's content audit pass.

## Tools and Automation

- **`apps-ayokoding-web-by-example-maker`** — creates FP-variant by-example content; responsible for applying this convention when generating or updating F# + Clojure tabs.
- **`apps-ayokoding-web-by-example-checker`** — validates tabs format, annotation density, and cross-paradigm annotation presence.
- **`apps-ayokoding-web-by-example-fixer`** — applies fixes to non-compliant pages (adds missing tabs, adds missing annotations, adjusts annotation density).

## References

**Related Conventions:**

- [By-Example Tutorial Convention](../tutorials/by-example.md) — primary authority for five-part example structure, annotation density (1.0–2.25 ratio), and coverage progression. This convention is a specialisation of that standard for FP-variant multi-language content.
- [Content Quality Principles](./quality.md) — universal markdown quality standards (active voice, heading nesting, accessibility) that apply to all content including FP-variant by-example pages.
- [Why It Matters Content Convention](./why-it-matters-content.md) — prohibits fabricated scenarios and unsourced claims in `**Why It Matters**:` sections; applies to both tabs in FP-variant examples.
- [Programming Language Content Standard](../tutorials/programming-language-content.md) — Full Set Tutorial Package architecture; FP-variant by-example is Component 3 (code-first priority track).
- [Programming Language Documentation Separation](../structure/programming-language-docs-separation.md) — scope boundary between ayokoding-web tutorial content and docs/explanation/ language reference material.

**Agents:**

- [`apps-ayokoding-web-by-example-maker`](../../../.claude/agents/apps-ayokoding-web-by-example-maker.md) — creates FP-variant by-example content following this convention
- [`apps-ayokoding-web-by-example-checker`](../../../.claude/agents/apps-ayokoding-web-by-example-checker.md) — validates compliance
- [`apps-ayokoding-web-by-example-fixer`](../../../.claude/agents/apps-ayokoding-web-by-example-fixer.md) — fixes violations

**In-FP-by-example overview pages:**

- [Architecture by-example: FP overview](../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/overview.md)
- [DDD: FP by-example overview](../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/overview.md)
- [Hexagonal Architecture: FP by-example overview](../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/hexagonal-architecture/in-fp-by-example/overview.md)
- [FSM: FP by-example overview](../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/overview.md)

**Repository Architecture:**

- [Repository Governance Architecture](../../repository-governance-architecture.md) — six-layer hierarchy. This convention is Layer 2 (Conventions), governing Layer 4 agents (`apps-ayokoding-web-by-example-*`) consumed at runtime by Layer 5 workflows (ayokoding-web by-example quality gate).
- [Diátaxis Framework](../structure/diataxis-framework.md) — FP-variant by-example tutorials are the Tutorial quadrant of Diátaxis (learning-oriented, hands-on, step-by-step).
