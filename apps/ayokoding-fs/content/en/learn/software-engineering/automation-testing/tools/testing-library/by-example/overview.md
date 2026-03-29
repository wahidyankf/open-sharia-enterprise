---
title: "Overview"
date: 2026-03-25T00:00:00+07:00
draft: false
weight: 10000000
description: "Learn Testing Library through 80 annotated code examples covering queries, user events, async testing, accessibility patterns, and production testing strategies - ideal for experienced developers switching to component testing"
tags: ["testing-library", "react", "testing", "accessibility", "tutorial", "by-example"]
---

**Want to quickly master Testing Library through working examples?** This by-example guide teaches 95% of Testing Library (React Testing Library focus) through 80 annotated code examples organized by complexity level.

## What Is By-Example Learning?

By-example learning is an **example-first approach** where you learn through annotated, runnable code rather than narrative explanations. Each example is self-contained, runnable with your test runner (Jest/Vitest), and heavily commented to show:

- **What each line does** - Inline comments explain the purpose and mechanism
- **Expected behaviors** - Using `// =>` notation to show test outcomes
- **Intermediate states** - DOM states and query results made visible
- **Key takeaways** - 1-2 sentence summaries of core concepts

This approach is **ideal for experienced developers** (React developers, developers familiar with Enzyme, or software engineers new to Testing Library) who want to quickly understand Testing Library's API, accessibility-first philosophy, and unique patterns through working code.

Unlike narrative tutorials that build understanding through explanation and storytelling, by-example learning lets you **see the code first, run it second, and understand it through direct interaction**. You learn by doing, not by reading about doing.

## Learning Path

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    A["Beginner<br/>Examples 1-28<br/>Core Fundamentals"] --> B["Intermediate<br/>Examples 29-55<br/>Production Patterns"]
    B --> C["Advanced<br/>Examples 56-80<br/>Expert Mastery"]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#fff
    style C fill:#029E73,color:#fff
```

Progress from fundamentals through practical async testing to advanced production patterns. Each level builds on the previous, increasing in sophistication and introducing more Testing Library-specific idioms.

## Coverage Philosophy

This by-example guide provides **95% coverage of Testing Library** through practical, annotated examples. The 95% figure represents the depth and breadth of concepts covered—focus is on **outcomes and understanding**, not duration.

### What's Covered

- **Core queries** - getBy*, queryBy*, findBy\* with all query types (role, text, label, placeholder, alt, testId)
- **User events** - userEvent setup, click, type, keyboard, mouse, clipboard interactions
- **Async testing** - waitFor, waitForElementToBeRemoved, findBy\* queries, async user actions
- **Custom renders** - Wrapping with providers (context, Redux, Router, theme)
- **Hook testing** - renderHook, act, custom hook patterns
- **Form testing** - Controlled inputs, validation, submission, reset
- **Accessibility** - Accessible queries, jest-axe integration, ARIA patterns
- **Advanced patterns** - MSW for API mocking, within() scoping, custom queries
- **Context and state** - React Context, Redux, Zustand store testing
- **Error handling** - Error boundaries, async errors, retry patterns
- **Production patterns** - Custom render utilities, test data builders, large-scale strategies

### What's NOT Covered

This guide focuses on **learning-oriented examples**, not problem-solving recipes or production deployment:

- **Framework-specific setup** - Vite/CRA/Next.js configuration differences beyond basics
- **Non-React frameworks** - Vue Testing Library, Angular Testing Library (different libraries)
- **Native mobile testing** - React Native Testing Library (separate package)

The 95% coverage goal maintains humility—no tutorial can cover everything. This guide teaches the **core concepts that unlock the remaining 5%** through your own exploration and project work.

## How to Use This Guide

1. **Sequential or selective** - Read examples in order for progressive learning, or jump to specific topics when you need a specific pattern
2. **Run everything** - Execute examples with `npx jest` or `npx vitest` to see results yourself. Experimentation solidifies understanding.
3. **Modify and explore** - Change queries, add assertions, break tests intentionally. Learn through experimentation.
4. **Use as reference** - Bookmark examples for quick lookups when you forget syntax or patterns
5. **Complement with narrative tutorials** - By-example learning is code-first; pair with comprehensive tutorials for deeper explanations

**Best workflow**: Open your editor in one window, this guide in another, terminal in a third. Run each example as you read it. When you encounter something unfamiliar, run the example, modify it, see what changes.

## Relationship to Other Tutorials

Understanding where by-example fits in the tutorial ecosystem helps you choose the right learning path:

| Tutorial Type    | Coverage                | Approach                       | Target Audience                   | When to Use                                 |
| ---------------- | ----------------------- | ------------------------------ | --------------------------------- | ------------------------------------------- |
| **By Example**   | 95% through 80 examples | Code-first, annotated examples | Experienced developers            | Quick Testing Library pickup, reference     |
| **Quick Start**  | 5-30% touchpoints       | Hands-on first test            | Newcomers to Testing Library      | First taste, decide if worth learning       |
| **Beginner**     | 0-60% comprehensive     | Narrative, explanatory         | Complete testing beginners        | Deep understanding, first component testing |
| **Intermediate** | 60-85%                  | Practical applications         | Past basics                       | Production patterns, async testing          |
| **Advanced**     | 85-95%                  | Complex systems                | Experienced Testing Library users | Custom queries, scale patterns              |
| **Cookbook**     | Problem-specific        | Recipe-based                   | All levels                        | Solve specific component testing problems   |

**By Example vs. Enzyme**: Testing Library's philosophy is "test behavior, not implementation." Enzyme's `wrapper.state()` and `wrapper.instance()` test internals; Testing Library's `getByRole()` tests what users experience. By Example teaches this philosophy through code, not lectures.

**By Example vs. React Testing Docs**: Official docs explain what APIs do; By Example shows how to combine them in real patterns with annotation density that builds intuition.

## Prerequisites

**Required**:

- Experience with React components (functional components, hooks)
- Ability to run Node.js commands and npm/npx
- Basic understanding of HTML and DOM structure
- Familiarity with Jest or Vitest test runner basics

**Recommended (helpful but not required)**:

- Familiarity with async/await in JavaScript/TypeScript
- Experience with another testing approach (Enzyme, manual testing)
- Understanding of accessibility concepts (ARIA roles, labels)

**No prior Testing Library experience required** - This guide assumes you're new to Testing Library but experienced with React development. You should be comfortable reading TypeScript/JavaScript code, understanding basic testing concepts (assertions, test structure), and learning through hands-on experimentation.

## Structure of Each Example

Every example follows a **mandatory five-part format**:

````markdown
### Example N: Concept Name

**Part 1: Brief Explanation** (2-3 sentences)
Explains what the concept is, why it matters in component testing, and when to use it.

**Part 2: Mermaid Diagram** (when appropriate)
Visual representation of concept relationships - query selection flow, render lifecycle, or async wait behavior. Not every example needs a diagram; they're used strategically to enhance understanding.

**Part 3: Heavily Annotated Code**

```typescript
import { render, screen } from "@testing-library/react";
// => Imports core Testing Library utilities
// => render: mounts component into jsdom
// => screen: global query object bound to document.body

import userEvent from "@testing-library/user-event";
// => userEvent: simulates real browser interactions
// => More realistic than fireEvent (dispatches full event sequences)

test("button click changes text", async () => {
  const user = userEvent.setup();
  // => Creates userEvent instance with shared state
  // => Enables clipboard, pointer state across interactions

  render(<Counter />);
  // => Mounts Counter component into jsdom
  // => Makes DOM available via screen queries

  await user.click(screen.getByRole("button", { name: "Increment" }));
  // => Finds button by accessible role and name
  // => Simulates full click sequence (pointerdown, mousedown, click, pointerup, mouseup)

  expect(screen.getByText("Count: 1")).toBeInTheDocument();
  // => Asserts text is present in document
  // => jest-dom matcher from @testing-library/jest-dom
});
```

**Part 4: Key Takeaway** (1-2 sentences)
Distills the core insight: the most important pattern, when to apply it in production, or common pitfalls to avoid.

**Part 5: Why It Matters** (2-3 sentences, 50-100 words)
Connects the concept to production relevance - why professionals care, how it compares to alternatives, and consequences for quality/performance/maintainability.
````

Each example follows this structure consistently, maintaining annotation density of 1.0-2.25 comment lines per code line. The **brief explanation** provides context, the **code** is heavily annotated with inline comments and `// =>` output notation, the **key takeaway** distills the concept, and **why it matters** shows production relevance.

## Learning Strategies

### For Enzyme Users

You're used to `shallow()`, `wrapper.find()`, and implementation testing. Testing Library teaches a different philosophy:

- **Accessibility-first queries**: `getByRole('button')` instead of `find('button')`
- **No implementation access**: No `wrapper.state()`, `wrapper.instance()`, or prop inspection
- **Behavior testing**: Test what users see and do, not how components are implemented

Focus on Examples 1-10 (queries and screen object) and Examples 17-20 (text matching) to build the accessibility-first query intuition.

### For Jest/Vanilla DOM Users

You understand testing frameworks but may be new to component testing:

- **jsdom integration**: Tests run in Node.js but with real DOM APIs
- **React lifecycle**: `render()` handles mounting, updates, and cleanup automatically
- **Async awareness**: React state updates require async handling (see Examples 29-35)

Focus on Examples 1-5 (render basics) and Examples 29-35 (async testing) to understand React's async nature.

### For React Developers New to Testing

You build React apps but haven't tested them deeply:

- **Accessible markup**: Testing Library rewards semantic HTML (roles, labels, headings)
- **User perspective**: Write tests from the user's point of view, not the developer's
- **Confidence over coverage**: One behavior test beats ten implementation tests

Focus on Examples 1-15 (core queries) and Examples 21-28 (user events) to build testing intuition fast.

### For TypeScript Developers

You appreciate type safety and want typed test utilities:

- **Typed queries**: All query functions are fully typed with element types
- **Component props**: Type your render helpers and test data builders
- **Generic patterns**: `renderHook<T>()` and custom query types

Focus on Examples 29-40 (custom render and providers) and Examples 56-65 (advanced patterns) to leverage TypeScript in tests.

## Core Philosophy

Testing Library exists to encourage writing tests that resemble how users use your software. This means:

- **Query by what users see**: text, role, label, placeholder—not `data-testid` as a first resort
- **Test behavior, not implementation**: Does it work? Not: is the state correct?
- **Accessibility is the path**: Semantic HTML that Testing Library can query is accessible HTML
- **User events over fireEvent**: Real interaction sequences reveal real bugs

This philosophy shows in every example. By the end of this guide, accessible-first testing will be your default instinct.

## Ready to Start?

Jump into the beginner examples to start learning Testing Library through code:

- [Beginner Examples (1-28)](/en/learn/software-engineering/automation-testing/tools/testing-library/by-example/beginner) - Core queries, screen object, user events, text matching, form basics
- [Intermediate Examples (29-55)](/en/learn/software-engineering/automation-testing/tools/testing-library/by-example/intermediate) - Async testing, custom render, hooks, context, Redux, accessibility
- [Advanced Examples (56-80)](/en/learn/software-engineering/automation-testing/tools/testing-library/by-example/advanced) - MSW, drag-and-drop, virtualized lists, i18n, custom queries, scale patterns

Each example is self-contained and runnable. Start with Example 1, or jump to topics that interest you most.
