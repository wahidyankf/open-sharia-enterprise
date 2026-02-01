---
title: "Overview"
date: 2025-02-01T22:30:00+07:00
draft: false
weight: 100000000
description: "Learn Playwright through 85 annotated code examples covering 95% of the framework - ideal for experienced testers and developers switching from Selenium or Cypress"
tags: ["playwright", "tutorial", "by-example", "examples", "testing", "automation"]
---

**Want to quickly master Playwright through working examples?** This by-example guide teaches 95% of Playwright through 85 annotated code examples organized by complexity level.

## What Is By-Example Learning?

By-example learning is an **example-first approach** where you learn through annotated, runnable code rather than narrative explanations. Each example is self-contained, immediately executable with `npx playwright test`, and heavily commented to show:

- **What each line does** - Inline comments explain the purpose and mechanism
- **Expected behaviors** - Using `// =>` notation to show test outcomes
- **Intermediate states** - Element states and page interactions made visible
- **Key takeaways** - 1-2 sentence summaries of core concepts

This approach is **ideal for experienced developers and testers** (manual testers, Selenium/Cypress users, or software engineers) who are familiar with web testing concepts and want to quickly understand Playwright's API, patterns, and unique features through working code.

Unlike narrative tutorials that build understanding through explanation and storytelling, by-example learning lets you **see the code first, run it second, and understand it through direct interaction**. You learn by doing, not by reading about doing.

## Learning Path

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    A["Beginner<br/>Examples 1-30<br/>Core Fundamentals"] --> B["Intermediate<br/>Examples 31-60<br/>Forms & Advanced Testing"]
    B --> C["Advanced<br/>Examples 61-85<br/>Production Patterns"]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#fff
    style C fill:#029E73,color:#fff
```

Progress from fundamentals through practical form testing to advanced production patterns. Each level builds on the previous, increasing in sophistication and introducing more Playwright-specific features.

## Coverage Philosophy

This by-example guide provides **95% coverage of Playwright** through practical, annotated examples. The 95% figure represents the depth and breadth of concepts covered, not a time estimate—focus is on **outcomes and understanding**, not duration.

### What's Covered

- **Core fundamentals** - Browser launch, navigation, basic locators, assertions
- **Selectors and locators** - All locator types, chaining, filtering, advanced patterns
- **Basic interactions** - Clicks, input, checkboxes, dropdowns, file uploads, keyboard/mouse
- **Form handling** - Multi-field forms, validation, dynamic forms, date pickers, rich editors
- **Advanced assertions** - URL, attributes, screenshots, accessibility, network, custom matchers
- **API testing** - API requests, authentication, mocking, fixtures, UI+API integration
- **Test organization** - Page Object Model, fixtures, hooks, annotations, retries
- **Advanced patterns** - Component objects, parameterized tests, visual regression, network interception
- **Debugging and diagnostics** - Trace viewer, debug mode, video recording, HAR files, console logs
- **CI/CD and performance** - Parallel execution, sharding, GitHub Actions, Docker, performance testing
- **Production patterns** - Authentication flows, test data management, environment config, error handling, reporting

### What's NOT Covered

This guide focuses on **learning-oriented examples**, not problem-solving recipes or production deployment. For additional topics:

- **Deep framework integrations** - Specific CI/CD platforms (beyond GitHub Actions), cloud testing services
- **Legacy browser support** - Internet Explorer, older browser versions
- **Mobile native testing** - Native mobile apps (Playwright is web/hybrid app focused)

The 95% coverage goal maintains humility—no tutorial can cover everything. This guide teaches the **core concepts that unlock the remaining 5%** through your own exploration and project work.

## How to Use This Guide

1. **Sequential or selective** - Read examples in order for progressive learning, or jump to specific topics when switching from Selenium/Cypress
2. **Run everything** - Execute examples with `npx playwright test` to see results yourself. Experimentation solidifies understanding.
3. **Modify and explore** - Change selectors, add assertions, break tests intentionally. Learn through experimentation.
4. **Use as reference** - Bookmark examples for quick lookups when you forget syntax or patterns
5. **Complement with narrative tutorials** - By-example learning is code-first; pair with comprehensive tutorials for deeper explanations

**Best workflow**: Open your editor in one window, this guide in another, terminal in a third. Run each example as you read it. When you encounter something unfamiliar, run the example, modify it, see what changes.

## Relationship to Other Tutorials

Understanding where by-example fits in the tutorial ecosystem helps you choose the right learning path:

| Tutorial Type    | Coverage                | Approach                       | Target Audience                | When to Use                                            |
| ---------------- | ----------------------- | ------------------------------ | ------------------------------ | ------------------------------------------------------ |
| **By Example**   | 95% through 85 examples | Code-first, annotated examples | Experienced developers/testers | Quick framework pickup, reference, tool switching      |
| **Quick Start**  | 5-30% touchpoints       | Hands-on first test            | Newcomers to Playwright        | First taste, decide if worth learning                  |
| **Beginner**     | 0-60% comprehensive     | Narrative, explanatory         | Complete testing beginners     | Deep understanding, first automation framework         |
| **Intermediate** | 60-85%                  | Practical applications         | Past basics                    | Production patterns, CI/CD integration                 |
| **Advanced**     | 85-95%                  | Complex systems                | Experienced Playwright users   | Custom reporters, advanced fixtures, browser internals |
| **Cookbook**     | Problem-specific        | Recipe-based                   | All levels                     | Solve specific testing problems                        |

**By Example vs. Quick Start**: By Example provides 95% coverage through examples vs. Quick Start's 5-30% through your first test. By Example is code-first reference; Quick Start is hands-on introduction.

**By Example vs. Beginner Tutorial**: By Example is code-first for experienced developers/testers; Beginner Tutorial is narrative-first for complete testing beginners. By Example shows patterns; Beginner Tutorial explains concepts.

**By Example vs. Cookbook**: By Example is learning-oriented (understand concepts); Cookbook is problem-solving oriented (fix specific issues). By Example teaches patterns; Cookbook provides solutions.

## Prerequisites

**Required**:

- Experience with web development or testing concepts
- Ability to run Node.js commands and npm/npx
- Basic understanding of HTML, CSS, and JavaScript

**Recommended (helpful but not required)**:

- Familiarity with async/await in JavaScript
- Experience with another testing framework (Selenium, Cypress, Puppeteer)
- Understanding of web browser DevTools

**No prior Playwright experience required** - This guide assumes you're new to Playwright but experienced with web testing or development in general. You should be comfortable reading JavaScript/TypeScript code, understanding basic testing concepts (assertions, test structure), and learning through hands-on experimentation.

## Structure of Each Example

Every example follows this consistent format:

````markdown
### Example N: Concept Name

Brief explanation of the concept in 2-3 sentences. Explains **what** the concept is and **why** it matters.

[OPTIONAL: Mermaid diagram when concept relationships need visualization]

**Code**:

```typescript
import { test, expect } from "@playwright/test";

test("example test", async ({ page }) => {
  // Inline comment for each significant line
  await page.goto("https://example.com"); // => Navigates to URL

  const button = page.getByRole("button", { name: "Submit" }); // => Locates button
  await button.click(); // => Clicks button, triggers action

  await expect(page.getByText("Success")).toBeVisible(); // => Asserts success message
});
```

**Key Takeaway**: 1-2 sentence summary highlighting the most important insight or pattern from this example.
````

The **brief explanation** provides context. The **code** is heavily annotated with inline comments and `// =>` output notation. The **key takeaway** distills the concept to its essence.

Mermaid diagrams appear when **visual representation clarifies concept relationships** - showing test flow, page object hierarchies, or fixture composition. Not every example needs a diagram; they're used strategically to enhance understanding.

## Learning Strategies

### For Selenium Users

You're used to explicit waits and WebDriver API. Playwright simplifies and modernizes testing:

- **Auto-waiting**: No explicit waits for element visibility, Playwright waits automatically
- **Modern selectors**: getByRole, getByText replace fragile CSS/XPath selectors
- **Built-in best practices**: Automatic screenshots, traces, and videos on failure

Focus on Examples 1-10 (browser basics and locators) and Examples 21-30 (auto-waiting) to unlearn explicit wait patterns.

### For Cypress Users

You understand modern web testing and fixtures. Playwright provides more control:

- **Multi-browser support**: Test Chrome, Firefox, and WebKit (Safari) with same code
- **True parallelization**: Run tests in parallel across browsers and files
- **API testing**: Built-in API request capabilities without plugins

Focus on Examples 51-55 (API testing) and Examples 76-80 (parallel execution) to leverage Playwright's unique strengths.

### For Manual Testers

You know what to test but may be new to automation. Playwright makes automation accessible:

- **Readable selectors**: getByRole('button'), getByText('Submit') read like English
- **Code generation**: Use Codegen tool to record tests while clicking through app
- **Debugging tools**: Trace viewer, debug mode show exactly what happened

Focus on Examples 1-20 (fundamentals and locators) and Examples 71-75 (debugging) to build automation confidence.

### For JavaScript/TypeScript Developers

You understand async/await and modern JavaScript. Playwright is native async testing:

- **Async-first design**: All actions return promises, use await consistently
- **TypeScript support**: Full type safety for page objects and fixtures
- **Fixture pattern**: Dependency injection for test setup and teardown

Focus on Examples 56-60 (fixtures and hooks) and Examples 61-70 (advanced patterns) to leverage your development skills.

### For Puppeteer Users

You're familiar with Chrome DevTools Protocol. Playwright builds on similar foundations:

- **Multi-browser**: Same API works across Chrome, Firefox, and WebKit
- **Testing focus**: Built-in assertions, fixtures, and test runner (no need for external frameworks)
- **Better debugging**: Trace viewer, codegen, inspector tools

Focus on Examples 41-50 (assertions) and Examples 71-75 (debugging) to see Playwright's testing-specific features.

## Code-First Philosophy

This tutorial prioritizes working code over theoretical discussion:

- **No lengthy prose**: Concepts are demonstrated, not explained at length
- **Runnable examples**: Every example runs with `npx playwright test`
- **Learn by doing**: Understanding comes from running and modifying code
- **Pattern recognition**: See the same patterns in different contexts across 85 examples

If you prefer narrative explanations, complement this guide with comprehensive tutorials. By-example learning works best when you learn through experimentation.

## Ready to Start?

Jump into the beginner examples to start learning Playwright through code:

- [Beginner Examples (1-30)](/en/learn/software-engineering/automation-testing/tools/playwright/by-example/beginner) - Core fundamentals, selectors, basic interactions
- [Intermediate Examples (31-60)](/en/learn/software-engineering/automation-testing/tools/playwright/by-example/intermediate) - Form handling, advanced assertions, API testing, test organization
- [Advanced Examples (61-85)](/en/learn/software-engineering/automation-testing/tools/playwright/by-example/advanced) - Advanced patterns, debugging, CI/CD, production patterns

Each example is self-contained and runnable. Start with Example 1, or jump to topics that interest you most.
