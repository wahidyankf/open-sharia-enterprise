---
title: "Implementation Workflow"
description: Three-stage development workflow - make it work, make it right, make it fast
category: explanation
subcategory: development
tags:
  - development
  - workflow
  - implementation
  - optimization
  - refactoring
created: 2025-12-15
updated: 2025-12-15
---

# Implementation Workflow

**Make it work, make it right, make it fast** - a three-stage development workflow that prioritizes functionality first, quality second, and optimization last (only when proven necessary).

## 🎯 What

The implementation workflow follows three sequential stages:

1. **Make it work** - Get functionality working with the simplest solution
2. **Make it right** - Refactor for readability, maintainability, and clean code
3. **Make it fast** - Optimize performance ONLY if proven necessary by measurements

**Key principle**: Each stage is complete before moving to the next. Don't skip stages or combine them.

## 💡 Why

## Principles Respected

This workflow respects three core principles:

- **[Simplicity Over Complexity](../../principles/general/simplicity-over-complexity.md)** - Start with the simplest solution that works
- **[YAGNI (You Aren't Gonna Need It)](../../principles/general/simplicity-over-complexity.md#yagni-principles)** - Don't optimize prematurely
- **[Progressive Disclosure](../../principles/content/progressive-disclosure.md)** - Layer refinement gradually

## Conventions Implemented/Respected

**REQUIRED SECTION**: All development practice documents MUST include this section to ensure traceability from practices to documentation standards.

This practice implements/respects the following conventions:

- **[Code Quality Convention](../quality/code.md)**: The "make it right" stage applies code quality standards (Prettier formatting, linting) before the "make it fast" stage to ensure clean code before optimization.

- **[Content Quality Principles](../conventions/content/quality.md)**: Implementation workflow follows the same progressive layering philosophy - start simple (work), add structure and clarity (right), then refine performance (fast).

### Benefits of This Workflow

1. **Faster to Working Software**: Focus on functionality first gets you to a working state quickly
2. **Prevents Over-Engineering**: Avoid building unnecessary abstractions or optimizations
3. **Clearer Thinking**: Separating concerns (work vs right vs fast) reduces cognitive load
4. **Data-Driven Optimization**: Only optimize based on actual measurements, not guesses
5. **Better Code Quality**: Clean code before optimization prevents optimization of bad code

### Problems with Premature Optimization

1. **Wasted Effort**: Optimizing code that doesn't need to be fast
2. **Complex Code**: Optimized code is often harder to understand and maintain
3. **Wrong Optimizations**: Optimizing the wrong parts (not the bottleneck)
4. **Delayed Delivery**: Time spent optimizing instead of delivering features
5. **Technical Debt**: Rushing quality to add optimization creates maintainability issues

### The Famous Quote

> "Premature optimization is the root of all evil (or at least most of it) in programming."
> — Donald Knuth, "The Art of Computer Programming"

## 📋 How It Applies

### Stage 1: Make It Work

**Goal**: Get functionality working with the simplest possible solution.

**What to do**:

- Write the most straightforward code that solves the problem
- Don't worry about performance, elegance, or abstractions yet
- Focus on passing tests and meeting requirements
- Hard-code values if it helps you move faster
- Copy-paste code if it gets you to working faster

**What NOT to do**:

- ❌ Don't create abstractions or design patterns yet
- ❌ Don't optimize for performance
- ❌ Don't worry about code duplication
- ❌ Don't refactor while implementing

**Example**:

```typescript
// ✅ MAKE IT WORK - Simple, straightforward implementation
function calculateOrderTotal(items: any[]) {
  let total = 0;
  for (let i = 0; i < items.length; i++) {
    total = total + items[i].price * items[i].quantity;
  }
  return total;
}

// ❌ DON'T DO THIS YET - Premature abstraction
class OrderCalculator {
  private strategy: PricingStrategy;
  constructor(strategy: PricingStrategy) {
    this.strategy = strategy;
  }
  calculate(items: OrderItem[]): Money {
    return this.strategy.computeTotal(items);
  }
}
```

**When you're done**: Functionality works, tests pass, requirements met.

### Stage 2: Make It Right

**Goal**: Refactor code for readability, maintainability, and clean code principles.

**What to do**:

- Extract repeated code into functions (Rule of Three)
- Use meaningful variable and function names
- Apply clean code principles (small functions, single responsibility)
- Add proper error handling
- Improve type safety
- Write comprehensive tests
- Add documentation where needed

**What NOT to do**:

- ❌ Don't optimize for performance yet
- ❌ Don't add features not in requirements
- ❌ Don't change functionality (keep tests green)

**Example**:

```typescript
// ✅ MAKE IT RIGHT - Clean, readable, maintainable
interface OrderItem {
  price: number;
  quantity: number;
}

function calculateOrderTotal(items: OrderItem[]): number {
  return items.reduce((total, item) => total + calculateItemTotal(item), 0);
}

function calculateItemTotal(item: OrderItem): number {
  return item.price * item.quantity;
}

// Tests remain green - functionality unchanged
```

**When you're done**: Code is clean, readable, well-tested, maintainable.

### Stage 3: Make It Fast (If Needed)

**Goal**: Optimize performance ONLY if measurements show it's necessary.

**Critical requirement**: **MEASURE FIRST**. Never optimize without profiling.

**What to do**:

1. **Profile the code** - Use profiling tools to find actual bottlenecks
2. **Measure baseline** - Record current performance metrics
3. **Identify bottleneck** - Find the slowest part (often 10% of code = 90% of time)
4. **Optimize bottleneck** - Apply targeted optimizations
5. **Measure improvement** - Verify optimization actually helped
6. **Keep tests green** - Ensure functionality didn't break

**What NOT to do**:

- ❌ Don't optimize without profiling data
- ❌ Don't optimize everything - only bottlenecks
- ❌ Don't sacrifice readability unless necessary
- ❌ Don't guess which parts are slow

**Example**:

```typescript
// Stage 2: Clean code
function calculateOrderTotal(items: OrderItem[]): number {
  return items.reduce((total, item) => total + calculateItemTotal(item), 0);
}

// Stage 3: Optimize ONLY if profiling shows this is a bottleneck
// AND measurements show significant performance impact
function calculateOrderTotalOptimized(items: OrderItem[]): number {
  // Optimized version with memoization, caching, or algorithmic improvement
  // ONLY if measurements prove it's needed
  const cached = orderCache.get(items);
  if (cached) return cached;

  const total = items.reduce((sum, item) => sum + item.price * item.quantity, 0);
  orderCache.set(items, total);
  return total;
}

// Document WHY optimization was needed:
// Profiling showed 80% of checkout time spent in calculateOrderTotal
// Baseline: 1000ms for 10,000 items
// After optimization: 50ms for 10,000 items (20x improvement)
```

**When you're done**: Performance meets requirements, code still clean, optimizations justified by data.

## 🚫 Anti-Patterns

### Premature Optimization

❌ **Problem**: Optimizing before making it work or right.

```typescript
// ❌ Stage 1: DON'T DO THIS - Premature optimization
function calculateOrderTotal(items: OrderItem[]): number {
  // Trying to optimize in Stage 1 (Make It Work)
  const cache = new WeakMap();
  const memoized = items.map((item) => {
    if (cache.has(item)) return cache.get(item);
    const result = item.price * item.quantity;
    cache.set(item, result);
    return result;
  });
  return memoized.reduce((a, b) => a + b, 0);
}
```

**Why it's bad**: Code is complex before it even works. Optimization might be in wrong place.

### Skipping "Make It Right"

❌ **Problem**: Optimizing messy code.

```typescript
// ❌ Skipped Stage 2 - Went from "working" to "optimized" with ugly code
function calcOrdTot(itms) {
  let t = 0,
    i = 0,
    l = itms.length;
  for (; i < l; ++i) t += itms[i].p * itms[i].q;
  return t;
}
```

**Why it's bad**: Optimized but unmaintainable. Hard to modify or debug later.

### Optimizing Everything

❌ **Problem**: Optimizing code that doesn't need it.

```typescript
// ❌ Optimizing a function that runs once per page load
function getAppTitle(): string {
  // Unnecessary memoization for function called once
  if (this.cachedTitle) return this.cachedTitle;
  this.cachedTitle = "Open Sharia Enterprise";
  return this.cachedTitle;
}
```

**Why it's bad**: Wasted effort. Adds complexity with no benefit.

### Optimization Without Measurement

❌ **Problem**: Guessing which parts are slow.

```typescript
// ❌ "I think this is slow" - NO PROFILING DATA
// Developer spends 2 days optimizing this function
// Profiler shows it takes 0.1% of total execution time
```

**Why it's bad**: Optimizing the wrong thing. Real bottleneck remains unoptimized.

## ✅ Best Practices

### 1. Always Start Simple

**First implementation should be the simplest**:

```typescript
// ✅ Stage 1: Simple and obvious
function isValidEmail(email: string): boolean {
  return email.includes("@") && email.includes(".");
}

// Later Stage 2: Make it right (proper validation)
function isValidEmail(email: string): boolean {
  const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
  return emailRegex.test(email);
}

// Only if Stage 3 needed: Optimize (cache regex, use faster library)
```

### 2. Write Tests Before Refactoring

**Ensure tests pass before "Make It Right"**:

```typescript
// ✅ Tests lock in behavior before refactoring
describe("calculateOrderTotal", () => {
  it("calculates total for multiple items", () => {
    const items = [
      { price: 10, quantity: 2 },
      { price: 5, quantity: 3 },
    ];
    expect(calculateOrderTotal(items)).toBe(35);
  });
});

// Now safe to refactor - tests will catch breakage
```

### 3. Profile Before Optimizing

**Always measure, never guess**:

```bash
# ✅ Profile first
npm run profile

# Output shows:
# calculateOrderTotal: 850ms (85% of total time) ← THIS is the bottleneck
# formatCurrency: 50ms (5% of total time)
# Other functions: 100ms (10% of total time)

# Optimize calculateOrderTotal, not formatCurrency
```

### 4. Document Optimization Decisions

**Explain WHY optimization was needed**:

```typescript
/**
 * Optimized version of calculateOrderTotal
 *
 * Profiling data (2025-12-15):
 * - Baseline: 850ms for 10,000 items (85% of checkout time)
 * - Bottleneck: Repeated item.price * item.quantity calculations
 * - Solution: Memoize item totals
 * - Result: 45ms for 10,000 items (95% improvement)
 */
function calculateOrderTotalOptimized(items: OrderItem[]): number {
  // ... optimized implementation
}
```

### 5. Keep Optimization Localized

**Optimize the bottleneck, keep rest of code clean**:

```typescript
// ✅ Most code remains clean and readable
function processOrder(order: Order) {
  validateOrder(order); // Clean code
  applyDiscounts(order); // Clean code
  const total = calculateOrderTotalOptimized(order.items); // ONLY this optimized
  chargeCustomer(order.customer, total); // Clean code
}
```

### 6. Re-measure After Optimization

**Verify optimization actually helped**:

```typescript
// ✅ Before optimization
console.time("calculateOrderTotal");
const total = calculateOrderTotal(items);
console.timeEnd("calculateOrderTotal");
// calculateOrderTotal: 850ms

// After optimization
console.time("calculateOrderTotal");
const total = calculateOrderTotalOptimized(items);
console.timeEnd("calculateOrderTotal");
// calculateOrderTotal: 45ms ← Verified 95% improvement
```

## 📊 When to Apply

### ✅ Use This Workflow For

**New feature development**:

```
1. Make it work: Get feature functioning
2. Make it right: Clean up code, add tests
3. Make it fast: Optimize ONLY if performance requirements not met
```

**Bug fixes**:

```
1. Make it work: Fix the bug with simplest solution
2. Make it right: Refactor to prevent similar bugs
3. Make it fast: Usually not needed for bug fixes
```

**Refactoring**:

```
1. Already works: Start at Stage 2
2. Make it right: Improve structure and readability
3. Make it fast: Only if measurements show need
```

### ❌ Exceptions to the Workflow

**Security fixes**: Priority is "make it secure" (right), not "make it work"

```typescript
// Security fix: Correctness > Speed
function sanitizeInput(input: string): string {
  // Make it RIGHT first (secure), not just working
  return DOMPurify.sanitize(input, { SAFE_FOR_TEMPLATES: true });
}
```

**Production hotfixes**: Sometimes "make it work" is enough (fix immediately, refactor later)

```typescript
// Hotfix: Stop the bleeding first
function emergencyFix() {
  // Stage 1: Make it work (deploy immediately)
  if (data === null) return []; // Quick fix

  // Stage 2: Create ticket to "make it right" later
  // TODO: Refactor data handling (Ticket #123)
}
```

**Performance-critical code**: May need optimization from start (e.g., game engines, real-time systems)

```typescript
// Real-time video processing: Performance is a requirement
function processVideoFrame(frame: Frame): ProcessedFrame {
  // Even Stage 1 must consider performance
  // But still: work → right → fast
}
```

## 📖 References

**Software Engineering Principles**:

- [The Art of Computer Programming](https://en.wikipedia.org/wiki/The_Art_of_Computer_Programming) - Donald Knuth (premature optimization quote)
- [Refactoring: Improving the Design of Existing Code](https://martinfowler.com/books/refactoring.html) - Martin Fowler
- [Clean Code](https://www.oreilly.com/library/view/clean-code-a/9780136083238/) - Robert C. Martin

**Make It Work, Make It Right, Make It Fast**:

- [Kent Beck on Twitter](https://twitter.com/kentbeck) - Original "make it work, make it right, make it fast" attribution
- [Extreme Programming Explained](https://www.oreilly.com/library/view/extreme-programming-explained/0201616416/) - Kent Beck

**Performance Optimization**:

- [High Performance Browser Networking](https://hpbn.co/) - Ilya Grigorik
- [JavaScript Performance](https://developer.mozilla.org/en-US/docs/Web/Performance) - MDN Web Docs
- [Web Performance Optimization](https://web.dev/fast/) - Google Web Fundamentals

## 🔗 Related Documentation

- [Simplicity Over Complexity](../../principles/general/simplicity-over-complexity.md) - Start simple principle
- [Code Quality Convention](../quality/code.md) - Automated quality checks
- [Trunk Based Development](./trunk-based-development.md) - Git workflow
- [Acceptance Criteria Convention](../infra/acceptance-criteria.md) - Defining "works" in Stage 1

---

**Last Updated**: 2025-12-15
