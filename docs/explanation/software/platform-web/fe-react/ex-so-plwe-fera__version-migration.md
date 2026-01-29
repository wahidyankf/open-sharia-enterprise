---
title: React Version Migration
description: Guide for upgrading React versions including migration paths, breaking changes, new features, and best practices for React 16 to 19
category: explanation
tags:
  - react
  - migration
  - upgrade
  - breaking-changes
  - version-management
created: 2026-01-29
updated: 2026-01-29
---

# React Version Migration

This document explains the upgrade paths, breaking changes, and new features when migrating between major React versions. Understanding these migrations is critical for maintaining modern React applications and taking advantage of performance improvements and new capabilities.

## 📋 Overview

React has undergone significant evolution across major versions. This guide covers:

- Migration paths from React 16 → 17 → 18
- Breaking changes and how to address them
- New features introduced in each version
- TypeScript migration considerations
- Testing library updates
- Common migration issues and solutions

## 🔄 React 16 to 17 Migration

React 17 is a "stepping stone" release focused on making upgrades easier. It introduced no new developer-facing features but prepared the ecosystem for React 18's concurrent features.

### Key Changes in React 17

**Event Delegation Changes**

React 17 changed where events are attached:

**React 16** (attaches to document):

```javascript
// Events bubble to document
document.addEventListener("click", handler);
```

**React 17** (attaches to root):

```javascript
// Events bubble to React root container
const root = document.getElementById("root");
root.addEventListener("click", handler);
```

**Why this matters**: Allows multiple React versions on the same page, making gradual upgrades possible.

**Event Pooling Removal**

React 17 removed event pooling for performance and to eliminate a common source of bugs.

**React 16** (pooled events):

```javascript
function handleClick(e) {
  // e is pooled - accessing e.target later causes errors
  setTimeout(() => {
    console.log(e.target); // ❌ null in React 16
  }, 1000);
}
```

**React 17** (no pooling):

```javascript
function handleClick(e) {
  // e is NOT pooled - safe to access later
  setTimeout(() => {
    console.log(e.target); // ✅ Works in React 17
  }, 1000);
}
```

**Migration impact**: Most code just works. Remove `e.persist()` calls if present.

### JSX Transform Update

React 17 introduced a new JSX transform that eliminates the need to import React in every file.

**React 16** (old transform):

```javascript
import React from "react"; // ← Required

function Component() {
  return <div>Hello</div>;
}
```

**React 17** (new transform):

```javascript
// No React import needed!
function Component() {
  return <div>Hello</div>;
}
```

**Enabling new transform**: Update babel/webpack configuration or use Create React App 4.0+.

### Gradual Adoption Strategy

React 17 enables gradual upgrades:

1. Upgrade entire app to React 17
2. Incrementally upgrade sections to React 18
3. Run multiple React versions simultaneously
4. Complete migration when ready

This is especially valuable for large applications.

## ⚡ React 17 to 18 Migration

React 18 introduces concurrent features, automatic batching, and new APIs. This is the most significant upgrade path.

### Automatic Batching

React 18 batches state updates automatically in ALL contexts, not just React event handlers.

**React 17** (limited batching):

```javascript
function handleClick() {
  // ✅ Batched - single re-render
  setCount((c) => c + 1);
  setFlag((f) => !f);
}

setTimeout(() => {
  // ❌ NOT batched - TWO re-renders
  setCount((c) => c + 1);
  setFlag((f) => !f);
}, 1000);
```

**React 18** (automatic batching):

```javascript
function handleClick() {
  // ✅ Batched
  setCount((c) => c + 1);
  setFlag((f) => !f);
}

setTimeout(() => {
  // ✅ Now batched automatically
  setCount((c) => c + 1);
  setFlag((f) => !f);
}, 1000);

fetch("/api/data").then(() => {
  // ✅ Batched in promises too
  setCount((c) => c + 1);
  setFlag((f) => !f);
});
```

**Opting out of batching**:

```javascript
import { flushSync } from "react-dom";

flushSync(() => {
  setCount((c) => c + 1); // Re-render immediately
});
flushSync(() => {
  setFlag((f) => !f); // Re-render immediately
});
```

### New Root API

React 18 introduces `createRoot` API, replacing `ReactDOM.render`.

**React 17** (legacy root):

```javascript
import ReactDOM from "react-dom";

ReactDOM.render(<App />, document.getElementById("root"));
```

**React 18** (concurrent root):

```javascript
import { createRoot } from "react-dom/client";

const root = createRoot(document.getElementById("root"));
root.render(<App />);
```

**Important**: Using `ReactDOM.render` in React 18 works but opts out of concurrent features.

### Concurrent Features

React 18's concurrent features allow React to work on multiple tasks simultaneously and interrupt rendering for higher-priority updates.

**Enabling concurrent features**: Use `createRoot` instead of `ReactDOM.render`.

**Automatic benefits**:

- Automatic batching
- Transitions API support
- Streaming SSR support
- Suspense improvements

### Hydration Changes

React 18 improves hydration with `hydrateRoot`:

**React 17**:

```javascript
import ReactDOM from "react-dom";

ReactDOM.hydrate(<App />, document.getElementById("root"));
```

**React 18**:

```javascript
import { hydrateRoot } from "react-dom/client";

hydrateRoot(document.getElementById("root"), <App />);
```

**Selective Hydration**: React 18 can hydrate parts of the tree independently, improving time-to-interactive.

## 💥 Breaking Changes

### React 18 Breaking Changes

**Automatic Batching**

- May expose existing bugs where component relied on immediate state updates
- Use `flushSync` if immediate updates are critical

**Stricter Strict Mode**

React 18 Strict Mode simulates mounting/unmounting components twice in development:

```javascript
function Component() {
  useEffect(() => {
    console.log("Mount");
    return () => console.log("Unmount");
  }, []);

  // In React 18 Strict Mode (development only):
  // Mount → Unmount → Mount
}
```

**Why**: Helps catch bugs related to missing cleanup in effects.

**Suspense Behavior Changes**

React 18 changed how Suspense works with effects:

- Effects inside Suspense boundaries fire after boundary resolves
- Improved streaming SSR support

**Internet Explorer Support Dropped**

React 18 no longer supports Internet Explorer. If IE11 support is required, stay on React 17.

### React 17 Breaking Changes

**Event Pooling Removed**

- Remove `e.persist()` calls (no longer needed)
- Event objects can be safely accessed asynchronously

**No IE Support for New JSX Transform**

- IE11 requires the old JSX transform
- Use React 16 for IE11 support

## 🆕 New Features in React 18

### useId Hook

Generates unique IDs for accessibility attributes, safe for server-side rendering.

```javascript
import { useId } from "react";

function Form() {
  const id = useId();

  return (
    <>
      <label htmlFor={id}>Name:</label>
      <input id={id} type="text" />
    </>
  );
}
```

**Why useful**: Prevents ID collisions in SSR and makes accessible forms easier.

### useTransition Hook

Marks state updates as non-urgent, keeping UI responsive during expensive operations.

```javascript
import { useState, useTransition } from "react";

function SearchResults() {
  const [query, setQuery] = useState("");
  const [isPending, startTransition] = useTransition();

  function handleChange(e) {
    const value = e.target.value;

    // Urgent: Update input immediately
    setQuery(value);

    // Non-urgent: Update results can be delayed
    startTransition(() => {
      setResults(filterResults(value));
    });
  }

  return (
    <>
      <input value={query} onChange={handleChange} />
      {isPending ? <Spinner /> : <Results data={results} />}
    </>
  );
}
```

**Key benefits**:

- Input stays responsive
- React can interrupt non-urgent work
- Built-in loading state (`isPending`)

### useDeferredValue Hook

Defers updating a value until more urgent updates are complete.

```javascript
import { useState, useDeferredValue } from "react";

function SearchResults() {
  const [query, setQuery] = useState("");
  const deferredQuery = useDeferredValue(query);

  // Expensive filtering uses deferred value
  const results = filterResults(deferredQuery);

  return (
    <>
      <input value={query} onChange={(e) => setQuery(e.target.value)} />
      <Results data={results} />
    </>
  );
}
```

**Difference from useTransition**: `useDeferredValue` defers the value itself, while `useTransition` wraps the state update.

### Suspense Improvements

React 18 improves Suspense for data fetching and streaming SSR.

**Data fetching with Suspense**:

```javascript
import { Suspense } from "react";

function Profile() {
  const user = use(fetchUser()); // Hypothetical data fetching hook
  return <div>{user.name}</div>;
}

function App() {
  return (
    <Suspense fallback={<Spinner />}>
      <Profile />
    </Suspense>
  );
}
```

**Streaming SSR**: Server can stream HTML as components load, improving time-to-first-byte.

**Nested Suspense boundaries**: Different parts of UI can load independently.

## 🔧 TypeScript Updates

### React 18 Type Changes

**children prop no longer implicit**:

**React 17**:

```typescript
interface Props {
  // children implicitly allowed
}

function Component(props: Props) {
  return <div>{props.children}</div>; // ✅ Works
}
```

**React 18**:

```typescript
interface Props {
  children?: React.ReactNode; // ← Must be explicit
}

function Component(props: Props) {
  return <div>{props.children}</div>; // ✅ Works
}
```

**FunctionComponent no longer includes children**:

**React 17**:

```typescript
const Component: React.FC = (props) => {
  return <div>{props.children}</div>; // ✅ children available
};
```

**React 18**:

```typescript
// Option 1: Explicit children prop
interface Props {
  children?: React.ReactNode;
}
const Component: React.FC<Props> = (props) => {
  return <div>{props.children}</div>; // ✅
};

// Option 2: Use function declaration
function Component({ children }: { children?: React.ReactNode }) {
  return <div>{children}</div>; // ✅
}
```

**New types for React 18 hooks**:

```typescript
import { useTransition, useDeferredValue, useId } from "react";

// useTransition
const [isPending, startTransition] = useTransition();

// useDeferredValue
const deferredValue = useDeferredValue<string>(query);

// useId
const id: string = useId();
```

### Updating @types/react

```bash
npm install --save-dev @types/react@18 @types/react-dom@18
```

## 🧪 Testing Migration

### React Testing Library Updates

**React 18 rendering changes**:

**React 17**:

```javascript
import { render } from "@testing-library/react";

test("renders component", () => {
  const { getByText } = render(<App />);
  expect(getByText("Hello")).toBeInTheDocument();
});
```

**React 18** (same API, but uses createRoot internally):

```javascript
import { render } from "@testing-library/react";

// API unchanged, but internally uses React 18's createRoot
test("renders component", () => {
  const { getByText } = render(<App />);
  expect(getByText("Hello")).toBeInTheDocument();
});
```

**Update React Testing Library**:

```bash
npm install --save-dev @testing-library/react@13
```

### Testing Concurrent Features

**Testing useTransition**:

```javascript
import { render, screen, waitFor } from "@testing-library/react";
import userEvent from "@testing-library/user-event";

test("handles transitions", async () => {
  render(<SearchComponent />);

  const input = screen.getByRole("textbox");
  await userEvent.type(input, "query");

  // Wait for transition to complete
  await waitFor(() => {
    expect(screen.getByText("Results")).toBeInTheDocument();
  });
});
```

**Testing Suspense boundaries**:

```javascript
import { render, screen } from "@testing-library/react";
import { Suspense } from "react";

test("shows fallback while loading", async () => {
  render(
    <Suspense fallback={<div>Loading...</div>}>
      <AsyncComponent />
    </Suspense>,
  );

  expect(screen.getByText("Loading...")).toBeInTheDocument();

  await screen.findByText("Loaded content");
});
```

## 📦 Dependency Updates

### Core React Packages

**React 18 upgrade**:

```bash
npm install react@18 react-dom@18
```

**TypeScript types**:

```bash
npm install --save-dev @types/react@18 @types/react-dom@18
```

### Ecosystem Package Compatibility

**React Router**:

```bash
# React Router 6 is React 18 compatible
npm install react-router-dom@6
```

**Redux**:

```bash
# Redux Toolkit 1.9+ supports React 18
npm install @reduxjs/toolkit@1.9 react-redux@8
```

**React Query**:

```bash
# React Query 4+ supports React 18
npm install @tanstack/react-query@4
```

**Styled Components**:

```bash
# Styled Components 6+ supports React 18
npm install styled-components@6
```

**Material-UI**:

```bash
# MUI 5+ supports React 18
npm install @mui/material@5
```

## 🛠️ Codemods

React provides automated codemods for some migrations:

**Install react-codemod**:

```bash
npx react-codemod
```

**Available transforms**:

```bash
# Update to React 18 createRoot API
npx react-codemod replace-reactdom-render

# Update React imports
npx react-codemod update-react-imports

# Remove PropTypes declarations (if using TypeScript)
npx react-codemod remove-prop-types
```

**Manual review required**: Always review codemod changes before committing.

## ✅ Migration Checklist

### React 16 → 17

- [ ] Update React and React-DOM to version 17
- [ ] Update @types/react and @types/react-dom to version 17
- [ ] Remove `e.persist()` calls from event handlers
- [ ] Test event handling behavior
- [ ] Update testing library to compatible version
- [ ] Run full test suite
- [ ] Test in production-like environment

### React 17 → 18

- [ ] Update React and React-DOM to version 18
- [ ] Update @types/react and @types/react-dom to version 18
- [ ] Replace `ReactDOM.render` with `createRoot`
- [ ] Replace `ReactDOM.hydrate` with `hydrateRoot`
- [ ] Add explicit `children` types to components
- [ ] Update React Testing Library to version 13+
- [ ] Test automatic batching behavior
- [ ] Review Strict Mode warnings in development
- [ ] Update ecosystem dependencies (Router, Redux, etc.)
- [ ] Test Suspense boundaries if used
- [ ] Run full test suite
- [ ] Test in production-like environment
- [ ] Monitor production after deployment

## 🐛 Common Issues

### Issue 1: Multiple Re-renders After Upgrade

**Problem**: Component re-renders more than expected after upgrading to React 18.

**Cause**: Automatic batching may expose existing bugs where code relied on immediate state updates.

**Solution**:

```javascript
import { flushSync } from "react-dom";

function handleClick() {
  flushSync(() => {
    setCount((c) => c + 1); // Force immediate update
  });

  // Code that depends on updated count
  console.log(countRef.current);
}
```

### Issue 2: TypeScript Errors with children

**Problem**: TypeScript errors about missing `children` prop after React 18 upgrade.

**Cause**: `children` is no longer implicit in React 18 types.

**Solution**:

```typescript
// Add explicit children prop
interface Props {
  children?: React.ReactNode;
}

function Component({ children }: Props) {
  return <div>{children}</div>;
}
```

### Issue 3: Strict Mode Double Mounting

**Problem**: Effects run twice in development after React 18 upgrade.

**Cause**: React 18 Strict Mode simulates mount/unmount to catch missing cleanup.

**Solution**: Add proper cleanup to effects:

```javascript
useEffect(() => {
  const subscription = api.subscribe();

  return () => {
    subscription.unsubscribe(); // ← Cleanup required
  };
}, []);
```

### Issue 4: Suspense Boundary Not Working

**Problem**: Suspense boundary shows fallback indefinitely.

**Cause**: Component may not be throwing a promise correctly.

**Solution**: Verify data fetching integrates with Suspense:

```javascript
// Correct: Throw promise during render
function Component() {
  const data = use(fetchData()); // Suspense-compatible hook
  return <div>{data}</div>;
}

// Incorrect: Async in effect
function Component() {
  const [data, setData] = useState(null);

  useEffect(() => {
    fetchData().then(setData); // ❌ Won't trigger Suspense
  }, []);

  return data ? <div>{data}</div> : null;
}
```

### Issue 5: Third-Party Library Incompatibility

**Problem**: Third-party library throws errors after React 18 upgrade.

**Cause**: Library may not be React 18 compatible yet.

**Solution**:

1. Check library's React 18 compatibility
2. Update to latest version
3. Report issue to library maintainers
4. Use legacy root API as temporary workaround:

```javascript
// Temporary workaround (not recommended long-term)
import ReactDOM from "react-dom";

ReactDOM.render(<App />, document.getElementById("root"));
```

## 🔮 Future: React 19 (Speculative)

**Note**: React 19 features are speculative and subject to change.

### Anticipated Changes

**Server Components Stabilization**

React Server Components (RSC) may become stable in React 19:

- Components that render on server without client-side JavaScript
- Improved data fetching patterns
- Reduced bundle sizes

**React Forget (Auto-Memoization)**

Compiler that automatically optimizes components:

- No need for manual `useMemo`/`useCallback`
- Automatic dependency tracking
- Performance improvements without code changes

**Asset Loading API**

Better control over resource loading:

```javascript
import { preload } from "react";

// Speculative API
function Component() {
  preload("/api/data", { as: "fetch" });
  preload("/image.png", { as: "image" });
}
```

**Improved Error Handling**

Enhanced error boundaries with recovery capabilities.

**Offscreen API**

Render components in background without displaying:

```javascript
// Speculative API
<Offscreen mode="hidden">
  <ExpensiveComponent />
</Offscreen>
```

### Staying Updated

**Official sources**:

- React Blog: <https://react.dev/blog>
- React RFC: <https://github.com/reactjs/rfcs>
- React Working Group: <https://github.com/reactwg>

**Follow React core team** on Twitter/X for early insights into future features.

## 📚 References

**Official Documentation**:

- [React 18 Upgrade Guide](https://react.dev/blog/2022/03/08/react-18-upgrade-guide)
- [React 18 Release Notes](https://react.dev/blog/2022/03/29/react-v18)
- [React 17 Release Notes](https://react.dev/blog/2020/10/20/react-v17)

**Related Explanation Documents**:

- [React Core Concepts](./ex-so-plwe-fera__core-concepts.md) - Fundamental React concepts
- [React Component Patterns](./ex-so-plwe-fera__component-patterns.md) - Modern component patterns
- [React Performance Optimization](./ex-so-plwe-fera__performance.md) - Performance best practices

**Testing**:

- [React Testing Library](https://testing-library.com/react) - Testing best practices
- [Jest Documentation](https://jestjs.io/) - Testing framework

---

**Last Updated**: 2026-01-29

**Migration Path Summary**: React 16 → 17 (stepping stone) → 18 (concurrent features) → 19 (anticipated improvements)
