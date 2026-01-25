---
title: "React Version Migration"
description: Upgrading React versions and migration strategies
category: explanation
subcategory: stack-libs
tags:
  - react
  - migration
  - upgrade
  - version
related:
  - ./README.md
principles:
  - automation-over-manual
last_updated: 2026-01-25
---

# React Version Migration

## Quick Reference

**Navigation**: [Stack Libraries](../README.md) > [TypeScript React](./README.md) > Version Migration

**Related Guides**:

- [Hooks](./ex-so-stli-tsre__hooks.md) - Modern hook patterns
- [Best Practices](./ex-so-stli-tsre__best-practices.md) - Current standards

## Overview

React version migrations require understanding breaking changes and new features. This guide covers upgrading from React 16 to 19 and migrating class components to functional components.

**Target Audience**: Developers maintaining React applications and planning version upgrades.

**React Version**: React 18.2+ (targeting React 19)

## React 17 to 18 Migration

### Key Changes

**Breaking Changes**:

- Automatic batching for all updates
- Strict mode effects run twice in development
- Removed IE support
- New root API required

**New Features**:

- Concurrent features (transitions, suspense)
- Automatic batching
- New hooks (useId, useTransition, useDeferredValue)

### Update Root API

```typescript
// ❌ React 17
import ReactDOM from 'react-dom';

ReactDOM.render(
  <React.StrictMode>
    <App />
  </React.StrictMode>,
  document.getElementById('root')
);

// ✅ React 18
import { createRoot } from 'react-dom/client';

const root = createRoot(document.getElementById('root')!);
root.render(
  <React.StrictMode>
    <App />
  </React.StrictMode>
);
```

### Automatic Batching

```typescript
// React 17 - only batched in event handlers
setTimeout(() => {
  setCount((c) => c + 1); // Re-render
  setFlag((f) => !f); // Re-render
}, 1000);

// React 18 - batched everywhere
setTimeout(() => {
  setCount((c) => c + 1); // Batched
  setFlag((f) => !f); // Batched (single re-render)
}, 1000);

// Opt out of batching if needed
import { flushSync } from "react-dom";

flushSync(() => {
  setCount((c) => c + 1); // Re-render immediately
});
setFlag((f) => !f); // Re-render immediately
```

## Class to Functional Component Migration

### Basic Conversion

```typescript
// ❌ Class component
class DonationCard extends React.Component<DonationCardProps> {
  render() {
    const { donation } = this.props;

    return (
      <div className="donation-card">
        <h3>{donation.campaignName}</h3>
        <p>{donation.amount}</p>
      </div>
    );
  }
}

// ✅ Functional component
const DonationCard: React.FC<DonationCardProps> = ({ donation }) => (
  <div className="donation-card">
    <h3>{donation.campaignName}</h3>
    <p>{donation.amount}</p>
  </div>
);
```

### State Conversion

```typescript
// ❌ Class with state
class Counter extends React.Component<{}, { count: number }> {
  state = { count: 0 };

  increment = () => {
    this.setState({ count: this.state.count + 1 });
  };

  render() {
    return (
      <div>
        <p>Count: {this.state.count}</p>
        <button onClick={this.increment}>Increment</button>
      </div>
    );
  }
}

// ✅ Functional with useState
const Counter: React.FC = () => {
  const [count, setCount] = useState(0);

  const increment = () => {
    setCount(prev => prev + 1);
  };

  return (
    <div>
      <p>Count: {count}</p>
      <button onClick={increment}>Increment</button>
    </div>
  );
};
```

### Lifecycle Methods to Hooks

```typescript
// ❌ componentDidMount
class UserProfile extends React.Component<{ userId: string }> {
  componentDidMount() {
    this.fetchUser(this.props.userId);
  }

  fetchUser(userId: string) {
    // Fetch logic
  }

  render() {
    return <div>{/* UI */}</div>;
  }
}

// ✅ useEffect
const UserProfile: React.FC<{ userId: string }> = ({ userId }) => {
  useEffect(() => {
    const fetchUser = async () => {
      // Fetch logic
    };

    fetchUser();
  }, []); // Empty deps = runs once on mount

  return <div>{/* UI */}</div>;
};

// ❌ componentDidUpdate
class SearchResults extends React.Component<{ query: string }> {
  componentDidUpdate(prevProps: { query: string }) {
    if (prevProps.query !== this.props.query) {
      this.fetchResults(this.props.query);
    }
  }

  render() {
    return <div>{/* UI */}</div>;
  }
}

// ✅ useEffect with dependencies
const SearchResults: React.FC<{ query: string }> = ({ query }) => {
  useEffect(() => {
    const fetchResults = async () => {
      // Fetch logic
    };

    fetchResults();
  }, [query]); // Runs when query changes

  return <div>{/* UI */}</div>;
};

// ❌ componentWillUnmount
class Timer extends React.Component {
  intervalId: number | null = null;

  componentDidMount() {
    this.intervalId = window.setInterval(() => {
      // Timer logic
    }, 1000);
  }

  componentWillUnmount() {
    if (this.intervalId) {
      clearInterval(this.intervalId);
    }
  }

  render() {
    return <div>{/* UI */}</div>;
  }
}

// ✅ useEffect cleanup
const Timer: React.FC = () => {
  useEffect(() => {
    const intervalId = setInterval(() => {
      // Timer logic
    }, 1000);

    return () => {
      clearInterval(intervalId);
    };
  }, []);

  return <div>{/* UI */}</div>;
};
```

## Related Documentation

- **[Hooks](./ex-so-stli-tsre__hooks.md)** - Modern hook patterns
- **[Best Practices](./ex-so-stli-tsre__best-practices.md)** - Current standards

---

**Last Updated**: 2026-01-25
