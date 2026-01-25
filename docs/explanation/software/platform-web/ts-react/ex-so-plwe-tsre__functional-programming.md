---
title: "React Functional Programming"
description: Functional programming patterns and principles in React
category: explanation
subcategory: platform-web
tags:
  - react
  - functional-programming
  - immutability
  - pure-functions
  - typescript
related:
  - ./ex-so-plwe-tsre__idioms.md
  - ./ex-so-plwe-tsre__best-practices.md
principles:
  - immutability
  - pure-functions
last_updated: 2026-01-25
---

# React Functional Programming

## Quick Reference

**Navigation**: [Stack Libraries](../README.md) > [TypeScript React](./README.md) > Functional Programming

**Related Guides**:

- [Idioms](./ex-so-plwe-tsre__idioms.md) - Functional patterns
- [Hooks](./ex-so-plwe-tsre__hooks.md) - Hooks as FP abstractions
- [Component Architecture](./ex-so-plwe-tsre__component-architecture.md) - Component patterns

## Overview

Functional programming principles align naturally with React's component model. This guide covers pure components, immutability, function composition, and functional patterns.

**Target Audience**: Developers building React applications with functional programming principles, particularly for Islamic finance platforms requiring predictable, testable code.

**React Version**: React 18.2+ with TypeScript 5+

## Pure Components

### Pure Function Components

```typescript
// Pure component - same props = same output
export const ZakatDisplay: React.FC<{
  wealth: number;
  nisab: number;
  rate: number;
}> = ({ wealth, nisab, rate }) => {
  // Pure calculation
  const isEligible = wealth >= nisab;
  const zakatDue = isEligible ? wealth * rate : 0;

  return (
    <div>
      <p>Wealth: {wealth}</p>
      <p>Nisab: {nisab}</p>
      <p>Zakat Due: {zakatDue}</p>
    </div>
  );
};

// Impure component - has side effects
export const ImpureComponent: React.FC = () => {
  // ❌ Side effect during render
  localStorage.setItem('renderCount', String(Date.now()));

  return <div>Component</div>;
};

// ✅ Side effects in useEffect
export const PureComponent: React.FC = () => {
  useEffect(() => {
    localStorage.setItem('renderCount', String(Date.now()));
  }, []);

  return <div>Component</div>;
};
```

## Immutability

### Immutable State Updates

```typescript
export const AssetManager: React.FC = () => {
  const [assets, setAssets] = useState<Asset[]>([]);

  // ❌ Mutable update
  const addAssetMutable = (asset: Asset) => {
    assets.push(asset); // Mutates array
    setAssets(assets); // Won't trigger re-render
  };

  // ✅ Immutable update
  const addAsset = (asset: Asset) => {
    setAssets(prev => [...prev, asset]); // New array
  };

  const updateAsset = (id: string, updates: Partial<Asset>) => {
    setAssets(prev =>
      prev.map(asset =>
        asset.id === id
          ? { ...asset, ...updates } // New object
          : asset
      )
    );
  };

  const removeAsset = (id: string) => {
    setAssets(prev => prev.filter(asset => asset.id !== id));
  };

  return <div>{/* UI */}</div>;
};
```

### Immutable Object Updates

```typescript
// Nested object updates
const [form, setForm] = useState({
  donor: {
    name: "",
    email: "",
    address: {
      city: "",
      country: "",
    },
  },
  amount: 0,
});

// ❌ Mutable nested update
const updateCityMutable = (city: string) => {
  form.donor.address.city = city; // Mutates
  setForm(form); // Won't trigger re-render
};

// ✅ Immutable nested update
const updateCity = (city: string) => {
  setForm((prev) => ({
    ...prev,
    donor: {
      ...prev.donor,
      address: {
        ...prev.donor.address,
        city,
      },
    },
  }));
};

// ✅ Using Immer for complex updates
import { useImmer } from "use-immer";

const [form, updateForm] = useImmer({
  donor: {
    name: "",
    email: "",
    address: { city: "", country: "" },
  },
  amount: 0,
});

const updateCity = (city: string) => {
  updateForm((draft) => {
    draft.donor.address.city = city; // Immer handles immutability
  });
};
```

## Function Composition

### Composing Functions

```typescript
// Pure utility functions
const formatCurrency = (amount: number, currency: string): string => {
  return new Intl.NumberFormat("en-US", {
    style: "currency",
    currency,
  }).format(amount);
};

const calculateZakat = (wealth: number): number => {
  return wealth * 0.025;
};

const isEligible = (wealth: number, nisab: number): boolean => {
  return wealth >= nisab;
};

// Compose functions
const pipe =
  <T>(...fns: Array<(arg: T) => T>) =>
  (value: T) =>
    fns.reduce((acc, fn) => fn(acc), value);

const compose =
  <T>(...fns: Array<(arg: T) => T>) =>
  (value: T) =>
    fns.reduceRight((acc, fn) => fn(acc), value);

// Usage
const processWealth = pipe(
  (wealth: number) => wealth, // Start value
  (w) => (isEligible(w, 5000) ? w : 0),
  (w) => calculateZakat(w),
  (z) => Math.round(z * 100) / 100,
);

const zakatAmount = processWealth(10000); // 250
```

### Higher-Order Components

```typescript
// HOC for logging
function withLogging<P extends object>(
  Component: React.ComponentType<P>,
  componentName: string
): React.FC<P> {
  return function LoggedComponent(props: P) {
    useEffect(() => {
      console.log(`${componentName} mounted`);
      return () => console.log(`${componentName} unmounted`);
    }, []);

    return <Component {...props} />;
  };
}

// Usage
const DonationFormWithLogging = withLogging(DonationForm, 'DonationForm');

// HOC composition
const enhance = <P extends object>(Component: React.ComponentType<P>) =>
  withAuth(
    withLogging(
      withErrorBoundary(Component)
    )
  );

const EnhancedDashboard = enhance(Dashboard);
```

## Related Documentation

- **[Idioms](./ex-so-plwe-tsre__idioms.md)** - Functional patterns
- **[Hooks](./ex-so-plwe-tsre__hooks.md)** - Hooks as FP abstractions
- **[Component Architecture](./ex-so-plwe-tsre__component-architecture.md)** - Component patterns
- **[State Management](./ex-so-plwe-tsre__state-management.md)** - Immutable state

---

**Last Updated**: 2026-01-25
