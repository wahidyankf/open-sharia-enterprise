---
title: "React Styling"
description: Styling approaches and patterns for React applications
category: explanation
subcategory: stack-libs
tags:
  - react
  - styling
  - css
  - tailwind
  - css-modules
related:
  - ./ex-so-stli-tsre__best-practices.md
principles:
  - explicit-over-implicit
last_updated: 2026-01-25
---

# React Styling

## Quick Reference

**Navigation**: [Stack Libraries](../README.md) > [TypeScript React](./README.md) > Styling

**Related Guides**:

- [Best Practices](./ex-so-stli-tsre__best-practices.md) - Styling standards
- [Accessibility](./ex-so-stli-tsre__accessibility.md) - Accessible styling

## Overview

React supports multiple styling approaches. This guide covers CSS Modules, Tailwind CSS, CSS-in-JS, theming, and responsive design patterns.

**Target Audience**: Developers building styled React applications, particularly Islamic finance platforms requiring consistent design systems and responsive layouts.

**React Version**: React 18.2+ with TypeScript 5+

## CSS Modules

### Basic Usage

```typescript
// DonationCard.module.css
.card {
  border: 1px solid #e0e0e0;
  border-radius: 8px;
  padding: 16px;
  margin-bottom: 12px;
}

.card_header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 12px;
}

.card_title {
  font-size: 18px;
  font-weight: 600;
  color: #333;
}

.card_amount {
  font-size: 24px;
  font-weight: 700;
  color: #2e7d32;
}

// DonationCard.tsx
import styles from './DonationCard.module.css';

export const DonationCard: React.FC<{ donation: Donation }> = ({ donation }) => (
  <div className={styles.card}>
    <div className={styles.card_header}>
      <h3 className={styles.card_title}>{donation.campaignName}</h3>
      <span className={styles.card_amount}>{donation.amount}</span>
    </div>
  </div>
);
```

### Conditional Classes

```typescript
import styles from './Button.module.css';
import clsx from 'clsx';

interface ButtonProps {
  variant: 'primary' | 'secondary';
  size: 'small' | 'medium' | 'large';
  disabled?: boolean;
  children: React.ReactNode;
}

export const Button: React.FC<ButtonProps> = ({
  variant,
  size,
  disabled,
  children,
}) => (
  <button
    className={clsx(
      styles.button,
      styles[`button_${variant}`],
      styles[`button_${size}`],
      disabled && styles.button_disabled
    )}
    disabled={disabled}
  >
    {children}
  </button>
);
```

## Tailwind CSS

### Basic Tailwind

```typescript
export const DonationCard: React.FC<{ donation: Donation }> = ({ donation }) => (
  <div className="border border-gray-200 rounded-lg p-4 mb-3">
    <div className="flex justify-between items-center mb-3">
      <h3 className="text-lg font-semibold text-gray-900">
        {donation.campaignName}
      </h3>
      <span className="text-2xl font-bold text-green-700">
        {donation.amount}
      </span>
    </div>
    <p className="text-sm text-gray-600">{donation.donor.name}</p>
  </div>
);
```

### Conditional Classes with clsx

```typescript
import clsx from 'clsx';

interface BadgeProps {
  status: 'pending' | 'completed' | 'failed';
  children: React.ReactNode;
}

export const StatusBadge: React.FC<BadgeProps> = ({ status, children }) => (
  <span
    className={clsx(
      'px-3 py-1 rounded-full text-sm font-medium',
      {
        'bg-yellow-100 text-yellow-800': status === 'pending',
        'bg-green-100 text-green-800': status === 'completed',
        'bg-red-100 text-red-800': status === 'failed',
      }
    )}
  >
    {children}
  </span>
);
```

## Theming

### CSS Variables

```typescript
// App.css
:root {
  --color-primary: #1976d2;
  --color-secondary: #388e3c;
  --color-error: #d32f2f;
  --color-background: #ffffff;
  --color-text: #333333;
  --spacing-unit: 8px;
}

[data-theme='dark'] {
  --color-primary: #42a5f5;
  --color-background: #1e1e1e;
  --color-text: #ffffff;
}

// Component
.button {
  background-color: var(--color-primary);
  color: var(--color-text);
  padding: calc(var(--spacing-unit) * 2);
}

// React component
export const ThemeToggle: React.FC = () => {
  const [theme, setTheme] = useState<'light' | 'dark'>('light');

  useEffect(() => {
    document.documentElement.setAttribute('data-theme', theme);
  }, [theme]);

  return (
    <button onClick={() => setTheme(theme === 'light' ? 'dark' : 'light')}>
      Toggle Theme
    </button>
  );
};
```

## Related Documentation

- **[Best Practices](./ex-so-stli-tsre__best-practices.md)** - Styling standards
- **[Accessibility](./ex-so-stli-tsre__accessibility.md)** - Accessible styling

---

**Last Updated**: 2026-01-25
