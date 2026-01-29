---
title: "React + TypeScript Anti-Patterns"
description: Common mistakes and problematic patterns to avoid in React + TypeScript development
category: explanation
subcategory: platform-web
tags:
  - react
  - typescript
  - anti-patterns
  - code-smells
  - mistakes
  - avoid
related:
  - ./ex-so-plwe-fera__idioms.md
  - ./ex-so-plwe-fera__best-practices.md
  - ./ex-so-plwe-fera__performance.md
principles:
  - explicit-over-implicit
  - immutability
  - pure-functions
last_updated: 2026-01-29
---

# React + TypeScript Anti-Patterns

## Quick Reference

### State Management Anti-Patterns

- [Prop Drilling](#1-prop-drilling) - Passing props through many layers
- [Unnecessary useEffect](#2-unnecessary-useeffect) - Overusing effects for derived state
- [Missing useCallback/useMemo](#3-missing-usecallbackusememo) - Performance issues from recreating functions
- [useState for Derived State](#4-usestate-for-derived-state) - Computing values in render

### Component Design Anti-Patterns

- [Large Components](#5-large-god-components) - God components doing too much
- [Mutating State Directly](#6-mutating-state-directly) - Breaking React's immutability
- [Not Handling Loading States](#7-not-handling-loading-states) - Poor UX during async operations
- [Missing Error Boundaries](#8-missing-error-boundaries) - Uncaught errors crashing app

### Context Anti-Patterns

- [Overusing Context](#9-overusing-context) - Performance problems from too much context
- [Context Without Memoization](#10-context-without-memoization) - Unnecessary re-renders

### TypeScript Anti-Patterns

- [Any Type](#11-any-type-in-typescript) - Losing type safety
- [Type Assertions](#12-excessive-type-assertions) - Unsafe casting
- [Missing Prop Types](#13-missing-prop-types) - Implicit any in props

### Effect Anti-Patterns

- [Not Cleaning up Effects](#14-not-cleaning-up-effects) - Memory leaks from subscriptions
- [Race Conditions](#15-race-conditions-in-effects) - Async bugs

### List Rendering Anti-Patterns

- [Index as Key](#16-index-as-key-in-lists) - Rendering bugs
- [Non-Unique Keys](#17-non-unique-keys) - State mismatches

### Accessibility and Security

- [Missing Accessibility](#18-missing-accessibility) - Excluding users
- [Client-Side Secrets](#19-client-side-secrets) - Security vulnerabilities

## Overview

This document identifies common anti-patterns in React + TypeScript applications that lead to maintainability issues, performance problems, security vulnerabilities, or poor user experience. Each anti-pattern includes a FAIL example showing the problem and a PASS example demonstrating the idiomatic React approach.

These examples use React 18+ with TypeScript 5+ and focus on Islamic financial domains including Zakat calculation, Murabaha contracts, and donation management.

## 1. Prop Drilling

### ❌ FAIL - Passing Props Through Many Layers

**Problem**: Passing props through multiple intermediate components that don't use them, creating tight coupling and maintenance burden.

```typescript
// Root component
interface User {
  id: string;
  name: string;
}

const App: React.FC = () => {
  const [user, setUser] = useState<User | null>(null);

  return <ZakatDashboard user={user} setUser={setUser} />;
};

// Dashboard doesn't use user, just passes it down
interface ZakatDashboardProps {
  user: User | null;
  setUser: (user: User | null) => void;
}

const ZakatDashboard: React.FC<ZakatDashboardProps> = ({ user, setUser }) => {
  return (
    <div>
      <ZakatSidebar user={user} setUser={setUser} />
      <ZakatContent user={user} setUser={setUser} />
    </div>
  );
};

// Sidebar doesn't use user either, just passes it down
const ZakatSidebar: React.FC<ZakatDashboardProps> = ({ user, setUser }) => {
  return (
    <nav>
      <UserProfile user={user} setUser={setUser} />
    </nav>
  );
};

// Finally used here - 3 layers deep!
const UserProfile: React.FC<ZakatDashboardProps> = ({ user, setUser }) => {
  if (!user) return <div>Please log in</div>;

  return (
    <div>
      <h3>{user.name}</h3>
      <button onClick={() => setUser(null)}>Logout</button>
    </div>
  );
};
```

**Issues**:

- Intermediate components coupled to user state they don't use
- Difficult to refactor component hierarchy
- Props must be threaded through every layer
- TypeScript interfaces must be updated in multiple places
- Cannot easily swap out intermediate components

### ✅ PASS - Use Context API

```typescript
// Create typed context
interface AuthContextValue {
  user: User | null;
  setUser: (user: User | null) => void;
}

const AuthContext = createContext<AuthContextValue | undefined>(undefined);

// Custom hook with error checking
export const useAuth = () => {
  const context = useContext(AuthContext);
  if (!context) {
    throw new Error('useAuth must be used within AuthProvider');
  }
  return context;
};

// Provider component
export const AuthProvider: React.FC<{ children: React.ReactNode }> = ({ children }) => {
  const [user, setUser] = useState<User | null>(null);

  const value = useMemo(
    () => ({ user, setUser }),
    [user]
  );

  return <AuthContext.Provider value={value}>{children}</AuthContext.Provider>;
};

// Root component
const App: React.FC = () => {
  return (
    <AuthProvider>
      <ZakatDashboard />
    </AuthProvider>
  );
};

// Intermediate components don't need props
const ZakatDashboard: React.FC = () => {
  return (
    <div>
      <ZakatSidebar />
      <ZakatContent />
    </div>
  );
};

const ZakatSidebar: React.FC = () => {
  return (
    <nav>
      <UserProfile />
    </nav>
  );
};

// Component consumes context directly
const UserProfile: React.FC = () => {
  const { user, setUser } = useAuth();

  if (!user) return <div>Please log in</div>;

  return (
    <div>
      <h3>{user.name}</h3>
      <button onClick={() => setUser(null)}>Logout</button>
    </div>
  );
};
```

## 2. Unnecessary useEffect

### ❌ FAIL - useEffect for Derived State

**Problem**: Using useEffect to compute values that can be calculated during render, creating synchronization bugs and extra renders.

```typescript
interface ZakatCalculatorProps {
  wealth: number;
  nisab: number;
}

const ZakatCalculator: React.FC<ZakatCalculatorProps> = ({ wealth, nisab }) => {
  const [zakatAmount, setZakatAmount] = useState(0);
  const [isEligible, setIsEligible] = useState(false);

  // ❌ Unnecessary effect - these values can be computed during render
  useEffect(() => {
    const eligible = wealth >= nisab;
    const amount = eligible ? wealth * 0.025 : 0;

    setZakatAmount(amount);
    setIsEligible(eligible);
  }, [wealth, nisab]);

  return (
    <div>
      <p>Wealth: ${wealth}</p>
      <p>Nisab: ${nisab}</p>
      <p>Eligible: {isEligible ? 'Yes' : 'No'}</p>
      <p>Zakat Amount: ${zakatAmount.toFixed(2)}</p>
    </div>
  );
};
```

**Issues**:

- Two extra state variables for derived data
- Effect runs after initial render (flash of incorrect content)
- Synchronization bugs if calculation logic isn't updated everywhere
- Extra re-renders from setState calls
- More complex to test

### ✅ PASS - Compute During Render

```typescript
interface ZakatCalculatorProps {
  wealth: number;
  nisab: number;
}

const ZakatCalculator: React.FC<ZakatCalculatorProps> = ({ wealth, nisab }) => {
  // ✅ Compute derived values during render
  const isEligible = wealth >= nisab;
  const zakatAmount = isEligible ? wealth * 0.025 : 0;

  return (
    <div>
      <p>Wealth: ${wealth}</p>
      <p>Nisab: ${nisab}</p>
      <p>Eligible: {isEligible ? 'Yes' : 'No'}</p>
      <p>Zakat Amount: ${zakatAmount.toFixed(2)}</p>
    </div>
  );
};

// ✅ For expensive calculations, use useMemo
const ZakatCalculatorExpensive: React.FC<ZakatCalculatorProps> = ({ wealth, nisab }) => {
  const isEligible = wealth >= nisab;

  // Only use useMemo for genuinely expensive calculations
  const zakatAmount = useMemo(() => {
    // Imagine complex calculation here
    return isEligible ? wealth * 0.025 : 0;
  }, [wealth, nisab, isEligible]);

  return (
    <div>
      <p>Wealth: ${wealth}</p>
      <p>Nisab: ${nisab}</p>
      <p>Eligible: {isEligible ? 'Yes' : 'No'}</p>
      <p>Zakat Amount: ${zakatAmount.toFixed(2)}</p>
    </div>
  );
};
```

## 3. Missing useCallback/useMemo

### ❌ FAIL - Recreating Functions on Every Render

**Problem**: Creating new function references on every render causes child components to re-render unnecessarily.

```typescript
interface Donation {
  id: string;
  amount: number;
  category: string;
}

const DonationList: React.FC<{ donations: Donation[] }> = ({ donations }) => {
  const [deletedIds, setDeletedIds] = useState<Set<string>>(new Set());

  // ❌ New function created every render
  const handleDelete = (id: string) => {
    setDeletedIds((prev) => new Set(prev).add(id));
  };

  return (
    <div>
      {donations.map((donation) => (
        <DonationItem
          key={donation.id}
          donation={donation}
          onDelete={handleDelete} // ❌ New reference every render!
        />
      ))}
    </div>
  );
};

// This component re-renders on every parent render
const DonationItem: React.FC<{
  donation: Donation;
  onDelete: (id: string) => void;
}> = ({ donation, onDelete }) => {
  console.log('Rendering DonationItem:', donation.id); // Logs on every parent render!

  return (
    <div>
      <span>{donation.category}</span>
      <span>${donation.amount}</span>
      <button onClick={() => onDelete(donation.id)}>Delete</button>
    </div>
  );
};
```

**Issues**:

- Child components re-render unnecessarily
- Performance degrades with many list items
- React.memo doesn't help without stable function references
- Wasted computation and DOM updates

### ✅ PASS - Use useCallback and React.memo

```typescript
const DonationList: React.FC<{ donations: Donation[] }> = ({ donations }) => {
  const [deletedIds, setDeletedIds] = useState<Set<string>>(new Set());

  // ✅ Stable function reference
  const handleDelete = useCallback((id: string) => {
    setDeletedIds((prev) => new Set(prev).add(id));
  }, []); // Empty deps - function never changes

  return (
    <div>
      {donations.map((donation) => (
        <DonationItem
          key={donation.id}
          donation={donation}
          onDelete={handleDelete} // ✅ Same reference every render
        />
      ))}
    </div>
  );
};

// ✅ Memoized component only re-renders when props change
const DonationItem = React.memo<{
  donation: Donation;
  onDelete: (id: string) => void;
}>(({ donation, onDelete }) => {
  console.log('Rendering DonationItem:', donation.id); // Only logs when donation changes!

  return (
    <div>
      <span>{donation.category}</span>
      <span>${donation.amount}</span>
      <button onClick={() => onDelete(donation.id)}>Delete</button>
    </div>
  );
});

DonationItem.displayName = 'DonationItem';
```

## 4. useState for Derived State

### ❌ FAIL - Redundant State Variables

**Problem**: Storing values in state that can be computed from existing state, leading to synchronization bugs.

```typescript
interface MurabahaContract {
  principal: number;
  profitRate: number;
  termMonths: number;
}

const MurabahaCalculator: React.FC = () => {
  const [principal, setPrincipal] = useState(10000);
  const [profitRate, setProfitRate] = useState(0.05);
  const [termMonths, setTermMonths] = useState(12);

  // ❌ These are derived - should not be in state!
  const [totalAmount, setTotalAmount] = useState(0);
  const [monthlyPayment, setMonthlyPayment] = useState(0);

  // ❌ Multiple useEffects to keep derived state in sync
  useEffect(() => {
    const total = principal + (principal * profitRate);
    setTotalAmount(total);
  }, [principal, profitRate]);

  useEffect(() => {
    const payment = totalAmount / termMonths;
    setMonthlyPayment(payment);
  }, [totalAmount, termMonths]);

  return (
    <div>
      <input
        type="number"
        value={principal}
        onChange={(e) => setPrincipal(Number(e.target.value))}
      />
      <input
        type="number"
        value={profitRate}
        onChange={(e) => setProfitRate(Number(e.target.value))}
      />
      <input
        type="number"
        value={termMonths}
        onChange={(e) => setTermMonths(Number(e.target.value))}
      />
      <p>Total Amount: ${totalAmount.toFixed(2)}</p>
      <p>Monthly Payment: ${monthlyPayment.toFixed(2)}</p>
    </div>
  );
};
```

**Issues**:

- Synchronization bugs if effect dependencies are incomplete
- Extra state variables and re-renders
- Effect chain creates multiple render cycles
- Difficult to reason about data flow

### ✅ PASS - Compute Derived Values

```typescript
const MurabahaCalculator: React.FC = () => {
  const [principal, setPrincipal] = useState(10000);
  const [profitRate, setProfitRate] = useState(0.05);
  const [termMonths, setTermMonths] = useState(12);

  // ✅ Compute derived values during render
  const totalAmount = principal + (principal * profitRate);
  const monthlyPayment = totalAmount / termMonths;

  return (
    <div>
      <input
        type="number"
        value={principal}
        onChange={(e) => setPrincipal(Number(e.target.value))}
      />
      <input
        type="number"
        value={profitRate}
        onChange={(e) => setProfitRate(Number(e.target.value))}
      />
      <input
        type="number"
        value={termMonths}
        onChange={(e) => setTermMonths(Number(e.target.value))}
      />
      <p>Total Amount: ${totalAmount.toFixed(2)}</p>
      <p>Monthly Payment: ${monthlyPayment.toFixed(2)}</p>
    </div>
  );
};
```

## 5. Large God Components

### ❌ FAIL - Component Doing Too Much

**Problem**: Single component handling multiple concerns, becoming difficult to maintain and test.

```typescript
const ZakatDashboard: React.FC = () => {
  // ❌ Too many responsibilities in one component!

  // Zakat calculation state
  const [wealth, setWealth] = useState(0);
  const [nisab, setNisab] = useState(0);
  const [calculations, setCalculations] = useState<any[]>([]);

  // Donation state
  const [donations, setDonations] = useState<any[]>([]);
  const [donationAmount, setDonationAmount] = useState(0);

  // User state
  const [user, setUser] = useState<User | null>(null);
  const [loading, setLoading] = useState(false);

  // Report state
  const [reports, setReports] = useState<any[]>([]);
  const [selectedYear, setSelectedYear] = useState(2024);

  // ❌ Many effects
  useEffect(() => {
    // Fetch calculations
  }, [user]);

  useEffect(() => {
    // Fetch donations
  }, [user]);

  useEffect(() => {
    // Fetch reports
  }, [user, selectedYear]);

  useEffect(() => {
    // Sync data
  }, [calculations, donations]);

  // ❌ Many handlers
  const handleCalculateZakat = () => {
    // Complex logic
  };

  const handleDonation = () => {
    // Complex logic
  };

  const handleGenerateReport = () => {
    // Complex logic
  };

  // ❌ Hundreds of lines of JSX
  return (
    <div>
      {/* Zakat calculation section */}
      {/* Donation section */}
      {/* Reports section */}
      {/* User profile section */}
      {/* ... 500+ lines of JSX */}
    </div>
  );
};
```

**Issues**:

- Single Responsibility Principle violation
- Difficult to test individual features
- Hard to navigate and understand
- Cannot reuse components
- Merge conflicts common

### ✅ PASS - Split Into Focused Components

```typescript
// ✅ Focused component for Zakat calculation
const ZakatCalculationSection: React.FC = () => {
  const [wealth, setWealth] = useState(0);
  const [nisab, setNisab] = useState(0);

  const zakatAmount = wealth >= nisab ? wealth * 0.025 : 0;

  return (
    <section>
      <h2>Zakat Calculator</h2>
      <input
        type="number"
        value={wealth}
        onChange={(e) => setWealth(Number(e.target.value))}
        placeholder="Wealth"
      />
      <input
        type="number"
        value={nisab}
        onChange={(e) => setNisab(Number(e.target.value))}
        placeholder="Nisab"
      />
      <p>Zakat Amount: ${zakatAmount.toFixed(2)}</p>
    </section>
  );
};

// ✅ Focused component for donations
const DonationsSection: React.FC = () => {
  const [donations, setDonations] = useState<Donation[]>([]);
  const [amount, setAmount] = useState(0);

  const handleDonate = async () => {
    // Donation logic
  };

  return (
    <section>
      <h2>Donations</h2>
      <input
        type="number"
        value={amount}
        onChange={(e) => setAmount(Number(e.target.value))}
      />
      <button onClick={handleDonate}>Donate</button>
      <DonationList donations={donations} />
    </section>
  );
};

// ✅ Focused component for reports
const ReportsSection: React.FC = () => {
  const [year, setYear] = useState(2024);
  const [reports, setReports] = useState<Report[]>([]);

  useEffect(() => {
    // Fetch reports for selected year
  }, [year]);

  return (
    <section>
      <h2>Reports</h2>
      <select value={year} onChange={(e) => setYear(Number(e.target.value))}>
        <option value={2023}>2023</option>
        <option value={2024}>2024</option>
      </select>
      <ReportList reports={reports} />
    </section>
  );
};

// ✅ Composed dashboard
const ZakatDashboard: React.FC = () => {
  return (
    <div>
      <ZakatCalculationSection />
      <DonationsSection />
      <ReportsSection />
    </div>
  );
};
```

## 6. Mutating State Directly

### ❌ FAIL - Direct Mutation

**Problem**: Mutating state objects directly breaks React's change detection and causes bugs.

```typescript
interface PaymentSchedule {
  id: string;
  payments: Array<{ id: string; amount: number; paid: boolean }>;
}

const PaymentTracker: React.FC = () => {
  const [schedule, setSchedule] = useState<PaymentSchedule>({
    id: '1',
    payments: [
      { id: 'p1', amount: 100, paid: false },
      { id: 'p2', amount: 100, paid: false },
    ],
  });

  const markAsPaid = (paymentId: string) => {
    // ❌ Direct mutation - React won't detect change!
    const payment = schedule.payments.find((p) => p.id === paymentId);
    if (payment) {
      payment.paid = true; // ❌ Mutating nested object
    }
    setSchedule(schedule); // ❌ Same reference - no re-render!
  };

  return (
    <div>
      {schedule.payments.map((payment) => (
        <div key={payment.id}>
          <span>${payment.amount}</span>
          <span>{payment.paid ? '✓ Paid' : 'Pending'}</span>
          <button onClick={() => markAsPaid(payment.id)}>Mark Paid</button>
        </div>
      ))}
    </div>
  );
};
```

**Issues**:

- React doesn't detect changes (same object reference)
- Component doesn't re-render
- UI shows stale data
- Violates immutability principle

### ✅ PASS - Immutable Updates

```typescript
const PaymentTracker: React.FC = () => {
  const [schedule, setSchedule] = useState<PaymentSchedule>({
    id: '1',
    payments: [
      { id: 'p1', amount: 100, paid: false },
      { id: 'p2', amount: 100, paid: false },
    ],
  });

  const markAsPaid = (paymentId: string) => {
    // ✅ Create new state with immutable update
    setSchedule((prevSchedule) => ({
      ...prevSchedule,
      payments: prevSchedule.payments.map((payment) =>
        payment.id === paymentId
          ? { ...payment, paid: true } // ✅ New payment object
          : payment
      ),
    }));
  };

  return (
    <div>
      {schedule.payments.map((payment) => (
        <div key={payment.id}>
          <span>${payment.amount}</span>
          <span>{payment.paid ? '✓ Paid' : 'Pending'}</span>
          <button onClick={() => markAsPaid(payment.id)}>Mark Paid</button>
        </div>
      ))}
    </div>
  );
};
```

## 7. Not Handling Loading States

### ❌ FAIL - No Loading Indicators

**Problem**: Not showing loading states during async operations creates poor user experience.

```typescript
const DonationHistory: React.FC<{ userId: string }> = ({ userId }) => {
  const [donations, setDonations] = useState<Donation[]>([]);

  useEffect(() => {
    // ❌ No loading state!
    fetch(`/api/donations/${userId}`)
      .then((res) => res.json())
      .then((data) => setDonations(data));
  }, [userId]);

  // ❌ User sees empty list for 2-3 seconds
  return (
    <div>
      <h2>Donation History</h2>
      {donations.length === 0 ? (
        <p>No donations yet</p> // ❌ Or still loading?
      ) : (
        <ul>
          {donations.map((d) => (
            <li key={d.id}>{d.amount}</li>
          ))}
        </ul>
      )}
    </div>
  );
};
```

**Issues**:

- User doesn't know if data is loading or empty
- Poor user experience
- Cannot distinguish between loading and empty states
- No error handling

### ✅ PASS - Proper Loading States

```typescript
const DonationHistory: React.FC<{ userId: string }> = ({ userId }) => {
  const [donations, setDonations] = useState<Donation[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    let cancelled = false;

    const fetchDonations = async () => {
      try {
        setLoading(true);
        setError(null);

        const response = await fetch(`/api/donations/${userId}`);
        if (!response.ok) throw new Error('Failed to fetch donations');

        const data = await response.json();

        if (!cancelled) {
          setDonations(data);
        }
      } catch (err) {
        if (!cancelled) {
          setError(err instanceof Error ? err.message : 'Unknown error');
        }
      } finally {
        if (!cancelled) {
          setLoading(false);
        }
      }
    };

    fetchDonations();

    return () => {
      cancelled = true;
    };
  }, [userId]);

  // ✅ Clear loading state
  if (loading) {
    return <LoadingSpinner />;
  }

  // ✅ Clear error state
  if (error) {
    return (
      <div>
        <p>Error: {error}</p>
        <button onClick={() => window.location.reload()}>Retry</button>
      </div>
    );
  }

  // ✅ Now we know it's truly empty
  if (donations.length === 0) {
    return <p>No donations yet. Make your first donation!</p>;
  }

  return (
    <div>
      <h2>Donation History</h2>
      <ul>
        {donations.map((d) => (
          <li key={d.id}>{d.amount}</li>
        ))}
      </ul>
    </div>
  );
};
```

## 8. Missing Error Boundaries

### ❌ FAIL - Uncaught Errors Crash App

**Problem**: Not using Error Boundaries to catch and handle errors gracefully.

```typescript
// ❌ No error boundary - errors crash entire app
const App: React.FC = () => {
  return (
    <div>
      <Header />
      <ZakatCalculator /> {/* If this throws, entire app crashes */}
      <DonationList />
      <Footer />
    </div>
  );
};

const ZakatCalculator: React.FC = () => {
  // ❌ Error in calculation crashes app
  const result = someComplexCalculation(); // Throws error

  return <div>{result}</div>;
};
```

**Issues**:

- Single error crashes entire application
- Poor user experience
- No error reporting
- No fallback UI

### ✅ PASS - Use Error Boundaries

```typescript
// ✅ Error boundary component
class ErrorBoundary extends React.Component<
  { children: React.ReactNode; fallback?: React.ReactNode },
  { hasError: boolean; error: Error | null }
> {
  constructor(props: { children: React.ReactNode; fallback?: React.ReactNode }) {
    super(props);
    this.state = { hasError: false, error: null };
  }

  static getDerivedStateFromError(error: Error) {
    return { hasError: true, error };
  }

  componentDidCatch(error: Error, errorInfo: React.ErrorInfo) {
    // Log to error reporting service
    console.error('Error caught by boundary:', error, errorInfo);
  }

  render() {
    if (this.state.hasError) {
      return (
        this.props.fallback || (
          <div>
            <h2>Something went wrong</h2>
            <p>{this.state.error?.message}</p>
            <button onClick={() => this.setState({ hasError: false, error: null })}>
              Try again
            </button>
          </div>
        )
      );
    }

    return this.props.children;
  }
}

// ✅ Wrap components in error boundaries
const App: React.FC = () => {
  return (
    <div>
      <Header />
      <ErrorBoundary fallback={<div>Calculator temporarily unavailable</div>}>
        <ZakatCalculator />
      </ErrorBoundary>
      <ErrorBoundary fallback={<div>Donation list temporarily unavailable</div>}>
        <DonationList />
      </ErrorBoundary>
      <Footer />
    </div>
  );
};
```

## 9. Overusing Context

### ❌ FAIL - Everything in Context

**Problem**: Putting too much unrelated state in a single context causes performance issues.

```typescript
// ❌ God context with everything
interface AppContextValue {
  user: User | null;
  donations: Donation[];
  calculations: Calculation[];
  theme: 'light' | 'dark';
  language: 'en' | 'ar';
  notifications: Notification[];
  settings: Settings;
  // ... 20 more fields
}

const AppContext = createContext<AppContextValue | undefined>(undefined);

const App: React.FC = () => {
  const [user, setUser] = useState<User | null>(null);
  const [donations, setDonations] = useState<Donation[]>([]);
  // ... 20 more useState calls

  // ❌ Huge object - any change re-renders ALL consumers
  const value = {
    user,
    donations,
    calculations,
    theme,
    language,
    notifications,
    settings,
    // ...
  };

  return <AppContext.Provider value={value}>{/* ... */}</AppContext.Provider>;
};

// ❌ This component only needs theme, but re-renders on ANY context change
const ThemeToggle: React.FC = () => {
  const context = useContext(AppContext);
  // Re-renders when donations change, user changes, etc.

  return <button>Toggle {context?.theme}</button>;
};
```

**Issues**:

- All consumers re-render on any context change
- Cannot optimize re-renders
- Performance degrades with many consumers
- Difficult to reason about dependencies

### ✅ PASS - Separate Focused Contexts

```typescript
// ✅ Separate contexts for different concerns
const AuthContext = createContext<AuthContextValue | undefined>(undefined);
const ThemeContext = createContext<ThemeContextValue | undefined>(undefined);
const DonationsContext = createContext<DonationsContextValue | undefined>(undefined);

// ✅ Theme provider with memoization
const ThemeProvider: React.FC<{ children: React.ReactNode }> = ({ children }) => {
  const [theme, setTheme] = useState<'light' | 'dark'>('light');

  const value = useMemo(
    () => ({ theme, setTheme }),
    [theme]
  );

  return <ThemeContext.Provider value={value}>{children}</ThemeContext.Provider>;
};

// ✅ Auth provider
const AuthProvider: React.FC<{ children: React.ReactNode }> = ({ children }) => {
  const [user, setUser] = useState<User | null>(null);

  const value = useMemo(
    () => ({ user, setUser }),
    [user]
  );

  return <AuthContext.Provider value={value}>{children}</AuthContext.Provider>;
};

// ✅ Composed providers
const App: React.FC = () => {
  return (
    <AuthProvider>
      <ThemeProvider>
        <DonationsProvider>
          <AppContent />
        </DonationsProvider>
      </ThemeProvider>
    </AuthProvider>
  );
};

// ✅ Component only re-renders when theme changes
const ThemeToggle: React.FC = () => {
  const { theme, setTheme } = useContext(ThemeContext)!;
  // Only re-renders on theme changes!

  return (
    <button onClick={() => setTheme(theme === 'light' ? 'dark' : 'light')}>
      {theme}
    </button>
  );
};
```

## 10. Context Without Memoization

### ❌ FAIL - Context Value Not Memoized

**Problem**: Creating new context value object on every render causes unnecessary re-renders of all consumers.

```typescript
const AuthProvider: React.FC<{ children: React.ReactNode }> = ({ children }) => {
  const [user, setUser] = useState<User | null>(null);

  // ❌ New object created on every render!
  const value = {
    user,
    setUser,
  };

  return <AuthContext.Provider value={value}>{children}</AuthContext.Provider>;
};
```

**Issues**:

- New object reference on every parent render
- All context consumers re-render unnecessarily
- Performance degrades with many consumers

### ✅ PASS - Memoize Context Value

```typescript
const AuthProvider: React.FC<{ children: React.ReactNode }> = ({ children }) => {
  const [user, setUser] = useState<User | null>(null);

  // ✅ Stable reference - only changes when user changes
  const value = useMemo(
    () => ({
      user,
      setUser,
    }),
    [user]
  );

  return <AuthContext.Provider value={value}>{children}</AuthContext.Provider>;
};
```

## 11. Any Type in TypeScript

### ❌ FAIL - Losing Type Safety

**Problem**: Using `any` type defeats the purpose of TypeScript and allows bugs.

```typescript
// ❌ any everywhere
const calculateZakat = (data: any) => {
  return data.wealth * 0.025; // No type checking!
};

interface BadProps {
  donation: any; // ❌ Could be anything
  onSubmit: (value: any) => void; // ❌ No validation
}

const BadComponent: React.FC<BadProps> = ({ donation, onSubmit }) => {
  // ❌ TypeScript can't help us here
  return <div>{donation.amount}</div>;
};
```

**Issues**:

- No compile-time type checking
- Runtime errors from typos or wrong types
- No IDE autocomplete
- Defeats purpose of TypeScript

### ✅ PASS - Proper Types

```typescript
// ✅ Explicit types
interface ZakatData {
  wealth: number;
  nisab: number;
}

const calculateZakat = (data: ZakatData): number => {
  return data.wealth * 0.025; // Type-safe!
};

interface Donation {
  id: string;
  amount: number;
  category: string;
  date: Date;
}

interface GoodProps {
  donation: Donation;
  onSubmit: (donation: Donation) => void;
}

const GoodComponent: React.FC<GoodProps> = ({ donation, onSubmit }) => {
  // ✅ TypeScript validates everything
  return (
    <div>
      <p>{donation.amount}</p> {/* Autocomplete works */}
      <button onClick={() => onSubmit(donation)}>Submit</button>
    </div>
  );
};
```

## 12. Excessive Type Assertions

### ❌ FAIL - Forcing Types with `as`

**Problem**: Using type assertions (`as`) instead of proper type guards bypasses type safety.

```typescript
const DonationList: React.FC = () => {
  const [donations, setDonations] = useState<unknown>(null);

  useEffect(() => {
    fetch('/api/donations')
      .then((res) => res.json())
      .then((data) => {
        // ❌ Unsafe type assertion!
        setDonations(data as Donation[]);
      });
  }, []);

  // ❌ Another unsafe assertion
  return (
    <div>
      {(donations as Donation[]).map((d) => (
        <div key={d.id}>{d.amount}</div>
      ))}
    </div>
  );
};
```

**Issues**:

- Bypasses type checking
- Runtime errors if data shape doesn't match
- No validation
- False sense of type safety

### ✅ PASS - Type Guards and Validation

```typescript
// ✅ Type guard
function isDonation(value: unknown): value is Donation {
  return (
    typeof value === 'object' &&
    value !== null &&
    'id' in value &&
    typeof (value as any).id === 'string' &&
    'amount' in value &&
    typeof (value as any).amount === 'number' &&
    'category' in value &&
    typeof (value as any).category === 'string'
  );
}

function isDonationArray(value: unknown): value is Donation[] {
  return Array.isArray(value) && value.every(isDonation);
}

const DonationList: React.FC = () => {
  const [donations, setDonations] = useState<Donation[]>([]);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    fetch('/api/donations')
      .then((res) => res.json())
      .then((data) => {
        // ✅ Validate before using
        if (isDonationArray(data)) {
          setDonations(data);
        } else {
          setError('Invalid data format');
        }
      })
      .catch(() => setError('Failed to fetch'));
  }, []);

  if (error) return <div>Error: {error}</div>;

  // ✅ Type-safe
  return (
    <div>
      {donations.map((d) => (
        <div key={d.id}>{d.amount}</div>
      ))}
    </div>
  );
};
```

## 13. Missing Prop Types

### ❌ FAIL - No TypeScript Interfaces

**Problem**: Not typing component props loses type safety and documentation.

```typescript
// ❌ No prop types
const BadComponent = ({ donation, onDelete }) => {
  return (
    <div>
      <span>{donation.amount}</span>
      <button onClick={() => onDelete(donation.id)}>Delete</button>
    </div>
  );
};
```

**Issues**:

- No type checking
- No IDE autocomplete
- No prop validation
- Runtime errors from typos

### ✅ PASS - Explicit Prop Types

```typescript
// ✅ Explicit interface
interface DonationItemProps {
  donation: Donation;
  onDelete: (id: string) => void;
}

const GoodComponent: React.FC<DonationItemProps> = ({ donation, onDelete }) => {
  return (
    <div>
      <span>{donation.amount}</span>
      <button onClick={() => onDelete(donation.id)}>Delete</button>
    </div>
  );
};
```

## 14. Not Cleaning up Effects

### ❌ FAIL - Memory Leaks from Subscriptions

**Problem**: Not cleaning up subscriptions, timers, or event listeners in useEffect.

```typescript
const DonationTracker: React.FC = () => {
  const [total, setTotal] = useState(0);

  useEffect(() => {
    // ❌ No cleanup - memory leak!
    const interval = setInterval(() => {
      fetch('/api/donations/total')
        .then((res) => res.json())
        .then((data) => setTotal(data.total));
    }, 5000);

    // ❌ Missing cleanup function
  }, []);

  return <div>Total Donations: ${total}</div>;
};
```

**Issues**:

- Memory leaks
- Interval continues after unmount
- Multiple intervals if component remounts
- setState on unmounted component warnings

### ✅ PASS - Cleanup Functions

```typescript
const DonationTracker: React.FC = () => {
  const [total, setTotal] = useState(0);

  useEffect(() => {
    const interval = setInterval(() => {
      fetch('/api/donations/total')
        .then((res) => res.json())
        .then((data) => setTotal(data.total));
    }, 5000);

    // ✅ Cleanup function
    return () => {
      clearInterval(interval);
    };
  }, []);

  return <div>Total Donations: ${total}</div>;
};

// ✅ Cleanup with AbortController
const DataFetcher: React.FC = () => {
  const [data, setData] = useState(null);

  useEffect(() => {
    const controller = new AbortController();

    fetch('/api/data', { signal: controller.signal })
      .then((res) => res.json())
      .then((data) => setData(data))
      .catch((err) => {
        if (err.name !== 'AbortError') {
          console.error('Fetch error:', err);
        }
      });

    return () => {
      controller.abort(); // ✅ Cancel pending requests
    };
  }, []);

  return <div>{data ? JSON.stringify(data) : 'Loading...'}</div>;
};
```

## 15. Race Conditions in Effects

### ❌ FAIL - Not Handling Async Races

**Problem**: Not handling component unmount or rapid state changes in async effects.

```typescript
const UserProfile: React.FC<{ userId: string }> = ({ userId }) => {
  const [user, setUser] = useState<User | null>(null);

  useEffect(() => {
    // ❌ Race condition!
    fetch(`/api/users/${userId}`)
      .then((res) => res.json())
      .then((data) => {
        // Component may have unmounted or userId changed
        setUser(data);
      });
  }, [userId]);

  return <div>{user?.name}</div>;
};
```

**Issues**:

- Race conditions when userId changes quickly
- setState on unmounted component
- Shows wrong user data

### ✅ PASS - Handle Cancellation

```typescript
const UserProfile: React.FC<{ userId: string }> = ({ userId }) => {
  const [user, setUser] = useState<User | null>(null);

  useEffect(() => {
    let cancelled = false;

    const fetchUser = async () => {
      try {
        const response = await fetch(`/api/users/${userId}`);
        const data = await response.json();

        // ✅ Only update if not cancelled
        if (!cancelled) {
          setUser(data);
        }
      } catch (error) {
        if (!cancelled) {
          console.error('Failed to fetch user:', error);
        }
      }
    };

    fetchUser();

    // ✅ Cleanup cancels the effect
    return () => {
      cancelled = true;
    };
  }, [userId]);

  return <div>{user?.name}</div>;
};
```

## 16. Index as Key in Lists

### ❌ FAIL - Using Array Index

**Problem**: Using array index as key causes bugs when list items are reordered or removed.

```typescript
const PaymentList: React.FC<{ payments: Payment[] }> = ({ payments }) => {
  return (
    <ul>
      {payments.map((payment, index) => (
        // ❌ Using index as key
        <li key={index}>
          <input type="checkbox" />
          {payment.amount}
        </li>
      ))}
    </ul>
  );
};
```

**Issues**:

- React reuses components based on key
- Checkbox states get mixed up when list reorders
- Component state persists to wrong items
- Breaks React reconciliation

### ✅ PASS - Use Unique IDs

```typescript
const PaymentList: React.FC<{ payments: Payment[] }> = ({ payments }) => {
  return (
    <ul>
      {payments.map((payment) => (
        // ✅ Using unique ID
        <li key={payment.id}>
          <input type="checkbox" />
          {payment.amount}
        </li>
      ))}
    </ul>
  );
};
```

## 17. Non-Unique Keys

### ❌ FAIL - Duplicate or Non-Unique Keys

**Problem**: Using non-unique values as keys causes state mismatches.

```typescript
const CategoryList: React.FC<{ categories: string[] }> = ({ categories }) => {
  return (
    <ul>
      {categories.map((category) => (
        // ❌ Not unique if duplicates exist
        <li key={category}>{category}</li>
      ))}
    </ul>
  );
};

// Example: categories = ["Zakat", "Sadaqah", "Zakat"]
// Two "Zakat" items have same key!
```

**Issues**:

- React warns about duplicate keys
- State gets mixed up between duplicate items
- Reconciliation errors

### ✅ PASS - Ensure Uniqueness

```typescript
interface Category {
  id: string;
  name: string;
}

const CategoryList: React.FC<{ categories: Category[] }> = ({ categories }) => {
  return (
    <ul>
      {categories.map((category) => (
        // ✅ Unique ID
        <li key={category.id}>{category.name}</li>
      ))}
    </ul>
  );
};

// ✅ If only strings available, generate stable IDs
const CategoryListFromStrings: React.FC<{ categories: string[] }> = ({ categories }) => {
  const categoriesWithIds = useMemo(
    () => categories.map((name, index) => ({ id: `cat-${index}-${name}`, name })),
    [categories]
  );

  return (
    <ul>
      {categoriesWithIds.map((category) => (
        <li key={category.id}>{category.name}</li>
      ))}
    </ul>
  );
};
```

## 18. Missing Accessibility

### ❌ FAIL - No ARIA Labels or Keyboard Support

**Problem**: Not providing proper accessibility attributes excludes users with disabilities.

```typescript
const DonationForm: React.FC = () => {
  const [amount, setAmount] = useState(0);

  return (
    <div>
      {/* ❌ No label for screen readers */}
      <input
        type="number"
        value={amount}
        onChange={(e) => setAmount(Number(e.target.value))}
      />

      {/* ❌ div with onClick - not keyboard accessible */}
      <div onClick={() => console.log('Donate')}>
        Donate
      </div>

      {/* ❌ No alt text */}
      <img src="/zakat-icon.png" />
    </div>
  );
};
```

**Issues**:

- Screen readers cannot identify inputs
- Keyboard users cannot interact with div buttons
- Images have no descriptions
- Violates WCAG guidelines

### ✅ PASS - Proper Accessibility

```typescript
const DonationForm: React.FC = () => {
  const [amount, setAmount] = useState(0);

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    console.log('Donate:', amount);
  };

  return (
    <form onSubmit={handleSubmit}>
      {/* ✅ Label for screen readers */}
      <label htmlFor="donation-amount">Donation Amount</label>
      <input
        id="donation-amount"
        type="number"
        value={amount}
        onChange={(e) => setAmount(Number(e.target.value))}
        aria-label="Enter donation amount in USD"
        aria-required="true"
      />

      {/* ✅ Proper button element */}
      <button type="submit">Donate</button>

      {/* ✅ Alt text for images */}
      <img src="/zakat-icon.png" alt="Zakat donation icon showing scales of justice" />
    </form>
  );
};
```

## 19. Client-Side Secrets

### ❌ FAIL - Exposing Secrets in Frontend

**Problem**: Hardcoding API keys or secrets in client code exposes them to attackers.

```typescript
// ❌ NEVER DO THIS!
const API_KEY = 'sk_live_secret_key_12345';

const PaymentComponent: React.FC = () => {
  const processPayment = async () => {
    // ❌ Secret exposed in frontend code!
    const response = await fetch('https://api.stripe.com/v1/charges', {
      method: 'POST',
      headers: {
        Authorization: `Bearer ${API_KEY}`, // ❌ Anyone can see this!
      },
      body: JSON.stringify({ amount: 1000 }),
    });
  };

  return <button onClick={processPayment}>Pay</button>;
};
```

**Issues**:

- API keys visible in client bundle
- Attackers can extract and abuse secrets
- Security breach
- Violates best practices

### ✅ PASS - Backend Proxy for Secrets

```typescript
// ✅ No secrets in frontend
const PaymentComponent: React.FC = () => {
  const processPayment = async () => {
    // ✅ Call backend endpoint - backend handles secrets
    const response = await fetch('/api/process-payment', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({ amount: 1000 }),
    });

    return response.json();
  };

  return <button onClick={processPayment}>Pay</button>;
};

// ✅ Backend handles secrets (Node.js example)
// app.post('/api/process-payment', async (req, res) => {
//   const stripe = require('stripe')(process.env.STRIPE_SECRET_KEY);
//   const charge = await stripe.charges.create({
//     amount: req.body.amount,
//     currency: 'usd',
//     source: req.body.token,
//   });
//   res.json(charge);
// });
```

## Related Documentation

- **[React Idioms](./ex-so-plwe-fera__idioms.md)** - Idiomatic patterns
- **[React Best Practices](./ex-so-plwe-fera__best-practices.md)** - Production standards
- **[Performance](./ex-so-plwe-fera__performance.md)** - Optimization strategies
- **[TypeScript Anti-Patterns](../../prog-lang/typescript/ex-so-prla-ts__anti-patterns.md)** - TypeScript language anti-patterns
- **[Security Best Practices](./ex-so-plwe-fera__security.md)** - Frontend security

---

**Last Updated**: 2026-01-29
**React Version**: 18+
**TypeScript Version**: 5+
