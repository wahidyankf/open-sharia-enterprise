---
title: "React + TypeScript Idioms"
description: React + TypeScript-specific patterns and idiomatic framework usage
category: explanation
subcategory: platform-web
tags:
  - react
  - typescript
  - idioms
  - patterns
  - hooks
  - components
related:
  - ./ex-so-plwe-fera__best-practices.md
  - ./ex-so-plwe-fera__anti-patterns.md
principles:
  - automation-over-manual
  - explicit-over-implicit
  - immutability
  - pure-functions
last_updated: 2026-01-29
---

# React + TypeScript Idioms

## Quick Reference

### Core React + TypeScript Patterns

**Component Patterns**:

- [Functional Components with TypeScript](#1-functional-components-with-typescript) - Typing components and props
- [Props Interface Patterns](#2-props-interface-patterns) - Interface design for props
- [Custom Hooks for Logic Reuse](#3-custom-hooks-for-logic-reuse) - Extracting stateful logic

**State Management**:

- [useState and useReducer](#4-usestate-and-usereducer) - Local state patterns
- [Context Pattern for Global State](#5-context-pattern-for-global-state) - Avoiding prop drilling
- [TypeScript Discriminated Unions](#6-typescript-discriminated-unions) - Type-safe state

**Performance and Effects**:

- [Effect Hook Patterns](#7-effect-hook-patterns) - useEffect, cleanup, dependencies
- [Memoization Patterns](#8-memoization-patterns) - useMemo, useCallback, React.memo
- [Error Boundaries for Resilience](#9-error-boundaries-for-resilience) - Error handling

**Design Patterns**:

- [Composition over Inheritance](#10-composition-over-inheritance) - Component composition
- [Controlled Components for Forms](#11-controlled-components-for-forms) - Form handling
- [Render Props and Higher-Order Components](#12-render-props-and-higher-order-components) - Advanced patterns

### Related Documentation

- [Best Practices](ex-so-plwe-fere__best-practices.md)
- [Anti-Patterns](ex-so-plwe-fere__anti-patterns.md)
- [TypeScript Best Practices](../../prog-lang/typescript/ex-so-prla-ty__best-practices.md)
- [Functional Programming](../../prog-lang/typescript/ex-so-prla-ty__functional-programming.md)

## Overview

React + TypeScript idioms are established patterns that leverage React's component model, hooks system, and TypeScript's type safety. These patterns align with React's philosophy of declarative UIs, unidirectional data flow, and functional programming principles.

This guide focuses on **React 18+ idioms** with TypeScript 5+, incorporating examples from Islamic financial domains including Zakat calculation, donation forms, and contract dashboards.

### Why React + TypeScript Idioms Matter

- **Type Safety**: Catch errors at compile time instead of runtime
- **Developer Experience**: IDE autocomplete, refactoring support, inline documentation
- **Maintainability**: Self-documenting code with clear interfaces
- **Scalability**: Patterns designed for large applications
- **Performance**: Efficient rendering through memoization and optimization

### Target Audience

This document targets developers building React applications in the open-sharia-enterprise platform, particularly those working on interactive financial UIs and type-safe component architectures.

## Core React + TypeScript Idioms

### 1. Functional Components with TypeScript

**Pattern**: Define components as typed functions with explicit props interfaces.

**Idiom**: Components are typed functions - props in, JSX out.

**Basic Component**:

```typescript
interface GreetingProps {
  name: string;
  age?: number;
}

function Greeting({ name, age }: GreetingProps): JSX.Element {
  return (
    <div>
      <h1>Hello, {name}!</h1>
      {age && <p>You are {age} years old.</p>}
    </div>
  );
}
```

**Component with Children**:

```typescript
interface CardProps {
  title: string;
  children: React.ReactNode;
  variant?: "primary" | "secondary";
}

function Card({ title, children, variant = "primary" }: CardProps): JSX.Element {
  return (
    <div className={`card card-${variant}`}>
      <h2>{title}</h2>
      <div className="card-content">{children}</div>
    </div>
  );
}
```

**Component with Event Handlers**:

```typescript
interface ButtonProps {
  label: string;
  onClick: (event: React.MouseEvent<HTMLButtonElement>) => void;
  disabled?: boolean;
  type?: "button" | "submit" | "reset";
}

function Button({
  label,
  onClick,
  disabled = false,
  type = "button",
}: ButtonProps): JSX.Element {
  return (
    <button type={type} onClick={onClick} disabled={disabled}>
      {label}
    </button>
  );
}
```

**Benefits**:

- Type checking prevents prop errors
- IDE autocomplete for props
- Refactoring support
- Self-documenting components

### 2. Props Interface Patterns

**Pattern**: Design props interfaces for flexibility, reusability, and type safety.

**Idiom**: Interfaces define contracts - explicit is better than implicit.

**Generic Props Interface**:

```typescript
interface ZakatCalculationDisplayProps {
  calculation: {
    id: string;
    wealth: number;
    nisab: number;
    zakatAmount: number;
    eligible: boolean;
    calculationDate: Date;
  };
  onEdit?: () => void;
  onDelete?: () => void;
}

function ZakatCalculationDisplay({
  calculation,
  onEdit,
  onDelete,
}: ZakatCalculationDisplayProps): JSX.Element {
  return (
    <div className="calculation-card">
      <h3>Zakat Calculation - {calculation.calculationDate.toLocaleDateString()}</h3>
      <div className="calculation-details">
        <p>Wealth: ${calculation.wealth.toFixed(2)}</p>
        <p>Nisab: ${calculation.nisab.toFixed(2)}</p>
        <p>Zakat Amount: ${calculation.zakatAmount.toFixed(2)}</p>
        <p>Eligible: {calculation.eligible ? "Yes" : "No"}</p>
      </div>
      <div className="actions">
        {onEdit && <button onClick={onEdit}>Edit</button>}
        {onDelete && <button onClick={onDelete}>Delete</button>}
      </div>
    </div>
  );
}
```

**Props with Discriminated Unions**:

```typescript
interface LoadingState {
  status: "loading";
}

interface ErrorState {
  status: "error";
  message: string;
}

interface SuccessState {
  status: "success";
  data: {
    totalDonations: number;
    totalAmount: number;
    recentDonations: Array<{
      id: string;
      amount: number;
      date: Date;
    }>;
  };
}

type DonationDashboardState = LoadingState | ErrorState | SuccessState;

interface DonationDashboardProps {
  state: DonationDashboardState;
  onRetry?: () => void;
}

function DonationDashboard({ state, onRetry }: DonationDashboardProps): JSX.Element {
  if (state.status === "loading") {
    return <div>Loading donations...</div>;
  }

  if (state.status === "error") {
    return (
      <div className="error">
        <p>Error: {state.message}</p>
        {onRetry && <button onClick={onRetry}>Retry</button>}
      </div>
    );
  }

  // TypeScript knows state.status === "success" here
  return (
    <div className="dashboard">
      <h2>Donation Dashboard</h2>
      <div className="stats">
        <p>Total Donations: {state.data.totalDonations}</p>
        <p>Total Amount: ${state.data.totalAmount.toFixed(2)}</p>
      </div>
      <div className="recent">
        <h3>Recent Donations</h3>
        {state.data.recentDonations.map((donation) => (
          <div key={donation.id}>
            ${donation.amount} - {donation.date.toLocaleDateString()}
          </div>
        ))}
      </div>
    </div>
  );
}
```

**Generic Component Props**:

```typescript
interface ListProps<T> {
  items: T[];
  renderItem: (item: T, index: number) => React.ReactNode;
  keyExtractor: (item: T) => string;
  emptyMessage?: string;
}

function List<T>({
  items,
  renderItem,
  keyExtractor,
  emptyMessage = "No items",
}: ListProps<T>): JSX.Element {
  if (items.length === 0) {
    return <div className="empty">{emptyMessage}</div>;
  }

  return (
    <ul>
      {items.map((item, index) => (
        <li key={keyExtractor(item)}>{renderItem(item, index)}</li>
      ))}
    </ul>
  );
}

// Usage with Zakat calculations
interface ZakatCalculation {
  id: string;
  wealth: number;
  zakatAmount: number;
  calculationDate: Date;
}

function ZakatList({ calculations }: { calculations: ZakatCalculation[] }): JSX.Element {
  return (
    <List
      items={calculations}
      keyExtractor={(calc) => calc.id}
      renderItem={(calc) => (
        <div>
          {calc.calculationDate.toLocaleDateString()}: ${calc.zakatAmount.toFixed(2)}
        </div>
      )}
      emptyMessage="No calculations yet"
    />
  );
}
```

### 3. Custom Hooks for Logic Reuse

**Pattern**: Extract stateful logic into reusable custom hooks.

**Idiom**: Hooks extract behavior - share logic without sharing components.

**Basic Custom Hook**:

```typescript
function useZakatCalculation() {
  const [wealth, setWealth] = React.useState<number>(0);
  const [nisab, setNisab] = React.useState<number>(0);

  const zakatAmount = React.useMemo(() => {
    if (wealth >= nisab && nisab > 0) {
      return wealth * 0.025;
    }
    return 0;
  }, [wealth, nisab]);

  const isEligible = wealth >= nisab && nisab > 0;

  return {
    wealth,
    setWealth,
    nisab,
    setNisab,
    zakatAmount,
    isEligible,
  };
}

// Usage
function ZakatCalculator(): JSX.Element {
  const { wealth, setWealth, nisab, setNisab, zakatAmount, isEligible } =
    useZakatCalculation();

  return (
    <div>
      <input
        type="number"
        value={wealth}
        onChange={(e) => setWealth(parseFloat(e.target.value) || 0)}
        placeholder="Wealth"
      />
      <input
        type="number"
        value={nisab}
        onChange={(e) => setNisab(parseFloat(e.target.value) || 0)}
        placeholder="Nisab"
      />
      <div>
        <p>Zakat Amount: ${zakatAmount.toFixed(2)}</p>
        <p>Eligible: {isEligible ? "Yes" : "No"}</p>
      </div>
    </div>
  );
}
```

**Custom Hook with API Call**:

```typescript
interface UseApiOptions<T> {
  url: string;
  initialData?: T;
  autoFetch?: boolean;
}

interface UseApiResult<T> {
  data: T | null;
  loading: boolean;
  error: Error | null;
  refetch: () => Promise<void>;
}

function useApi<T>({
  url,
  initialData = null,
  autoFetch = true,
}: UseApiOptions<T>): UseApiResult<T> {
  const [data, setData] = React.useState<T | null>(initialData);
  const [loading, setLoading] = React.useState<boolean>(false);
  const [error, setError] = React.useState<Error | null>(null);

  const fetchData = React.useCallback(async () => {
    setLoading(true);
    setError(null);

    try {
      const response = await fetch(url);
      if (!response.ok) {
        throw new Error(`HTTP error: ${response.status}`);
      }
      const result = await response.json();
      setData(result);
    } catch (err) {
      setError(err instanceof Error ? err : new Error("Unknown error"));
    } finally {
      setLoading(false);
    }
  }, [url]);

  React.useEffect(() => {
    if (autoFetch) {
      fetchData();
    }
  }, [autoFetch, fetchData]);

  return { data, loading, error, refetch: fetchData };
}

// Usage
interface DonationCampaign {
  id: string;
  name: string;
  goal: number;
  raised: number;
}

function DonationCampaignList(): JSX.Element {
  const { data, loading, error, refetch } = useApi<DonationCampaign[]>({
    url: "/api/campaigns",
  });

  if (loading) return <div>Loading campaigns...</div>;
  if (error) return <div>Error: {error.message}</div>;
  if (!data) return <div>No campaigns</div>;

  return (
    <div>
      <button onClick={refetch}>Refresh</button>
      {data.map((campaign) => (
        <div key={campaign.id}>
          <h3>{campaign.name}</h3>
          <p>
            ${campaign.raised} / ${campaign.goal}
          </p>
        </div>
      ))}
    </div>
  );
}
```

**Custom Hook with Local Storage**:

```typescript
function useLocalStorage<T>(key: string, initialValue: T): [T, (value: T) => void] {
  const [storedValue, setStoredValue] = React.useState<T>(() => {
    try {
      const item = window.localStorage.getItem(key);
      return item ? JSON.parse(item) : initialValue;
    } catch (error) {
      console.error(`Error loading localStorage key "${key}":`, error);
      return initialValue;
    }
  });

  const setValue = React.useCallback(
    (value: T) => {
      try {
        setStoredValue(value);
        window.localStorage.setItem(key, JSON.stringify(value));
      } catch (error) {
        console.error(`Error setting localStorage key "${key}":`, error);
      }
    },
    [key]
  );

  return [storedValue, setValue];
}

// Usage
interface UserPreferences {
  theme: "light" | "dark";
  currency: string;
  language: string;
}

function UserSettings(): JSX.Element {
  const [preferences, setPreferences] = useLocalStorage<UserPreferences>(
    "userPreferences",
    {
      theme: "light",
      currency: "USD",
      language: "en",
    }
  );

  return (
    <div>
      <label>
        Theme:
        <select
          value={preferences.theme}
          onChange={(e) =>
            setPreferences({
              ...preferences,
              theme: e.target.value as "light" | "dark",
            })
          }
        >
          <option value="light">Light</option>
          <option value="dark">Dark</option>
        </select>
      </label>
    </div>
  );
}
```

### 4. useState and useReducer

**Pattern**: Use useState for simple state, useReducer for complex state transitions.

**Idiom**: Simple state → useState, complex logic → useReducer.

**useState for Simple State**:

```typescript
function DonationForm(): JSX.Element {
  const [amount, setAmount] = React.useState<number>(0);
  const [donorName, setDonorName] = React.useState<string>("");
  const [message, setMessage] = React.useState<string>("");
  const [submitting, setSubmitting] = React.useState<boolean>(false);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setSubmitting(true);

    try {
      await fetch("/api/donations", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ amount, donorName, message }),
      });

      // Reset form
      setAmount(0);
      setDonorName("");
      setMessage("");
    } catch (error) {
      console.error("Donation failed:", error);
    } finally {
      setSubmitting(false);
    }
  };

  return (
    <form onSubmit={handleSubmit}>
      <input
        type="number"
        value={amount}
        onChange={(e) => setAmount(parseFloat(e.target.value) || 0)}
        placeholder="Amount"
      />
      <input
        type="text"
        value={donorName}
        onChange={(e) => setDonorName(e.target.value)}
        placeholder="Name"
      />
      <textarea
        value={message}
        onChange={(e) => setMessage(e.target.value)}
        placeholder="Message"
      />
      <button type="submit" disabled={submitting}>
        {submitting ? "Submitting..." : "Donate"}
      </button>
    </form>
  );
}
```

**useReducer for Complex State**:

```typescript
interface MurabahaContract {
  assetCost: number;
  profitRate: number;
  termMonths: number;
  monthlyPayment: number;
  totalAmount: number;
}

type ContractState =
  | { status: "editing"; draft: Partial<MurabahaContract> }
  | { status: "previewing"; contract: MurabahaContract }
  | { status: "submitting"; contract: MurabahaContract }
  | { status: "submitted"; contract: MurabahaContract; contractId: string }
  | { status: "error"; error: string };

type ContractAction =
  | { type: "UPDATE_DRAFT"; field: keyof MurabahaContract; value: number }
  | { type: "PREVIEW" }
  | { type: "EDIT" }
  | { type: "SUBMIT" }
  | { type: "SUBMIT_SUCCESS"; contractId: string }
  | { type: "SUBMIT_ERROR"; error: string };

function calculateContract(
  draft: Partial<MurabahaContract>
): MurabahaContract | null {
  const { assetCost, profitRate, termMonths } = draft;

  if (
    assetCost === undefined ||
    profitRate === undefined ||
    termMonths === undefined
  ) {
    return null;
  }

  const totalAmount = assetCost * (1 + profitRate);
  const monthlyPayment = totalAmount / termMonths;

  return {
    assetCost,
    profitRate,
    termMonths,
    monthlyPayment,
    totalAmount,
  };
}

function contractReducer(state: ContractState, action: ContractAction): ContractState {
  switch (action.type) {
    case "UPDATE_DRAFT":
      if (state.status !== "editing") return state;
      return {
        ...state,
        draft: { ...state.draft, [action.field]: action.value },
      };

    case "PREVIEW":
      if (state.status !== "editing") return state;
      const contract = calculateContract(state.draft);
      if (!contract) return state;
      return { status: "previewing", contract };

    case "EDIT":
      if (state.status !== "previewing") return state;
      return { status: "editing", draft: state.contract };

    case "SUBMIT":
      if (state.status !== "previewing") return state;
      return { status: "submitting", contract: state.contract };

    case "SUBMIT_SUCCESS":
      if (state.status !== "submitting") return state;
      return {
        status: "submitted",
        contract: state.contract,
        contractId: action.contractId,
      };

    case "SUBMIT_ERROR":
      if (state.status !== "submitting") return state;
      return { status: "error", error: action.error };

    default:
      return state;
  }
}

function MurabahaContractForm(): JSX.Element {
  const [state, dispatch] = React.useReducer(contractReducer, {
    status: "editing",
    draft: {},
  });

  const handleFieldChange = (field: keyof MurabahaContract, value: number) => {
    dispatch({ type: "UPDATE_DRAFT", field, value });
  };

  const handleSubmit = async () => {
    if (state.status !== "previewing") return;

    dispatch({ type: "SUBMIT" });

    try {
      const response = await fetch("/api/contracts", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(state.contract),
      });

      const { id } = await response.json();
      dispatch({ type: "SUBMIT_SUCCESS", contractId: id });
    } catch (error) {
      dispatch({
        type: "SUBMIT_ERROR",
        error: error instanceof Error ? error.message : "Unknown error",
      });
    }
  };

  if (state.status === "editing") {
    return (
      <div>
        <h2>Create Murabaha Contract</h2>
        <input
          type="number"
          value={state.draft.assetCost || ""}
          onChange={(e) =>
            handleFieldChange("assetCost", parseFloat(e.target.value) || 0)
          }
          placeholder="Asset Cost"
        />
        <input
          type="number"
          value={state.draft.profitRate || ""}
          onChange={(e) =>
            handleFieldChange("profitRate", parseFloat(e.target.value) || 0)
          }
          placeholder="Profit Rate"
        />
        <input
          type="number"
          value={state.draft.termMonths || ""}
          onChange={(e) =>
            handleFieldChange("termMonths", parseInt(e.target.value) || 0)
          }
          placeholder="Term (Months)"
        />
        <button onClick={() => dispatch({ type: "PREVIEW" })}>Preview</button>
      </div>
    );
  }

  if (state.status === "previewing") {
    return (
      <div>
        <h2>Contract Preview</h2>
        <p>Asset Cost: ${state.contract.assetCost.toFixed(2)}</p>
        <p>Profit Rate: {(state.contract.profitRate * 100).toFixed(2)}%</p>
        <p>Term: {state.contract.termMonths} months</p>
        <p>Monthly Payment: ${state.contract.monthlyPayment.toFixed(2)}</p>
        <p>Total Amount: ${state.contract.totalAmount.toFixed(2)}</p>
        <button onClick={() => dispatch({ type: "EDIT" })}>Edit</button>
        <button onClick={handleSubmit}>Submit</button>
      </div>
    );
  }

  if (state.status === "submitting") {
    return <div>Submitting contract...</div>;
  }

  if (state.status === "submitted") {
    return (
      <div>
        <h2>Contract Submitted</h2>
        <p>Contract ID: {state.contractId}</p>
      </div>
    );
  }

  if (state.status === "error") {
    return <div>Error: {state.error}</div>;
  }

  return <div>Unknown state</div>;
}
```

### 5. Context Pattern for Global State

**Pattern**: Use React Context to avoid prop drilling for global state.

**Idiom**: Context provides, components consume - pass data deep without props.

**Context Definition**:

```typescript
interface User {
  id: string;
  name: string;
  email: string;
  verified: boolean;
}

interface AuthContextValue {
  user: User | null;
  loading: boolean;
  login: (email: string, password: string) => Promise<void>;
  logout: () => Promise<void>;
}

const AuthContext = React.createContext<AuthContextValue | undefined>(undefined);

function useAuth(): AuthContextValue {
  const context = React.useContext(AuthContext);
  if (!context) {
    throw new Error("useAuth must be used within AuthProvider");
  }
  return context;
}

interface AuthProviderProps {
  children: React.ReactNode;
}

function AuthProvider({ children }: AuthProviderProps): JSX.Element {
  const [user, setUser] = React.useState<User | null>(null);
  const [loading, setLoading] = React.useState<boolean>(true);

  React.useEffect(() => {
    // Check if user is logged in on mount
    const checkAuth = async () => {
      try {
        const response = await fetch("/api/auth/me");
        if (response.ok) {
          const userData = await response.json();
          setUser(userData);
        }
      } catch (error) {
        console.error("Auth check failed:", error);
      } finally {
        setLoading(false);
      }
    };

    checkAuth();
  }, []);

  const login = async (email: string, password: string) => {
    const response = await fetch("/api/auth/login", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ email, password }),
    });

    if (!response.ok) {
      throw new Error("Login failed");
    }

    const userData = await response.json();
    setUser(userData);
  };

  const logout = async () => {
    await fetch("/api/auth/logout", { method: "POST" });
    setUser(null);
  };

  const value: AuthContextValue = {
    user,
    loading,
    login,
    logout,
  };

  return <AuthContext.Provider value={value}>{children}</AuthContext.Provider>;
}

// Usage in components
function DonationForm(): JSX.Element {
  const { user } = useAuth();

  if (!user) {
    return <div>Please log in to donate</div>;
  }

  if (!user.verified) {
    return <div>Please verify your email to donate</div>;
  }

  return <div>Donation form for {user.name}</div>;
}

// App setup
function App(): JSX.Element {
  return (
    <AuthProvider>
      <DonationForm />
    </AuthProvider>
  );
}
```

**Multiple Contexts**:

```typescript
interface ThemeContextValue {
  theme: "light" | "dark";
  toggleTheme: () => void;
}

const ThemeContext = React.createContext<ThemeContextValue | undefined>(undefined);

function useTheme(): ThemeContextValue {
  const context = React.useContext(ThemeContext);
  if (!context) {
    throw new Error("useTheme must be used within ThemeProvider");
  }
  return context;
}

function ThemeProvider({ children }: { children: React.ReactNode }): JSX.Element {
  const [theme, setTheme] = React.useState<"light" | "dark">("light");

  const toggleTheme = () => {
    setTheme((prev) => (prev === "light" ? "dark" : "light"));
  };

  return (
    <ThemeContext.Provider value={{ theme, toggleTheme }}>
      {children}
    </ThemeContext.Provider>
  );
}

// Composing multiple providers
function AppProviders({ children }: { children: React.ReactNode }): JSX.Element {
  return (
    <AuthProvider>
      <ThemeProvider>{children}</ThemeProvider>
    </AuthProvider>
  );
}
```

### 6. TypeScript Discriminated Unions

**Pattern**: Use discriminated unions for type-safe state management.

**Idiom**: Discriminated unions encode state - TypeScript narrows types automatically.

**API Request State**:

```typescript
type ApiState<T> =
  | { status: "idle" }
  | { status: "loading" }
  | { status: "success"; data: T }
  | { status: "error"; error: string };

interface ZakatCalculation {
  id: string;
  wealth: number;
  zakatAmount: number;
}

function ZakatCalculationList(): JSX.Element {
  const [state, setState] = React.useState<ApiState<ZakatCalculation[]>>({
    status: "idle",
  });

  React.useEffect(() => {
    const fetchCalculations = async () => {
      setState({ status: "loading" });

      try {
        const response = await fetch("/api/zakat/calculations");
        if (!response.ok) {
          throw new Error(`HTTP error: ${response.status}`);
        }
        const data = await response.json();
        setState({ status: "success", data });
      } catch (error) {
        setState({
          status: "error",
          error: error instanceof Error ? error.message : "Unknown error",
        });
      }
    };

    fetchCalculations();
  }, []);

  // TypeScript narrows the type based on status
  switch (state.status) {
    case "idle":
      return <div>Click to load calculations</div>;

    case "loading":
      return <div>Loading...</div>;

    case "error":
      // TypeScript knows state.error exists here
      return <div>Error: {state.error}</div>;

    case "success":
      // TypeScript knows state.data exists here
      return (
        <ul>
          {state.data.map((calc) => (
            <li key={calc.id}>
              Wealth: ${calc.wealth}, Zakat: ${calc.zakatAmount}
            </li>
          ))}
        </ul>
      );
  }
}
```

**Form State with Validation**:

```typescript
type FormField<T> =
  | { status: "pristine"; value: T }
  | { status: "validating"; value: T }
  | { status: "valid"; value: T }
  | { status: "invalid"; value: T; error: string };

interface DonationFormState {
  amount: FormField<number>;
  email: FormField<string>;
}

function validateAmount(amount: number): FormField<number> {
  if (amount <= 0) {
    return { status: "invalid", value: amount, error: "Amount must be positive" };
  }
  if (amount > 1000000) {
    return {
      status: "invalid",
      value: amount,
      error: "Amount exceeds maximum",
    };
  }
  return { status: "valid", value: amount };
}

function validateEmail(email: string): FormField<string> {
  const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
  if (!emailRegex.test(email)) {
    return { status: "invalid", value: email, error: "Invalid email format" };
  }
  return { status: "valid", value: email };
}

function DonationFormWithValidation(): JSX.Element {
  const [formState, setFormState] = React.useState<DonationFormState>({
    amount: { status: "pristine", value: 0 },
    email: { status: "pristine", value: "" },
  });

  const handleAmountChange = (value: number) => {
    setFormState((prev) => ({
      ...prev,
      amount: validateAmount(value),
    }));
  };

  const handleEmailChange = (value: string) => {
    setFormState((prev) => ({
      ...prev,
      email: validateEmail(value),
    }));
  };

  const isFormValid =
    formState.amount.status === "valid" && formState.email.status === "valid";

  return (
    <form>
      <div>
        <input
          type="number"
          value={formState.amount.value}
          onChange={(e) => handleAmountChange(parseFloat(e.target.value) || 0)}
        />
        {formState.amount.status === "invalid" && (
          <span className="error">{formState.amount.error}</span>
        )}
      </div>
      <div>
        <input
          type="email"
          value={formState.email.value}
          onChange={(e) => handleEmailChange(e.target.value)}
        />
        {formState.email.status === "invalid" && (
          <span className="error">{formState.email.error}</span>
        )}
      </div>
      <button type="submit" disabled={!isFormValid}>
        Submit
      </button>
    </form>
  );
}
```

### 7. Effect Hook Patterns

**Pattern**: Use useEffect for side effects with proper cleanup and dependencies.

**Idiom**: Effects synchronize - cleanup prevents leaks, dependencies control re-runs.

**Data Fetching Effect**:

```typescript
function CampaignDetails({ campaignId }: { campaignId: string }): JSX.Element {
  const [campaign, setCampaign] = React.useState<DonationCampaign | null>(null);
  const [loading, setLoading] = React.useState<boolean>(true);

  React.useEffect(() => {
    let cancelled = false;

    const fetchCampaign = async () => {
      setLoading(true);

      try {
        const response = await fetch(`/api/campaigns/${campaignId}`);
        const data = await response.json();

        // Only update state if not cancelled
        if (!cancelled) {
          setCampaign(data);
        }
      } catch (error) {
        if (!cancelled) {
          console.error("Failed to fetch campaign:", error);
        }
      } finally {
        if (!cancelled) {
          setLoading(false);
        }
      }
    };

    fetchCampaign();

    // Cleanup function - prevents state updates after unmount
    return () => {
      cancelled = true;
    };
  }, [campaignId]); // Re-run when campaignId changes

  if (loading) return <div>Loading...</div>;
  if (!campaign) return <div>Campaign not found</div>;

  return (
    <div>
      <h2>{campaign.name}</h2>
      <p>Goal: ${campaign.goal}</p>
    </div>
  );
}
```

**Subscription Effect**:

```typescript
function DonationLiveUpdates({ campaignId }: { campaignId: string }): JSX.Element {
  const [donations, setDonations] = React.useState<Array<{ id: string; amount: number }>>(
    []
  );

  React.useEffect(() => {
    // Subscribe to WebSocket updates
    const ws = new WebSocket(`ws://localhost:4000/campaigns/${campaignId}`);

    ws.onmessage = (event) => {
      const donation = JSON.parse(event.data);
      setDonations((prev) => [donation, ...prev].slice(0, 10)); // Keep last 10
    };

    ws.onerror = (error) => {
      console.error("WebSocket error:", error);
    };

    // Cleanup - close WebSocket connection
    return () => {
      ws.close();
    };
  }, [campaignId]);

  return (
    <div>
      <h3>Live Donations</h3>
      {donations.map((donation) => (
        <div key={donation.id}>${donation.amount}</div>
      ))}
    </div>
  );
}
```

**Timer Effect**:

```typescript
function CountdownTimer({ targetDate }: { targetDate: Date }): JSX.Element {
  const [timeLeft, setTimeLeft] = React.useState<number>(
    targetDate.getTime() - Date.now()
  );

  React.useEffect(() => {
    const interval = setInterval(() => {
      setTimeLeft(targetDate.getTime() - Date.now());
    }, 1000);

    // Cleanup - clear interval
    return () => {
      clearInterval(interval);
    };
  }, [targetDate]);

  if (timeLeft <= 0) {
    return <div>Campaign ended</div>;
  }

  const days = Math.floor(timeLeft / (1000 * 60 * 60 * 24));
  const hours = Math.floor((timeLeft / (1000 * 60 * 60)) % 24);

  return (
    <div>
      Time left: {days}d {hours}h
    </div>
  );
}
```

### 8. Memoization Patterns

**Pattern**: Use useMemo, useCallback, and React.memo to optimize performance.

**Idiom**: Memoize expensive computations - prevent unnecessary re-renders.

**useMemo for Expensive Calculations**:

```typescript
interface Donation {
  id: string;
  amount: number;
  date: Date;
  campaignId: string;
}

function DonationStatistics({ donations }: { donations: Donation[] }): JSX.Element {
  // Expensive calculation - only re-run when donations change
  const statistics = React.useMemo(() => {
    const total = donations.reduce((sum, d) => sum + d.amount, 0);
    const average = donations.length > 0 ? total / donations.length : 0;
    const max = Math.max(...donations.map((d) => d.amount), 0);
    const byCampaign = donations.reduce((acc, d) => {
      acc[d.campaignId] = (acc[d.campaignId] || 0) + d.amount;
      return acc;
    }, {} as Record<string, number>);

    return { total, average, max, byCampaign };
  }, [donations]);

  return (
    <div>
      <p>Total: ${statistics.total.toFixed(2)}</p>
      <p>Average: ${statistics.average.toFixed(2)}</p>
      <p>Largest: ${statistics.max.toFixed(2)}</p>
    </div>
  );
}
```

**useCallback for Event Handlers**:

```typescript
interface TodoItem {
  id: string;
  text: string;
  completed: boolean;
}

function TodoList(): JSX.Element {
  const [todos, setTodos] = React.useState<TodoItem[]>([]);
  const [filter, setFilter] = React.useState<"all" | "active" | "completed">("all");

  // Memoize callback - prevent child re-renders
  const handleToggle = React.useCallback((id: string) => {
    setTodos((prev) =>
      prev.map((todo) =>
        todo.id === id ? { ...todo, completed: !todo.completed } : todo
      )
    );
  }, []);

  const handleDelete = React.useCallback((id: string) => {
    setTodos((prev) => prev.filter((todo) => todo.id !== id));
  }, []);

  // Memoize filtered list
  const filteredTodos = React.useMemo(() => {
    switch (filter) {
      case "active":
        return todos.filter((t) => !t.completed);
      case "completed":
        return todos.filter((t) => t.completed);
      default:
        return todos;
    }
  }, [todos, filter]);

  return (
    <div>
      <button onClick={() => setFilter("all")}>All</button>
      <button onClick={() => setFilter("active")}>Active</button>
      <button onClick={() => setFilter("completed")}>Completed</button>

      {filteredTodos.map((todo) => (
        <TodoItem
          key={todo.id}
          todo={todo}
          onToggle={handleToggle}
          onDelete={handleDelete}
        />
      ))}
    </div>
  );
}

// Memoized child component - only re-renders when props change
const TodoItem = React.memo(function TodoItem({
  todo,
  onToggle,
  onDelete,
}: {
  todo: TodoItem;
  onToggle: (id: string) => void;
  onDelete: (id: string) => void;
}): JSX.Element {
  return (
    <div>
      <input
        type="checkbox"
        checked={todo.completed}
        onChange={() => onToggle(todo.id)}
      />
      <span>{todo.text}</span>
      <button onClick={() => onDelete(todo.id)}>Delete</button>
    </div>
  );
});
```

### 9. Error Boundaries for Resilience

**Pattern**: Use Error Boundaries to catch rendering errors and provide fallback UI.

**Idiom**: Error boundaries catch errors - prevent whole app crashes.

**Class Component Error Boundary**:

```typescript
interface ErrorBoundaryProps {
  children: React.ReactNode;
  fallback?: React.ReactNode;
  onError?: (error: Error, errorInfo: React.ErrorInfo) => void;
}

interface ErrorBoundaryState {
  hasError: boolean;
  error: Error | null;
}

class ErrorBoundary extends React.Component<ErrorBoundaryProps, ErrorBoundaryState> {
  constructor(props: ErrorBoundaryProps) {
    super(props);
    this.state = { hasError: false, error: null };
  }

  static getDerivedStateFromError(error: Error): ErrorBoundaryState {
    return { hasError: true, error };
  }

  componentDidCatch(error: Error, errorInfo: React.ErrorInfo): void {
    console.error("Error caught by boundary:", error, errorInfo);
    this.props.onError?.(error, errorInfo);
  }

  render(): React.ReactNode {
    if (this.state.hasError) {
      if (this.props.fallback) {
        return this.props.fallback;
      }

      return (
        <div className="error-boundary">
          <h2>Something went wrong</h2>
          <p>{this.state.error?.message}</p>
          <button onClick={() => this.setState({ hasError: false, error: null })}>
            Try again
          </button>
        </div>
      );
    }

    return this.props.children;
  }
}

// Usage
function App(): JSX.Element {
  return (
    <ErrorBoundary
      fallback={<div>Error loading donations</div>}
      onError={(error) => {
        // Log to error tracking service
        console.error("Logged error:", error);
      }}
    >
      <DonationCampaignList />
    </ErrorBoundary>
  );
}
```

**Multiple Error Boundaries**:

```typescript
function DashboardApp(): JSX.Element {
  return (
    <div>
      <ErrorBoundary fallback={<div>Header error</div>}>
        <Header />
      </ErrorBoundary>

      <ErrorBoundary fallback={<div>Sidebar error</div>}>
        <Sidebar />
      </ErrorBoundary>

      <ErrorBoundary fallback={<div>Main content error</div>}>
        <MainContent />
      </ErrorBoundary>
    </div>
  );
}
```

### 10. Composition over Inheritance

**Pattern**: Compose components through props and children instead of inheritance.

**Idiom**: Compose components - reuse through composition, not inheritance.

**Layout Composition**:

```typescript
interface PageLayoutProps {
  header: React.ReactNode;
  sidebar: React.ReactNode;
  children: React.ReactNode;
  footer?: React.ReactNode;
}

function PageLayout({
  header,
  sidebar,
  children,
  footer,
}: PageLayoutProps): JSX.Element {
  return (
    <div className="page-layout">
      <header className="page-header">{header}</header>
      <div className="page-body">
        <aside className="page-sidebar">{sidebar}</aside>
        <main className="page-content">{children}</main>
      </div>
      {footer && <footer className="page-footer">{footer}</footer>}
    </div>
  );
}

// Usage
function DonationDashboard(): JSX.Element {
  return (
    <PageLayout
      header={<h1>Donation Dashboard</h1>}
      sidebar={
        <nav>
          <a href="#campaigns">Campaigns</a>
          <a href="#donations">Donations</a>
        </nav>
      }
      footer={<p>&copy; 2026 Open Sharia Enterprise</p>}
    >
      <DonationCampaignList />
    </PageLayout>
  );
}
```

**Component Composition with Slots**:

```typescript
interface CardProps {
  title?: React.ReactNode;
  actions?: React.ReactNode;
  children: React.ReactNode;
  variant?: "primary" | "secondary";
}

function Card({ title, actions, children, variant = "primary" }: CardProps): JSX.Element {
  return (
    <div className={`card card-${variant}`}>
      {title && (
        <div className="card-header">
          <h3>{title}</h3>
          {actions && <div className="card-actions">{actions}</div>}
        </div>
      )}
      <div className="card-body">{children}</div>
    </div>
  );
}

// Usage
function ZakatCalculationCard({ calculation }: { calculation: ZakatCalculation }): JSX.Element {
  return (
    <Card
      title={`Calculation - ${calculation.calculationDate.toLocaleDateString()}`}
      actions={
        <>
          <button>Edit</button>
          <button>Delete</button>
        </>
      }
    >
      <p>Wealth: ${calculation.wealth.toFixed(2)}</p>
      <p>Zakat: ${calculation.zakatAmount.toFixed(2)}</p>
    </Card>
  );
}
```

**Higher-Order Component Pattern (Composition Alternative)**:

```typescript
function withLoading<P extends object>(
  Component: React.ComponentType<P>
): React.FC<P & { loading: boolean }> {
  return function WithLoadingComponent({ loading, ...props }: P & { loading: boolean }) {
    if (loading) {
      return <div>Loading...</div>;
    }
    return <Component {...(props as P)} />;
  };
}

// Usage
function DonationList({ donations }: { donations: Donation[] }): JSX.Element {
  return (
    <ul>
      {donations.map((d) => (
        <li key={d.id}>${d.amount}</li>
      ))}
    </ul>
  );
}

const DonationListWithLoading = withLoading(DonationList);

function App(): JSX.Element {
  const [donations, setDonations] = React.useState<Donation[]>([]);
  const [loading, setLoading] = React.useState(true);

  return <DonationListWithLoading donations={donations} loading={loading} />;
}
```

### 11. Controlled Components for Forms

**Pattern**: Use controlled components where React state drives form values.

**Idiom**: Controlled components - React is the source of truth.

**Controlled Input**:

```typescript
function ZakatCalculatorForm(): JSX.Element {
  const [wealth, setWealth] = React.useState<string>("");
  const [nisab, setNisab] = React.useState<string>("");
  const [zakatAmount, setZakatAmount] = React.useState<number>(0);

  const handleCalculate = () => {
    const wealthNum = parseFloat(wealth) || 0;
    const nisabNum = parseFloat(nisab) || 0;

    if (wealthNum >= nisabNum && nisabNum > 0) {
      setZakatAmount(wealthNum * 0.025);
    } else {
      setZakatAmount(0);
    }
  };

  return (
    <div>
      <div>
        <label>
          Wealth:
          <input
            type="number"
            value={wealth}
            onChange={(e) => setWealth(e.target.value)}
          />
        </label>
      </div>
      <div>
        <label>
          Nisab:
          <input type="number" value={nisab} onChange={(e) => setNisab(e.target.value)} />
        </label>
      </div>
      <button onClick={handleCalculate}>Calculate</button>
      <div>Zakat Amount: ${zakatAmount.toFixed(2)}</div>
    </div>
  );
}
```

**Controlled Form with Validation**:

```typescript
interface DonationFormData {
  amount: string;
  name: string;
  email: string;
  message: string;
}

interface DonationFormErrors {
  amount?: string;
  name?: string;
  email?: string;
}

function DonationFormControlled(): JSX.Element {
  const [formData, setFormData] = React.useState<DonationFormData>({
    amount: "",
    name: "",
    email: "",
    message: "",
  });

  const [errors, setErrors] = React.useState<DonationFormErrors>({});

  const handleChange = (field: keyof DonationFormData, value: string) => {
    setFormData((prev) => ({ ...prev, [field]: value }));

    // Clear error when user starts typing
    if (errors[field as keyof DonationFormErrors]) {
      setErrors((prev) => ({ ...prev, [field]: undefined }));
    }
  };

  const validate = (): boolean => {
    const newErrors: DonationFormErrors = {};

    const amountNum = parseFloat(formData.amount);
    if (isNaN(amountNum) || amountNum <= 0) {
      newErrors.amount = "Amount must be positive";
    }

    if (formData.name.trim().length === 0) {
      newErrors.name = "Name is required";
    }

    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    if (!emailRegex.test(formData.email)) {
      newErrors.email = "Invalid email";
    }

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();

    if (!validate()) {
      return;
    }

    console.log("Submitting:", formData);
  };

  return (
    <form onSubmit={handleSubmit}>
      <div>
        <label>
          Amount:
          <input
            type="number"
            value={formData.amount}
            onChange={(e) => handleChange("amount", e.target.value)}
          />
        </label>
        {errors.amount && <span className="error">{errors.amount}</span>}
      </div>

      <div>
        <label>
          Name:
          <input
            type="text"
            value={formData.name}
            onChange={(e) => handleChange("name", e.target.value)}
          />
        </label>
        {errors.name && <span className="error">{errors.name}</span>}
      </div>

      <div>
        <label>
          Email:
          <input
            type="email"
            value={formData.email}
            onChange={(e) => handleChange("email", e.target.value)}
          />
        </label>
        {errors.email && <span className="error">{errors.email}</span>}
      </div>

      <div>
        <label>
          Message:
          <textarea
            value={formData.message}
            onChange={(e) => handleChange("message", e.target.value)}
          />
        </label>
      </div>

      <button type="submit">Donate</button>
    </form>
  );
}
```

### 12. Render Props and Higher-Order Components

**Pattern**: Use render props and HOCs for cross-cutting concerns.

**Idiom**: Render props share behavior - pass functions as children.

**Render Props Pattern**:

```typescript
interface MousePositionProps {
  children: (position: { x: number; y: number }) => React.ReactNode;
}

function MousePosition({ children }: MousePositionProps): JSX.Element {
  const [position, setPosition] = React.useState({ x: 0, y: 0 });

  React.useEffect(() => {
    const handleMouseMove = (event: MouseEvent) => {
      setPosition({ x: event.clientX, y: event.clientY });
    };

    window.addEventListener("mousemove", handleMouseMove);
    return () => window.removeEventListener("mousemove", handleMouseMove);
  }, []);

  return <>{children(position)}</>;
}

// Usage
function App(): JSX.Element {
  return (
    <MousePosition>
      {({ x, y }) => (
        <div>
          Mouse position: {x}, {y}
        </div>
      )}
    </MousePosition>
  );
}
```

**Data Fetching with Render Props**:

```typescript
interface FetchDataProps<T> {
  url: string;
  children: (state: {
    data: T | null;
    loading: boolean;
    error: Error | null;
  }) => React.ReactNode;
}

function FetchData<T>({ url, children }: FetchDataProps<T>): JSX.Element {
  const [data, setData] = React.useState<T | null>(null);
  const [loading, setLoading] = React.useState<boolean>(true);
  const [error, setError] = React.useState<Error | null>(null);

  React.useEffect(() => {
    let cancelled = false;

    const fetchData = async () => {
      try {
        const response = await fetch(url);
        if (!response.ok) throw new Error(`HTTP error: ${response.status}`);
        const result = await response.json();
        if (!cancelled) {
          setData(result);
          setLoading(false);
        }
      } catch (err) {
        if (!cancelled) {
          setError(err instanceof Error ? err : new Error("Unknown error"));
          setLoading(false);
        }
      }
    };

    fetchData();
    return () => {
      cancelled = true;
    };
  }, [url]);

  return <>{children({ data, loading, error })}</>;
}

// Usage
function DonationCampaignList(): JSX.Element {
  return (
    <FetchData<DonationCampaign[]> url="/api/campaigns">
      {({ data, loading, error }) => {
        if (loading) return <div>Loading...</div>;
        if (error) return <div>Error: {error.message}</div>;
        if (!data) return <div>No campaigns</div>;

        return (
          <ul>
            {data.map((campaign) => (
              <li key={campaign.id}>{campaign.name}</li>
            ))}
          </ul>
        );
      }}
    </FetchData>
  );
}
```

**Higher-Order Component for Auth**:

```typescript
function withAuth<P extends object>(
  Component: React.ComponentType<P>
): React.FC<P> {
  return function WithAuthComponent(props: P) {
    const { user, loading } = useAuth();

    if (loading) {
      return <div>Loading...</div>;
    }

    if (!user) {
      return <div>Please log in</div>;
    }

    return <Component {...props} />;
  };
}

// Usage
function DonationDashboard(): JSX.Element {
  return <div>Dashboard content</div>;
}

const ProtectedDonationDashboard = withAuth(DonationDashboard);

function App(): JSX.Element {
  return (
    <AuthProvider>
      <ProtectedDonationDashboard />
    </AuthProvider>
  );
}
```

## Related Documentation

- **[React Best Practices](ex-so-plwe-fere__best-practices.md)** - Production standards
- **[React Anti-Patterns](ex-so-plwe-fere__anti-patterns.md)** - Common mistakes
- **[TypeScript Best Practices](../../prog-lang/typescript/ex-so-prla-ty__best-practices.md)** - TypeScript standards
- **[Functional Programming](../../prog-lang/typescript/ex-so-prla-ty__functional-programming.md)** - FP patterns
- **[Immutability Principle](../../../../governance/principles/software-engineering/immutability.md)** - Immutability over mutability
- **[Pure Functions Principle](../../../../governance/principles/software-engineering/pure-functions.md)** - Pure functions over side effects

---

**Last Updated**: 2026-01-29
**React Version**: 18+
**TypeScript Version**: 5+
