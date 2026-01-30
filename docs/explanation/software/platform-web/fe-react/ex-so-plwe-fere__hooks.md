---
title: React Hooks
description: Comprehensive guide to React hooks and custom hook patterns for state management and side effects
category: explanation
tags:
  - react
  - hooks
  - state-management
  - frontend
  - javascript
created: 2026-01-29
last_updated: 2026-01-29
---

# React Hooks

React Hooks are functions that enable you to use state and other React features in function components. Introduced in React 16.8, hooks revolutionized React development by allowing functional components to manage state, handle side effects, and reuse stateful logic without class components.

## 📚 Built-in Hooks Overview

React provides several built-in hooks for different use cases:

**State Hooks**: Manage component state

- `useState` - Simple state management
- `useReducer` - Complex state with actions

**Effect Hooks**: Handle side effects and lifecycle

- `useEffect` - Side effects, subscriptions, data fetching
- `useLayoutEffect` - Synchronous DOM updates
- `useInsertionEffect` - CSS-in-JS library support

**Context Hooks**: Access React context

- `useContext` - Consume context values

**Ref Hooks**: Reference values and DOM nodes

- `useRef` - Mutable values, DOM references
- `useImperativeHandle` - Customize exposed ref

**Performance Hooks**: Optimize rendering

- `useMemo` - Memoize expensive calculations
- `useCallback` - Memoize functions

**Concurrent Hooks**: Concurrent React features

- `useTransition` - Non-blocking state updates
- `useDeferredValue` - Defer non-urgent updates

**Other Hooks**:

- `useId` - Generate unique IDs
- `useDebugValue` - Custom hook debugging
- `useSyncExternalStore` - Subscribe to external stores

## 🎯 useState Deep Dive

`useState` is the fundamental hook for managing component state.

### Basic Usage

```javascript
import { useState } from "react";

function ZakatCalculator() {
  const [amount, setAmount] = useState(0);
  const [zakatRate] = useState(0.025); // 2.5%

  const zakatAmount = amount * zakatRate;

  return (
    <div>
      <input type="number" value={amount} onChange={(e) => setAmount(Number(e.target.value))} />
      <p>Zakat: {zakatAmount.toFixed(2)}</p>
    </div>
  );
}
```

### State Updater Functions

Use updater functions when new state depends on previous state:

```javascript
function DonationCounter() {
  const [count, setCount] = useState(0);

  // ❌ Wrong - can miss updates in rapid clicks
  const handleIncrement = () => {
    setCount(count + 1);
    setCount(count + 1); // Still count + 1, not count + 2
  };

  // ✅ Correct - uses previous state
  const handleIncrementCorrect = () => {
    setCount((prevCount) => prevCount + 1);
    setCount((prevCount) => prevCount + 1); // Now count + 2
  };

  return <button onClick={handleIncrementCorrect}>Donate: {count}</button>;
}
```

### State Batching

React 18+ automatically batches state updates for performance:

```javascript
function MurabahaForm() {
  const [principal, setPrincipal] = useState(0);
  const [profitRate, setProfitRate] = useState(0);
  const [term, setTerm] = useState(0);

  const handleSubmit = () => {
    // All three updates batched into single re-render
    setPrincipal(100000);
    setProfitRate(0.05);
    setTerm(12);
  };

  // Component only re-renders once after handleSubmit
  return <form onSubmit={handleSubmit}>...</form>;
}
```

### Complex State Objects

Managing objects with `useState`:

```javascript
function DonorProfile() {
  const [donor, setDonor] = useState({
    name: "",
    email: "",
    amount: 0,
    isRecurring: false,
  });

  // ✅ Spread existing state to avoid losing properties
  const updateName = (name) => {
    setDonor((prev) => ({ ...prev, name }));
  };

  const updateEmail = (email) => {
    setDonor((prev) => ({ ...prev, email }));
  };

  // ❌ Wrong - loses other properties
  const updateAmountWrong = (amount) => {
    setDonor({ amount }); // name, email, isRecurring lost!
  };

  return (
    <div>
      <input value={donor.name} onChange={(e) => updateName(e.target.value)} />
      <input value={donor.email} onChange={(e) => updateEmail(e.target.value)} />
    </div>
  );
}
```

### Lazy Initialization

Initialize state with expensive computation only once:

```javascript
function ZakatHistory() {
  // ❌ Runs on every render
  const [records] = useState(loadRecordsFromLocalStorage());

  // ✅ Runs only once during mount
  const [recordsOptimized] = useState(() => {
    return loadRecordsFromLocalStorage();
  });

  return <RecordsList records={recordsOptimized} />;
}

function loadRecordsFromLocalStorage() {
  console.log("Loading from localStorage...");
  return JSON.parse(localStorage.getItem("zakatRecords") || "[]");
}
```

## ⚡ useEffect Deep Dive

`useEffect` handles side effects in function components.

### Basic Syntax

```javascript
import { useEffect, useState } from "react";

function DonationTracker() {
  const [donations, setDonations] = useState([]);

  useEffect(() => {
    // Effect runs after render
    console.log("Component rendered");

    // Cleanup function (optional)
    return () => {
      console.log("Component unmounting or effect re-running");
    };
  }); // No dependency array - runs after every render

  return <div>Donations: {donations.length}</div>;
}
```

### Dependency Array

Control when effects run:

```javascript
function PrayerTimes() {
  const [location, setLocation] = useState(null);
  const [times, setTimes] = useState(null);

  // Runs only once after mount (empty array)
  useEffect(() => {
    navigator.geolocation.getCurrentPosition((pos) => {
      setLocation({
        lat: pos.coords.latitude,
        lng: pos.coords.longitude,
      });
    });
  }, []); // Empty dependency array

  // Runs when location changes
  useEffect(() => {
    if (!location) return;

    fetchPrayerTimes(location).then(setTimes);
  }, [location]); // Re-run when location changes

  return <div>{times ? <PrayerTimesList times={times} /> : "Loading..."}</div>;
}
```

### Cleanup Functions

Clean up subscriptions, timers, and event listeners:

```javascript
function LiveDonationCounter() {
  const [count, setCount] = useState(0);

  useEffect(() => {
    // Subscribe to real-time updates
    const subscription = donationStream.subscribe((donation) => {
      setCount((prev) => prev + 1);
    });

    // Cleanup: unsubscribe when component unmounts
    return () => {
      subscription.unsubscribe();
    };
  }, []);

  return <div>Live donations: {count}</div>;
}

function AutoSaveForm() {
  const [formData, setFormData] = useState({});

  useEffect(() => {
    // Set up auto-save timer
    const timer = setInterval(() => {
      saveToLocalStorage(formData);
    }, 5000);

    // Cleanup: clear timer on unmount or when formData changes
    return () => {
      clearInterval(timer);
    };
  }, [formData]);

  return <form>...</form>;
}
```

### Effect Timing

`useEffect` runs after paint (asynchronous):

```javascript
function DataFetcher() {
  const [data, setData] = useState(null);

  useEffect(() => {
    // Runs after browser paints
    fetchData().then(setData);
  }, []);

  // For synchronous DOM updates before paint, use useLayoutEffect
  useLayoutEffect(() => {
    // Runs before browser paints (blocks painting)
    // Use for DOM measurements, animations
    const height = document.getElementById("element").offsetHeight;
    console.log("Height before paint:", height);
  }, []);

  return <div id="element">{data}</div>;
}
```

### Common Effect Patterns

**Data Fetching**:

```javascript
function DonationList() {
  const [donations, setDonations] = useState([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);

  useEffect(() => {
    let cancelled = false;

    async function fetchDonations() {
      try {
        setLoading(true);
        const response = await fetch("/api/donations");
        const data = await response.json();

        if (!cancelled) {
          setDonations(data);
        }
      } catch (err) {
        if (!cancelled) {
          setError(err.message);
        }
      } finally {
        if (!cancelled) {
          setLoading(false);
        }
      }
    }

    fetchDonations();

    // Cleanup: prevent state updates if component unmounts
    return () => {
      cancelled = true;
    };
  }, []);

  if (loading) return <div>Loading...</div>;
  if (error) return <div>Error: {error}</div>;
  return <DonationsList donations={donations} />;
}
```

**Event Listeners**:

```javascript
function WindowResizeTracker() {
  const [width, setWidth] = useState(window.innerWidth);

  useEffect(() => {
    const handleResize = () => {
      setWidth(window.innerWidth);
    };

    window.addEventListener("resize", handleResize);

    // Cleanup: remove listener
    return () => {
      window.removeEventListener("resize", handleResize);
    };
  }, []);

  return <div>Window width: {width}px</div>;
}
```

## 🌐 useContext

`useContext` provides access to React context values without prop drilling.

### Basic Usage

```javascript
import { createContext, useContext, useState } from "react";

// Create context
const AuthContext = createContext(null);

// Provider component
function AuthProvider({ children }) {
  const [user, setUser] = useState(null);

  const login = (userData) => {
    setUser(userData);
  };

  const logout = () => {
    setUser(null);
  };

  return <AuthContext.Provider value={{ user, login, logout }}>{children}</AuthContext.Provider>;
}

// Consumer component
function UserProfile() {
  const { user, logout } = useContext(AuthContext);

  if (!user) {
    return <div>Not logged in</div>;
  }

  return (
    <div>
      <p>Welcome, {user.name}</p>
      <button onClick={logout}>Logout</button>
    </div>
  );
}

// App structure
function App() {
  return (
    <AuthProvider>
      <UserProfile />
    </AuthProvider>
  );
}
```

### Avoiding Re-renders

Context triggers re-renders for all consumers when value changes:

```javascript
// ❌ Creates new object on every render - re-renders all consumers
function AuthProviderBad({ children }) {
  const [user, setUser] = useState(null);

  return <AuthContext.Provider value={{ user, setUser }}>{children}</AuthContext.Provider>;
}

// ✅ Memoize context value
function AuthProviderGood({ children }) {
  const [user, setUser] = useState(null);

  const value = useMemo(() => ({ user, setUser }), [user]);

  return <AuthContext.Provider value={value}>{children}</AuthContext.Provider>;
}
```

### Multiple Contexts

```javascript
const ThemeContext = createContext("light");
const LanguageContext = createContext("en");

function MultiContextComponent() {
  const theme = useContext(ThemeContext);
  const language = useContext(LanguageContext);

  return (
    <div className={theme}>
      <p>{language === "en" ? "Hello" : "Halo"}</p>
    </div>
  );
}

function App() {
  return (
    <ThemeContext.Provider value="dark">
      <LanguageContext.Provider value="id">
        <MultiContextComponent />
      </LanguageContext.Provider>
    </ThemeContext.Provider>
  );
}
```

## 🔄 useReducer

`useReducer` manages complex state with actions and a reducer function.

### Basic Usage

```javascript
import { useReducer } from "react";

// Define action types
const ACTIONS = {
  ADD_DONATION: "add_donation",
  REMOVE_DONATION: "remove_donation",
  UPDATE_TOTAL: "update_total",
  RESET: "reset",
};

// Reducer function
function donationReducer(state, action) {
  switch (action.type) {
    case ACTIONS.ADD_DONATION:
      return {
        ...state,
        donations: [...state.donations, action.payload],
        total: state.total + action.payload.amount,
      };

    case ACTIONS.REMOVE_DONATION:
      const removedDonation = state.donations.find((d) => d.id === action.payload);
      return {
        ...state,
        donations: state.donations.filter((d) => d.id !== action.payload),
        total: state.total - (removedDonation?.amount || 0),
      };

    case ACTIONS.RESET:
      return {
        donations: [],
        total: 0,
      };

    default:
      return state;
  }
}

function DonationManager() {
  const [state, dispatch] = useReducer(donationReducer, {
    donations: [],
    total: 0,
  });

  const addDonation = (amount, donor) => {
    dispatch({
      type: ACTIONS.ADD_DONATION,
      payload: {
        id: Date.now(),
        amount,
        donor,
        timestamp: new Date(),
      },
    });
  };

  const removeDonation = (id) => {
    dispatch({
      type: ACTIONS.REMOVE_DONATION,
      payload: id,
    });
  };

  const resetDonations = () => {
    dispatch({ type: ACTIONS.RESET });
  };

  return (
    <div>
      <h2>Total: ${state.total}</h2>
      <button onClick={() => addDonation(100, "Ahmad")}>Add Donation</button>
      <button onClick={resetDonations}>Reset</button>
      <ul>
        {state.donations.map((donation) => (
          <li key={donation.id}>
            {donation.donor}: ${donation.amount}
            <button onClick={() => removeDonation(donation.id)}>Remove</button>
          </li>
        ))}
      </ul>
    </div>
  );
}
```

### When to Use useReducer vs useState

**Use `useState` when**:

- Simple state (single value, boolean)
- Independent state updates
- No complex state transitions

**Use `useReducer` when**:

- Complex state shape (nested objects, arrays)
- State transitions depend on multiple values
- Multiple ways to update same state
- State logic becomes complex

### Lazy Initialization

```javascript
function createInitialState(initialCount) {
  console.log("Computing initial state...");
  return { count: initialCount };
}

function Counter({ initialCount }) {
  const [state, dispatch] = useReducer(
    reducer,
    initialCount,
    createInitialState, // Lazy initializer
  );

  // createInitialState runs only once during mount
  return <div>Count: {state.count}</div>;
}
```

## 🚀 useMemo and useCallback

Performance optimization hooks for expensive calculations and function references.

### useMemo - Memoize Values

```javascript
import { useMemo, useState } from "react";

function ZakatCalculatorOptimized() {
  const [amount, setAmount] = useState(0);
  const [nisab, setNisab] = useState(85); // grams of gold
  const [goldPrice, setGoldPrice] = useState(60); // per gram

  // ❌ Recalculates on every render
  const zakatAmountSlow = calculateZakat(amount, nisab, goldPrice);

  // ✅ Only recalculates when dependencies change
  const zakatAmount = useMemo(() => {
    console.log("Calculating zakat...");
    return calculateZakat(amount, nisab, goldPrice);
  }, [amount, nisab, goldPrice]);

  return (
    <div>
      <input type="number" value={amount} onChange={(e) => setAmount(Number(e.target.value))} />
      <p>Zakat: {zakatAmount}</p>
    </div>
  );
}

function calculateZakat(amount, nisab, goldPrice) {
  const nisabValue = nisab * goldPrice;
  if (amount < nisabValue) return 0;
  return amount * 0.025; // 2.5%
}
```

### useCallback - Memoize Functions

```javascript
import { useCallback, useState, memo } from "react";

function DonationForm() {
  const [donations, setDonations] = useState([]);

  // ❌ Creates new function on every render - DonationInput re-renders
  const handleAddDonation = (amount) => {
    setDonations((prev) => [...prev, amount]);
  };

  // ✅ Memoized function - DonationInput only re-renders when needed
  const handleAddDonationMemoized = useCallback((amount) => {
    setDonations((prev) => [...prev, amount]);
  }, []); // Empty deps - function never changes

  return (
    <div>
      <DonationInput onAdd={handleAddDonationMemoized} />
      <Total donations={donations} />
    </div>
  );
}

// memo prevents re-renders when props don't change
const DonationInput = memo(({ onAdd }) => {
  const [amount, setAmount] = useState(0);

  return (
    <div>
      <input type="number" value={amount} onChange={(e) => setAmount(Number(e.target.value))} />
      <button onClick={() => onAdd(amount)}>Add</button>
    </div>
  );
});
```

### When to Use Memoization

**Use `useMemo` when**:

- Expensive calculations (heavy processing, large arrays)
- Passing objects/arrays as props to memoized components
- Preventing unnecessary re-computations

**Use `useCallback` when**:

- Passing callbacks to memoized child components
- Functions used as dependencies in other hooks
- Event handlers for performance-critical components

**Don't overuse**: Memoization has overhead. Profile before optimizing.

## 📍 useRef

`useRef` creates mutable references that persist across renders without triggering re-renders.

### DOM References

```javascript
import { useRef, useEffect } from "react";

function DonationInput() {
  const inputRef = useRef(null);

  useEffect(() => {
    // Focus input on mount
    inputRef.current.focus();
  }, []);

  const handleClear = () => {
    inputRef.current.value = "";
    inputRef.current.focus();
  };

  return (
    <div>
      <input ref={inputRef} type="number" />
      <button onClick={handleClear}>Clear</button>
    </div>
  );
}
```

### Mutable Values (Not State)

```javascript
function Timer() {
  const [seconds, setSeconds] = useState(0);
  const intervalRef = useRef(null);

  const startTimer = () => {
    // Store interval ID in ref
    intervalRef.current = setInterval(() => {
      setSeconds((s) => s + 1);
    }, 1000);
  };

  const stopTimer = () => {
    // Access interval ID from ref
    clearInterval(intervalRef.current);
  };

  useEffect(() => {
    return () => {
      // Cleanup on unmount
      if (intervalRef.current) {
        clearInterval(intervalRef.current);
      }
    };
  }, []);

  return (
    <div>
      <p>Seconds: {seconds}</p>
      <button onClick={startTimer}>Start</button>
      <button onClick={stopTimer}>Stop</button>
    </div>
  );
}
```

### Previous Value Pattern

```javascript
function usePrevious(value) {
  const ref = useRef();

  useEffect(() => {
    ref.current = value;
  }, [value]);

  return ref.current;
}

function DonationTracker({ amount }) {
  const previousAmount = usePrevious(amount);

  return (
    <div>
      <p>Current: ${amount}</p>
      <p>Previous: ${previousAmount}</p>
      <p>{amount > previousAmount ? "Increased" : amount < previousAmount ? "Decreased" : "Same"}</p>
    </div>
  );
}
```

### useRef vs useState

**Use `useRef` when**:

- Value doesn't affect rendering
- Need to store mutable value across renders
- Accessing DOM elements
- Storing interval/timeout IDs

**Use `useState` when**:

- Value affects what's rendered
- Need to trigger re-render on change

## 🛠️ Custom Hooks

Custom hooks extract and reuse stateful logic across components.

### Naming Convention

Custom hooks MUST start with `use`:

```javascript
// ✅ Correct
function useZakatCalculator() { ... }
function useDonationForm() { ... }

// ❌ Wrong
function zakatCalculator() { ... }
function calculateZakat() { ... }
```

### Basic Custom Hook

```javascript
import { useState, useEffect } from "react";

function useLocalStorage(key, initialValue) {
  // Initialize state from localStorage
  const [value, setValue] = useState(() => {
    try {
      const item = window.localStorage.getItem(key);
      return item ? JSON.parse(item) : initialValue;
    } catch (error) {
      console.error(error);
      return initialValue;
    }
  });

  // Update localStorage when value changes
  useEffect(() => {
    try {
      window.localStorage.setItem(key, JSON.stringify(value));
    } catch (error) {
      console.error(error);
    }
  }, [key, value]);

  return [value, setValue];
}

// Usage
function DonationForm() {
  const [donations, setDonations] = useLocalStorage("donations", []);

  const addDonation = (amount) => {
    setDonations([...donations, { amount, timestamp: Date.now() }]);
  };

  return <div>...</div>;
}
```

### useZakatCalculator Hook

```javascript
function useZakatCalculator() {
  const [amount, setAmount] = useState(0);
  const [nisab, setNisab] = useState(85);
  const [goldPrice, setGoldPrice] = useState(60);

  const nisabValue = useMemo(() => nisab * goldPrice, [nisab, goldPrice]);

  const zakatAmount = useMemo(() => {
    if (amount < nisabValue) return 0;
    return amount * 0.025;
  }, [amount, nisabValue]);

  const isAboveNisab = amount >= nisabValue;

  return {
    amount,
    setAmount,
    nisab,
    setNisab,
    goldPrice,
    setGoldPrice,
    nisabValue,
    zakatAmount,
    isAboveNisab,
  };
}

// Usage
function ZakatCalculatorApp() {
  const { amount, setAmount, zakatAmount, nisabValue, isAboveNisab } = useZakatCalculator();

  return (
    <div>
      <input type="number" value={amount} onChange={(e) => setAmount(Number(e.target.value))} />
      <p>Nisab Value: ${nisabValue}</p>
      <p>Above Nisab: {isAboveNisab ? "Yes" : "No"}</p>
      {isAboveNisab && <p>Zakat Due: ${zakatAmount.toFixed(2)}</p>}
    </div>
  );
}
```

### useDonationForm Hook

```javascript
function useDonationForm(initialValues = {}) {
  const [values, setValues] = useState(initialValues);
  const [errors, setErrors] = useState({});
  const [isSubmitting, setIsSubmitting] = useState(false);

  const handleChange = useCallback(
    (e) => {
      const { name, value, type, checked } = e.target;
      setValues((prev) => ({
        ...prev,
        [name]: type === "checkbox" ? checked : value,
      }));

      // Clear error when field changes
      if (errors[name]) {
        setErrors((prev) => {
          const newErrors = { ...prev };
          delete newErrors[name];
          return newErrors;
        });
      }
    },
    [errors],
  );

  const validate = useCallback(() => {
    const newErrors = {};

    if (!values.amount || values.amount <= 0) {
      newErrors.amount = "Amount must be greater than 0";
    }

    if (!values.donor || values.donor.trim() === "") {
      newErrors.donor = "Donor name is required";
    }

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  }, [values]);

  const handleSubmit = useCallback(
    async (onSubmit) => {
      setIsSubmitting(true);

      if (validate()) {
        try {
          await onSubmit(values);
          setValues(initialValues); // Reset form
        } catch (error) {
          setErrors({ submit: error.message });
        }
      }

      setIsSubmitting(false);
    },
    [values, validate, initialValues],
  );

  const reset = useCallback(() => {
    setValues(initialValues);
    setErrors({});
    setIsSubmitting(false);
  }, [initialValues]);

  return {
    values,
    errors,
    isSubmitting,
    handleChange,
    handleSubmit,
    reset,
  };
}

// Usage
function DonationFormComponent() {
  const { values, errors, isSubmitting, handleChange, handleSubmit } = useDonationForm({
    amount: 0,
    donor: "",
    isAnonymous: false,
  });

  const onSubmit = async (values) => {
    await fetch("/api/donations", {
      method: "POST",
      body: JSON.stringify(values),
    });
  };

  return (
    <form
      onSubmit={(e) => {
        e.preventDefault();
        handleSubmit(onSubmit);
      }}
    >
      <input name="amount" type="number" value={values.amount} onChange={handleChange} />
      {errors.amount && <span>{errors.amount}</span>}

      <input name="donor" type="text" value={values.donor} onChange={handleChange} />
      {errors.donor && <span>{errors.donor}</span>}

      <label>
        <input name="isAnonymous" type="checkbox" checked={values.isAnonymous} onChange={handleChange} />
        Anonymous
      </label>

      <button type="submit" disabled={isSubmitting}>
        {isSubmitting ? "Submitting..." : "Donate"}
      </button>
    </form>
  );
}
```

### useMurabahaContract Hook

```javascript
function useMurabahaContract() {
  const [principal, setPrincipal] = useState(0);
  const [profitRate, setProfitRate] = useState(0.05);
  const [term, setTerm] = useState(12); // months

  const totalProfit = useMemo(() => {
    return principal * profitRate;
  }, [principal, profitRate]);

  const totalAmount = useMemo(() => {
    return principal + totalProfit;
  }, [principal, totalProfit]);

  const monthlyPayment = useMemo(() => {
    return term > 0 ? totalAmount / term : 0;
  }, [totalAmount, term]);

  const schedule = useMemo(() => {
    const payments = [];
    let remaining = totalAmount;

    for (let month = 1; month <= term; month++) {
      const payment = monthlyPayment;
      remaining -= payment;

      payments.push({
        month,
        payment: payment.toFixed(2),
        remaining: Math.max(0, remaining).toFixed(2),
      });
    }

    return payments;
  }, [totalAmount, monthlyPayment, term]);

  return {
    principal,
    setPrincipal,
    profitRate,
    setProfitRate,
    term,
    setTerm,
    totalProfit,
    totalAmount,
    monthlyPayment,
    schedule,
  };
}

// Usage
function MurabahaCalculator() {
  const {
    principal,
    setPrincipal,
    profitRate,
    setProfitRate,
    term,
    setTerm,
    totalProfit,
    totalAmount,
    monthlyPayment,
    schedule,
  } = useMurabahaContract();

  return (
    <div>
      <h2>Murabaha Contract Calculator</h2>

      <label>
        Principal:
        <input type="number" value={principal} onChange={(e) => setPrincipal(Number(e.target.value))} />
      </label>

      <label>
        Profit Rate:
        <input type="number" step="0.01" value={profitRate} onChange={(e) => setProfitRate(Number(e.target.value))} />
      </label>

      <label>
        Term (months):
        <input type="number" value={term} onChange={(e) => setTerm(Number(e.target.value))} />
      </label>

      <div>
        <h3>Summary</h3>
        <p>Total Profit: ${totalProfit.toFixed(2)}</p>
        <p>Total Amount: ${totalAmount.toFixed(2)}</p>
        <p>Monthly Payment: ${monthlyPayment.toFixed(2)}</p>
      </div>

      <div>
        <h3>Payment Schedule</h3>
        <table>
          <thead>
            <tr>
              <th>Month</th>
              <th>Payment</th>
              <th>Remaining</th>
            </tr>
          </thead>
          <tbody>
            {schedule.map((item) => (
              <tr key={item.month}>
                <td>{item.month}</td>
                <td>${item.payment}</td>
                <td>${item.remaining}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </div>
  );
}
```

## 🆔 useId

`useId` generates unique IDs for accessibility attributes.

### Basic Usage

```javascript
import { useId } from "react";

function DonationFormWithId() {
  const amountId = useId();
  const donorId = useId();

  return (
    <form>
      <label htmlFor={amountId}>Amount:</label>
      <input id={amountId} type="number" />

      <label htmlFor={donorId}>Donor:</label>
      <input id={donorId} type="text" />
    </form>
  );
}
```

### Multiple IDs from Single useId

```javascript
function FormField({ label, type = "text" }) {
  const id = useId();
  const inputId = `${id}-input`;
  const errorId = `${id}-error`;

  return (
    <div>
      <label htmlFor={inputId}>{label}</label>
      <input id={inputId} type={type} aria-describedby={errorId} />
      <span id={errorId} role="alert">
        Error message here
      </span>
    </div>
  );
}
```

### Why Not Math.random() or Incrementing Counter?

```javascript
// ❌ Wrong - not stable across server/client renders
function BadFormField() {
  const id = Math.random().toString();
  return <input id={id} />;
}

// ❌ Wrong - different IDs on server vs client in SSR
let counter = 0;
function BadFormField2() {
  const id = `field-${counter++}`;
  return <input id={id} />;
}

// ✅ Correct - stable, SSR-safe
function GoodFormField() {
  const id = useId();
  return <input id={id} />;
}
```

## ⚡ useTransition and useDeferredValue

Concurrent React features for non-blocking updates.

### useTransition

Mark state updates as non-urgent:

```javascript
import { useState, useTransition } from "react";

function SearchableDonationList() {
  const [query, setQuery] = useState("");
  const [isPending, startTransition] = useTransition();
  const [filteredDonations, setFilteredDonations] = useState([]);

  const handleSearch = (value) => {
    setQuery(value); // Urgent: update input immediately

    startTransition(() => {
      // Non-urgent: filter large list
      const filtered = allDonations.filter((d) => d.donor.toLowerCase().includes(value.toLowerCase()));
      setFilteredDonations(filtered);
    });
  };

  return (
    <div>
      <input type="text" value={query} onChange={(e) => handleSearch(e.target.value)} />
      {isPending && <span>Loading...</span>}
      <ul>
        {filteredDonations.map((donation) => (
          <li key={donation.id}>
            {donation.donor}: ${donation.amount}
          </li>
        ))}
      </ul>
    </div>
  );
}

const allDonations = Array.from({ length: 10000 }, (_, i) => ({
  id: i,
  donor: `Donor ${i}`,
  amount: Math.floor(Math.random() * 1000),
}));
```

### useDeferredValue

Defer updating a value:

```javascript
import { useState, useDeferredValue, useMemo } from "react";

function DonationSearch() {
  const [query, setQuery] = useState("");
  const deferredQuery = useDeferredValue(query);

  // Expensive filtering uses deferred value
  const filteredDonations = useMemo(() => {
    return allDonations.filter((d) => d.donor.toLowerCase().includes(deferredQuery.toLowerCase()));
  }, [deferredQuery]);

  return (
    <div>
      <input type="text" value={query} onChange={(e) => setQuery(e.target.value)} />
      {/* Show stale results while filtering */}
      <DonationList donations={filteredDonations} />
    </div>
  );
}

function DonationList({ donations }) {
  return (
    <ul>
      {donations.map((donation) => (
        <li key={donation.id}>
          {donation.donor}: ${donation.amount}
        </li>
      ))}
    </ul>
  );
}
```

### useTransition vs useDeferredValue

**useTransition**:

- You control when to mark updates as non-urgent
- Provides `isPending` flag
- Use when you trigger the update

**useDeferredValue**:

- React decides when to defer
- No pending flag (show stale content)
- Use when you receive the value as prop

## 📏 Hook Rules

React enforces two critical rules for hooks:

### Rule 1: Only Call Hooks at Top Level

```javascript
// ❌ Wrong - conditional hook call
function BadComponent({ isLoggedIn }) {
  if (isLoggedIn) {
    const [user, setUser] = useState(null); // ERROR
  }
  return <div>...</div>;
}

// ❌ Wrong - hook in loop
function BadComponentLoop({ items }) {
  for (let i = 0; i < items.length; i++) {
    const [value, setValue] = useState(null); // ERROR
  }
  return <div>...</div>;
}

// ✅ Correct - hooks at top level
function GoodComponent({ isLoggedIn }) {
  const [user, setUser] = useState(null);

  if (isLoggedIn) {
    // Use the hook result conditionally, not the hook itself
    return <div>Welcome, {user?.name}</div>;
  }

  return <div>Please log in</div>;
}
```

### Rule 2: Only Call Hooks from React Functions

```javascript
// ❌ Wrong - hook in regular function
function processData() {
  const [data, setData] = useState(null); // ERROR
  return data;
}

// ✅ Correct - hook in component
function DataComponent() {
  const [data, setData] = useState(null);
  return <div>{data}</div>;
}

// ✅ Correct - hook in custom hook
function useData() {
  const [data, setData] = useState(null);
  return [data, setData];
}
```

### ESLint Plugin

Install `eslint-plugin-react-hooks` to enforce rules:

```bash
npm install eslint-plugin-react-hooks --save-dev
```

**Configuration**:

```json
{
  "plugins": ["react-hooks"],
  "rules": {
    "react-hooks/rules-of-hooks": "error",
    "react-hooks/exhaustive-deps": "warn"
  }
}
```

## 🧪 Testing Hooks

Use `@testing-library/react-hooks` (legacy) or `@testing-library/react` (modern) to test hooks.

### Modern Approach (Recommended)

```javascript
import { renderHook, act } from "@testing-library/react";
import { useZakatCalculator } from "./useZakatCalculator";

describe("useZakatCalculator", () => {
  test("calculates zakat correctly", () => {
    const { result } = renderHook(() => useZakatCalculator());

    expect(result.current.zakatAmount).toBe(0);

    act(() => {
      result.current.setAmount(10000);
    });

    expect(result.current.zakatAmount).toBe(250); // 2.5% of 10000
  });

  test("returns 0 when below nisab", () => {
    const { result } = renderHook(() => useZakatCalculator());

    act(() => {
      result.current.setAmount(1000);
      result.current.setNisab(85);
      result.current.setGoldPrice(60);
    });

    const nisabValue = 85 * 60; // 5100
    expect(result.current.isAboveNisab).toBe(false);
    expect(result.current.zakatAmount).toBe(0);
  });

  test("updates nisab value when gold price changes", () => {
    const { result } = renderHook(() => useZakatCalculator());

    act(() => {
      result.current.setNisab(85);
      result.current.setGoldPrice(50);
    });

    expect(result.current.nisabValue).toBe(4250);

    act(() => {
      result.current.setGoldPrice(60);
    });

    expect(result.current.nisabValue).toBe(5100);
  });
});
```

### Testing Custom Hooks with Context

```javascript
import { renderHook } from "@testing-library/react";
import { AuthProvider, useAuth } from "./AuthContext";

describe("useAuth", () => {
  test("provides auth context", () => {
    const wrapper = ({ children }) => <AuthProvider>{children}</AuthProvider>;

    const { result } = renderHook(() => useAuth(), { wrapper });

    expect(result.current.user).toBeNull();
    expect(typeof result.current.login).toBe("function");
    expect(typeof result.current.logout).toBe("function");
  });

  test("login updates user", () => {
    const wrapper = ({ children }) => <AuthProvider>{children}</AuthProvider>;

    const { result } = renderHook(() => useAuth(), { wrapper });

    act(() => {
      result.current.login({ id: 1, name: "Ahmad" });
    });

    expect(result.current.user).toEqual({ id: 1, name: "Ahmad" });
  });
});
```

### Testing Effects

```javascript
import { renderHook, waitFor } from "@testing-library/react";

function useFetchDonations() {
  const [donations, setDonations] = useState([]);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    fetch("/api/donations")
      .then((res) => res.json())
      .then((data) => {
        setDonations(data);
        setLoading(false);
      });
  }, []);

  return { donations, loading };
}

describe("useFetchDonations", () => {
  beforeEach(() => {
    global.fetch = jest.fn();
  });

  test("fetches donations on mount", async () => {
    const mockDonations = [{ id: 1, amount: 100, donor: "Ahmad" }];

    global.fetch.mockResolvedValueOnce({
      json: async () => mockDonations,
    });

    const { result } = renderHook(() => useFetchDonations());

    expect(result.current.loading).toBe(true);
    expect(result.current.donations).toEqual([]);

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    expect(result.current.donations).toEqual(mockDonations);
    expect(global.fetch).toHaveBeenCalledWith("/api/donations");
  });
});
```

## 📚 References

**Official Documentation**:

- [React Hooks Documentation](https://react.dev/reference/react/hooks) - Complete hook reference
- [Rules of Hooks](https://react.dev/warnings/invalid-hook-call-warning) - Hook usage rules
- [Building Your Own Hooks](https://react.dev/learn/reusing-logic-with-custom-hooks) - Custom hook patterns

**Testing**:

- [React Testing Library](https://testing-library.com/docs/react-testing-library/intro/) - Component and hook testing
- [Testing React Hooks](https://react-hooks-testing-library.com/) - Legacy hook testing library

**Related Documentation**:

- [React Component Patterns](./ex-so-plwe-fera__component-patterns.md) - Component composition patterns
- [React State Management](ex-so-plwe-fere__state-management.md) - Advanced state management strategies
- [React Performance Optimization](./ex-so-plwe-fera__performance-optimization.md) - Performance best practices

**Core Principles**:

- [Functional Programming](../../../../development/pattern/functional-programming.md) - Pure functions and immutability
- [Accessibility First](../../../../principles/content/accessibility-first.md) - Accessibility standards (useId for ARIA)

---

React hooks enable functional components to manage state, handle side effects, and reuse stateful logic. Understanding hook fundamentals, rules, and patterns is essential for modern React development.
