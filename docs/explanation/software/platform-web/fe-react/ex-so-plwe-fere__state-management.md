---
title: React State Management
description: Comprehensive guide to state management strategies in React applications including local state, global state, server state, and form state with practical examples
category: explanation
tags:
  - react
  - state-management
  - frontend
  - typescript
  - forms
  - api
created: 2026-01-29
updated: 2026-01-29
---

# React State Management

This guide provides comprehensive coverage of state management strategies in React applications, from local component state to complex global state patterns. We'll explore when to use each approach, best practices, and practical examples using Sharia-compliant business scenarios.

## 📋 Overview

State management is one of the most critical aspects of React application development. Choosing the right state management approach depends on:

- **Scope**: How many components need access?
- **Lifetime**: How long should the state persist?
- **Source**: Where does the data originate?
- **Frequency**: How often does it change?
- **Complexity**: How complex are state updates?

## 🎯 State Categories

Modern React applications typically manage five categories of state:

1. **Local State** - Component-level state (useState, useReducer)
2. **Global State** - Application-wide state (Context API, Zustand, Redux)
3. **Server State** - Backend data cache (React Query, SWR)
4. **Form State** - Form inputs and validation (React Hook Form)
5. **URL State** - Navigation and query parameters (React Router)

## 🔧 Local State

### useState Hook

The `useState` hook is the foundation of local state management in React.

**When to use**:

- Simple component-level state
- UI state (toggles, modals, tabs)
- Form inputs (simple forms)
- Derived state from props

**Basic Example - Donation Amount Selector**:

```typescript
import { useState } from 'react';

interface DonationSelectorProps {
  onAmountChange: (amount: number) => void;
}

export function DonationSelector({ onAmountChange }: DonationSelectorProps) {
  const [amount, setAmount] = useState<number>(0);
  const [customAmount, setCustomAmount] = useState<string>('');

  const presetAmounts = [50, 100, 250, 500, 1000];

  const handlePresetClick = (preset: number) => {
    setAmount(preset);
    setCustomAmount('');
    onAmountChange(preset);
  };

  const handleCustomChange = (value: string) => {
    setCustomAmount(value);
    const numValue = parseFloat(value);
    if (!isNaN(numValue) && numValue > 0) {
      setAmount(numValue);
      onAmountChange(numValue);
    }
  };

  return (
    <div className="donation-selector">
      <div className="preset-buttons">
        {presetAmounts.map((preset) => (
          <button
            key={preset}
            onClick={() => handlePresetClick(preset)}
            className={amount === preset ? 'active' : ''}
          >
            ${preset}
          </button>
        ))}
      </div>

      <div className="custom-amount">
        <label htmlFor="custom">Custom Amount:</label>
        <input
          id="custom"
          type="number"
          value={customAmount}
          onChange={(e) => handleCustomChange(e.target.value)}
          placeholder="Enter amount"
        />
      </div>

      {amount > 0 && (
        <p className="selected-amount">
          Selected: ${amount.toFixed(2)}
        </p>
      )}
    </div>
  );
}
```

### useReducer Hook

The `useReducer` hook provides more structured state management for complex state logic.

**When to use**:

- Complex state objects with multiple sub-values
- State transitions depend on previous state
- Multiple related state updates
- State logic becomes complex

**Advanced Example - Zakat Calculator State**:

```typescript
import { useReducer } from "react";

// State shape
interface ZakatState {
  assets: {
    cash: number;
    gold: number;
    silver: number;
    investments: number;
    inventory: number;
  };
  liabilities: {
    shortTermDebt: number;
    immediateExpenses: number;
  };
  nisab: number;
  zakatRate: number;
  calculationResult: {
    netAssets: number;
    zakatDue: number;
    isEligible: boolean;
  } | null;
  isCalculating: boolean;
  error: string | null;
}

// Action types
type ZakatAction =
  | { type: "UPDATE_ASSET"; field: keyof ZakatState["assets"]; value: number }
  | { type: "UPDATE_LIABILITY"; field: keyof ZakatState["liabilities"]; value: number }
  | { type: "UPDATE_NISAB"; value: number }
  | { type: "CALCULATE_ZAKAT" }
  | { type: "CALCULATION_SUCCESS"; result: ZakatState["calculationResult"] }
  | { type: "CALCULATION_ERROR"; error: string }
  | { type: "RESET" };

// Initial state
const initialState: ZakatState = {
  assets: {
    cash: 0,
    gold: 0,
    silver: 0,
    investments: 0,
    inventory: 0,
  },
  liabilities: {
    shortTermDebt: 0,
    immediateExpenses: 0,
  },
  nisab: 5000, // Example nisab value in currency
  zakatRate: 0.025, // 2.5%
  calculationResult: null,
  isCalculating: false,
  error: null,
};

// Reducer function
function zakatReducer(state: ZakatState, action: ZakatAction): ZakatState {
  switch (action.type) {
    case "UPDATE_ASSET":
      return {
        ...state,
        assets: {
          ...state.assets,
          [action.field]: action.value,
        },
        calculationResult: null, // Clear previous result
      };

    case "UPDATE_LIABILITY":
      return {
        ...state,
        liabilities: {
          ...state.liabilities,
          [action.field]: action.value,
        },
        calculationResult: null,
      };

    case "UPDATE_NISAB":
      return {
        ...state,
        nisab: action.value,
        calculationResult: null,
      };

    case "CALCULATE_ZAKAT": {
      // Calculate total assets
      const totalAssets = Object.values(state.assets).reduce((sum, value) => sum + value, 0);

      // Calculate total liabilities
      const totalLiabilities = Object.values(state.liabilities).reduce((sum, value) => sum + value, 0);

      // Calculate net zakatable assets
      const netAssets = totalAssets - totalLiabilities;

      // Check if eligible (meets nisab threshold)
      const isEligible = netAssets >= state.nisab;

      // Calculate zakat due
      const zakatDue = isEligible ? netAssets * state.zakatRate : 0;

      return {
        ...state,
        isCalculating: false,
        calculationResult: {
          netAssets,
          zakatDue,
          isEligible,
        },
        error: null,
      };
    }

    case "CALCULATION_SUCCESS":
      return {
        ...state,
        isCalculating: false,
        calculationResult: action.result,
        error: null,
      };

    case "CALCULATION_ERROR":
      return {
        ...state,
        isCalculating: false,
        error: action.error,
      };

    case "RESET":
      return initialState;

    default:
      return state;
  }
}

// Hook wrapper for reusability
export function useZakatCalculator() {
  const [state, dispatch] = useReducer(zakatReducer, initialState);

  const updateAsset = (field: keyof ZakatState["assets"], value: number) => {
    dispatch({ type: "UPDATE_ASSET", field, value });
  };

  const updateLiability = (field: keyof ZakatState["liabilities"], value: number) => {
    dispatch({ type: "UPDATE_LIABILITY", field, value });
  };

  const updateNisab = (value: number) => {
    dispatch({ type: "UPDATE_NISAB", value });
  };

  const calculateZakat = () => {
    dispatch({ type: "CALCULATE_ZAKAT" });
  };

  const reset = () => {
    dispatch({ type: "RESET" });
  };

  return {
    state,
    updateAsset,
    updateLiability,
    updateNisab,
    calculateZakat,
    reset,
  };
}
```

**Usage**:

```typescript
export function ZakatCalculatorComponent() {
  const {
    state,
    updateAsset,
    updateLiability,
    calculateZakat,
    reset,
  } = useZakatCalculator();

  return (
    <div className="zakat-calculator">
      <h2>Zakat Calculator</h2>

      {/* Asset inputs */}
      <section>
        <h3>Assets</h3>
        <input
          type="number"
          value={state.assets.cash}
          onChange={(e) => updateAsset('cash', parseFloat(e.target.value) || 0)}
          placeholder="Cash"
        />
        {/* More asset inputs... */}
      </section>

      {/* Liability inputs */}
      <section>
        <h3>Liabilities</h3>
        <input
          type="number"
          value={state.liabilities.shortTermDebt}
          onChange={(e) => updateLiability('shortTermDebt', parseFloat(e.target.value) || 0)}
          placeholder="Short-term Debt"
        />
        {/* More liability inputs... */}
      </section>

      <button onClick={calculateZakat}>Calculate Zakat</button>
      <button onClick={reset}>Reset</button>

      {/* Results */}
      {state.calculationResult && (
        <div className="results">
          <p>Net Assets: ${state.calculationResult.netAssets.toFixed(2)}</p>
          {state.calculationResult.isEligible ? (
            <p>Zakat Due: ${state.calculationResult.zakatDue.toFixed(2)}</p>
          ) : (
            <p>Not eligible (below nisab threshold)</p>
          )}
        </div>
      )}

      {state.error && <p className="error">{state.error}</p>}
    </div>
  );
}
```

## 🌐 Global State Strategies

### When to Use Global State

**Consider global state when**:

- Multiple components across different tree branches need the same data
- State needs to persist across route changes
- Prop drilling becomes excessive (3+ levels)
- State represents true application-level concern

**Avoid global state when**:

- State is only needed in one component tree
- State is short-lived or temporary
- Server data can be cached instead
- Props can be passed through composition

### Context API

React's built-in solution for sharing state across component trees.

**When to use**:

- Theme, language, authentication status
- Small to medium application state
- Avoiding prop drilling
- No need for time-travel debugging

**Example - Authentication Context**:

```typescript
import { createContext, useContext, useState, useCallback, ReactNode } from 'react';

// User type
interface User {
  id: string;
  name: string;
  email: string;
  role: 'donor' | 'recipient' | 'admin';
}

// Context value type
interface AuthContextValue {
  user: User | null;
  isAuthenticated: boolean;
  isLoading: boolean;
  login: (email: string, password: string) => Promise<void>;
  logout: () => Promise<void>;
  updateUser: (updates: Partial<User>) => void;
}

// Create context with undefined default (forces usage within provider)
const AuthContext = createContext<AuthContextValue | undefined>(undefined);

// Provider props
interface AuthProviderProps {
  children: ReactNode;
}

// Provider component
export function AuthProvider({ children }: AuthProviderProps) {
  const [user, setUser] = useState<User | null>(null);
  const [isLoading, setIsLoading] = useState<boolean>(false);

  const login = useCallback(async (email: string, password: string) => {
    setIsLoading(true);
    try {
      // Simulate API call
      const response = await fetch('/api/auth/login', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ email, password }),
      });

      if (!response.ok) {
        throw new Error('Login failed');
      }

      const userData = await response.json();
      setUser(userData);
    } catch (error) {
      console.error('Login error:', error);
      throw error;
    } finally {
      setIsLoading(false);
    }
  }, []);

  const logout = useCallback(async () => {
    setIsLoading(true);
    try {
      await fetch('/api/auth/logout', { method: 'POST' });
      setUser(null);
    } catch (error) {
      console.error('Logout error:', error);
    } finally {
      setIsLoading(false);
    }
  }, []);

  const updateUser = useCallback((updates: Partial<User>) => {
    setUser((prev) => (prev ? { ...prev, ...updates } : null));
  }, []);

  const value: AuthContextValue = {
    user,
    isAuthenticated: user !== null,
    isLoading,
    login,
    logout,
    updateUser,
  };

  return <AuthContext.Provider value={value}>{children}</AuthContext.Provider>;
}

// Custom hook for consuming context
export function useAuth() {
  const context = useContext(AuthContext);
  if (context === undefined) {
    throw new Error('useAuth must be used within AuthProvider');
  }
  return context;
}
```

**Usage**:

```typescript
// In app root
export function App() {
  return (
    <AuthProvider>
      <Router>
        <AppRoutes />
      </Router>
    </AuthProvider>
  );
}

// In any component
export function ProfileButton() {
  const { user, isAuthenticated, logout } = useAuth();

  if (!isAuthenticated) {
    return <Link to="/login">Login</Link>;
  }

  return (
    <div className="profile-menu">
      <span>Welcome, {user?.name}</span>
      <button onClick={logout}>Logout</button>
    </div>
  );
}
```

### State Management Libraries

#### Zustand (Recommended)

Lightweight state management with minimal boilerplate.

**When to use**:

- Need global state without Context API re-render issues
- Want simpler API than Redux
- Need middleware (persist, devtools)
- Medium to large applications

**Example - Donation Campaign Store**:

```typescript
import { create } from "zustand";
import { devtools, persist } from "zustand/middleware";

// Campaign type
interface Campaign {
  id: string;
  title: string;
  goal: number;
  raised: number;
  category: "zakat" | "sadaqah" | "emergency" | "education";
  isActive: boolean;
}

// Store state
interface CampaignStore {
  campaigns: Campaign[];
  selectedCampaign: Campaign | null;
  filter: Campaign["category"] | "all";
  isLoading: boolean;
  error: string | null;

  // Actions
  setCampaigns: (campaigns: Campaign[]) => void;
  selectCampaign: (id: string) => void;
  setFilter: (filter: Campaign["category"] | "all") => void;
  addCampaign: (campaign: Campaign) => void;
  updateCampaign: (id: string, updates: Partial<Campaign>) => void;
  deleteCampaign: (id: string) => void;
  fetchCampaigns: () => Promise<void>;
}

// Create store with middleware
export const useCampaignStore = create<CampaignStore>()(
  devtools(
    persist(
      (set, get) => ({
        // Initial state
        campaigns: [],
        selectedCampaign: null,
        filter: "all",
        isLoading: false,
        error: null,

        // Actions
        setCampaigns: (campaigns) => set({ campaigns }),

        selectCampaign: (id) => {
          const campaign = get().campaigns.find((c) => c.id === id);
          set({ selectedCampaign: campaign || null });
        },

        setFilter: (filter) => set({ filter }),

        addCampaign: (campaign) =>
          set((state) => ({
            campaigns: [...state.campaigns, campaign],
          })),

        updateCampaign: (id, updates) =>
          set((state) => ({
            campaigns: state.campaigns.map((c) => (c.id === id ? { ...c, ...updates } : c)),
          })),

        deleteCampaign: (id) =>
          set((state) => ({
            campaigns: state.campaigns.filter((c) => c.id !== id),
            selectedCampaign: state.selectedCampaign?.id === id ? null : state.selectedCampaign,
          })),

        fetchCampaigns: async () => {
          set({ isLoading: true, error: null });
          try {
            const response = await fetch("/api/campaigns");
            if (!response.ok) throw new Error("Failed to fetch campaigns");
            const campaigns = await response.json();
            set({ campaigns, isLoading: false });
          } catch (error) {
            set({
              error: error instanceof Error ? error.message : "Unknown error",
              isLoading: false,
            });
          }
        },
      }),
      {
        name: "campaign-storage", // localStorage key
        partialize: (state) => ({ filter: state.filter }), // Only persist filter
      },
    ),
  ),
);

// Selector hooks for optimized re-renders
export const useActiveCampaigns = () =>
  useCampaignStore((state) =>
    state.campaigns.filter((c) => c.isActive && (state.filter === "all" || c.category === state.filter)),
  );

export const useCampaignById = (id: string) => useCampaignStore((state) => state.campaigns.find((c) => c.id === id));
```

**Usage**:

```typescript
export function CampaignList() {
  const campaigns = useActiveCampaigns();
  const { setFilter, fetchCampaigns, isLoading } = useCampaignStore();

  useEffect(() => {
    fetchCampaigns();
  }, [fetchCampaigns]);

  if (isLoading) return <div>Loading campaigns...</div>;

  return (
    <div>
      <div className="filters">
        <button onClick={() => setFilter('all')}>All</button>
        <button onClick={() => setFilter('zakat')}>Zakat</button>
        <button onClick={() => setFilter('sadaqah')}>Sadaqah</button>
        <button onClick={() => setFilter('emergency')}>Emergency</button>
      </div>

      <div className="campaign-grid">
        {campaigns.map((campaign) => (
          <CampaignCard key={campaign.id} campaign={campaign} />
        ))}
      </div>
    </div>
  );
}
```

#### Redux Toolkit

Industry-standard state management with powerful tooling.

**When to use**:

- Large, complex applications
- Need time-travel debugging
- Strong typing requirements
- Team familiar with Redux patterns

**Example - Donation History Slice**:

```typescript
import { createSlice, createAsyncThunk, PayloadAction } from "@reduxjs/toolkit";

// Types
interface Donation {
  id: string;
  amount: number;
  campaignId: string;
  date: string;
  status: "pending" | "completed" | "failed";
  receiptUrl?: string;
}

interface DonationHistoryState {
  donations: Donation[];
  currentPage: number;
  totalPages: number;
  isLoading: boolean;
  error: string | null;
}

// Async thunks
export const fetchDonations = createAsyncThunk("donationHistory/fetch", async (page: number, { rejectWithValue }) => {
  try {
    const response = await fetch(`/api/donations?page=${page}`);
    if (!response.ok) throw new Error("Failed to fetch donations");
    return await response.json();
  } catch (error) {
    return rejectWithValue(error instanceof Error ? error.message : "Unknown error");
  }
});

// Slice
const donationHistorySlice = createSlice({
  name: "donationHistory",
  initialState: {
    donations: [],
    currentPage: 1,
    totalPages: 1,
    isLoading: false,
    error: null,
  } as DonationHistoryState,
  reducers: {
    setCurrentPage: (state, action: PayloadAction<number>) => {
      state.currentPage = action.payload;
    },
    addDonation: (state, action: PayloadAction<Donation>) => {
      state.donations.unshift(action.payload);
    },
    updateDonationStatus: (state, action: PayloadAction<{ id: string; status: Donation["status"] }>) => {
      const donation = state.donations.find((d) => d.id === action.payload.id);
      if (donation) {
        donation.status = action.payload.status;
      }
    },
  },
  extraReducers: (builder) => {
    builder
      .addCase(fetchDonations.pending, (state) => {
        state.isLoading = true;
        state.error = null;
      })
      .addCase(fetchDonations.fulfilled, (state, action) => {
        state.isLoading = false;
        state.donations = action.payload.donations;
        state.totalPages = action.payload.totalPages;
      })
      .addCase(fetchDonations.rejected, (state, action) => {
        state.isLoading = false;
        state.error = action.payload as string;
      });
  },
});

export const { setCurrentPage, addDonation, updateDonationStatus } = donationHistorySlice.actions;

export default donationHistorySlice.reducer;
```

## 🔄 Server State Management

Server state (data from APIs) requires different handling than client state due to caching, refetching, and synchronization needs.

### React Query (TanStack Query)

Modern server state management with intelligent caching.

**When to use**:

- Fetching, caching, and updating server data
- Need background refetching
- Want optimistic updates
- Pagination, infinite scroll

**Installation**:

```bash
npm install @tanstack/react-query
```

**Setup**:

```typescript
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { ReactQueryDevtools } from '@tanstack/react-query-devtools';

// Create client
const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      staleTime: 60 * 1000, // 1 minute
      retry: 1,
    },
  },
});

// Wrap app
export function App() {
  return (
    <QueryClientProvider client={queryClient}>
      <AppContent />
      <ReactQueryDevtools initialIsOpen={false} />
    </QueryClientProvider>
  );
}
```

**Example - Campaign Data Fetching**:

```typescript
import { useQuery, useMutation, useQueryClient } from "@tanstack/react-query";

// Types
interface Campaign {
  id: string;
  title: string;
  goal: number;
  raised: number;
}

// API functions
const campaignApi = {
  getAll: async (): Promise<Campaign[]> => {
    const response = await fetch("/api/campaigns");
    if (!response.ok) throw new Error("Failed to fetch campaigns");
    return response.json();
  },

  getById: async (id: string): Promise<Campaign> => {
    const response = await fetch(`/api/campaigns/${id}`);
    if (!response.ok) throw new Error("Failed to fetch campaign");
    return response.json();
  },

  create: async (data: Omit<Campaign, "id">): Promise<Campaign> => {
    const response = await fetch("/api/campaigns", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(data),
    });
    if (!response.ok) throw new Error("Failed to create campaign");
    return response.json();
  },

  donate: async (campaignId: string, amount: number): Promise<void> => {
    const response = await fetch(`/api/campaigns/${campaignId}/donate`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ amount }),
    });
    if (!response.ok) throw new Error("Donation failed");
  },
};

// Custom hooks
export function useCampaigns() {
  return useQuery({
    queryKey: ["campaigns"],
    queryFn: campaignApi.getAll,
  });
}

export function useCampaign(id: string) {
  return useQuery({
    queryKey: ["campaigns", id],
    queryFn: () => campaignApi.getById(id),
    enabled: !!id, // Only fetch if id exists
  });
}

export function useCreateCampaign() {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: campaignApi.create,
    onSuccess: () => {
      // Invalidate and refetch campaigns list
      queryClient.invalidateQueries({ queryKey: ["campaigns"] });
    },
  });
}

export function useDonate(campaignId: string) {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: (amount: number) => campaignApi.donate(campaignId, amount),
    onMutate: async (amount) => {
      // Cancel outgoing refetches
      await queryClient.cancelQueries({ queryKey: ["campaigns", campaignId] });

      // Snapshot previous value
      const previousCampaign = queryClient.getQueryData<Campaign>(["campaigns", campaignId]);

      // Optimistically update
      queryClient.setQueryData<Campaign>(["campaigns", campaignId], (old) =>
        old ? { ...old, raised: old.raised + amount } : undefined,
      );

      return { previousCampaign };
    },
    onError: (err, amount, context) => {
      // Rollback on error
      if (context?.previousCampaign) {
        queryClient.setQueryData(["campaigns", campaignId], context.previousCampaign);
      }
    },
    onSettled: () => {
      // Refetch after success or error
      queryClient.invalidateQueries({ queryKey: ["campaigns", campaignId] });
      queryClient.invalidateQueries({ queryKey: ["campaigns"] });
    },
  });
}
```

**Usage**:

```typescript
export function CampaignDetail({ campaignId }: { campaignId: string }) {
  const { data: campaign, isLoading, error } = useCampaign(campaignId);
  const donate = useDonate(campaignId);
  const [amount, setAmount] = useState<number>(0);

  const handleDonate = async () => {
    try {
      await donate.mutateAsync(amount);
      alert('Donation successful!');
      setAmount(0);
    } catch (error) {
      alert('Donation failed. Please try again.');
    }
  };

  if (isLoading) return <div>Loading campaign...</div>;
  if (error) return <div>Error: {error.message}</div>;
  if (!campaign) return <div>Campaign not found</div>;

  return (
    <div className="campaign-detail">
      <h1>{campaign.title}</h1>
      <p>Goal: ${campaign.goal}</p>
      <p>Raised: ${campaign.raised}</p>

      <div className="donate-form">
        <input
          type="number"
          value={amount}
          onChange={(e) => setAmount(parseFloat(e.target.value) || 0)}
          placeholder="Enter amount"
        />
        <button onClick={handleDonate} disabled={donate.isPending}>
          {donate.isPending ? 'Processing...' : 'Donate'}
        </button>
      </div>
    </div>
  );
}
```

## 📝 Form State Management

### React Hook Form

Performant, flexible form library with minimal re-renders.

**When to use**:

- Complex forms with validation
- Performance-critical forms
- Need integration with validation libraries (Zod, Yup)
- Want uncontrolled components

**Installation**:

```bash
npm install react-hook-form zod @hookform/resolvers
```

**Example - Donation Form with Validation**:

```typescript
import { useForm, Controller } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { z } from 'zod';

// Validation schema
const donationSchema = z.object({
  amount: z
    .number({
      required_error: 'Amount is required',
      invalid_type_error: 'Amount must be a number',
    })
    .min(1, 'Minimum donation is $1')
    .max(1000000, 'Maximum donation is $1,000,000'),

  campaignId: z.string().min(1, 'Please select a campaign'),

  frequency: z.enum(['once', 'monthly', 'yearly'], {
    required_error: 'Please select donation frequency',
  }),

  isAnonymous: z.boolean().default(false),

  dedicationMessage: z
    .string()
    .max(500, 'Message must be less than 500 characters')
    .optional(),

  paymentMethod: z.enum(['card', 'bank', 'wallet'], {
    required_error: 'Please select payment method',
  }),
});

type DonationFormData = z.infer<typeof donationSchema>;

// Component
export function DonationForm() {
  const {
    register,
    handleSubmit,
    control,
    watch,
    formState: { errors, isSubmitting, isValid },
    reset,
  } = useForm<DonationFormData>({
    resolver: zodResolver(donationSchema),
    defaultValues: {
      amount: 0,
      campaignId: '',
      frequency: 'once',
      isAnonymous: false,
      dedicationMessage: '',
      paymentMethod: 'card',
    },
    mode: 'onChange', // Validate on change
  });

  const frequency = watch('frequency');
  const amount = watch('amount');

  const onSubmit = async (data: DonationFormData) => {
    try {
      const response = await fetch('/api/donations', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(data),
      });

      if (!response.ok) throw new Error('Donation failed');

      alert('Donation successful!');
      reset(); // Reset form after success
    } catch (error) {
      alert('Donation failed. Please try again.');
    }
  };

  return (
    <form onSubmit={handleSubmit(onSubmit)} className="donation-form">
      {/* Amount */}
      <div className="form-field">
        <label htmlFor="amount">Donation Amount *</label>
        <input
          id="amount"
          type="number"
          {...register('amount', { valueAsNumber: true })}
        />
        {errors.amount && (
          <span className="error">{errors.amount.message}</span>
        )}
      </div>

      {/* Campaign Selection */}
      <div className="form-field">
        <label htmlFor="campaignId">Campaign *</label>
        <select id="campaignId" {...register('campaignId')}>
          <option value="">Select a campaign</option>
          <option value="campaign-1">Emergency Relief</option>
          <option value="campaign-2">Education Fund</option>
          <option value="campaign-3">Zakat Distribution</option>
        </select>
        {errors.campaignId && (
          <span className="error">{errors.campaignId.message}</span>
        )}
      </div>

      {/* Frequency */}
      <div className="form-field">
        <label>Donation Frequency *</label>
        <div className="radio-group">
          <label>
            <input type="radio" value="once" {...register('frequency')} />
            One-time
          </label>
          <label>
            <input type="radio" value="monthly" {...register('frequency')} />
            Monthly
          </label>
          <label>
            <input type="radio" value="yearly" {...register('frequency')} />
            Yearly
          </label>
        </div>
        {errors.frequency && (
          <span className="error">{errors.frequency.message}</span>
        )}
      </div>

      {/* Recurring donation info */}
      {frequency !== 'once' && amount > 0 && (
        <div className="info-box">
          <p>
            You will be charged ${amount} {frequency} starting today.
          </p>
        </div>
      )}

      {/* Anonymous donation */}
      <div className="form-field">
        <label>
          <input type="checkbox" {...register('isAnonymous')} />
          Make this donation anonymous
        </label>
      </div>

      {/* Dedication message */}
      <div className="form-field">
        <label htmlFor="dedicationMessage">
          Dedication Message (Optional)
        </label>
        <textarea
          id="dedicationMessage"
          {...register('dedicationMessage')}
          rows={4}
          placeholder="In memory of..."
        />
        {errors.dedicationMessage && (
          <span className="error">{errors.dedicationMessage.message}</span>
        )}
      </div>

      {/* Payment method */}
      <div className="form-field">
        <label htmlFor="paymentMethod">Payment Method *</label>
        <Controller
          name="paymentMethod"
          control={control}
          render={({ field }) => (
            <select id="paymentMethod" {...field}>
              <option value="card">Credit/Debit Card</option>
              <option value="bank">Bank Transfer</option>
              <option value="wallet">Digital Wallet</option>
            </select>
          )}
        />
        {errors.paymentMethod && (
          <span className="error">{errors.paymentMethod.message}</span>
        )}
      </div>

      {/* Submit */}
      <button type="submit" disabled={!isValid || isSubmitting}>
        {isSubmitting ? 'Processing...' : `Donate $${amount}`}
      </button>
    </form>
  );
}
```

### Controlled vs Uncontrolled Components

**Controlled Components**: React state controls input value.

```typescript
// Controlled
const [value, setValue] = useState('');

<input
  value={value}
  onChange={(e) => setValue(e.target.value)}
/>
```

**Pros**:

- Full control over input
- Easy to validate in real-time
- Can transform input immediately

**Cons**:

- More re-renders
- More verbose code

**Uncontrolled Components**: DOM controls input value (use refs).

```typescript
// Uncontrolled
const inputRef = useRef<HTMLInputElement>(null);

<input ref={inputRef} defaultValue="" />

// Access value when needed
const value = inputRef.current?.value;
```

**Pros**:

- Better performance (fewer re-renders)
- Less code
- Works well with React Hook Form

**Cons**:

- Less control
- Harder to validate in real-time
- Need refs to access values

**Recommendation**: Use React Hook Form (uncontrolled) for most forms. Use controlled components when you need immediate input transformation or complex cross-field validation.

## 🔗 URL State Management

URL state stores application state in the URL, enabling shareable, bookmarkable application states.

### React Router

**When to use URL state**:

- Pagination, filtering, sorting
- Search queries
- Tab selection
- Modal open/close
- Shareable application states

**Example - Campaign List with URL Parameters**:

```typescript
import { useSearchParams, useNavigate } from 'react-router-dom';

interface CampaignFilters {
  category: string;
  sortBy: string;
  page: number;
  search: string;
}

export function CampaignListPage() {
  const [searchParams, setSearchParams] = useSearchParams();
  const navigate = useNavigate();

  // Parse URL parameters
  const filters: CampaignFilters = {
    category: searchParams.get('category') || 'all',
    sortBy: searchParams.get('sortBy') || 'recent',
    page: parseInt(searchParams.get('page') || '1', 10),
    search: searchParams.get('search') || '',
  };

  // Update URL when filters change
  const updateFilter = (key: keyof CampaignFilters, value: string | number) => {
    const newParams = new URLSearchParams(searchParams);
    newParams.set(key, value.toString());

    // Reset to page 1 when filters change
    if (key !== 'page') {
      newParams.set('page', '1');
    }

    setSearchParams(newParams);
  };

  const handleCategoryChange = (category: string) => {
    updateFilter('category', category);
  };

  const handleSortChange = (sortBy: string) => {
    updateFilter('sortBy', sortBy);
  };

  const handlePageChange = (page: number) => {
    updateFilter('page', page);
  };

  const handleSearch = (search: string) => {
    updateFilter('search', search);
  };

  // Fetch campaigns based on URL parameters
  const { data: campaigns, isLoading } = useCampaigns(filters);

  return (
    <div className="campaign-list-page">
      {/* Search */}
      <input
        type="search"
        value={filters.search}
        onChange={(e) => handleSearch(e.target.value)}
        placeholder="Search campaigns..."
      />

      {/* Category filter */}
      <div className="category-filter">
        <button
          onClick={() => handleCategoryChange('all')}
          className={filters.category === 'all' ? 'active' : ''}
        >
          All
        </button>
        <button
          onClick={() => handleCategoryChange('zakat')}
          className={filters.category === 'zakat' ? 'active' : ''}
        >
          Zakat
        </button>
        <button
          onClick={() => handleCategoryChange('sadaqah')}
          className={filters.category === 'sadaqah' ? 'active' : ''}
        >
          Sadaqah
        </button>
      </div>

      {/* Sort */}
      <select value={filters.sortBy} onChange={(e) => handleSortChange(e.target.value)}>
        <option value="recent">Most Recent</option>
        <option value="raised">Most Raised</option>
        <option value="goal">Goal Amount</option>
      </select>

      {/* Results */}
      {isLoading ? (
        <div>Loading...</div>
      ) : (
        <>
          <div className="campaign-grid">
            {campaigns?.items.map((campaign) => (
              <CampaignCard key={campaign.id} campaign={campaign} />
            ))}
          </div>

          {/* Pagination */}
          <div className="pagination">
            <button
              onClick={() => handlePageChange(filters.page - 1)}
              disabled={filters.page === 1}
            >
              Previous
            </button>
            <span>Page {filters.page}</span>
            <button
              onClick={() => handlePageChange(filters.page + 1)}
              disabled={filters.page >= (campaigns?.totalPages || 1)}
            >
              Next
            </button>
          </div>
        </>
      )}
    </div>
  );
}
```

## 💾 State Persistence

Persisting state across page refreshes using browser storage.

### localStorage

Synchronous, persistent storage (no expiration).

```typescript
import { useState, useEffect } from "react";

// Generic localStorage hook
function useLocalStorage<T>(key: string, initialValue: T) {
  const [storedValue, setStoredValue] = useState<T>(() => {
    try {
      const item = window.localStorage.getItem(key);
      return item ? JSON.parse(item) : initialValue;
    } catch (error) {
      console.error(`Error reading localStorage key "${key}":`, error);
      return initialValue;
    }
  });

  const setValue = (value: T | ((val: T) => T)) => {
    try {
      const valueToStore = value instanceof Function ? value(storedValue) : value;

      setStoredValue(valueToStore);
      window.localStorage.setItem(key, JSON.stringify(valueToStore));
    } catch (error) {
      console.error(`Error setting localStorage key "${key}":`, error);
    }
  };

  return [storedValue, setValue] as const;
}

// Usage - User preferences
interface UserPreferences {
  theme: "light" | "dark";
  language: "en" | "ar";
  currency: "USD" | "EUR" | "GBP";
  notifications: boolean;
}

export function useUserPreferences() {
  const [preferences, setPreferences] = useLocalStorage<UserPreferences>("user-preferences", {
    theme: "light",
    language: "en",
    currency: "USD",
    notifications: true,
  });

  const updatePreference = <K extends keyof UserPreferences>(key: K, value: UserPreferences[K]) => {
    setPreferences((prev) => ({ ...prev, [key]: value }));
  };

  return { preferences, updatePreference };
}
```

### sessionStorage

Similar to localStorage but cleared when tab closes.

```typescript
function useSessionStorage<T>(key: string, initialValue: T) {
  const [storedValue, setStoredValue] = useState<T>(() => {
    try {
      const item = window.sessionStorage.getItem(key);
      return item ? JSON.parse(item) : initialValue;
    } catch (error) {
      return initialValue;
    }
  });

  const setValue = (value: T | ((val: T) => T)) => {
    try {
      const valueToStore = value instanceof Function ? value(storedValue) : value;

      setStoredValue(valueToStore);
      window.sessionStorage.setItem(key, JSON.stringify(valueToStore));
    } catch (error) {
      console.error("Error setting sessionStorage:", error);
    }
  };

  return [storedValue, setValue] as const;
}

// Usage - Form draft
export function useDraftDonation() {
  const [draft, setDraft] = useSessionStorage("donation-draft", {
    amount: 0,
    campaignId: "",
  });

  return { draft, setDraft };
}
```

## 🚀 State Initialization

### From Props

Initializing state from props requires careful handling.

```typescript
import { useState, useEffect } from 'react';

interface CampaignEditorProps {
  initialCampaign?: Campaign;
}

// ❌ WRONG - Creates stale state
export function CampaignEditor({ initialCampaign }: CampaignEditorProps) {
  const [campaign, setCampaign] = useState(initialCampaign);
  // Campaign won't update if initialCampaign prop changes!

  return <div>{/* ... */}</div>;
}

// ✅ CORRECT - Using key prop
export function CampaignEditorWrapper({ campaignId }: { campaignId: string }) {
  const { data: campaign } = useCampaign(campaignId);

  return <CampaignEditor key={campaignId} initialCampaign={campaign} />;
  // Key change forces remount and reinitializes state
}

// ✅ ALTERNATIVE - Sync with useEffect
export function CampaignEditor({ initialCampaign }: CampaignEditorProps) {
  const [campaign, setCampaign] = useState(initialCampaign);

  useEffect(() => {
    if (initialCampaign) {
      setCampaign(initialCampaign);
    }
  }, [initialCampaign]);

  return <div>{/* ... */}</div>;
}
```

### Async Initialization

Loading initial state asynchronously.

```typescript
import { useState, useEffect } from 'react';

interface AsyncInitState<T> {
  data: T | null;
  isLoading: boolean;
  error: Error | null;
}

function useAsyncInit<T>(
  asyncFn: () => Promise<T>,
  deps: React.DependencyList = []
): AsyncInitState<T> {
  const [state, setState] = useState<AsyncInitState<T>>({
    data: null,
    isLoading: true,
    error: null,
  });

  useEffect(() => {
    let cancelled = false;

    setState({ data: null, isLoading: true, error: null });

    asyncFn()
      .then((data) => {
        if (!cancelled) {
          setState({ data, isLoading: false, error: null });
        }
      })
      .catch((error) => {
        if (!cancelled) {
          setState({ data: null, isLoading: false, error });
        }
      });

    return () => {
      cancelled = true;
    };
  }, deps);

  return state;
}

// Usage
export function CampaignDetail({ campaignId }: { campaignId: string }) {
  const { data: campaign, isLoading, error } = useAsyncInit(
    () => fetch(`/api/campaigns/${campaignId}`).then((r) => r.json()),
    [campaignId]
  );

  if (isLoading) return <div>Loading...</div>;
  if (error) return <div>Error: {error.message}</div>;
  if (!campaign) return <div>Not found</div>;

  return <div>{campaign.title}</div>;
}
```

## 🔷 TypeScript Patterns

### Type-Safe State

Using TypeScript for type-safe state management.

```typescript
// Discriminated unions for state status
type AsyncState<T> =
  | { status: 'idle' }
  | { status: 'loading' }
  | { status: 'success'; data: T }
  | { status: 'error'; error: Error };

function useCampaignAsync(id: string) {
  const [state, setState] = useState<AsyncState<Campaign>>({ status: 'idle' });

  useEffect(() => {
    setState({ status: 'loading' });

    fetch(`/api/campaigns/${id}`)
      .then((r) => r.json())
      .then((data) => setState({ status: 'success', data }))
      .catch((error) => setState({ status: 'error', error }));
  }, [id]);

  return state;
}

// Usage with type narrowing
export function CampaignDisplay({ campaignId }: { campaignId: string }) {
  const state = useCampaignAsync(campaignId);

  switch (state.status) {
    case 'idle':
      return <div>Click to load</div>;

    case 'loading':
      return <div>Loading...</div>;

    case 'success':
      // TypeScript knows state.data exists here
      return <div>{state.data.title}</div>;

    case 'error':
      // TypeScript knows state.error exists here
      return <div>Error: {state.error.message}</div>;
  }
}
```

### Generic State Hooks

Creating reusable, type-safe hooks.

```typescript
// Generic CRUD hook
interface CrudState<T> {
  items: T[];
  isLoading: boolean;
  error: Error | null;
}

interface CrudActions<T> {
  create: (item: Omit<T, 'id'>) => Promise<void>;
  update: (id: string, updates: Partial<T>) => Promise<void>;
  delete: (id: string) => Promise<void>;
  refresh: () => Promise<void>;
}

function useCrud<T extends { id: string }>(
  endpoint: string
): [CrudState<T>, CrudActions<T>] {
  const [state, setState] = useState<CrudState<T>>({
    items: [],
    isLoading: false,
    error: null,
  });

  const refresh = async () => {
    setState((prev) => ({ ...prev, isLoading: true }));
    try {
      const response = await fetch(endpoint);
      const items = await response.json();
      setState({ items, isLoading: false, error: null });
    } catch (error) {
      setState((prev) => ({
        ...prev,
        isLoading: false,
        error: error as Error,
      }));
    }
  };

  const create = async (item: Omit<T, 'id'>) => {
    const response = await fetch(endpoint, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(item),
    });
    if (!response.ok) throw new Error('Create failed');
    await refresh();
  };

  const update = async (id: string, updates: Partial<T>) => {
    const response = await fetch(`${endpoint}/${id}`, {
      method: 'PATCH',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(updates),
    });
    if (!response.ok) throw new Error('Update failed');
    await refresh();
  };

  const deleteItem = async (id: string) => {
    const response = await fetch(`${endpoint}/${id}`, {
      method: 'DELETE',
    });
    if (!response.ok) throw new Error('Delete failed');
    await refresh();
  };

  useEffect(() => {
    refresh();
  }, [endpoint]);

  return [
    state,
    {
      create,
      update,
      delete: deleteItem,
      refresh,
    },
  ];
}

// Usage
interface Campaign {
  id: string;
  title: string;
  goal: number;
}

export function CampaignManager() {
  const [state, actions] = useCrud<Campaign>('/api/campaigns');

  return (
    <div>
      {state.items.map((campaign) => (
        <div key={campaign.id}>
          <span>{campaign.title}</span>
          <button onClick={() => actions.delete(campaign.id)}>Delete</button>
        </div>
      ))}
    </div>
  );
}
```

## 🧪 Testing State

### Testing useState and useReducer

```typescript
import { renderHook, act } from "@testing-library/react";
import { useZakatCalculator } from "./useZakatCalculator";

describe("useZakatCalculator", () => {
  it("should calculate zakat correctly", () => {
    const { result } = renderHook(() => useZakatCalculator());

    act(() => {
      result.current.updateAsset("cash", 10000);
      result.current.updateAsset("gold", 5000);
      result.current.calculateZakat();
    });

    expect(result.current.state.calculationResult?.netAssets).toBe(15000);
    expect(result.current.state.calculationResult?.zakatDue).toBe(375); // 2.5% of 15000
    expect(result.current.state.calculationResult?.isEligible).toBe(true);
  });

  it("should not calculate zakat if below nisab", () => {
    const { result } = renderHook(() => useZakatCalculator());

    act(() => {
      result.current.updateAsset("cash", 1000); // Below default nisab of 5000
      result.current.calculateZakat();
    });

    expect(result.current.state.calculationResult?.isEligible).toBe(false);
    expect(result.current.state.calculationResult?.zakatDue).toBe(0);
  });
});
```

### Testing Context

```typescript
import { render, screen } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { AuthProvider, useAuth } from './AuthContext';

function TestComponent() {
  const { user, login, logout } = useAuth();

  return (
    <div>
      {user ? (
        <>
          <span>Logged in as {user.name}</span>
          <button onClick={logout}>Logout</button>
        </>
      ) : (
        <button onClick={() => login('test@example.com', 'password')}>
          Login
        </button>
      )}
    </div>
  );
}

describe('AuthContext', () => {
  it('should login and logout', async () => {
    const user = userEvent.setup();

    render(
      <AuthProvider>
        <TestComponent />
      </AuthProvider>
    );

    // Initially not logged in
    expect(screen.getByText('Login')).toBeInTheDocument();

    // Login
    await user.click(screen.getByText('Login'));

    // After login
    await screen.findByText(/Logged in as/);

    // Logout
    await user.click(screen.getByText('Logout'));

    // After logout
    expect(screen.getByText('Login')).toBeInTheDocument();
  });
});
```

### Testing Zustand Store

```typescript
import { renderHook, act } from "@testing-library/react";
import { useCampaignStore } from "./campaignStore";

describe("campaignStore", () => {
  beforeEach(() => {
    // Reset store before each test
    useCampaignStore.setState({
      campaigns: [],
      selectedCampaign: null,
      filter: "all",
    });
  });

  it("should add campaign", () => {
    const { result } = renderHook(() => useCampaignStore());

    const campaign = {
      id: "1",
      title: "Test Campaign",
      goal: 10000,
      raised: 0,
      category: "zakat" as const,
      isActive: true,
    };

    act(() => {
      result.current.addCampaign(campaign);
    });

    expect(result.current.campaigns).toHaveLength(1);
    expect(result.current.campaigns[0]).toEqual(campaign);
  });

  it("should filter campaigns", () => {
    const { result } = renderHook(() => useCampaignStore());

    const campaigns = [
      {
        id: "1",
        title: "Zakat Campaign",
        goal: 10000,
        raised: 0,
        category: "zakat" as const,
        isActive: true,
      },
      {
        id: "2",
        title: "Sadaqah Campaign",
        goal: 5000,
        raised: 0,
        category: "sadaqah" as const,
        isActive: true,
      },
    ];

    act(() => {
      result.current.setCampaigns(campaigns);
      result.current.setFilter("zakat");
    });

    expect(result.current.filter).toBe("zakat");
  });
});
```

## ⚡ Performance Optimization

### Avoiding Unnecessary Re-renders

**Problem**: Every state update causes component re-render.

**Solutions**:

#### 1. Split State

```typescript
// ❌ BAD - Changes to any field re-render entire form
function DonationForm() {
  const [formData, setFormData] = useState({
    amount: 0,
    campaignId: '',
    frequency: 'once',
    message: '',
  });

  return <div>{/* All inputs re-render on any change */}</div>;
}

// ✅ GOOD - Independent state for each field
function DonationForm() {
  const [amount, setAmount] = useState(0);
  const [campaignId, setCampaignId] = useState('');
  const [frequency, setFrequency] = useState('once');
  const [message, setMessage] = useState('');

  return <div>{/* Only changed input re-renders */}</div>;
}
```

#### 2. Use React.memo

```typescript
import { memo } from 'react';

interface CampaignCardProps {
  campaign: Campaign;
  onDonate: (id: string) => void;
}

// Memoize component to prevent re-renders when props haven't changed
export const CampaignCard = memo(function CampaignCard({
  campaign,
  onDonate,
}: CampaignCardProps) {
  return (
    <div className="campaign-card">
      <h3>{campaign.title}</h3>
      <p>Goal: ${campaign.goal}</p>
      <p>Raised: ${campaign.raised}</p>
      <button onClick={() => onDonate(campaign.id)}>Donate</button>
    </div>
  );
});
```

#### 3. Use useCallback and useMemo

```typescript
import { useState, useCallback, useMemo } from 'react';

export function CampaignList() {
  const [campaigns, setCampaigns] = useState<Campaign[]>([]);
  const [filter, setFilter] = useState<string>('all');

  // Memoize callback to prevent child re-renders
  const handleDonate = useCallback((id: string) => {
    console.log('Donating to campaign:', id);
    // Donation logic...
  }, []); // Empty deps - callback never changes

  // Memoize expensive computation
  const filteredCampaigns = useMemo(() => {
    return campaigns.filter((c) =>
      filter === 'all' ? true : c.category === filter
    );
  }, [campaigns, filter]); // Only recompute when campaigns or filter change

  return (
    <div>
      <select value={filter} onChange={(e) => setFilter(e.target.value)}>
        <option value="all">All</option>
        <option value="zakat">Zakat</option>
        <option value="sadaqah">Sadaqah</option>
      </select>

      {filteredCampaigns.map((campaign) => (
        <CampaignCard
          key={campaign.id}
          campaign={campaign}
          onDonate={handleDonate}
        />
      ))}
    </div>
  );
}
```

#### 4. Use Selectors (Zustand)

```typescript
// ❌ BAD - Component re-renders on ANY store change
function CampaignCount() {
  const store = useCampaignStore();
  return <div>Total: {store.campaigns.length}</div>;
}

// ✅ GOOD - Only re-renders when campaigns.length changes
function CampaignCount() {
  const count = useCampaignStore((state) => state.campaigns.length);
  return <div>Total: {count}</div>;
}
```

### State Update Batching

React 18 automatically batches state updates.

```typescript
// All three updates are batched into single re-render
function handleClick() {
  setAmount(100);
  setCampaignId("campaign-1");
  setFrequency("monthly");
  // Only one re-render!
}
```

### Lazy Initialization

Expensive initial state calculation can be lazily initialized.

```typescript
// ❌ BAD - Runs expensive calculation on every render
const [state, setState] = useState(expensiveCalculation());

// ✅ GOOD - Runs expensive calculation only once
const [state, setState] = useState(() => expensiveCalculation());
```

## 📚 References

**React Documentation**:

- [useState Hook](https://react.dev/reference/react/useState)
- [useReducer Hook](https://react.dev/reference/react/useReducer)
- [Context API](https://react.dev/learn/passing-data-deeply-with-context)
- [Performance Optimization](https://react.dev/learn/render-and-commit)

**Libraries**:

- [Zustand](https://github.com/pmndrs/zustand) - Lightweight state management
- [Redux Toolkit](https://redux-toolkit.js.org/) - Redux with less boilerplate
- [React Query](https://tanstack.com/query/latest) - Server state management
- [React Hook Form](https://react-hook-form.com/) - Performant form library
- [Zod](https://zod.dev/) - TypeScript-first schema validation

**Related Documentation**:

- [Component Composition](./ex-so-plwe-fera__component-composition.md) - Component design patterns
- [Performance Optimization](ex-so-plwe-fere__performance.md) - React performance best practices
- [TypeScript Patterns](./ex-so-plwe-fera__typescript.md) - TypeScript in React
- [Testing React Apps](ex-so-plwe-fere__testing.md) - Testing strategies

---

This comprehensive guide covers React state management from local state to complex global patterns. Choose the right tool for your specific use case, prioritize simplicity, and optimize only when necessary.
