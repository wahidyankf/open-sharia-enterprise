# React Custom Hook Template

Production-ready TypeScript custom hook template following OSE Platform conventions.

## Template

```typescript
import { useState, useEffect, useCallback, useRef } from "react";

/**
 * Options for the custom hook
 */
interface UseCustomHookOptions {
  enabled?: boolean;
  onSuccess?: (data: CustomData) => void;
  onError?: (error: Error) => void;
}

/**
 * Return type for the custom hook
 */
interface UseCustomHookReturn {
  data: CustomData | null;
  loading: boolean;
  error: Error | null;
  refetch: () => Promise<void>;
}

/**
 * Data structure
 */
interface CustomData {
  id: string;
  value: number;
  timestamp: Date;
}

/**
 * useCustomHook - Brief description of what this hook does
 *
 * @param param - Required parameter
 * @param options - Optional configuration
 * @returns Hook state and methods
 */
export function useCustomHook(param: string, options: UseCustomHookOptions = {}): UseCustomHookReturn {
  const { enabled = true, onSuccess, onError } = options;

  // State
  const [data, setData] = useState<CustomData | null>(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<Error | null>(null);

  // Ref to track mounted state
  const isMountedRef = useRef(true);

  // Fetch function
  const fetchData = useCallback(async () => {
    if (!enabled) return;

    setLoading(true);
    setError(null);

    try {
      const response = await fetch(`/api/endpoint/${param}`);
      if (!response.ok) {
        throw new Error(`HTTP error: ${response.status}`);
      }

      const result = await response.json();

      // Only update state if component is still mounted
      if (isMountedRef.current) {
        setData(result);
        onSuccess?.(result);
      }
    } catch (err) {
      const error = err instanceof Error ? err : new Error("Unknown error");

      if (isMountedRef.current) {
        setError(error);
        onError?.(error);
      }
    } finally {
      if (isMountedRef.current) {
        setLoading(false);
      }
    }
  }, [param, enabled, onSuccess, onError]);

  // Fetch on mount and when dependencies change
  useEffect(() => {
    fetchData();
  }, [fetchData]);

  // Cleanup
  useEffect(() => {
    return () => {
      isMountedRef.current = false;
    };
  }, []);

  return {
    data,
    loading,
    error,
    refetch: fetchData,
  };
}
```

## Usage

### Basic Usage

```typescript
import { useCustomHook } from './useCustomHook';

function MyComponent() {
  const { data, loading, error } = useCustomHook('param-value');

  if (loading) return <div>Loading...</div>;
  if (error) return <div>Error: {error.message}</div>;

  return <div>Data: {JSON.stringify(data)}</div>;
}
```

### With Options

```typescript
import { useCustomHook } from './useCustomHook';

function MyComponent() {
  const { data, loading, error, refetch } = useCustomHook('param-value', {
    enabled: true,
    onSuccess: (data) => {
      console.log('Data loaded:', data);
    },
    onError: (error) => {
      console.error('Failed to load:', error);
    },
  });

  return (
    <div>
      {loading && <p>Loading...</p>}
      {error && <p>Error: {error.message}</p>}
      {data && <p>Value: {data.value}</p>}
      <button onClick={refetch}>Refresh</button>
    </div>
  );
}
```

## OSE Platform Examples

### useZakatCalculation Hook

```typescript
import { useState, useCallback } from 'react';

interface ZakatCalculationInput {
  wealth: number;
  currency: string;
  userId: string;
}

interface ZakatCalculationResult {
  nisabThreshold: number;
  zakatAmount: number;
  zakatDue: boolean;
  calculationDate: Date;
}

interface UseZakatCalculationReturn {
  result: ZakatCalculationResult | null;
  loading: boolean;
  error: Error | null;
  calculate: (input: ZakatCalculationInput) => Promise<void>;
}

export function useZakatCalculation(): UseZakatCalculationReturn {
  const [result, setResult] = useState<ZakatCalculationResult | null>(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<Error | null>(null);

  const calculate = useCallback(async (input: ZakatCalculationInput) => {
    setLoading(true);
    setError(null);

    try {
      const response = await fetch('/api/zakat/calculate', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(input),
      });

      if (!response.ok) {
        throw new Error('Failed to calculate zakat');
      }

      const data = await response.json();
      setResult(data);
    } catch (err) {
      setError(err instanceof Error ? err : new Error('Unknown error'));
    } finally {
      setLoading(false);
    }
  }, []);

  return { result, loading, error, calculate };
}

// Usage
function ZakatCalculatorComponent() {
  const [wealth, setWealth] = useState(0);
  const { result, loading, error, calculate } = useZakatCalculation();

  const handleCalculate = () => {
    calculate({
      wealth,
      currency: 'USD',
      userId: 'current-user-id',
    });
  };

  return (
    <div>
      <input
        type="number"
        value={wealth}
        onChange={(e) => setWealth(Number(e.target.value))}
      />
      <button onClick={handleCalculate} disabled={loading}>
        Calculate
      </button>

      {loading && <p>Calculating...</p>}
      {error && <p>Error: {error.message}</p>}
      {result && (
        <div>
          <p>Nisab Threshold: ${result.nisabThreshold.toFixed(2)}</p>
          <p>Zakat Due: {result.zakatDue ? 'Yes' : 'No'}</p>
          {result.zakatDue && (
            <p>Zakat Amount: ${result.zakatAmount.toFixed(2)}</p>
          )}
        </div>
      )}
    </div>
  );
}
```

### useMurabahaContract Hook

```typescript
import { useState, useEffect } from "react";

interface MurabahaContract {
  id: string;
  purchasePrice: number;
  markup: number;
  totalAmount: number;
  installments: number;
  status: "pending" | "active" | "completed";
}

interface UseMurabahaContractReturn {
  contract: MurabahaContract | null;
  loading: boolean;
  error: Error | null;
  refresh: () => void;
}

export function useMurabahaContract(contractId: string): UseMurabahaContractReturn {
  const [contract, setContract] = useState<MurabahaContract | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<Error | null>(null);
  const [refreshKey, setRefreshKey] = useState(0);

  useEffect(() => {
    let cancelled = false;

    const fetchContract = async () => {
      setLoading(true);
      setError(null);

      try {
        const response = await fetch(`/api/murabaha/contracts/${contractId}`);
        if (!response.ok) throw new Error("Failed to fetch contract");

        const data = await response.json();

        if (!cancelled) {
          setContract(data);
        }
      } catch (err) {
        if (!cancelled) {
          setError(err instanceof Error ? err : new Error("Unknown error"));
        }
      } finally {
        if (!cancelled) {
          setLoading(false);
        }
      }
    };

    fetchContract();

    return () => {
      cancelled = true;
    };
  }, [contractId, refreshKey]);

  const refresh = () => {
    setRefreshKey((prev) => prev + 1);
  };

  return { contract, loading, error, refresh };
}
```

### useWaqfDonations Hook

```typescript
import { useState, useEffect } from "react";

interface WaqfDonation {
  id: string;
  amount: number;
  projectId: string;
  projectName: string;
  date: Date;
  recurring: boolean;
}

interface UseWaqfDonationsOptions {
  userId: string;
  projectId?: string;
  limit?: number;
}

interface UseWaqfDonationsReturn {
  donations: WaqfDonation[];
  totalDonated: number;
  loading: boolean;
  error: Error | null;
}

export function useWaqfDonations(options: UseWaqfDonationsOptions): UseWaqfDonationsReturn {
  const { userId, projectId, limit = 10 } = options;

  const [donations, setDonations] = useState<WaqfDonation[]>([]);
  const [totalDonated, setTotalDonated] = useState(0);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<Error | null>(null);

  useEffect(() => {
    const fetchDonations = async () => {
      try {
        const params = new URLSearchParams({
          userId,
          ...(projectId && { projectId }),
          limit: limit.toString(),
        });

        const response = await fetch(`/api/waqf/donations?${params}`);
        if (!response.ok) throw new Error("Failed to fetch donations");

        const data = await response.json();
        setDonations(data.donations);
        setTotalDonated(data.totalAmount);
      } catch (err) {
        setError(err instanceof Error ? err : new Error("Unknown error"));
      } finally {
        setLoading(false);
      }
    };

    fetchDonations();
  }, [userId, projectId, limit]);

  return { donations, totalDonated, loading, error };
}
```

## Hook Patterns

### State Management

```typescript
// Multiple related states
const [data, setData] = useState<Data | null>(null);
const [loading, setLoading] = useState(false);
const [error, setError] = useState<Error | null>(null);

// Or single state object
interface HookState {
  data: Data | null;
  loading: boolean;
  error: Error | null;
}
const [state, setState] = useState<HookState>({
  data: null,
  loading: false,
  error: null,
});
```

### Async Operations

```typescript
// With useCallback for stable reference
const fetchData = useCallback(async () => {
  setLoading(true);
  try {
    const result = await apiCall();
    setData(result);
  } catch (err) {
    setError(err as Error);
  } finally {
    setLoading(false);
  }
}, [dependencies]);
```

### Cleanup

```typescript
// Track mounted state
const isMountedRef = useRef(true);

useEffect(() => {
  return () => {
    isMountedRef.current = false;
  };
}, []);

// Use in async operations
if (isMountedRef.current) {
  setData(result);
}
```

## Best Practices

1. **Naming**: Always prefix with `use` (e.g., `useZakatCalculation`)
2. **TypeScript**: Define interfaces for options and return types
3. **Cleanup**: Use `isMountedRef` to prevent state updates after unmount
4. **Memoization**: Use `useCallback` for functions to maintain stable references
5. **Options Object**: Use options object for multiple optional parameters
6. **Return Object**: Return object with named properties, not array
7. **Error Handling**: Always handle errors and expose them to consumers
8. **Loading States**: Expose loading state for UI feedback
9. **Cancellation**: Cancel async operations on unmount
10. **Testing**: Write unit tests for all hooks

## Testing

```typescript
import { renderHook, waitFor } from "@testing-library/react";
import { useCustomHook } from "./useCustomHook";

describe("useCustomHook", () => {
  it("fetches data successfully", async () => {
    global.fetch = jest.fn(() =>
      Promise.resolve({
        ok: true,
        json: () => Promise.resolve({ id: "1", value: 42 }),
      }),
    ) as jest.Mock;

    const { result } = renderHook(() => useCustomHook("test-param"));

    expect(result.current.loading).toBe(true);

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    expect(result.current.data).toEqual({ id: "1", value: 42 });
    expect(result.current.error).toBeNull();
  });

  it("handles errors", async () => {
    global.fetch = jest.fn(() => Promise.reject(new Error("API Error")));

    const { result } = renderHook(() => useCustomHook("test-param"));

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    expect(result.current.data).toBeNull();
    expect(result.current.error?.message).toBe("API Error");
  });

  it("calls onSuccess callback", async () => {
    const onSuccess = jest.fn();

    global.fetch = jest.fn(() =>
      Promise.resolve({
        ok: true,
        json: () => Promise.resolve({ id: "1", value: 42 }),
      }),
    ) as jest.Mock;

    renderHook(() => useCustomHook("test-param", { onSuccess }));

    await waitFor(() => {
      expect(onSuccess).toHaveBeenCalledWith({ id: "1", value: 42 });
    });
  });

  it("refetches data", async () => {
    global.fetch = jest.fn(() =>
      Promise.resolve({
        ok: true,
        json: () => Promise.resolve({ id: "1", value: 42 }),
      }),
    ) as jest.Mock;

    const { result } = renderHook(() => useCustomHook("test-param"));

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    expect(global.fetch).toHaveBeenCalledTimes(1);

    await result.current.refetch();

    expect(global.fetch).toHaveBeenCalledTimes(2);
  });
});
```
