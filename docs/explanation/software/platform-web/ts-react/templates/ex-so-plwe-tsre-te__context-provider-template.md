# React Context Provider Template

Production-ready TypeScript Context Provider template following OSE Platform conventions.

## Template

```typescript
import React, { createContext, useContext, useState, useCallback, ReactNode } from 'react';

/**
 * Context state interface
 */
interface ContextState {
  value: string;
  count: number;
  isEnabled: boolean;
}

/**
 * Context methods interface
 */
interface ContextMethods {
  setValue: (value: string) => void;
  increment: () => void;
  toggle: () => void;
  reset: () => void;
}

/**
 * Combined context interface
 */
interface ContextValue extends ContextState, ContextMethods {}

/**
 * Provider props
 */
interface ProviderProps {
  children: ReactNode;
  initialValue?: Partial<ContextState>;
}

// Create context with undefined default (will error if used outside provider)
const CustomContext = createContext<ContextValue | undefined>(undefined);

/**
 * CustomProvider - Provides context state and methods to children
 */
export const CustomProvider: React.FC<ProviderProps> = ({
  children,
  initialValue = {},
}) => {
  // Default state
  const defaultState: ContextState = {
    value: '',
    count: 0,
    isEnabled: true,
  };

  // Initialize state with defaults and overrides
  const [state, setState] = useState<ContextState>({
    ...defaultState,
    ...initialValue,
  });

  // Methods
  const setValue = useCallback((value: string) => {
    setState(prev => ({ ...prev, value }));
  }, []);

  const increment = useCallback(() => {
    setState(prev => ({ ...prev, count: prev.count + 1 }));
  }, []);

  const toggle = useCallback(() => {
    setState(prev => ({ ...prev, isEnabled: !prev.isEnabled }));
  }, []);

  const reset = useCallback(() => {
    setState(defaultState);
  }, []);

  // Combine state and methods
  const value: ContextValue = {
    ...state,
    setValue,
    increment,
    toggle,
    reset,
  };

  return (
    <CustomContext.Provider value={value}>
      {children}
    </CustomContext.Provider>
  );
};

/**
 * useCustom - Hook to access context
 * Throws error if used outside provider
 */
export const useCustom = (): ContextValue => {
  const context = useContext(CustomContext);

  if (context === undefined) {
    throw new Error('useCustom must be used within CustomProvider');
  }

  return context;
};

// Export provider and hook together
export default { CustomProvider, useCustom };
```

## Usage

### Basic Setup

```typescript
import React from 'react';
import ReactDOM from 'react-dom/client';
import { CustomProvider } from './CustomContext';
import App from './App';

const root = ReactDOM.createRoot(document.getElementById('root')!);

root.render(
  <CustomProvider>
    <App />
  </CustomProvider>
);
```

### Using the Context

```typescript
import { useCustom } from './CustomContext';

function MyComponent() {
  const { value, count, isEnabled, setValue, increment, toggle } = useCustom();

  return (
    <div>
      <p>Value: {value}</p>
      <p>Count: {count}</p>
      <p>Enabled: {isEnabled ? 'Yes' : 'No'}</p>

      <input
        value={value}
        onChange={(e) => setValue(e.target.value)}
      />

      <button onClick={increment}>Increment</button>
      <button onClick={toggle}>Toggle</button>
    </div>
  );
}
```

### With Initial Values

```typescript
<CustomProvider
  initialValue={{
    value: 'Hello',
    count: 10,
    isEnabled: false,
  }}
>
  <App />
</CustomProvider>
```

## OSE Platform Examples

### Auth Context

```typescript
import React, { createContext, useContext, useState, useCallback, ReactNode } from 'react';

interface User {
  id: string;
  email: string;
  name: string;
  roles: string[];
}

interface AuthState {
  user: User | null;
  token: string | null;
  isAuthenticated: boolean;
  loading: boolean;
}

interface AuthMethods {
  login: (email: string, password: string) => Promise<void>;
  logout: () => Promise<void>;
  refreshToken: () => Promise<void>;
}

interface AuthContextValue extends AuthState, AuthMethods {}

const AuthContext = createContext<AuthContextValue | undefined>(undefined);

export const AuthProvider: React.FC<{ children: ReactNode }> = ({ children }) => {
  const [state, setState] = useState<AuthState>({
    user: null,
    token: null,
    isAuthenticated: false,
    loading: true,
  });

  // Login method
  const login = useCallback(async (email: string, password: string) => {
    setState(prev => ({ ...prev, loading: true }));

    try {
      const response = await fetch('/api/auth/login', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ email, password }),
      });

      if (!response.ok) {
        throw new Error('Login failed');
      }

      const { user, token } = await response.json();

      setState({
        user,
        token,
        isAuthenticated: true,
        loading: false,
      });

      // Store token
      localStorage.setItem('auth_token', token);
    } catch (error) {
      setState(prev => ({ ...prev, loading: false }));
      throw error;
    }
  }, []);

  // Logout method
  const logout = useCallback(async () => {
    try {
      await fetch('/api/auth/logout', { method: 'POST' });
    } finally {
      setState({
        user: null,
        token: null,
        isAuthenticated: false,
        loading: false,
      });
      localStorage.removeItem('auth_token');
    }
  }, []);

  // Refresh token
  const refreshToken = useCallback(async () => {
    const token = localStorage.getItem('auth_token');
    if (!token) return;

    try {
      const response = await fetch('/api/auth/refresh', {
        headers: { Authorization: `Bearer ${token}` },
      });

      if (response.ok) {
        const { user, token: newToken } = await response.json();
        setState({
          user,
          token: newToken,
          isAuthenticated: true,
          loading: false,
        });
        localStorage.setItem('auth_token', newToken);
      }
    } catch (error) {
      console.error('Token refresh failed:', error);
    }
  }, []);

  const value: AuthContextValue = {
    ...state,
    login,
    logout,
    refreshToken,
  };

  return <AuthContext.Provider value={value}>{children}</AuthContext.Provider>;
};

export const useAuth = (): AuthContextValue => {
  const context = useContext(AuthContext);
  if (context === undefined) {
    throw new Error('useAuth must be used within AuthProvider');
  }
  return context;
};

// Usage
function LoginForm() {
  const { login, loading } = useAuth();
  const [email, setEmail] = useState('');
  const [password, setPassword] = useState('');

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    try {
      await login(email, password);
    } catch (error) {
      alert('Login failed');
    }
  };

  return (
    <form onSubmit={handleSubmit}>
      <input
        type="email"
        value={email}
        onChange={(e) => setEmail(e.target.value)}
      />
      <input
        type="password"
        value={password}
        onChange={(e) => setPassword(e.target.value)}
      />
      <button type="submit" disabled={loading}>
        Login
      </button>
    </form>
  );
}
```

### Zakat Context

```typescript
import React, { createContext, useContext, useState, useCallback, ReactNode } from 'react';

interface ZakatCalculation {
  id: string;
  wealth: number;
  nisabThreshold: number;
  zakatAmount: number;
  zakatDue: boolean;
  calculationDate: Date;
}

interface ZakatState {
  calculations: ZakatCalculation[];
  currentCalculation: ZakatCalculation | null;
  loading: boolean;
}

interface ZakatMethods {
  calculate: (wealth: number, currency: string) => Promise<void>;
  loadHistory: () => Promise<void>;
  clearHistory: () => void;
}

interface ZakatContextValue extends ZakatState, ZakatMethods {}

const ZakatContext = createContext<ZakatContextValue | undefined>(undefined);

export const ZakatProvider: React.FC<{ children: ReactNode }> = ({ children }) => {
  const [state, setState] = useState<ZakatState>({
    calculations: [],
    currentCalculation: null,
    loading: false,
  });

  const calculate = useCallback(async (wealth: number, currency: string) => {
    setState(prev => ({ ...prev, loading: true }));

    try {
      const response = await fetch('/api/zakat/calculate', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ wealth, currency }),
      });

      const calculation = await response.json();

      setState(prev => ({
        calculations: [calculation, ...prev.calculations],
        currentCalculation: calculation,
        loading: false,
      }));
    } catch (error) {
      setState(prev => ({ ...prev, loading: false }));
      throw error;
    }
  }, []);

  const loadHistory = useCallback(async () => {
    setState(prev => ({ ...prev, loading: true }));

    try {
      const response = await fetch('/api/zakat/calculations');
      const calculations = await response.json();

      setState(prev => ({
        ...prev,
        calculations,
        loading: false,
      }));
    } catch (error) {
      setState(prev => ({ ...prev, loading: false }));
      throw error;
    }
  }, []);

  const clearHistory = useCallback(() => {
    setState(prev => ({
      ...prev,
      calculations: [],
      currentCalculation: null,
    }));
  }, []);

  const value: ZakatContextValue = {
    ...state,
    calculate,
    loadHistory,
    clearHistory,
  };

  return <ZakatContext.Provider value={value}>{children}</ZakatContext.Provider>;
};

export const useZakat = (): ZakatContextValue => {
  const context = useContext(ZakatContext);
  if (context === undefined) {
    throw new Error('useZakat must be used within ZakatProvider');
  }
  return context;
};
```

### Murabaha Context

```typescript
import React, { createContext, useContext, useState, useCallback, ReactNode } from 'react';

interface MurabahaContract {
  id: string;
  purchasePrice: number;
  markup: number;
  totalAmount: number;
  installments: number;
  status: 'pending' | 'active' | 'completed';
}

interface MurabahaState {
  contracts: MurabahaContract[];
  activeContract: MurabahaContract | null;
  loading: boolean;
}

interface MurabahaMethods {
  createContract: (data: Partial<MurabahaContract>) => Promise<void>;
  loadContracts: () => Promise<void>;
  selectContract: (contractId: string) => void;
}

interface MurabahaContextValue extends MurabahaState, MurabahaMethods {}

const MurabahaContext = createContext<MurabahaContextValue | undefined>(undefined);

export const MurabahaProvider: React.FC<{ children: ReactNode }> = ({ children }) => {
  const [state, setState] = useState<MurabahaState>({
    contracts: [],
    activeContract: null,
    loading: false,
  });

  const createContract = useCallback(async (data: Partial<MurabahaContract>) => {
    setState(prev => ({ ...prev, loading: true }));

    try {
      const response = await fetch('/api/murabaha/contracts', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(data),
      });

      const contract = await response.json();

      setState(prev => ({
        contracts: [contract, ...prev.contracts],
        activeContract: contract,
        loading: false,
      }));
    } catch (error) {
      setState(prev => ({ ...prev, loading: false }));
      throw error;
    }
  }, []);

  const loadContracts = useCallback(async () => {
    setState(prev => ({ ...prev, loading: true }));

    try {
      const response = await fetch('/api/murabaha/contracts');
      const contracts = await response.json();

      setState(prev => ({
        ...prev,
        contracts,
        loading: false,
      }));
    } catch (error) {
      setState(prev => ({ ...prev, loading: false }));
      throw error;
    }
  }, []);

  const selectContract = useCallback((contractId: string) => {
    setState(prev => ({
      ...prev,
      activeContract: prev.contracts.find(c => c.id === contractId) || null,
    }));
  }, []);

  const value: MurabahaContextValue = {
    ...state,
    createContract,
    loadContracts,
    selectContract,
  };

  return <MurabahaContext.Provider value={value}>{children}</MurabahaContext.Provider>;
};

export const useMurabaha = (): MurabahaContextValue => {
  const context = useContext(MurabahaContext);
  if (context === undefined) {
    throw new Error('useMurabaha must be used within MurabahaProvider');
  }
  return context;
};
```

## Context Patterns

### Nested Providers

```typescript
function App() {
  return (
    <AuthProvider>
      <ZakatProvider>
        <MurabahaProvider>
          <Routes />
        </MurabahaProvider>
      </ZakatProvider>
    </AuthProvider>
  );
}
```

### Combined Provider

```typescript
interface AppProvidersProps {
  children: ReactNode;
}

export const AppProviders: React.FC<AppProvidersProps> = ({ children }) => {
  return (
    <AuthProvider>
      <ZakatProvider>
        <MurabahaProvider>
          {children}
        </MurabahaProvider>
      </ZakatProvider>
    </AuthProvider>
  );
};

// Usage
<AppProviders>
  <App />
</AppProviders>
```

## Best Practices

1. **Single Context**: Don't combine unrelated state in one context
2. **Type Safety**: Always use TypeScript interfaces for context value
3. **Error Handling**: Throw error if hook used outside provider
4. **Memoization**: Use `useCallback` for methods to prevent re-renders
5. **Split State/Methods**: Separate state interface from methods interface
6. **Default Values**: Provide sensible defaults or undefined (with error)
7. **Provider Composition**: Nest providers or create combined provider component
8. **Performance**: Split large contexts into smaller, focused contexts
9. **Testing**: Test provider and hook separately
10. **Documentation**: Document all state and methods with JSDoc

## Testing

```typescript
import { renderHook, act } from '@testing-library/react';
import { CustomProvider, useCustom } from './CustomContext';

describe('CustomContext', () => {
  const wrapper = ({ children }: { children: React.ReactNode }) => (
    <CustomProvider>{children}</CustomProvider>
  );

  it('provides initial state', () => {
    const { result } = renderHook(() => useCustom(), { wrapper });

    expect(result.current.value).toBe('');
    expect(result.current.count).toBe(0);
    expect(result.current.isEnabled).toBe(true);
  });

  it('updates value', () => {
    const { result } = renderHook(() => useCustom(), { wrapper });

    act(() => {
      result.current.setValue('test');
    });

    expect(result.current.value).toBe('test');
  });

  it('increments count', () => {
    const { result } = renderHook(() => useCustom(), { wrapper });

    act(() => {
      result.current.increment();
    });

    expect(result.current.count).toBe(1);
  });

  it('toggles enabled', () => {
    const { result } = renderHook(() => useCustom(), { wrapper });

    act(() => {
      result.current.toggle();
    });

    expect(result.current.isEnabled).toBe(false);
  });

  it('resets to defaults', () => {
    const { result } = renderHook(() => useCustom(), { wrapper });

    act(() => {
      result.current.setValue('test');
      result.current.increment();
      result.current.toggle();
      result.current.reset();
    });

    expect(result.current.value).toBe('');
    expect(result.current.count).toBe(0);
    expect(result.current.isEnabled).toBe(true);
  });

  it('throws error when used outside provider', () => {
    // Suppress console.error for this test
    const spy = jest.spyOn(console, 'error').mockImplementation(() => {});

    expect(() => {
      renderHook(() => useCustom());
    }).toThrow('useCustom must be used within CustomProvider');

    spy.mockRestore();
  });
});
```
