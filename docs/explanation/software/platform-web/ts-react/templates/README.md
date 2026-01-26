# React Code Templates

Production-ready TypeScript React templates for the open-sharia-enterprise platform. These templates provide complete, functional boilerplate code following OSE Platform conventions and React best practices.

## 📋 Available Templates

### 1. Component Template

**File**: [ex-so-plwe-tsre-te\_\_component-template.md](./ex-so-plwe-tsre-te__component-template.md)

Complete React functional component template with:

- TypeScript interfaces for props and state
- useState for state management
- useEffect for data fetching
- useCallback for memoized functions
- Loading, error, and success states
- OSE Platform examples (Zakat, Murabaha, Waqf)

**Use when**: Creating reusable UI components with state and side effects

**Example scenarios**:

- Zakat calculator with real-time calculations
- Murabaha contract display with status tracking
- Waqf project card with donation progress

### 2. Custom Hook Template

**File**: [ex-so-plwe-tsre-te\_\_custom-hook-template.md](./ex-so-plwe-tsre-te__custom-hook-template.md)

Custom React hook template with:

- TypeScript interfaces for options and return types
- State management (data, loading, error)
- Async operations with cleanup
- Memoized callbacks with useCallback
- Mounted state tracking with useRef
- Success/error callbacks

**Use when**: Creating reusable logic that can be shared across components

**Example scenarios**:

- useZakatCalculation for zakat calculations
- useMurabahaContract for contract data fetching
- useWaqfDonations for donation history

### 3. Context Provider Template

**File**: [ex-so-plwe-tsre-te\_\_context-provider-template.md](./ex-so-plwe-tsre-te__context-provider-template.md)

React Context Provider template with:

- State and methods interfaces
- Context creation with undefined default
- Provider component with memoized methods
- Custom hook with error checking
- Nested provider patterns
- OSE Platform examples

**Use when**: Managing global or shared state across component tree

**Example scenarios**:

- Auth context for user authentication
- Zakat context for calculation state
- Murabaha context for contract management

### 4. Form Template

**File**: [ex-so-plwe-tsre-te\_\_form-template.md](./ex-so-plwe-tsre-te__form-template.md)

Comprehensive form template with:

- Controlled inputs with TypeScript
- Validation with error messages
- Submission handling with loading states
- Success/error feedback
- Field-level error clearing
- OSE Platform form examples

**Use when**: Creating forms with validation and error handling

**Example scenarios**:

- Zakat calculation input form
- Murabaha financing application form
- Waqf donation form

### 5. API Client Template

**File**: [ex-so-plwe-tsre-te\_\_api-client-template.md](./ex-so-plwe-tsre-te__api-client-template.md)

Type-safe API client template with:

- Singleton API client class
- GET, POST, PUT, PATCH, DELETE methods
- Authentication token management
- Error handling with custom ApiError class
- Request timeout with AbortController
- OSE Platform API examples

**Use when**: Creating API client for backend communication

**Example scenarios**:

- Zakat API client for calculations
- Murabaha API client for contracts
- Waqf API client for donations and projects

## 🚀 Quick Start

### Using a Template

1. **Choose appropriate template** based on what you're creating (Component, Hook, Context, Form, or API Client)

2. **Copy template code** from the template file

3. **Replace placeholders**:
   - Interface names (e.g., `ComponentNameProps` → `ZakatCalculatorProps`)
   - Function names (e.g., `useCustomHook` → `useZakatCalculation`)
   - Field names and types
   - API endpoints

4. **Test thoroughly**:
   - Unit tests with React Testing Library
   - Integration tests for full flows
   - E2E tests for critical paths

### Example: Creating a Zakat Calculator Component

**Step 1**: Copy component template from `ex-so-plwe-tsre-te__component-template.md`

**Step 2**: Replace placeholders:

```typescript
// From template:
interface ComponentNameProps {
  id: string;
  title: string;
}

// Your code:
interface ZakatCalculatorProps {
  userId: string;
  currency: string;
}
```

**Step 3**: Customize business logic:

```typescript
import React, { useState } from 'react';

interface ZakatCalculatorProps {
  userId: string;
  currency: string;
}

interface ZakatCalculation {
  wealth: number;
  nisabThreshold: number;
  zakatAmount: number;
  zakatDue: boolean;
}

export const ZakatCalculator: React.FC<ZakatCalculatorProps> = ({
  userId,
  currency,
}) => {
  const [wealth, setWealth] = useState<number>(0);
  const [calculation, setCalculation] = useState<ZakatCalculation | null>(null);
  const [loading, setLoading] = useState(false);

  const calculateZakat = async () => {
    setLoading(true);
    try {
      const response = await fetch('/api/zakat/calculate', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ userId, wealth, currency }),
      });

      const result = await response.json();
      setCalculation(result);
    } catch (error) {
      console.error('Zakat calculation failed:', error);
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="zakat-calculator">
      <h2>Zakat Calculator</h2>

      <div className="input-group">
        <label htmlFor="wealth">Total Wealth ({currency})</label>
        <input
          id="wealth"
          type="number"
          value={wealth}
          onChange={(e) => setWealth(Number(e.target.value))}
          min="0"
          step="0.01"
        />
      </div>

      <button onClick={calculateZakat} disabled={loading}>
        {loading ? 'Calculating...' : 'Calculate Zakat'}
      </button>

      {calculation && (
        <div className="calculation-result">
          <h3>Results</h3>
          <p>Wealth: {calculation.wealth.toFixed(2)} {currency}</p>
          <p>Nisab Threshold: {calculation.nisabThreshold.toFixed(2)} {currency}</p>
          <p>Zakat Due: {calculation.zakatDue ? 'Yes' : 'No'}</p>
          {calculation.zakatDue && (
            <p className="zakat-amount">
              Zakat Amount: {calculation.zakatAmount.toFixed(2)} {currency}
            </p>
          )}
        </div>
      )}
    </div>
  );
};
```

**Step 4**: Write tests:

```typescript
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { ZakatCalculator } from './ZakatCalculator';

describe('ZakatCalculator', () => {
  it('renders calculator form', () => {
    render(<ZakatCalculator userId="user-123" currency="USD" />);

    expect(screen.getByText('Zakat Calculator')).toBeInTheDocument();
    expect(screen.getByLabelText(/Total Wealth/)).toBeInTheDocument();
    expect(screen.getByText('Calculate Zakat')).toBeInTheDocument();
  });

  it('calculates zakat when wealth above nisab', async () => {
    global.fetch = jest.fn(() =>
      Promise.resolve({
        json: () => Promise.resolve({
          wealth: 10000,
          nisabThreshold: 5000,
          zakatAmount: 250,
          zakatDue: true,
        }),
      })
    ) as jest.Mock;

    render(<ZakatCalculator userId="user-123" currency="USD" />);

    const input = screen.getByLabelText(/Total Wealth/);
    fireEvent.change(input, { target: { value: '10000' } });

    const button = screen.getByText('Calculate Zakat');
    fireEvent.click(button);

    await waitFor(() => {
      expect(screen.getByText('Zakat Due: Yes')).toBeInTheDocument();
      expect(screen.getByText(/Zakat Amount: 250.00/)).toBeInTheDocument();
    });
  });
});
```

**Step 5**: Integrate and deploy

## 📝 Template Conventions

All templates follow these conventions:

### TypeScript

- **Interfaces**: Define interfaces for all props, state, and data structures
- **Type annotations**: Use explicit type annotations for all parameters and return types
- **Generic types**: Use generics for reusable components (`React.FC<Props>`)
- **Optional properties**: Use `?` for optional fields, provide defaults where appropriate

### React Patterns

- **Functional components**: Use function components with hooks (not class components)
- **Controlled inputs**: Always use controlled inputs with state
- **Memoization**: Use `useCallback` for functions passed to children
- **Cleanup**: Return cleanup functions from `useEffect` when needed
- **Context**: Create context with undefined default, check in hook

### Error Handling

- **Try-catch**: Wrap async operations in try-catch blocks
- **Error state**: Track error state and display to user
- **Loading state**: Show loading indicators during async operations
- **Error messages**: Provide clear, actionable error messages

### OSE Platform Integration

- **Domain examples**: Include Zakat, Murabaha, and Waqf examples
- **Real APIs**: Use actual API endpoints in examples
- **Business logic**: Demonstrate domain-specific calculations and validations
- **Type safety**: Define TypeScript interfaces for all OSE domain objects

## 🎨 OSE Platform Context

Templates include examples from the OSE Platform Islamic finance domain:

### Zakat (Obligatory Charity)

- **Calculator component**: Input wealth, get zakat amount
- **History component**: Display past calculations
- **Nisab tracking**: Real-time gold/silver price updates
- **Notification integration**: Reminders when zakat is due

**Domain concepts**:

- Nisab: Minimum wealth threshold (85g gold or 595g silver)
- Hawal: Lunar year (354 days) for wealth tracking
- Zakat rate: 2.5% of qualifying wealth

### Murabaha (Cost-Plus Financing)

- **Application form**: Multi-step financing application
- **Contract display**: Show contract terms and installments
- **Payment component**: Make installment payments
- **Status tracking**: Track application and contract status

**Domain concepts**:

- Purchase price: Original asset cost
- Markup: Profit margin percentage
- Installments: Monthly payments over contract period
- Grace period: Days before late payment penalties

### Waqf (Endowment)

- **Project list**: Display active charitable projects
- **Donation form**: One-time or recurring donations
- **Impact tracker**: Monitor project outcomes
- **Donor dashboard**: View donation history and impact

**Domain concepts**:

- Waqf project: Charitable project with funding goal
- Donation: One-time or recurring contribution
- Impact report: Quarterly reports on project outcomes
- Donor receipt: Tax-deductible donation receipts

## 🔒 Security Best Practices

Templates incorporate security measures:

- **Input validation**: Validate all user inputs before submission
- **XSS prevention**: React auto-escapes content (no `dangerouslySetInnerHTML` in templates)
- **Authentication**: Check auth state before rendering sensitive components
- **Authorization**: Verify user permissions for actions
- **HTTPS**: Always use HTTPS for API calls in production
- **Token storage**: Store auth tokens in memory or secure storage
- **Error handling**: Don't expose sensitive info in error messages

### Example: Secured Component

```typescript
import React from 'react';
import { useAuth } from './AuthContext';

export const SecuredZakatCalculator: React.FC = () => {
  const { isAuthenticated, user } = useAuth();

  if (!isAuthenticated) {
    return (
      <div className="error">
        Please log in to use the Zakat Calculator.
      </div>
    );
  }

  if (!user.roles.includes('zakat_user')) {
    return (
      <div className="error">
        You do not have permission to access the Zakat Calculator.
      </div>
    );
  }

  return <ZakatCalculator userId={user.id} currency={user.preferredCurrency} />;
};
```

## 🧪 Testing Guidelines

Test templates thoroughly:

### Component Tests

```typescript
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { ComponentName } from './ComponentName';

describe('ComponentName', () => {
  it('renders with required props', () => {
    render(<ComponentName requiredProp="value" />);
    expect(screen.getByText(/expected text/)).toBeInTheDocument();
  });

  it('handles user interactions', () => {
    render(<ComponentName />);
    fireEvent.click(screen.getByText('Button'));
    expect(screen.getByText('Updated Text')).toBeInTheDocument();
  });

  it('handles async operations', async () => {
    render(<ComponentName />);
    fireEvent.click(screen.getByText('Load Data'));

    await waitFor(() => {
      expect(screen.getByText('Data Loaded')).toBeInTheDocument();
    });
  });
});
```

### Hook Tests

```typescript
import { renderHook, act, waitFor } from "@testing-library/react";
import { useCustomHook } from "./useCustomHook";

describe("useCustomHook", () => {
  it("returns initial state", () => {
    const { result } = renderHook(() => useCustomHook("param"));

    expect(result.current.data).toBeNull();
    expect(result.current.loading).toBe(true);
  });

  it("fetches data successfully", async () => {
    const { result } = renderHook(() => useCustomHook("param"));

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
      expect(result.current.data).toBeDefined();
    });
  });

  it("handles errors", async () => {
    // Mock failed fetch
    global.fetch = jest.fn(() => Promise.reject(new Error("API Error")));

    const { result } = renderHook(() => useCustomHook("param"));

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
      expect(result.current.error).toBeDefined();
    });
  });
});
```

### Context Tests

```typescript
import { renderHook, act } from '@testing-library/react';
import { CustomProvider, useCustom } from './CustomContext';

describe('CustomContext', () => {
  const wrapper = ({ children }: { children: React.ReactNode }) => (
    <CustomProvider>{children}</CustomProvider>
  );

  it('provides initial state', () => {
    const { result } = renderHook(() => useCustom(), { wrapper });

    expect(result.current.value).toBeDefined();
  });

  it('updates state', () => {
    const { result } = renderHook(() => useCustom(), { wrapper });

    act(() => {
      result.current.setValue('new value');
    });

    expect(result.current.value).toBe('new value');
  });

  it('throws error when used outside provider', () => {
    expect(() => {
      renderHook(() => useCustom());
    }).toThrow('useCustom must be used within CustomProvider');
  });
});
```

---

**Last Updated**: 2026-01-26
**React Version**: 18+
**TypeScript Version**: 5+
