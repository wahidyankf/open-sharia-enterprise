# React Component Template

Production-ready TypeScript React component template following OSE Platform conventions.

## Template

```typescript
import React, { useState, useEffect, useCallback } from 'react';

/**
 * Props interface for the component
 */
interface ComponentNameProps {
  // Required props
  id: string;
  title: string;

  // Optional props with defaults
  isEnabled?: boolean;
  onUpdate?: (data: ComponentData) => void;

  // Optional styling
  className?: string;
}

/**
 * Data structure for component state
 */
interface ComponentData {
  value: number;
  status: 'idle' | 'loading' | 'success' | 'error';
  message?: string;
}

/**
 * ComponentName - Brief description of what this component does
 *
 * @param props - Component props
 * @returns React component
 */
export const ComponentName: React.FC<ComponentNameProps> = ({
  id,
  title,
  isEnabled = true,
  onUpdate,
  className = '',
}) => {
  // State management
  const [data, setData] = useState<ComponentData>({
    value: 0,
    status: 'idle',
  });
  const [error, setError] = useState<string | null>(null);

  // Fetch data on mount
  useEffect(() => {
    if (!isEnabled) return;

    const fetchData = async () => {
      try {
        setData(prev => ({ ...prev, status: 'loading' }));

        // API call
        const response = await fetch(`/api/endpoint/${id}`);
        if (!response.ok) {
          throw new Error(`HTTP error: ${response.status}`);
        }

        const result = await response.json();

        setData({
          value: result.value,
          status: 'success',
          message: result.message,
        });
        setError(null);
      } catch (err) {
        setError(err instanceof Error ? err.message : 'Unknown error');
        setData(prev => ({ ...prev, status: 'error' }));
      }
    };

    fetchData();
  }, [id, isEnabled]);

  // Memoized callback
  const handleUpdate = useCallback((newValue: number) => {
    const updatedData = { ...data, value: newValue };
    setData(updatedData);

    // Notify parent
    onUpdate?.(updatedData);
  }, [data, onUpdate]);

  // Loading state
  if (data.status === 'loading') {
    return (
      <div className={`component-loading ${className}`}>
        <p>Loading...</p>
      </div>
    );
  }

  // Error state
  if (data.status === 'error') {
    return (
      <div className={`component-error ${className}`}>
        <p>Error: {error}</p>
      </div>
    );
  }

  // Success state
  return (
    <div className={`component-container ${className}`}>
      <h2>{title}</h2>
      <div className="component-content">
        <p>Value: {data.value}</p>
        {data.message && <p>{data.message}</p>}

        <button
          onClick={() => handleUpdate(data.value + 1)}
          disabled={!isEnabled}
        >
          Increment
        </button>
      </div>
    </div>
  );
};

export default ComponentName;
```

## Usage

### Basic Usage

```typescript
import { ComponentName } from './ComponentName';

function App() {
  return (
    <ComponentName
      id="123"
      title="My Component"
    />
  );
}
```

### With All Props

```typescript
import { ComponentName } from './ComponentName';

function App() {
  const handleUpdate = (data: ComponentData) => {
    console.log('Component updated:', data);
  };

  return (
    <ComponentName
      id="123"
      title="My Component"
      isEnabled={true}
      onUpdate={handleUpdate}
      className="custom-class"
    />
  );
}
```

## OSE Platform Examples

### Zakat Calculator Component

```typescript
import React, { useState, useEffect } from 'react';

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

### Murabaha Contract Display Component

```typescript
import React, { useEffect, useState } from 'react';

interface MurabahaContract {
  id: string;
  purchasePrice: number;
  markup: number;
  totalAmount: number;
  installments: number;
  installmentAmount: number;
  status: 'pending' | 'active' | 'completed';
}

interface MurabahaContractProps {
  contractId: string;
  onStatusChange?: (status: string) => void;
}

export const MurabahaContractDisplay: React.FC<MurabahaContractProps> = ({
  contractId,
  onStatusChange,
}) => {
  const [contract, setContract] = useState<MurabahaContract | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    const fetchContract = async () => {
      try {
        const response = await fetch(`/api/murabaha/contracts/${contractId}`);
        if (!response.ok) throw new Error('Failed to fetch contract');

        const data = await response.json();
        setContract(data);
        onStatusChange?.(data.status);
      } catch (err) {
        setError(err instanceof Error ? err.message : 'Unknown error');
      } finally {
        setLoading(false);
      }
    };

    fetchContract();
  }, [contractId, onStatusChange]);

  if (loading) return <div>Loading contract...</div>;
  if (error) return <div>Error: {error}</div>;
  if (!contract) return <div>Contract not found</div>;

  return (
    <div className="murabaha-contract">
      <h2>Murabaha Contract #{contract.id}</h2>

      <div className="contract-details">
        <div className="detail-row">
          <span className="label">Purchase Price:</span>
          <span className="value">${contract.purchasePrice.toFixed(2)}</span>
        </div>

        <div className="detail-row">
          <span className="label">Markup:</span>
          <span className="value">${contract.markup.toFixed(2)}</span>
        </div>

        <div className="detail-row">
          <span className="label">Total Amount:</span>
          <span className="value">${contract.totalAmount.toFixed(2)}</span>
        </div>

        <div className="detail-row">
          <span className="label">Installments:</span>
          <span className="value">{contract.installments} months</span>
        </div>

        <div className="detail-row">
          <span className="label">Monthly Payment:</span>
          <span className="value">${contract.installmentAmount.toFixed(2)}</span>
        </div>

        <div className="detail-row">
          <span className="label">Status:</span>
          <span className={`status status-${contract.status}`}>
            {contract.status.toUpperCase()}
          </span>
        </div>
      </div>
    </div>
  );
};
```

## Component Patterns

### State Management

```typescript
// Simple state
const [value, setValue] = useState<string>("");

// Complex state with interface
interface FormState {
  email: string;
  password: string;
  rememberMe: boolean;
}
const [form, setForm] = useState<FormState>({
  email: "",
  password: "",
  rememberMe: false,
});

// Updating complex state
const updateEmail = (email: string) => {
  setForm((prev) => ({ ...prev, email }));
};
```

### Effects

```typescript
// Run on mount only
useEffect(() => {
  fetchData();
}, []);

// Run when dependency changes
useEffect(() => {
  if (userId) {
    fetchUserData(userId);
  }
}, [userId]);

// Cleanup
useEffect(() => {
  const subscription = subscribeToUpdates();

  return () => {
    subscription.unsubscribe();
  };
}, []);
```

### Callbacks

```typescript
// Memoized callback
const handleClick = useCallback(() => {
  console.log('Clicked with value:', value);
}, [value]);

// Pass to child components to prevent re-renders
<ChildComponent onClick={handleClick} />
```

## Best Practices

1. **TypeScript Types**: Always define interfaces for props and state
2. **Functional Components**: Use function components with hooks
3. **Default Props**: Use default parameter values, not `defaultProps`
4. **Conditional Rendering**: Handle loading, error, and success states
5. **Memoization**: Use `useCallback` for functions passed to children
6. **Effects Cleanup**: Return cleanup function from effects when needed
7. **Prop Validation**: Use TypeScript for compile-time validation
8. **Error Boundaries**: Wrap components in error boundaries for production
9. **Accessibility**: Use semantic HTML and ARIA attributes
10. **Testing**: Write unit tests for all components

## Testing

```typescript
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { ComponentName } from './ComponentName';

describe('ComponentName', () => {
  it('renders with required props', () => {
    render(<ComponentName id="123" title="Test" />);
    expect(screen.getByText('Test')).toBeInTheDocument();
  });

  it('fetches data on mount', async () => {
    global.fetch = jest.fn(() =>
      Promise.resolve({
        ok: true,
        json: () => Promise.resolve({ value: 42, message: 'Success' }),
      })
    ) as jest.Mock;

    render(<ComponentName id="123" title="Test" />);

    await waitFor(() => {
      expect(screen.getByText('Value: 42')).toBeInTheDocument();
    });
  });

  it('handles errors', async () => {
    global.fetch = jest.fn(() => Promise.reject(new Error('API Error')));

    render(<ComponentName id="123" title="Test" />);

    await waitFor(() => {
      expect(screen.getByText(/Error: API Error/)).toBeInTheDocument();
    });
  });

  it('calls onUpdate when button clicked', async () => {
    const handleUpdate = jest.fn();

    global.fetch = jest.fn(() =>
      Promise.resolve({
        ok: true,
        json: () => Promise.resolve({ value: 10 }),
      })
    ) as jest.Mock;

    render(
      <ComponentName id="123" title="Test" onUpdate={handleUpdate} />
    );

    await waitFor(() => {
      expect(screen.getByText('Value: 10')).toBeInTheDocument();
    });

    fireEvent.click(screen.getByText('Increment'));

    expect(handleUpdate).toHaveBeenCalledWith({
      value: 11,
      status: 'success',
    });
  });
});
```
