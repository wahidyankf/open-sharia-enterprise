---
title: React + TypeScript Best Practices
description: Comprehensive best practices for building production-ready React applications with TypeScript, covering architecture, performance, security, and code quality
category: explanation
tags:
  - react
  - typescript
  - frontend
  - best-practices
  - web-development
created: 2026-01-29
updated: 2026-01-29
---

# React + TypeScript Best Practices

This document provides comprehensive best practices for building production-ready React applications with TypeScript. These guidelines emphasize maintainability, type safety, performance, accessibility, and security for enterprise applications.

## 📐 Project Structure

### Feature-Based Organization (Recommended)

Organize code by business features rather than technical layers for better scalability and team autonomy.

```
src/
├── features/
│   ├── zakat-calculation/
│   │   ├── components/
│   │   │   ├── ZakatCalculator.tsx
│   │   │   ├── ZakatForm.tsx
│   │   │   └── ZakatSummary.tsx
│   │   ├── hooks/
│   │   │   ├── useZakatCalculation.ts
│   │   │   └── useZakatRates.ts
│   │   ├── types/
│   │   │   └── zakat.types.ts
│   │   ├── api/
│   │   │   └── zakatApi.ts
│   │   ├── utils/
│   │   │   └── zakatFormulas.ts
│   │   └── index.ts
│   ├── murabaha-contracts/
│   │   ├── components/
│   │   ├── hooks/
│   │   ├── types/
│   │   └── index.ts
│   └── islamic-calendar/
│       └── ...
├── shared/
│   ├── components/
│   │   ├── ui/
│   │   │   ├── Button.tsx
│   │   │   ├── Input.tsx
│   │   │   └── Modal.tsx
│   │   └── layout/
│   │       ├── Header.tsx
│   │       └── Sidebar.tsx
│   ├── hooks/
│   │   ├── useAuth.ts
│   │   └── useLocalStorage.ts
│   ├── types/
│   │   └── common.types.ts
│   ├── utils/
│   │   ├── formatting.ts
│   │   └── validation.ts
│   └── api/
│       └── client.ts
├── App.tsx
└── main.tsx
```

**Benefits**:

- **Modularity**: Features can be developed, tested, and deployed independently
- **Scalability**: Easy to add new features without affecting existing code
- **Team autonomy**: Teams can work on different features without conflicts
- **Code discoverability**: Related code lives together

### Layer-Based Organization (Alternative)

Organize by technical responsibility when building small applications or component libraries.

```
src/
├── components/
│   ├── ZakatCalculator.tsx
│   ├── MurabahaContract.tsx
│   └── IslamicCalendar.tsx
├── hooks/
│   ├── useZakatCalculation.ts
│   └── useAuth.ts
├── types/
│   ├── zakat.types.ts
│   └── contract.types.ts
├── api/
│   ├── zakatApi.ts
│   └── contractApi.ts
├── utils/
│   └── formatting.ts
├── App.tsx
└── main.tsx
```

**When to use**: Small projects (&lt;20 components), component libraries, simple applications.

## 🧩 Component Design

### Single Responsibility Principle

Each component should do one thing well.

**❌ Wrong** (multiple responsibilities):

```typescript
// ZakatCalculatorWithValidationAndSubmission.tsx
export function ZakatCalculatorWithValidationAndSubmission() {
  const [gold, setGold] = useState(0);
  const [errors, setErrors] = useState({});

  // Validation logic
  const validate = () => { /* ... */ };

  // Calculation logic
  const calculateZakat = () => { /* ... */ };

  // API submission logic
  const submitToApi = async () => { /* ... */ };

  return (
    <div>
      {/* Form, validation messages, results, submission UI all in one */}
    </div>
  );
}
```

**✅ Correct** (separated concerns):

```typescript
// ZakatCalculator.tsx
interface ZakatCalculatorProps {
  onCalculate: (result: ZakatResult) => void;
}

export function ZakatCalculator({ onCalculate }: ZakatCalculatorProps) {
  const [gold, setGold] = useState(0);
  const { calculateZakat } = useZakatCalculation();

  const handleSubmit = () => {
    const result = calculateZakat({ gold });
    onCalculate(result);
  };

  return (
    <form onSubmit={handleSubmit}>
      <Input value={gold} onChange={setGold} label="Gold (grams)" />
      <Button type="submit">Calculate Zakat</Button>
    </form>
  );
}

// ZakatForm.tsx
export function ZakatForm() {
  const { submitZakat } = useZakatApi();
  const [result, setResult] = useState<ZakatResult | null>(null);

  const handleCalculate = async (result: ZakatResult) => {
    setResult(result);
    await submitZakat(result);
  };

  return (
    <div>
      <ZakatCalculator onCalculate={handleCalculate} />
      {result && <ZakatSummary result={result} />}
    </div>
  );
}
```

### Composition Over Prop Drilling

Use composition patterns to avoid deeply nested prop passing.

**❌ Wrong** (prop drilling):

```typescript
function App() {
  const user = useAuth();
  return <Dashboard user={user} />;
}

function Dashboard({ user }) {
  return <Sidebar user={user} />;
}

function Sidebar({ user }) {
  return <UserProfile user={user} />;
}

function UserProfile({ user }) {
  return <div>{user.name}</div>;
}
```

**✅ Correct** (composition with context):

```typescript
// AuthContext.tsx
const AuthContext = createContext<AuthUser | null>(null);

export function AuthProvider({ children }: { children: ReactNode }) {
  const user = useAuth();
  return (
    <AuthContext.Provider value={user}>
      {children}
    </AuthContext.Provider>
  );
}

export function useAuthContext() {
  const context = useContext(AuthContext);
  if (context === null) {
    throw new Error('useAuthContext must be used within AuthProvider');
  }
  return context;
}

// App.tsx
function App() {
  return (
    <AuthProvider>
      <Dashboard />
    </AuthProvider>
  );
}

// UserProfile.tsx
function UserProfile() {
  const user = useAuthContext();
  return <div>{user.name}</div>;
}
```

### Props Design

Design clear, type-safe component APIs.

**✅ Best practices**:

```typescript
// Prefer interfaces for component props (better for extension)
interface MurabahaContractProps {
  // Required props first
  contractId: string;
  principalAmount: number;
  profitMargin: number;

  // Optional props with clear defaults
  currency?: string;
  paymentSchedule?: PaymentSchedule;

  // Callbacks with descriptive names
  onApprove?: (contract: MurabahaContract) => void;
  onReject?: (reason: string) => void;

  // Avoid boolean props for multi-state values
  status: "draft" | "pending" | "approved" | "rejected";

  // Use discriminated unions for conditional props
  variant: "compact" | "detailed";
}

// Good: Explicit prop types
export function MurabahaContract({
  contractId,
  principalAmount,
  profitMargin,
  currency = "USD",
  paymentSchedule,
  onApprove,
  onReject,
  status,
  variant,
}: MurabahaContractProps) {
  // Implementation
}

// Discriminated unions for conditional props
type ButtonProps =
  | {
      variant: "primary";
      onClick: () => void;
      href?: never;
    }
  | {
      variant: "link";
      href: string;
      onClick?: never;
    };
```

## 🔤 TypeScript Usage

### Strict Mode Configuration

Always enable strict mode in tsconfig.json.

```json
{
  "compilerOptions": {
    "strict": true,
    "noUncheckedIndexedAccess": true,
    "noImplicitReturns": true,
    "noFallthroughCasesInSwitch": true,
    "noUnusedLocals": true,
    "noUnusedParameters": true,
    "exactOptionalPropertyTypes": true
  }
}
```

### Interface vs Type

**Use interfaces for**:

- Object shapes (especially component props)
- Public APIs that may be extended
- Class contracts

```typescript
// Preferred for component props
interface ZakatCalculationProps {
  assets: Asset[];
  nisabThreshold: number;
}

// Can be extended
interface ExtendedZakatProps extends ZakatCalculationProps {
  autoCalculate: boolean;
}
```

**Use types for**:

- Unions and intersections
- Primitive aliases
- Tuple types
- Function types

```typescript
// Union types
type PaymentStatus = "pending" | "completed" | "failed";

// Intersection types
type AuditedContract = MurabahaContract & AuditMetadata;

// Function types
type CalculateZakat = (assets: Asset[]) => ZakatResult;

// Tuple types
type Coordinates = [number, number];
```

### Generics for Reusable Components

Use generics for type-safe, reusable components.

```typescript
// Generic list component
interface ListProps<T> {
  items: T[];
  renderItem: (item: T, index: number) => ReactNode;
  keyExtractor: (item: T) => string;
  emptyMessage?: string;
}

export function List<T>({
  items,
  renderItem,
  keyExtractor,
  emptyMessage = 'No items found',
}: ListProps<T>) {
  if (items.length === 0) {
    return <div>{emptyMessage}</div>;
  }

  return (
    <ul>
      {items.map((item, index) => (
        <li key={keyExtractor(item)}>
          {renderItem(item, index)}
        </li>
      ))}
    </ul>
  );
}

// Usage with type inference
<List
  items={contracts}
  renderItem={(contract) => <ContractCard contract={contract} />}
  keyExtractor={(contract) => contract.id}
/>
```

### Type Guards and Narrowing

Use type guards for safe type narrowing.

```typescript
// Type guard functions
function isMurabahaContract(contract: Contract): contract is MurabahaContract {
  return contract.type === "murabaha";
}

function isIjaraContract(contract: Contract): contract is IjaraContract {
  return contract.type === "ijara";
}

// Usage
function processContract(contract: Contract) {
  if (isMurabahaContract(contract)) {
    // TypeScript knows contract is MurabahaContract here
    console.log(contract.profitMargin);
  } else if (isIjaraContract(contract)) {
    // TypeScript knows contract is IjaraContract here
    console.log(contract.rentalAmount);
  }
}

// Discriminated unions (preferred when possible)
type Contract = { type: "murabaha"; profitMargin: number } | { type: "ijara"; rentalAmount: number };

function processContract(contract: Contract) {
  switch (contract.type) {
    case "murabaha":
      console.log(contract.profitMargin);
      break;
    case "ijara":
      console.log(contract.rentalAmount);
      break;
  }
}
```

### Avoid Type Assertions

Minimize use of `as` type assertions. Prefer type guards and proper typing.

**❌ Wrong**:

```typescript
const data = await fetchContract(id);
const contract = data as MurabahaContract; // Unsafe assumption
```

**✅ Correct**:

```typescript
const data = await fetchContract(id);

// Option 1: Type guard
if (isMurabahaContract(data)) {
  const contract = data; // Safely narrowed
}

// Option 2: Runtime validation
const contract = MurabahaContractSchema.parse(data); // Throws if invalid

// Option 3: Proper API typing
const contract: MurabahaContract = await fetchMurabahaContract(id);
```

## 📊 State Management

### Local vs Global State

**Use local state for**:

- UI state (modal open/closed, form inputs)
- Temporary data (draft forms, filters)
- Component-specific data

**Use global state for**:

- Authentication state
- User preferences
- Shared application data
- Server cache

### When to Lift State Up

Lift state when:

1. Multiple components need the same data
2. State changes must trigger updates in multiple places
3. State needs to be preserved across route changes

**❌ Wrong** (premature lifting):

```typescript
// App.tsx - lifted too early
function App() {
  const [selectedTab, setSelectedTab] = useState('summary');
  return <ZakatDashboard selectedTab={selectedTab} />;
}

// ZakatDashboard.tsx
function ZakatDashboard({ selectedTab }: { selectedTab: string }) {
  // Only this component uses selectedTab
}
```

**✅ Correct** (local state):

```typescript
// ZakatDashboard.tsx
function ZakatDashboard() {
  const [selectedTab, setSelectedTab] = useState("summary");
  // State lives where it's used
}
```

### State Management Patterns

**Context for shared state**:

```typescript
// ContractContext.tsx
interface ContractContextValue {
  contracts: Contract[];
  loading: boolean;
  error: Error | null;
  refreshContracts: () => Promise<void>;
}

const ContractContext = createContext<ContractContextValue | null>(null);

export function ContractProvider({ children }: { children: ReactNode }) {
  const [contracts, setContracts] = useState<Contract[]>([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<Error | null>(null);

  const refreshContracts = async () => {
    setLoading(true);
    try {
      const data = await fetchContracts();
      setContracts(data);
    } catch (err) {
      setError(err as Error);
    } finally {
      setLoading(false);
    }
  };

  return (
    <ContractContext.Provider
      value={{ contracts, loading, error, refreshContracts }}
    >
      {children}
    </ContractContext.Provider>
  );
}

export function useContracts() {
  const context = useContext(ContractContext);
  if (context === null) {
    throw new Error('useContracts must be used within ContractProvider');
  }
  return context;
}
```

**React Query for server state**:

```typescript
// contractQueries.ts
export function useContracts() {
  return useQuery({
    queryKey: ["contracts"],
    queryFn: fetchContracts,
    staleTime: 5 * 60 * 1000, // 5 minutes
  });
}

export function useContract(id: string) {
  return useQuery({
    queryKey: ["contracts", id],
    queryFn: () => fetchContract(id),
    enabled: Boolean(id), // Only fetch when id exists
  });
}

export function useCreateContract() {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: createContract,
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["contracts"] });
    },
  });
}
```

## ⚡ Performance Optimization

### Code Splitting

Split code by routes for faster initial load.

```typescript
// App.tsx
import { lazy, Suspense } from 'react';

const ZakatCalculator = lazy(() => import('./features/zakat-calculation'));
const MurabahaContracts = lazy(() => import('./features/murabaha-contracts'));
const IslamicCalendar = lazy(() => import('./features/islamic-calendar'));

function App() {
  return (
    <Suspense fallback={<LoadingSpinner />}>
      <Routes>
        <Route path="/zakat" element={<ZakatCalculator />} />
        <Route path="/contracts" element={<MurabahaContracts />} />
        <Route path="/calendar" element={<IslamicCalendar />} />
      </Routes>
    </Suspense>
  );
}
```

### Memoization

Use memoization strategically for expensive computations.

**✅ Good use cases**:

```typescript
// Expensive calculation
const zakatAmount = useMemo(() => {
  return calculateComplexZakat(assets, rates, adjustments);
}, [assets, rates, adjustments]);

// Preventing unnecessary re-renders
const MurabahaCard = memo(function MurabahaCard({
  contract,
  onApprove,
}: MurabahaCardProps) {
  return <div>{/* ... */}</div>;
});

// Stabilizing callback references
const handleSubmit = useCallback((data: FormData) => {
  submitToApi(data);
}, [submitToApi]);
```

**❌ Avoid premature optimization**:

```typescript
// Unnecessary for simple operations
const doubled = useMemo(() => count * 2, [count]); // Overkill

// No benefit when deps change frequently
const filtered = useMemo(
  () => items.filter((item) => item.id === searchId),
  [items, searchId], // Changes every keystroke
);
```

### Virtual Lists

Use virtual lists for long lists (&gt;100 items).

```typescript
import { useVirtualizer } from '@tanstack/react-virtual';

function ContractList({ contracts }: { contracts: Contract[] }) {
  const parentRef = useRef<HTMLDivElement>(null);

  const virtualizer = useVirtualizer({
    count: contracts.length,
    getScrollElement: () => parentRef.current,
    estimateSize: () => 100, // Estimated row height
  });

  return (
    <div ref={parentRef} style={{ height: '600px', overflow: 'auto' }}>
      <div
        style={{
          height: `${virtualizer.getTotalSize()}px`,
          position: 'relative',
        }}
      >
        {virtualizer.getVirtualItems().map((virtualItem) => {
          const contract = contracts[virtualItem.index];
          return (
            <div
              key={contract.id}
              style={{
                position: 'absolute',
                top: 0,
                left: 0,
                width: '100%',
                transform: `translateY(${virtualItem.start}px)`,
              }}
            >
              <ContractCard contract={contract} />
            </div>
          );
        })}
      </div>
    </div>
  );
}
```

## 🚨 Error Handling

### Error Boundaries

Catch rendering errors and display fallback UI.

```typescript
// ErrorBoundary.tsx
interface ErrorBoundaryProps {
  children: ReactNode;
  fallback?: (error: Error) => ReactNode;
}

interface ErrorBoundaryState {
  hasError: boolean;
  error: Error | null;
}

export class ErrorBoundary extends Component<
  ErrorBoundaryProps,
  ErrorBoundaryState
> {
  constructor(props: ErrorBoundaryProps) {
    super(props);
    this.state = { hasError: false, error: null };
  }

  static getDerivedStateFromError(error: Error): ErrorBoundaryState {
    return { hasError: true, error };
  }

  componentDidCatch(error: Error, errorInfo: ErrorInfo) {
    console.error('Error boundary caught:', error, errorInfo);
    // Log to error tracking service
    logErrorToService(error, errorInfo);
  }

  render() {
    if (this.state.hasError) {
      if (this.props.fallback) {
        return this.props.fallback(this.state.error!);
      }

      return (
        <div role="alert">
          <h2>Something went wrong</h2>
          <p>We're sorry for the inconvenience. Please try refreshing the page.</p>
        </div>
      );
    }

    return this.props.children;
  }
}

// Usage
function App() {
  return (
    <ErrorBoundary fallback={(error) => <ErrorPage error={error} />}>
      <ZakatCalculator />
    </ErrorBoundary>
  );
}
```

### Async Error Handling

Handle async errors in data fetching.

```typescript
// With React Query
function ContractDetails({ id }: { id: string }) {
  const { data, error, isLoading } = useQuery({
    queryKey: ['contracts', id],
    queryFn: () => fetchContract(id),
    retry: 3,
    retryDelay: (attemptIndex) => Math.min(1000 * 2 ** attemptIndex, 30000),
  });

  if (isLoading) {
    return <LoadingSpinner />;
  }

  if (error) {
    return (
      <ErrorMessage
        title="Failed to load contract"
        message={error.message}
        onRetry={() => queryClient.invalidateQueries(['contracts', id])}
      />
    );
  }

  return <ContractCard contract={data} />;
}

// With async/await in event handlers
async function handleSubmit(data: FormData) {
  try {
    setLoading(true);
    setError(null);

    await submitContract(data);

    showSuccessToast('Contract submitted successfully');
    navigate('/contracts');
  } catch (err) {
    const error = err as Error;
    setError(error.message);
    logError(error);
  } finally {
    setLoading(false);
  }
}
```

### User-Friendly Error Messages

Provide actionable error messages to users.

```typescript
// ErrorMessage.tsx
interface ErrorMessageProps {
  title: string;
  message: string;
  actions?: Array<{ label: string; onClick: () => void }>;
}

export function ErrorMessage({ title, message, actions }: ErrorMessageProps) {
  return (
    <div role="alert" className="error-container">
      <h3>{title}</h3>
      <p>{message}</p>
      {actions && (
        <div className="error-actions">
          {actions.map((action, index) => (
            <Button key={index} onClick={action.onClick}>
              {action.label}
            </Button>
          ))}
        </div>
      )}
    </div>
  );
}

// Usage with helpful actions
<ErrorMessage
  title="Failed to calculate zakat"
  message="The zakat calculation service is temporarily unavailable."
  actions={[
    { label: 'Try Again', onClick: retry },
    { label: 'Calculate Manually', onClick: showManualCalculator },
  ]}
/>
```

## 🧪 Testing

### Unit Tests for Utilities

Test pure functions and business logic.

```typescript
// zakatFormulas.test.ts
import { describe, it, expect } from "vitest";
import { calculateZakat, isAboveNisab } from "./zakatFormulas";

describe("calculateZakat", () => {
  it("calculates 2.5% of zakatable wealth", () => {
    const wealth = 10000;
    const result = calculateZakat(wealth);
    expect(result).toBe(250);
  });

  it("returns 0 for wealth below nisab", () => {
    const wealth = 500;
    const nisab = 1000;
    const result = calculateZakat(wealth, nisab);
    expect(result).toBe(0);
  });

  it("handles decimal values correctly", () => {
    const wealth = 10000.5;
    const result = calculateZakat(wealth);
    expect(result).toBeCloseTo(250.01, 2);
  });
});

describe("isAboveNisab", () => {
  it("returns true when wealth exceeds nisab", () => {
    expect(isAboveNisab(2000, 1000)).toBe(true);
  });

  it("returns false when wealth is below nisab", () => {
    expect(isAboveNisab(500, 1000)).toBe(false);
  });

  it("returns true when wealth equals nisab", () => {
    expect(isAboveNisab(1000, 1000)).toBe(true);
  });
});
```

### Component Tests

Test component behavior and user interactions.

```typescript
// ZakatCalculator.test.tsx
import { describe, it, expect, vi } from 'vitest';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { ZakatCalculator } from './ZakatCalculator';

describe('ZakatCalculator', () => {
  it('renders input fields for asset values', () => {
    render(<ZakatCalculator />);

    expect(screen.getByLabelText(/gold/i)).toBeInTheDocument();
    expect(screen.getByLabelText(/silver/i)).toBeInTheDocument();
    expect(screen.getByLabelText(/cash/i)).toBeInTheDocument();
  });

  it('calculates zakat when form is submitted', async () => {
    const onCalculate = vi.fn();
    render(<ZakatCalculator onCalculate={onCalculate} />);

    fireEvent.change(screen.getByLabelText(/gold/i), {
      target: { value: '1000' },
    });
    fireEvent.change(screen.getByLabelText(/cash/i), {
      target: { value: '5000' },
    });

    fireEvent.click(screen.getByRole('button', { name: /calculate/i }));

    await waitFor(() => {
      expect(onCalculate).toHaveBeenCalledWith(
        expect.objectContaining({
          totalWealth: 6000,
          zakatAmount: 150,
        })
      );
    });
  });

  it('displays error for invalid input', async () => {
    render(<ZakatCalculator />);

    fireEvent.change(screen.getByLabelText(/gold/i), {
      target: { value: '-100' },
    });
    fireEvent.click(screen.getByRole('button', { name: /calculate/i }));

    expect(await screen.findByText(/must be positive/i)).toBeInTheDocument();
  });
});
```

### Integration Tests

Test user workflows across multiple components.

```typescript
// contract-workflow.test.tsx
import { describe, it, expect } from 'vitest';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MurabahaWorkflow } from './MurabahaWorkflow';

describe('Murabaha Contract Workflow', () => {
  it('completes full contract creation flow', async () => {
    const queryClient = new QueryClient();

    render(
      <QueryClientProvider client={queryClient}>
        <MurabahaWorkflow />
      </QueryClientProvider>
    );

    // Step 1: Enter contract details
    fireEvent.change(screen.getByLabelText(/principal amount/i), {
      target: { value: '100000' },
    });
    fireEvent.change(screen.getByLabelText(/profit margin/i), {
      target: { value: '5' },
    });
    fireEvent.click(screen.getByRole('button', { name: /next/i }));

    // Step 2: Review calculation
    await waitFor(() => {
      expect(screen.getByText(/total amount: 105,000/i)).toBeInTheDocument();
    });
    fireEvent.click(screen.getByRole('button', { name: /confirm/i }));

    // Step 3: Submit contract
    await waitFor(() => {
      expect(screen.getByText(/contract created successfully/i)).toBeInTheDocument();
    });
  });
});
```

## ♿ Accessibility

### ARIA Labels and Roles

Provide semantic markup and ARIA attributes.

```typescript
// Accessible modal
function Modal({ isOpen, onClose, title, children }: ModalProps) {
  return (
    <div
      role="dialog"
      aria-modal="true"
      aria-labelledby="modal-title"
      hidden={!isOpen}
    >
      <div className="modal-overlay" onClick={onClose} />
      <div className="modal-content">
        <h2 id="modal-title">{title}</h2>
        <button
          onClick={onClose}
          aria-label="Close modal"
          className="modal-close"
        >
          ×
        </button>
        {children}
      </div>
    </div>
  );
}

// Accessible form
function ZakatForm() {
  return (
    <form aria-label="Zakat calculation form">
      <div>
        <label htmlFor="gold-input">Gold (grams)</label>
        <input
          id="gold-input"
          type="number"
          aria-required="true"
          aria-describedby="gold-help"
        />
        <p id="gold-help">Enter the weight of gold in grams</p>
      </div>

      <button type="submit">Calculate Zakat</button>
    </form>
  );
}
```

### Keyboard Navigation

Ensure all interactive elements are keyboard accessible.

```typescript
// Keyboard-accessible dropdown
function Dropdown({ options, value, onChange }: DropdownProps) {
  const [isOpen, setIsOpen] = useState(false);
  const [focusedIndex, setFocusedIndex] = useState(0);

  const handleKeyDown = (e: KeyboardEvent<HTMLDivElement>) => {
    switch (e.key) {
      case 'Enter':
      case ' ':
        setIsOpen(!isOpen);
        break;
      case 'ArrowDown':
        e.preventDefault();
        setFocusedIndex((prev) => Math.min(prev + 1, options.length - 1));
        break;
      case 'ArrowUp':
        e.preventDefault();
        setFocusedIndex((prev) => Math.max(prev - 1, 0));
        break;
      case 'Escape':
        setIsOpen(false);
        break;
    }
  };

  return (
    <div
      role="combobox"
      aria-expanded={isOpen}
      aria-haspopup="listbox"
      aria-controls="dropdown-listbox"
      tabIndex={0}
      onKeyDown={handleKeyDown}
    >
      <button onClick={() => setIsOpen(!isOpen)}>
        {value || 'Select option'}
      </button>

      {isOpen && (
        <ul id="dropdown-listbox" role="listbox">
          {options.map((option, index) => (
            <li
              key={option.value}
              role="option"
              aria-selected={option.value === value}
              className={index === focusedIndex ? 'focused' : ''}
              onClick={() => onChange(option.value)}
            >
              {option.label}
            </li>
          ))}
        </ul>
      )}
    </div>
  );
}
```

### Screen Reader Support

Provide meaningful text for screen readers.

```typescript
// Loading states
function ContractList() {
  const { data, isLoading } = useContracts();

  if (isLoading) {
    return (
      <div role="status" aria-live="polite">
        <LoadingSpinner />
        <span className="sr-only">Loading contracts...</span>
      </div>
    );
  }

  return (
    <div>
      <h2>Contracts ({data.length})</h2>
      <ul aria-label="Contract list">
        {data.map((contract) => (
          <li key={contract.id}>
            <ContractCard contract={contract} />
          </li>
        ))}
      </ul>
    </div>
  );
}

// Error announcements
function ErrorMessage({ message }: { message: string }) {
  return (
    <div role="alert" aria-live="assertive">
      <span className="sr-only">Error:</span>
      {message}
    </div>
  );
}

// CSS for screen-reader only content
/*
.sr-only {
  position: absolute;
  width: 1px;
  height: 1px;
  padding: 0;
  margin: -1px;
  overflow: hidden;
  clip: rect(0, 0, 0, 0);
  white-space: nowrap;
  border-width: 0;
}
*/
```

## 🔒 Security

### XSS Prevention

Always sanitize user input when rendering as HTML.

```typescript
import DOMPurify from 'dompurify';

// ❌ NEVER do this
function UnsafeComment({ comment }: { comment: string }) {
  return <div dangerouslySetInnerHTML={{ __html: comment }} />;
}

// ✅ Sanitize before rendering
function SafeComment({ comment }: { comment: string }) {
  const sanitizedComment = DOMPurify.sanitize(comment, {
    ALLOWED_TAGS: ['b', 'i', 'em', 'strong', 'a'],
    ALLOWED_ATTR: ['href'],
  });

  return <div dangerouslySetInnerHTML={{ __html: sanitizedComment }} />;
}

// ✅ Best: Avoid dangerouslySetInnerHTML
function SafestComment({ comment }: { comment: string }) {
  return <div>{comment}</div>; // React escapes automatically
}
```

### Content Security Policy

Configure CSP headers to prevent XSS attacks.

```typescript
// vite.config.ts
export default defineConfig({
  plugins: [
    react(),
    {
      name: "csp-plugin",
      configureServer(server) {
        server.middlewares.use((req, res, next) => {
          res.setHeader(
            "Content-Security-Policy",
            [
              "default-src 'self'",
              "script-src 'self' 'unsafe-inline'",
              "style-src 'self' 'unsafe-inline'",
              "img-src 'self' data: https:",
              "font-src 'self' data:",
              "connect-src 'self' https://api.example.com",
            ].join("; "),
          );
          next();
        });
      },
    },
  ],
});
```

### Auth Token Handling

Store and transmit tokens securely.

```typescript
// ❌ NEVER store tokens in localStorage
localStorage.setItem("token", token); // Vulnerable to XSS

// ✅ Use httpOnly cookies (set by server)
// Server sets: Set-Cookie: token=xxx; HttpOnly; Secure; SameSite=Strict

// ✅ Or use in-memory storage with refresh token rotation
class TokenManager {
  private accessToken: string | null = null;

  setToken(token: string) {
    this.accessToken = token;
  }

  getToken(): string | null {
    return this.accessToken;
  }

  clearToken() {
    this.accessToken = null;
  }
}

export const tokenManager = new TokenManager();

// API client
async function apiRequest(url: string, options: RequestInit = {}) {
  const token = tokenManager.getToken();

  return fetch(url, {
    ...options,
    headers: {
      ...options.headers,
      ...(token && { Authorization: `Bearer ${token}` }),
    },
  });
}
```

### Secure Data Transmission

Always use HTTPS and validate SSL certificates.

```typescript
// Environment-based API URL
const API_URL = import.meta.env.VITE_API_URL;

// ❌ NEVER hardcode HTTP in production
const BAD_URL = "http://api.example.com"; // Insecure

// ✅ Enforce HTTPS
if (!API_URL.startsWith("https://") && import.meta.env.PROD) {
  throw new Error("API URL must use HTTPS in production");
}

// ✅ Validate origin for sensitive operations
function validateOrigin() {
  const allowedOrigins = ["https://example.com", "https://app.example.com"];

  if (!allowedOrigins.includes(window.location.origin)) {
    throw new Error("Invalid origin");
  }
}
```

## 📏 Code Quality

### ESLint Configuration

Configure ESLint for TypeScript and React.

```javascript
// .eslintrc.cjs
module.exports = {
  extends: [
    "eslint:recommended",
    "plugin:@typescript-eslint/recommended",
    "plugin:@typescript-eslint/recommended-requiring-type-checking",
    "plugin:react/recommended",
    "plugin:react-hooks/recommended",
    "plugin:jsx-a11y/recommended",
  ],
  parser: "@typescript-eslint/parser",
  parserOptions: {
    ecmaVersion: "latest",
    sourceType: "module",
    project: "./tsconfig.json",
  },
  plugins: ["@typescript-eslint", "react", "react-hooks", "jsx-a11y"],
  rules: {
    "@typescript-eslint/no-unused-vars": ["error", { argsIgnorePattern: "^_" }],
    "@typescript-eslint/explicit-function-return-type": "off",
    "@typescript-eslint/explicit-module-boundary-types": "off",
    "@typescript-eslint/no-explicit-any": "error",
    "react/react-in-jsx-scope": "off", // Not needed in React 17+
    "react/prop-types": "off", // Using TypeScript for type checking
    "react-hooks/rules-of-hooks": "error",
    "react-hooks/exhaustive-deps": "warn",
  },
};
```

### Prettier Configuration

Standardize code formatting.

```javascript
// .prettierrc.cjs
module.exports = {
  semi: true,
  trailingComma: "es5",
  singleQuote: true,
  printWidth: 80,
  tabWidth: 2,
  arrowParens: "always",
  endOfLine: "lf",
};
```

### TypeScript Strict Mode

Enable all strict type-checking options.

```json
{
  "compilerOptions": {
    "target": "ES2020",
    "lib": ["ES2020", "DOM", "DOM.Iterable"],
    "module": "ESNext",
    "skipLibCheck": true,

    "moduleResolution": "bundler",
    "allowImportingTsExtensions": true,
    "resolveJsonModule": true,
    "isolatedModules": true,
    "noEmit": true,
    "jsx": "react-jsx",

    "strict": true,
    "noUncheckedIndexedAccess": true,
    "noImplicitReturns": true,
    "noFallthroughCasesInSwitch": true,
    "noUnusedLocals": true,
    "noUnusedParameters": true,
    "exactOptionalPropertyTypes": true,
    "forceConsistentCasingInFileNames": true
  }
}
```

## 🔌 API Integration

### Error Handling

Handle API errors gracefully.

```typescript
// api/client.ts
export class ApiError extends Error {
  constructor(
    message: string,
    public status: number,
    public code?: string,
  ) {
    super(message);
    this.name = "ApiError";
  }
}

export async function apiRequest<T>(url: string, options: RequestInit = {}): Promise<T> {
  try {
    const response = await fetch(url, {
      ...options,
      headers: {
        "Content-Type": "application/json",
        ...options.headers,
      },
    });

    if (!response.ok) {
      const error = await response.json().catch(() => ({}));
      throw new ApiError(error.message || "Request failed", response.status, error.code);
    }

    return response.json();
  } catch (error) {
    if (error instanceof ApiError) {
      throw error;
    }

    throw new ApiError("Network error", 0);
  }
}

// Usage
try {
  const contract = await apiRequest<Contract>("/api/contracts/123");
} catch (error) {
  if (error instanceof ApiError) {
    if (error.status === 404) {
      showNotFound();
    } else if (error.status === 401) {
      redirectToLogin();
    } else {
      showErrorMessage(error.message);
    }
  }
}
```

### Loading States

Provide feedback during async operations.

```typescript
function ContractDetails({ id }: { id: string }) {
  const { data, error, isLoading } = useQuery({
    queryKey: ['contracts', id],
    queryFn: () => fetchContract(id),
  });

  // Loading state
  if (isLoading) {
    return (
      <div className="contract-loading">
        <Skeleton height={200} />
        <Skeleton height={40} count={3} />
      </div>
    );
  }

  // Error state
  if (error) {
    return (
      <ErrorMessage
        title="Failed to load contract"
        message={error.message}
      />
    );
  }

  // Success state
  return <ContractCard contract={data} />;
}
```

### Request Caching

Cache API responses to reduce network requests.

```typescript
// React Query configuration
const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      staleTime: 5 * 60 * 1000, // 5 minutes
      cacheTime: 10 * 60 * 1000, // 10 minutes
      retry: 3,
      retryDelay: (attemptIndex) => Math.min(1000 * 2 ** attemptIndex, 30000),
    },
  },
});

// Prefetch data
function PrefetchContracts() {
  const queryClient = useQueryClient();

  useEffect(() => {
    queryClient.prefetchQuery({
      queryKey: ["contracts"],
      queryFn: fetchContracts,
    });
  }, [queryClient]);

  return null;
}

// Optimistic updates
function useUpdateContract() {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: updateContract,
    onMutate: async (updatedContract) => {
      await queryClient.cancelQueries(["contracts", updatedContract.id]);

      const previousContract = queryClient.getQueryData(["contracts", updatedContract.id]);

      queryClient.setQueryData(["contracts", updatedContract.id], updatedContract);

      return { previousContract };
    },
    onError: (err, updatedContract, context) => {
      queryClient.setQueryData(["contracts", updatedContract.id], context?.previousContract);
    },
    onSettled: (updatedContract) => {
      queryClient.invalidateQueries(["contracts", updatedContract?.id]);
    },
  });
}
```

## 🏗️ Build Configuration

### Vite Configuration

Optimize build for production.

```typescript
// vite.config.ts
import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import { visualizer } from "rollup-plugin-visualizer";

export default defineConfig({
  plugins: [
    react(),
    visualizer({ open: true }), // Bundle size analysis
  ],
  build: {
    target: "es2020",
    outDir: "dist",
    sourcemap: true,
    rollupOptions: {
      output: {
        manualChunks: {
          "vendor-react": ["react", "react-dom", "react-router-dom"],
          "vendor-ui": ["@radix-ui/react-dialog", "@radix-ui/react-dropdown-menu"],
          "vendor-query": ["@tanstack/react-query"],
        },
      },
    },
  },
  server: {
    port: 3000,
    proxy: {
      "/api": {
        target: "http://localhost:8080",
        changeOrigin: true,
      },
    },
  },
});
```

### Environment Variables

Manage environment-specific configuration.

```typescript
// .env.example
VITE_API_URL=https://api.example.com
VITE_APP_NAME=Sharia Enterprise Platform
VITE_ENABLE_ANALYTICS=false

// env.d.ts
/// <reference types="vite/client" />

interface ImportMetaEnv {
  readonly VITE_API_URL: string;
  readonly VITE_APP_NAME: string;
  readonly VITE_ENABLE_ANALYTICS: string;
}

interface ImportMeta {
  readonly env: ImportMetaEnv;
}

// config.ts
export const config = {
  apiUrl: import.meta.env.VITE_API_URL,
  appName: import.meta.env.VITE_APP_NAME,
  enableAnalytics: import.meta.env.VITE_ENABLE_ANALYTICS === 'true',
} as const;

// Validate required env vars at startup
const requiredEnvVars = ['VITE_API_URL'] as const;

for (const envVar of requiredEnvVars) {
  if (!import.meta.env[envVar]) {
    throw new Error(`Missing required environment variable: ${envVar}`);
  }
}
```

## 📚 Documentation

### JSDoc Comments

Document complex functions and components.

````typescript
/**
 * Calculates zakat amount based on zakatable wealth and nisab threshold.
 *
 * @param wealth - Total zakatable wealth in the specified currency
 * @param nisab - Nisab threshold value (defaults to gold-based nisab)
 * @param rate - Zakat rate as decimal (default 0.025 for 2.5%)
 * @returns Calculated zakat amount, or 0 if wealth is below nisab
 *
 * @example
 * ```typescript
 * const zakat = calculateZakat(10000, 5000); // Returns 250
 * const noZakat = calculateZakat(3000, 5000); // Returns 0 (below nisab)
 * ```
 */
export function calculateZakat(wealth: number, nisab: number = GOLD_NISAB, rate: number = 0.025): number {
  if (wealth < nisab) {
    return 0;
  }
  return wealth * rate;
}

/**
 * Props for the MurabahaContract component.
 */
interface MurabahaContractProps {
  /** Unique identifier for the contract */
  contractId: string;

  /** Principal amount in the specified currency */
  principalAmount: number;

  /** Profit margin as a percentage (e.g., 5 for 5%) */
  profitMargin: number;

  /** Optional currency code (defaults to USD) */
  currency?: string;

  /** Callback fired when contract is approved */
  onApprove?: (contract: MurabahaContract) => void;
}
````

### Prop Documentation

Use TypeScript types and JSDoc for component props.

````typescript
/**
 * Displays a Murabaha contract with approval/rejection actions.
 *
 * @example
 * ```tsx
 * <MurabahaContract
 *   contractId="MC-001"
 *   principalAmount={100000}
 *   profitMargin={5}
 *   currency="USD"
 *   onApprove={(contract) => console.log('Approved:', contract)}
 * />
 * ```
 */
export function MurabahaContract({
  contractId,
  principalAmount,
  profitMargin,
  currency = "USD",
  onApprove,
}: MurabahaContractProps) {
  // Implementation
}
````

### Storybook Stories

Create interactive component documentation.

```typescript
// MurabahaContract.stories.tsx
import type { Meta, StoryObj } from "@storybook/react";
import { MurabahaContract } from "./MurabahaContract";

const meta: Meta<typeof MurabahaContract> = {
  title: "Features/Contracts/MurabahaContract",
  component: MurabahaContract,
  tags: ["autodocs"],
  argTypes: {
    principalAmount: {
      control: { type: "number", min: 0 },
      description: "Principal amount in the specified currency",
    },
    profitMargin: {
      control: { type: "number", min: 0, max: 100 },
      description: "Profit margin percentage",
    },
    currency: {
      control: { type: "select" },
      options: ["USD", "EUR", "GBP", "SAR"],
    },
  },
};

export default meta;
type Story = StoryObj<typeof MurabahaContract>;

export const Default: Story = {
  args: {
    contractId: "MC-001",
    principalAmount: 100000,
    profitMargin: 5,
    currency: "USD",
  },
};

export const LargeAmount: Story = {
  args: {
    contractId: "MC-002",
    principalAmount: 1000000,
    profitMargin: 3.5,
    currency: "SAR",
  },
};

export const WithCallbacks: Story = {
  args: {
    contractId: "MC-003",
    principalAmount: 50000,
    profitMargin: 4,
    onApprove: (contract) => console.log("Approved:", contract),
    onReject: (reason) => console.log("Rejected:", reason),
  },
};
```

## 📖 Related Resources

**Official Documentation**:

- [React Documentation](https://react.dev) - Official React docs
- [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html) - TypeScript reference
- [React Query](https://tanstack.com/query/latest) - Data fetching and caching
- [Testing Library](https://testing-library.com/react) - Component testing

**Related Documentation**:

- [React Idioms](./ex-so-plwe-fere__idioms.md) - React + TypeScript patterns
- [React Anti-Patterns](./ex-so-plwe-fere__anti-patterns.md) - Common mistakes
- [React Configuration](./ex-so-plwe-fere__configuration.md) - Build and environment setup

---

This document provides comprehensive best practices for React + TypeScript development. Apply these patterns consistently across all React applications in the enterprise platform to ensure code quality, maintainability, and user experience.
