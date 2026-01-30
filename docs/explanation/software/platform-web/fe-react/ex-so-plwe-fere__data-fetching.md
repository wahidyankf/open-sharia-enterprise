---
title: React Data Fetching
description: Data fetching patterns and API integration for React applications with TypeScript, covering Fetch API, React Query, SWR, error handling, and authentication
category: explanation
tags:
  - react
  - data-fetching
  - api
  - typescript
  - http-client
  - caching
  - authentication
created: 2026-01-29
updated: 2026-01-29
---

# React Data Fetching

This document provides comprehensive guidance on data fetching patterns and API integration for React applications with TypeScript. It covers native Fetch API, HTTP clients, data fetching libraries, error handling, caching strategies, and authentication patterns for enterprise applications.

## 📋 Overview

Data fetching is a fundamental aspect of React applications that interact with backend APIs. Modern React applications require robust patterns for:

- **Fetching data** from REST and GraphQL APIs
- **Managing loading and error states** gracefully
- **Caching responses** to improve performance
- **Handling authentication** with tokens and refresh logic
- **Optimistic updates** for better user experience
- **Polling and real-time updates** for live data

This guide focuses on **production-ready patterns** for enterprise applications, with examples from Islamic financial domains including Zakat calculations, Murabaha contracts, and donation management.

## 🌐 1. Fetch API

The native Fetch API provides a modern interface for making HTTP requests in JavaScript.

### Basic Fetch Request

```typescript
interface ZakatCalculation {
  id: string;
  wealth: number;
  nisab: number;
  zakatAmount: number;
  calculationDate: string;
}

async function fetchZakatCalculation(id: string): Promise<ZakatCalculation> {
  const response = await fetch(`/api/zakat/calculations/${id}`);

  if (!response.ok) {
    throw new Error(`HTTP error: ${response.status}`);
  }

  return response.json();
}
```

### POST Request with JSON Body

```typescript
interface CreateDonationRequest {
  amount: number;
  donorName: string;
  campaignId: string;
  message?: string;
}

interface CreateDonationResponse {
  id: string;
  amount: number;
  createdAt: string;
}

async function createDonation(data: CreateDonationRequest): Promise<CreateDonationResponse> {
  const response = await fetch("/api/donations", {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
    body: JSON.stringify(data),
  });

  if (!response.ok) {
    const error = await response.json().catch(() => ({}));
    throw new Error(error.message || "Failed to create donation");
  }

  return response.json();
}
```

### Type-Safe Fetch Wrapper

```typescript
interface ApiError {
  message: string;
  code?: string;
  details?: unknown;
}

class FetchError extends Error {
  constructor(
    message: string,
    public status: number,
    public code?: string,
    public details?: unknown,
  ) {
    super(message);
    this.name = "FetchError";
  }
}

async function apiFetch<T>(url: string, options: RequestInit = {}): Promise<T> {
  const defaultHeaders = {
    "Content-Type": "application/json",
  };

  const response = await fetch(url, {
    ...options,
    headers: {
      ...defaultHeaders,
      ...options.headers,
    },
  });

  if (!response.ok) {
    const error: ApiError = await response.json().catch(() => ({
      message: "Request failed",
    }));

    throw new FetchError(error.message, response.status, error.code, error.details);
  }

  // Handle 204 No Content
  if (response.status === 204) {
    return undefined as T;
  }

  return response.json();
}

// Usage
const calculation = await apiFetch<ZakatCalculation>("/api/zakat/calculations/123");
```

## 🔧 2. Axios

Axios is a popular HTTP client with built-in features like request/response interceptors, automatic JSON transformation, and better error handling.

### Basic Configuration

```typescript
import axios from "axios";

const apiClient = axios.create({
  baseURL: import.meta.env.VITE_API_URL,
  timeout: 10000,
  headers: {
    "Content-Type": "application/json",
  },
});

// Request interceptor (add auth token)
apiClient.interceptors.request.use(
  (config) => {
    const token = localStorage.getItem("authToken");
    if (token) {
      config.headers.Authorization = `Bearer ${token}`;
    }
    return config;
  },
  (error) => Promise.reject(error),
);

// Response interceptor (handle errors)
apiClient.interceptors.response.use(
  (response) => response,
  (error) => {
    if (error.response?.status === 401) {
      // Handle unauthorized (redirect to login)
      window.location.href = "/login";
    }
    return Promise.reject(error);
  },
);

export default apiClient;
```

### Axios with TypeScript

```typescript
interface MurabahaContract {
  id: string;
  assetCost: number;
  profitRate: number;
  termMonths: number;
  status: "draft" | "pending" | "approved" | "rejected";
  createdAt: string;
}

interface CreateMurabahaRequest {
  assetCost: number;
  profitRate: number;
  termMonths: number;
}

// GET request
async function getMurabahaContract(id: string): Promise<MurabahaContract> {
  const response = await apiClient.get<MurabahaContract>(`/contracts/murabaha/${id}`);
  return response.data;
}

// POST request
async function createMurabahaContract(data: CreateMurabahaRequest): Promise<MurabahaContract> {
  const response = await apiClient.post<MurabahaContract>("/contracts/murabaha", data);
  return response.data;
}

// PUT request
async function updateMurabahaContract(id: string, data: Partial<CreateMurabahaRequest>): Promise<MurabahaContract> {
  const response = await apiClient.put<MurabahaContract>(`/contracts/murabaha/${id}`, data);
  return response.data;
}

// DELETE request
async function deleteMurabahaContract(id: string): Promise<void> {
  await apiClient.delete(`/contracts/murabaha/${id}`);
}
```

## ⚛️ 3. React Query

React Query (TanStack Query) is a powerful data fetching library that handles caching, synchronization, and background updates automatically.

### Installation

```bash
npm install @tanstack/react-query
```

### Setup

```typescript
// App.tsx
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { ReactQueryDevtools } from '@tanstack/react-query-devtools';

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

function App() {
  return (
    <QueryClientProvider client={queryClient}>
      <YourApp />
      <ReactQueryDevtools initialIsOpen={false} />
    </QueryClientProvider>
  );
}
```

### Queries

```typescript
import { useQuery } from '@tanstack/react-query';

// Query function
async function fetchDonationCampaigns() {
  const response = await apiClient.get<DonationCampaign[]>('/campaigns');
  return response.data;
}

// Hook
function useDonationCampaigns() {
  return useQuery({
    queryKey: ['campaigns'],
    queryFn: fetchDonationCampaigns,
  });
}

// Component
function DonationCampaignList() {
  const { data, isLoading, error } = useDonationCampaigns();

  if (isLoading) {
    return <div>Loading campaigns...</div>;
  }

  if (error) {
    return <div>Error: {error.message}</div>;
  }

  return (
    <ul>
      {data?.map((campaign) => (
        <li key={campaign.id}>
          <h3>{campaign.name}</h3>
          <p>Goal: ${campaign.goal}</p>
          <p>Raised: ${campaign.raised}</p>
        </li>
      ))}
    </ul>
  );
}
```

### Parameterized Queries

```typescript
function useZakatCalculation(id: string) {
  return useQuery({
    queryKey: ['zakat-calculations', id],
    queryFn: () => fetchZakatCalculation(id),
    enabled: Boolean(id), // Only fetch when id exists
  });
}

// Usage
function ZakatCalculationDetails({ id }: { id: string }) {
  const { data, isLoading, error } = useZakatCalculation(id);

  if (isLoading) return <div>Loading...</div>;
  if (error) return <div>Error: {error.message}</div>;

  return (
    <div>
      <h2>Calculation Details</h2>
      <p>Wealth: ${data?.wealth}</p>
      <p>Zakat: ${data?.zakatAmount}</p>
    </div>
  );
}
```

### Mutations

```typescript
import { useMutation, useQueryClient } from '@tanstack/react-query';

function useCreateDonation() {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: createDonation,
    onSuccess: () => {
      // Invalidate campaigns query to refetch
      queryClient.invalidateQueries({ queryKey: ['campaigns'] });
    },
    onError: (error) => {
      console.error('Failed to create donation:', error);
    },
  });
}

// Component
function DonationForm() {
  const createDonationMutation = useCreateDonation();

  const handleSubmit = async (data: CreateDonationRequest) => {
    try {
      await createDonationMutation.mutateAsync(data);
      alert('Donation created successfully!');
    } catch (error) {
      alert('Failed to create donation');
    }
  };

  return (
    <form onSubmit={(e) => {
      e.preventDefault();
      const formData = new FormData(e.currentTarget);
      handleSubmit({
        amount: Number(formData.get('amount')),
        donorName: String(formData.get('name')),
        campaignId: String(formData.get('campaignId')),
      });
    }}>
      <input name="amount" type="number" required />
      <input name="name" type="text" required />
      <input name="campaignId" type="text" required />
      <button type="submit" disabled={createDonationMutation.isLoading}>
        {createDonationMutation.isLoading ? 'Submitting...' : 'Donate'}
      </button>
    </form>
  );
}
```

## 🔄 4. SWR

SWR (stale-while-revalidate) is a React hooks library for data fetching developed by Vercel.

### Installation

```bash
npm install swr
```

### Basic Usage

```typescript
import useSWR from 'swr';

// Fetcher function
const fetcher = (url: string) => apiClient.get(url).then((res) => res.data);

// Hook
function useMurabahaContracts() {
  const { data, error, isLoading } = useSWR<MurabahaContract[]>(
    '/contracts/murabaha',
    fetcher
  );

  return {
    contracts: data,
    isLoading,
    error,
  };
}

// Component
function MurabahaContractList() {
  const { contracts, isLoading, error } = useMurabahaContracts();

  if (isLoading) return <div>Loading contracts...</div>;
  if (error) return <div>Error loading contracts</div>;

  return (
    <ul>
      {contracts?.map((contract) => (
        <li key={contract.id}>
          <h3>Contract {contract.id}</h3>
          <p>Asset Cost: ${contract.assetCost}</p>
          <p>Status: {contract.status}</p>
        </li>
      ))}
    </ul>
  );
}
```

### Configuration Options

```typescript
import useSWR from "swr";

function useZakatCalculations() {
  return useSWR<ZakatCalculation[]>("/api/zakat/calculations", fetcher, {
    refreshInterval: 30000, // Refresh every 30 seconds
    revalidateOnFocus: true, // Revalidate when window gains focus
    revalidateOnReconnect: true, // Revalidate when network reconnects
    dedupingInterval: 2000, // Dedupe requests within 2 seconds
    shouldRetryOnError: true,
    errorRetryCount: 3,
  });
}
```

### Mutation with SWR

```typescript
import useSWR, { mutate } from 'swr';

async function updateContract(
  id: string,
  data: Partial<MurabahaContract>
): Promise<MurabahaContract> {
  const response = await apiClient.put(`/contracts/murabaha/${id}`, data);
  return response.data;
}

function MurabahaContractEditor({ id }: { id: string }) {
  const { data: contract, error } = useSWR<MurabahaContract>(
    `/contracts/murabaha/${id}`,
    fetcher
  );

  const handleUpdate = async (updates: Partial<MurabahaContract>) => {
    // Optimistic update
    mutate(
      `/contracts/murabaha/${id}`,
      { ...contract, ...updates },
      false // Don't revalidate immediately
    );

    try {
      await updateContract(id, updates);
      // Revalidate to get server state
      mutate(`/contracts/murabaha/${id}`);
    } catch (error) {
      // Revert on error
      mutate(`/contracts/murabaha/${id}`);
      alert('Update failed');
    }
  };

  if (!contract) return <div>Loading...</div>;
  if (error) return <div>Error loading contract</div>;

  return (
    <div>
      <h2>Edit Contract {contract.id}</h2>
      <button onClick={() => handleUpdate({ status: 'approved' })}>
        Approve
      </button>
    </div>
  );
}
```

## 🚨 5. Error Handling

### Typed Error Handling

```typescript
interface ApiErrorResponse {
  message: string;
  code: string;
  errors?: Record<string, string[]>;
}

class ApiError extends Error {
  constructor(
    message: string,
    public status: number,
    public code?: string,
    public errors?: Record<string, string[]>,
  ) {
    super(message);
    this.name = "ApiError";
  }
}

async function handleApiError(response: Response): Promise<never> {
  let errorData: ApiErrorResponse;

  try {
    errorData = await response.json();
  } catch {
    throw new ApiError("Request failed", response.status);
  }

  throw new ApiError(errorData.message, response.status, errorData.code, errorData.errors);
}

// Usage
async function fetchWithErrorHandling<T>(url: string): Promise<T> {
  const response = await fetch(url);

  if (!response.ok) {
    await handleApiError(response);
  }

  return response.json();
}
```

### Network Error Handling

```typescript
async function fetchWithNetworkRetry<T>(url: string, maxRetries = 3): Promise<T> {
  let lastError: Error;

  for (let attempt = 0; attempt <= maxRetries; attempt++) {
    try {
      const response = await fetch(url);

      if (!response.ok) {
        throw new ApiError("Request failed", response.status);
      }

      return response.json();
    } catch (error) {
      lastError = error as Error;

      // Don't retry on client errors (4xx)
      if (error instanceof ApiError && error.status >= 400 && error.status < 500) {
        throw error;
      }

      // Wait before retry (exponential backoff)
      if (attempt < maxRetries) {
        const delay = Math.min(1000 * 2 ** attempt, 10000);
        await new Promise((resolve) => setTimeout(resolve, delay));
      }
    }
  }

  throw lastError!;
}
```

### Error Display Component

```typescript
interface ErrorMessageProps {
  error: Error | null;
  onRetry?: () => void;
}

function ErrorMessage({ error, onRetry }: ErrorMessageProps) {
  if (!error) return null;

  const isApiError = error instanceof ApiError;

  return (
    <div role="alert" className="error-message">
      <h3>Something went wrong</h3>
      <p>{error.message}</p>

      {isApiError && error.errors && (
        <ul>
          {Object.entries(error.errors).map(([field, messages]) => (
            <li key={field}>
              <strong>{field}:</strong> {messages.join(', ')}
            </li>
          ))}
        </ul>
      )}

      {onRetry && (
        <button onClick={onRetry}>Try Again</button>
      )}
    </div>
  );
}
```

## ⏳ 6. Loading States

### Skeleton Loading

```typescript
function Skeleton({ height = 20, width = '100%' }: { height?: number; width?: string | number }) {
  return (
    <div
      style={{
        height: `${height}px`,
        width,
        backgroundColor: '#e0e0e0',
        borderRadius: '4px',
        animation: 'pulse 1.5s ease-in-out infinite',
      }}
    />
  );
}

function ContractListSkeleton() {
  return (
    <div>
      {Array.from({ length: 5 }).map((_, index) => (
        <div key={index} style={{ marginBottom: '16px' }}>
          <Skeleton height={24} width="60%" />
          <Skeleton height={16} width="40%" />
          <Skeleton height={16} width="80%" />
        </div>
      ))}
    </div>
  );
}

// Usage
function MurabahaContractList() {
  const { data, isLoading } = useMurabahaContracts();

  if (isLoading) {
    return <ContractListSkeleton />;
  }

  return (
    <ul>
      {data?.map((contract) => (
        <li key={contract.id}>{contract.id}</li>
      ))}
    </ul>
  );
}
```

### Suspense Integration

```typescript
import { Suspense } from 'react';

function DonationDashboard() {
  return (
    <Suspense fallback={<ContractListSkeleton />}>
      <MurabahaContractList />
    </Suspense>
  );
}
```

## ⚡ 7. Optimistic Updates

### React Query Optimistic Update

```typescript
function useUpdateContract() {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: ({ id, data }: { id: string; data: Partial<MurabahaContract> }) => updateContract(id, data),

    // Optimistic update
    onMutate: async ({ id, data }) => {
      // Cancel outgoing refetches
      await queryClient.cancelQueries({ queryKey: ["contracts", id] });

      // Snapshot previous value
      const previousContract = queryClient.getQueryData<MurabahaContract>(["contracts", id]);

      // Optimistically update
      queryClient.setQueryData<MurabahaContract>(["contracts", id], (old) => (old ? { ...old, ...data } : old));

      return { previousContract };
    },

    // Rollback on error
    onError: (err, { id }, context) => {
      if (context?.previousContract) {
        queryClient.setQueryData(["contracts", id], context.previousContract);
      }
    },

    // Always refetch after error or success
    onSettled: (data, error, { id }) => {
      queryClient.invalidateQueries({ queryKey: ["contracts", id] });
    },
  });
}
```

## 💾 8. Data Caching

### Cache Invalidation Strategies

```typescript
import { useQueryClient } from "@tanstack/react-query";

function CacheManager() {
  const queryClient = useQueryClient();

  const invalidateAll = () => {
    queryClient.invalidateQueries();
  };

  const invalidateContracts = () => {
    queryClient.invalidateQueries({ queryKey: ["contracts"] });
  };

  const invalidateSpecificContract = (id: string) => {
    queryClient.invalidateQueries({ queryKey: ["contracts", id] });
  };

  const clearCache = () => {
    queryClient.clear();
  };

  return {
    invalidateAll,
    invalidateContracts,
    invalidateSpecificContract,
    clearCache,
  };
}
```

### Prefetching

```typescript
function usePrefetchContract(id: string) {
  const queryClient = useQueryClient();

  return () => {
    queryClient.prefetchQuery({
      queryKey: ['contracts', id],
      queryFn: () => getMurabahaContract(id),
    });
  };
}

// Usage
function ContractListItem({ contract }: { contract: MurabahaContract }) {
  const prefetch = usePrefetchContract(contract.id);

  return (
    <li onMouseEnter={prefetch}>
      <Link to={`/contracts/${contract.id}`}>
        {contract.id}
      </Link>
    </li>
  );
}
```

## 📄 9. Pagination

### Cursor-Based Pagination

```typescript
interface PaginatedResponse<T> {
  data: T[];
  nextCursor: string | null;
  hasMore: boolean;
}

function usePaginatedContracts() {
  return useInfiniteQuery({
    queryKey: ['contracts', 'paginated'],
    queryFn: async ({ pageParam = null }) => {
      const params = new URLSearchParams();
      if (pageParam) {
        params.set('cursor', pageParam);
      }

      const response = await apiClient.get<PaginatedResponse<MurabahaContract>>(
        `/contracts/murabaha?${params}`
      );
      return response.data;
    },
    getNextPageParam: (lastPage) => lastPage.nextCursor,
  });
}

// Component
function InfiniteContractList() {
  const {
    data,
    fetchNextPage,
    hasNextPage,
    isFetchingNextPage,
    isLoading,
  } = usePaginatedContracts();

  if (isLoading) return <div>Loading...</div>;

  return (
    <div>
      {data?.pages.map((page, pageIndex) => (
        <div key={pageIndex}>
          {page.data.map((contract) => (
            <div key={contract.id}>
              <h3>Contract {contract.id}</h3>
              <p>Status: {contract.status}</p>
            </div>
          ))}
        </div>
      ))}

      {hasNextPage && (
        <button onClick={() => fetchNextPage()} disabled={isFetchingNextPage}>
          {isFetchingNextPage ? 'Loading more...' : 'Load More'}
        </button>
      )}
    </div>
  );
}
```

## 🔄 10. Polling

### Automatic Polling

```typescript
function useLiveDonations(campaignId: string) {
  return useQuery({
    queryKey: ["donations", "live", campaignId],
    queryFn: () => fetchDonations(campaignId),
    refetchInterval: 5000, // Poll every 5 seconds
    refetchIntervalInBackground: false, // Stop polling when tab is not active
  });
}
```

### Manual Polling Control

```typescript
function useLiveDonationsWithControl(campaignId: string) {
  const [isPolling, setIsPolling] = useState(true);

  const query = useQuery({
    queryKey: ["donations", "live", campaignId],
    queryFn: () => fetchDonations(campaignId),
    refetchInterval: isPolling ? 5000 : false,
  });

  return {
    ...query,
    startPolling: () => setIsPolling(true),
    stopPolling: () => setIsPolling(false),
  };
}
```

## 🔐 11. Authentication

### Token Management

```typescript
class TokenManager {
  private static instance: TokenManager;
  private accessToken: string | null = null;
  private refreshToken: string | null = null;

  static getInstance(): TokenManager {
    if (!TokenManager.instance) {
      TokenManager.instance = new TokenManager();
    }
    return TokenManager.instance;
  }

  setTokens(access: string, refresh: string) {
    this.accessToken = access;
    this.refreshToken = refresh;
  }

  getAccessToken(): string | null {
    return this.accessToken;
  }

  getRefreshToken(): string | null {
    return this.refreshToken;
  }

  clearTokens() {
    this.accessToken = null;
    this.refreshToken = null;
  }
}

export const tokenManager = TokenManager.getInstance();
```

### Refresh Token Flow

```typescript
let isRefreshing = false;
let refreshSubscribers: Array<(token: string) => void> = [];

function subscribeTokenRefresh(callback: (token: string) => void) {
  refreshSubscribers.push(callback);
}

function onTokenRefreshed(token: string) {
  refreshSubscribers.forEach((callback) => callback(token));
  refreshSubscribers = [];
}

async function refreshAccessToken(): Promise<string> {
  const refreshToken = tokenManager.getRefreshToken();

  if (!refreshToken) {
    throw new Error("No refresh token available");
  }

  const response = await fetch("/api/auth/refresh", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ refreshToken }),
  });

  if (!response.ok) {
    throw new Error("Token refresh failed");
  }

  const { accessToken, refreshToken: newRefreshToken } = await response.json();
  tokenManager.setTokens(accessToken, newRefreshToken);

  return accessToken;
}

// Axios interceptor with refresh logic
apiClient.interceptors.response.use(
  (response) => response,
  async (error) => {
    const originalRequest = error.config;

    if (error.response?.status === 401 && !originalRequest._retry) {
      if (isRefreshing) {
        // Wait for token refresh
        return new Promise((resolve) => {
          subscribeTokenRefresh((token) => {
            originalRequest.headers.Authorization = `Bearer ${token}`;
            resolve(apiClient(originalRequest));
          });
        });
      }

      originalRequest._retry = true;
      isRefreshing = true;

      try {
        const newToken = await refreshAccessToken();
        isRefreshing = false;
        onTokenRefreshed(newToken);
        originalRequest.headers.Authorization = `Bearer ${newToken}`;
        return apiClient(originalRequest);
      } catch (refreshError) {
        isRefreshing = false;
        tokenManager.clearTokens();
        window.location.href = "/login";
        return Promise.reject(refreshError);
      }
    }

    return Promise.reject(error);
  },
);
```

## 🔒 12. TypeScript Integration

### Generic API Client

```typescript
interface ApiClient {
  get<T>(url: string, config?: RequestConfig): Promise<T>;
  post<T>(url: string, data?: unknown, config?: RequestConfig): Promise<T>;
  put<T>(url: string, data?: unknown, config?: RequestConfig): Promise<T>;
  delete<T>(url: string, config?: RequestConfig): Promise<T>;
}

interface RequestConfig {
  headers?: Record<string, string>;
  params?: Record<string, string | number>;
}

class TypedApiClient implements ApiClient {
  constructor(private baseURL: string) {}

  async get<T>(url: string, config?: RequestConfig): Promise<T> {
    const response = await fetch(`${this.baseURL}${url}`, {
      method: "GET",
      headers: config?.headers,
    });

    if (!response.ok) {
      throw new ApiError("GET request failed", response.status);
    }

    return response.json();
  }

  async post<T>(url: string, data?: unknown, config?: RequestConfig): Promise<T> {
    const response = await fetch(`${this.baseURL}${url}`, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        ...config?.headers,
      },
      body: JSON.stringify(data),
    });

    if (!response.ok) {
      throw new ApiError("POST request failed", response.status);
    }

    return response.json();
  }

  async put<T>(url: string, data?: unknown, config?: RequestConfig): Promise<T> {
    const response = await fetch(`${this.baseURL}${url}`, {
      method: "PUT",
      headers: {
        "Content-Type": "application/json",
        ...config?.headers,
      },
      body: JSON.stringify(data),
    });

    if (!response.ok) {
      throw new ApiError("PUT request failed", response.status);
    }

    return response.json();
  }

  async delete<T>(url: string, config?: RequestConfig): Promise<T> {
    const response = await fetch(`${this.baseURL}${url}`, {
      method: "DELETE",
      headers: config?.headers,
    });

    if (!response.ok) {
      throw new ApiError("DELETE request failed", response.status);
    }

    return response.json();
  }
}

// Usage
const api = new TypedApiClient(import.meta.env.VITE_API_URL);

const calculation = await api.get<ZakatCalculation>("/zakat/calculations/123");
const newDonation = await api.post<CreateDonationResponse>("/donations", {
  amount: 100,
  donorName: "John Doe",
});
```

### Type-Safe Query Keys

```typescript
type QueryKey =
  | ["contracts"]
  | ["contracts", string]
  | ["campaigns"]
  | ["campaigns", string]
  | ["zakat-calculations"]
  | ["zakat-calculations", string];

function useTypedQuery<T>(key: QueryKey, fetcher: () => Promise<T>) {
  return useQuery({
    queryKey: key,
    queryFn: fetcher,
  });
}

// Usage with type inference
const { data } = useTypedQuery(["contracts", "123"], () => getMurabahaContract("123"));
```

## 📖 Related Resources

**Official Documentation**:

- [Fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API) - MDN Fetch API documentation
- [Axios](https://axios-http.com/) - Promise-based HTTP client
- [React Query](https://tanstack.com/query/latest) - Powerful data synchronization
- [SWR](https://swr.vercel.app/) - React hooks for data fetching

**Repository Conventions**:

- [React Best Practices](./ex-so-plwe-fere__best-practices.md) - Production-ready React patterns
- [TypeScript Best Practices](../../prog-lang/typescript/ex-so-prla-ty__best-practices.md) - TypeScript standards
- [Error Handling Patterns](../../prog-lang/typescript/ex-so-prla-ty__error-handling.md) - Error handling in TypeScript

**Related Documentation**:

- [React State Management](./ex-so-plwe-fere__state-management.md) - State patterns
- [React REST APIs](./ex-so-plwe-fere__rest-apis.md) - Consuming REST APIs
- [React Security](./ex-so-plwe-fere__security.md) - Authentication and authorization

---

This document provides comprehensive data fetching patterns for React applications. Choose the appropriate pattern based on your application's needs: native Fetch for simplicity, Axios for rich features, React Query for sophisticated caching, or SWR for simplicity with automatic revalidation.
