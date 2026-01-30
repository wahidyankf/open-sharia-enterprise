---
title: Consuming REST APIs in React
description: Client-side API integration patterns for React applications including Axios configuration, type-safe clients, CRUD operations, error handling, authentication, and OpenAPI integration
category: explanation
tags:
  - react
  - rest-api
  - http-client
  - axios
  - authentication
  - typescript
  - api-integration
created: 2026-01-29
updated: 2026-01-29
---

# Consuming REST APIs in React

This document provides comprehensive guidance on consuming REST APIs from React applications. It covers client setup, type-safe API clients, CRUD operations, error handling, authentication patterns, request cancellation, file uploads, query parameters, API versioning, OpenAPI integration, and mocking strategies for enterprise-grade React applications.

## 📋 Overview

Modern React applications depend heavily on REST APIs for backend communication. Effective API integration requires:

- **Type-safe API clients** - TypeScript interfaces for requests and responses
- **Error handling** - Network errors, HTTP errors, and user-friendly error messages
- **Authentication** - Token management, refresh logic, and secure header handling
- **Request optimization** - Cancellation, caching, retry logic, and timeouts
- **Developer experience** - Mock APIs for testing, OpenAPI type generation, and debugging tools

This guide focuses on **production-ready patterns** using Axios as the primary HTTP client, with examples from Islamic finance domains including Zakat calculations, Murabaha contracts, and donation management.

## 🔧 1. API Client Setup

### Installing Axios

```bash
npm install axios
```

### Basic Configuration

Create a centralized API client with consistent configuration.

```typescript
// src/lib/api/client.ts
import axios, { AxiosError, AxiosInstance, AxiosRequestConfig, AxiosResponse } from "axios";

// Base configuration
const apiClient: AxiosInstance = axios.create({
  baseURL: import.meta.env.VITE_API_URL,
  timeout: 10000, // 10 seconds
  headers: {
    "Content-Type": "application/json",
  },
});

export default apiClient;
```

### Environment Variables

Store API URLs in environment-specific configuration.

```bash
# .env.development
VITE_API_URL=http://localhost:3000/api

# .env.production
VITE_API_URL=https://api.example.com/v1
```

### Advanced Configuration

Add request/response interceptors for cross-cutting concerns.

```typescript
// src/lib/api/client.ts
import axios, { AxiosInstance, InternalAxiosRequestConfig, AxiosResponse, AxiosError } from "axios";

const apiClient: AxiosInstance = axios.create({
  baseURL: import.meta.env.VITE_API_URL,
  timeout: 10000,
  headers: {
    "Content-Type": "application/json",
  },
});

// Request interceptor - Add auth token
apiClient.interceptors.request.use(
  (config: InternalAxiosRequestConfig) => {
    const token = localStorage.getItem("authToken");
    if (token && config.headers) {
      config.headers.Authorization = `Bearer ${token}`;
    }
    return config;
  },
  (error: AxiosError) => {
    return Promise.reject(error);
  },
);

// Response interceptor - Handle errors globally
apiClient.interceptors.response.use(
  (response: AxiosResponse) => response,
  (error: AxiosError) => {
    if (error.response?.status === 401) {
      // Unauthorized - Redirect to login
      window.location.href = "/login";
    }

    if (error.response?.status === 503) {
      // Service unavailable - Show maintenance message
      console.error("API service unavailable. Please try again later.");
    }

    return Promise.reject(error);
  },
);

export default apiClient;
```

## 🔒 2. Type-Safe API Clients

Define TypeScript types for all API requests and responses to ensure type safety.

### Domain Types

```typescript
// src/types/zakat.ts
export interface ZakatCalculation {
  id: string;
  userId: string;
  wealth: number;
  nisab: number;
  zakatAmount: number;
  calculationDate: string;
  status: "draft" | "submitted" | "paid";
}

export interface CreateZakatCalculationRequest {
  wealth: number;
  nisab: number;
}

export interface UpdateZakatCalculationRequest {
  status: "draft" | "submitted" | "paid";
}

// src/types/murabaha.ts
export interface MurabahaContract {
  id: string;
  customerId: string;
  assetDescription: string;
  assetCost: number;
  profitRate: number;
  termMonths: number;
  monthlyPayment: number;
  status: "draft" | "pending" | "approved" | "active" | "completed" | "rejected";
  createdAt: string;
  updatedAt: string;
}

export interface CreateMurabahaContractRequest {
  customerId: string;
  assetDescription: string;
  assetCost: number;
  profitRate: number;
  termMonths: number;
}

// src/types/donation.ts
export interface Donation {
  id: string;
  donorId: string;
  campaignId: string;
  amount: number;
  currency: "USD" | "EUR" | "GBP";
  message?: string;
  isAnonymous: boolean;
  status: "pending" | "completed" | "failed";
  createdAt: string;
}

export interface CreateDonationRequest {
  campaignId: string;
  amount: number;
  currency: "USD" | "EUR" | "GBP";
  message?: string;
  isAnonymous?: boolean;
}
```

### API Service Layer

Organize API calls into service modules by domain.

```typescript
// src/services/zakatService.ts
import apiClient from "@/lib/api/client";
import type { ZakatCalculation, CreateZakatCalculationRequest, UpdateZakatCalculationRequest } from "@/types/zakat";

export const zakatService = {
  // GET all calculations
  getAll: async (): Promise<ZakatCalculation[]> => {
    const response = await apiClient.get<ZakatCalculation[]>("/zakat/calculations");
    return response.data;
  },

  // GET single calculation by ID
  getById: async (id: string): Promise<ZakatCalculation> => {
    const response = await apiClient.get<ZakatCalculation>(`/zakat/calculations/${id}`);
    return response.data;
  },

  // POST create new calculation
  create: async (data: CreateZakatCalculationRequest): Promise<ZakatCalculation> => {
    const response = await apiClient.post<ZakatCalculation>("/zakat/calculations", data);
    return response.data;
  },

  // PUT update calculation
  update: async (id: string, data: UpdateZakatCalculationRequest): Promise<ZakatCalculation> => {
    const response = await apiClient.put<ZakatCalculation>(`/zakat/calculations/${id}`, data);
    return response.data;
  },

  // DELETE calculation
  delete: async (id: string): Promise<void> => {
    await apiClient.delete(`/zakat/calculations/${id}`);
  },
};
```

## ✅ 3. CRUD Operations

Implement create, read, update, and delete operations with type safety.

### GET Requests

Retrieve resources from the server.

```typescript
// src/services/murabahaService.ts
import apiClient from "@/lib/api/client";
import type { MurabahaContract } from "@/types/murabaha";

export const murabahaService = {
  // Get all contracts
  getAllContracts: async (): Promise<MurabahaContract[]> => {
    const response = await apiClient.get<MurabahaContract[]>("/contracts/murabaha");
    return response.data;
  },

  // Get single contract by ID
  getContractById: async (id: string): Promise<MurabahaContract> => {
    const response = await apiClient.get<MurabahaContract>(`/contracts/murabaha/${id}`);
    return response.data;
  },

  // Get contracts by customer ID
  getContractsByCustomer: async (customerId: string): Promise<MurabahaContract[]> => {
    const response = await apiClient.get<MurabahaContract[]>("/contracts/murabaha", {
      params: { customerId },
    });
    return response.data;
  },
};
```

### POST Requests

Create new resources.

```typescript
// src/services/donationService.ts
import apiClient from "@/lib/api/client";
import type { Donation, CreateDonationRequest } from "@/types/donation";

export const donationService = {
  create: async (data: CreateDonationRequest): Promise<Donation> => {
    const response = await apiClient.post<Donation>("/donations", data);
    return response.data;
  },
};

// Usage in component
import { donationService } from "@/services/donationService";

async function handleDonate() {
  try {
    const donation = await donationService.create({
      campaignId: "campaign-123",
      amount: 100,
      currency: "USD",
      message: "May Allah bless this initiative",
      isAnonymous: false,
    });

    console.log("Donation successful:", donation);
  } catch (error) {
    console.error("Donation failed:", error);
  }
}
```

### PUT Requests

Update existing resources (full replacement).

```typescript
// src/services/murabahaService.ts
import apiClient from "@/lib/api/client";
import type { MurabahaContract, CreateMurabahaContractRequest } from "@/types/murabaha";

export const murabahaService = {
  updateContract: async (id: string, data: CreateMurabahaContractRequest): Promise<MurabahaContract> => {
    const response = await apiClient.put<MurabahaContract>(`/contracts/murabaha/${id}`, data);
    return response.data;
  },
};
```

### PATCH Requests

Partially update resources.

```typescript
// src/services/murabahaService.ts
import apiClient from "@/lib/api/client";
import type { MurabahaContract } from "@/types/murabaha";

export const murabahaService = {
  patchContract: async (id: string, updates: Partial<MurabahaContract>): Promise<MurabahaContract> => {
    const response = await apiClient.patch<MurabahaContract>(`/contracts/murabaha/${id}`, updates);
    return response.data;
  },
};

// Usage
await murabahaService.patchContract("contract-123", {
  status: "approved",
});
```

### DELETE Requests

Remove resources.

```typescript
// src/services/zakatService.ts
import apiClient from "@/lib/api/client";

export const zakatService = {
  delete: async (id: string): Promise<void> => {
    await apiClient.delete(`/zakat/calculations/${id}`);
  },
};

// Usage with confirmation
async function handleDelete(id: string) {
  if (confirm("Are you sure you want to delete this calculation?")) {
    try {
      await zakatService.delete(id);
      console.log("Calculation deleted successfully");
    } catch (error) {
      console.error("Failed to delete calculation:", error);
    }
  }
}
```

## 🚨 4. Error Handling

Handle network errors, HTTP errors, and display user-friendly error messages.

### Custom Error Types

```typescript
// src/lib/api/errors.ts
export interface ApiErrorResponse {
  message: string;
  code?: string;
  errors?: Record<string, string[]>; // Validation errors
  statusCode?: number;
}

export class ApiError extends Error {
  constructor(
    message: string,
    public statusCode: number,
    public code?: string,
    public errors?: Record<string, string[]>,
  ) {
    super(message);
    this.name = "ApiError";
  }

  static fromAxiosError(error: AxiosError<ApiErrorResponse>): ApiError {
    if (error.response) {
      // Server responded with error status
      const data = error.response.data;
      return new ApiError(data.message || "Request failed", error.response.status, data.code, data.errors);
    } else if (error.request) {
      // Request made but no response received
      return new ApiError("Network error. Please check your connection.", 0, "NETWORK_ERROR");
    } else {
      // Something else happened
      return new ApiError(error.message || "Unknown error occurred", 0, "UNKNOWN_ERROR");
    }
  }
}
```

### Error Handling Interceptor

```typescript
// src/lib/api/client.ts
import axios, { AxiosError, AxiosResponse } from "axios";
import { ApiError, ApiErrorResponse } from "./errors";

const apiClient = axios.create({
  baseURL: import.meta.env.VITE_API_URL,
  timeout: 10000,
});

apiClient.interceptors.response.use(
  (response: AxiosResponse) => response,
  (error: AxiosError<ApiErrorResponse>) => {
    const apiError = ApiError.fromAxiosError(error);
    return Promise.reject(apiError);
  },
);
```

### Error Display Component

```typescript
// src/components/ErrorMessage.tsx
import { ApiError } from "@/lib/api/errors";

interface ErrorMessageProps {
  error: Error | ApiError | null;
  onRetry?: () => void;
}

export function ErrorMessage({ error, onRetry }: ErrorMessageProps) {
  if (!error) return null;

  const isApiError = error instanceof ApiError;

  return (
    <div role="alert" className="error-container">
      <h3 className="error-title">Something went wrong</h3>
      <p className="error-message">{error.message}</p>

      {isApiError && error.errors && (
        <div className="validation-errors">
          <h4>Validation Errors:</h4>
          <ul>
            {Object.entries(error.errors).map(([field, messages]) => (
              <li key={field}>
                <strong>{field}:</strong> {messages.join(", ")}
              </li>
            ))}
          </ul>
        </div>
      )}

      {onRetry && (
        <button onClick={onRetry} className="retry-button">
          Try Again
        </button>
      )}
    </div>
  );
}
```

### Usage in Components

```typescript
import { useState } from "react";
import { zakatService } from "@/services/zakatService";
import { ErrorMessage } from "@/components/ErrorMessage";
import type { ApiError } from "@/lib/api/errors";

export function ZakatCalculationList() {
  const [error, setError] = useState<ApiError | null>(null);
  const [calculations, setCalculations] = useState([]);

  const fetchCalculations = async () => {
    try {
      setError(null);
      const data = await zakatService.getAll();
      setCalculations(data);
    } catch (err) {
      setError(err as ApiError);
    }
  };

  return (
    <div>
      <ErrorMessage error={error} onRetry={fetchCalculations} />
      {/* Render calculations */}
    </div>
  );
}
```

## 🔐 5. Authentication

Handle authentication tokens, refresh logic, and secure API requests.

### Token Manager

```typescript
// src/lib/auth/tokenManager.ts
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

  setTokens(access: string, refresh: string): void {
    this.accessToken = access;
    this.refreshToken = refresh;
    localStorage.setItem("accessToken", access);
    localStorage.setItem("refreshToken", refresh);
  }

  getAccessToken(): string | null {
    if (!this.accessToken) {
      this.accessToken = localStorage.getItem("accessToken");
    }
    return this.accessToken;
  }

  getRefreshToken(): string | null {
    if (!this.refreshToken) {
      this.refreshToken = localStorage.getItem("refreshToken");
    }
    return this.refreshToken;
  }

  clearTokens(): void {
    this.accessToken = null;
    this.refreshToken = null;
    localStorage.removeItem("accessToken");
    localStorage.removeItem("refreshToken");
  }
}

export const tokenManager = TokenManager.getInstance();
```

### Refresh Token Flow

```typescript
// src/lib/api/auth.ts
import axios, { AxiosError, InternalAxiosRequestConfig } from "axios";
import { tokenManager } from "@/lib/auth/tokenManager";

let isRefreshing = false;
let refreshSubscribers: Array<(token: string) => void> = [];

function subscribeTokenRefresh(callback: (token: string) => void): void {
  refreshSubscribers.push(callback);
}

function onTokenRefreshed(token: string): void {
  refreshSubscribers.forEach((callback) => callback(token));
  refreshSubscribers = [];
}

async function refreshAccessToken(): Promise<string> {
  const refreshToken = tokenManager.getRefreshToken();

  if (!refreshToken) {
    throw new Error("No refresh token available");
  }

  const response = await axios.post("/api/auth/refresh", {
    refreshToken,
  });

  const { accessToken, refreshToken: newRefreshToken } = response.data;
  tokenManager.setTokens(accessToken, newRefreshToken);

  return accessToken;
}

// Add to apiClient interceptor
apiClient.interceptors.response.use(
  (response) => response,
  async (error: AxiosError) => {
    const originalRequest = error.config as InternalAxiosRequestConfig & { _retry?: boolean };

    if (error.response?.status === 401 && !originalRequest._retry) {
      if (isRefreshing) {
        // Wait for token refresh
        return new Promise((resolve) => {
          subscribeTokenRefresh((token: string) => {
            if (originalRequest.headers) {
              originalRequest.headers.Authorization = `Bearer ${token}`;
            }
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

        if (originalRequest.headers) {
          originalRequest.headers.Authorization = `Bearer ${newToken}`;
        }
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

## ❌ 6. Request Cancellation

Cancel ongoing requests when components unmount or when requests become obsolete.

### Using AbortController

```typescript
// src/hooks/useZakatCalculations.ts
import { useState, useEffect } from "react";
import { zakatService } from "@/services/zakatService";
import type { ZakatCalculation } from "@/types/zakat";

export function useZakatCalculations() {
  const [calculations, setCalculations] = useState<ZakatCalculation[]>([]);
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState<Error | null>(null);

  useEffect(() => {
    const abortController = new AbortController();

    const fetchCalculations = async () => {
      setIsLoading(true);
      setError(null);

      try {
        const response = await apiClient.get<ZakatCalculation[]>("/zakat/calculations", {
          signal: abortController.signal,
        });
        setCalculations(response.data);
      } catch (err) {
        if (axios.isCancel(err)) {
          console.log("Request cancelled:", err.message);
        } else {
          setError(err as Error);
        }
      } finally {
        setIsLoading(false);
      }
    };

    fetchCalculations();

    // Cleanup: Cancel request when component unmounts
    return () => {
      abortController.abort();
    };
  }, []);

  return { calculations, isLoading, error };
}
```

### Manual Cancellation

```typescript
// src/components/SearchDonations.tsx
import { useState, useRef } from "react";
import apiClient from "@/lib/api/client";
import type { Donation } from "@/types/donation";

export function SearchDonations() {
  const [query, setQuery] = useState("");
  const [results, setResults] = useState<Donation[]>([]);
  const abortControllerRef = useRef<AbortController | null>(null);

  const handleSearch = async (searchQuery: string) => {
    // Cancel previous request if exists
    if (abortControllerRef.current) {
      abortControllerRef.current.abort();
    }

    // Create new AbortController for this request
    abortControllerRef.current = new AbortController();

    try {
      const response = await apiClient.get<Donation[]>("/donations/search", {
        params: { q: searchQuery },
        signal: abortControllerRef.current.signal,
      });

      setResults(response.data);
    } catch (error) {
      if (!axios.isCancel(error)) {
        console.error("Search failed:", error);
      }
    }
  };

  return (
    <div>
      <input type="search" value={query} onChange={(e) => setQuery(e.target.value)} placeholder="Search donations..." />
      <button onClick={() => handleSearch(query)}>Search</button>
      {/* Render results */}
    </div>
  );
}
```

## 📤 7. File Uploads

Upload files with progress tracking.

### Upload with FormData

```typescript
// src/services/uploadService.ts
import apiClient from "@/lib/api/client";

export interface UploadProgress {
  loaded: number;
  total: number;
  percentage: number;
}

export const uploadService = {
  uploadDocument: async (
    file: File,
    contractId: string,
    onProgress?: (progress: UploadProgress) => void,
  ): Promise<{ url: string }> => {
    const formData = new FormData();
    formData.append("file", file);
    formData.append("contractId", contractId);

    const response = await apiClient.post<{ url: string }>("/uploads/contract-documents", formData, {
      headers: {
        "Content-Type": "multipart/form-data",
      },
      onUploadProgress: (progressEvent) => {
        if (progressEvent.total && onProgress) {
          const percentage = Math.round((progressEvent.loaded * 100) / progressEvent.total);
          onProgress({
            loaded: progressEvent.loaded,
            total: progressEvent.total,
            percentage,
          });
        }
      },
    });

    return response.data;
  },
};
```

### Upload Component

```typescript
// src/components/DocumentUploader.tsx
import { useState } from "react";
import { uploadService, UploadProgress } from "@/services/uploadService";

export function DocumentUploader({ contractId }: { contractId: string }) {
  const [uploading, setUploading] = useState(false);
  const [progress, setProgress] = useState<UploadProgress | null>(null);

  const handleFileChange = async (event: React.ChangeEvent<HTMLInputElement>) => {
    const file = event.target.files?.[0];
    if (!file) return;

    setUploading(true);
    setProgress(null);

    try {
      const result = await uploadService.uploadDocument(file, contractId, (progressData) => {
        setProgress(progressData);
      });

      console.log("File uploaded successfully:", result.url);
      alert("Document uploaded successfully!");
    } catch (error) {
      console.error("Upload failed:", error);
      alert("Upload failed. Please try again.");
    } finally {
      setUploading(false);
      setProgress(null);
    }
  };

  return (
    <div className="document-uploader">
      <input type="file" onChange={handleFileChange} disabled={uploading} accept=".pdf,.doc,.docx" />

      {uploading && progress && (
        <div className="upload-progress">
          <progress value={progress.percentage} max="100" />
          <span>{progress.percentage}% uploaded</span>
        </div>
      )}
    </div>
  );
}
```

## 🔍 8. Query Parameters

Handle search, filtering, sorting, and pagination with query parameters.

### Building Query Strings

```typescript
// src/services/donationService.ts
import apiClient from "@/lib/api/client";
import type { Donation } from "@/types/donation";

export interface DonationFilters {
  campaignId?: string;
  status?: "pending" | "completed" | "failed";
  minAmount?: number;
  maxAmount?: number;
  sortBy?: "amount" | "date" | "status";
  sortOrder?: "asc" | "desc";
  page?: number;
  limit?: number;
}

export interface PaginatedResponse<T> {
  items: T[];
  total: number;
  page: number;
  totalPages: number;
}

export const donationService = {
  search: async (filters: DonationFilters): Promise<PaginatedResponse<Donation>> => {
    const response = await apiClient.get<PaginatedResponse<Donation>>("/donations", {
      params: filters, // Axios automatically converts to query string
    });
    return response.data;
  },
};

// Usage
const donations = await donationService.search({
  status: "completed",
  minAmount: 50,
  sortBy: "date",
  sortOrder: "desc",
  page: 1,
  limit: 20,
});
// GET /donations?status=completed&minAmount=50&sortBy=date&sortOrder=desc&page=1&limit=20
```

### Custom Query Builder

```typescript
// src/lib/api/queryBuilder.ts
export class QueryBuilder {
  private params: Record<string, string | number | boolean> = {};

  filter(key: string, value: string | number | boolean): this {
    this.params[key] = value;
    return this;
  }

  sort(field: string, order: "asc" | "desc" = "asc"): this {
    this.params.sortBy = field;
    this.params.sortOrder = order;
    return this;
  }

  paginate(page: number, limit: number = 20): this {
    this.params.page = page;
    this.params.limit = limit;
    return this;
  }

  build(): Record<string, string | number | boolean> {
    return this.params;
  }
}

// Usage
const params = new QueryBuilder()
  .filter("status", "completed")
  .filter("minAmount", 100)
  .sort("date", "desc")
  .paginate(1, 20)
  .build();

const response = await apiClient.get("/donations", { params });
```

## 📋 9. API Versioning

Handle multiple API versions gracefully.

### Version Headers

```typescript
// src/lib/api/client.ts
import axios from "axios";

const apiClient = axios.create({
  baseURL: import.meta.env.VITE_API_URL,
  headers: {
    "Content-Type": "application/json",
    "API-Version": "v2", // Version header
  },
});
```

### URL Versioning

```typescript
// src/services/zakatService.v1.ts
import apiClient from "@/lib/api/client";

export const zakatServiceV1 = {
  getAll: async () => {
    const response = await apiClient.get("/v1/zakat/calculations");
    return response.data;
  },
};

// src/services/zakatService.v2.ts
export const zakatServiceV2 = {
  getAll: async () => {
    const response = await apiClient.get("/v2/zakat/calculations");
    return response.data;
  },
};

// Usage
import { zakatServiceV2 } from "@/services/zakatService.v2";

const calculations = await zakatServiceV2.getAll();
```

## 🔧 10. OpenAPI Integration

Generate TypeScript types from OpenAPI schemas for automatic type safety.

### Installing OpenAPI Generator

```bash
npm install --save-dev openapi-typescript
```

### Generate Types

```bash
# Generate types from OpenAPI spec
npx openapi-typescript https://api.example.com/openapi.json -o src/types/api.d.ts
```

### Usage with Generated Types

```typescript
// src/types/api.d.ts (generated)
export interface paths {
  "/zakat/calculations": {
    get: {
      responses: {
        200: {
          content: {
            "application/json": components["schemas"]["ZakatCalculation"][];
          };
        };
      };
    };
  };
}

export interface components {
  schemas: {
    ZakatCalculation: {
      id: string;
      wealth: number;
      nisab: number;
      zakatAmount: number;
    };
  };
}

// src/services/zakatService.ts
import type { components } from "@/types/api";
import apiClient from "@/lib/api/client";

type ZakatCalculation = components["schemas"]["ZakatCalculation"];

export const zakatService = {
  getAll: async (): Promise<ZakatCalculation[]> => {
    const response = await apiClient.get<ZakatCalculation[]>("/zakat/calculations");
    return response.data;
  },
};
```

## 🧪 11. Mock API

Mock API responses for testing and development using Mock Service Worker (MSW).

### Installing MSW

```bash
npm install --save-dev msw
```

### Setting Up Handlers

```typescript
// src/mocks/handlers.ts
import { http, HttpResponse } from "msw";
import type { ZakatCalculation } from "@/types/zakat";

const mockCalculations: ZakatCalculation[] = [
  {
    id: "1",
    userId: "user-123",
    wealth: 10000,
    nisab: 5000,
    zakatAmount: 250,
    calculationDate: "2026-01-29",
    status: "submitted",
  },
  {
    id: "2",
    userId: "user-123",
    wealth: 25000,
    nisab: 5000,
    zakatAmount: 625,
    calculationDate: "2026-01-15",
    status: "paid",
  },
];

export const handlers = [
  // GET /zakat/calculations
  http.get("/api/zakat/calculations", () => {
    return HttpResponse.json(mockCalculations);
  }),

  // GET /zakat/calculations/:id
  http.get("/api/zakat/calculations/:id", ({ params }) => {
    const { id } = params;
    const calculation = mockCalculations.find((c) => c.id === id);

    if (!calculation) {
      return new HttpResponse(null, { status: 404 });
    }

    return HttpResponse.json(calculation);
  }),

  // POST /zakat/calculations
  http.post("/api/zakat/calculations", async ({ request }) => {
    const body = (await request.json()) as { wealth: number; nisab: number };

    const newCalculation: ZakatCalculation = {
      id: String(mockCalculations.length + 1),
      userId: "user-123",
      wealth: body.wealth,
      nisab: body.nisab,
      zakatAmount: body.wealth * 0.025,
      calculationDate: new Date().toISOString(),
      status: "draft",
    };

    mockCalculations.push(newCalculation);
    return HttpResponse.json(newCalculation, { status: 201 });
  }),
];
```

### Browser Setup

```typescript
// src/mocks/browser.ts
import { setupWorker } from "msw/browser";
import { handlers } from "./handlers";

export const worker = setupWorker(...handlers);
```

### Enable in Development

```typescript
// src/main.tsx
import React from "react";
import ReactDOM from "react-dom/client";
import App from "./App";

async function enableMocking() {
  if (import.meta.env.MODE !== "development") {
    return;
  }

  const { worker } = await import("./mocks/browser");
  return worker.start();
}

enableMocking().then(() => {
  ReactDOM.createRoot(document.getElementById("root")!).render(
    <React.StrictMode>
      <App />
    </React.StrictMode>,
  );
});
```

## ⚡ 12. Best Practices

### Retry Logic

```typescript
// src/lib/api/retry.ts
import type { AxiosRequestConfig } from "axios";
import apiClient from "./client";

export async function apiWithRetry<T>(
  url: string,
  config: AxiosRequestConfig = {},
  maxRetries: number = 3,
): Promise<T> {
  let lastError: Error;

  for (let attempt = 0; attempt <= maxRetries; attempt++) {
    try {
      const response = await apiClient.get<T>(url, config);
      return response.data;
    } catch (error) {
      lastError = error as Error;

      // Don't retry on client errors (4xx)
      if (
        axios.isAxiosError(error) &&
        error.response?.status &&
        error.response.status >= 400 &&
        error.response.status < 500
      ) {
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

### Request Timeouts

```typescript
// src/services/zakatService.ts
import apiClient from "@/lib/api/client";

export const zakatService = {
  getAll: async (): Promise<ZakatCalculation[]> => {
    const response = await apiClient.get<ZakatCalculation[]>("/zakat/calculations", {
      timeout: 5000, // 5 seconds timeout
    });
    return response.data;
  },
};
```

### Response Caching

```typescript
// src/lib/api/cache.ts
const responseCache = new Map<string, { data: unknown; timestamp: number }>();

export function getCachedResponse<T>(key: string, maxAge: number = 60000): T | null {
  const cached = responseCache.get(key);
  if (!cached) return null;

  const age = Date.now() - cached.timestamp;
  if (age > maxAge) {
    responseCache.delete(key);
    return null;
  }

  return cached.data as T;
}

export function setCachedResponse(key: string, data: unknown): void {
  responseCache.set(key, {
    data,
    timestamp: Date.now(),
  });
}

// Usage
export const zakatService = {
  getAll: async (): Promise<ZakatCalculation[]> => {
    const cacheKey = "/zakat/calculations";
    const cached = getCachedResponse<ZakatCalculation[]>(cacheKey);

    if (cached) {
      return cached;
    }

    const response = await apiClient.get<ZakatCalculation[]>(cacheKey);
    setCachedResponse(cacheKey, response.data);
    return response.data;
  },
};
```

## 📖 Related Resources

**Official Documentation**:

- [Axios Documentation](https://axios-http.com/) - Promise-based HTTP client
- [Fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API) - Native browser API
- [MSW Documentation](https://mswjs.io/) - Mock Service Worker for API mocking
- [OpenAPI TypeScript](https://github.com/drwpow/openapi-typescript) - Type generation from OpenAPI specs

**Repository Conventions**:

- [React Data Fetching](ex-so-plwe-fere__data-fetching.md) - Data fetching patterns with React Query and SWR
- [React State Management](ex-so-plwe-fere__state-management.md) - Managing API response state
- [TypeScript Best Practices](../../prog-lang/typescript/ex-so-prla-ty__best-practices.md) - TypeScript standards
- [Error Handling Patterns](../../prog-lang/typescript/ex-so-prla-ty__error-handling.md) - Error handling in TypeScript

**Related Documentation**:

- [React Data Fetching](./ex-so-plwe-fere__data-fetching.md) - Data fetching patterns
- [React Security](./ex-so-plwe-fere__security.md) - Authentication and authorization
- [React Testing](./ex-so-plwe-fere__testing.md) - Testing API integrations

---

This document provides comprehensive patterns for consuming REST APIs in React applications. Choose Axios for rich features, implement type-safe clients with TypeScript, handle errors gracefully, manage authentication securely, and use MSW for testing and development.
