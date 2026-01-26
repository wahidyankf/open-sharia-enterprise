# React API Client Template

Production-ready TypeScript API client template with error handling following OSE Platform conventions.

## Template

```typescript
/**
 * API Response wrapper
 */
interface ApiResponse<T> {
  data?: T;
  error?: string;
  status: number;
}

/**
 * API Error class
 */
export class ApiError extends Error {
  constructor(
    message: string,
    public status: number,
    public response?: any,
  ) {
    super(message);
    this.name = "ApiError";
  }
}

/**
 * Base API Client configuration
 */
interface ApiClientConfig {
  baseURL: string;
  timeout?: number;
  headers?: Record<string, string>;
}

/**
 * API Client class
 */
export class ApiClient {
  private baseURL: string;
  private timeout: number;
  private defaultHeaders: Record<string, string>;

  constructor(config: ApiClientConfig) {
    this.baseURL = config.baseURL;
    this.timeout = config.timeout || 30000;
    this.defaultHeaders = {
      "Content-Type": "application/json",
      ...config.headers,
    };
  }

  /**
   * Set authentication token
   */
  setAuthToken(token: string): void {
    this.defaultHeaders["Authorization"] = `Bearer ${token}`;
  }

  /**
   * Remove authentication token
   */
  clearAuthToken(): void {
    delete this.defaultHeaders["Authorization"];
  }

  /**
   * Generic request method
   */
  private async request<T>(endpoint: string, options: RequestInit = {}): Promise<ApiResponse<T>> {
    const url = `${this.baseURL}${endpoint}`;

    const controller = new AbortController();
    const timeoutId = setTimeout(() => controller.abort(), this.timeout);

    try {
      const response = await fetch(url, {
        ...options,
        headers: {
          ...this.defaultHeaders,
          ...options.headers,
        },
        signal: controller.signal,
      });

      clearTimeout(timeoutId);

      // Parse response
      const contentType = response.headers.get("content-type");
      const data = contentType?.includes("application/json") ? await response.json() : await response.text();

      if (!response.ok) {
        throw new ApiError(data.message || `HTTP error: ${response.status}`, response.status, data);
      }

      return {
        data,
        status: response.status,
      };
    } catch (error) {
      clearTimeout(timeoutId);

      if (error instanceof ApiError) {
        throw error;
      }

      if (error instanceof DOMException && error.name === "AbortError") {
        throw new ApiError("Request timeout", 408);
      }

      throw new ApiError(error instanceof Error ? error.message : "Unknown error", 0);
    }
  }

  /**
   * GET request
   */
  async get<T>(endpoint: string, params?: Record<string, any>): Promise<T> {
    const queryString = params ? "?" + new URLSearchParams(params).toString() : "";

    const response = await this.request<T>(endpoint + queryString, {
      method: "GET",
    });

    return response.data!;
  }

  /**
   * POST request
   */
  async post<T>(endpoint: string, data?: any): Promise<T> {
    const response = await this.request<T>(endpoint, {
      method: "POST",
      body: data ? JSON.stringify(data) : undefined,
    });

    return response.data!;
  }

  /**
   * PUT request
   */
  async put<T>(endpoint: string, data?: any): Promise<T> {
    const response = await this.request<T>(endpoint, {
      method: "PUT",
      body: data ? JSON.stringify(data) : undefined,
    });

    return response.data!;
  }

  /**
   * PATCH request
   */
  async patch<T>(endpoint: string, data?: any): Promise<T> {
    const response = await this.request<T>(endpoint, {
      method: "PATCH",
      body: data ? JSON.stringify(data) : undefined,
    });

    return response.data!;
  }

  /**
   * DELETE request
   */
  async delete<T>(endpoint: string): Promise<T> {
    const response = await this.request<T>(endpoint, {
      method: "DELETE",
    });

    return response.data!;
  }
}

// Create singleton instance
export const apiClient = new ApiClient({
  baseURL: process.env.REACT_APP_API_URL || "http://localhost:8080/api",
  timeout: 30000,
});

export default apiClient;
```

## Usage

### Basic Usage

```typescript
import apiClient from "./apiClient";

// GET request
const users = await apiClient.get("/users");

// POST request
const newUser = await apiClient.post("/users", {
  name: "John Doe",
  email: "john@example.com",
});

// PUT request
const updatedUser = await apiClient.put("/users/123", {
  name: "Jane Doe",
});

// DELETE request
await apiClient.delete("/users/123");
```

### With Authentication

```typescript
import apiClient from "./apiClient";

// Set token after login
apiClient.setAuthToken("your-jwt-token");

// Make authenticated requests
const profile = await apiClient.get("/profile");

// Clear token on logout
apiClient.clearAuthToken();
```

### Error Handling

```typescript
import apiClient, { ApiError } from "./apiClient";

try {
  const data = await apiClient.get("/users");
  console.log(data);
} catch (error) {
  if (error instanceof ApiError) {
    if (error.status === 401) {
      // Redirect to login
      window.location.href = "/login";
    } else if (error.status === 404) {
      console.error("Resource not found");
    } else {
      console.error("API error:", error.message);
    }
  } else {
    console.error("Unknown error:", error);
  }
}
```

## OSE Platform Examples

### Zakat API Client

```typescript
import apiClient from "./apiClient";

export interface ZakatCalculationInput {
  wealth: number;
  currency: string;
  userId: string;
}

export interface ZakatCalculationResult {
  id: string;
  wealth: number;
  nisabThreshold: number;
  zakatAmount: number;
  zakatDue: boolean;
  calculationDate: string;
}

export const zakatApi = {
  /**
   * Calculate zakat
   */
  async calculate(input: ZakatCalculationInput): Promise<ZakatCalculationResult> {
    return apiClient.post<ZakatCalculationResult>("/zakat/calculate", input);
  },

  /**
   * Get calculation history
   */
  async getHistory(userId: string): Promise<ZakatCalculationResult[]> {
    return apiClient.get<ZakatCalculationResult[]>("/zakat/calculations", {
      userId,
    });
  },

  /**
   * Get calculation by ID
   */
  async getById(id: string): Promise<ZakatCalculationResult> {
    return apiClient.get<ZakatCalculationResult>(`/zakat/calculations/${id}`);
  },

  /**
   * Delete calculation
   */
  async deleteCalculation(id: string): Promise<void> {
    return apiClient.delete(`/zakat/calculations/${id}`);
  },

  /**
   * Get nisab threshold
   */
  async getNisabThreshold(currency: string): Promise<number> {
    return apiClient.get<number>("/zakat/nisab-threshold", { currency });
  },
};

// Usage
const result = await zakatApi.calculate({
  wealth: 10000,
  currency: "USD",
  userId: "user-123",
});

console.log("Zakat amount:", result.zakatAmount);
```

### Murabaha API Client

```typescript
import apiClient from "./apiClient";

export interface MurabahaApplication {
  applicantName: string;
  email: string;
  phoneNumber: string;
  requestedAmount: number;
  purpose: string;
  installmentMonths: number;
}

export interface MurabahaContract {
  id: string;
  purchasePrice: number;
  markup: number;
  totalAmount: number;
  installments: number;
  installmentAmount: number;
  status: "pending" | "active" | "completed";
  nextPaymentDate: string;
}

export interface InstallmentPayment {
  id: string;
  contractId: string;
  amount: number;
  dueDate: string;
  paidDate?: string;
  status: "pending" | "paid" | "overdue";
}

export const murabahaApi = {
  /**
   * Submit application
   */
  async submitApplication(application: MurabahaApplication): Promise<{ applicationId: string }> {
    return apiClient.post("/murabaha/applications", application);
  },

  /**
   * Get contracts
   */
  async getContracts(userId: string): Promise<MurabahaContract[]> {
    return apiClient.get<MurabahaContract[]>("/murabaha/contracts", { userId });
  },

  /**
   * Get contract by ID
   */
  async getContract(contractId: string): Promise<MurabahaContract> {
    return apiClient.get<MurabahaContract>(`/murabaha/contracts/${contractId}`);
  },

  /**
   * Get installment schedule
   */
  async getInstallments(contractId: string): Promise<InstallmentPayment[]> {
    return apiClient.get<InstallmentPayment[]>(`/murabaha/contracts/${contractId}/installments`);
  },

  /**
   * Make payment
   */
  async makePayment(contractId: string, installmentId: string, amount: number): Promise<{ paymentId: string }> {
    return apiClient.post(`/murabaha/contracts/${contractId}/payments`, {
      installmentId,
      amount,
    });
  },

  /**
   * Calculate contract terms
   */
  async calculateTerms(
    purchasePrice: number,
    installmentMonths: number,
  ): Promise<{
    markup: number;
    totalAmount: number;
    installmentAmount: number;
  }> {
    return apiClient.post("/murabaha/calculate-terms", {
      purchasePrice,
      installmentMonths,
    });
  },
};

// Usage
const application = await murabahaApi.submitApplication({
  applicantName: "John Doe",
  email: "john@example.com",
  phoneNumber: "+1234567890",
  requestedAmount: 50000,
  purpose: "Home renovation",
  installmentMonths: 36,
});

console.log("Application ID:", application.applicationId);
```

### Waqf API Client

```typescript
import apiClient from "./apiClient";

export interface WaqfProject {
  id: string;
  name: string;
  description: string;
  targetAmount: number;
  currentAmount: number;
  status: "active" | "completed" | "paused";
}

export interface WaqfDonation {
  id: string;
  projectId: string;
  donorId: string;
  amount: number;
  recurring: boolean;
  frequency?: "monthly" | "quarterly" | "annually";
  donationDate: string;
}

export interface ImpactReport {
  projectId: string;
  reportDate: string;
  beneficiariesReached: number;
  activitiesCompleted: number;
  totalSpent: number;
  summary: string;
}

export const waqfApi = {
  /**
   * Get active projects
   */
  async getProjects(): Promise<WaqfProject[]> {
    return apiClient.get<WaqfProject[]>("/waqf/projects");
  },

  /**
   * Get project by ID
   */
  async getProject(projectId: string): Promise<WaqfProject> {
    return apiClient.get<WaqfProject>(`/waqf/projects/${projectId}`);
  },

  /**
   * Make donation
   */
  async makeDonation(donation: Omit<WaqfDonation, "id" | "donationDate">): Promise<WaqfDonation> {
    return apiClient.post<WaqfDonation>("/waqf/donations", donation);
  },

  /**
   * Get donor's donations
   */
  async getDonations(donorId: string): Promise<WaqfDonation[]> {
    return apiClient.get<WaqfDonation[]>("/waqf/donations", { donorId });
  },

  /**
   * Cancel recurring donation
   */
  async cancelRecurring(donationId: string): Promise<void> {
    return apiClient.delete(`/waqf/donations/${donationId}/recurring`);
  },

  /**
   * Get impact reports
   */
  async getImpactReports(projectId: string): Promise<ImpactReport[]> {
    return apiClient.get<ImpactReport[]>(`/waqf/projects/${projectId}/impact-reports`);
  },

  /**
   * Get donor statistics
   */
  async getDonorStats(donorId: string): Promise<{
    totalDonated: number;
    projectsSupported: number;
    recurringDonations: number;
  }> {
    return apiClient.get(`/waqf/donors/${donorId}/stats`);
  },
};

// Usage
const projects = await waqfApi.getProjects();

const donation = await waqfApi.makeDonation({
  projectId: "project-123",
  donorId: "donor-456",
  amount: 100,
  recurring: true,
  frequency: "monthly",
});

console.log("Donation ID:", donation.id);
```

## API Client Patterns

### Request Interceptors

```typescript
class ApiClient {
  private requestInterceptors: Array<(config: RequestInit) => RequestInit> = [];

  addRequestInterceptor(interceptor: (config: RequestInit) => RequestInit) {
    this.requestInterceptors.push(interceptor);
  }

  private applyRequestInterceptors(config: RequestInit): RequestInit {
    return this.requestInterceptors.reduce((acc, interceptor) => interceptor(acc), config);
  }
}

// Usage
apiClient.addRequestInterceptor((config) => {
  console.log("Request:", config);
  return config;
});
```

### Response Interceptors

```typescript
class ApiClient {
  private responseInterceptors: Array<(response: Response) => Response> = [];

  addResponseInterceptor(interceptor: (response: Response) => Response) {
    this.responseInterceptors.push(interceptor);
  }
}

// Usage
apiClient.addResponseInterceptor((response) => {
  console.log("Response status:", response.status);
  return response;
});
```

### Retry Logic

```typescript
async function withRetry<T>(fn: () => Promise<T>, retries: number = 3, delay: number = 1000): Promise<T> {
  try {
    return await fn();
  } catch (error) {
    if (retries === 0) throw error;

    await new Promise((resolve) => setTimeout(resolve, delay));
    return withRetry(fn, retries - 1, delay * 2);
  }
}

// Usage
const data = await withRetry(() => apiClient.get("/users"));
```

## Best Practices

1. **Error Handling**: Always wrap API calls in try-catch blocks
2. **Type Safety**: Define TypeScript interfaces for all request/response types
3. **Timeout**: Set reasonable timeout values (default 30s)
4. **Auth Token**: Store token securely, set after login, clear on logout
5. **Base URL**: Use environment variables for API base URL
6. **Singleton**: Export singleton instance for app-wide usage
7. **Request Cancellation**: Use AbortController for timeout and manual cancellation
8. **Loading States**: Track loading state in components
9. **Retry Logic**: Implement retry for transient failures
10. **Testing**: Mock API client in tests

## Testing

```typescript
import apiClient, { ApiError } from "./apiClient";

// Mock fetch globally
global.fetch = jest.fn();

describe("ApiClient", () => {
  beforeEach(() => {
    (global.fetch as jest.Mock).mockClear();
  });

  it("makes GET request", async () => {
    (global.fetch as jest.Mock).mockResolvedValueOnce({
      ok: true,
      status: 200,
      headers: new Headers({ "content-type": "application/json" }),
      json: async () => ({ id: 1, name: "Test" }),
    });

    const result = await apiClient.get("/users/1");

    expect(result).toEqual({ id: 1, name: "Test" });
    expect(global.fetch).toHaveBeenCalledWith(
      expect.stringContaining("/users/1"),
      expect.objectContaining({ method: "GET" }),
    );
  });

  it("makes POST request", async () => {
    (global.fetch as jest.Mock).mockResolvedValueOnce({
      ok: true,
      status: 201,
      headers: new Headers({ "content-type": "application/json" }),
      json: async () => ({ id: 1, name: "Test" }),
    });

    const result = await apiClient.post("/users", { name: "Test" });

    expect(result).toEqual({ id: 1, name: "Test" });
    expect(global.fetch).toHaveBeenCalledWith(
      expect.any(String),
      expect.objectContaining({
        method: "POST",
        body: JSON.stringify({ name: "Test" }),
      }),
    );
  });

  it("handles 404 error", async () => {
    (global.fetch as jest.Mock).mockResolvedValueOnce({
      ok: false,
      status: 404,
      headers: new Headers({ "content-type": "application/json" }),
      json: async () => ({ message: "Not found" }),
    });

    await expect(apiClient.get("/users/999")).rejects.toThrow(ApiError);
  });

  it("handles timeout", async () => {
    jest.useFakeTimers();

    (global.fetch as jest.Mock).mockImplementationOnce(
      () =>
        new Promise((resolve) => {
          setTimeout(() => resolve({ ok: true }), 40000);
        }),
    );

    const promise = apiClient.get("/users");

    jest.advanceTimersByTime(35000);

    await expect(promise).rejects.toThrow("Request timeout");

    jest.useRealTimers();
  });

  it("sets auth token", async () => {
    apiClient.setAuthToken("test-token");

    (global.fetch as jest.Mock).mockResolvedValueOnce({
      ok: true,
      status: 200,
      headers: new Headers({ "content-type": "application/json" }),
      json: async () => ({ data: "test" }),
    });

    await apiClient.get("/protected");

    expect(global.fetch).toHaveBeenCalledWith(
      expect.any(String),
      expect.objectContaining({
        headers: expect.objectContaining({
          Authorization: "Bearer test-token",
        }),
      }),
    );

    apiClient.clearAuthToken();
  });
});
```
