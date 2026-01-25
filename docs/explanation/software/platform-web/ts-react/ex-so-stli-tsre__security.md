---
title: "React Security"
description: Security best practices and patterns for React applications
category: explanation
subcategory: stack-libs
tags:
  - react
  - security
  - xss
  - authentication
  - authorization
related:
  - ./ex-so-stli-tsre__best-practices.md
  - ./ex-so-stli-tsre__anti-patterns.md
principles:
  - explicit-over-implicit
last_updated: 2026-01-25
---

# React Security

## Quick Reference

**Navigation**: [Stack Libraries](../README.md) > [TypeScript React](./README.md) > Security

**Related Guides**:

- [Best Practices](./ex-so-stli-tsre__best-practices.md) - Security standards
- [Anti-Patterns](./ex-so-stli-tsre__anti-patterns.md) - Security mistakes

## Overview

Security is critical for React applications handling sensitive data. This guide covers XSS prevention, CSRF protection, authentication, authorization, and secure API communication.

**Target Audience**: Developers building secure React applications, particularly Islamic finance platforms handling financial transactions and personal data.

**React Version**: React 18.2+ with TypeScript 5+

## XSS Prevention

### Avoid dangerouslySetInnerHTML

```typescript
// ❌ Dangerous - XSS vulnerability
export const UserComment: React.FC<{ comment: string }> = ({ comment }) => (
  <div dangerouslySetInnerHTML={{ __html: comment }} />
);

// ✅ Safe - React escapes by default
export const UserComment: React.FC<{ comment: string }> = ({ comment }) => (
  <div>{comment}</div>
);

// If HTML is needed, sanitize first
import DOMPurify from 'dompurify';

export const SafeHtmlContent: React.FC<{ html: string }> = ({ html }) => {
  const sanitizedHtml = DOMPurify.sanitize(html, {
    ALLOWED_TAGS: ['p', 'strong', 'em', 'a'],
    ALLOWED_ATTR: ['href'],
  });

  return <div dangerouslySetInnerHTML={{ __html: sanitizedHtml }} />;
};
```

### Input Validation

```typescript
export const DonationForm: React.FC = () => {
  const [amount, setAmount] = useState('');
  const [error, setError] = useState('');

  const handleAmountChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const value = e.target.value;

    // Validate input
    if (!/^\d*\.?\d*$/.test(value)) {
      setError('Amount must be a valid number');
      return;
    }

    const numValue = parseFloat(value);
    if (numValue < 0) {
      setError('Amount must be positive');
      return;
    }

    setError('');
    setAmount(value);
  };

  return (
    <div>
      <input
        type="text"
        value={amount}
        onChange={handleAmountChange}
        pattern="[0-9]*\.?[0-9]*"
      />
      {error && <span className="error">{error}</span>}
    </div>
  );
};
```

## Authentication

### Protected Routes

```typescript
import { Navigate, Outlet } from 'react-router-dom';
import { useAuth } from '../hooks/useAuth';

export const ProtectedRoute: React.FC = () => {
  const { user, loading } = useAuth();

  if (loading) {
    return <LoadingSpinner />;
  }

  if (!user) {
    return <Navigate to="/login" replace />;
  }

  return <Outlet />;
};

// Usage
<Routes>
  <Route element={<ProtectedRoute />}>
    <Route path="/dashboard" element={<Dashboard />} />
    <Route path="/zakat" element={<ZakatCalculator />} />
  </Route>
</Routes>
```

### JWT Token Management

```typescript
// api/authApi.ts
export const authApi = {
  async login(email: string, password: string): Promise<AuthResponse> {
    const response = await fetch("/api/auth/login", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ email, password }),
    });

    if (!response.ok) throw new Error("Login failed");

    const data = await response.json();

    // Store tokens securely
    sessionStorage.setItem("accessToken", data.accessToken);
    // Never store in localStorage - use httpOnly cookies for refresh tokens

    return data;
  },

  async refreshToken(): Promise<string> {
    const response = await fetch("/api/auth/refresh", {
      method: "POST",
      credentials: "include", // Send httpOnly cookie
    });

    if (!response.ok) throw new Error("Token refresh failed");

    const data = await response.json();
    sessionStorage.setItem("accessToken", data.accessToken);

    return data.accessToken;
  },

  logout() {
    sessionStorage.removeItem("accessToken");
    // Clear httpOnly cookie via API call
    fetch("/api/auth/logout", {
      method: "POST",
      credentials: "include",
    });
  },
};

// API interceptor
async function apiCall(url: string, options: RequestInit = {}) {
  const token = sessionStorage.getItem("accessToken");

  const response = await fetch(url, {
    ...options,
    headers: {
      ...options.headers,
      Authorization: token ? `Bearer ${token}` : "",
    },
  });

  // Handle token expiration
  if (response.status === 401) {
    try {
      const newToken = await authApi.refreshToken();
      // Retry with new token
      return fetch(url, {
        ...options,
        headers: {
          ...options.headers,
          Authorization: `Bearer ${newToken}`,
        },
      });
    } catch {
      // Refresh failed, redirect to login
      window.location.href = "/login";
      throw new Error("Authentication required");
    }
  }

  return response;
}
```

## CSRF Protection

### CSRF Token Handling

```typescript
// Get CSRF token from meta tag
function getCsrfToken(): string {
  const token = document.querySelector<HTMLMetaElement>('meta[name="csrf-token"]')?.content;

  if (!token) {
    throw new Error("CSRF token not found");
  }

  return token;
}

// Include in requests
async function apiPost(url: string, data: any) {
  const response = await fetch(url, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      "X-CSRF-Token": getCsrfToken(),
    },
    body: JSON.stringify(data),
  });

  return response;
}
```

## Related Documentation

- **[Best Practices](./ex-so-stli-tsre__best-practices.md)** - Security standards
- **[Anti-Patterns](./ex-so-stli-tsre__anti-patterns.md)** - Security mistakes

---

**Last Updated**: 2026-01-25
