---
title: React Security
description: Comprehensive frontend security practices for React applications including XSS prevention, authentication, authorization, and secure data handling
category: explanation
tags:
  - react
  - security
  - frontend
  - xss
  - csrf
  - authentication
  - authorization
created: 2026-01-29
updated: 2026-01-29
---

# React Security

## 📋 Overview

Frontend security in React applications is critical for protecting users from attacks, safeguarding sensitive data, and maintaining trust in Sharia-compliant systems. This document covers comprehensive security practices including XSS prevention, CSRF protection, authentication patterns, authorization models, and secure data handling.

**Key Security Priorities**:

- Prevent Cross-Site Scripting (XSS) attacks
- Implement robust authentication and authorization
- Protect against Cross-Site Request Forgery (CSRF)
- Enforce HTTPS and Content Security Policy
- Secure sensitive data storage and transmission
- Validate and sanitize all user inputs
- Manage dependencies and security updates

## 🛡️ XSS Prevention

### Understanding XSS Attacks

**Cross-Site Scripting (XSS)** occurs when attackers inject malicious scripts into web applications. React provides built-in protection, but developers must understand vulnerabilities.

**XSS Attack Types**:

- **Stored XSS**: Malicious script stored in database, executed when rendered
- **Reflected XSS**: Malicious script in URL parameters, reflected back to user
- **DOM-based XSS**: Client-side script manipulation without server involvement

### React's Built-in Protection

React automatically escapes values embedded in JSX, preventing most XSS attacks:

```javascript
// ✅ Safe - React escapes user input
function SafeComponent({ userInput }) {
  return <div>{userInput}</div>;
}

// User enters: <script>alert('XSS')</script>
// React renders: &lt;script&gt;alert('XSS')&lt;/script&gt;
```

### The Danger of dangerouslySetInnerHTML

**CRITICAL**: `dangerouslySetInnerHTML` bypasses React's XSS protection. Use only with sanitized content.

```javascript
// ❌ DANGEROUS - Never use with unsanitized user input
function UnsafeComponent({ userHTML }) {
  return <div dangerouslySetInnerHTML={{ __html: userHTML }} />;
}

// ✅ Safe - Sanitize with DOMPurify before rendering
import DOMPurify from "dompurify";

function SafeHTMLComponent({ userHTML }) {
  const sanitized = DOMPurify.sanitize(userHTML, {
    ALLOWED_TAGS: ["b", "i", "em", "strong", "p", "br"],
    ALLOWED_ATTR: [],
  });

  return <div dangerouslySetInnerHTML={{ __html: sanitized }} />;
}
```

### Sanitization Best Practices

**Use DOMPurify for HTML sanitization**:

```javascript
import DOMPurify from "dompurify";

// Configure allowed tags and attributes
const sanitizeConfig = {
  ALLOWED_TAGS: ["p", "br", "strong", "em", "u", "h1", "h2", "h3", "ul", "ol", "li", "a", "img"],
  ALLOWED_ATTR: ["href", "src", "alt", "title"],
  ALLOW_DATA_ATTR: false,
};

function renderUserContent(html) {
  return DOMPurify.sanitize(html, sanitizeConfig);
}
```

**For URLs, validate and sanitize**:

```javascript
// ✅ Validate URL scheme to prevent javascript: URLs
function SafeLink({ href, children }) {
  const isSafeURL = (url) => {
    try {
      const parsed = new URL(url, window.location.origin);
      return ["http:", "https:", "mailto:"].includes(parsed.protocol);
    } catch {
      return false;
    }
  };

  const safeHref = isSafeURL(href) ? href : "#";

  return (
    <a href={safeHref} rel="noopener noreferrer">
      {children}
    </a>
  );
}
```

### Input Validation

**Never trust user input**:

```javascript
// ✅ Validate and sanitize all inputs
function DonationForm() {
  const [amount, setAmount] = useState("");

  const handleAmountChange = (e) => {
    const value = e.target.value;

    // Only allow positive numbers
    if (/^\d*\.?\d{0,2}$/.test(value)) {
      setAmount(value);
    }
  };

  const handleSubmit = (e) => {
    e.preventDefault();

    // Server-side validation is still required
    const numAmount = parseFloat(amount);

    if (isNaN(numAmount) || numAmount <= 0) {
      alert("Invalid donation amount");
      return;
    }

    submitDonation(numAmount);
  };

  return (
    <form onSubmit={handleSubmit}>
      <input type="text" value={amount} onChange={handleAmountChange} placeholder="Enter amount" />
      <button type="submit">Donate</button>
    </form>
  );
}
```

## 🔒 CSRF Protection

### Understanding CSRF Attacks

**Cross-Site Request Forgery (CSRF)** tricks authenticated users into executing unwanted actions on web applications where they're authenticated.

**Attack Scenario**:

1. User logs into `bankapp.com`
2. User visits malicious site `evil.com`
3. `evil.com` sends forged request to `bankapp.com`
4. Browser automatically includes authentication cookies
5. Unauthorized action executed on user's behalf

### Token-Based CSRF Protection

**Double Submit Cookie Pattern**:

```javascript
// Server sets CSRF token in cookie (non-httpOnly)
// Client reads token and includes in request header

import axios from "axios";

// Configure axios to include CSRF token
axios.defaults.xsrfCookieName = "csrftoken";
axios.defaults.xsrfHeaderName = "X-CSRFToken";

// Make authenticated request
async function submitMurabahaContract(contractData) {
  try {
    const response = await axios.post("/api/murabaha/contracts", contractData);
    return response.data;
  } catch (error) {
    console.error("Contract submission failed:", error);
    throw error;
  }
}
```

**Synchronizer Token Pattern**:

```javascript
// Server provides CSRF token in initial page load
// Client includes token in every state-changing request

function MurabahaForm() {
  const [csrfToken, setCsrfToken] = useState("");

  useEffect(() => {
    // Fetch CSRF token on component mount
    fetch("/api/csrf-token")
      .then((res) => res.json())
      .then((data) => setCsrfToken(data.token));
  }, []);

  const handleSubmit = async (formData) => {
    await fetch("/api/murabaha/contracts", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        "X-CSRF-Token": csrfToken,
      },
      body: JSON.stringify(formData),
    });
  };

  return <form onSubmit={handleSubmit}>{/* form fields */}</form>;
}
```

### SameSite Cookie Attribute

**Modern CSRF protection using SameSite cookies**:

```javascript
// Server-side configuration (Express.js example)
app.use(
  session({
    secret: process.env.SESSION_SECRET,
    cookie: {
      httpOnly: true,
      secure: true, // HTTPS only
      sameSite: "strict", // Prevents CSRF
      maxAge: 3600000, // 1 hour
    },
  }),
);
```

**SameSite Options**:

- `Strict`: Cookie never sent in cross-site requests (strongest protection)
- `Lax`: Cookie sent only for top-level navigation GET requests
- `None`: Cookie sent in all cross-site requests (requires `Secure` flag)

## 🔐 Authentication

### JWT Storage Best Practices

**CRITICAL**: JWT storage location has security implications.

**Storage Options Comparison**:

| Storage           | XSS Risk | CSRF Risk | Best For              |
| ----------------- | -------- | --------- | --------------------- |
| localStorage      | ❌ High  | ✅ None   | Short-lived tokens    |
| sessionStorage    | ❌ High  | ✅ None   | Session-only apps     |
| Cookie (httpOnly) | ✅ Low   | ❌ High   | Long-lived tokens     |
| Memory (state)    | ✅ Low   | ✅ None   | Highly sensitive apps |

**Recommended Approach: httpOnly Cookie + Access/Refresh Token Pattern**:

```javascript
// Authentication service
class AuthService {
  async login(email, password) {
    // Server sets httpOnly cookie with refresh token
    // Server returns short-lived access token in response body
    const response = await fetch("/api/auth/login", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      credentials: "include", // Include cookies
      body: JSON.stringify({ email, password }),
    });

    const { accessToken, user } = await response.json();

    // Store access token in memory (React state/context)
    return { accessToken, user };
  }

  async refreshAccessToken() {
    // Refresh token sent automatically via httpOnly cookie
    const response = await fetch("/api/auth/refresh", {
      method: "POST",
      credentials: "include",
    });

    const { accessToken } = await response.json();
    return accessToken;
  }

  async logout() {
    await fetch("/api/auth/logout", {
      method: "POST",
      credentials: "include",
    });
  }
}
```

### Authentication Context

**Centralized authentication management**:

```javascript
import { createContext, useContext, useState, useEffect } from "react";

const AuthContext = createContext(null);

export function AuthProvider({ children }) {
  const [user, setUser] = useState(null);
  const [accessToken, setAccessToken] = useState(null);
  const [loading, setLoading] = useState(true);

  // Restore session on app load
  useEffect(() => {
    const restoreSession = async () => {
      try {
        // Try to refresh access token using httpOnly refresh token
        const response = await fetch("/api/auth/refresh", {
          method: "POST",
          credentials: "include",
        });

        if (response.ok) {
          const { accessToken, user } = await response.json();
          setAccessToken(accessToken);
          setUser(user);
        }
      } catch (error) {
        console.error("Session restoration failed:", error);
      } finally {
        setLoading(false);
      }
    };

    restoreSession();
  }, []);

  // Auto-refresh access token before expiry
  useEffect(() => {
    if (!accessToken) return;

    // Refresh 5 minutes before expiry
    const tokenExpiry = JSON.parse(atob(accessToken.split(".")[1])).exp;
    const refreshTime = tokenExpiry * 1000 - Date.now() - 5 * 60 * 1000;

    const timer = setTimeout(async () => {
      try {
        const newToken = await authService.refreshAccessToken();
        setAccessToken(newToken);
      } catch (error) {
        console.error("Token refresh failed:", error);
        logout();
      }
    }, refreshTime);

    return () => clearTimeout(timer);
  }, [accessToken]);

  const login = async (email, password) => {
    const { accessToken, user } = await authService.login(email, password);
    setAccessToken(accessToken);
    setUser(user);
  };

  const logout = async () => {
    await authService.logout();
    setAccessToken(null);
    setUser(null);
  };

  return <AuthContext.Provider value={{ user, accessToken, login, logout, loading }}>{children}</AuthContext.Provider>;
}

export const useAuth = () => {
  const context = useContext(AuthContext);
  if (!context) {
    throw new Error("useAuth must be used within AuthProvider");
  }
  return context;
};
```

### Authenticated API Requests

**Axios interceptor pattern**:

```javascript
import axios from "axios";

function setupAxiosInterceptors(getAccessToken, logout) {
  // Request interceptor: Add access token to headers
  axios.interceptors.request.use(
    (config) => {
      const token = getAccessToken();
      if (token) {
        config.headers.Authorization = `Bearer ${token}`;
      }
      return config;
    },
    (error) => Promise.reject(error),
  );

  // Response interceptor: Handle token expiry
  axios.interceptors.response.use(
    (response) => response,
    async (error) => {
      const originalRequest = error.config;

      // If 401 and not already retried
      if (error.response?.status === 401 && !originalRequest._retry) {
        originalRequest._retry = true;

        try {
          // Try to refresh access token
          const newToken = await authService.refreshAccessToken();

          // Retry original request with new token
          originalRequest.headers.Authorization = `Bearer ${newToken}`;
          return axios(originalRequest);
        } catch (refreshError) {
          // Refresh failed, logout user
          logout();
          return Promise.reject(refreshError);
        }
      }

      return Promise.reject(error);
    },
  );
}
```

## 🔑 Authorization

### Role-Based Access Control (RBAC)

**Implementing role checks in React**:

```javascript
// Define roles and permissions
const ROLES = {
  ADMIN: "admin",
  MANAGER: "manager",
  USER: "user",
};

const PERMISSIONS = {
  MANAGE_USERS: "manage_users",
  VIEW_CONTRACTS: "view_contracts",
  CREATE_CONTRACTS: "create_contracts",
  APPROVE_CONTRACTS: "approve_contracts",
};

const ROLE_PERMISSIONS = {
  [ROLES.ADMIN]: [
    PERMISSIONS.MANAGE_USERS,
    PERMISSIONS.VIEW_CONTRACTS,
    PERMISSIONS.CREATE_CONTRACTS,
    PERMISSIONS.APPROVE_CONTRACTS,
  ],
  [ROLES.MANAGER]: [PERMISSIONS.VIEW_CONTRACTS, PERMISSIONS.CREATE_CONTRACTS, PERMISSIONS.APPROVE_CONTRACTS],
  [ROLES.USER]: [PERMISSIONS.VIEW_CONTRACTS, PERMISSIONS.CREATE_CONTRACTS],
};

// Authorization hook
function useAuthorization() {
  const { user } = useAuth();

  const hasRole = (role) => {
    return user?.roles?.includes(role);
  };

  const hasPermission = (permission) => {
    if (!user?.roles) return false;

    return user.roles.some((role) => {
      return ROLE_PERMISSIONS[role]?.includes(permission);
    });
  };

  const hasAnyRole = (roles) => {
    return roles.some(hasRole);
  };

  const hasAllPermissions = (permissions) => {
    return permissions.every(hasPermission);
  };

  return {
    hasRole,
    hasPermission,
    hasAnyRole,
    hasAllPermissions,
  };
}
```

### Protected Routes

**Route guards for authorization**:

```javascript
import { Navigate, Outlet } from "react-router-dom";

// Require authentication
function PrivateRoute() {
  const { user, loading } = useAuth();

  if (loading) {
    return <LoadingSpinner />;
  }

  return user ? <Outlet /> : <Navigate to="/login" replace />;
}

// Require specific role
function RoleRoute({ allowedRoles }) {
  const { user } = useAuth();
  const { hasAnyRole } = useAuthorization();

  if (!user) {
    return <Navigate to="/login" replace />;
  }

  if (!hasAnyRole(allowedRoles)) {
    return <Navigate to="/unauthorized" replace />;
  }

  return <Outlet />;
}

// Require specific permission
function PermissionRoute({ requiredPermission }) {
  const { user } = useAuth();
  const { hasPermission } = useAuthorization();

  if (!user) {
    return <Navigate to="/login" replace />;
  }

  if (!hasPermission(requiredPermission)) {
    return <Navigate to="/unauthorized" replace />;
  }

  return <Outlet />;
}

// Usage in router
function AppRoutes() {
  return (
    <Routes>
      <Route path="/login" element={<LoginPage />} />

      {/* Protected routes */}
      <Route element={<PrivateRoute />}>
        <Route path="/dashboard" element={<Dashboard />} />

        {/* Role-based routes */}
        <Route element={<RoleRoute allowedRoles={[ROLES.ADMIN, ROLES.MANAGER]} />}>
          <Route path="/contracts/approve" element={<ApproveContracts />} />
        </Route>

        {/* Permission-based routes */}
        <Route element={<PermissionRoute requiredPermission={PERMISSIONS.MANAGE_USERS} />}>
          <Route path="/users" element={<UserManagement />} />
        </Route>
      </Route>
    </Routes>
  );
}
```

### UI-Level Authorization

**Conditionally render components based on permissions**:

```javascript
// Authorization components
function IfHasRole({ role, children, fallback = null }) {
  const { hasRole } = useAuthorization();
  return hasRole(role) ? children : fallback;
}

function IfHasPermission({ permission, children, fallback = null }) {
  const { hasPermission } = useAuthorization();
  return hasPermission(permission) ? children : fallback;
}

// Usage in components
function MurabahaContractPage({ contract }) {
  return (
    <div>
      <h1>Murabaha Contract #{contract.id}</h1>

      {/* Only managers and admins can approve */}
      <IfHasPermission permission={PERMISSIONS.APPROVE_CONTRACTS}>
        <button onClick={() => approveContract(contract.id)}>Approve Contract</button>
      </IfHasPermission>

      {/* Only admins can delete */}
      <IfHasRole role={ROLES.ADMIN}>
        <button onClick={() => deleteContract(contract.id)}>Delete Contract</button>
      </IfHasRole>
    </div>
  );
}
```

**CRITICAL**: UI-level authorization is for UX only. Always enforce authorization on the server.

## 🔐 Content Security Policy (CSP)

### Understanding CSP

**Content Security Policy (CSP)** is an HTTP header that helps prevent XSS, clickjacking, and other code injection attacks by specifying which content sources are trusted.

### CSP Configuration

**Recommended CSP for React apps**:

```javascript
// Server-side configuration (Express.js example)
const helmet = require("helmet");

app.use(
  helmet.contentSecurityPolicy({
    directives: {
      defaultSrc: ["'self'"],
      scriptSrc: [
        "'self'",
        "'nonce-{RANDOM_NONCE}'", // Replace per request
      ],
      styleSrc: ["'self'", "'nonce-{RANDOM_NONCE}'"],
      imgSrc: ["'self'", "data:", "https:"],
      fontSrc: ["'self'", "data:"],
      connectSrc: ["'self'", "https://api.example.com"],
      frameSrc: ["'none'"],
      objectSrc: ["'none'"],
      upgradeInsecureRequests: [],
    },
  }),
);
```

### CSP with React

**Using nonces for inline scripts**:

```javascript
// Server generates unique nonce per request
const crypto = require("crypto");

app.get("*", (req, res) => {
  const nonce = crypto.randomBytes(16).toString("base64");

  res.setHeader("Content-Security-Policy", `script-src 'self' 'nonce-${nonce}'; style-src 'self' 'nonce-${nonce}';`);

  // Inject nonce into HTML
  const html = renderToString(<App nonce={nonce} />);
  res.send(html);
});

// React component uses nonce
function App({ nonce }) {
  return (
    <html>
      <head>
        <script nonce={nonce} src="/app.js"></script>
        <style nonce={nonce}>{/* inline styles */}</style>
      </head>
      <body>
        <div id="root"></div>
      </body>
    </html>
  );
}
```

### CSP Reporting

**Monitor CSP violations**:

```javascript
app.use(
  helmet.contentSecurityPolicy({
    directives: {
      // ... other directives
      reportUri: "/api/csp-report",
    },
  }),
);

// Handle CSP violation reports
app.post("/api/csp-report", (req, res) => {
  console.error("CSP Violation:", req.body);

  // Log to monitoring service
  logger.error("CSP Violation", {
    violation: req.body,
    userAgent: req.headers["user-agent"],
    ip: req.ip,
  });

  res.status(204).end();
});
```

## 🔒 HTTPS Enforcement

### Strict Transport Security (HSTS)

**Force HTTPS connections**:

```javascript
// Server-side configuration
const helmet = require("helmet");

app.use(
  helmet.hsts({
    maxAge: 31536000, // 1 year in seconds
    includeSubDomains: true,
    preload: true,
  }),
);

// Redirect HTTP to HTTPS
app.use((req, res, next) => {
  if (req.headers["x-forwarded-proto"] !== "https") {
    return res.redirect(301, `https://${req.headers.host}${req.url}`);
  }
  next();
});
```

### Mixed Content Prevention

**Ensure all resources loaded over HTTPS**:

```javascript
// ✅ Correct - Relative URLs use same protocol as page
<img src="/images/logo.png" alt="Logo" />
<script src="/scripts/app.js"></script>

// ✅ Correct - Explicit HTTPS URLs
<img src="https://cdn.example.com/image.png" alt="Image" />

// ❌ Wrong - HTTP URLs on HTTPS page (mixed content)
<img src="http://example.com/image.png" alt="Image" />
```

**Upgrade insecure requests in CSP**:

```javascript
// Server-side CSP configuration
app.use(
  helmet.contentSecurityPolicy({
    directives: {
      upgradeInsecureRequests: [], // Upgrade HTTP to HTTPS automatically
    },
  }),
);
```

## 💾 Secure Storage

### Storage Security Comparison

**Storage Mechanisms**:

| Storage        | XSS Access | Expiration   | Capacity | Use Case                    |
| -------------- | ---------- | ------------ | -------- | --------------------------- |
| localStorage   | ✅ Yes     | Never        | ~5-10 MB | Non-sensitive, long-term    |
| sessionStorage | ✅ Yes     | Tab close    | ~5-10 MB | Non-sensitive, session-only |
| Cookies        | Depends    | Configurable | ~4 KB    | Authentication, tracking    |
| IndexedDB      | ✅ Yes     | Never        | Large    | Offline data, caching       |

### Secure Cookie Configuration

**httpOnly cookies prevent JavaScript access**:

```javascript
// Server-side configuration
app.use(
  session({
    secret: process.env.SESSION_SECRET,
    cookie: {
      httpOnly: true, // Prevent JavaScript access
      secure: true, // HTTPS only
      sameSite: "strict", // CSRF protection
      maxAge: 3600000, // 1 hour
      domain: ".example.com", // Subdomain access
      path: "/",
    },
  }),
);
```

### Avoid Sensitive Data in Client Storage

**CRITICAL**: Never store sensitive data in localStorage or sessionStorage.

```javascript
// ❌ WRONG - Sensitive data exposed to XSS
localStorage.setItem("creditCard", "1234-5678-9012-3456");
localStorage.setItem("password", "secretpassword");

// ✅ CORRECT - Only store non-sensitive data
localStorage.setItem("theme", "dark");
localStorage.setItem("language", "en");
```

### Encryption for Sensitive Client Data

**If you must store sensitive data client-side, encrypt it**:

```javascript
import CryptoJS from "crypto-js";

class SecureStorage {
  constructor(encryptionKey) {
    this.key = encryptionKey;
  }

  setItem(key, value) {
    const encrypted = CryptoJS.AES.encrypt(JSON.stringify(value), this.key).toString();

    sessionStorage.setItem(key, encrypted);
  }

  getItem(key) {
    const encrypted = sessionStorage.getItem(key);
    if (!encrypted) return null;

    try {
      const decrypted = CryptoJS.AES.decrypt(encrypted, this.key);
      return JSON.parse(decrypted.toString(CryptoJS.enc.Utf8));
    } catch {
      return null;
    }
  }

  removeItem(key) {
    sessionStorage.removeItem(key);
  }
}

// Usage
const secureStorage = new SecureStorage(process.env.REACT_APP_ENCRYPTION_KEY);
secureStorage.setItem("temporaryData", { userId: 123 });
```

**IMPORTANT**: This provides obfuscation, not true security. Server-side storage is always preferred.

## 🌐 API Security

### CORS Configuration

**Control which origins can access your API**:

```javascript
// Server-side CORS configuration
const cors = require("cors");

app.use(
  cors({
    origin: ["https://oseplatform.com", "https://www.oseplatform.com"],
    credentials: true, // Allow cookies
    methods: ["GET", "POST", "PUT", "DELETE"],
    allowedHeaders: ["Content-Type", "Authorization"],
  }),
);
```

**Client-side credentials handling**:

```javascript
// Include credentials (cookies) in cross-origin requests
fetch("https://api.example.com/data", {
  method: "GET",
  credentials: "include", // Include cookies
})
  .then((response) => response.json())
  .then((data) => console.log(data));

// Axios configuration
axios.defaults.withCredentials = true;
```

### API Key Security

**CRITICAL**: Never expose API keys in client-side code.

```javascript
// ❌ WRONG - API key exposed in client code
const API_KEY = "sk_live_1234567890abcdef";
fetch(`https://api.example.com/data?apiKey=${API_KEY}`);

// ✅ CORRECT - Use server-side proxy
// Client makes request to your backend
fetch("/api/proxy/data").then((response) => response.json());

// Backend proxies request with API key
app.get("/api/proxy/data", async (req, res) => {
  const response = await fetch("https://api.example.com/data", {
    headers: {
      Authorization: `Bearer ${process.env.API_KEY}`,
    },
  });

  const data = await response.json();
  res.json(data);
});
```

### Rate Limiting

**Protect against abuse with rate limiting**:

```javascript
// Server-side rate limiting
const rateLimit = require("express-rate-limit");

const limiter = rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 100, // Limit each IP to 100 requests per windowMs
  message: "Too many requests from this IP, please try again later.",
});

app.use("/api/", limiter);

// Stricter limits for authentication endpoints
const authLimiter = rateLimit({
  windowMs: 15 * 60 * 1000,
  max: 5, // Only 5 login attempts per 15 minutes
  skipSuccessfulRequests: true,
});

app.use("/api/auth/login", authLimiter);
```

## 📦 Dependency Security

### Regular Audits

**Run npm audit regularly**:

```bash
# Check for vulnerabilities
npm audit

# Fix automatically fixable vulnerabilities
npm audit fix

# Force fix (may include breaking changes)
npm audit fix --force
```

### Automated Scanning

**Integrate security scanning in CI/CD**:

```yaml
# GitHub Actions example
name: Security Audit

on: [push, pull_request]

jobs:
  audit:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions/setup-node@v3
        with:
          node-version: "20"
      - run: npm ci
      - run: npm audit --audit-level=high
```

### Snyk Integration

**Use Snyk for comprehensive vulnerability scanning**:

```bash
# Install Snyk CLI
npm install -g snyk

# Authenticate
snyk auth

# Test project for vulnerabilities
snyk test

# Monitor project continuously
snyk monitor
```

### Dependency Update Strategy

**Keep dependencies up to date**:

```bash
# Check outdated packages
npm outdated

# Update to latest versions within semver range
npm update

# Update to latest versions (may include breaking changes)
npm install <package>@latest
```

## ✅ Input Validation

### Client-Side Validation

**Validate all user inputs**:

```javascript
function DonationForm() {
  const [amount, setAmount] = useState("");
  const [email, setEmail] = useState("");
  const [errors, setErrors] = useState({});

  const validateEmail = (email) => {
    const re = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    return re.test(email);
  };

  const validateAmount = (amount) => {
    const num = parseFloat(amount);
    return !isNaN(num) && num > 0 && num <= 1000000;
  };

  const handleSubmit = (e) => {
    e.preventDefault();

    const newErrors = {};

    if (!validateEmail(email)) {
      newErrors.email = "Invalid email address";
    }

    if (!validateAmount(amount)) {
      newErrors.amount = "Amount must be between 0 and 1,000,000";
    }

    if (Object.keys(newErrors).length > 0) {
      setErrors(newErrors);
      return;
    }

    // Submit to server (server must also validate)
    submitDonation({ email, amount: parseFloat(amount) });
  };

  return (
    <form onSubmit={handleSubmit}>
      <div>
        <input type="email" value={email} onChange={(e) => setEmail(e.target.value)} placeholder="Email" />
        {errors.email && <span className="error">{errors.email}</span>}
      </div>

      <div>
        <input type="text" value={amount} onChange={(e) => setAmount(e.target.value)} placeholder="Amount" />
        {errors.amount && <span className="error">{errors.amount}</span>}
      </div>

      <button type="submit">Donate</button>
    </form>
  );
}
```

### Schema Validation

**Use validation libraries for complex schemas**:

```javascript
import * as Yup from "yup";

const murabahaContractSchema = Yup.object().shape({
  clientName: Yup.string()
    .required("Client name is required")
    .min(2, "Name must be at least 2 characters")
    .max(100, "Name must not exceed 100 characters"),

  contractAmount: Yup.number()
    .required("Contract amount is required")
    .positive("Amount must be positive")
    .max(10000000, "Amount exceeds maximum limit"),

  profitMargin: Yup.number()
    .required("Profit margin is required")
    .min(0, "Profit margin cannot be negative")
    .max(30, "Profit margin cannot exceed 30%"),

  duration: Yup.number()
    .required("Duration is required")
    .integer("Duration must be a whole number")
    .min(1, "Duration must be at least 1 month")
    .max(60, "Duration cannot exceed 60 months"),

  email: Yup.string().required("Email is required").email("Invalid email address"),
});

function MurabahaContractForm() {
  const handleSubmit = async (values) => {
    try {
      // Validate against schema
      await murabahaContractSchema.validate(values, { abortEarly: false });

      // Submit to server
      submitContract(values);
    } catch (err) {
      if (err instanceof Yup.ValidationError) {
        const errors = err.inner.reduce((acc, error) => {
          acc[error.path] = error.message;
          return acc;
        }, {});

        setFormErrors(errors);
      }
    }
  };

  return <form onSubmit={handleSubmit}>{/* form fields */}</form>;
}
```

### Server-Side Validation is Required

**CRITICAL**: Client-side validation is for UX only. Always validate on the server.

```javascript
// Server-side validation (Express.js example)
const { body, validationResult } = require("express-validator");

app.post(
  "/api/donations",
  [body("email").isEmail().normalizeEmail(), body("amount").isFloat({ min: 0.01, max: 1000000 })],
  (req, res) => {
    const errors = validationResult(req);

    if (!errors.isEmpty()) {
      return res.status(400).json({ errors: errors.array() });
    }

    // Process valid donation
    processDonation(req.body);
  },
);
```

## ⚠️ Error Handling

### Avoid Information Disclosure

**Never expose sensitive details in error messages**:

```javascript
// ❌ WRONG - Exposes database structure
function LoginForm() {
  const handleLogin = async (email, password) => {
    try {
      await login(email, password);
    } catch (error) {
      // Exposes server error details
      alert(`Login failed: ${error.message}`);
      // Example: "User with email 'user@example.com' not found in users table"
    }
  };
}

// ✅ CORRECT - Generic error message
function LoginForm() {
  const handleLogin = async (email, password) => {
    try {
      await login(email, password);
    } catch (error) {
      // Log detailed error for debugging
      console.error("Login error:", error);

      // Show generic message to user
      alert("Login failed. Please check your credentials and try again.");
    }
  };
}
```

### Secure Error Logging

**Log errors without exposing sensitive data**:

```javascript
function sanitizeErrorForLogging(error, context) {
  return {
    message: error.message,
    stack: error.stack,
    context: {
      // Include non-sensitive context
      url: context.url,
      method: context.method,
      timestamp: new Date().toISOString(),
      // Exclude sensitive data (passwords, tokens, etc.)
    },
  };
}

function SecureComponent() {
  const handleAction = async () => {
    try {
      await performSensitiveAction();
    } catch (error) {
      // Log sanitized error to monitoring service
      const sanitized = sanitizeErrorForLogging(error, {
        url: window.location.href,
        method: "performSensitiveAction",
      });

      logger.error(sanitized);

      // Show generic message to user
      toast.error("An error occurred. Please try again.");
    }
  };
}
```

## 🛡️ Security Headers

### Helmet.js Configuration

**Comprehensive security headers**:

```javascript
const helmet = require("helmet");

app.use(
  helmet({
    // Content Security Policy
    contentSecurityPolicy: {
      directives: {
        defaultSrc: ["'self'"],
        scriptSrc: ["'self'", "'nonce-{NONCE}'"],
        styleSrc: ["'self'", "'nonce-{NONCE}'"],
        imgSrc: ["'self'", "data:", "https:"],
        connectSrc: ["'self'", "https://api.example.com"],
        fontSrc: ["'self'"],
        objectSrc: ["'none'"],
        mediaSrc: ["'self'"],
        frameSrc: ["'none'"],
      },
    },

    // Strict Transport Security
    hsts: {
      maxAge: 31536000,
      includeSubDomains: true,
      preload: true,
    },

    // X-Frame-Options (clickjacking protection)
    frameguard: {
      action: "deny",
    },

    // X-Content-Type-Options
    noSniff: true,

    // X-XSS-Protection
    xssFilter: true,

    // Referrer-Policy
    referrerPolicy: {
      policy: "strict-origin-when-cross-origin",
    },

    // Permissions-Policy (formerly Feature-Policy)
    permissionsPolicy: {
      features: {
        geolocation: ["'none'"],
        microphone: ["'none'"],
        camera: ["'none'"],
        payment: ["'self'"],
      },
    },
  }),
);
```

## 🔍 Example: Secure Donation Form

**Comprehensive secure donation form**:

```javascript
import { useState } from "react";
import DOMPurify from "dompurify";
import * as Yup from "yup";

const donationSchema = Yup.object().shape({
  amount: Yup.number()
    .required("Amount is required")
    .positive("Amount must be positive")
    .max(1000000, "Amount exceeds maximum"),

  email: Yup.string().required("Email is required").email("Invalid email address"),

  message: Yup.string().max(500, "Message must not exceed 500 characters"),
});

function SecureDonationForm() {
  const [formData, setFormData] = useState({
    amount: "",
    email: "",
    message: "",
  });

  const [errors, setErrors] = useState({});
  const { accessToken } = useAuth();

  const handleChange = (e) => {
    const { name, value } = e.target;

    // Sanitize input
    const sanitizedValue = DOMPurify.sanitize(value, {
      ALLOWED_TAGS: [],
      ALLOWED_ATTR: [],
    });

    setFormData((prev) => ({
      ...prev,
      [name]: sanitizedValue,
    }));
  };

  const handleSubmit = async (e) => {
    e.preventDefault();
    setErrors({});

    try {
      // Client-side validation
      await donationSchema.validate(formData, { abortEarly: false });

      // Submit to server with CSRF and authentication
      const response = await fetch("/api/donations", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
          Authorization: `Bearer ${accessToken}`,
          "X-CSRF-Token": getCsrfToken(),
        },
        credentials: "include", // Include cookies
        body: JSON.stringify(formData),
      });

      if (!response.ok) {
        throw new Error("Donation failed");
      }

      const result = await response.json();

      // Success - redirect or show confirmation
      window.location.href = `/donations/${result.id}/confirmation`;
    } catch (err) {
      if (err instanceof Yup.ValidationError) {
        const validationErrors = err.inner.reduce((acc, error) => {
          acc[error.path] = error.message;
          return acc;
        }, {});

        setErrors(validationErrors);
      } else {
        // Log error securely
        console.error("Donation error:", err);

        // Show generic message to user
        setErrors({
          submit: "Donation failed. Please try again.",
        });
      }
    }
  };

  return (
    <form onSubmit={handleSubmit}>
      <div>
        <label htmlFor="amount">Amount ($)</label>
        <input
          id="amount"
          name="amount"
          type="text"
          value={formData.amount}
          onChange={handleChange}
          placeholder="Enter amount"
          autoComplete="off"
        />
        {errors.amount && <span className="error">{errors.amount}</span>}
      </div>

      <div>
        <label htmlFor="email">Email</label>
        <input
          id="email"
          name="email"
          type="email"
          value={formData.email}
          onChange={handleChange}
          placeholder="Enter email"
          autoComplete="email"
        />
        {errors.email && <span className="error">{errors.email}</span>}
      </div>

      <div>
        <label htmlFor="message">Message (optional)</label>
        <textarea
          id="message"
          name="message"
          value={formData.message}
          onChange={handleChange}
          placeholder="Add a message"
          maxLength={500}
        />
        {errors.message && <span className="error">{errors.message}</span>}
      </div>

      {errors.submit && <div className="error">{errors.submit}</div>}

      <button type="submit">Submit Donation</button>
    </form>
  );
}
```

## 🔒 Example: Protected Murabaha Contract Pages

**Comprehensive authorization for contract pages**:

```javascript
import { Navigate, useParams } from "react-router-dom";

function MurabahaContractPage() {
  const { contractId } = useParams();
  const { user, accessToken } = useAuth();
  const { hasPermission } = useAuthorization();

  const [contract, setContract] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);

  useEffect(() => {
    const fetchContract = async () => {
      try {
        const response = await fetch(`/api/murabaha/contracts/${contractId}`, {
          headers: {
            Authorization: `Bearer ${accessToken}`,
          },
          credentials: "include",
        });

        if (response.status === 401) {
          throw new Error("Unauthorized");
        }

        if (response.status === 403) {
          throw new Error("Forbidden");
        }

        if (!response.ok) {
          throw new Error("Failed to fetch contract");
        }

        const data = await response.json();
        setContract(data);
      } catch (err) {
        console.error("Contract fetch error:", err);
        setError(err.message);
      } finally {
        setLoading(false);
      }
    };

    fetchContract();
  }, [contractId, accessToken]);

  // Require authentication
  if (!user) {
    return <Navigate to="/login" replace />;
  }

  // Loading state
  if (loading) {
    return <LoadingSpinner />;
  }

  // Error handling
  if (error === "Unauthorized" || error === "Forbidden") {
    return <Navigate to="/unauthorized" replace />;
  }

  if (error) {
    return <ErrorMessage message="Failed to load contract" />;
  }

  // Authorization checks
  const canView = hasPermission(PERMISSIONS.VIEW_CONTRACTS);
  const canApprove = hasPermission(PERMISSIONS.APPROVE_CONTRACTS);
  const canDelete = hasPermission(PERMISSIONS.MANAGE_USERS); // Admin only

  if (!canView) {
    return <Navigate to="/unauthorized" replace />;
  }

  const handleApprove = async () => {
    try {
      const response = await fetch(`/api/murabaha/contracts/${contractId}/approve`, {
        method: "POST",
        headers: {
          Authorization: `Bearer ${accessToken}`,
          "X-CSRF-Token": getCsrfToken(),
        },
        credentials: "include",
      });

      if (!response.ok) {
        throw new Error("Approval failed");
      }

      // Refresh contract data
      window.location.reload();
    } catch (err) {
      console.error("Approval error:", err);
      alert("Failed to approve contract");
    }
  };

  const handleDelete = async () => {
    if (!window.confirm("Are you sure you want to delete this contract?")) {
      return;
    }

    try {
      const response = await fetch(`/api/murabaha/contracts/${contractId}`, {
        method: "DELETE",
        headers: {
          Authorization: `Bearer ${accessToken}`,
          "X-CSRF-Token": getCsrfToken(),
        },
        credentials: "include",
      });

      if (!response.ok) {
        throw new Error("Deletion failed");
      }

      // Redirect to contracts list
      window.location.href = "/contracts";
    } catch (err) {
      console.error("Deletion error:", err);
      alert("Failed to delete contract");
    }
  };

  return (
    <div>
      <h1>Murabaha Contract #{contract.id}</h1>

      <div>
        <p>
          <strong>Client:</strong> {contract.clientName}
        </p>
        <p>
          <strong>Amount:</strong> ${contract.amount.toLocaleString()}
        </p>
        <p>
          <strong>Profit Margin:</strong> {contract.profitMargin}%
        </p>
        <p>
          <strong>Duration:</strong> {contract.duration} months
        </p>
        <p>
          <strong>Status:</strong> {contract.status}
        </p>
      </div>

      <div>
        {canApprove && contract.status === "pending" && <button onClick={handleApprove}>Approve Contract</button>}

        {canDelete && (
          <button onClick={handleDelete} className="danger">
            Delete Contract
          </button>
        )}
      </div>
    </div>
  );
}
```

## 📚 Security Checklist

**Before deploying React applications**:

- [ ] All user inputs sanitized (DOMPurify for HTML)
- [ ] No use of `dangerouslySetInnerHTML` with unsanitized content
- [ ] CSRF protection implemented (tokens or SameSite cookies)
- [ ] Authentication uses httpOnly cookies + access/refresh tokens
- [ ] Authorization enforced on both client and server
- [ ] Protected routes implemented with route guards
- [ ] Content Security Policy configured with nonces
- [ ] HTTPS enforced with HSTS headers
- [ ] No sensitive data in localStorage/sessionStorage
- [ ] Secure cookies configured (httpOnly, secure, sameSite)
- [ ] CORS properly configured
- [ ] API keys not exposed in client code
- [ ] Rate limiting implemented on server
- [ ] Dependencies audited (`npm audit`)
- [ ] Client-side validation implemented (UX)
- [ ] Server-side validation enforced (security)
- [ ] Error messages don't expose sensitive information
- [ ] Security headers configured (Helmet.js)
- [ ] All code examples tested and verified

## 🔗 Related Documentation

- [React REST APIs](./ex-so-plwe-fere__rest-apis.md) - Consuming REST APIs securely
- [React Data Fetching](./ex-so-plwe-fere__data-fetching.md) - API integration patterns
- [React Configuration](./ex-so-plwe-fere__configuration.md) - Environment variables and secrets
- [React Deployment](./ex-so-plwe-fere__deployment.md) - Secure deployment practices

---

**Security is never complete.** Regularly review security practices, update dependencies, monitor for vulnerabilities, and stay informed about emerging threats.
