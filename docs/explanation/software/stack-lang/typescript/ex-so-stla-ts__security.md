---
title: "TypeScript Security"
description: Security best practices for TypeScript applications
category: explanation
subcategory: stack-lang
tags:
  - typescript
  - security
  - xss
  - sql-injection
  - csrf
  - authentication
  - authorization
  - owasp
related:
  - ./ex-so-stla-ts__best-practices.md
  - ./ex-so-stla-ts__web-services.md
  - ./ex-so-stla-ts__error-handling.md
principles:
  - explicit-over-implicit
  - automation-over-manual
last_updated: 2025-01-23
---

# TypeScript Security

**Quick Reference**: [Overview](#overview) | [Input Validation](#input-validation) | [Injection Prevention](#injection-prevention) | [XSS Prevention](#cross-site-scripting-xss-prevention) | [Authentication](#authentication) | [Authorization](#authorization) | [Cryptography](#cryptography) | [CSRF Protection](#csrf-protection) | [Security Headers](#security-headers) | [OWASP Top 10](#owasp-top-10-in-typescript) | [Related Documentation](#related-documentation)

## Overview

Security is critical for financial applications. This guide covers OWASP Top 10 vulnerabilities and how to prevent them in TypeScript applications handling donations, Zakat calculations, and financial transactions.

### Security Principles

- **Defense in Depth**: Multiple layers of security
- **Least Privilege**: Minimal access rights
- **Fail Securely**: Default deny, explicit allow
- **Complete Mediation**: Check every access
- **Input Validation**: Never trust user input
- **Output Encoding**: Prevent injection attacks

## Input Validation

### Schema Validation with Zod

```typescript
import { z } from "zod";

// Donation input schema
const donationInputSchema = z.object({
  donorId: z.string().regex(/^DNR-\d{10}$/, "Invalid donor ID format"),
  amount: z.number().positive("Amount must be positive").max(1000000, "Amount exceeds maximum limit"),
  currency: z.enum(["USD", "EUR", "SAR"], {
    errorMap: () => ({ message: "Invalid currency" }),
  }),
  category: z.enum(["zakat", "sadaqah", "waqf"]),
  message: z.string().max(500, "Message too long").optional(),
  email: z.string().email("Invalid email format").optional(),
});

type DonationInput = z.infer<typeof donationInputSchema>;

// Validate input
function validateDonation(input: unknown): DonationInput {
  return donationInputSchema.parse(input);
}

// Safe usage
try {
  const validatedInput = validateDonation(userInput);
  // Process validatedInput safely
} catch (error) {
  if (error instanceof z.ZodError) {
    console.error("Validation errors:", error.errors);
  }
}
```

### Sanitization

```typescript
import DOMPurify from "isomorphic-dompurify";
import validator from "validator";

// Sanitize HTML input
function sanitizeHtml(input: string): string {
  return DOMPurify.sanitize(input, {
    ALLOWED_TAGS: ["b", "i", "em", "strong", "p"],
    ALLOWED_ATTR: [],
  });
}

// Sanitize and validate email
function sanitizeEmail(input: string): string | null {
  const trimmed = validator.trim(input);
  const normalized = validator.normalizeEmail(trimmed);

  if (!normalized || !validator.isEmail(normalized)) {
    return null;
  }

  return normalized;
}

// Sanitize user message
function sanitizeMessage(input: string): string {
  // Remove control characters
  let sanitized = input.replace(/[\x00-\x1F\x7F]/g, "");

  // Trim whitespace
  sanitized = validator.trim(sanitized);

  // Escape HTML
  sanitized = validator.escape(sanitized);

  return sanitized;
}

// Example usage
const userMessage = sanitizeMessage(req.body.message);
const userEmail = sanitizeEmail(req.body.email);
```

### Type Guards for Runtime Validation

```typescript
// Type guard for DonorId
function isDonorId(value: unknown): value is DonorId {
  return typeof value === "string" && /^DNR-\d{10}$/.test(value);
}

// Type guard for positive number
function isPositiveNumber(value: unknown): value is number {
  return typeof value === "number" && value > 0 && isFinite(value);
}

// Type guard for valid currency
const VALID_CURRENCIES = ["USD", "EUR", "SAR"] as const;
type Currency = (typeof VALID_CURRENCIES)[number];

function isCurrency(value: unknown): value is Currency {
  return typeof value === "string" && VALID_CURRENCIES.includes(value as any);
}

// Safe processing with type guards
function processDonation(input: unknown) {
  if (typeof input !== "object" || input === null || !("amount" in input) || !("currency" in input)) {
    throw new Error("Invalid donation input");
  }

  const { amount, currency } = input as any;

  if (!isPositiveNumber(amount)) {
    throw new Error("Invalid amount");
  }

  if (!isCurrency(currency)) {
    throw new Error("Invalid currency");
  }

  // Safe to use amount and currency
  return { amount, currency };
}
```

## Injection Prevention

### SQL Injection Prevention

```typescript
import { Pool } from "pg";

const pool = new Pool({
  connectionString: process.env.DATABASE_URL,
});

// ❌ UNSAFE: SQL injection vulnerability
async function getDonationsUnsafe(donorId: string) {
  const query = `SELECT * FROM donations WHERE donor_id = '${donorId}'`;
  const result = await pool.query(query);
  return result.rows;
}

// ✅ SAFE: Parameterized query
async function getDonationsSafe(donorId: string) {
  const query = "SELECT * FROM donations WHERE donor_id = $1";
  const result = await pool.query(query, [donorId]);
  return result.rows;
}

// ✅ SAFE: Using query builder (Prisma)
import { PrismaClient } from "@prisma/client";

const prisma = new PrismaClient();

async function getDonationsWithPrisma(donorId: string) {
  return prisma.donation.findMany({
    where: { donorId },
  });
}
```

### NoSQL Injection Prevention

```typescript
import { MongoClient, ObjectId } from "mongodb";

// ❌ UNSAFE: NoSQL injection
async function getDonorUnsafe(donorId: string) {
  const query = { donorId: JSON.parse(donorId) };
  return db.collection("donors").findOne(query);
}

// ✅ SAFE: Validate input type
async function getDonorSafe(donorId: string) {
  if (typeof donorId !== "string" || !isDonorId(donorId)) {
    throw new Error("Invalid donor ID");
  }

  return db.collection("donors").findOne({ donorId });
}

// ✅ SAFE: Use ObjectId validation
async function getDonorByObjectId(id: string) {
  if (!ObjectId.isValid(id)) {
    throw new Error("Invalid ObjectId");
  }

  return db.collection("donors").findOne({
    _id: new ObjectId(id),
  });
}
```

### Command Injection Prevention

```typescript
import { exec } from "child_process";
import { promisify } from "util";

const execAsync = promisify(exec);

// ❌ UNSAFE: Command injection
async function generateReceiptUnsafe(donationId: string) {
  const command = `pdftk donation-${donationId}.pdf output receipt.pdf`;
  await execAsync(command);
}

// ✅ SAFE: Use libraries instead of shell commands
import PDFDocument from "pdfkit";
import fs from "fs";

async function generateReceiptSafe(donationId: string) {
  const doc = new PDFDocument();
  const stream = fs.createWriteStream(`receipt-${donationId}.pdf`);

  doc.pipe(stream);
  doc.text(`Donation Receipt: ${donationId}`);
  doc.end();

  return new Promise((resolve, reject) => {
    stream.on("finish", resolve);
    stream.on("error", reject);
  });
}
```

## Cross-Site Scripting (XSS) Prevention

### Output Encoding

```typescript
import he from "he";

// HTML entity encoding
function encodeHtml(text: string): string {
  return he.encode(text, {
    useNamedReferences: true,
  });
}

// JavaScript encoding
function encodeJavaScript(text: string): string {
  return text
    .replace(/\\/g, "\\\\")
    .replace(/'/g, "\\'")
    .replace(/"/g, '\\"')
    .replace(/</g, "\\x3C")
    .replace(/>/g, "\\x3E")
    .replace(/\//g, "\\/")
    .replace(/\n/g, "\\n")
    .replace(/\r/g, "\\r");
}

// URL encoding
function encodeUrl(text: string): string {
  return encodeURIComponent(text);
}

// Example: Rendering donation message
function renderDonationMessage(donation: { message: string; donorName: string }) {
  const safeMessage = encodeHtml(donation.message);
  const safeName = encodeHtml(donation.donorName);

  return `<div class="donation-message">
    <p><strong>${safeName}</strong> says:</p>
    <p>${safeMessage}</p>
  </div>`;
}
```

### Content Security Policy (CSP)

```typescript
import helmet from "helmet";
import express from "express";

const app = express();

app.use(
  helmet.contentSecurityPolicy({
    directives: {
      defaultSrc: ["'self'"],
      scriptSrc: ["'self'", "'sha256-...'"],
      styleSrc: ["'self'", "'unsafe-inline'"],
      imgSrc: ["'self'", "data:", "https:"],
      connectSrc: ["'self'", "https://api.example.com"],
      fontSrc: ["'self'"],
      objectSrc: ["'none'"],
      mediaSrc: ["'self'"],
      frameSrc: ["'none'"],
      upgradeInsecureRequests: [],
    },
  }),
);
```

### React XSS Prevention

```typescript
import React from "react";
import DOMPurify from "isomorphic-dompurify";

interface DonationMessageProps {
  message: string;
  donorName: string;
}

// ✅ SAFE: React automatically escapes
function DonationMessage({ message, donorName }: DonationMessageProps) {
  return (
    <div className="donation-message">
      <p>
        <strong>{donorName}</strong> says:
      </p>
      <p>{message}</p>
    </div>
  );
}

// If you must render HTML, sanitize first
function DonationMessageWithHtml({ html }: { html: string }) {
  const sanitized = DOMPurify.sanitize(html);

  return (
    <div
      className="donation-message"
      dangerouslySetInnerHTML={{ __html: sanitized }}
    />
  );
}
```

## Authentication

### JWT Authentication

```typescript
import jwt from "jsonwebtoken";
import bcrypt from "bcrypt";

const JWT_SECRET = process.env.JWT_SECRET || "your-secret-key";
const SALT_ROUNDS = 12;

interface TokenPayload {
  userId: string;
  role: "donor" | "admin" | "beneficiary";
  iat: number;
  exp: number;
}

// Hash password
async function hashPassword(password: string): Promise<string> {
  return bcrypt.hash(password, SALT_ROUNDS);
}

// Verify password
async function verifyPassword(password: string, hash: string): Promise<boolean> {
  return bcrypt.compare(password, hash);
}

// Generate JWT
function generateToken(userId: string, role: string): string {
  const payload = {
    userId,
    role,
  };

  return jwt.sign(payload, JWT_SECRET, {
    expiresIn: "1h",
    algorithm: "HS256",
  });
}

// Verify JWT
function verifyToken(token: string): TokenPayload {
  try {
    return jwt.verify(token, JWT_SECRET) as TokenPayload;
  } catch (error) {
    throw new Error("Invalid token");
  }
}

// Middleware
import { Request, Response, NextFunction } from "express";

declare global {
  namespace Express {
    interface Request {
      user?: TokenPayload;
    }
  }
}

function authenticateToken(req: Request, res: Response, next: NextFunction): void {
  const authHeader = req.headers.authorization;
  const token = authHeader?.split(" ")[1];

  if (!token) {
    res.status(401).json({ error: "No token provided" });
    return;
  }

  try {
    const user = verifyToken(token);
    req.user = user;
    next();
  } catch (error) {
    res.status(403).json({ error: "Invalid token" });
  }
}
```

### Session-Based Authentication

```typescript
import session from "express-session";
import RedisStore from "connect-redis";
import { createClient } from "redis";

const redisClient = createClient({
  url: process.env.REDIS_URL,
});

redisClient.connect();

const sessionMiddleware = session({
  store: new RedisStore({ client: redisClient }),
  secret: process.env.SESSION_SECRET || "session-secret",
  resave: false,
  saveUninitialized: false,
  cookie: {
    secure: process.env.NODE_ENV === "production",
    httpOnly: true,
    maxAge: 1000 * 60 * 60 * 24, // 24 hours
    sameSite: "strict",
  },
});

app.use(sessionMiddleware);

// Login endpoint
app.post("/api/auth/login", async (req, res) => {
  const { email, password } = req.body;

  // Find user
  const user = await findUserByEmail(email);
  if (!user) {
    res.status(401).json({ error: "Invalid credentials" });
    return;
  }

  // Verify password
  const isValid = await verifyPassword(password, user.passwordHash);
  if (!isValid) {
    res.status(401).json({ error: "Invalid credentials" });
    return;
  }

  // Create session
  req.session.userId = user.id;
  req.session.role = user.role;

  res.json({ message: "Login successful" });
});

// Logout endpoint
app.post("/api/auth/logout", (req, res) => {
  req.session.destroy((err) => {
    if (err) {
      res.status(500).json({ error: "Logout failed" });
      return;
    }
    res.json({ message: "Logout successful" });
  });
});
```

### OAuth 2.0 / OIDC

```typescript
import passport from "passport";
import { Strategy as OAuth2Strategy } from "passport-oauth2";

passport.use(
  new OAuth2Strategy(
    {
      authorizationURL: "https://provider.com/oauth2/authorize",
      tokenURL: "https://provider.com/oauth2/token",
      clientID: process.env.OAUTH_CLIENT_ID!,
      clientSecret: process.env.OAUTH_CLIENT_SECRET!,
      callbackURL: "https://yourapp.com/auth/callback",
    },
    async (accessToken, refreshToken, profile, done) => {
      try {
        // Find or create user
        const user = await findOrCreateUser(profile);
        done(null, user);
      } catch (error) {
        done(error);
      }
    },
  ),
);

app.get("/auth/oauth", passport.authenticate("oauth2"));

app.get(
  "/auth/callback",
  passport.authenticate("oauth2", {
    failureRedirect: "/login",
  }),
  (req, res) => {
    res.redirect("/dashboard");
  },
);
```

## Authorization

### Role-Based Access Control (RBAC)

```typescript
type Role = "donor" | "admin" | "beneficiary";

type Permission =
  | "donation:create"
  | "donation:view"
  | "donation:delete"
  | "campaign:create"
  | "campaign:edit"
  | "user:manage";

const rolePermissions: Record<Role, Permission[]> = {
  donor: ["donation:create", "donation:view"],
  admin: ["donation:create", "donation:view", "donation:delete", "campaign:create", "campaign:edit", "user:manage"],
  beneficiary: ["donation:view", "campaign:create"],
};

function hasPermission(role: Role, permission: Permission): boolean {
  return rolePermissions[role].includes(permission);
}

// Middleware
function requirePermission(permission: Permission) {
  return (req: Request, res: Response, next: NextFunction): void => {
    if (!req.user) {
      res.status(401).json({ error: "Unauthorized" });
      return;
    }

    if (!hasPermission(req.user.role as Role, permission)) {
      res.status(403).json({ error: "Forbidden" });
      return;
    }

    next();
  };
}

// Usage
app.delete("/api/donations/:id", authenticateToken, requirePermission("donation:delete"), async (req, res) => {
  // Delete donation
});
```

### Attribute-Based Access Control (ABAC)

```typescript
interface AccessContext {
  user: {
    id: string;
    role: Role;
    organizationId: string;
  };
  resource: {
    type: "donation" | "campaign";
    ownerId: string;
    organizationId: string;
  };
  action: "create" | "read" | "update" | "delete";
}

function canAccess(context: AccessContext): boolean {
  // Admin can do everything
  if (context.user.role === "admin") {
    return true;
  }

  // Users can only access resources in their organization
  if (context.user.organizationId !== context.resource.organizationId) {
    return false;
  }

  // Users can read any resource in their organization
  if (context.action === "read") {
    return true;
  }

  // Users can only modify their own resources
  if (["update", "delete"].includes(context.action) && context.user.id === context.resource.ownerId) {
    return true;
  }

  return false;
}

// Middleware
async function checkAccess(req: Request, res: Response, next: NextFunction): Promise<void> {
  const donationId = req.params.id;
  const donation = await getDonationById(donationId);

  if (!donation) {
    res.status(404).json({ error: "Not found" });
    return;
  }

  const context: AccessContext = {
    user: {
      id: req.user!.userId,
      role: req.user!.role as Role,
      organizationId: "ORG-001",
    },
    resource: {
      type: "donation",
      ownerId: donation.donorId,
      organizationId: donation.organizationId,
    },
    action: "update",
  };

  if (!canAccess(context)) {
    res.status(403).json({ error: "Forbidden" });
    return;
  }

  next();
}
```

## Cryptography

### Hashing

```typescript
import crypto from "crypto";

// SHA-256 hash
function hash256(data: string): string {
  return crypto.createHash("sha256").update(data).digest("hex");
}

// HMAC for message authentication
function createHmac(message: string, secret: string): string {
  return crypto.createHmac("sha256", secret).update(message).digest("hex");
}

function verifyHmac(message: string, signature: string, secret: string): boolean {
  const expected = createHmac(message, secret);
  return crypto.timingSafeEqual(Buffer.from(signature), Buffer.from(expected));
}

// Generate secure random token
function generateToken(length: number = 32): string {
  return crypto.randomBytes(length).toString("hex");
}
```

### Encryption

```typescript
import crypto from "crypto";

const ALGORITHM = "aes-256-gcm";
const KEY_LENGTH = 32;
const IV_LENGTH = 12;
const TAG_LENGTH = 16;

// Derive encryption key from password
function deriveKey(password: string, salt: Buffer): Buffer {
  return crypto.pbkdf2Sync(password, salt, 100000, KEY_LENGTH, "sha256");
}

// Encrypt data
function encrypt(plaintext: string, password: string): string {
  const salt = crypto.randomBytes(16);
  const key = deriveKey(password, salt);
  const iv = crypto.randomBytes(IV_LENGTH);

  const cipher = crypto.createCipheriv(ALGORITHM, key, iv, {
    authTagLength: TAG_LENGTH,
  });

  let ciphertext = cipher.update(plaintext, "utf8", "hex");
  ciphertext += cipher.final("hex");

  const authTag = cipher.getAuthTag();

  // Format: salt:iv:authTag:ciphertext
  return [salt.toString("hex"), iv.toString("hex"), authTag.toString("hex"), ciphertext].join(":");
}

// Decrypt data
function decrypt(encrypted: string, password: string): string {
  const [saltHex, ivHex, authTagHex, ciphertext] = encrypted.split(":");

  const salt = Buffer.from(saltHex, "hex");
  const iv = Buffer.from(ivHex, "hex");
  const authTag = Buffer.from(authTagHex, "hex");
  const key = deriveKey(password, salt);

  const decipher = crypto.createDecipheriv(ALGORITHM, key, iv, {
    authTagLength: TAG_LENGTH,
  });

  decipher.setAuthTag(authTag);

  let plaintext = decipher.update(ciphertext, "hex", "utf8");
  plaintext += decipher.final("utf8");

  return plaintext;
}

// Example: Encrypt sensitive donation data
const sensitiveData = JSON.stringify({
  donorName: "Ahmad Ibrahim",
  amount: 10000,
  bankAccount: "1234567890",
});

const encryptionKey = process.env.ENCRYPTION_KEY!;
const encrypted = encrypt(sensitiveData, encryptionKey);
const decrypted = decrypt(encrypted, encryptionKey);
```

## CSRF Protection

### CSRF Token Implementation

```typescript
import csrf from "csurf";
import cookieParser from "cookie-parser";

const csrfProtection = csrf({
  cookie: {
    httpOnly: true,
    secure: process.env.NODE_ENV === "production",
    sameSite: "strict",
  },
});

app.use(cookieParser());

// Serve CSRF token
app.get("/api/csrf-token", csrfProtection, (req, res) => {
  res.json({ csrfToken: req.csrfToken() });
});

// Protect state-changing endpoints
app.post("/api/donations", csrfProtection, async (req, res) => {
  // Create donation
});

// Client-side usage
async function createDonation(data: any) {
  // Get CSRF token
  const tokenResponse = await fetch("/api/csrf-token");
  const { csrfToken } = await tokenResponse.json();

  // Make request with token
  const response = await fetch("/api/donations", {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      "CSRF-Token": csrfToken,
    },
    body: JSON.stringify(data),
  });

  return response.json();
}
```

## Security Headers

### Helmet Configuration

```typescript
import helmet from "helmet";

app.use(
  helmet({
    contentSecurityPolicy: {
      directives: {
        defaultSrc: ["'self'"],
        styleSrc: ["'self'", "'unsafe-inline'"],
        scriptSrc: ["'self'"],
        imgSrc: ["'self'", "data:", "https:"],
      },
    },
    hsts: {
      maxAge: 31536000,
      includeSubDomains: true,
      preload: true,
    },
    frameguard: {
      action: "deny",
    },
    noSniff: true,
    xssFilter: true,
    referrerPolicy: {
      policy: "strict-origin-when-cross-origin",
    },
  }),
);
```

### CORS Configuration

```typescript
import cors from "cors";

app.use(
  cors({
    origin: ["https://yourapp.com", "https://admin.yourapp.com"],
    methods: ["GET", "POST", "PUT", "DELETE"],
    allowedHeaders: ["Content-Type", "Authorization"],
    credentials: true,
    maxAge: 86400,
  }),
);
```

## OWASP Top 10 in TypeScript

### A01: Broken Access Control

```typescript
// ✅ SAFE: Check ownership before access
app.get("/api/donations/:id", authenticateToken, async (req, res) => {
  const donation = await getDonationById(req.params.id);

  if (!donation) {
    return res.status(404).json({ error: "Not found" });
  }

  // Verify user owns this donation
  if (donation.donorId !== req.user!.userId && req.user!.role !== "admin") {
    return res.status(403).json({ error: "Forbidden" });
  }

  res.json(donation);
});
```

### A02: Cryptographic Failures

```typescript
// ✅ SAFE: Use strong encryption
import crypto from "crypto";

// Store passwords with bcrypt (12+ rounds)
import bcrypt from "bcrypt";
const hash = await bcrypt.hash(password, 12);

// Use secure random for tokens
const token = crypto.randomBytes(32).toString("hex");

// Encrypt sensitive data
const encryptedData = encrypt(sensitiveInfo, encryptionKey);
```

### A03: Injection

```typescript
// ✅ SAFE: Use parameterized queries
const result = await pool.query("SELECT * FROM donations WHERE donor_id = $1", [donorId]);

// ✅ SAFE: Validate input
const validatedInput = donationSchema.parse(userInput);

// ✅ SAFE: Use ORM
const donations = await prisma.donation.findMany({
  where: { donorId },
});
```

### A04: Insecure Design

```typescript
// ✅ SAFE: Rate limiting for sensitive operations
import rateLimit from "express-rate-limit";

const passwordResetLimiter = rateLimit({
  windowMs: 60 * 60 * 1000, // 1 hour
  max: 3, // 3 requests per hour
  message: "Too many password reset attempts",
});

app.post("/api/auth/reset-password", passwordResetLimiter, async (req, res) => {
  // Handle password reset
});

// ✅ SAFE: Implement account lockout
let failedAttempts = 0;
const MAX_ATTEMPTS = 5;

async function attemptLogin(email: string, password: string) {
  if (failedAttempts >= MAX_ATTEMPTS) {
    throw new Error("Account locked. Try again later.");
  }

  const user = await findUserByEmail(email);
  const valid = await verifyPassword(password, user.passwordHash);

  if (!valid) {
    failedAttempts++;
    throw new Error("Invalid credentials");
  }

  failedAttempts = 0;
  return user;
}
```

### A05: Security Misconfiguration

```typescript
// ✅ SAFE: Environment-based configuration
const config = {
  port: process.env.PORT || 3000,
  nodeEnv: process.env.NODE_ENV || "development",
  database: {
    url: process.env.DATABASE_URL,
    ssl: process.env.NODE_ENV === "production",
  },
  jwt: {
    secret: process.env.JWT_SECRET!,
    expiresIn: "1h",
  },
  cors: {
    origin: process.env.ALLOWED_ORIGINS?.split(",") || ["http://localhost:3000"],
  },
};

// ✅ SAFE: Disable debug in production
if (config.nodeEnv === "production") {
  app.set("view cache", true);
  app.set("trust proxy", 1);
}

// ✅ SAFE: Remove sensitive headers
app.disable("x-powered-by");
```

### A06: Vulnerable Components

```typescript
// ✅ SAFE: Regular dependency updates
// package.json
{
  "scripts": {
    "audit": "npm audit",
    "audit:fix": "npm audit fix",
    "outdated": "npm outdated"
  }
}

// ✅ SAFE: Use npm audit in CI/CD
// .github/workflows/security.yml
// - run: npm audit --audit-level=moderate
```

### A07: Identification and Authentication Failures

```typescript
// ✅ SAFE: Strong password requirements
import zxcvbn from "zxcvbn";

function validatePasswordStrength(password: string): boolean {
  const result = zxcvbn(password);

  // Require score of 3 or higher (out of 4)
  if (result.score < 3) {
    throw new Error(`Weak password. ${result.feedback.warning}. ${result.feedback.suggestions.join(" ")}`);
  }

  return true;
}

// ✅ SAFE: Multi-factor authentication
import speakeasy from "speakeasy";

function generateTOTP() {
  const secret = speakeasy.generateSecret({
    name: "Donation Platform",
  });

  return {
    secret: secret.base32,
    qrCode: secret.otpauth_url,
  };
}

function verifyTOTP(token: string, secret: string): boolean {
  return speakeasy.totp.verify({
    secret,
    encoding: "base32",
    token,
    window: 2,
  });
}
```

### A08: Software and Data Integrity Failures

```typescript
// ✅ SAFE: Verify package integrity
// package-lock.json contains integrity hashes

// ✅ SAFE: Subresource Integrity (SRI)
// <script src="https://cdn.example.com/lib.js"
//         integrity="sha384-hash"
//         crossorigin="anonymous"></script>

// ✅ SAFE: Code signing
// Sign deployments and verify signatures
```

### A09: Security Logging Failures

```typescript
import winston from "winston";

const logger = winston.createLogger({
  level: "info",
  format: winston.format.json(),
  transports: [
    new winston.transports.File({ filename: "error.log", level: "error" }),
    new winston.transports.File({ filename: "combined.log" }),
  ],
});

// Log security events
function logSecurityEvent(event: string, details: any) {
  logger.info({
    type: "security",
    event,
    timestamp: new Date().toISOString(),
    ...details,
  });
}

// Examples
app.post("/api/auth/login", async (req, res) => {
  const { email, password } = req.body;

  try {
    const user = await attemptLogin(email, password);

    logSecurityEvent("login_success", {
      userId: user.id,
      ip: req.ip,
    });

    res.json({ token: generateToken(user.id, user.role) });
  } catch (error) {
    logSecurityEvent("login_failure", {
      email,
      ip: req.ip,
      reason: error.message,
    });

    res.status(401).json({ error: "Invalid credentials" });
  }
});
```

### A10: Server-Side Request Forgery (SSRF)

```typescript
import validator from "validator";

// ✅ SAFE: Validate and restrict URLs
const ALLOWED_DOMAINS = ["api.example.com", "cdn.example.com"];

function isAllowedUrl(url: string): boolean {
  try {
    const parsed = new URL(url);

    // Only allow HTTPS
    if (parsed.protocol !== "https:") {
      return false;
    }

    // Check domain whitelist
    if (!ALLOWED_DOMAINS.includes(parsed.hostname)) {
      return false;
    }

    // Block private IPs
    const ip = parsed.hostname;
    if (validator.isIP(ip) && (ip.startsWith("10.") || ip.startsWith("192.168.") || ip.startsWith("172."))) {
      return false;
    }

    return true;
  } catch {
    return false;
  }
}

// Safe external request
async function fetchDonationReceipt(receiptUrl: string) {
  if (!isAllowedUrl(receiptUrl)) {
    throw new Error("Invalid URL");
  }

  const response = await fetch(receiptUrl);
  return response.blob();
}
```

## Related Documentation

- **[TypeScript Best Practices](./ex-so-stla-ts__best-practices.md)** - Coding standards
- **[TypeScript Web Services](./ex-so-stla-ts__web-services.md)** - API development
- **[TypeScript Error Handling](./ex-so-stla-ts__error-handling.md)** - Error patterns

---

**Last Updated**: 2025-01-23
**TypeScript Version**: 5.0+ (baseline), 5.4+ (milestone), 5.6+ (stable), 5.7.3+ (latest stable)
**Maintainers**: OSE Documentation Team
