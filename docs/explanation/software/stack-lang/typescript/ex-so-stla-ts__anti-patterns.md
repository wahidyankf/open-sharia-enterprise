---
title: "TypeScript Anti-Patterns"
description: Common TypeScript mistakes and how to avoid them
category: explanation
subcategory: stack-lang
tags:
  - typescript
  - anti-patterns
  - code-smell
  - best-practices
related:
  - ./ex-so-stla-ts__best-practices.md
  - ./ex-so-stla-ts__type-safety.md
  - ./ex-so-stla-ts__error-handling.md
principles:
  - explicit-over-implicit
  - simplicity-over-complexity
last_updated: 2025-01-23
---

# TypeScript Anti-Patterns

**Quick Reference**: [Overview](#overview) | [Type Safety Anti-Patterns](#type-safety-anti-patterns) | [Error Handling Anti-Patterns](#error-handling-anti-patterns) | [Async Anti-Patterns](#async-anti-patterns) | [Performance Anti-Patterns](#performance-anti-patterns) | [Design Anti-Patterns](#design-anti-patterns) | [Security Anti-Patterns](#security-anti-patterns) | [Related Documentation](#related-documentation)

## Overview

Anti-patterns are common solutions that appear beneficial but lead to problematic code. This guide covers TypeScript anti-patterns in financial applications and their recommended alternatives.

## Type Safety Anti-Patterns

### Using `any`

```typescript
// ❌ ANTI-PATTERN: Defeats TypeScript's purpose
function processDonation(data: any) {
  return data.amount * 0.025; // No type safety
}

// ✅ SOLUTION: Use proper types
interface Donation {
  donationId: string;
  amount: number;
  currency: string;
}

function processDonationSafe(data: Donation): number {
  return data.amount * 0.025;
}
```

### Type Assertions Instead of Validation

```typescript
// ❌ ANTI-PATTERN: Assumes type without validation
const donation = JSON.parse(input) as Donation;

// ✅ SOLUTION: Validate runtime data
import { z } from "zod";

const donationSchema = z.object({
  donationId: z.string(),
  amount: z.number().positive(),
  currency: z.string(),
});

const donation = donationSchema.parse(JSON.parse(input));
```

### Non-Null Assertions

```typescript
// ❌ ANTI-PATTERN: Assumes value exists
function getDonorName(donorId: string): string {
  const donor = findDonor(donorId);
  return donor!.name; // Dangerous!
}

// ✅ SOLUTION: Handle null explicitly
function getDonorNameSafe(donorId: string): string | null {
  const donor = findDonor(donorId);
  return donor ? donor.name : null;
}

// ✅ BETTER: Use Result pattern
function getDonorNameResult(donorId: string): Result<string, Error> {
  const donor = findDonor(donorId);
  if (!donor) {
    return err(new Error("Donor not found"));
  }
  return ok(donor.name);
}
```

### Overly Broad Types

```typescript
// ❌ ANTI-PATTERN: Too permissive
interface DonationFilters {
  [key: string]: any;
}

// ✅ SOLUTION: Explicit properties
interface DonationFilters {
  category?: "zakat" | "sadaqah" | "waqf";
  minAmount?: number;
  maxAmount?: number;
  donorId?: string;
  dateFrom?: Date;
  dateTo?: Date;
}
```

## Error Handling Anti-Patterns

### Silent Failures

```typescript
// ❌ ANTI-PATTERN: Swallow errors
async function saveDonation(donation: Donation) {
  try {
    await db.save(donation);
  } catch (error) {
    // Silent failure
  }
}

// ✅ SOLUTION: Handle or propagate errors
async function saveDonationSafe(donation: Donation): Promise<Result<void, Error>> {
  try {
    await db.save(donation);
    return ok(undefined);
  } catch (error) {
    return err(error as Error);
  }
}
```

### Generic Error Messages

```typescript
// ❌ ANTI-PATTERN: Uninformative errors
throw new Error("Something went wrong");

// ✅ SOLUTION: Specific error messages
class DonationValidationError extends Error {
  constructor(
    public field: string,
    message: string,
  ) {
    super(`Invalid ${field}: ${message}`);
    this.name = "DonationValidationError";
  }
}

if (donation.amount <= 0) {
  throw new DonationValidationError("amount", "Must be positive");
}
```

### Try-Catch Everywhere

```typescript
// ❌ ANTI-PATTERN: Defensive try-catch
function calculateZakat(wealth: number): number {
  try {
    try {
      try {
        if (wealth < 0) throw new Error("Negative wealth");
        return wealth * 0.025;
      } catch (e1) {
        throw e1;
      }
    } catch (e2) {
      throw e2;
    }
  } catch (e3) {
    throw e3;
  }
}

// ✅ SOLUTION: Single try-catch at boundary
function calculateZakatSafe(wealth: number): number {
  if (wealth < 0) {
    throw new Error("Wealth cannot be negative");
  }
  return wealth * 0.025;
}

// Call site handles errors
try {
  const zakat = calculateZakatSafe(wealth);
} catch (error) {
  handleError(error);
}
```

## Async Anti-Patterns

### Unnecessary Await

```typescript
// ❌ ANTI-PATTERN: Unnecessary await
async function getDonation(id: string) {
  return await fetchDonation(id);
}

// ✅ SOLUTION: Return promise directly
async function getDonationBetter(id: string) {
  return fetchDonation(id);
}

// Only await when you need the value
async function processDonation(id: string) {
  const donation = await fetchDonation(id);
  const validated = validateDonation(donation);
  return validated;
}
```

### Sequential Instead of Parallel

```typescript
// ❌ ANTI-PATTERN: Sequential when independent
async function getDashboardData() {
  const donations = await fetchDonations();
  const donors = await fetchDonors();
  const campaigns = await fetchCampaigns();
  return { donations, donors, campaigns };
}

// ✅ SOLUTION: Parallel execution
async function getDashboardDataParallel() {
  const [donations, donors, campaigns] = await Promise.all([fetchDonations(), fetchDonors(), fetchCampaigns()]);
  return { donations, donors, campaigns };
}
```

### Async Function Not Awaited

```typescript
// ❌ ANTI-PATTERN: Fire and forget
function processDonations() {
  saveDonations(); // Returns unhandled promise
  console.log("Done"); // Logs before save completes
}

// ✅ SOLUTION: Await async functions
async function processDonationsSafe() {
  await saveDonations();
  console.log("Done");
}

// OR: Handle promise explicitly
function processDonationsExplicit() {
  saveDonations()
    .then(() => console.log("Done"))
    .catch(handleError);
}
```

### Callback Hell

```typescript
// ❌ ANTI-PATTERN: Nested callbacks
function processDonation(id: string, callback: (err: Error | null, result?: any) => void) {
  fetchDonation(id, (err1, donation) => {
    if (err1) return callback(err1);

    validateDonation(donation, (err2, valid) => {
      if (err2) return callback(err2);

      saveDonation(valid, (err3, saved) => {
        if (err3) return callback(err3);
        callback(null, saved);
      });
    });
  });
}

// ✅ SOLUTION: Use async/await
async function processDonationAsync(id: string) {
  const donation = await fetchDonation(id);
  const valid = await validateDonation(donation);
  const saved = await saveDonation(valid);
  return saved;
}
```

## Performance Anti-Patterns

### String Concatenation in Loops

```typescript
// ❌ ANTI-PATTERN: Repeated string concatenation
let report = "";
for (const donation of donations) {
  report += `${donation.id}: ${donation.amount}\n`;
}

// ✅ SOLUTION: Array join
const lines = donations.map((d) => `${d.id}: ${d.amount}`);
const report = lines.join("\n");
```

### Unnecessary Object Creation

```typescript
// ❌ ANTI-PATTERN: Creates object every iteration
function filterDonations(donations: Donation[]) {
  return donations.filter((d) => {
    const minAmount = 100; // Created every iteration
    return d.amount >= minAmount;
  });
}

// ✅ SOLUTION: Hoist constants
function filterDonationsBetter(donations: Donation[]) {
  const minAmount = 100; // Created once
  return donations.filter((d) => d.amount >= minAmount);
}
```

### Not Using Memoization

```typescript
// ❌ ANTI-PATTERN: Recalculate expensive operations
class DonationStats {
  constructor(private donations: Donation[]) {}

  getTotal(): number {
    return this.donations.reduce((sum, d) => sum + d.amount, 0);
  }

  getAverage(): number {
    return this.getTotal() / this.donations.length; // Recalculates total
  }
}

// ✅ SOLUTION: Memoize expensive calculations
class DonationStatsMemo {
  private _total: number | null = null;

  constructor(private donations: Donation[]) {}

  getTotal(): number {
    if (this._total === null) {
      this._total = this.donations.reduce((sum, d) => sum + d.amount, 0);
    }
    return this._total;
  }

  getAverage(): number {
    return this.getTotal() / this.donations.length; // Uses cached total
  }
}
```

### Loading Everything at Once

```typescript
// ❌ ANTI-PATTERN: Load all data
async function processDonations() {
  const allDonations = await fetchAllDonations(); // Could be millions

  for (const donation of allDonations) {
    await process(donation);
  }
}

// ✅ SOLUTION: Paginate
async function processDonationsPaginated() {
  let page = 0;
  const pageSize = 1000;

  while (true) {
    const donations = await fetchDonations(page, pageSize);
    if (donations.length === 0) break;

    for (const donation of donations) {
      await process(donation);
    }

    page++;
  }
}
```

## Design Anti-Patterns

### God Object

```typescript
// ❌ ANTI-PATTERN: One object does everything
class DonationManager {
  createDonation() {}
  validateDonation() {}
  processDonation() {}
  sendEmail() {}
  generateReceipt() {}
  updateStats() {}
  logActivity() {}
  checkFraud() {}
}

// ✅ SOLUTION: Single Responsibility
class DonationService {
  create(data: DonationInput): Promise<Donation> {}
  validate(donation: Donation): ValidationResult {}
}

class EmailService {
  sendReceipt(donation: Donation): Promise<void> {}
}

class ReceiptGenerator {
  generate(donation: Donation): Promise<Buffer> {}
}

class DonationAnalytics {
  updateStats(donation: Donation): Promise<void> {}
}
```

### Primitive Obsession

```typescript
// ❌ ANTI-PATTERN: Using primitives
function processDonation(donationId: string, donorId: string, amount: number, currency: string) {
  // Easy to mix up parameters
}

processDonation("DON-123", "DNR-456", 1000, "USD");
processDonation("DNR-456", "DON-123", 1000, "USD"); // Bug!

// ✅ SOLUTION: Value objects
type DonationId = string & { __brand: "DonationId" };
type DonorId = string & { __brand: "DonorId" };

interface Money {
  amount: number;
  currency: string;
}

function processDonationSafe(donationId: DonationId, donorId: DonorId, payment: Money) {
  // Type-safe
}
```

### Shotgun Surgery

```typescript
// ❌ ANTI-PATTERN: Change requires modifying many files
// donation-service.ts
const FEE_PERCENTAGE = 0.03;

// payment-processor.ts
const FEE_PERCENTAGE = 0.03;

// receipt-generator.ts
const FEE_PERCENTAGE = 0.03;

// ✅ SOLUTION: Centralize configuration
// config.ts
export const DONATION_CONFIG = {
  feePercentage: 0.03,
  minimumAmount: 10,
  maximumAmount: 100000,
} as const;

// All files import from config
import { DONATION_CONFIG } from "./config";
```

### Spaghetti Code

```typescript
// ❌ ANTI-PATTERN: Complex control flow
function processDonation(donation: any) {
  if (donation.amount > 0) {
    if (donation.currency === "USD") {
      if (donation.category === "zakat") {
        if (donation.amount >= 100) {
          // nested logic
          if (donation.donor.verified) {
            // more nesting
          } else {
            // and more
          }
        }
      }
    }
  }
}

// ✅ SOLUTION: Early returns and extraction
function processDonationClear(donation: Donation): Result<void, Error> {
  if (donation.amount <= 0) {
    return err(new Error("Invalid amount"));
  }

  if (donation.currency !== "USD") {
    return err(new Error("Invalid currency"));
  }

  if (!validateCategory(donation.category)) {
    return err(new Error("Invalid category"));
  }

  if (donation.amount < MINIMUM_AMOUNT) {
    return err(new Error("Amount below minimum"));
  }

  return processVerifiedDonation(donation);
}

function processVerifiedDonation(donation: Donation): Result<void, Error> {
  if (!donation.donor.verified) {
    return err(new Error("Donor not verified"));
  }

  // Process donation
  return ok(undefined);
}
```

## Security Anti-Patterns

### Storing Secrets in Code

```typescript
// ❌ ANTI-PATTERN: Hardcoded secrets
const API_KEY = "sk-1234567890abcdef";
const DATABASE_URL = "postgresql://user:password@localhost/db";

// ✅ SOLUTION: Environment variables
const API_KEY = process.env.API_KEY!;
const DATABASE_URL = process.env.DATABASE_URL!;

// Validate at startup
if (!API_KEY || !DATABASE_URL) {
  throw new Error("Missing required environment variables");
}
```

### SQL Injection Risk

```typescript
// ❌ ANTI-PATTERN: String concatenation
async function getDonations(donorId: string) {
  const query = `SELECT * FROM donations WHERE donor_id = '${donorId}'`;
  return db.query(query);
}

// ✅ SOLUTION: Parameterized queries
async function getDonationsSafe(donorId: string) {
  const query = "SELECT * FROM donations WHERE donor_id = $1";
  return db.query(query, [donorId]);
}
```

### Exposing Sensitive Data

```typescript
// ❌ ANTI-PATTERN: Return sensitive data
app.get("/api/users/:id", async (req, res) => {
  const user = await db.findUser(req.params.id);
  res.json(user); // Includes password hash, email, etc.
});

// ✅ SOLUTION: DTO pattern
interface UserDTO {
  id: string;
  name: string;
  publicProfile: string;
}

function toUserDTO(user: User): UserDTO {
  return {
    id: user.id,
    name: user.name,
    publicProfile: user.publicProfile,
  };
}

app.get("/api/users/:id", async (req, res) => {
  const user = await db.findUser(req.params.id);
  res.json(toUserDTO(user));
});
```

### Not Validating Input

```typescript
// ❌ ANTI-PATTERN: Trust user input
app.post("/api/donations", async (req, res) => {
  const donation = await createDonation(req.body);
  res.json(donation);
});

// ✅ SOLUTION: Validate all input
const donationSchema = z.object({
  donorId: z.string().regex(/^DNR-\d{10}$/),
  amount: z.number().positive().max(1000000),
  currency: z.enum(["USD", "EUR", "SAR"]),
  category: z.enum(["zakat", "sadaqah", "waqf"]),
});

app.post("/api/donations", async (req, res) => {
  try {
    const data = donationSchema.parse(req.body);
    const donation = await createDonation(data);
    res.json(donation);
  } catch (error) {
    res.status(400).json({ error: "Invalid input" });
  }
});
```

## Testing Anti-Patterns

### Testing Implementation Details

```typescript
// ❌ ANTI-PATTERN: Test private methods
class ZakatCalculator {
  private calculateNisab(wealth: number): boolean {
    return wealth >= 3000;
  }

  calculate(wealth: number): number {
    if (this.calculateNisab(wealth)) {
      return wealth * 0.025;
    }
    return 0;
  }
}

// Testing private method
test("calculateNisab", () => {
  const calc = new ZakatCalculator();
  expect((calc as any).calculateNisab(5000)).toBe(true);
});

// ✅ SOLUTION: Test public interface
test("calculate zakat for wealth above nisab", () => {
  const calc = new ZakatCalculator();
  expect(calc.calculate(5000)).toBe(125);
});

test("calculate zakat for wealth below nisab", () => {
  const calc = new ZakatCalculator();
  expect(calc.calculate(2000)).toBe(0);
});
```

### Not Testing Edge Cases

```typescript
// ❌ ANTI-PATTERN: Only happy path
test("create donation", () => {
  const donation = createDonation({
    donorId: "DNR-1234567890",
    amount: 1000,
    currency: "USD",
    category: "zakat",
  });

  expect(donation).toBeDefined();
});

// ✅ SOLUTION: Test edge cases
describe("createDonation", () => {
  it("creates valid donation", () => {
    const donation = createDonation(validInput);
    expect(donation).toBeDefined();
  });

  it("rejects negative amount", () => {
    expect(() => createDonation({ ...validInput, amount: -100 })).toThrow();
  });

  it("rejects invalid currency", () => {
    expect(() => createDonation({ ...validInput, currency: "XYZ" })).toThrow();
  });

  it("rejects zero amount", () => {
    expect(() => createDonation({ ...validInput, amount: 0 })).toThrow();
  });

  it("rejects amount exceeding limit", () => {
    expect(() => createDonation({ ...validInput, amount: 10000000 })).toThrow();
  });
});
```

### Mocking Everything

```typescript
// ❌ ANTI-PATTERN: Over-mocking
test("process donation", async () => {
  const mockValidate = jest.fn().mockReturnValue(true);
  const mockSave = jest.fn().mockResolvedValue({ id: "123" });
  const mockEmail = jest.fn().mockResolvedValue(true);

  // Test doesn't verify actual logic
});

// ✅ SOLUTION: Mock external dependencies only
test("process donation", async () => {
  // Mock external services
  const mockEmailService = {
    send: jest.fn().mockResolvedValue(true),
  };

  // Use real validation logic
  const donation = await processDonation(validInput, mockEmailService);

  expect(donation).toBeDefined();
  expect(mockEmailService.send).toHaveBeenCalledWith(donation);
});
```

## Related Documentation

- **[TypeScript Best Practices](./ex-so-stla-ts__best-practices.md)** - Recommended patterns
- **[TypeScript Type Safety](./ex-so-stla-ts__type-safety.md)** - Type patterns
- **[TypeScript Error Handling](./ex-so-stla-ts__error-handling.md)** - Error patterns

---

**Last Updated**: 2025-01-23
**TypeScript Version**: 5.0+ (baseline), 5.4+ (milestone), 5.6+ (stable), 5.7.3+ (latest stable)
**Maintainers**: OSE Documentation Team
