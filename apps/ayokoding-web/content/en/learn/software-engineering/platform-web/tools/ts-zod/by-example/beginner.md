---
title: "Beginner"
weight: 10000001
date: 2026-03-25T00:00:00+07:00
draft: false
description: "Master Zod fundamentals through 28 annotated examples covering primitives, objects, arrays, enums, unions, optionals, parsing, type inference, and error handling"
tags: ["zod", "typescript", "validation", "schema", "tutorial", "by-example", "beginner"]
---

This beginner tutorial covers Zod fundamentals through 28 heavily annotated examples. Each example maintains 1-2.25 comment lines per code line to ensure deep understanding.

## Prerequisites

Before starting, ensure you understand:

- TypeScript basics (types, interfaces, generics)
- ES6+ JavaScript (imports, destructuring, arrow functions)
- npm package installation (`npm install zod`)
- Basic programming concepts (variables, functions, conditionals)

## Group 1: Primitive Schemas

### Example 1: String Schema

Zod's `z.string()` creates a schema that validates string values at runtime. Any non-string input fails validation.

```typescript
import { z } from "zod";
// => z is the main Zod namespace — all schemas start here

const nameSchema = z.string();
// => Creates a ZodString schema
// => Validates: input must be typeof "string"

// Parse with valid data
const validName = nameSchema.parse("Aisha");
// => parse() returns the validated value as its TypeScript type
// => validName = "Aisha" (type: string)

console.log(validName);
// => Output: Aisha

// Parse with invalid data — throws ZodError
try {
  nameSchema.parse(42);
  // => 42 is a number, not string — fails validation
} catch (error) {
  console.log("Validation failed");
  // => Output: Validation failed
  // => ZodError contains details about what failed
}
```

**Key Takeaway**: `z.string()` validates string values at runtime. Use `.parse()` to validate and get back a typed value; it throws on failure.

**Why It Matters**: TypeScript only checks types at compile time. When data arrives from API responses, form inputs, or environment variables, TypeScript cannot verify it. `z.string()` provides the runtime check that TypeScript cannot. Every validated string schema creates a clear contract between what your application expects and what it receives — catching mismatches immediately rather than letting invalid data propagate through your system causing subtle bugs.

---

### Example 2: Number Schema

`z.number()` validates numeric values. Zod distinguishes between integer and floating-point constraints through chainable methods.

```typescript
import { z } from "zod";

const ageSchema = z.number();
// => Creates a ZodNumber schema
// => Validates: input must be typeof "number" and not NaN

const validAge = ageSchema.parse(28);
// => parse returns 28 as typed number
// => validAge = 28 (type: number)

// Number with constraints
const percentSchema = z.number().min(0).max(100);
// => .min(0) — adds constraint: value >= 0
// => .max(100) — adds constraint: value <= 100
// => Constraints chain — both apply to same schema

const validPercent = percentSchema.parse(75.5);
// => 75.5 passes: 0 <= 75.5 <= 100
// => validPercent = 75.5 (type: number)

// Integer constraint
const countSchema = z.number().int().nonnegative();
// => .int() — validates value has no decimal component
// => .nonnegative() — validates value >= 0

try {
  countSchema.parse(3.7);
  // => 3.7 is not integer — fails .int() check
} catch (error) {
  console.log("Not an integer");
  // => Output: Not an integer
}
```

**Key Takeaway**: `z.number()` validates numeric input; chain `.min()`, `.max()`, `.int()`, `.positive()`, `.nonnegative()` to add mathematical constraints.

**Why It Matters**: APIs and user inputs frequently send numbers as strings or include NaN in unexpected ways. Zod's number schema catches these issues before they reach your business logic. The chainable constraints let you express domain rules — ages must be positive, percentages between 0 and 100, counts must be integers — directly in the schema definition, co-locating validation with type information.

---

### Example 3: Boolean Schema

`z.boolean()` validates boolean values. It strictly requires `true` or `false` — strings like `"true"` fail unless coercion is used.

```typescript
import { z } from "zod";

const activeSchema = z.boolean();
// => Creates a ZodBoolean schema
// => Validates: input must be exactly true or false (strict)

const isActive = activeSchema.parse(true);
// => isActive = true (type: boolean)

const isInactive = activeSchema.parse(false);
// => isInactive = false (type: boolean)

// String "true" fails strict boolean check
try {
  activeSchema.parse("true");
  // => "true" is a string, not boolean — fails validation
  // => Use z.coerce.boolean() if you need string-to-boolean conversion
} catch (error) {
  console.log("String is not boolean");
  // => Output: String is not boolean
}

// Practical usage: feature flags
const FeatureFlagSchema = z.object({
  // => z.object() wraps multiple schemas into one — see Example 9
  darkMode: z.boolean(),
  // => Required boolean flag
  betaFeatures: z.boolean().default(false),
  // => Optional with default — see Example 13 for .default()
});

type FeatureFlags = z.infer<typeof FeatureFlagSchema>;
// => FeatureFlags = { darkMode: boolean; betaFeatures: boolean }
```

**Key Takeaway**: `z.boolean()` validates strict `true`/`false` values. For truthy/falsy coercion from strings, use `z.coerce.boolean()` instead.

**Why It Matters**: Boolean validation matters most for configuration flags, feature toggles, and permission systems. Strict boolean validation prevents the common JavaScript pitfall where any truthy value is treated as `true`. When your feature flag system receives `"true"` instead of `true` from an environment variable or API, strict validation catches it before your conditional logic silently fails.

---

### Example 4: String Validations

`z.string()` supports a rich set of format validators. Chain them to validate email, URL, UUID, length, and regex patterns.

```typescript
import { z } from "zod";

// Email validation
const emailSchema = z.string().email();
// => .email() validates RFC-compliant email format
// => Checks for @ symbol, domain, TLD structure

const validEmail = emailSchema.parse("aisha@example.com");
// => validEmail = "aisha@example.com" (type: string)

// URL validation
const urlSchema = z.string().url();
// => .url() validates URL format (requires protocol)

const validUrl = urlSchema.parse("https://ayokoding.com");
// => validUrl = "https://ayokoding.com" (type: string)

// UUID validation
const idSchema = z.string().uuid();
// => .uuid() validates UUID v4 format: xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx

// Length constraints
const usernameSchema = z.string().min(3).max(20);
// => .min(3) — string length >= 3
// => .max(20) — string length <= 20

// Regex validation
const slugSchema = z.string().regex(/^[a-z0-9-]+$/);
// => .regex() validates against custom pattern
// => This slug allows: lowercase letters, numbers, hyphens only

try {
  emailSchema.parse("not-an-email");
  // => Missing @ and domain — fails .email() check
} catch (error) {
  console.log("Invalid email format");
  // => Output: Invalid email format
}
```

**Key Takeaway**: Chain format validators like `.email()`, `.url()`, `.uuid()`, `.min()`, `.max()`, `.regex()` on `z.string()` for declarative format validation.

**Why It Matters**: Input validation is the first line of defense against bad data. Zod's built-in validators cover the most common formats — emails, URLs, UUIDs — without requiring separate validation libraries. Chaining multiple validators creates readable, self-documenting validation rules that serve as executable documentation of your data contracts. Production applications validate these formats at API boundaries, form submissions, and configuration loading.

---

### Example 5: Number Validations

Number schemas support mathematical constraints through chainable methods that express domain-specific rules.

```typescript
import { z } from "zod";

// Positive number
const priceSchema = z.number().positive();
// => .positive() validates value > 0 (excludes zero)

const validPrice = priceSchema.parse(29.99);
// => validPrice = 29.99 (type: number)

// Integer validation
const pageSchema = z.number().int().min(1);
// => .int() ensures no decimal component
// => .min(1) ensures page number starts at 1

const validPage = pageSchema.parse(5);
// => validPage = 5 (type: number)

// Finite validation — excludes Infinity
const measurementSchema = z.number().finite();
// => .finite() rejects Infinity and -Infinity
// => Useful for mathematical operations where Infinity breaks calculations

// Multiple constraints combined
const latitudeSchema = z.number().min(-90).max(90);
// => GPS latitude range: -90 to 90 degrees

const longitudeSchema = z.number().min(-180).max(180);
// => GPS longitude range: -180 to 180 degrees

// Safe integer validation
const idSchema = z.number().int().positive().max(Number.MAX_SAFE_INTEGER);
// => Database ID: positive integer within JavaScript safe integer range
// => MAX_SAFE_INTEGER = 2^53 - 1 = 9007199254740991
```

**Key Takeaway**: Combine `.int()`, `.positive()`, `.negative()`, `.min()`, `.max()`, `.finite()` on `z.number()` to express mathematical domain constraints declaratively.

**Why It Matters**: Domain rules for numbers are frequently violated in real data. Prices cannot be negative; page numbers start at 1; latitude must be between -90 and 90. Without explicit validation, these violations silently corrupt data or cause runtime errors deep in business logic. Zod's number validators move domain rule enforcement to the data ingestion boundary, making violations immediately visible.

---

### Example 6: Literal Schema

`z.literal()` creates a schema that validates exactly one specific value. The inferred TypeScript type is the literal value itself.

```typescript
import { z } from "zod";

// String literal
const successSchema = z.literal("success");
// => Validates: input must be exactly the string "success"
// => Inferred type: "success" (literal string type, not string)

const result = successSchema.parse("success");
// => result = "success" (type: "success", not string)

// Number literal
const zeroSchema = z.literal(0);
// => Validates: input must be exactly 0
// => type: 0 (literal number type)

// Boolean literal
const trueSchema = z.literal(true);
// => Validates: input must be exactly true
// => type: true (literal boolean type)

// Practical use: discriminant fields in unions
const SuccessResponse = z.object({
  // => z.object for structured schemas — see Example 9
  status: z.literal("success"),
  // => status must be exactly "success"
  data: z.string(),
  // => data is a string
});

type SuccessResponseType = z.infer<typeof SuccessResponse>;
// => { status: "success"; data: string }
// => TypeScript uses "success" literal type — enables discriminated unions

try {
  successSchema.parse("failure");
  // => "failure" !== "success" — fails literal check
} catch (error) {
  console.log("Not the expected literal value");
  // => Output: Not the expected literal value
}
```

**Key Takeaway**: `z.literal()` validates exact values and infers literal TypeScript types — essential for discriminated unions and type narrowing.

**Why It Matters**: Literal types are the foundation of TypeScript's discriminated union pattern. When an API response includes a `type` or `status` field with known values, `z.literal()` validates those exact values while also producing the precise TypeScript literal type needed for type narrowing. This enables TypeScript to understand which variant of a union you're working with, eliminating type assertions and unsafe casts throughout your codebase.

---

### Example 7: Date Schema

`z.date()` validates JavaScript Date objects. Use `.min()` and `.max()` for date range constraints.

```typescript
import { z } from "zod";

const dateSchema = z.date();
// => Creates a ZodDate schema
// => Validates: input must be a JavaScript Date instance
// => Rejects strings like "2026-03-25" — use z.coerce.date() for those

const today = dateSchema.parse(new Date("2026-03-25"));
// => today = Date object for 2026-03-25
// => type: Date

// Date range validation
const birthDateSchema = z
  .date()
  .min(new Date("1900-01-01"))
  // => .min() validates date is after this lower bound
  .max(new Date());
// => .max() validates date is before or equal to current time
// => new Date() creates today's date at parse time

// Future date validation
const scheduledAtSchema = z.date().min(new Date());
// => Validates date is in the future (after now)
// => Useful for scheduling features

const validDate = dateSchema.parse(new Date());
// => validDate is the current Date object (type: Date)

// String dates fail — must be Date objects
try {
  dateSchema.parse("2026-03-25");
  // => String representation fails ZodDate
  // => Use z.coerce.date() to accept strings — see Intermediate examples
} catch (error) {
  console.log("Strings are not Date objects");
  // => Output: Strings are not Date objects
}
```

**Key Takeaway**: `z.date()` validates JavaScript Date instances strictly. For string date inputs, use `z.coerce.date()` which converts strings to Date objects before validation.

**Why It Matters**: Date handling is a common source of bugs in production applications. APIs often send dates as ISO strings, but business logic works with Date objects. Explicit date validation catches when a serialized date string arrives where a Date object is expected. Date range validation — birth dates within valid range, scheduled events in the future — is business logic that belongs in the schema, not scattered throughout your application handlers.

---

## Group 2: Object and Array Schemas

### Example 8: Object Schema

`z.object()` creates a schema for structured objects. Each property maps to its own schema, and the object schema validates all properties together.

```typescript
import { z } from "zod";

// Define object schema
const UserSchema = z.object({
  // => Each key maps to a validator
  id: z.string().uuid(),
  // => id must be a valid UUID string
  name: z.string().min(1),
  // => name must be a non-empty string
  age: z.number().int().nonnegative(),
  // => age: integer, non-negative (0 is allowed)
  email: z.string().email(),
  // => email must be valid email format
});

// Infer TypeScript type from schema
type User = z.infer<typeof UserSchema>;
// => User = {
// =>   id: string;
// =>   name: string;
// =>   age: number;
// =>   email: string;
// => }

// Validate an object
const validUser = UserSchema.parse({
  id: "550e8400-e29b-41d4-a716-446655440000",
  name: "Omar",
  age: 22,
  email: "omar@example.com",
});
// => validUser is typed as User
// => All fields validated against their individual schemas

console.log(validUser.name);
// => Output: Omar

// Missing required field fails
try {
  UserSchema.parse({ id: "550e8400-e29b-41d4-a716-446655440000", name: "Ali" });
  // => Missing age and email — both required
} catch (error) {
  console.log("Missing required fields");
  // => Output: Missing required fields
}
```

**Key Takeaway**: `z.object()` composes multiple schemas into a structured validator. The inferred `z.infer<>` type matches the schema shape exactly — no manual interface definition needed.

**Why It Matters**: Objects are the primary data structure in API communication. Validating every field of an incoming object — not just its shape but each field's format, range, and requirements — prevents subtle bugs from partial data. The automatic TypeScript type inference eliminates the duplication of maintaining both a Zod schema and a separate TypeScript interface, ensuring your type definitions stay synchronized with your validation rules.

---

### Example 9: Nested Object Schema

Objects can nest other object schemas to validate deeply structured data. Zod validates all levels of nesting.

```typescript
import { z } from "zod";

// Reusable sub-schemas
const AddressSchema = z.object({
  street: z.string().min(1),
  // => street: required non-empty string
  city: z.string().min(1),
  // => city: required non-empty string
  country: z.string().length(2),
  // => country: exactly 2 characters (ISO country code)
  postalCode: z.string().regex(/^\d{5}(-\d{4})?$/),
  // => postalCode: US ZIP format (12345 or 12345-6789)
});

// Nest AddressSchema inside UserSchema
const UserWithAddressSchema = z.object({
  name: z.string(),
  // => name: any string
  address: AddressSchema,
  // => address field validated by AddressSchema
  // => All AddressSchema validations apply recursively
});

type UserWithAddress = z.infer<typeof UserWithAddressSchema>;
// => UserWithAddress = {
// =>   name: string;
// =>   address: {
// =>     street: string;
// =>     city: string;
// =>     country: string;
// =>     postalCode: string;
// =>   };
// => }

const user = UserWithAddressSchema.parse({
  name: "Fatima",
  address: {
    street: "123 Main St",
    city: "Jakarta",
    country: "ID",
    postalCode: "12345",
  },
});
// => Full nested validation passes
// => user.address.city = "Jakarta" (type: string)

console.log(user.address.city);
// => Output: Jakarta
```

**Key Takeaway**: Compose schemas by nesting object schemas. Zod recursively validates all levels, and `z.infer<>` produces the complete nested TypeScript type.

**Why It Matters**: Real-world API payloads are deeply nested. Validation logic that handles only the top level while leaving nested objects unvalidated creates false security — the deeply nested field you failed to validate is often the one that causes a production outage. Zod's compositional design makes validating complex nested structures as natural as defining the shape of your data, and separating sub-schemas into reusable constants improves maintainability across endpoints that share common structures.

---

### Example 10: Array Schema

`z.array()` wraps another schema to create a validator for arrays of that type. Chain `.min()`, `.max()`, and `.nonempty()` for length constraints.

```typescript
import { z } from "zod";

// Array of strings
const tagsSchema = z.array(z.string());
// => Creates ZodArray<ZodString>
// => Validates: input is an array, every element is a string

const validTags = tagsSchema.parse(["typescript", "zod", "validation"]);
// => validTags = ["typescript", "zod", "validation"]
// => type: string[]

// Array with length constraints
const teamSchema = z.array(z.string()).min(1).max(10);
// => .min(1) — array must have at least 1 element
// => .max(10) — array cannot exceed 10 elements

// Non-empty array (at least 1 element, type guaranteed)
const nonEmptySchema = z.array(z.string()).nonempty();
// => .nonempty() guarantees array has at least one element
// => Inferred type: [string, ...string[]] (tuple with rest)

// Array of objects
const ProductSchema = z.object({
  name: z.string(),
  price: z.number().positive(),
});
const productsSchema = z.array(ProductSchema);
// => Array of Product objects — each element validated by ProductSchema

type Products = z.infer<typeof productsSchema>;
// => Products = Array<{ name: string; price: number }>

const products = productsSchema.parse([
  { name: "Book", price: 15.99 },
  { name: "Pen", price: 2.5 },
]);
// => Each element validated against ProductSchema
// => products[0].name = "Book" (type: string)
```

**Key Takeaway**: `z.array(schema)` validates arrays where every element matches the inner schema. Chain length methods for array size constraints.

**Why It Matters**: Arrays are ubiquitous in API responses — lists of products, search results, user records. Without array validation, a single malformed element silently corrupts iteration logic. Zod validates every element in an array, ensuring your downstream map/filter/reduce operations always receive correctly typed data. The `.nonempty()` method is particularly valuable for business logic that assumes at least one element exists, providing a runtime guarantee that prevents empty-array edge cases.

---

### Example 11: Tuple Schema

`z.tuple()` validates fixed-length arrays where each position has a specific type. Unlike `z.array()`, tuple positions have different types.

```typescript
import { z } from "zod";

// Simple tuple: [name, age]
const personTupleSchema = z.tuple([z.string(), z.number()]);
// => Position 0: must be string (name)
// => Position 1: must be number (age)
// => Length must be exactly 2

type PersonTuple = z.infer<typeof personTupleSchema>;
// => PersonTuple = [string, number]

const person = personTupleSchema.parse(["Aisha", 28]);
// => person[0] = "Aisha" (type: string)
// => person[1] = 28 (type: number)

// Tuple with rest elements
const atLeastTwoSchema = z.tuple([z.string(), z.string()]).rest(z.string());
// => First two positions: required strings
// => .rest() allows additional elements of the rest schema type
// => type: [string, string, ...string[]]

const validTuple = atLeastTwoSchema.parse(["a", "b", "c", "d"]);
// => First two are required, rest are optional additional strings
// => validTuple = ["a", "b", "c", "d"]

// RGB color tuple
const colorSchema = z.tuple([
  z.number().min(0).max(255), // => Red: 0-255
  z.number().min(0).max(255), // => Green: 0-255
  z.number().min(0).max(255), // => Blue: 0-255
]);

type RGB = z.infer<typeof colorSchema>;
// => RGB = [number, number, number]

const red = colorSchema.parse([255, 0, 0]);
// => red = [255, 0, 0] — valid RGB red color
```

**Key Takeaway**: `z.tuple()` validates fixed-length, heterogeneous arrays where each position has its own schema. Use for coordinate pairs, CSV records, and positional data.

**Why It Matters**: Tuples appear in TypeScript patterns for returning multiple typed values from functions, representing coordinates, encoding states, and processing CSV rows. Without tuple validation, the first element might silently be a number when you expected a string. Zod's tuple schema encodes both length and per-position type constraints, making destructuring assignment safe with verified types at each position.

---

## Group 3: Optional, Nullable, and Default

### Example 12: Optional Schema

`.optional()` makes a field accept `undefined` in addition to its primary type. Optional fields can be omitted from object schemas entirely.

```typescript
import { z } from "zod";

// Optional string
const optionalNameSchema = z.string().optional();
// => Adds undefined to accepted types
// => type: string | undefined

const withName = optionalNameSchema.parse("Aisha");
// => withName = "Aisha" (type: string | undefined)

const withUndefined = optionalNameSchema.parse(undefined);
// => withUndefined = undefined (type: string | undefined)

// Optional in object — field can be omitted
const ProfileSchema = z.object({
  name: z.string(),
  // => Required: must be present and string
  bio: z.string().optional(),
  // => Optional: can be omitted or explicitly undefined
  website: z.string().url().optional(),
  // => Optional URL: if provided, must be valid URL format
});

type Profile = z.infer<typeof ProfileSchema>;
// => Profile = {
// =>   name: string;
// =>   bio?: string | undefined;
// =>   website?: string | undefined;
// => }

// All these are valid
const minimal = ProfileSchema.parse({ name: "Omar" });
// => minimal.bio = undefined (field was omitted)

const full = ProfileSchema.parse({ name: "Omar", bio: "Developer", website: "https://omar.dev" });
// => full.bio = "Developer"
// => full.website = "https://omar.dev"
```

**Key Takeaway**: `.optional()` accepts `undefined` and makes object fields omissible. The inferred type includes `| undefined` to force null-checking in downstream code.

**Why It Matters**: Optional fields appear everywhere in real-world schemas — user profiles with optional bios, orders with optional notes, events with optional end dates. Making optionality explicit in the schema prevents the common bug where you forget to handle the missing-value case in business logic. TypeScript's strict null checks combined with Zod's optional validation create a complete safety net: the schema catches it at the boundary, TypeScript forces you to handle it in code.

---

### Example 13: Nullable Schema

`.nullable()` makes a schema accept `null` in addition to its primary type. Null and undefined are different in Zod — use `.nullish()` to accept both.

```typescript
import { z } from "zod";

// Nullable string
const nullableNameSchema = z.string().nullable();
// => Accepts string or null
// => type: string | null (NOT string | undefined)

const withNull = nullableNameSchema.parse(null);
// => withNull = null (type: string | null)

const withString = nullableNameSchema.parse("Fatima");
// => withString = "Fatima" (type: string | null)

// null vs undefined: different in Zod
try {
  nullableNameSchema.parse(undefined);
  // => undefined is NOT null — fails nullable
} catch (error) {
  console.log("undefined is not null");
  // => Output: undefined is not null
}

// Nullish: accepts both null and undefined
const nullishSchema = z.string().nullish();
// => type: string | null | undefined

// Practical: database fields that can be null
const UserProfileSchema = z.object({
  userId: z.string().uuid(),
  // => Required ID
  deletedAt: z.date().nullable(),
  // => null means not deleted; Date means when it was deleted
  lastLoginAt: z.date().nullable(),
  // => null means never logged in
});

type UserProfile = z.infer<typeof UserProfileSchema>;
// => { userId: string; deletedAt: Date | null; lastLoginAt: Date | null }
```

**Key Takeaway**: `.nullable()` accepts `null`; `.optional()` accepts `undefined`; `.nullish()` accepts both. Zod distinguishes these explicitly, matching JavaScript's null/undefined distinction.

**Why It Matters**: Databases represent absence as NULL, which becomes `null` in JSON serialization. APIs often use `null` deliberately — a `deletedAt: null` means not deleted, while `deletedAt: "2026-01-01"` means deleted. Treating `null` and `undefined` as interchangeable leads to subtle bugs where "not deleted" is confused with "field missing." Zod's explicit distinction forces you to model your data's absence semantics correctly.

---

### Example 14: Default Values

`.default()` provides a fallback value when the input is `undefined`. It transforms missing fields into their default values during parsing.

```typescript
import { z } from "zod";

// Default value for optional field
const countSchema = z.number().default(0);
// => When input is undefined, output is 0
// => When input is a number, output is that number

const withDefault = countSchema.parse(undefined);
// => withDefault = 0 (default applied)
// => type: number (NOT number | undefined)

const withValue = countSchema.parse(42);
// => withValue = 42 (default not applied)

// Default in object schema
const PaginationSchema = z.object({
  page: z.number().int().positive().default(1),
  // => page defaults to 1 if omitted
  limit: z.number().int().positive().max(100).default(20),
  // => limit defaults to 20, cannot exceed 100
  sortBy: z.string().default("createdAt"),
  // => sortBy defaults to "createdAt"
});

type Pagination = z.infer<typeof PaginationSchema>;
// => { page: number; limit: number; sortBy: string }
// => Note: all fields are non-optional in the output type

const defaults = PaginationSchema.parse({});
// => {} triggers all defaults
// => defaults = { page: 1, limit: 20, sortBy: "createdAt" }

const partial = PaginationSchema.parse({ page: 3 });
// => page: 3 (user provided)
// => limit: 20 (default), sortBy: "createdAt" (default)
```

**Key Takeaway**: `.default(value)` replaces `undefined` with the default value during parsing. The output type removes `undefined`, making the field always-present after validation.

**Why It Matters**: Default values in schemas create consistent, predictable behavior for query parameters, configuration objects, and optional settings. Instead of scattering `?? defaultValue` expressions throughout your codebase, you centralize defaults in the schema definition where they serve as documentation. Zod's defaults also apply during parsing, meaning validated data always has the correct shape — you never need to check "was this field provided?" in downstream code.

---

### Example 15: Catch (Fallback on Error)

`.catch()` provides a fallback value when validation fails, unlike `.default()` which only applies to `undefined`. This allows graceful degradation.

```typescript
import { z } from "zod";

// Catch provides fallback when validation fails
const safeColorSchema = z.string().catch("grey");
// => If input is not a string (or any validation fails), use "grey"
// => type: string (never throws — always returns string)

const fromValid = safeColorSchema.parse("blue");
// => fromValid = "blue" (validation passed)

const fromInvalid = safeColorSchema.parse(12345);
// => 12345 is not a string — catch triggers
// => fromInvalid = "grey" (fallback value)

const fromNull = safeColorSchema.parse(null);
// => null is not a string — catch triggers
// => fromNull = "grey" (fallback value)

// Catch in object schema — resilient parsing
const ConfigSchema = z.object({
  theme: z.enum(["light", "dark"]).catch("light"),
  // => Invalid theme → default to "light"
  language: z.string().min(2).max(5).catch("en"),
  // => Invalid language code → default to "en"
  fontSize: z.number().min(12).max(32).catch(16),
  // => Out-of-range font size → default to 16
});

const config = ConfigSchema.parse({
  theme: "invalid-theme", // => Not in enum → "light"
  language: null, // => Not a string → "en"
  fontSize: 999, // => Exceeds max → 16
});
// => config = { theme: "light", language: "en", fontSize: 16 }
```

**Key Takeaway**: `.catch(fallback)` silently uses the fallback value when validation fails, enabling graceful degradation. Unlike `.default()`, it handles any validation failure, not just `undefined`.

**Why It Matters**: User preferences and configuration data often contain values that were valid in an older version of your application but are no longer valid. Rather than rejecting stored preferences entirely, `.catch()` lets you gracefully fall back to safe defaults while still validating new data strictly. This pattern is especially useful for feature flags, theme preferences, and configuration objects where a bad value should degrade gracefully rather than crash the application.

---

## Group 4: Enum, Union, and Intersection

### Example 16: Enum Schema

`z.enum()` validates string literal union types. It creates both a Zod schema and provides access to the enum values as a constant.

```typescript
import { z } from "zod";

// Define enum schema
const StatusSchema = z.enum(["pending", "active", "inactive", "deleted"]);
// => Accepts only the listed string values
// => type: "pending" | "active" | "inactive" | "deleted"

type Status = z.infer<typeof StatusSchema>;
// => Status = "pending" | "active" | "inactive" | "deleted"

const validStatus = StatusSchema.parse("active");
// => validStatus = "active" (type: Status)

// Access enum values as constant
const statusValues = StatusSchema.options;
// => statusValues = ["pending", "active", "inactive", "deleted"]
// => Useful for generating select options, iterating valid values

try {
  StatusSchema.parse("unknown");
  // => "unknown" is not in the enum — fails validation
} catch (error) {
  console.log("Invalid status value");
  // => Output: Invalid status value
}

// Native enum support
enum Direction {
  North = "NORTH",
  South = "SOUTH",
  East = "EAST",
  West = "WEST",
}

const DirectionSchema = z.nativeEnum(Direction);
// => z.nativeEnum() wraps TypeScript enums
// => Validates against enum values ("NORTH", "SOUTH", "EAST", "WEST")

const dir = DirectionSchema.parse(Direction.North);
// => dir = "NORTH" (type: Direction)
```

**Key Takeaway**: `z.enum([...])` validates against a fixed set of string values and infers a union literal type. Access valid values via `.options` for UI generation.

**Why It Matters**: Status fields, category selectors, and role definitions are naturally enumerations. Without enum validation, an invalid status string silently passes validation and causes incorrect business logic — an order status of "shiped" (typo) is processed as if valid. Zod enums enforce the closed-world assumption: only known values are allowed. The `.options` array is particularly useful for populating dropdown menus and generating exhaustive switch statement cases in TypeScript.

---

### Example 17: Union Schema

`z.union()` creates a schema that accepts values matching any of its member schemas. The inferred type is a union of member types.

```typescript
import { z } from "zod";

// String or number union
const idSchema = z.union([z.string(), z.number()]);
// => Accepts string or number
// => type: string | number

const stringId = idSchema.parse("abc-123");
// => stringId = "abc-123" (type: string | number)

const numericId = idSchema.parse(42);
// => numericId = 42 (type: string | number)

// Union of object schemas
const TextContentSchema = z.object({
  type: z.literal("text"),
  // => Discriminant: must be "text"
  content: z.string(),
  // => Text content
});

const ImageContentSchema = z.object({
  type: z.literal("image"),
  // => Discriminant: must be "image"
  url: z.string().url(),
  // => Image URL — must be valid URL
  altText: z.string(),
  // => Accessibility alt text
});

const ContentSchema = z.union([TextContentSchema, ImageContentSchema]);
// => Accepts either TextContent or ImageContent
// => type: { type: "text"; content: string } | { type: "image"; url: string; altText: string }

const textBlock = ContentSchema.parse({ type: "text", content: "Hello World" });
// => Matches TextContentSchema (type: "text")

const imageBlock = ContentSchema.parse({ type: "image", url: "https://img.example.com/photo.jpg", altText: "A photo" });
// => Matches ImageContentSchema (type: "image")
```

**Key Takeaway**: `z.union([...])` accepts any of its member schemas. For discriminated unions with a shared discriminant field, prefer `z.discriminatedUnion()` (see Intermediate) for better performance and error messages.

**Why It Matters**: Real-world APIs return heterogeneous data — a field might be a string ID or numeric ID, a content block might be text or an image. Union types are TypeScript's mechanism for modeling this variability, and Zod's union schema validates it at runtime. Without union validation, you resort to `any` or unsafe type assertions. With union validation, you get runtime type safety that matches TypeScript's compile-time guarantees, enabling safe type narrowing in downstream code.

---

### Example 18: Optional and Nullable Combined

Combining `.optional()` and `.nullable()` creates schemas that accept `string | null | undefined`. Understanding the behavior difference between the two is essential.

```typescript
import { z } from "zod";

// Four states for a field
const strictSchema = z.string();
// => type: string — only string, no null, no undefined

const optionalSchema = z.string().optional();
// => type: string | undefined — undefined allowed

const nullableSchema = z.string().nullable();
// => type: string | null — null allowed

const nullishSchema = z.string().nullish();
// => type: string | null | undefined — both null and undefined allowed
// => Equivalent to z.string().optional().nullable()

// In object context — practical differences
const FormSchema = z.object({
  // Required: never null or undefined
  email: z.string().email(),

  // Can be omitted from input (becomes undefined)
  nickname: z.string().optional(),

  // Explicitly nullable — API may return null
  middleName: z.string().nullable(),

  // API returns null, but field may also be absent
  deletedAt: z.date().nullish(),
});

type FormData = z.infer<typeof FormSchema>;
// => {
// =>   email: string;
// =>   nickname?: string | undefined;
// =>   middleName: string | null;
// =>   deletedAt?: Date | null | undefined;
// => }

const parsed = FormSchema.parse({
  email: "aisha@example.com",
  middleName: null,
  // nickname omitted → undefined
  // deletedAt omitted → undefined
});
// => parsed.middleName = null
// => parsed.nickname = undefined
```

**Key Takeaway**: `optional()` handles absence (undefined), `nullable()` handles explicit null, `nullish()` handles both. Choose based on whether your API/database uses null explicitly or omits fields.

**Why It Matters**: The null/undefined distinction matters when serializing to JSON (undefined is omitted; null is serialized), interacting with databases (NULL is explicit absence), and calling APIs that distinguish between "field not provided" and "field explicitly cleared." Using the wrong modifier creates subtle serialization bugs or incorrect database queries. Modeling absence correctly in schemas forces correct handling throughout your application.

---

## Group 5: Records, Maps, and Sets

### Example 19: Record Schema

`z.record()` validates objects with dynamic keys where all values follow the same schema. Unlike `z.object()`, keys are unknown at schema definition time.

```typescript
import { z } from "zod";

// Record with string keys and number values
const scoreMapSchema = z.record(z.string(), z.number());
// => First arg: key schema (must extend string | number | symbol)
// => Second arg: value schema
// => type: Record<string, number>

const scores = scoreMapSchema.parse({
  aisha: 95,
  omar: 87,
  fatima: 92,
});
// => scores = { aisha: 95, omar: 87, fatima: 92 }
// => type: Record<string, number>

// Record with literal key constraint
const ColorMap = z.record(
  z.enum(["red", "green", "blue"]),
  // => Keys must be "red", "green", or "blue"
  z.string(),
  // => Values are hex color strings
);

type Colors = z.infer<typeof ColorMap>;
// => Colors = Partial<Record<"red" | "green" | "blue", string>>

// Useful for feature flags
const featureFlagSchema = z.record(z.string(), z.boolean());
// => Dynamic feature flag map: key = flag name, value = enabled/disabled

const flags = featureFlagSchema.parse({
  darkMode: true,
  betaFeatures: false,
  newDashboard: true,
});
// => flags = { darkMode: true, betaFeatures: false, newDashboard: true }
// => type: Record<string, boolean>

// Short form: single argument means string keys
const configSchema = z.record(z.string());
// => Equivalent to z.record(z.string(), z.string())
// => type: Record<string, string>
```

**Key Takeaway**: `z.record(keySchema, valueSchema)` validates dynamic key-value objects where the key set is unknown at design time but values must follow a consistent shape.

**Why It Matters**: Configuration objects, feature flag maps, localization dictionaries, and metadata objects commonly have dynamic keys. Without record validation, you either use `Record<string, any>` which provides no value guarantees, or validate each key individually which is impossible when keys are dynamic. Zod's record schema validates the entire value structure regardless of key count, ensuring consistent value types even for dictionary-style objects.

---

### Example 20: Map Schema

`z.map()` validates JavaScript `Map` instances with typed key and value schemas. Unlike records, Maps support non-string keys.

```typescript
import { z } from "zod";

// Map with string keys and number values
const cacheSchema = z.map(z.string(), z.number());
// => Validates a JavaScript Map object
// => Keys: string, Values: number
// => type: Map<string, number>

const cache = new Map<string, number>();
cache.set("user:123", 42);
cache.set("user:456", 87);

const validCache = cacheSchema.parse(cache);
// => validCache = Map { "user:123" => 42, "user:456" => 87 }
// => type: Map<string, number>

// Map with object keys
const PointSchema = z.object({ x: z.number(), y: z.number() });
const pointCacheSchema = z.map(PointSchema, z.string());
// => Keys: objects with x,y coordinates; Values: labels

// Practical: lookup table
const userRoleMapSchema = z.map(
  z.string().uuid(),
  // => Keys: UUIDs (user IDs)
  z.enum(["admin", "editor", "viewer"]),
  // => Values: role enum
);

const roleMap = new Map([
  ["550e8400-e29b-41d4-a716-446655440000", "admin"],
  ["550e8400-e29b-41d4-a716-446655440001", "viewer"],
]);

const validRoleMap = userRoleMapSchema.parse(roleMap);
// => Both entries validated: keys are UUIDs, values are valid roles
// => type: Map<string, "admin" | "editor" | "viewer">
```

**Key Takeaway**: `z.map(keySchema, valueSchema)` validates JavaScript Map objects. Unlike record schemas, Maps support non-string keys and maintain insertion order.

**Why It Matters**: Maps are the correct JavaScript data structure when you need frequent key-value lookups with non-string keys or when insertion order matters. While records (plain objects) are more common in JSON, Maps appear in in-memory caches, lookup tables, and graph data structures. Validating Maps ensures both keys and values conform to expected types before your application logic operates on them.

---

### Example 21: Set Schema

`z.set()` validates JavaScript `Set` instances containing unique values of the specified type. Chain `.min()` and `.max()` for size constraints.

```typescript
import { z } from "zod";

// Set of strings
const tagSetSchema = z.set(z.string());
// => Validates a JavaScript Set containing strings
// => type: Set<string>

const tags = new Set(["typescript", "zod", "validation"]);

const validTags = tagSetSchema.parse(tags);
// => validTags = Set { "typescript", "zod", "validation" }
// => type: Set<string>
// => Duplicates are inherently impossible — Set guarantees uniqueness

// Set with size constraints
const permissionSetSchema = z.set(z.string()).min(1).max(5);
// => .min(1) — must have at least 1 permission
// => .max(5) — cannot have more than 5 permissions

const validPermissions = permissionSetSchema.parse(new Set(["read", "write", "delete"]));
// => validPermissions = Set { "read", "write", "delete" }

// Set of numbers — unique IDs
const idSetSchema = z.set(z.number().int().positive());
// => Set of positive integers — useful for selected item IDs

const selectedIds = idSetSchema.parse(new Set([1, 5, 23, 42]));
// => selectedIds = Set { 1, 5, 23, 42 }
// => type: Set<number>

// Non-empty set constraint
const nonEmptySetSchema = z.set(z.string()).nonempty();
// => Guarantees at least one element in set
```

**Key Takeaway**: `z.set(schema)` validates JavaScript Set objects where all elements match the inner schema. Sets guarantee uniqueness — Zod validates element types, not uniqueness which is enforced by Set itself.

**Why It Matters**: Sets are the semantically correct data structure for collections requiring uniqueness — selected items, granted permissions, visited nodes in a graph. Using arrays for unique collections requires manual deduplication. Zod's set schema validates that you're working with actual Set objects and that all elements have the correct type, enabling type-safe access to Set methods like `.has()`, `.add()`, and iteration.

---

## Group 6: Parse Methods and Type Inference

### Example 22: parse vs safeParse

Zod provides two parsing modes: `parse()` throws on failure while `safeParse()` returns a result object. Choose based on whether you want to handle errors or let them propagate.

```typescript
import { z } from "zod";

const AgeSchema = z.number().int().min(0).max(150);
// => Integer in range 0-150

// parse() — throws ZodError on validation failure
try {
  const age = AgeSchema.parse(25);
  // => age = 25 (type: number)
  console.log("Valid age:", age);
  // => Output: Valid age: 25
} catch (error) {
  // => Only runs if validation fails
  console.error("Validation failed:", error);
}

// safeParse() — returns result object, never throws
const result1 = AgeSchema.safeParse(25);
// => result1 = { success: true, data: 25 }
// => TypeScript discriminated union: success/failure

if (result1.success) {
  console.log("Valid age:", result1.data);
  // => result1.data is typed as number (narrowed by success check)
  // => Output: Valid age: 25
}

const result2 = AgeSchema.safeParse(-5);
// => result2 = { success: false, error: ZodError }
// => result2.data is not accessible when success is false

if (!result2.success) {
  console.log("Error:", result2.error.message);
  // => result2.error is typed as ZodError (narrowed by !success check)
  // => Output: Error: Number must be greater than or equal to 0
}
```

**Key Takeaway**: Use `parse()` when validation failure should throw (server middleware, startup validation). Use `safeParse()` when you need to handle errors explicitly (form validation, API input processing).

**Why It Matters**: The choice between `parse()` and `safeParse()` reflects error handling philosophy. In middleware that validates request bodies, an invalid input should throw and be caught by your error handler — `parse()` is appropriate. In form validation where you want to show error messages to users without crashing, `safeParse()` returns structured error information. The `safeParse()` result is a discriminated union that TypeScript fully understands, enabling type-safe error handling without try/catch.

---

### Example 23: Type Inference with z.infer

`z.infer<typeof Schema>` extracts the TypeScript type from a Zod schema. This eliminates the need to maintain separate interface definitions.

```typescript
import { z } from "zod";

// Schema first — type derived automatically
const ProductSchema = z.object({
  id: z.string().uuid(),
  name: z.string().min(1),
  price: z.number().positive(),
  tags: z.array(z.string()),
  isAvailable: z.boolean().default(true),
});

// Derive TypeScript type from schema
type Product = z.infer<typeof ProductSchema>;
// => Product = {
// =>   id: string;
// =>   name: string;
// =>   price: number;
// =>   tags: string[];
// =>   isAvailable: boolean; (not optional — default resolves undefined)
// => }

// Type used for TypeScript code — schema used for validation
function displayProduct(product: Product): string {
  // => product is fully typed — TypeScript knows all fields
  return `${product.name}: $${product.price}`;
  // => Accessing product.nonexistent would be a compile error
}

const rawData = { id: "550e8400-e29b-41d4-a716-446655440000", name: "Book", price: 15.99, tags: ["education"] };
const product = ProductSchema.parse(rawData);
// => product is typed as Product after validation
// => TypeScript infers the exact schema type

console.log(displayProduct(product));
// => Output: Book: $15.99

// z.input for pre-transform types
type ProductInput = z.input<typeof ProductSchema>;
// => ProductInput = { id: string; name: string; price: number; tags: string[]; isAvailable?: boolean | undefined }
// => z.input captures type BEFORE defaults/transforms are applied
```

**Key Takeaway**: `z.infer<typeof Schema>` derives the TypeScript type from a schema. `z.input<>` captures pre-transform types; `z.output<>` captures post-transform types (same as `z.infer<>`).

**Why It Matters**: Maintaining parallel schema and type definitions creates drift — when you update the schema, you must remember to update the interface. `z.infer<>` eliminates this source of bugs by making the TypeScript type a derived artifact of the schema. The schema becomes the single source of truth for both validation and typing. This principle — define once, use everywhere — dramatically reduces the surface area for validation/type mismatch bugs in large codebases.

---

### Example 24: ZodError Structure

When validation fails, Zod throws a `ZodError` containing structured information about every validation failure. Understanding this structure is essential for good error messages.

```typescript
import { z, ZodError } from "zod";

const UserSchema = z.object({
  name: z.string().min(2),
  age: z.number().int().min(0),
  email: z.string().email(),
});

// Attempt to parse invalid data
const result = UserSchema.safeParse({
  name: "A", // => Too short (min 2)
  age: -1, // => Negative (min 0)
  email: "not-email", // => Invalid email format
});

if (!result.success) {
  const error: ZodError = result.error;
  // => ZodError contains array of ZodIssue objects

  console.log(error.issues.length);
  // => Output: 3 (one issue per failed field)

  error.issues.forEach((issue) => {
    console.log(issue.path, issue.message);
    // => issue.path: array of keys indicating which field failed
    // => issue.message: human-readable error description
  });
  // => Output: ["name"] "String must contain at least 2 character(s)"
  // => Output: ["age"] "Number must be greater than or equal to 0"
  // => Output: ["email"] "Invalid email"

  // Flatten to simple key-value error map
  const flat = error.flatten();
  // => flat.fieldErrors = { name: ["..."], age: ["..."], email: ["..."] }
  // => flat.formErrors = [] (top-level errors)
  console.log(flat.fieldErrors);
  // => Output: { name: [...], age: [...], email: [...] }
}
```

**Key Takeaway**: `ZodError.issues` is an array of structured validation failures. Use `.flatten()` for simple key-value error maps or `.format()` for nested error trees.

**Why It Matters**: Good error messages are the difference between a frustrating user experience and a helpful one. Zod's structured errors contain the exact path to each failed field, making it trivial to map validation errors back to form inputs. The `.flatten()` method transforms the structured error tree into the simple `{ fieldName: ["error message"] }` format expected by most form libraries, while `.format()` produces nested trees for complex nested object validation.

---

### Example 25: Async Validation

`parseAsync()` and `safeParseAsync()` support asynchronous validation — essential when validation requires database lookups or API calls.

```typescript
import { z } from "zod";

// Schema with async refinement
const UniqueEmailSchema = z
  .string()
  .email()
  .refine(
    // => .refine() adds custom validation — see Intermediate for full coverage
    async (email) => {
      // => Async validation: check if email is available
      // => In production, this would query a database
      const existingEmails = ["taken@example.com", "used@example.com"];
      // => Simulated database of taken emails
      return !existingEmails.includes(email);
      // => Returns true if email is available (validation passes)
      // => Returns false if email is taken (validation fails)
    },
    "Email is already taken",
    // => Error message when validation returns false
  );

async function validateEmail(email: string): Promise<void> {
  // Parse async — await required for async refinements
  const result = await UniqueEmailSchema.safeParseAsync(email);
  // => safeParseAsync returns Promise<SafeParseReturnType>
  // => await resolves the Promise

  if (result.success) {
    console.log("Email available:", result.data);
    // => result.data is typed as string
  } else {
    console.log("Error:", result.error.issues[0].message);
    // => Accesses first issue's message
  }
}

// Call the async validator
validateEmail("new@example.com").then(() => {
  // => Output: Email available: new@example.com
});

validateEmail("taken@example.com").then(() => {
  // => Output: Error: Email is already taken
});
```

**Key Takeaway**: Use `parseAsync()` and `safeParseAsync()` when schemas include async refinements (database lookups, API validation). Sync schemas can also use async methods without performance penalty.

**Why It Matters**: Registration flows, username availability checks, and permission validation commonly require database queries during validation. Using synchronous parse methods with async refinements causes silent errors or incorrect results. Zod's async parse methods properly await all refinement promises, ensuring database-backed validation works correctly. The async API mirrors the synchronous API exactly — same result shape, same TypeScript types — making adoption straightforward.

---

## Group 7: Schema Composition Basics

### Example 26: Schema Intersection

`z.intersection()` (or `.and()`) combines two schemas requiring both to pass. The inferred type is the intersection of both types.

```typescript
import { z } from "zod";

// Two separate schemas
const BaseEntitySchema = z.object({
  id: z.string().uuid(),
  // => All entities have UUID id
  createdAt: z.date(),
  // => All entities have creation timestamp
  updatedAt: z.date(),
  // => All entities have update timestamp
});

const ProductDetailsSchema = z.object({
  name: z.string().min(1),
  // => Product name
  price: z.number().positive(),
  // => Positive price
  stock: z.number().int().nonnegative(),
  // => Non-negative integer stock count
});

// Intersection: input must satisfy BOTH schemas
const ProductSchema = z.intersection(BaseEntitySchema, ProductDetailsSchema);
// => Equivalent: BaseEntitySchema.and(ProductDetailsSchema)
// => type: { id: string; createdAt: Date; updatedAt: Date } & { name: string; price: number; stock: number }

type Product = z.infer<typeof ProductSchema>;
// => Product = {
// =>   id: string;
// =>   createdAt: Date;
// =>   updatedAt: Date;
// =>   name: string;
// =>   price: number;
// =>   stock: number;
// => }

const product = ProductSchema.parse({
  id: "550e8400-e29b-41d4-a716-446655440000",
  createdAt: new Date(),
  updatedAt: new Date(),
  name: "TypeScript Book",
  price: 49.99,
  stock: 100,
});
// => All fields from both schemas required and validated
```

**Key Takeaway**: `z.intersection()` requires input to satisfy both schemas simultaneously. Prefer `.merge()` on object schemas (see Intermediate) for cleaner composition of object types.

**Why It Matters**: Intersection types model "has all the properties of A and all the properties of B" — essential for entity schemas with common base fields (id, timestamps) and domain-specific fields (name, price). While `z.intersection()` works for any two schemas, `.merge()` on object schemas is more ergonomic for object composition and produces better TypeScript error messages. Understanding the primitive intersection operation provides the conceptual foundation for the higher-level `.merge()` and `.extend()` patterns.

---

### Example 27: Schema Branching with .or()

`.or()` creates a union schema inline as a method chain. It's equivalent to `z.union([schemaA, schemaB])` but reads more naturally.

```typescript
import { z } from "zod";

// Using .or() for inline union
const stringOrNumberSchema = z.string().or(z.number());
// => Equivalent to z.union([z.string(), z.number()])
// => type: string | number

const fromString = stringOrNumberSchema.parse("hello");
// => fromString = "hello" (type: string | number)

const fromNumber = stringOrNumberSchema.parse(42);
// => fromNumber = 42 (type: string | number)

// Practical: flexible ID field
const FlexibleIdSchema = z.object({
  id: z.string().uuid().or(z.number().int().positive()),
  // => id can be UUID string or positive integer
  name: z.string(),
});

type FlexibleId = z.infer<typeof FlexibleIdSchema>;
// => { id: string | number; name: string }

// Chaining multiple .or()
const multiTypeSchema = z.string().or(z.number()).or(z.boolean());
// => type: string | number | boolean
// => Equivalent to z.union([z.string(), z.number(), z.boolean()])

// Nullable as union — equivalent forms
const nullableStringA = z.string().nullable();
// => type: string | null

const nullableStringB = z.string().or(z.null());
// => Equivalent: type: string | null
// => .nullable() is syntactic sugar for this pattern
```

**Key Takeaway**: `.or(schema)` is the method-chain form of `z.union()`. Use it for inline unions in object definitions; use `z.union([])` when defining unions as standalone schemas.

**Why It Matters**: Method chaining makes complex schema definitions more readable, especially when defining fields within object schemas. The choice between `.or()` and `z.union()` is stylistic, but method chaining reads as "this field can be X or Y" which closely mirrors how developers think about flexible field types. Understanding both forms lets you write Zod schemas that match the natural language description of your data requirements.

---

### Example 28: Schema Reuse and Composition

Schemas are reusable TypeScript values. Composing complex schemas from simpler ones reduces duplication and improves maintainability.

```typescript
import { z } from "zod";

// Primitive building blocks
const UUIDSchema = z.string().uuid();
// => Reusable UUID validator

const EmailSchema = z.string().email().toLowerCase();
// => .toLowerCase() transforms email to lowercase before validation
// => Ensures consistent storage format

const TimestampSchema = z.object({
  createdAt: z.date(),
  // => Creation timestamp
  updatedAt: z.date(),
  // => Last update timestamp
});

// Compose from building blocks
const UserBaseSchema = z.object({
  id: UUIDSchema,
  // => Reuses UUID validator
  email: EmailSchema,
  // => Reuses email validator with lowercasing
  name: z.string().min(1).max(100),
  // => Name: 1-100 characters
});

// Extend with timestamps
const UserSchema = UserBaseSchema.merge(TimestampSchema);
// => .merge() combines two object schemas
// => UserSchema has all fields from both
// => Full coverage: Intermediate examples cover merge/extend/pick/omit in detail

type User = z.infer<typeof UserSchema>;
// => User = {
// =>   id: string;
// =>   email: string;
// =>   name: string;
// =>   createdAt: Date;
// =>   updatedAt: Date;
// => }

const user = UserSchema.parse({
  id: "550e8400-e29b-41d4-a716-446655440000",
  email: "AISHA@EXAMPLE.COM", // => Will be lowercased
  name: "Aisha",
  createdAt: new Date(),
  updatedAt: new Date(),
});
// => user.email = "aisha@example.com" (lowercased by transform)
console.log(user.email);
// => Output: aisha@example.com
```

**Key Takeaway**: Define schemas as reusable constants and compose them using `.merge()`, `.extend()`, and nesting. Schema composition reduces duplication and ensures consistent validation across related data shapes.

**Why It Matters**: Large applications have dozens of entity types sharing common fields — timestamps, soft-delete flags, audit fields. Defining these fields once in a base schema and composing them into entity schemas ensures consistent validation across the entire application. When a common field's validation rule changes — expanding a name's max length, adding a new email format constraint — updating the shared schema propagates the change everywhere it's used. This DRY principle applied to schemas dramatically reduces the maintenance burden of validation logic.
