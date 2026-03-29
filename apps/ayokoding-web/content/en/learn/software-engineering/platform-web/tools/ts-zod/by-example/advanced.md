---
title: "Advanced"
weight: 10000003
date: 2026-03-25T00:00:00+07:00
draft: false
description: "Master expert Zod patterns through 25 annotated examples covering branded types, custom ZodType, OpenAPI generation, tRPC integration, React Hook Form, performance optimization, generic schemas, and migration patterns"
tags: ["zod", "typescript", "validation", "schema", "tutorial", "by-example", "advanced"]
---

This advanced tutorial covers expert Zod patterns through 25 heavily annotated examples. Each example assumes you understand intermediate concepts (refinements, transforms, discriminated unions, schema composition).

## Prerequisites

Before starting, ensure you understand:

- Zod intermediate examples (Examples 29-55)
- TypeScript advanced patterns (generics, conditional types, utility types)
- tRPC concepts (optional, for integration examples)
- React Hook Form basics (optional, for form integration examples)

## Group 1: Custom Types and Extensions

### Example 56: Custom ZodType

Extend Zod with a fully custom schema type by subclassing `ZodType`. This enables schemas for data structures that Zod doesn't natively support.

```typescript
import { z, ZodType, ZodTypeDef, ZodParsedType } from "zod";

// Custom schema for BigInt validation
class ZodBigInt extends ZodType<bigint, ZodTypeDef, bigint> {
  // => ZodType<Output, Def, Input>
  // => Output: bigint (what parse() returns)
  // => Input: bigint (what parse() accepts)

  _parse(input: z.ParseInput): z.ParseReturnType<bigint> {
    // => _parse() is the core validation method
    const { ctx } = this._processInputParams(input);
    // => ctx contains parsed input and issue-adding methods

    if (typeof ctx.data !== "bigint") {
      // => Validate: input must be a JavaScript BigInt
      this._addIssueToContext(ctx, {
        code: z.ZodIssueCode.invalid_type,
        expected: ZodParsedType.bigint,
        // => expected: what type was expected
        received: ctx.parsedType,
        // => received: what type was actually provided
      });
      return z.INVALID;
      // => z.INVALID: sentinel value for failed validation
    }

    return z.OK(ctx.data);
    // => z.OK(value): wraps successful validation result
  }
}

// Factory function following Zod's z.* naming convention
const zodBigInt = () => new ZodBigInt({ typeName: "ZodBigInt" as z.ZodFirstPartyTypeKind });
// => Creates instances with the custom type definition

const BigIntSchema = zodBigInt();
// => ZodBigInt instance — behaves like any Zod schema

const big = BigIntSchema.parse(9007199254740992n);
// => 9007199254740992n is a JavaScript BigInt literal
// => big = 9007199254740992n (type: bigint)

try {
  BigIntSchema.parse(42);
  // => 42 is a number, not bigint — fails custom validation
} catch (error) {
  console.log("Not a BigInt");
  // => Output: Not a BigInt
}
```

**Key Takeaway**: Subclass `ZodType<Output, Def, Input>` and implement `_parse()` to create fully custom Zod schemas with proper error reporting and TypeScript type inference.

**Why It Matters**: Zod's built-in types cover most use cases, but some domains require custom data types — BigInt for arbitrary precision arithmetic, custom binary formats, specialized date types, or domain-specific value objects. By extending `ZodType`, custom schemas participate fully in the Zod ecosystem: they chain with `.optional()`, `.nullable()`, `.transform()`, and all other modifiers. Tooling that works with Zod schemas (OpenAPI generators, form libraries) can also interact with custom types through the standard Zod interface.

---

### Example 57: Generic Schema Factory

Generic schema factories create reusable schema patterns parameterized by type, enabling DRY validation for common wrapper types.

```typescript
import { z } from "zod";

// Generic paginated response factory
function createPaginatedSchema<T extends z.ZodTypeAny>(itemSchema: T) {
  // => T: any Zod schema type
  // => itemSchema: the schema for items in the list
  return z.object({
    items: z.array(itemSchema),
    // => items: array of T's inferred type
    total: z.number().int().nonnegative(),
    // => Total count across all pages
    page: z.number().int().positive(),
    // => Current page number
    limit: z.number().int().positive().max(100),
    // => Items per page
    hasNextPage: z.boolean(),
    // => Whether more pages exist
  });
}

// Product and User schemas
const ProductSchema = z.object({ id: z.string().uuid(), name: z.string(), price: z.number().positive() });
const UserSchema = z.object({ id: z.string().uuid(), name: z.string(), email: z.string().email() });

// Create paginated variants
const PaginatedProductsSchema = createPaginatedSchema(ProductSchema);
const PaginatedUsersSchema = createPaginatedSchema(UserSchema);
// => Each has items typed to the specific entity

type PaginatedProducts = z.infer<typeof PaginatedProductsSchema>;
// => { items: { id: string; name: string; price: number }[]; total: number; page: number; ... }

type PaginatedUsers = z.infer<typeof PaginatedUsersSchema>;
// => { items: { id: string; name: string; email: string }[]; total: number; page: number; ... }

// Generic API response wrapper
function createApiResponse<T extends z.ZodTypeAny>(dataSchema: T) {
  return z.discriminatedUnion("success", [
    z.object({ success: z.literal(true), data: dataSchema }),
    // => Success: contains typed data
    z.object({ success: z.literal(false), error: z.string(), code: z.number().optional() }),
    // => Failure: contains error info
  ]);
}

const ProductResponseSchema = createApiResponse(ProductSchema);
// => Discriminated union: success with Product data OR error with message
```

**Key Takeaway**: Generic schema factories parameterize Zod schemas with `<T extends z.ZodTypeAny>`, creating type-safe reusable patterns. `z.infer<>` correctly propagates through generic parameters.

**Why It Matters**: Production APIs consistently wrap responses in common envelopes — pagination containers, success/error wrappers, versioned response formats. Without generic schema factories, you duplicate the envelope structure for every entity type. Generic factories define the envelope once and stamp it for any item type. TypeScript's generic type inference makes this transparent — when you use `PaginatedProductsSchema`, TypeScript knows `items` contains products without explicit annotation. This pattern dramatically reduces schema boilerplate in large applications.

---

### Example 58: Schema-to-OpenAPI with zod-to-json-schema

The `zod-to-json-schema` library converts Zod schemas to JSON Schema (OpenAPI-compatible format). This enables schema-driven API documentation.

```typescript
import { z } from "zod";
// Note: requires npm install zod-to-json-schema
// import { zodToJsonSchema } from "zod-to-json-schema";
// => For demonstration, we show the pattern and expected output

// Define schemas with .describe() for rich OpenAPI documentation
const CreateProductSchema = z.object({
  name: z.string().min(1).max(200).describe("Product display name"),
  // => .describe() becomes OpenAPI description property
  price: z.number().positive().describe("Price in USD, must be positive"),
  description: z.string().max(2000).optional().describe("Optional product description"),
  categoryId: z.string().uuid().describe("UUID of the product's category"),
  tags: z.array(z.string()).max(10).default([]).describe("Product tags for search"),
  status: z.enum(["active", "inactive", "draft"]).default("draft").describe("Publication status"),
});

// zodToJsonSchema converts to JSON Schema format
// const jsonSchema = zodToJsonSchema(CreateProductSchema, { name: "CreateProduct" });
// => Returns:
// => {
// =>   "$schema": "http://json-schema.org/draft-07/schema#",
// =>   "title": "CreateProduct",
// =>   "type": "object",
// =>   "properties": {
// =>     "name": { "type": "string", "minLength": 1, "maxLength": 200, "description": "Product display name" },
// =>     "price": { "type": "number", "exclusiveMinimum": 0, "description": "Price in USD, must be positive" },
// =>     "tags": { "type": "array", "items": { "type": "string" }, "maxItems": 10, "default": [] },
// =>     "status": { "type": "string", "enum": ["active", "inactive", "draft"], "default": "draft" }
// =>   },
// =>   "required": ["name", "price", "categoryId", "status"]
// => }

// The generated JSON Schema can be embedded in OpenAPI specs:
// paths:
//   /products:
//     post:
//       requestBody:
//         content:
//           application/json:
//             schema: $ref: '#/components/schemas/CreateProduct'

type CreateProduct = z.infer<typeof CreateProductSchema>;
// => TypeScript type from same schema used for documentation
// => Single source of truth: schema defines both types and docs
```

**Key Takeaway**: `zod-to-json-schema` converts Zod schemas to JSON Schema / OpenAPI format. Add `.describe()` annotations to schemas to enrich generated documentation with field descriptions.

**Why It Matters**: API documentation that drifts from implementation is one of the most common developer experience problems. When Zod schemas are the source of truth for both TypeScript types AND OpenAPI documentation, they cannot diverge — adding a field or changing a constraint automatically updates the documentation. This eliminates the entire category of "docs say X but API does Y" bugs. The `.describe()` method transforms Zod schemas from pure validation tools into living documentation that auto-updates with every schema change.

---

## Group 2: Framework Integrations

### Example 59: tRPC Integration

tRPC uses Zod schemas for automatic input validation, output typing, and type-safe procedure definitions across the full stack.

```typescript
import { z } from "zod";
// Note: requires npm install @trpc/server @trpc/client
// This example shows the pattern — actual tRPC setup requires framework config

// Input schemas for tRPC procedures
const GetUserInput = z.object({
  userId: z.string().uuid("Invalid user ID format"),
  // => Procedure input — validated by tRPC before handler runs
});

const CreateUserInput = z.object({
  name: z.string().min(1).max(100),
  email: z.string().email(),
  role: z.enum(["admin", "editor", "viewer"]).default("viewer"),
});

const UpdateUserInput = z.object({
  userId: z.string().uuid(),
  // => Which user to update
  updates: z
    .object({
      name: z.string().min(1).optional(),
      email: z.string().email().optional(),
      role: z.enum(["admin", "editor", "viewer"]).optional(),
    })
    .refine(
      (updates) => Object.keys(updates).length > 0,
      // => At least one field must be provided for update
      "At least one field must be provided",
    ),
});

// tRPC router definition (conceptual — actual API varies by tRPC version)
// const router = createTRPCRouter({
//   getUser: publicProcedure
//     .input(GetUserInput)
//     // => tRPC validates input before calling resolver
//     // => resolver receives typed input: { userId: string }
//     .query(async ({ input }) => {
//       // => input.userId is typed as string (UUID validated)
//       return await db.users.findById(input.userId);
//     }),
//
//   createUser: protectedProcedure
//     .input(CreateUserInput)
//     .mutation(async ({ input }) => {
//       // => input.name, input.email, input.role all validated
//       return await db.users.create(input);
//     }),
// });

// Client-side: input types inferred from schemas
type GetUserInput = z.infer<typeof GetUserInput>;
// => { userId: string } — same type used client and server
type CreateUserInput = z.infer<typeof CreateUserInput>;
// => { name: string; email: string; role: "admin" | "editor" | "viewer" }
```

**Key Takeaway**: Define tRPC procedure inputs as Zod schemas. tRPC automatically validates inputs before passing to resolvers, and `z.infer<>` provides the same TypeScript types client-side and server-side.

**Why It Matters**: tRPC's primary value proposition is end-to-end type safety — a single change to a procedure's input schema immediately reflects in client-side TypeScript types. Zod is tRPC's preferred validation library because `z.infer<>` works equally well for server-side handler typing and client-side call typing. This creates a true single source of truth for API contracts: the Zod schema on the server is the contract, and TypeScript enforces it everywhere the procedure is called without any code generation step.

---

### Example 60: React Hook Form Integration

React Hook Form's `zodResolver` adapter integrates Zod schemas with form state management, providing schema-driven form validation.

```typescript
import { z } from "zod";
// Note: requires npm install react-hook-form @hookform/resolvers
// import { useForm } from "react-hook-form";
// import { zodResolver } from "@hookform/resolvers/zod";

// Form schema
const RegistrationSchema = z
  .object({
    firstName: z.string().min(1, "First name is required"),
    lastName: z.string().min(1, "Last name is required"),
    email: z.string().email("Invalid email address"),
    password: z
      .string()
      .min(8, "Password must be at least 8 characters")
      .regex(/[A-Z]/, "Must contain uppercase letter")
      .regex(/[0-9]/, "Must contain a number"),
    confirmPassword: z.string(),
    agreeToTerms: z.literal(true, {
      errorMap: () => ({ message: "You must accept the terms" }),
    }),
  })
  .refine(
    (data) => data.password === data.confirmPassword,
    { message: "Passwords do not match", path: ["confirmPassword"] },
    // => Cross-field validation: password match
  );

type RegistrationFormValues = z.infer<typeof RegistrationSchema>;
// => Form values type: { firstName, lastName, email, password, confirmPassword, agreeToTerms }
// => Used for typed form field access

// React Hook Form usage (JSX omitted — conceptual demonstration)
// const { register, handleSubmit, formState: { errors } } = useForm<RegistrationFormValues>({
//   resolver: zodResolver(RegistrationSchema),
//   // => zodResolver bridges Zod validation with React Hook Form
//   // => Runs schema.safeParse on form submission
//   // => Maps Zod errors to React Hook Form error structure
// });

// The resolver maps Zod field errors to React Hook Form errors:
// errors.email?.message → "Invalid email address"
// errors.confirmPassword?.message → "Passwords do not match"
// errors.agreeToTerms?.message → "You must accept the terms"

// Submit handler receives validated, typed data
// const onSubmit = (data: RegistrationFormValues) => {
//   // => data is TypeScript-typed as RegistrationFormValues
//   // => All Zod validations passed before this runs
//   await registerUser(data);
// };
```

**Key Takeaway**: `zodResolver(schema)` from `@hookform/resolvers/zod` integrates Zod validation into React Hook Form. Form values type comes from `z.infer<>`, and field errors map directly from Zod issues.

**Why It Matters**: React Hook Form manages form state; Zod manages validation logic. The `zodResolver` adapter connects them without duplication — you define validation once in the schema, and React Hook Form displays errors automatically per-field. The schema serves triple duty: TypeScript type for form values, validation logic for submission, and error source for UI display. This integration eliminates the custom validation functions, manual error state management, and type assertions that previously characterized form handling in React applications.

---

### Example 61: Next.js Server Action Validation

Validate Next.js Server Actions using Zod to secure server-side form processing. Server actions receive untyped `FormData` — Zod converts and validates.

```typescript
"use server";
// => Next.js Server Action directive
import { z } from "zod";

// Server action schema
const ContactActionSchema = z.object({
  name: z.string().min(1, "Name is required").max(100),
  email: z.string().email("Invalid email address"),
  subject: z.string().min(1, "Subject is required").max(200),
  message: z.string().min(20, "Message too short").max(2000),
});

type ContactActionInput = z.infer<typeof ContactActionSchema>;
// => { name: string; email: string; subject: string; message: string }

// Server action return type
type ActionResult = { success: true; messageId: string } | { success: false; errors: Record<string, string[]> };

// Server action with Zod validation
export async function submitContactForm(formData: FormData): Promise<ActionResult> {
  // => FormData: raw form submission (all values are strings)

  // Extract FormData to plain object
  const rawData = {
    name: formData.get("name"),
    // => formData.get() returns string | null
    email: formData.get("email"),
    subject: formData.get("subject"),
    message: formData.get("message"),
  };

  // Validate with Zod
  const result = ContactActionSchema.safeParse(rawData);
  // => Validates each string field against schema

  if (!result.success) {
    return {
      success: false,
      errors: result.error.flatten().fieldErrors as Record<string, string[]>,
      // => Return field errors for form display
    };
  }

  // Process validated data
  const data: ContactActionInput = result.data;
  // => data is typed: name, email, subject, message all verified
  console.log("Processing contact:", data.email);
  // => Downstream code uses typed, validated data only

  return { success: true, messageId: "msg-" + Date.now() };
}
```

**Key Takeaway**: Extract `FormData` fields to a plain object, then use `safeParse()` to validate and type them. Return structured errors for client-side display.

**Why It Matters**: Next.js Server Actions accept raw `FormData` where all values are strings or null. Without validation, server code receives untyped data from untrusted form submissions. Zod transforms this raw FormData into typed, validated domain objects, applying the same validation logic used in client-side form validation for consistent behavior. Server-side validation is essential even when client validation exists — users can bypass client-side checks by submitting requests directly.

---

## Group 3: Performance and Optimization

### Example 62: Schema Caching and Reuse

Zod schemas are immutable objects — create them once at module scope rather than recreating them on every function call or request.

```typescript
import { z } from "zod";

// Anti-pattern: creating schemas inside functions (repeated on every call)
function validateUserBad(data: unknown) {
  const schema = z.object({
    name: z.string(),
    email: z.string().email(),
    // => New schema object created on EVERY validateUserBad() call
    // => Wastes memory and computation for complex schemas
  });
  return schema.parse(data);
}

// Correct pattern: schemas at module scope (created once)
const UserSchema = z.object({
  name: z.string(),
  email: z.string().email(),
  // => Schema created ONCE when module loads
  // => Reused for every validation call
});

function validateUserGood(data: unknown) {
  return UserSchema.parse(data);
  // => References the module-scope schema
  // => No schema construction overhead per call
}

// For schemas used in multiple files, export from a shared module
// schemas/user.ts
export const CreateUserSchema = z.object({
  name: z.string().min(1),
  email: z.string().email(),
  password: z.string().min(8),
});

export type CreateUser = z.infer<typeof CreateUserSchema>;
// => Both schema and type exported together
// => Import both from one location: no sync issues

// Lazy schemas for circular references are an exception
// z.lazy() callbacks run on each parse by design
// This is unavoidable for recursive schemas
```

**Key Takeaway**: Always define Zod schemas at module scope, not inside functions. Module-scope schemas are created once and reused, avoiding repeated object allocation during request handling.

**Why It Matters**: In high-throughput API handlers, a schema defined inside a function creates hundreds of ZodObject instances per second — each with its own property schemas, refinements, and transforms. Module-scope schemas pay the construction cost once at startup. For complex schemas with many refinements, transforms, and nested objects, the construction cost is non-trivial. This optimization requires no architectural changes — just moving schema definitions from inside functions to module scope.

---

### Example 63: Lazy Validation with safeParse

Use `safeParse()` strategically to avoid redundant validation. Validate at boundaries but avoid re-validating already-validated data.

```typescript
import { z } from "zod";

const OrderSchema = z
  .object({
    orderId: z.string().uuid(),
    customerId: z.string().uuid(),
    items: z
      .array(
        z.object({
          productId: z.string().uuid(),
          quantity: z.number().int().positive(),
          unitPrice: z.number().positive(),
        }),
      )
      .min(1),
    // => At least one item required
    totalAmount: z.number().positive(),
  })
  .refine(
    (order) => {
      const calculatedTotal = order.items.reduce((sum, item) => sum + item.quantity * item.unitPrice, 0);
      // => Sum quantity * price for all items
      return Math.abs(calculatedTotal - order.totalAmount) < 0.01;
      // => Allow 1 cent rounding difference
    },
    "Total amount doesn't match items sum",
    // => Cross-field: totalAmount must match computed sum
  );

// Validate once at API boundary
function processOrderRequest(rawBody: unknown) {
  const result = OrderSchema.safeParse(rawBody);
  // => Validate at the boundary

  if (!result.success) {
    return { error: "Invalid order", details: result.error.flatten() };
  }

  const order = result.data;
  // => order is typed Order — validated once

  // Pass validated data to services — NO re-validation needed
  processPayment(order);
  updateInventory(order);
  sendConfirmation(order);
  // => Each service receives typed, validated data
  // => Re-validating in each service is wasteful and redundant

  return { success: true };
}

function processPayment(order: z.infer<typeof OrderSchema>) {
  // => TypeScript guarantees type — no runtime check needed here
  console.log("Processing payment:", order.totalAmount);
}

function updateInventory(order: z.infer<typeof OrderSchema>) {
  order.items.forEach((item) => {
    console.log("Reducing stock:", item.productId, item.quantity);
  });
}

function sendConfirmation(order: z.infer<typeof OrderSchema>) {
  console.log("Order confirmed:", order.orderId);
}
```

**Key Takeaway**: Validate once at the system boundary with `safeParse()`. Pass the validated, typed result through to services — TypeScript's type system prevents invalid data from reaching services without validation.

**Why It Matters**: Validation has a cost — CPU for parsing, memory for ZodError construction when errors occur. Re-validating the same data at every service boundary multiplies this cost without adding safety. Validate at the entry point (API endpoint, message queue consumer, form submit handler) where data is untrusted, then pass the Zod-validated TypeScript type through the rest of the call chain. TypeScript's type system is your guarantee in trusted code — Zod handles the untrusted boundary.

---

### Example 64: Batch Validation

Validate arrays of items efficiently using `z.array()` with `.safeParse()`, accumulating errors for bulk import operations.

```typescript
import { z } from "zod";

const ProductRowSchema = z.object({
  name: z.string().min(1).max(200),
  price: z.number().positive(),
  sku: z.string().regex(/^[A-Z]{2}-\d{6}$/, "SKU format: XX-123456"),
  // => SKU: two uppercase letters, dash, six digits
  category: z.enum(["electronics", "clothing", "food", "home"]),
});

type ProductRow = z.infer<typeof ProductRowSchema>;

interface BulkImportResult {
  successful: ProductRow[];
  // => Valid rows that were processed
  failed: { row: number; errors: Record<string, string[]> }[];
  // => Failed rows with row number and field errors
}

// Process CSV import — collect all errors before returning
function bulkImportProducts(rawRows: unknown[]): BulkImportResult {
  const successful: ProductRow[] = [];
  const failed: { row: number; errors: Record<string, string[]> }[] = [];

  rawRows.forEach((row, index) => {
    const result = ProductRowSchema.safeParse(row);
    // => Validate each row independently

    if (result.success) {
      successful.push(result.data);
      // => Collect valid rows
    } else {
      failed.push({
        row: index + 1,
        // => 1-indexed row number for user-facing messages
        errors: result.error.flatten().fieldErrors as Record<string, string[]>,
        // => Field-level errors for this row
      });
    }
  });

  return { successful, failed };
  // => Report: how many succeeded, which rows failed and why
}

const result = bulkImportProducts([
  { name: "Laptop", price: 999, sku: "EL-123456", category: "electronics" },
  { name: "", price: -10, sku: "INVALID", category: "books" },
  // => Row 2: multiple validation failures
  { name: "T-Shirt", price: 29.99, sku: "CL-789012", category: "clothing" },
]);

console.log("Successful:", result.successful.length);
// => Output: Successful: 2
console.log(
  "Failed rows:",
  result.failed.map((f) => f.row),
);
// => Output: Failed rows: [2]
```

**Key Takeaway**: For bulk validation, use `safeParse()` per item inside a loop, collecting successes and failures separately. Report all errors at once rather than stopping at the first failure.

**Why It Matters**: Bulk import operations — CSV uploads, batch API calls, data migrations — must provide comprehensive feedback about all invalid rows, not just the first one. Stopping at the first error in a 10,000-row import forces users to fix errors iteratively, round-tripping to the server for each batch of errors. All-at-once error reporting lets users fix all issues before resubmitting. Separating `successful` from `failed` items also enables partial processing — import the valid rows immediately while reporting which rows need correction.

---

## Group 4: Advanced Patterns

### Example 65: Runtime Type Guards with Zod

Zod `safeParse()` results function as TypeScript type guards. Use them to narrow types in conditional logic.

```typescript
import { z } from "zod";

// Schema definitions for different message types
const TextMessageSchema = z.object({
  type: z.literal("text"),
  content: z.string(),
  senderId: z.string().uuid(),
});

const FileMessageSchema = z.object({
  type: z.literal("file"),
  filename: z.string(),
  size: z.number().int().positive(),
  mimeType: z.string(),
  senderId: z.string().uuid(),
});

type TextMessage = z.infer<typeof TextMessageSchema>;
type FileMessage = z.infer<typeof FileMessageSchema>;

// Type guard using Zod
function isTextMessage(data: unknown): data is TextMessage {
  return TextMessageSchema.safeParse(data).success;
  // => safeParse().success is a boolean that also acts as type predicate
  // => After this function returns true, TypeScript narrows to TextMessage
}

function isFileMessage(data: unknown): data is FileMessage {
  return FileMessageSchema.safeParse(data).success;
  // => Same pattern for FileMessage
}

// Usage with unknown WebSocket messages
function handleWebSocketMessage(rawMessage: unknown): void {
  if (isTextMessage(rawMessage)) {
    console.log("Text from", rawMessage.senderId, ":", rawMessage.content);
    // => TypeScript knows rawMessage is TextMessage here
    // => rawMessage.content is accessible (type: string)
  } else if (isFileMessage(rawMessage)) {
    console.log("File from", rawMessage.senderId, ":", rawMessage.filename);
    // => TypeScript knows rawMessage is FileMessage here
    // => rawMessage.filename is accessible (type: string)
  } else {
    console.warn("Unknown message format");
    // => Neither schema matched
  }
}
```

**Key Takeaway**: Zod `safeParse().success` is a boolean that TypeScript recognizes as a type guard when used in a `data is T` function. Combine with union schemas for runtime type discrimination.

**Why It Matters**: WebSocket handlers, event processors, and message bus consumers receive `unknown` data that needs both runtime validation and TypeScript type narrowing. Custom type guard functions built on `safeParse()` provide both simultaneously — the Zod schema validates the structure, and the type predicate tells TypeScript which type the data is. This pattern replaces brittle `instanceof` checks and manual property existence tests with declarative schema-based type discrimination.

---

### Example 66: Composable Validation Middleware

Build a composable validation middleware system using Zod schemas as first-class configuration objects.

```typescript
import { z } from "zod";

// Generic middleware type
type Middleware<TInput, TOutput = TInput> = (input: TInput) => TOutput;

// Schema-based validation middleware factory
function validate<T>(schema: z.ZodType<T>): Middleware<unknown, T> {
  // => Takes a Zod schema, returns a middleware function
  return (input: unknown): T => {
    const result = schema.safeParse(input);
    // => Validate input against schema

    if (!result.success) {
      const message = result.error.issues.map((i) => `${i.path.join(".")}: ${i.message}`).join("; ");
      // => Format all issues into one error message
      throw new Error(`Validation failed: ${message}`);
      // => Throw for upstream error handling
    }

    return result.data;
    // => Return typed, validated data
  };
}

// Compose multiple validation steps
function compose<A, B, C>(f: Middleware<A, B>, g: Middleware<B, C>): Middleware<A, C> {
  // => Compose two middleware functions
  return (input: A): C => g(f(input));
  // => Run f first, then pass result to g
}

// Schema definitions
const RawInputSchema = z.object({
  userId: z.string(),
  amount: z.string(),
  // => Raw: amount comes as string from query params
});

const ProcessedSchema = z.object({
  userId: z.string().uuid(),
  // => Validated UUID
  amount: z.coerce.number().positive(),
  // => Coerced to number and validated positive
});

// Validation pipeline
const validateRaw = validate(RawInputSchema);
// => First stage: validates presence of fields
const validateProcessed = validate(ProcessedSchema);
// => Second stage: validates types and constraints

const fullPipeline = compose(validateRaw, validateProcessed);
// => Both stages composed into one function

const validated = fullPipeline({
  userId: "550e8400-e29b-41d4-a716-446655440000",
  amount: "99.99",
});
// => Stage 1: validates object has userId and amount strings
// => Stage 2: validates UUID and coerces amount to positive number
// => validated = { userId: "...", amount: 99.99 } (typed)
```

**Key Takeaway**: Build composable validation pipelines by treating Zod schemas as configuration objects for middleware factories. `compose()` chains validation stages with type-safe intermediate types.

**Why It Matters**: Complex validation pipelines — normalize input, validate structure, validate business rules, transform output — benefit from composition. Each stage is independently testable and reusable across different endpoints that share validation steps. TypeScript tracks types through the composition chain, ensuring each middleware receives the correctly typed output from the previous stage. This pattern enables building validation pipelines from shared validation components without coupling, following the single-responsibility principle.

---

### Example 67: Versioned Schema Migration

Handle API version migrations by defining transformations between schema versions using Zod's transform and pipe capabilities.

```typescript
import { z } from "zod";

// Version 1 schema (legacy)
const UserV1Schema = z.object({
  id: z.number().int().positive(),
  // => V1: numeric ID
  fullName: z.string(),
  // => V1: single full name field
  emailAddress: z.string().email(),
  // => V1: different field name
});

// Version 2 schema (current)
const UserV2Schema = z.object({
  id: z.string().uuid(),
  // => V2: UUID string ID
  firstName: z.string(),
  // => V2: split name fields
  lastName: z.string(),
  email: z.string().email(),
  // => V2: renamed field
});

type UserV1 = z.infer<typeof UserV1Schema>;
type UserV2 = z.infer<typeof UserV2Schema>;

// Migration transform: V1 → V2
const V1toV2Transform = UserV1Schema.transform((v1User): UserV2 => {
  // => Transform: receives validated V1 user, returns V2 shape
  const nameParts = v1User.fullName.trim().split(" ");
  // => Split full name on space
  return {
    id: `legacy-${v1User.id}`,
    // => Prefix numeric ID with "legacy-" for V2 string format
    firstName: nameParts[0] ?? "",
    // => First word as first name
    lastName: nameParts.slice(1).join(" "),
    // => Remaining words as last name
    email: v1User.emailAddress,
    // => Rename emailAddress → email
  };
});

// Accepts V1 data, returns V2 type
const v2User = V1toV2Transform.parse({
  id: 42,
  fullName: "Aisha Rahman",
  emailAddress: "aisha@example.com",
});
// => v2User = { id: "legacy-42", firstName: "Aisha", lastName: "Rahman", email: "aisha@example.com" }
// => type: UserV2

console.log(v2User.firstName);
// => Output: Aisha

// Accept both versions with union
const AnyVersionSchema = z.union([
  V1toV2Transform,
  // => V1 input: transform to V2
  UserV2Schema,
  // => V2 input: use directly
]);
// => Accepts both V1 and V2, always returns V2
```

**Key Takeaway**: Use `.transform()` to define migrations between schema versions. Union old and new schemas to accept either input format while producing consistent output.

**Why It Matters**: APIs evolve. Client versions in production may send old request formats; stored data may use deprecated field names. Schema migration transforms let you accept multiple input formats while normalizing to a single output type. This is cleaner than handling both versions in business logic — the schema layer absorbs version differences, and all downstream code sees only the current version. The union-with-transform pattern gracefully handles mixed-version environments during rolling deployments.

---

### Example 68: Dynamic Schema Generation

Generate Zod schemas programmatically from configuration objects or JSON Schema definitions.

```typescript
import { z } from "zod";

// Config-driven schema generation
type FieldConfig = {
  type: "string" | "number" | "boolean" | "email" | "uuid";
  required: boolean;
  min?: number;
  max?: number;
  label: string;
};

// Generate Zod schema from field configuration
function fieldToSchema(config: FieldConfig): z.ZodTypeAny {
  // => Returns a Zod schema based on config
  let schema: z.ZodTypeAny;

  switch (config.type) {
    case "string":
      let strSchema = z.string().describe(config.label);
      if (config.min !== undefined) strSchema = strSchema.min(config.min);
      if (config.max !== undefined) strSchema = strSchema.max(config.max);
      schema = strSchema;
      break;
    case "number":
      let numSchema = z.number().describe(config.label);
      if (config.min !== undefined) numSchema = numSchema.min(config.min);
      if (config.max !== undefined) numSchema = numSchema.max(config.max);
      schema = numSchema;
      break;
    case "boolean":
      schema = z.boolean().describe(config.label);
      break;
    case "email":
      schema = z.string().email().describe(config.label);
      break;
    case "uuid":
      schema = z.string().uuid().describe(config.label);
      break;
  }

  return config.required ? schema : schema.optional();
  // => Apply required/optional based on config
}

// Build object schema from field map
function buildSchema(fields: Record<string, FieldConfig>): z.ZodObject<z.ZodRawShape> {
  const shape: z.ZodRawShape = {};
  // => z.ZodRawShape = Record<string, z.ZodTypeAny>

  for (const [fieldName, config] of Object.entries(fields)) {
    shape[fieldName] = fieldToSchema(config);
    // => Generate schema for each field
  }

  return z.object(shape);
  // => Assemble into object schema
}

// Usage: schema from CMS/config
const dynamicSchema = buildSchema({
  title: { type: "string", required: true, min: 1, max: 200, label: "Title" },
  content: { type: "string", required: true, min: 10, label: "Content" },
  publishedAt: { type: "string", required: false, label: "Publish Date" },
});

const article = dynamicSchema.parse({ title: "Hello World", content: "This is my first article" });
// => Validated against dynamically generated schema
```

**Key Takeaway**: Zod schemas are JavaScript objects that can be constructed programmatically. Build schema generation utilities for config-driven forms, CMS content types, and database schema introspection.

**Why It Matters**: Content management systems, form builders, and plugin architectures define data structures at runtime through configuration rather than code. Generating Zod schemas from these configurations brings runtime validation to dynamically-defined structures without hardcoding schemas. This pattern powers low-code platforms where field definitions are stored in databases and schemas are generated fresh for each content type. The resulting schema maintains full Zod functionality — `safeParse()`, type inference, error formatting — even though it was built dynamically.

---

### Example 69: Memoized Validation Results

Cache validation results for expensive schemas to avoid recomputing on repeated calls with the same input.

```typescript
import { z } from "zod";

// Schema with expensive async validation
const ExpensiveSchema = z
  .object({
    email: z.string().email(),
    username: z.string().min(3).max(20),
  })
  .superRefine(async (data, ctx) => {
    // => Simulate database uniqueness check
    const takenEmails = ["taken@example.com", "used@example.com"];
    const takenUsernames = ["admin", "root", "system"];

    if (takenEmails.includes(data.email)) {
      ctx.addIssue({ code: z.ZodIssueCode.custom, message: "Email already taken", path: ["email"] });
    }

    if (takenUsernames.includes(data.username)) {
      ctx.addIssue({ code: z.ZodIssueCode.custom, message: "Username taken", path: ["username"] });
    }
  });

// Simple memoization cache for validation results
const validationCache = new Map<string, z.SafeParseReturnType<unknown, unknown>>();
// => Cache: input fingerprint → result

async function cachedValidate(schema: z.ZodTypeAny, data: unknown): Promise<z.SafeParseReturnType<unknown, unknown>> {
  const cacheKey = JSON.stringify(data);
  // => Fingerprint: JSON serialization of input
  // => Note: only works for JSON-serializable inputs

  if (validationCache.has(cacheKey)) {
    return validationCache.get(cacheKey)!;
    // => Return cached result immediately
  }

  const result = await schema.safeParseAsync(data);
  // => Run actual validation (potentially expensive)

  validationCache.set(cacheKey, result);
  // => Cache for future calls with same input

  return result;
}

// Usage: same input reuses cached result
async function demo() {
  const input = { email: "new@example.com", username: "aisha" };

  const result1 = await cachedValidate(ExpensiveSchema, input);
  // => First call: runs full validation including async refinements

  const result2 = await cachedValidate(ExpensiveSchema, input);
  // => Second call: returns cached result without running validation again

  console.log(result1.success, result2.success);
  // => Output: true true
}
```

**Key Takeaway**: Cache `safeParse()` results keyed by input fingerprint to avoid redundant validation of identical inputs. Particularly valuable for async schemas with database lookups.

**Why It Matters**: Async validation that queries databases (uniqueness checks, permission validation, resource existence) can be called repeatedly with the same input — for example, validating a username as each character is typed. Without caching, every keystroke triggers a database query. A simple memoization layer reduces database load dramatically for repeated inputs. The trade-off is cache staleness — a username that was available when cached may be taken by the time the form submits, so final validation before persistence should bypass the cache.

---

### Example 70: Schema from TypeScript Types (Reverse Inference)

While Zod normally goes schema → TypeScript type, you can create schema definitions that satisfy existing TypeScript interfaces using type constraints.

```typescript
import { z } from "zod";

// Existing TypeScript interface (from external library, generated code, etc.)
interface ExternalProduct {
  id: string;
  name: string;
  price: number;
  tags: string[];
  metadata: Record<string, string>;
}

// Create a Zod schema that satisfies the TypeScript interface
// z.ZodType<T> constrains: schema must be parseable to T
const ExternalProductSchema: z.ZodType<ExternalProduct> = z.object({
  id: z.string(),
  // => Must match ExternalProduct.id: string
  name: z.string(),
  // => Must match ExternalProduct.name: string
  price: z.number(),
  // => Must match ExternalProduct.price: number
  tags: z.array(z.string()),
  // => Must match ExternalProduct.tags: string[]
  metadata: z.record(z.string(), z.string()),
  // => Must match ExternalProduct.metadata: Record<string, string>
  // => TypeScript error if any field mismatches ExternalProduct
});
// => TypeScript verifies schema output matches ExternalProduct
// => Adding a .transform() that changes the shape would cause a compile error

// Type-safe: ExternalProductSchema must output ExternalProduct
const product: ExternalProduct = ExternalProductSchema.parse({
  id: "123",
  name: "Book",
  price: 15.99,
  tags: ["education"],
  metadata: { publisher: "Apress" },
});
// => TypeScript knows product is ExternalProduct (not just z.infer)

// Adding extra validation beyond the interface is safe
const StrictExternalProductSchema: z.ZodType<ExternalProduct> = z.object({
  id: z.string().uuid(),
  // => More restrictive than interface requires — that's fine
  name: z.string().min(1).max(200),
  // => Extra constraints don't break type compatibility
  price: z.number().positive(),
  tags: z.array(z.string()).max(20),
  metadata: z.record(z.string(), z.string()),
});
```

**Key Takeaway**: `const schema: z.ZodType<T>` constrains a Zod schema to parse to a specific TypeScript type. TypeScript verifies schema-type compatibility at compile time.

**Why It Matters**: When integrating with external systems — OpenAPI-generated types, shared library interfaces, database ORMs — you need Zod schemas that produce the exact type the external system expects. `z.ZodType<T>` makes the type constraint explicit and compiler-verified. If you add a `.transform()` that changes the shape, or use the wrong type for a field, TypeScript catches it immediately. This is the correct pattern for creating Zod validators for types you don't control.

---

## Group 5: Migration Patterns

### Example 71: Migrating from Yup

Translating common Yup validation patterns to their Zod equivalents. Yup uses method chaining on mutable builders; Zod uses immutable schema composition.

```typescript
// Yup (before migration):
// import * as yup from "yup";
// const yupSchema = yup.object({
//   name: yup.string().required("Name required").min(2),
//   age: yup.number().required().positive().integer(),
//   email: yup.string().email("Invalid email").required(),
//   role: yup.string().oneOf(["admin", "user"]).required(),
// });

// Zod equivalent:
import { z } from "zod";

const ZodEquivalentSchema = z.object({
  name: z.string({ required_error: "Name required" }).min(2),
  // => Yup: yup.string().required("Name required").min(2)
  // => Zod: z.string() is required by default; custom message via required_error

  age: z.number().positive().int(),
  // => Yup: yup.number().required().positive().integer()
  // => Zod: z.number().positive().int() — required by default

  email: z.string().email("Invalid email"),
  // => Yup: yup.string().email("Invalid email").required()
  // => Zod: string is required by default; email message as argument

  role: z.enum(["admin", "user"]),
  // => Yup: yup.string().oneOf(["admin", "user"]).required()
  // => Zod: z.enum() validates membership in the tuple
});

// Key differences:
// 1. Yup fields are optional by default; Zod fields are required by default
//    Yup: needs .required(); Zod: needs .optional()
// 2. Yup validates lazily (first error per field); Zod validates eagerly (all errors)
// 3. Zod type inference is automatic; Yup requires yup.InferType<typeof schema>
// 4. Yup has better async validation ergonomics; Zod uses .safeParseAsync()

type MigratedType = z.infer<typeof ZodEquivalentSchema>;
// => { name: string; age: number; email: string; role: "admin" | "user" }
// => Same shape as the Yup schema would produce

// Custom Yup-style .when() conditional (use superRefine in Zod):
// Yup: email.when("role", { is: "admin", then: yup.string().required() })
// Zod equivalent:
const ConditionalSchema = z
  .object({
    role: z.enum(["admin", "user"]),
    adminEmail: z.string().email().optional(),
  })
  .superRefine((data, ctx) => {
    if (data.role === "admin" && !data.adminEmail) {
      ctx.addIssue({ code: z.ZodIssueCode.custom, message: "Admin email required", path: ["adminEmail"] });
    }
  });
```

**Key Takeaway**: Yup fields are optional by default (need `.required()`); Zod fields are required by default (need `.optional()`). Replace Yup's `.oneOf()` with `z.enum()` and `.when()` with `.superRefine()`.

**Why It Matters**: Many codebases use Yup for form validation (especially with Formik). When migrating to Zod (for tRPC, React Hook Form, or better TypeScript integration), the semantic differences cause bugs if overlooked. The most common migration mistake is omitting `.optional()` on Zod fields that were optional in Yup — suddenly all previously-optional fields become required. This example catalogs the key behavioral differences to enable confident, correct migrations without regression.

---

### Example 72: Migrating from io-ts

Translating io-ts codec patterns to Zod schemas. io-ts uses functional programming patterns; Zod uses a more imperative builder pattern.

```typescript
// io-ts (before migration):
// import * as t from "io-ts";
// const UserCodec = t.type({
//   id: t.string,
//   name: t.string,
//   age: t.number,
//   status: t.union([t.literal("active"), t.literal("inactive")]),
//   address: t.partial({
//     street: t.string,
//     city: t.string,
//   }),
// });

// Zod equivalent:
import { z } from "zod";

const UserSchema = z.object({
  id: z.string(),
  // => io-ts: t.string → z.string()

  name: z.string(),
  // => io-ts: t.string → z.string()

  age: z.number(),
  // => io-ts: t.number → z.number()

  status: z.union([z.literal("active"), z.literal("inactive")]),
  // => io-ts: t.union([t.literal("active"), t.literal("inactive")])
  // => Zod: almost identical syntax — simpler alternative: z.enum(["active", "inactive"])

  address: z
    .object({
      street: z.string().optional(),
      city: z.string().optional(),
      // => io-ts: t.partial() makes all fields optional
      // => Zod: apply .optional() per field, or use z.object({...}).partial()
    })
    .optional(),
  // => The address object itself is also optional
});

// io-ts uses Either for results: PathReporter for errors
// Zod uses SafeParseReturn with ZodError

// io-ts decode pattern:
// const result = UserCodec.decode(rawData);
// if (either.isLeft(result)) { /* PathReporter.report(result) */ }

// Zod equivalent:
const result = UserSchema.safeParse(rawData);
if (!result.success) {
  result.error.issues.forEach((i) => console.log(i.path.join("."), i.message));
}

type User = z.infer<typeof UserSchema>;
// => { id: string; name: string; age: number; status: "active" | "inactive"; address?: {...} }
// => io-ts equivalent: t.TypeOf<typeof UserCodec>

const rawData = { id: "1", name: "Aisha", age: 28, status: "active" };
```

**Key Takeaway**: io-ts `t.type` → Zod `z.object`; `t.partial` → `.partial()` method; `t.union` → `z.union`; `Either` pattern → `safeParse().success`. Zod is less functional but more ergonomic.

**Why It Matters**: io-ts provides rigorous functional programming guarantees but has a steep learning curve. Many teams migrate to Zod for better ergonomics while retaining runtime type safety. Understanding the mapping prevents validation gaps during migration — particularly the `t.partial()` to `.partial()` conversion for optional field objects, and the `Either`-based error handling to `safeParse()` pattern. The TypeScript output types are semantically equivalent, ensuring downstream code continues to work without type errors.

---

### Example 73: Effect Schemas (Advanced Transform Pattern)

The "effect" pattern uses transforms to model operations with side effects — HTTP calls, database writes, event emissions — in a composable, testable way.

```typescript
import { z } from "zod";

// Effect pattern: schema that represents an operation with a result
// This is a design pattern, not a separate library feature

// Pure validation schema
const CreateUserInputSchema = z.object({
  email: z.string().email(),
  name: z.string().min(1),
  password: z.string().min(8),
});

// "Effect" schema: validates input AND defines the effect (DB creation)
type CreateUserEffect = {
  type: "CREATE_USER";
  email: string;
  name: string;
  hashedPassword: string;
};

const CreateUserEffectSchema = CreateUserInputSchema.transform(async (input): Promise<CreateUserEffect> => {
  // => Transform: validate input → produce effect descriptor

  // Simulate password hashing (in production: use bcrypt)
  const hashedPassword = `hashed:${input.password}`;
  // => Hash the password as part of validation → effect creation

  return {
    type: "CREATE_USER",
    // => Effect type discriminant
    email: input.email,
    name: input.name,
    hashedPassword,
    // => Transformed: raw password replaced with hash
  };
});

// Execute the effect
async function executeCreateUser(rawInput: unknown): Promise<{ userId: string }> {
  const effectResult = await CreateUserEffectSchema.safeParseAsync(rawInput);
  // => Parse produces the effect descriptor

  if (!effectResult.success) {
    throw new Error("Invalid input: " + effectResult.error.message);
  }

  const effect = effectResult.data;
  // => effect.type = "CREATE_USER"
  // => effect.hashedPassword = "hashed:..."
  // => Ready to execute against database

  // Execute effect (simulated)
  console.log("Creating user:", effect.email);
  return { userId: "new-user-uuid" };
}

executeCreateUser({ email: "aisha@example.com", name: "Aisha", password: "SecurePass1" }).then((result) =>
  console.log("Created:", result.userId),
);
// => Output: Creating user: aisha@example.com
// => Output: Created: new-user-uuid
```

**Key Takeaway**: The effect pattern uses async `.transform()` to produce effect descriptors from validated input — separating validation from execution while keeping them co-located in the schema.

**Why It Matters**: Validation and transformation frequently blend with side effects — hashing passwords, generating IDs, fetching related data. The effect schema pattern separates these concerns: the schema validates input and produces a complete, self-contained effect descriptor; the executor runs the effect. This makes validation logic testable without executing side effects, and effect execution testable without validation. The pattern also creates a natural audit log — the effect descriptor describes what the system did in response to validated input.

---

### Example 74: Schema Registry Pattern

A schema registry centralizes all schemas in an application, enabling dynamic schema lookup, versioning, and documentation generation.

```typescript
import { z } from "zod";

// Schema registry with versioning
class SchemaRegistry {
  private schemas = new Map<string, Map<number, z.ZodTypeAny>>();
  // => Map<name, Map<version, schema>>

  register<T extends z.ZodTypeAny>(name: string, version: number, schema: T): T {
    // => Register schema under name + version key
    if (!this.schemas.has(name)) {
      this.schemas.set(name, new Map());
      // => Create version map for this name
    }
    this.schemas.get(name)!.set(version, schema);
    // => Store schema under version number
    return schema;
    // => Return schema for chaining
  }

  get(name: string, version: number = 1): z.ZodTypeAny {
    // => Retrieve schema by name and version (default: 1)
    const versions = this.schemas.get(name);
    if (!versions) throw new Error(`Unknown schema: ${name}`);
    const schema = versions.get(version);
    if (!schema) throw new Error(`Unknown version ${version} for schema: ${name}`);
    return schema;
  }

  validate(name: string, data: unknown, version: number = 1) {
    // => Validate data against named schema
    return this.get(name, version).safeParse(data);
  }
}

// Application-wide registry
const registry = new SchemaRegistry();

// Register schemas
const UserV1 = registry.register(
  "User",
  1,
  z.object({
    id: z.number().int().positive(),
    name: z.string(),
  }),
);

const UserV2 = registry.register(
  "User",
  2,
  z.object({
    id: z.string().uuid(),
    firstName: z.string(),
    lastName: z.string(),
  }),
);

// Dynamic validation by name + version
const v1Result = registry.validate("User", { id: 42, name: "Aisha" }, 1);
// => Validates against UserV1 schema
// => v1Result.success = true

const v2Result = registry.validate(
  "User",
  { id: "550e8400-e29b-41d4-a716-446655440000", firstName: "Aisha", lastName: "Rahman" },
  2,
);
// => Validates against UserV2 schema
// => v2Result.success = true

// Use in webhook processing
function processWebhook(schemaName: string, version: number, body: unknown) {
  const result = registry.validate(schemaName, body, version);
  // => Dynamic schema lookup by webhook type
  if (!result.success) throw new Error("Invalid webhook payload");
  return result.data;
}
```

**Key Takeaway**: A schema registry centralizes schema management with version support. Enables dynamic schema lookup for webhook processors, plugin systems, and multi-version API support.

**Why It Matters**: Large applications with many entity types benefit from centralized schema management — a single place to find, update, and audit all validation schemas. Versioning support enables simultaneous multi-version API handling without conditionals scattered throughout handlers. Plugin architectures let extensions register their own schemas for validation without modifying core code. The registry pattern also enables tooling — iterating all registered schemas to generate documentation, validate coverage, or detect breaking changes between versions.

---

### Example 75: Complete Production Schema Architecture

Demonstrate a complete, production-ready schema architecture combining all advanced patterns: branded types, generic factories, versioned schemas, and middleware integration.

```typescript
import { z } from "zod";

// ─── Branded primitive types ──────────────────────────────────────────────

const UserId = z.string().uuid().brand("UserId");
const OrderId = z.string().uuid().brand("OrderId");
const Money = z.number().positive().multipleOf(0.01).brand("Money");
// => .multipleOf(0.01) ensures at most 2 decimal places
// => Branded Money prevents mixing currencies or raw numbers with Money

type UserId = z.infer<typeof UserId>;
type OrderId = z.infer<typeof OrderId>;
type Money = z.infer<typeof Money>;

// ─── Base schemas ─────────────────────────────────────────────────────────

const TimestampsSchema = z.object({
  createdAt: z.date(),
  updatedAt: z.date(),
});

const SoftDeleteSchema = z.object({
  deletedAt: z.date().nullable(),
});

// ─── Domain schemas ───────────────────────────────────────────────────────

const OrderItemSchema = z.object({
  productId: z.string().uuid(),
  quantity: z.number().int().positive(),
  unitPrice: Money,
  // => Branded: ensures this is validated Money, not arbitrary number
});

const OrderSchema = z
  .object({
    id: OrderId,
    // => Branded: prevents mixing OrderId with UserId
    customerId: UserId,
    // => Branded: this ID must be a validated UserId
    items: z.array(OrderItemSchema).min(1),
    totalAmount: Money,
    status: z.enum(["pending", "processing", "shipped", "delivered", "cancelled"]),
  })
  .merge(TimestampsSchema)
  // => Add audit timestamps
  .merge(SoftDeleteSchema)
  // => Add soft-delete support
  .refine(
    (order) => {
      const computed = order.items.reduce((sum, item) => sum + item.quantity * item.unitPrice, 0);
      return Math.abs(computed - order.totalAmount) < 0.01;
    },
    "Total amount must equal sum of item prices",
    // => Business rule: totals must balance
  );

type Order = z.infer<typeof OrderSchema>;
// => Complete Order type: all fields from all composed schemas

// ─── Generic API wrapper ──────────────────────────────────────────────────

function ApiSuccessSchema<T extends z.ZodTypeAny>(dataSchema: T) {
  return z.object({
    success: z.literal(true),
    data: dataSchema,
    timestamp: z.string().datetime(),
    // => ISO 8601 timestamp
    requestId: z.string().uuid(),
    // => Request correlation ID
  });
}

const OrderResponseSchema = ApiSuccessSchema(OrderSchema);
type OrderResponse = z.infer<typeof OrderResponseSchema>;
// => Complete API response: { success: true; data: Order; timestamp: string; requestId: string }

// This architecture demonstrates:
// 1. Branded types prevent ID/value mixing
// 2. Composed schemas reduce duplication
// 3. Generic factories handle common patterns
// 4. Business rules embedded in schemas as refinements
// 5. Single z.infer<> call for complete type information
```

**Key Takeaway**: Production schema architecture combines branded types for nominal safety, composed base schemas for DRY common fields, generic factories for response wrappers, and inline refinements for business rules.

**Why It Matters**: This example demonstrates how every Zod feature composes into a coherent architecture for real applications. Branded types prevent ID confusion at the type level. Composed base schemas ensure timestamp and soft-delete fields are uniformly defined. Generic factories eliminate response envelope duplication. Business rule refinements keep validation logic co-located with the data shape it validates. The result is a self-documenting, type-safe schema system where `z.infer<>` produces complete TypeScript types for every layer of your API stack.

---

### Example 76: Testing Schemas

Write unit tests for Zod schemas to verify validation behavior, error messages, and transforms under all conditions.

```typescript
import { z } from "zod";
// Note: test runner syntax is conceptual — works with Jest/Vitest

const ProductSchema = z.object({
  name: z.string().min(1, "Name required").max(200),
  price: z.number().positive("Price must be positive"),
  category: z.enum(["electronics", "clothing", "food"]),
  tags: z.array(z.string()).default([]),
});

// Test helper: assert schema accepts valid data
function assertValid<T>(schema: z.ZodType<T>, data: unknown): T {
  const result = schema.safeParse(data);
  if (!result.success) {
    throw new Error(`Expected valid data but got: ${result.error.message}`);
  }
  return result.data;
  // => Returns validated data for further assertions
}

// Test helper: assert schema rejects with specific error
function assertInvalid(schema: z.ZodTypeAny, data: unknown, expectedMessage: string): void {
  const result = schema.safeParse(data);
  if (result.success) {
    throw new Error(`Expected validation to fail but it succeeded`);
  }
  const messages = result.error.issues.map((i) => i.message);
  if (!messages.some((m) => m.includes(expectedMessage))) {
    throw new Error(`Expected error "${expectedMessage}" but got: ${messages.join(", ")}`);
  }
}

// Tests (framework-agnostic)
function runSchemaTests(): void {
  // Test 1: valid product passes
  const product = assertValid(ProductSchema, {
    name: "Laptop",
    price: 999.99,
    category: "electronics",
  });
  console.assert(product.tags.length === 0, "Default tags should be empty array");
  // => Tags default applied: product.tags = []

  // Test 2: empty name fails with custom message
  assertInvalid(ProductSchema, { name: "", price: 10, category: "food" }, "Name required");
  // => min(1) fails with "Name required" message

  // Test 3: negative price fails
  assertInvalid(ProductSchema, { name: "Book", price: -5, category: "food" }, "Price must be positive");

  // Test 4: invalid category fails
  assertInvalid(ProductSchema, { name: "Book", price: 10, category: "toys" }, "Invalid enum value");

  console.log("All schema tests passed");
  // => Output: All schema tests passed
}

runSchemaTests();
```

**Key Takeaway**: Test schemas with helper functions that assert valid data passes and invalid data fails with expected messages. Test default values, transforms, and custom refinements explicitly.

**Why It Matters**: Schemas are code — they need tests like any other code. Schema tests catch regressions when validation rules change, verify custom error messages are correct, confirm default values work, and document expected behavior for reviewers. A failing schema test is far better than a failing production validation that rejects valid user input or accepts invalid data. Schema tests are particularly valuable for `.refine()` and `.transform()` logic that involves business rules — test every branch of conditional validation logic.

---

### Example 77: Advanced Error Path Navigation

Navigate complex nested `ZodError` structures for deeply nested objects, arrays, and discriminated unions.

```typescript
import { z } from "zod";

// Deeply nested schema
const OrderSchema = z.object({
  customer: z.object({
    name: z.string().min(1),
    address: z.object({
      street: z.string().min(1),
      city: z.string().min(1),
      country: z.string().length(2),
    }),
  }),
  items: z.array(
    z.object({
      productId: z.string().uuid("Invalid product ID"),
      quantity: z.number().int().positive("Quantity must be positive"),
    }),
  ),
});

const result = OrderSchema.safeParse({
  customer: {
    name: "Aisha",
    address: {
      street: "", // => Empty — fails min(1)
      city: "Jakarta",
      country: "IDN", // => Three chars — fails length(2)
    },
  },
  items: [
    { productId: "550e8400-e29b-41d4-a716-446655440000", quantity: 2 },
    { productId: "not-uuid", quantity: -1 },
    // => Item index 1: both fields fail
  ],
});

if (!result.success) {
  result.error.issues.forEach((issue) => {
    // => issue.path is an array of string/number path segments
    const pathStr = issue.path.map((p) => (typeof p === "number" ? `[${p}]` : p)).join(".");
    // => ["customer", "address", "street"] → "customer.address.street"
    // => ["items", 1, "productId"] → "items.[1].productId"
    console.log(`${pathStr}: ${issue.message}`);
  });
  // => Output: customer.address.street: String must contain at least 1 character(s)
  // => Output: customer.address.country: String must contain exactly 2 character(s)
  // => Output: items.[1].productId: Invalid product ID
  // => Output: items.[1].quantity: Quantity must be positive

  // Access nested errors using .format()
  const formatted = result.error.format();
  console.log(formatted.customer?.address?.street?._errors);
  // => Output: ["String must contain at least 1 character(s)"]

  console.log(formatted.items?.[1]?.quantity?._errors);
  // => Output: ["Quantity must be positive"]
  // => Numeric array index in formatted output
}
```

**Key Takeaway**: `issue.path` contains the full path as an array of string/number segments. Use `.format()` for direct nested access; map `path` arrays for custom error formatting or path-based error display.

**Why It Matters**: Nested object and array errors require path navigation to display errors next to the correct form field. An error on `items[1].quantity` must highlight the quantity input in the second item row, not just show a generic "validation failed" message. Understanding the path structure enables building sophisticated form validation UIs that accurately map Zod errors to the correct input elements regardless of nesting depth. Array indices in paths are particularly important for dynamic list forms where items are added and removed.

---

### Example 78: Schema Cloning and Modification

Create modified versions of schemas while preserving the original. Essential for building schema variants from a base without duplication.

```typescript
import { z } from "zod";

// Base schema
const ProductSchema = z.object({
  id: z.string().uuid(),
  name: z.string().min(1).max(200),
  price: z.number().positive(),
  description: z.string().optional(),
  stock: z.number().int().nonnegative().default(0),
  category: z.enum(["electronics", "clothing", "food"]),
  isActive: z.boolean().default(true),
});

// Derive schemas for different operations

// Create: omit server-generated fields
const CreateProductSchema = ProductSchema.omit({ id: true });
// => No id — server generates it

// Update: all fields optional (PATCH semantics)
const UpdateProductSchema = ProductSchema.partial().required({ id: true });
// => .partial() makes all optional, .required({ id: true }) makes id required again
// => { id: string; name?: string; price?: number; ... }

// List response: omit large description for efficiency
const ProductListItemSchema = ProductSchema.omit({ description: true });
// => Lighter schema for list views — description only in detail view

// Public product: omit internal fields
const PublicProductSchema = ProductSchema.omit({ stock: true });
// => stock not exposed to public API

// Bulk import: require all fields explicitly
const ImportProductSchema = ProductSchema.required().omit({ id: true, isActive: true });
// => .required(): removes all defaults (everything required)
// => .omit(): exclude server-managed fields

type CreateProduct = z.infer<typeof CreateProductSchema>;
// => { name: string; price: number; description?: string; stock: number; category: ...; isActive: boolean }

type UpdateProduct = z.infer<typeof UpdateProductSchema>;
// => { id: string; name?: string; price?: number; description?: string; ... }

type PublicProduct = z.infer<typeof PublicProductSchema>;
// => { id: string; name: string; price: number; description?: string; category: ...; isActive: boolean }
```

**Key Takeaway**: Chain `.omit()`, `.pick()`, `.partial()`, `.required()`, `.extend()`, and `.merge()` from a single base schema to derive all operation variants. Every variant stays synchronized with the base.

**Why It Matters**: REST APIs need different schema variants per HTTP method: POST omits server-generated IDs, PATCH makes all fields optional, GET omits sensitive fields, list endpoints omit heavy fields for efficiency. Deriving these from a single base schema guarantees consistency — when you add a field to the base schema, all derived schemas update automatically based on their derivation rules. This eliminates the drift between operation schemas that causes inconsistent validation across API methods.

---

### Example 79: Zod with Drizzle ORM Integration

Drizzle ORM's `createInsertSchema` and `createSelectSchema` generate Zod schemas from database table definitions, keeping DB schemas and validation schemas synchronized.

```typescript
// Note: requires npm install drizzle-orm drizzle-zod
// import { pgTable, text, integer, boolean, timestamp } from "drizzle-orm/pg-core";
// import { createInsertSchema, createSelectSchema } from "drizzle-zod";
import { z } from "zod";

// Drizzle table definition (conceptual)
// const users = pgTable("users", {
//   id: text("id").primaryKey(),
//   name: text("name").notNull(),
//   email: text("email").notNull().unique(),
//   age: integer("age"),
//   isActive: boolean("is_active").notNull().default(true),
//   createdAt: timestamp("created_at").notNull().defaultNow(),
// });

// Generated schemas from Drizzle table (conceptual output):
// const insertUserSchema = createInsertSchema(users);
// => Schema for INSERT operations — server defaults omitted
// => { id?: string; name: string; email: string; age?: number; isActive?: boolean }

// const selectUserSchema = createSelectSchema(users);
// => Schema for SELECT results — all fields present
// => { id: string; name: string; email: string; age: number | null; isActive: boolean; createdAt: Date }

// Extending generated schemas with additional validation
// const CreateUserSchema = insertUserSchema.extend({
//   name: z.string().min(1).max(100),
//   // => Override name with length constraints (Drizzle only knows "text not null")
//   email: z.string().email(),
//   // => Override email with format validation
//   password: z.string().min(8),
//   // => Add field not in DB (hashed before insert)
// }).omit({ id: true });
// => id omitted (UUID generated server-side)

// Manual equivalent (when Drizzle integration not available)
const UserInsertSchema = z.object({
  name: z.string().min(1).max(100),
  // => Application-level: length constraints
  email: z.string().email(),
  // => Application-level: format validation
  age: z.number().int().positive().optional().nullable(),
  // => Optional nullable: matches DB column (integer, nullable)
  isActive: z.boolean().default(true),
  // => Matches DB default
  password: z.string().min(8),
  // => Application-level: not in DB (hashed before storage)
});

type UserInsert = z.infer<typeof UserInsertSchema>;
// => Typed insert input — safe to use with ORM after removing password, adding hash
```

**Key Takeaway**: Drizzle ORM's `drizzle-zod` generates base schemas from table definitions. Extend with `.extend()` to add application-level constraints beyond what the database schema captures.

**Why It Matters**: Database schemas and application validation schemas often drift — the database knows column types and constraints, but not business rules like minimum lengths, email formats, or cross-field dependencies. `drizzle-zod` bridges this gap by auto-generating base schemas from table definitions, then `.extend()` adds application logic on top. When the database schema changes (new column, changed nullable), the generated base schema updates automatically, and your extensions continue to apply on top. This eliminates the category of bugs where the database has a NOT NULL column but the validation schema marks it optional.

---

### Example 80: Building a Schema-First API Layer

Combine all advanced patterns into a schema-first API layer that derives types, validation, documentation, and error handling from a single schema definition.

```typescript
import { z } from "zod";

// ─── Schema definitions ────────────────────────────────────────────────────

const CreateOrderInput = z
  .object({
    customerId: z.string().uuid("Customer ID must be a valid UUID"),
    items: z
      .array(
        z.object({
          productId: z.string().uuid("Product ID must be a valid UUID"),
          quantity: z.number().int().positive("Quantity must be a positive integer"),
        }),
      )
      .min(1, "Order must have at least one item"),
    notes: z.string().max(500).optional(),
  })
  .describe("Create a new order");

const OrderResponse = z
  .object({
    orderId: z.string().uuid(),
    status: z.literal("pending"),
    estimatedTotal: z.number().positive(),
    createdAt: z.date(),
  })
  .describe("Order creation response");

// ─── Derived types ─────────────────────────────────────────────────────────

type CreateOrderRequest = z.infer<typeof CreateOrderInput>;
// => Input type for API handler

type CreateOrderResult = z.infer<typeof OrderResponse>;
// => Output type from handler

// ─── Schema-first handler factory ─────────────────────────────────────────

function createHandler<TInput, TOutput>(
  inputSchema: z.ZodType<TInput>,
  outputSchema: z.ZodType<TOutput>,
  handler: (input: TInput) => Promise<TOutput>,
) {
  return async (
    rawInput: unknown,
  ): Promise<{ success: true; data: TOutput } | { success: false; errors: z.ZodIssue[] }> => {
    // Validate input
    const inputResult = inputSchema.safeParse(rawInput);
    if (!inputResult.success) {
      return { success: false, errors: inputResult.error.issues };
      // => Return structured errors
    }

    // Execute handler with validated input
    const output = await handler(inputResult.data);
    // => Handler receives typed input

    // Validate output (catches handler bugs)
    const outputResult = outputSchema.safeParse(output);
    if (!outputResult.success) {
      console.error("Handler returned invalid output:", outputResult.error);
      return { success: false, errors: [] };
      // => Internal error: handler violated its own output contract
    }

    return { success: true, data: outputResult.data };
    // => Return validated output to caller
  };
}

// ─── Usage ─────────────────────────────────────────────────────────────────

const createOrderHandler = createHandler(
  CreateOrderInput,
  OrderResponse,
  async (input: CreateOrderRequest): Promise<CreateOrderResult> => {
    // => input is fully typed — TypeScript guarantees validity
    console.log("Creating order for customer:", input.customerId);
    return {
      orderId: "550e8400-e29b-41d4-a716-446655440000",
      status: "pending",
      estimatedTotal: input.items.length * 50,
      // => Simplified estimate
      createdAt: new Date(),
    };
  },
);

createOrderHandler({
  customerId: "550e8400-e29b-41d4-a716-446655440001",
  items: [{ productId: "550e8400-e29b-41d4-a716-446655440002", quantity: 2 }],
}).then((result) => {
  if (result.success) console.log("Order created:", result.data.orderId);
  // => Output: Order created: 550e8400-e29b-41d4-a716-446655440000
});
```

**Key Takeaway**: A schema-first handler factory validates both input and output, ensuring API handlers receive verified data and return data that matches their declared output type. Schemas become the contract between API layers.

**Why It Matters**: This final example demonstrates the complete vision of schema-first API development: schemas defined once, driving validation, type inference, error handling, and documentation simultaneously. Validating both input and output creates a double fence — input validation prevents bad data from reaching handlers, output validation catches bugs where handlers return incorrectly shaped data. The factory pattern makes this systematic: every handler is automatically wrapped with the same validation infrastructure, ensuring consistent behavior across all API endpoints without per-handler validation code.
