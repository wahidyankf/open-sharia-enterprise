---
title: Next.js Server Action Template
description: Production-ready template for Next.js Server Actions with form handling, validation, revalidation, optimistic updates, and error handling
category: explanation
tags:
  - nextjs
  - template
  - server-actions
  - forms
  - validation
  - mutations
created: 2026-01-26
updated: 2026-01-26
---

# Next.js Server Action Template

Production-ready template for creating type-safe Next.js Server Actions with Zod validation, authentication, revalidation, optimistic updates, and comprehensive error handling for OSE Platform.

## 📋 Template Usage

**File location**: `actions/*.ts` or `app/*/actions.ts`

**Use this template when**:

- Handling form submissions
- Performing data mutations
- Need progressive enhancement
- Want type-safe form handling

## 🎯 Basic Server Action Template

```typescript
// actions/example.ts
"use server";

import { revalidatePath } from "next/cache";
import { redirect } from "next/navigation";
import { z } from "zod";
import { auth } from "@/lib/auth";
import { db } from "@/lib/db";

// Input validation schema
const schema = z.object({
  // Define your schema
});

// Result type
interface ActionResult {
  success: boolean;
  error?: string;
  data?: any;
}

export async function exampleAction(prevState: ActionResult | null, formData: FormData): Promise<ActionResult> {
  try {
    // Authentication check
    const session = await auth();

    if (!session) {
      return { success: false, error: "Unauthorized" };
    }

    // Parse and validate form data
    const rawData = {
      // Extract fields from formData
    };

    const validation = schema.safeParse(rawData);

    if (!validation.success) {
      return {
        success: false,
        error: validation.error.errors[0]?.message ?? "Validation failed",
      };
    }

    const data = validation.data;

    // Perform mutation

    // Revalidate affected paths
    revalidatePath("/path");

    return { success: true, data };
  } catch (error) {
    console.error("Action error:", error);
    return { success: false, error: "Operation failed" };
  }
}
```

## ✅ Complete Zakat Calculation Server Action

```typescript
// actions/zakat/calculate.ts
"use server";

import { revalidatePath } from "next/cache";
import { redirect } from "next/navigation";
import { z } from "zod";
import { auth } from "@/lib/auth";
import { db } from "@/lib/db";

// Validation schema
const calculateZakatSchema = z.object({
  wealth: z
    .number({
      required_error: "Wealth is required",
      invalid_type_error: "Wealth must be a number",
    })
    .positive("Wealth must be positive")
    .max(1_000_000_000, "Wealth exceeds maximum"),
  nisab: z.number().positive("Nisab must be positive").max(1_000_000, "Nisab exceeds maximum"),
  liabilities: z.number().nonnegative("Liabilities cannot be negative").optional().default(0),
});

// Result type
interface CalculateZakatResult {
  success: boolean;
  error?: string;
  data?: {
    calculationId: string;
    zakatAmount: number;
    eligibleForZakat: boolean;
  };
}

export async function calculateZakat(
  prevState: CalculateZakatResult | null,
  formData: FormData,
): Promise<CalculateZakatResult> {
  try {
    // Authentication
    const session = await auth();

    if (!session) {
      return { success: false, error: "You must be logged in to calculate zakat" };
    }

    // Parse form data
    const rawData = {
      wealth: parseFloat(formData.get("wealth") as string),
      nisab: parseFloat(formData.get("nisab") as string),
      liabilities: formData.get("liabilities") ? parseFloat(formData.get("liabilities") as string) : 0,
    };

    // Validate
    const validation = calculateZakatSchema.safeParse(rawData);

    if (!validation.success) {
      return {
        success: false,
        error: validation.error.errors[0]?.message ?? "Invalid input",
      };
    }

    const data = validation.data;

    // Calculate zakat
    const netWealth = data.wealth - data.liabilities;
    const eligibleForZakat = netWealth >= data.nisab;
    const zakatAmount = eligibleForZakat ? netWealth * 0.025 : 0;

    // Save to database
    const calculation = await db.zakatCalculation.create({
      data: {
        userId: session.user.id,
        wealth: data.wealth,
        nisab: data.nisab,
        liabilities: data.liabilities,
        zakatAmount,
        eligibleForZakat,
      },
    });

    // Revalidate dashboard
    revalidatePath("/zakat/dashboard");

    return {
      success: true,
      data: {
        calculationId: calculation.id,
        zakatAmount,
        eligibleForZakat,
      },
    };
  } catch (error) {
    console.error("Calculate zakat error:", error);
    return {
      success: false,
      error: "Failed to calculate zakat. Please try again.",
    };
  }
}
```

## 🔄 Server Action with Redirect

```typescript
// actions/murabaha/submit-application.ts
"use server";

import { redirect } from "next/navigation";
import { z } from "zod";
import { auth } from "@/lib/auth";
import { db } from "@/lib/db";

const applicationSchema = z.object({
  productName: z.string().min(3, "Product name must be at least 3 characters"),
  purchasePrice: z.number().positive("Purchase price must be positive"),
  preferredTerm: z.number().int().min(6).max(60, "Term must be between 6 and 60 months"),
});

export async function submitMurabahaApplication(formData: FormData) {
  try {
    const session = await auth();

    if (!session) {
      throw new Error("Unauthorized");
    }

    const rawData = {
      productName: formData.get("productName") as string,
      purchasePrice: parseFloat(formData.get("purchasePrice") as string),
      preferredTerm: parseInt(formData.get("preferredTerm") as string, 10),
    };

    const validation = applicationSchema.safeParse(rawData);

    if (!validation.success) {
      throw new Error(validation.error.errors[0]?.message ?? "Validation failed");
    }

    const data = validation.data;

    // Calculate Murabaha terms
    const profitMargin = 0.15; // 15% profit margin
    const profitAmount = data.purchasePrice * profitMargin;
    const sellingPrice = data.purchasePrice + profitAmount;
    const monthlyPayment = sellingPrice / data.preferredTerm;

    // Create application
    const application = await db.murabahaApplication.create({
      data: {
        userId: session.user.id,
        productName: data.productName,
        purchasePrice: data.purchasePrice,
        profitMargin,
        sellingPrice,
        preferredTerm: data.preferredTerm,
        monthlyPayment,
        status: "PENDING",
      },
    });

    // Redirect to application details
    redirect(`/murabaha/applications/${application.id}`);
  } catch (error) {
    console.error("Submit application error:", error);
    throw error;
  }
}
```

## 🗑️ Delete Action with Revalidation

```typescript
// actions/zakat/delete-calculation.ts
"use server";

import { revalidatePath } from "next/cache";
import { z } from "zod";
import { auth } from "@/lib/auth";
import { db } from "@/lib/db";

const deleteSchema = z.object({
  calculationId: z.string().uuid("Invalid calculation ID"),
});

interface DeleteResult {
  success: boolean;
  error?: string;
}

export async function deleteCalculation(prevState: DeleteResult | null, formData: FormData): Promise<DeleteResult> {
  try {
    const session = await auth();

    if (!session) {
      return { success: false, error: "Unauthorized" };
    }

    const rawData = {
      calculationId: formData.get("calculationId") as string,
    };

    const validation = deleteSchema.safeParse(rawData);

    if (!validation.success) {
      return {
        success: false,
        error: validation.error.errors[0]?.message ?? "Invalid ID",
      };
    }

    const { calculationId } = validation.data;

    // Verify ownership before deletion
    const calculation = await db.zakatCalculation.findUnique({
      where: {
        id: calculationId,
        userId: session.user.id,
      },
    });

    if (!calculation) {
      return {
        success: false,
        error: "Calculation not found or you do not have permission to delete it",
      };
    }

    // Delete
    await db.zakatCalculation.delete({
      where: { id: calculationId },
    });

    // Revalidate affected paths
    revalidatePath("/zakat/dashboard");
    revalidatePath("/zakat/history");

    return { success: true };
  } catch (error) {
    console.error("Delete calculation error:", error);
    return { success: false, error: "Failed to delete calculation" };
  }
}
```

## 🔄 Update Action

```typescript
// actions/waqf/update-donation.ts
"use server";

import { revalidatePath } from "next/cache";
import { z } from "zod";
import { auth } from "@/lib/auth";
import { db } from "@/lib/db";

const updateDonationSchema = z.object({
  donationId: z.string().uuid(),
  amount: z.number().positive("Amount must be positive").optional(),
  recurring: z.boolean().optional(),
  frequency: z.enum(["monthly", "quarterly", "annually"]).optional(),
});

interface UpdateResult {
  success: boolean;
  error?: string;
  data?: {
    id: string;
    amount: number;
  };
}

export async function updateDonation(prevState: UpdateResult | null, formData: FormData): Promise<UpdateResult> {
  try {
    const session = await auth();

    if (!session) {
      return { success: false, error: "Unauthorized" };
    }

    const rawData = {
      donationId: formData.get("donationId") as string,
      amount: formData.get("amount") ? parseFloat(formData.get("amount") as string) : undefined,
      recurring: formData.get("recurring") === "true",
      frequency: formData.get("frequency") as any,
    };

    const validation = updateDonationSchema.safeParse(rawData);

    if (!validation.success) {
      return {
        success: false,
        error: validation.error.errors[0]?.message ?? "Validation failed",
      };
    }

    const data = validation.data;

    // Verify ownership
    const existing = await db.waqfDonation.findUnique({
      where: {
        id: data.donationId,
        userId: session.user.id,
      },
    });

    if (!existing) {
      return { success: false, error: "Donation not found" };
    }

    // Update
    const updated = await db.waqfDonation.update({
      where: { id: data.donationId },
      data: {
        amount: data.amount,
        recurring: data.recurring,
        frequency: data.frequency,
      },
    });

    revalidatePath("/waqf/donations");

    return {
      success: true,
      data: {
        id: updated.id,
        amount: updated.amount,
      },
    };
  } catch (error) {
    console.error("Update donation error:", error);
    return { success: false, error: "Failed to update donation" };
  }
}
```

## 📝 Form Component Using Server Action

```typescript
// components/zakat/ZakatCalculatorForm.tsx
'use client';

import { useFormState, useFormStatus } from 'react';
import { calculateZakat } from '@/actions/zakat/calculate';

function SubmitButton() {
  const { pending } = useFormStatus();

  return (
    <button
      type="submit"
      disabled={pending}
      className="px-4 py-2 bg-primary text-white rounded disabled:opacity-50"
    >
      {pending ? 'Calculating...' : 'Calculate Zakat'}
    </button>
  );
}

export function ZakatCalculatorForm() {
  const [state, formAction] = useFormState(calculateZakat, null);

  return (
    <form action={formAction} className="space-y-4">
      <div>
        <label htmlFor="wealth" className="block font-medium mb-1">
          Total Wealth ($)
        </label>
        <input
          id="wealth"
          name="wealth"
          type="number"
          step="0.01"
          required
          className="w-full px-3 py-2 border rounded"
        />
      </div>

      <div>
        <label htmlFor="nisab" className="block font-medium mb-1">
          Nisab Threshold ($)
        </label>
        <input
          id="nisab"
          name="nisab"
          type="number"
          step="0.01"
          required
          className="w-full px-3 py-2 border rounded"
        />
      </div>

      <div>
        <label htmlFor="liabilities" className="block font-medium mb-1">
          Liabilities ($)
        </label>
        <input
          id="liabilities"
          name="liabilities"
          type="number"
          step="0.01"
          defaultValue="0"
          className="w-full px-3 py-2 border rounded"
        />
      </div>

      <SubmitButton />

      {state && !state.success && (
        <div className="p-3 bg-red-50 text-red-700 rounded">
          {state.error}
        </div>
      )}

      {state && state.success && state.data && (
        <div className="p-4 bg-green-50 border border-green-200 rounded">
          <h3 className="font-semibold mb-2">Calculation Result</h3>
          {state.data.eligibleForZakat ? (
            <p>
              Zakat Due: <strong>${state.data.zakatAmount.toFixed(2)}</strong>
            </p>
          ) : (
            <p>Your wealth is below the nisab threshold. No zakat is due.</p>
          )}
        </div>
      )}
    </form>
  );
}
```

## ⚡ Optimistic Updates

```typescript
// components/waqf/DonationToggle.tsx
'use client';

import { useOptimistic } from 'react';
import { toggleRecurring } from '@/actions/waqf/toggle-recurring';

interface DonationToggleProps {
  donationId: string;
  isRecurring: boolean;
}

export function DonationToggle({
  donationId,
  isRecurring,
}: DonationToggleProps) {
  const [optimisticRecurring, setOptimisticRecurring] = useOptimistic(
    isRecurring,
    (state, newState: boolean) => newState
  );

  const handleToggle = async () => {
    // Optimistically update UI
    setOptimisticRecurring(!optimisticRecurring);

    // Perform server action
    const formData = new FormData();
    formData.append('donationId', donationId);
    formData.append('recurring', String(!isRecurring));

    await toggleRecurring(null, formData);
  };

  return (
    <button
      onClick={handleToggle}
      className={`px-4 py-2 rounded ${
        optimisticRecurring
          ? 'bg-primary text-white'
          : 'bg-gray-200 text-gray-700'
      }`}
    >
      {optimisticRecurring ? 'Recurring' : 'One-time'}
    </button>
  );
}
```

```typescript
// actions/waqf/toggle-recurring.ts
"use server";

import { revalidatePath } from "next/cache";
import { z } from "zod";
import { auth } from "@/lib/auth";
import { db } from "@/lib/db";

const toggleSchema = z.object({
  donationId: z.string().uuid(),
  recurring: z.string().transform((val) => val === "true"),
});

interface ToggleResult {
  success: boolean;
  error?: string;
}

export async function toggleRecurring(prevState: ToggleResult | null, formData: FormData): Promise<ToggleResult> {
  try {
    const session = await auth();

    if (!session) {
      return { success: false, error: "Unauthorized" };
    }

    const rawData = {
      donationId: formData.get("donationId") as string,
      recurring: formData.get("recurring") as string,
    };

    const validation = toggleSchema.safeParse(rawData);

    if (!validation.success) {
      return { success: false, error: "Invalid input" };
    }

    const { donationId, recurring } = validation.data;

    await db.waqfDonation.update({
      where: {
        id: donationId,
        userId: session.user.id,
      },
      data: { recurring },
    });

    revalidatePath("/waqf/donations");

    return { success: true };
  } catch (error) {
    console.error("Toggle recurring error:", error);
    return { success: false, error: "Failed to update donation" };
  }
}
```

## 🔄 Batch Operations

```typescript
// actions/zakat/batch-delete.ts
"use server";

import { revalidatePath } from "next/cache";
import { z } from "zod";
import { auth } from "@/lib/auth";
import { db } from "@/lib/db";

const batchDeleteSchema = z.object({
  calculationIds: z.array(z.string().uuid()).min(1, "At least one ID required"),
});

interface BatchDeleteResult {
  success: boolean;
  error?: string;
  deletedCount?: number;
}

export async function batchDeleteCalculations(
  prevState: BatchDeleteResult | null,
  formData: FormData,
): Promise<BatchDeleteResult> {
  try {
    const session = await auth();

    if (!session) {
      return { success: false, error: "Unauthorized" };
    }

    // Parse JSON array from form data
    const idsJson = formData.get("calculationIds") as string;
    const calculationIds = JSON.parse(idsJson);

    const validation = batchDeleteSchema.safeParse({ calculationIds });

    if (!validation.success) {
      return {
        success: false,
        error: validation.error.errors[0]?.message ?? "Invalid IDs",
      };
    }

    const { calculationIds: validatedIds } = validation.data;

    // Delete in batch (verify ownership)
    const result = await db.zakatCalculation.deleteMany({
      where: {
        id: { in: validatedIds },
        userId: session.user.id, // Ensures user can only delete their own
      },
    });

    revalidatePath("/zakat/history");

    return {
      success: true,
      deletedCount: result.count,
    };
  } catch (error) {
    console.error("Batch delete error:", error);
    return { success: false, error: "Failed to delete calculations" };
  }
}
```

## 📊 Server Action with File Upload

```typescript
// actions/documents/upload.ts
"use server";

import { writeFile, mkdir } from "fs/promises";
import path from "path";
import { randomUUID } from "crypto";
import { z } from "zod";
import { auth } from "@/lib/auth";
import { db } from "@/lib/db";

const MAX_FILE_SIZE = 5 * 1024 * 1024; // 5MB

interface UploadResult {
  success: boolean;
  error?: string;
  data?: {
    fileId: string;
    filename: string;
  };
}

export async function uploadDocument(prevState: UploadResult | null, formData: FormData): Promise<UploadResult> {
  try {
    const session = await auth();

    if (!session) {
      return { success: false, error: "Unauthorized" };
    }

    const file = formData.get("file") as File | null;

    if (!file) {
      return { success: false, error: "No file provided" };
    }

    if (file.size > MAX_FILE_SIZE) {
      return { success: false, error: "File size exceeds 5MB limit" };
    }

    const allowedTypes = ["application/pdf", "image/jpeg", "image/png"];

    if (!allowedTypes.includes(file.type)) {
      return { success: false, error: "Invalid file type" };
    }

    // Generate safe filename
    const ext = path.extname(file.name);
    const filename = `${randomUUID()}${ext}`;

    // Create upload directory
    const uploadDir = path.join(process.cwd(), "uploads", session.user.id);
    await mkdir(uploadDir, { recursive: true });

    // Save file
    const filepath = path.join(uploadDir, filename);
    const bytes = await file.arrayBuffer();
    await writeFile(filepath, Buffer.from(bytes));

    // Save metadata to database
    const document = await db.document.create({
      data: {
        userId: session.user.id,
        filename,
        originalName: file.name,
        size: file.size,
        mimeType: file.type,
        path: filepath,
      },
    });

    return {
      success: true,
      data: {
        fileId: document.id,
        filename: document.filename,
      },
    };
  } catch (error) {
    console.error("Upload document error:", error);
    return { success: false, error: "File upload failed" };
  }
}
```

## 📚 Best Practices

### 1. Always Use 'use server' Directive

```typescript
"use server";

// This marks the file as containing Server Actions
```

### 2. Validate All Input with Zod

```typescript
const schema = z.object({
  field: z.string().min(1),
});

const validation = schema.safeParse(data);

if (!validation.success) {
  return { success: false, error: validation.error.errors[0]?.message };
}
```

### 3. Check Authentication

```typescript
const session = await auth();

if (!session) {
  return { success: false, error: "Unauthorized" };
}
```

### 4. Verify Ownership

```typescript
const resource = await db.resource.findUnique({
  where: {
    id: resourceId,
    userId: session.user.id, // Ensures user owns the resource
  },
});

if (!resource) {
  return { success: false, error: "Not found or unauthorized" };
}
```

### 5. Revalidate Affected Paths

```typescript
revalidatePath("/dashboard");
revalidatePath("/data/[id]", "page"); // Specific page
revalidatePath("/data", "layout"); // Entire layout
```

### 6. Use Proper Return Types

```typescript
interface ActionResult {
  success: boolean;
  error?: string;
  data?: any;
}

export async function myAction(): Promise<ActionResult> {
  // Implementation
}
```

### 7. Handle Errors Gracefully

```typescript
try {
  // Action logic
} catch (error) {
  console.error("Action error:", error);
  return { success: false, error: "Operation failed. Please try again." };
}
```

### 8. Use FormData for Form Submissions

```typescript
export async function submitForm(formData: FormData) {
  const field = formData.get("field") as string;
  // Process field
}
```

### 9. Implement Progressive Enhancement

```typescript
// Form works without JavaScript
<form action={serverAction}>
  <input name="field" />
  <button type="submit">Submit</button>
</form>
```

### 10. Use useFormState for State Management

```typescript
'use client';

import { useFormState } from 'react';
import { myAction } from '@/actions/myAction';

export function MyForm() {
  const [state, formAction] = useFormState(myAction, null);

  return <form action={formAction}>{/* Form fields */}</form>;
}
```

## 🔗 Related Templates

- [Page Template](./ex-so-plwe-tsnext-te__page-template.md) - Page components
- [API Route Template](./ex-so-plwe-tsnext-te__api-route-template.md) - REST API endpoints
- [Layout Template](./ex-so-plwe-tsnext-te__layout-template.md) - App structure

---

This template provides comprehensive Server Action patterns for Next.js App Router with OSE Platform security, validation, and best practices.
