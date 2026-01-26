# Next.js Templates

Production-ready templates for Next.js App Router development. These templates provide complete, type-safe boilerplate code following OSE Platform conventions and best practices.

## 📋 Available Templates

### 1. Page Component Template

**File**: [ex-so-plwe-tsnext-te\_\_page-template.md](./ex-so-plwe-tsnext-te__page-template.md)

Complete page component template with:

- Metadata export for SEO
- Server Component with async data fetching
- Error handling patterns
- Loading states with Suspense
- TypeScript types for params and searchParams
- OSE Platform examples (Zakat, Murabaha, Waqf)

**Use when**: Creating new pages in `app/*/page.tsx`

### 2. Layout Component Template

**File**: [ex-so-plwe-tsnext-te\_\_layout-template.md](./ex-so-plwe-tsnext-te__layout-template.md)

Layout component template with:

- Root layout with HTML structure
- Font optimization with next/font
- Global providers (auth, theme, analytics)
- Nested layout patterns
- Navigation components
- Metadata configuration

**Use when**: Creating layouts in `app/*/layout.tsx`

### 3. API Route Template

**File**: [ex-so-plwe-tsnext-te\_\_api-route-template.md](./ex-so-plwe-tsnext-te__api-route-template.md)

API route handler template with:

- GET, POST, PUT, DELETE handlers
- Input validation with Zod
- Error handling and status codes
- Authentication middleware
- CORS configuration
- Rate limiting patterns

**Use when**: Creating API routes in `app/api/*/route.ts`

### 4. Server Action Template

**File**: [ex-so-plwe-tsnext-te\_\_server-action-template.md](./ex-so-plwe-tsnext-te__server-action-template.md)

Server Action template with:

- Form data handling
- Validation with Zod
- Revalidation patterns
- Optimistic updates
- Error handling with Result types
- Authentication checks

**Use when**: Creating Server Actions in `actions/*.ts`

## 🚀 Quick Start

### Using a Template

1. **Choose appropriate template** based on what you're creating (page, layout, API route, or Server Action)

2. **Copy template code** from the template file

3. **Replace placeholders**:
   - `[FeatureName]` → Your feature name (e.g., `Zakat`, `Murabaha`)
   - `[feature-path]` → URL path (e.g., `zakat`, `murabaha/applications`)
   - `[Entity]` → Domain entity name
   - Customize imports, types, and logic

4. **Test thoroughly**:
   - TypeScript compilation
   - Runtime behavior
   - Error cases
   - Edge cases

### Example: Creating a Zakat Calculator Page

**Step 1**: Copy page template

**Step 2**: Replace placeholders:

```typescript
// From template:
export default async function [FeatureName]Page({ params, searchParams }: PageProps) {
  // ...
}

// Your code:
export default async function ZakatCalculatorPage({ params, searchParams }: PageProps) {
  // ...
}
```

**Step 3**: Customize data fetching:

```typescript
async function getZakatData(userId: string) {
  const calculations = await db.zakatCalculation.findMany({
    where: { userId },
    orderBy: { createdAt: "desc" },
  });

  return calculations;
}
```

**Step 4**: Test and deploy

## 📝 Template Conventions

All templates follow these conventions:

### TypeScript

- **Strict type safety**: All types explicitly defined
- **Zod validation**: Runtime validation for external data
- **Type imports**: Use `type` keyword for type-only imports

### Error Handling

- **Result types**: Return `{ success: true, data }` or `{ success: false, error }`
- **Try-catch blocks**: Wrap async operations
- **User-friendly messages**: Clear, actionable error messages

### Authentication

- **Server-side checks**: Verify authentication in Server Components/Actions
- **Session validation**: Use `auth()` helper
- **Unauthorized handling**: Return 401 for missing auth, 403 for insufficient permissions

### Data Fetching

- **Server Components**: Use async/await directly
- **Type-safe queries**: TypeScript types for database queries
- **Error boundaries**: Handle fetch failures gracefully

### Accessibility

- **Semantic HTML**: Use proper HTML5 elements
- **ARIA attributes**: Add when needed for dynamic content
- **Keyboard navigation**: Ensure all interactions keyboard accessible
- **Screen reader support**: Descriptive labels and announcements

## 🎨 OSE Platform Context

Templates include examples from the OSE Platform Islamic finance domain:

### Zakat (Obligatory Charity)

- Wealth calculation
- Nisab threshold checking
- Payment tracking
- Annual calculations

### Murabaha (Cost-Plus Financing)

- Contract applications
- Installment schedules
- Payment processing
- Contract management

### Waqf (Endowment)

- Project donations
- Recurring contributions
- Impact tracking
- Donor management

## 🔒 Security Best Practices

Templates incorporate security measures:

- **Input validation**: Zod schemas for all external data
- **SQL injection prevention**: Prisma ORM parameterized queries
- **XSS prevention**: React automatic escaping
- **CSRF protection**: Built-in Next.js Server Action protection
- **Authentication**: Session-based auth checks
- **Authorization**: Role-based access control
- **Rate limiting**: API endpoint protection

## 🧪 Testing Guidelines

Test templates thoroughly:

### Unit Tests

```typescript
// Example: Testing Server Action
import { calculateZakat } from "@/actions/zakat";

describe("calculateZakat", () => {
  it("calculates zakat correctly", async () => {
    const formData = new FormData();
    formData.append("wealth", "10000");
    formData.append("nisab", "5000");

    const result = await calculateZakat(null, formData);

    expect(result.success).toBe(true);
    expect(result.data?.zakatAmount).toBe(250);
  });
});
```

### Integration Tests

```typescript
// Example: Testing API route
import { POST } from "@/app/api/zakat/calculate/route";

describe("POST /api/zakat/calculate", () => {
  it("returns zakat calculation", async () => {
    const request = new NextRequest("http://localhost:3000/api/zakat/calculate", {
      method: "POST",
      body: JSON.stringify({ wealth: 10000, nisab: 5000 }),
    });

    const response = await POST(request);
    const data = await response.json();

    expect(response.status).toBe(200);
    expect(data.zakatAmount).toBe(250);
  });
});
```

### E2E Tests

```typescript
// Example: Testing page with Playwright
import { test, expect } from "@playwright/test";

test("Zakat calculator page", async ({ page }) => {
  await page.goto("/zakat/calculate");

  await page.fill('input[name="wealth"]', "10000");
  await page.fill('input[name="nisab"]', "5000");
  await page.click('button[type="submit"]');

  await expect(page.locator("text=Zakat Amount: $250")).toBeVisible();
});
```

## 📚 Related Documentation

- [Next.js README](../README.md) - Next.js overview and getting started
- [Next.js TypeScript](../ex-so-plwe-tsnext__typescript.md) - Type-safe patterns
- [Next.js Security](../ex-so-plwe-tsnext__security.md) - Security best practices
- [Next.js Testing](../ex-so-plwe-tsnext__testing.md) - Testing strategies
- [Next.js Best Practices](../ex-so-plwe-tsnext__best-practices.md) - Production guidelines

## 🔧 Customization Tips

### Adapting for Your Domain

Replace OSE Platform examples with your domain:

```typescript
// Template (Zakat example):
const calculations = await db.zakatCalculation.findMany();

// Your domain (E-commerce):
const orders = await db.order.findMany();
```

### Adding Custom Logic

Extend templates with domain-specific business logic:

```typescript
// Add custom validation
const customValidation = (data: FormData) => {
  // Your validation logic
};

// Add custom transformations
const transformData = (raw: RawData) => {
  // Your transformation logic
};
```

### Performance Optimization

Apply performance patterns:

```typescript
// Add caching
export const revalidate = 3600; // Cache for 1 hour

// Add streaming
<Suspense fallback={<Loading />}>
  <DataComponent />
</Suspense>
```

## 💡 Tips and Tricks

### Template Selection

**Choose Page Template when**:

- Creating user-facing pages
- Need SEO metadata
- Displaying data to users

**Choose Layout Template when**:

- Creating app structure
- Need persistent UI (navigation, sidebar)
- Managing global state providers

**Choose API Route Template when**:

- Creating REST endpoints
- Need external API access
- Building backend services

**Choose Server Action Template when**:

- Handling form submissions
- Performing mutations
- Need progressive enhancement

### Combining Templates

Mix templates for complex features:

1. **Layout** for structure
2. **Page** for content display
3. **Server Action** for form handling
4. **API Route** for external integrations

### Template Maintenance

Keep templates updated:

- Review when Next.js releases new versions
- Update based on production learnings
- Add new patterns as they emerge
- Remove deprecated patterns

## 🤝 Contributing

Improve templates:

1. Identify missing patterns
2. Test thoroughly
3. Document usage
4. Submit improvements
5. Share learnings

---

These templates provide battle-tested patterns for Next.js App Router development. Use them as starting points and customize for your specific needs.
