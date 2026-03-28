# Component Patterns Reference

## Standard Component Template

Every shared component follows this structure:

```
button/
├── button.tsx            # Implementation
├── button.variants.ts    # CVA variant definitions
├── button.test.tsx       # Unit tests with vitest-axe
└── button.stories.tsx    # Storybook stories
```

## Complete Button Example

### Variant Definitions (`button.variants.ts`)

```typescript
import { cva, type VariantProps } from "class-variance-authority";

export const buttonVariants = cva(
  "inline-flex items-center justify-center gap-2 whitespace-nowrap rounded-md text-sm font-medium transition-colors disabled:pointer-events-none disabled:opacity-50 [&_svg:not([class*='size-'])]:size-4 [&_svg]:pointer-events-none [&_svg]:shrink-0",
  {
    variants: {
      variant: {
        default: "bg-primary text-primary-foreground shadow-xs hover:bg-primary/90",
        destructive:
          "bg-destructive text-destructive-foreground shadow-xs hover:bg-destructive/90 focus-visible:ring-destructive/20 dark:focus-visible:ring-destructive/40",
        outline: "border border-input bg-background shadow-xs hover:bg-accent hover:text-accent-foreground",
        secondary: "bg-secondary text-secondary-foreground shadow-xs hover:bg-secondary/80",
        ghost: "hover:bg-accent hover:text-accent-foreground",
        link: "text-primary underline-offset-4 hover:underline",
      },
      size: {
        default: "h-9 px-4 py-2",
        xs: "h-6 rounded-md px-2 text-xs",
        sm: "h-8 rounded-md px-3 text-xs",
        lg: "h-10 rounded-md px-6",
        icon: "size-9",
        "icon-xs": "size-6",
        "icon-sm": "size-8",
        "icon-lg": "size-10",
      },
    },
    defaultVariants: {
      variant: "default",
      size: "default",
    },
  },
);

export type ButtonVariantProps = VariantProps<typeof buttonVariants>;
```

### Component Implementation (`button.tsx`)

```tsx
import * as React from "react";
import { Slot } from "radix-ui";

import { cn } from "@open-sharia-enterprise/ts-ui";
import { buttonVariants, type ButtonVariantProps } from "./button.variants";

function Button({
  className,
  variant,
  size,
  asChild = false,
  ...props
}: React.ComponentProps<"button"> & ButtonVariantProps & { asChild?: boolean }) {
  const Comp = asChild ? Slot.Root : "button";

  return (
    <Comp
      data-slot="button"
      data-variant={variant}
      data-size={size}
      className={cn(buttonVariants({ variant, size, className }))}
      {...props}
    />
  );
}

export { Button, buttonVariants };
```

## Key Patterns

### React.ComponentProps (NOT forwardRef)

```tsx
// CORRECT — modern pattern
function Input({ className, ...props }: React.ComponentProps<"input">) {
  return <input data-slot="input" className={cn("...", className)} {...props} />;
}

// WRONG — legacy pattern
const Input = React.forwardRef<HTMLInputElement, InputProps>((props, ref) => {
  return <input ref={ref} {...props} />;
});
```

### Radix UI Unified Import

```tsx
// CORRECT — unified package
import { Slot, Dialog, DropdownMenu } from "radix-ui";
// Use Slot.Root, Dialog.Root, Dialog.Trigger, etc.

// WRONG — individual packages
import { Slot } from "@radix-ui/react-slot";
import * as DialogPrimitive from "@radix-ui/react-dialog";
```

### cn() Usage

```tsx
import { cn } from "@open-sharia-enterprise/ts-ui";

// Conditional classes
<div className={cn("flex items-center", isActive && "bg-primary")} />

// Responsive classes
<div className={cn("p-2 md:p-4 lg:p-6")} />

// Variant override
<Button className={cn(buttonVariants({ variant: "outline" }), "custom-class")} />
```

### data-slot Attribute

Every component root element MUST have `data-slot`:

```tsx
<button data-slot="button" />
<div data-slot="card" />
<input data-slot="input" />
<dialog data-slot="dialog-content" />
```

### Required Component States

Every interactive component must handle:

| State         | CSS/Attribute                                      | Required For               |
| ------------- | -------------------------------------------------- | -------------------------- |
| Default       | (base styles)                                      | All components             |
| Hover         | `hover:`                                           | Buttons, links, cards      |
| Focus visible | `focus-visible:ring-*`                             | All interactive elements   |
| Active        | `active:`                                          | Buttons                    |
| Disabled      | `disabled:opacity-50 disabled:pointer-events-none` | Buttons, inputs            |
| Loading       | Custom (spinner + aria-busy)                       | Buttons with async actions |
| Error         | `aria-invalid:border-destructive`                  | Form inputs                |
| Success       | Custom (check icon + aria feedback)                | Form submissions           |

### asChild Pattern

Use Radix `Slot` to render as a different element:

```tsx
// Renders as <a> instead of <button>
<Button asChild>
  <a href="/page">Navigate</a>
</Button>

// Renders as Next.js Link
<Button asChild>
  <Link href="/page">Navigate</Link>
</Button>
```
