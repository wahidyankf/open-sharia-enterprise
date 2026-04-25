import * as React from "react";
import { cva, type VariantProps } from "class-variance-authority";

import { type HueName } from "../hue-picker/hue-picker";
import { cn } from "../../utils/cn";

const badgeVariants = cva("inline-flex items-center font-bold uppercase tracking-wide rounded-full", {
  variants: {
    variant: {
      default: "bg-[var(--hue-color)] text-white",
      outline: "border border-[var(--hue-border)] bg-[var(--hue-wash)] text-[var(--hue-ink)]",
      secondary: "bg-secondary text-secondary-foreground",
      destructive: "bg-destructive text-white",
    },
    size: {
      sm: "text-[11px] px-2 py-0.5 gap-1",
      md: "text-[13px] px-2.5 py-1 gap-1.5",
    },
  },
  defaultVariants: { variant: "default", size: "sm" },
});

interface BadgeProps extends React.ComponentProps<"span">, VariantProps<typeof badgeVariants> {
  hue?: HueName;
}

function Badge({ hue, variant, size, className, style, ...props }: BadgeProps) {
  const hueStyle = hue
    ? ({
        "--hue-color": `var(--hue-${hue})`,
        "--hue-border": `var(--hue-${hue})`,
        "--hue-wash": `var(--hue-${hue}-wash)`,
        "--hue-ink": `var(--hue-${hue}-ink)`,
      } as React.CSSProperties)
    : {};

  return (
    <span
      data-slot="badge"
      style={{ ...hueStyle, ...style }}
      className={cn(badgeVariants({ variant, size }), className)}
      {...props}
    />
  );
}

export { Badge, badgeVariants };
export type { BadgeProps };
