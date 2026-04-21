"use client";

import { cn } from "../../utils/cn";

export const HUES = ["terracotta", "honey", "sage", "teal", "sky", "plum"] as const;
export type HueName = (typeof HUES)[number];

export interface HuePickerProps {
  value: HueName;
  onChange: (hue: HueName) => void;
}

export function HuePicker({ value, onChange }: HuePickerProps) {
  return (
    <div className="flex gap-2">
      {HUES.map((hue) => (
        <button
          key={hue}
          type="button"
          onClick={() => onChange(hue)}
          aria-label={hue}
          aria-pressed={value === hue}
          style={{ backgroundColor: `var(--hue-${hue})` }}
          className={cn(
            "h-8 w-8 cursor-pointer rounded-[10px] transition-all",
            value === hue && "outline outline-[3px] outline-offset-2 outline-[var(--color-foreground)]",
          )}
        />
      ))}
    </div>
  );
}
