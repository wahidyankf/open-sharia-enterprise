"use client";

import * as React from "react";
import { Sheet } from "../sheet/sheet";
import { Button } from "../button/button";
import { cn } from "../../utils/cn";

export interface InfoTipProps {
  title: string;
  text: string;
}

export function InfoTip({ title, text }: InfoTipProps) {
  const [open, setOpen] = React.useState(false);

  return (
    <>
      <button
        type="button"
        aria-label={title}
        onClick={() => setOpen(true)}
        className={cn(
          "inline-flex h-5 w-5 items-center justify-center rounded-full bg-secondary text-xs text-muted-foreground hover:bg-[var(--hue-sky-wash)]",
        )}
      >
        i
      </button>
      {open && (
        <Sheet title={title} onClose={() => setOpen(false)}>
          <p>{text}</p>
          <Button variant="outline" onClick={() => setOpen(false)} className="mt-4 w-full">
            Got it
          </Button>
        </Sheet>
      )}
    </>
  );
}
