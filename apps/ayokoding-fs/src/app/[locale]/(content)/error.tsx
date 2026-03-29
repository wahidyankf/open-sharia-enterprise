"use client";

import { Button } from "@open-sharia-enterprise/ts-ui";

export default function ContentError({ reset }: { error: Error; reset: () => void }) {
  return (
    <div className="flex flex-1 flex-col items-center justify-center px-4 py-16">
      <h2 className="mb-4 text-2xl font-bold">Something went wrong</h2>
      <p className="mb-6 text-muted-foreground">An error occurred while rendering this content.</p>
      <Button onClick={reset}>Try again</Button>
    </div>
  );
}
