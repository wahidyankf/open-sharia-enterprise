import * as React from "react";

export interface AppHeaderProps {
  title: string;
  subtitle?: string;
  onBack?: () => void;
  trailing?: React.ReactNode;
}

export function AppHeader({ title, subtitle, onBack, trailing }: AppHeaderProps) {
  return (
    <header className="flex items-center gap-3 px-4 py-3">
      {onBack && (
        <button
          type="button"
          aria-label="Go back"
          onClick={onBack}
          className="flex h-10 w-10 flex-shrink-0 items-center justify-center rounded-xl bg-secondary"
        >
          ←
        </button>
      )}
      <div className="min-w-0 flex-1">
        <h1 className="truncate text-xl leading-none font-extrabold tracking-tight">{title}</h1>
        {subtitle && <p className="mt-0.5 text-xs text-muted-foreground">{subtitle}</p>}
      </div>
      {trailing && <div className="flex-shrink-0">{trailing}</div>}
    </header>
  );
}
