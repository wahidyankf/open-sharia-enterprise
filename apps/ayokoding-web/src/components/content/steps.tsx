import type { ReactNode } from "react";

interface StepsProps {
  children: ReactNode;
}

export function Steps({ children }: StepsProps) {
  return <div className="my-6 ml-4 border-l-2 border-border pl-6 [counter-reset:step]">{children}</div>;
}
