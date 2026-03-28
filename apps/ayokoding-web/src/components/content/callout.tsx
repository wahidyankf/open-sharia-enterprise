import { Alert, AlertDescription } from "@open-sharia-enterprise/ts-ui";
import { AlertTriangle, Info, Lightbulb } from "lucide-react";
import type { ReactNode } from "react";

interface CalloutProps {
  type: string;
  children: ReactNode;
}

const iconMap: Record<string, ReactNode> = {
  warning: <AlertTriangle className="h-4 w-4" />,
  info: <Info className="h-4 w-4" />,
  tip: <Lightbulb className="h-4 w-4" />,
};

const variantMap: Record<string, "default" | "destructive"> = {
  warning: "destructive",
  info: "default",
  tip: "default",
};

export function Callout({ type, children }: CalloutProps) {
  return (
    <Alert variant={variantMap[type] ?? "default"} className="my-4">
      {iconMap[type]}
      <AlertDescription>{children}</AlertDescription>
    </Alert>
  );
}
