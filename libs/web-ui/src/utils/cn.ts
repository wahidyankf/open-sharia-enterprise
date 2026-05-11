import { type ClassValue, clsx } from "clsx";
import { twMerge } from "tailwind-merge";

/** Merges Tailwind CSS class names, resolving conflicts via tailwind-merge and handling conditionals via clsx.
 * @example
 * cn("px-4 py-2", isActive && "bg-primary", className)
 */
export function cn(...inputs: ClassValue[]) {
  return twMerge(clsx(inputs));
}
