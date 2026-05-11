/* eslint-disable @typescript-eslint/no-empty-object-type */
import "vitest";

interface AxeMatchers {
  toHaveNoViolations(): void;
}

declare module "vitest" {
  interface Assertion<T = unknown> extends AxeMatchers {}
  interface AsymmetricMatchersContaining extends AxeMatchers {}
}
