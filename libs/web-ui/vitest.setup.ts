import { expect } from "vitest";

// Manually register vitest-axe's toHaveNoViolations matcher
// because vitest-axe@0.1.0 targets Vi.Assertion (deprecated in vitest 3.x)
expect.extend({
  toHaveNoViolations(received: { violations: Array<{ id: string; description: string; nodes: unknown[] }> }) {
    const violations = received.violations;
    const pass = violations.length === 0;
    return {
      pass,
      message: () =>
        pass
          ? "Expected accessibility violations but found none"
          : `Expected no accessibility violations but found ${violations.length}:\n${violations.map((v) => `  - ${v.id}: ${v.description} (${v.nodes.length} node(s))`).join("\n")}`,
      actual: violations,
    };
  },
});
