export default {
  default: {
    paths: ["../../specs/apps/a-demo/be/gherkin/**/*.feature"],
    import: ["tests/integration/hooks.ts", "tests/integration/world.ts", "tests/integration/steps/**/*.ts"],
    loader: ["tsx"],
    format: ["progress", "json:coverage/cucumber-report.json"],
    worldParameters: {},
  },
};
